//! High-level pattern player that connects patterns to audio output.
//!
//! This module provides a simple API to play patterns using either the
//! built-in synthesizer or SuperCollider via OSC.

use crate::lisp::{run_lisp, Value};
use crate::pattern::Pattern;
use crate::scheduler::{play_blocking, SchedulerConfig, SchedulerHandle, start_scheduler};

#[cfg(feature = "audio")]
use crate::audio::{AudioEngine, SoundParams};

#[cfg(feature = "osc")]
use crate::osc::{DirtParams, OscSender};

/// Output mode for the player.
#[derive(Debug, Clone)]
pub enum OutputMode {
    /// Use built-in synthesizer.
    #[cfg(feature = "audio")]
    Audio,
    /// Send to SuperCollider/SuperDirt via OSC.
    #[cfg(feature = "osc")]
    SuperDirt { address: String },
    /// Print events to console (for debugging).
    Console,
    /// Custom callback.
    Custom,
}

impl Default for OutputMode {
    fn default() -> Self {
        #[cfg(feature = "audio")]
        return OutputMode::Audio;
        #[cfg(not(feature = "audio"))]
        return OutputMode::Console;
    }
}

/// Configuration for the player.
#[derive(Debug, Clone)]
pub struct PlayerConfig {
    /// Cycles per second (tempo).
    pub cps: f64,
    /// Output mode.
    pub output: OutputMode,
    /// Latency in seconds.
    pub latency: f64,
}

impl Default for PlayerConfig {
    fn default() -> Self {
        PlayerConfig {
            cps: 0.5,
            output: OutputMode::default(),
            latency: 0.1,
        }
    }
}

/// A pattern player that handles audio output.
#[cfg(feature = "audio")]
pub struct Player {
    config: PlayerConfig,
    audio_engine: Option<AudioEngine>,
}

#[cfg(feature = "audio")]
impl Player {
    /// Create a new player with the given configuration.
    pub fn new(config: PlayerConfig) -> Result<Self, PlayerError> {
        let audio_engine = match &config.output {
            OutputMode::Audio => Some(AudioEngine::new().map_err(PlayerError::Audio)?),
            _ => None,
        };

        Ok(Player {
            config,
            audio_engine,
        })
    }

    /// Create a player with default audio output.
    pub fn with_audio() -> Result<Self, PlayerError> {
        Self::new(PlayerConfig {
            output: OutputMode::Audio,
            ..Default::default()
        })
    }

    /// Create a player that outputs to SuperDirt.
    #[cfg(feature = "osc")]
    pub fn with_superdirt() -> Result<Self, PlayerError> {
        Self::new(PlayerConfig {
            output: OutputMode::SuperDirt {
                address: format!("127.0.0.1:{}", crate::osc::SUPERDIRT_PORT),
            },
            ..Default::default()
        })
    }

    /// Set the tempo in cycles per second.
    pub fn set_cps(&mut self, cps: f64) {
        self.config.cps = cps;
    }

    /// Set the tempo in BPM (assuming 4 beats per cycle).
    pub fn set_bpm(&mut self, bpm: f64) {
        self.config.cps = bpm / 60.0 / 4.0;
    }

    /// Play a pattern for a given number of cycles.
    pub fn play_cycles<T>(&self, pattern: Pattern<T>, cycles: f64)
    where
        T: Clone + Send + Sync + 'static + IntoSoundParams,
    {
        let scheduler_config = SchedulerConfig {
            cps: self.config.cps,
            latency: self.config.latency,
            ..Default::default()
        };

        // For audio output, use the engine's play_at method via main thread
        // Since we're blocking, we can call it directly after collecting events
        match &self.config.output {
            OutputMode::Audio => {
                if let Some(ref engine) = self.audio_engine {
                    // Collect and play events synchronously
                    let duration_secs = cycles / self.config.cps;
                    let start = std::time::Instant::now();

                    let (handle, rx) = crate::scheduler::start_scheduler_with_channel(
                        scheduler_config,
                        pattern,
                    );

                    while start.elapsed().as_secs_f64() < duration_secs {
                        // Process any pending events
                        while let Ok(event) = rx.try_recv() {
                            let params = event.hap.value.into_sound_params();
                            engine.play_at(event.trigger_time, event.duration, params);
                        }
                        std::thread::sleep(std::time::Duration::from_millis(5));
                    }

                    handle.stop();
                }
            }
            #[cfg(feature = "osc")]
            OutputMode::SuperDirt { ref address } => {
                let addr = address.clone();
                play_blocking(pattern, cycles, scheduler_config, move |event| {
                    // Create sender per event (UDP is cheap)
                    if let Ok(sender) = OscSender::new(&addr) {
                        let params = event.hap.value.into_dirt_params(event.cycle, event.duration);
                        let _ = sender.play(&params);
                    }
                });
            }
            OutputMode::Console => {
                play_blocking(pattern, cycles, scheduler_config, move |event| {
                    println!(
                        "[cycle {:.2}] trigger at {:.3}s, duration {:.3}s",
                        event.cycle, event.trigger_time, event.duration
                    );
                });
            }
            OutputMode::Custom => {}
        }
    }

    /// Play a pattern indefinitely until stopped.
    pub fn play<T>(&self, pattern: Pattern<T>) -> PlayHandle<T>
    where
        T: Clone + Send + Sync + 'static + IntoSoundParams,
    {
        let scheduler_config = SchedulerConfig {
            cps: self.config.cps,
            latency: self.config.latency,
            ..Default::default()
        };

        let output = self.config.output.clone();

        // For audio, we need to share the engine
        // This is a simplified version - a real implementation would use Arc
        let handle = start_scheduler(scheduler_config, pattern, move |event| {
            match &output {
                OutputMode::Console => {
                    println!(
                        "[cycle {:.2}] trigger at {:.3}s, duration {:.3}s",
                        event.cycle, event.trigger_time, event.duration
                    );
                }
                _ => {
                    // Other modes would need shared state
                }
            }
        });

        PlayHandle { inner: handle }
    }

    /// Play a Lisp expression for a given number of cycles.
    pub fn play_lisp(&self, code: &str, cycles: f64) -> Result<(), PlayerError> {
        let value = run_lisp(code).map_err(|e| PlayerError::LispError(e.to_string()))?;

        match value {
            Value::Pattern(pat) => {
                self.play_pattern_value(pat, cycles);
                Ok(())
            }
            _ => Err(PlayerError::NotAPattern),
        }
    }

    fn play_pattern_value(&self, pattern: Pattern<Value>, cycles: f64) {
        let scheduler_config = SchedulerConfig {
            cps: self.config.cps,
            latency: self.config.latency,
            ..Default::default()
        };

        match &self.config.output {
            OutputMode::Audio => {
                if let Some(ref engine) = self.audio_engine {
                    let duration_secs = cycles / self.config.cps;
                    let start = std::time::Instant::now();

                    let (handle, rx) = crate::scheduler::start_scheduler_with_channel(
                        scheduler_config,
                        pattern,
                    );

                    while start.elapsed().as_secs_f64() < duration_secs {
                        while let Ok(event) = rx.try_recv() {
                            let params = value_to_sound_params(&event.hap.value);
                            engine.play_at(event.trigger_time, event.duration, params);
                        }
                        std::thread::sleep(std::time::Duration::from_millis(5));
                    }

                    handle.stop();
                }
            }
            #[cfg(feature = "osc")]
            OutputMode::SuperDirt { ref address } => {
                let addr = address.clone();
                play_blocking(pattern, cycles, scheduler_config, move |event| {
                    if let Ok(sender) = OscSender::new(&addr) {
                        let dirt = value_to_dirt_params(&event.hap.value, event.cycle, event.duration);
                        let _ = sender.play(&dirt);
                    }
                });
            }
            OutputMode::Console => {
                play_blocking(pattern, cycles, scheduler_config, move |event| {
                    println!(
                        "[cycle {:.2}] {:?} at {:.3}s",
                        event.cycle, event.hap.value, event.trigger_time
                    );
                });
            }
            OutputMode::Custom => {}
        }
    }
}

/// Handle to control a playing pattern.
pub struct PlayHandle<T: Clone + Send + 'static = ()> {
    inner: SchedulerHandle<T>,
}

impl<T: Clone + Send + 'static> PlayHandle<T> {
    /// Stop playback.
    pub fn stop(&self) {
        self.inner.stop();
    }

    /// Check if still playing.
    pub fn is_playing(&self) -> bool {
        self.inner.is_running()
    }
}

/// Trait for types that can be converted to sound parameters.
pub trait IntoSoundParams {
    #[cfg(feature = "audio")]
    fn into_sound_params(&self) -> SoundParams;

    #[cfg(feature = "osc")]
    fn into_dirt_params(&self, cycle: f64, duration: f64) -> DirtParams;
}

// Implement for common types
impl IntoSoundParams for String {
    #[cfg(feature = "audio")]
    fn into_sound_params(&self) -> SoundParams {
        SoundParams {
            sound: self.clone(),
            ..Default::default()
        }
    }

    #[cfg(feature = "osc")]
    fn into_dirt_params(&self, cycle: f64, duration: f64) -> DirtParams {
        DirtParams {
            s: Some(self.clone()),
            cycle: Some(cycle as f32),
            delta: Some(duration as f32),
            ..Default::default()
        }
    }
}

impl IntoSoundParams for &str {
    #[cfg(feature = "audio")]
    fn into_sound_params(&self) -> SoundParams {
        SoundParams {
            sound: self.to_string(),
            ..Default::default()
        }
    }

    #[cfg(feature = "osc")]
    fn into_dirt_params(&self, cycle: f64, duration: f64) -> DirtParams {
        DirtParams {
            s: Some(self.to_string()),
            cycle: Some(cycle as f32),
            delta: Some(duration as f32),
            ..Default::default()
        }
    }
}

impl IntoSoundParams for i64 {
    #[cfg(feature = "audio")]
    fn into_sound_params(&self) -> SoundParams {
        SoundParams {
            note: Some(*self as f64),
            ..Default::default()
        }
    }

    #[cfg(feature = "osc")]
    fn into_dirt_params(&self, cycle: f64, duration: f64) -> DirtParams {
        DirtParams {
            note: Some(*self as f32),
            cycle: Some(cycle as f32),
            delta: Some(duration as f32),
            ..Default::default()
        }
    }
}

impl IntoSoundParams for f64 {
    #[cfg(feature = "audio")]
    fn into_sound_params(&self) -> SoundParams {
        SoundParams {
            freq: Some(*self),
            ..Default::default()
        }
    }

    #[cfg(feature = "osc")]
    fn into_dirt_params(&self, cycle: f64, duration: f64) -> DirtParams {
        DirtParams {
            freq: Some(*self as f32),
            cycle: Some(cycle as f32),
            delta: Some(duration as f32),
            ..Default::default()
        }
    }
}

#[cfg(feature = "audio")]
fn value_to_sound_params(value: &Value) -> SoundParams {
    match value {
        Value::String(s) => SoundParams {
            sound: s.clone(),
            ..Default::default()
        },
        Value::Integer(n) => SoundParams {
            note: Some(*n as f64),
            ..Default::default()
        },
        Value::Float(f) => SoundParams {
            freq: Some(*f),
            ..Default::default()
        },
        _ => SoundParams::default(),
    }
}

#[cfg(feature = "osc")]
fn value_to_dirt_params(value: &Value, cycle: f64, duration: f64) -> DirtParams {
    let mut params = DirtParams {
        cycle: Some(cycle as f32),
        delta: Some(duration as f32),
        ..Default::default()
    };

    match value {
        Value::String(s) => params.s = Some(s.clone()),
        Value::Integer(n) => params.note = Some(*n as f32),
        Value::Float(f) => params.freq = Some(*f as f32),
        _ => {}
    }

    params
}

/// Errors that can occur with the player.
#[derive(Debug)]
pub enum PlayerError {
    #[cfg(feature = "audio")]
    Audio(crate::audio::AudioError),
    #[cfg(feature = "osc")]
    Osc(std::io::Error),
    LispError(String),
    NotAPattern,
}

impl std::fmt::Display for PlayerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            #[cfg(feature = "audio")]
            PlayerError::Audio(e) => write!(f, "Audio error: {}", e),
            #[cfg(feature = "osc")]
            PlayerError::Osc(e) => write!(f, "OSC error: {}", e),
            PlayerError::LispError(e) => write!(f, "Lisp error: {}", e),
            PlayerError::NotAPattern => write!(f, "Expression did not evaluate to a pattern"),
        }
    }
}

impl std::error::Error for PlayerError {}

/// Convenience function to play a pattern for one cycle.
#[cfg(feature = "audio")]
pub fn play_once<T>(pattern: Pattern<T>)
where
    T: Clone + Send + Sync + 'static + IntoSoundParams,
{
    if let Ok(player) = Player::with_audio() {
        player.play_cycles(pattern, 1.0);
    }
}

/// Convenience function to play a Lisp pattern for one cycle.
#[cfg(feature = "audio")]
pub fn play_lisp_once(code: &str) -> Result<(), PlayerError> {
    let player = Player::with_audio()?;
    player.play_lisp(code, 1.0)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_into_sound_params_string() {
        let s = "kick".to_string();
        #[cfg(feature = "audio")]
        {
            let params = s.into_sound_params();
            assert_eq!(params.sound, "kick");
        }
        #[cfg(not(feature = "audio"))]
        let _ = s; // Silence unused warning
    }

    #[test]
    fn test_into_sound_params_note() {
        let n: i64 = 60;
        #[cfg(feature = "audio")]
        {
            let params = n.into_sound_params();
            assert_eq!(params.note, Some(60.0));
        }
        #[cfg(not(feature = "audio"))]
        let _ = n; // Silence unused warning
    }
}
