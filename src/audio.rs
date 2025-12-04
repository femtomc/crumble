//! Audio synthesis and output using cpal.
//!
//! This module provides a simple synthesizer that can play sounds triggered
//! by the scheduler. It supports basic waveforms and envelopes.

#[cfg(feature = "audio")]
use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
#[cfg(feature = "audio")]
use cpal::{Device, SampleFormat, Stream, StreamConfig};

use crossbeam_channel::{bounded, Sender};
use std::f64::consts::PI;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

/// Waveform types for synthesis.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Waveform {
    Sine,
    Saw,
    Square,
    Triangle,
    Noise,
}

impl Default for Waveform {
    fn default() -> Self {
        Waveform::Sine
    }
}

/// Parameters for a sound event.
#[derive(Debug, Clone)]
pub struct SoundParams {
    /// Sound name or waveform type.
    pub sound: String,
    /// Frequency in Hz (overrides note if set).
    pub freq: Option<f64>,
    /// MIDI note number (0-127).
    pub note: Option<f64>,
    /// Gain/volume (0.0 - 1.0).
    pub gain: f64,
    /// Pan (-1.0 left to 1.0 right).
    pub pan: f64,
    /// Attack time in seconds.
    pub attack: f64,
    /// Decay time in seconds.
    pub decay: f64,
    /// Sustain level (0.0 - 1.0).
    pub sustain: f64,
    /// Release time in seconds.
    pub release: f64,
    /// Low-pass filter cutoff frequency.
    pub lpf: Option<f64>,
    /// High-pass filter cutoff frequency.
    pub hpf: Option<f64>,
}

impl Default for SoundParams {
    fn default() -> Self {
        SoundParams {
            sound: "sine".to_string(),
            freq: None,
            note: Some(60.0), // Middle C
            gain: 0.5,
            pan: 0.0,
            attack: 0.005,
            decay: 0.1,
            sustain: 0.7,
            release: 0.1,
            lpf: None,
            hpf: None,
        }
    }
}

impl SoundParams {
    /// Get the frequency, either from `freq` or by converting `note` to Hz.
    pub fn get_freq(&self) -> f64 {
        if let Some(f) = self.freq {
            f
        } else if let Some(note) = self.note {
            // MIDI note to frequency: f = 440 * 2^((note - 69) / 12)
            440.0 * 2.0_f64.powf((note - 69.0) / 12.0)
        } else {
            440.0
        }
    }

    /// Get the waveform from the sound name.
    pub fn get_waveform(&self) -> Waveform {
        match self.sound.to_lowercase().as_str() {
            "sine" | "sin" => Waveform::Sine,
            "saw" | "sawtooth" => Waveform::Saw,
            "square" | "sq" | "pulse" => Waveform::Square,
            "tri" | "triangle" => Waveform::Triangle,
            "noise" | "white" => Waveform::Noise,
            _ => Waveform::Sine,
        }
    }
}

/// A scheduled note to be played.
#[derive(Debug, Clone)]
pub struct ScheduledNote {
    /// When to start (in samples from stream start).
    pub start_sample: u64,
    /// Duration in samples.
    pub duration_samples: u64,
    /// Sound parameters.
    pub params: SoundParams,
}

/// Message to send to the audio thread.
#[derive(Debug, Clone)]
pub enum AudioMessage {
    /// Schedule a note to play.
    PlayNote(ScheduledNote),
    /// Stop all sounds.
    StopAll,
    /// Shut down the audio engine.
    Shutdown,
}

/// An active voice being played.
struct Voice {
    waveform: Waveform,
    freq: f64,
    gain: f64,
    pan: f64,
    phase: f64,
    start_sample: u64,
    end_sample: u64,
    attack_samples: u64,
    decay_samples: u64,
    sustain_level: f64,
    release_samples: u64,
    // Simple one-pole lowpass filter state
    lpf_cutoff: Option<f64>,
    lpf_state: f64,
    // Simple one-pole highpass filter state
    hpf_cutoff: Option<f64>,
    hpf_state: f64,
    hpf_prev_input: f64,
    // For noise
    noise_state: u32,
}

impl Voice {
    fn new(note: &ScheduledNote, sample_rate: f64) -> Self {
        let params = &note.params;
        Voice {
            waveform: params.get_waveform(),
            freq: params.get_freq(),
            gain: params.gain,
            pan: params.pan,
            phase: 0.0,
            start_sample: note.start_sample,
            end_sample: note.start_sample + note.duration_samples,
            attack_samples: (params.attack * sample_rate) as u64,
            decay_samples: (params.decay * sample_rate) as u64,
            sustain_level: params.sustain,
            release_samples: (params.release * sample_rate) as u64,
            lpf_cutoff: params.lpf,
            lpf_state: 0.0,
            hpf_cutoff: params.hpf,
            hpf_state: 0.0,
            hpf_prev_input: 0.0,
            noise_state: 0x12345678,
        }
    }

    fn is_finished(&self, current_sample: u64) -> bool {
        current_sample > self.end_sample + self.release_samples
    }

    fn generate_sample(&mut self, current_sample: u64, sample_rate: f64) -> (f64, f64) {
        if current_sample < self.start_sample {
            return (0.0, 0.0);
        }

        let sample_in_note = current_sample - self.start_sample;

        // Generate raw waveform
        let phase_inc = self.freq / sample_rate;
        self.phase += phase_inc;
        if self.phase >= 1.0 {
            self.phase -= 1.0;
        }

        let raw = match self.waveform {
            Waveform::Sine => (self.phase * 2.0 * PI).sin(),
            Waveform::Saw => 2.0 * self.phase - 1.0,
            Waveform::Square => {
                if self.phase < 0.5 {
                    1.0
                } else {
                    -1.0
                }
            }
            Waveform::Triangle => {
                if self.phase < 0.5 {
                    4.0 * self.phase - 1.0
                } else {
                    3.0 - 4.0 * self.phase
                }
            }
            Waveform::Noise => {
                // Simple xorshift noise
                self.noise_state ^= self.noise_state << 13;
                self.noise_state ^= self.noise_state >> 17;
                self.noise_state ^= self.noise_state << 5;
                (self.noise_state as f64 / u32::MAX as f64) * 2.0 - 1.0
            }
        };

        // Apply lowpass filter
        let filtered = if let Some(cutoff) = self.lpf_cutoff {
            let rc = 1.0 / (2.0 * PI * cutoff);
            let dt = 1.0 / sample_rate;
            let alpha = dt / (rc + dt);
            self.lpf_state = self.lpf_state + alpha * (raw - self.lpf_state);
            self.lpf_state
        } else {
            raw
        };

        // Apply highpass filter
        let filtered = if let Some(cutoff) = self.hpf_cutoff {
            let rc = 1.0 / (2.0 * PI * cutoff);
            let dt = 1.0 / sample_rate;
            let alpha = rc / (rc + dt);
            self.hpf_state = alpha * (self.hpf_state + filtered - self.hpf_prev_input);
            self.hpf_prev_input = filtered;
            self.hpf_state
        } else {
            filtered
        };

        // Calculate envelope
        let note_duration = self.end_sample - self.start_sample;
        let envelope = if sample_in_note < self.attack_samples {
            // Attack phase
            sample_in_note as f64 / self.attack_samples as f64
        } else if sample_in_note < self.attack_samples + self.decay_samples {
            // Decay phase
            let decay_progress =
                (sample_in_note - self.attack_samples) as f64 / self.decay_samples as f64;
            1.0 - (1.0 - self.sustain_level) * decay_progress
        } else if sample_in_note < note_duration {
            // Sustain phase
            self.sustain_level
        } else {
            // Release phase
            let release_progress = (sample_in_note - note_duration) as f64 / self.release_samples as f64;
            self.sustain_level * (1.0 - release_progress).max(0.0)
        };

        let sample = filtered * envelope * self.gain;

        // Apply panning (constant power)
        let pan_angle = (self.pan + 1.0) * PI / 4.0; // 0 to PI/2
        let left = sample * pan_angle.cos();
        let right = sample * pan_angle.sin();

        (left, right)
    }
}

/// Audio engine state shared between threads.
struct AudioState {
    voices: Vec<Voice>,
    current_sample: u64,
    sample_rate: f64,
}

impl AudioState {
    fn new(sample_rate: f64) -> Self {
        AudioState {
            voices: Vec::with_capacity(128),
            current_sample: 0,
            sample_rate,
        }
    }

    fn add_voice(&mut self, note: ScheduledNote) {
        // Limit polyphony
        if self.voices.len() >= 64 {
            // Remove oldest finished voice, or oldest voice if none finished
            if let Some(idx) = self
                .voices
                .iter()
                .position(|v| v.is_finished(self.current_sample))
            {
                self.voices.remove(idx);
            } else {
                self.voices.remove(0);
            }
        }
        self.voices.push(Voice::new(&note, self.sample_rate));
    }

    fn generate_frame(&mut self) -> (f32, f32) {
        let mut left = 0.0;
        let mut right = 0.0;

        // Remove finished voices
        self.voices
            .retain(|v| !v.is_finished(self.current_sample));

        // Mix all voices
        for voice in &mut self.voices {
            let (l, r) = voice.generate_sample(self.current_sample, self.sample_rate);
            left += l;
            right += r;
        }

        self.current_sample += 1;

        // Soft clip to prevent harsh clipping
        let clip = |x: f64| -> f32 {
            let x = x.clamp(-2.0, 2.0);
            (x / (1.0 + x.abs())) as f32
        };

        (clip(left), clip(right))
    }

    fn stop_all(&mut self) {
        self.voices.clear();
    }
}

/// Handle to control the audio engine.
#[cfg(feature = "audio")]
pub struct AudioEngine {
    sender: Sender<AudioMessage>,
    running: Arc<AtomicBool>,
    stream: Stream,
    sample_rate: f64,
    stream_start: std::time::Instant,
}

#[cfg(feature = "audio")]
impl AudioEngine {
    /// Create and start a new audio engine.
    pub fn new() -> Result<Self, AudioError> {
        let host = cpal::default_host();
        let device = host
            .default_output_device()
            .ok_or(AudioError::NoOutputDevice)?;

        let supported_config = device
            .default_output_config()
            .map_err(|e| AudioError::ConfigError(e.to_string()))?;

        let sample_rate = supported_config.sample_rate().0 as f64;
        let config: StreamConfig = supported_config.clone().into();

        let (tx, rx) = bounded::<AudioMessage>(256);
        let running = Arc::new(AtomicBool::new(true));

        let state = Arc::new(Mutex::new(AudioState::new(sample_rate)));
        let state_clone = state.clone();
        let running_clone = running.clone();

        // Spawn message handler thread
        std::thread::spawn(move || {
            while running_clone.load(Ordering::SeqCst) {
                match rx.recv_timeout(std::time::Duration::from_millis(10)) {
                    Ok(AudioMessage::PlayNote(note)) => {
                        state_clone.lock().unwrap().add_voice(note);
                    }
                    Ok(AudioMessage::StopAll) => {
                        state_clone.lock().unwrap().stop_all();
                    }
                    Ok(AudioMessage::Shutdown) => {
                        running_clone.store(false, Ordering::SeqCst);
                        break;
                    }
                    Err(_) => {}
                }
            }
        });

        let stream = match supported_config.sample_format() {
            SampleFormat::F32 => Self::build_stream::<f32>(&device, &config, state.clone())?,
            SampleFormat::I16 => Self::build_stream::<i16>(&device, &config, state.clone())?,
            SampleFormat::U16 => Self::build_stream::<u16>(&device, &config, state.clone())?,
            _ => return Err(AudioError::UnsupportedFormat),
        };

        stream
            .play()
            .map_err(|e| AudioError::StreamError(e.to_string()))?;

        Ok(AudioEngine {
            sender: tx,
            running,
            stream,
            sample_rate,
            stream_start: std::time::Instant::now(),
        })
    }

    fn build_stream<S: cpal::SizedSample + cpal::FromSample<f32>>(
        device: &Device,
        config: &StreamConfig,
        state: Arc<Mutex<AudioState>>,
    ) -> Result<Stream, AudioError> {
        let channels = config.channels as usize;

        let stream = device
            .build_output_stream(
                config,
                move |data: &mut [S], _: &cpal::OutputCallbackInfo| {
                    let mut state = state.lock().unwrap();
                    for frame in data.chunks_mut(channels) {
                        let (left, right) = state.generate_frame();
                        frame[0] = S::from_sample(left);
                        if channels > 1 {
                            frame[1] = S::from_sample(right);
                        }
                    }
                },
                |err| eprintln!("Audio stream error: {}", err),
                None,
            )
            .map_err(|e| AudioError::StreamError(e.to_string()))?;

        Ok(stream)
    }

    /// Get the sample rate.
    pub fn sample_rate(&self) -> f64 {
        self.sample_rate
    }

    /// Get the current stream time in seconds.
    pub fn current_time(&self) -> f64 {
        self.stream_start.elapsed().as_secs_f64()
    }

    /// Schedule a note to play at a specific time.
    pub fn play_at(&self, time: f64, duration: f64, params: SoundParams) {
        let start_sample = (time * self.sample_rate) as u64;
        let duration_samples = (duration * self.sample_rate) as u64;

        let note = ScheduledNote {
            start_sample,
            duration_samples,
            params,
        };

        let _ = self.sender.send(AudioMessage::PlayNote(note));
    }

    /// Play a note immediately.
    pub fn play_now(&self, duration: f64, params: SoundParams) {
        self.play_at(self.current_time(), duration, params);
    }

    /// Stop all currently playing sounds.
    pub fn stop_all(&self) {
        let _ = self.sender.send(AudioMessage::StopAll);
    }

    /// Shut down the audio engine.
    pub fn shutdown(self) {
        self.running.store(false, Ordering::SeqCst);
        let _ = self.sender.send(AudioMessage::Shutdown);
        drop(self.stream);
    }
}

/// Errors that can occur with audio.
#[derive(Debug)]
pub enum AudioError {
    NoOutputDevice,
    ConfigError(String),
    StreamError(String),
    UnsupportedFormat,
}

impl std::fmt::Display for AudioError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AudioError::NoOutputDevice => write!(f, "No audio output device found"),
            AudioError::ConfigError(e) => write!(f, "Audio config error: {}", e),
            AudioError::StreamError(e) => write!(f, "Audio stream error: {}", e),
            AudioError::UnsupportedFormat => write!(f, "Unsupported audio format"),
        }
    }
}

impl std::error::Error for AudioError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sound_params_freq() {
        let params = SoundParams {
            note: Some(69.0), // A4
            ..Default::default()
        };
        assert!((params.get_freq() - 440.0).abs() < 0.01);

        let params = SoundParams {
            note: Some(60.0), // C4
            ..Default::default()
        };
        assert!((params.get_freq() - 261.63).abs() < 1.0);
    }

    #[test]
    fn test_waveform_detection() {
        assert_eq!(
            SoundParams {
                sound: "sine".to_string(),
                ..Default::default()
            }
            .get_waveform(),
            Waveform::Sine
        );
        assert_eq!(
            SoundParams {
                sound: "SAW".to_string(),
                ..Default::default()
            }
            .get_waveform(),
            Waveform::Saw
        );
    }
}
