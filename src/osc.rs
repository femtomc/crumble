//! OSC output for SuperCollider/SuperDirt communication.
//!
//! This module provides functionality to send pattern events to SuperCollider
//! via OSC (Open Sound Control), compatible with SuperDirt.

#[cfg(feature = "osc")]
use rosc::{encoder, OscMessage, OscPacket, OscType};

use std::collections::HashMap;
use std::net::UdpSocket;
use std::io;

/// Default SuperDirt OSC port.
pub const SUPERDIRT_PORT: u16 = 57120;

/// OSC client for sending messages to SuperCollider.
#[cfg(feature = "osc")]
pub struct OscSender {
    socket: UdpSocket,
    target: String,
}

#[cfg(feature = "osc")]
impl OscSender {
    /// Create a new OSC sender targeting the given address.
    pub fn new(target: &str) -> io::Result<Self> {
        let socket = UdpSocket::bind("0.0.0.0:0")?;
        Ok(OscSender {
            socket,
            target: target.to_string(),
        })
    }

    /// Create a new OSC sender targeting localhost SuperDirt.
    pub fn superdirt() -> io::Result<Self> {
        Self::new(&format!("127.0.0.1:{}", SUPERDIRT_PORT))
    }

    /// Send a raw OSC message.
    pub fn send(&self, msg: OscMessage) -> io::Result<()> {
        let packet = OscPacket::Message(msg);
        let buf = encoder::encode(&packet)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e.to_string()))?;
        self.socket.send_to(&buf, &self.target)?;
        Ok(())
    }

    /// Send a SuperDirt play message.
    ///
    /// The params map should contain string keys and values that can be
    /// converted to OSC types.
    pub fn play(&self, params: &DirtParams) -> io::Result<()> {
        let args = params.to_osc_args();
        let msg = OscMessage {
            addr: "/dirt/play".to_string(),
            args,
        };
        self.send(msg)
    }

    /// Send a handshake message to SuperDirt.
    pub fn handshake(&self) -> io::Result<()> {
        let msg = OscMessage {
            addr: "/dirt/handshake".to_string(),
            args: vec![],
        };
        self.send(msg)
    }
}

/// Parameters for a SuperDirt sound event.
#[derive(Debug, Clone, Default)]
pub struct DirtParams {
    /// Sound name (sample bank).
    pub s: Option<String>,
    /// Sample number within the bank.
    pub n: Option<i32>,
    /// Gain (0.0 - 1.0+).
    pub gain: Option<f32>,
    /// Pan (-1.0 to 1.0).
    pub pan: Option<f32>,
    /// Speed (1.0 = normal, 2.0 = double speed).
    pub speed: Option<f32>,
    /// Frequency for synths.
    pub freq: Option<f32>,
    /// MIDI note number.
    pub note: Option<f32>,
    /// Cycle number.
    pub cycle: Option<f32>,
    /// Delta (event duration in cycles).
    pub delta: Option<f32>,
    /// Orbit (effect bus number).
    pub orbit: Option<i32>,
    /// Low-pass filter cutoff.
    pub lpf: Option<f32>,
    /// Low-pass filter resonance.
    pub lpq: Option<f32>,
    /// High-pass filter cutoff.
    pub hpf: Option<f32>,
    /// High-pass filter resonance.
    pub hpq: Option<f32>,
    /// Reverb room size (0.0 - 1.0).
    pub room: Option<f32>,
    /// Reverb dry/wet.
    pub size: Option<f32>,
    /// Delay time.
    pub delay: Option<f32>,
    /// Delay feedback.
    pub delayfeedback: Option<f32>,
    /// Delay wet/dry.
    pub delaytime: Option<f32>,
    /// Attack time.
    pub attack: Option<f32>,
    /// Decay time.
    pub decay: Option<f32>,
    /// Sustain level.
    pub sustain: Option<f32>,
    /// Release time.
    pub release: Option<f32>,
    /// Sample start position (0.0 - 1.0).
    pub begin: Option<f32>,
    /// Sample end position (0.0 - 1.0).
    pub end: Option<f32>,
    /// Additional parameters.
    pub extra: HashMap<String, DirtValue>,
}

/// A value that can be sent to SuperDirt.
#[derive(Debug, Clone)]
pub enum DirtValue {
    Int(i32),
    Float(f32),
    String(String),
}

#[cfg(feature = "osc")]
impl DirtParams {
    /// Create new empty params.
    pub fn new() -> Self {
        Default::default()
    }

    /// Set the sound name.
    pub fn sound(mut self, s: &str) -> Self {
        self.s = Some(s.to_string());
        self
    }

    /// Set the sample number.
    pub fn sample(mut self, n: i32) -> Self {
        self.n = Some(n);
        self
    }

    /// Set the gain.
    pub fn with_gain(mut self, gain: f32) -> Self {
        self.gain = Some(gain);
        self
    }

    /// Set the pan.
    pub fn with_pan(mut self, pan: f32) -> Self {
        self.pan = Some(pan);
        self
    }

    /// Set the note.
    pub fn with_note(mut self, note: f32) -> Self {
        self.note = Some(note);
        self
    }

    /// Convert to OSC arguments for /dirt/play.
    pub fn to_osc_args(&self) -> Vec<OscType> {
        let mut args = Vec::new();

        macro_rules! add_param_f32 {
            ($key:expr, $val:expr) => {
                if let Some(v) = $val {
                    args.push(OscType::String($key.to_string()));
                    args.push(OscType::Float(v));
                }
            };
        }

        macro_rules! add_param_i32 {
            ($key:expr, $val:expr) => {
                if let Some(v) = $val {
                    args.push(OscType::String($key.to_string()));
                    args.push(OscType::Int(v));
                }
            };
        }

        // Sound and sample
        if let Some(ref s) = self.s {
            args.push(OscType::String("s".to_string()));
            args.push(OscType::String(s.clone()));
        }
        add_param_i32!("n", self.n);

        // Timing
        add_param_f32!("cycle", self.cycle);
        add_param_f32!("delta", self.delta);

        // Pitch
        add_param_f32!("freq", self.freq);
        add_param_f32!("note", self.note);
        add_param_f32!("speed", self.speed);

        // Amplitude
        add_param_f32!("gain", self.gain);
        add_param_f32!("pan", self.pan);

        // Filters
        add_param_f32!("lpf", self.lpf);
        add_param_f32!("lpq", self.lpq);
        add_param_f32!("hpf", self.hpf);
        add_param_f32!("hpq", self.hpq);

        // Effects
        add_param_i32!("orbit", self.orbit);
        add_param_f32!("room", self.room);
        add_param_f32!("size", self.size);
        add_param_f32!("delay", self.delay);
        add_param_f32!("delayfeedback", self.delayfeedback);
        add_param_f32!("delaytime", self.delaytime);

        // Envelope
        add_param_f32!("attack", self.attack);
        add_param_f32!("decay", self.decay);
        add_param_f32!("sustain", self.sustain);
        add_param_f32!("release", self.release);

        // Sample position
        add_param_f32!("begin", self.begin);
        add_param_f32!("end", self.end);

        // Extra parameters
        for (key, value) in &self.extra {
            args.push(OscType::String(key.clone()));
            match value {
                DirtValue::Int(i) => args.push(OscType::Int(*i)),
                DirtValue::Float(f) => args.push(OscType::Float(*f)),
                DirtValue::String(s) => args.push(OscType::String(s.clone())),
            }
        }

        args
    }
}

/// Convert a string-keyed HashMap to DirtParams.
/// This is useful for converting pattern values to OSC messages.
pub fn params_from_map(map: &HashMap<String, String>) -> DirtParams {
    let mut params = DirtParams::new();

    for (key, value) in map {
        match key.as_str() {
            "s" | "sound" => params.s = Some(value.clone()),
            "n" => params.n = value.parse().ok(),
            "gain" => params.gain = value.parse().ok(),
            "pan" => params.pan = value.parse().ok(),
            "speed" => params.speed = value.parse().ok(),
            "freq" => params.freq = value.parse().ok(),
            "note" => params.note = value.parse().ok(),
            "lpf" => params.lpf = value.parse().ok(),
            "hpf" => params.hpf = value.parse().ok(),
            "room" => params.room = value.parse().ok(),
            "delay" => params.delay = value.parse().ok(),
            "orbit" => params.orbit = value.parse().ok(),
            _ => {
                // Try to parse as float, then int, then string
                if let Ok(f) = value.parse::<f32>() {
                    params.extra.insert(key.clone(), DirtValue::Float(f));
                } else if let Ok(i) = value.parse::<i32>() {
                    params.extra.insert(key.clone(), DirtValue::Int(i));
                } else {
                    params
                        .extra
                        .insert(key.clone(), DirtValue::String(value.clone()));
                }
            }
        }
    }

    params
}

#[cfg(all(test, feature = "osc"))]
mod tests {
    use super::*;

    #[test]
    fn test_dirt_params() {
        let params = DirtParams::new()
            .sound("bd")
            .sample(0)
            .with_gain(0.8)
            .with_pan(0.5);

        let args = params.to_osc_args();
        assert!(!args.is_empty());

        // Check that "s" and "bd" are in the args
        let has_sound = args.windows(2).any(|w| {
            matches!(&w[0], OscType::String(s) if s == "s")
                && matches!(&w[1], OscType::String(s) if s == "bd")
        });
        assert!(has_sound);
    }

    #[test]
    fn test_params_from_map() {
        let mut map = HashMap::new();
        map.insert("s".to_string(), "cp".to_string());
        map.insert("gain".to_string(), "0.5".to_string());
        map.insert("n".to_string(), "2".to_string());

        let params = params_from_map(&map);
        assert_eq!(params.s, Some("cp".to_string()));
        assert_eq!(params.gain, Some(0.5));
        assert_eq!(params.n, Some(2));
    }
}
