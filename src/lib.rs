//! # crumble
//!
//! A Rust implementation of the Strudel pattern language for algorithmic music.
//!
//! Strudel is a live coding environment that implements the TidalCycles pattern
//! language. This crate provides the core pattern engine in Rust.
//!
//! ## Core Concepts
//!
//! - **Pattern**: A function from time to events. Patterns can be transformed,
//!   combined, and queried to produce musical events.
//! - **Hap**: An event (happening) with a value active during a timespan.
//! - **Fraction**: Rational numbers for precise timing within cycles.
//! - **TimeSpan**: An arc of time with begin and end points.
//!
//! ## Example
//!
//! ```rust
//! use crumble::*;
//!
//! // Create a simple pattern
//! let pat = sequence(vec![pure(1), pure(2), pure(3), pure(4)]);
//!
//! // Query the first cycle
//! let events = pat.first_cycle();
//! assert_eq!(events.len(), 4);
//!
//! // Transform the pattern
//! let doubled = pat.fmap(|x| x * 2);
//! let fast_pat = doubled.fast(Fraction::from_integer(2));
//! ```
//!
//! ## Playing Patterns
//!
//! With the `audio` feature, you can play patterns directly:
//!
//! ```rust,ignore
//! use crumble::player::Player;
//! use crumble::pattern::{sequence, pure};
//!
//! let player = Player::with_audio().unwrap();
//! let pat = sequence(vec![pure("sine"), pure("saw"), pure("square")]);
//! player.play_cycles(pat, 4.0); // Play for 4 cycles
//! ```
//!
//! Or use the Lisp DSL:
//!
//! ```rust,ignore
//! player.play_lisp("(fast 2 (seq sine saw square))", 4.0);
//! ```

// Core modules
pub mod fraction;
pub mod hap;
pub mod lisp;
pub mod pattern;
pub mod state;
pub mod timespan;

// Playback modules (scheduler data types always available)
pub mod scheduler;

#[cfg(feature = "audio")]
pub mod audio;

#[cfg(feature = "osc")]
pub mod osc;

#[cfg(feature = "audio")]
pub mod player;

// WASM bindings
#[cfg(feature = "wasm")]
pub mod wasm;

// Re-export core types
pub use fraction::Fraction;
pub use hap::{Context, Hap, Location};
pub use pattern::{
    app_both, app_left, app_right, cat, cosine, euclid, euclid_rot, fastcat, gap, interleave,
    iota, pure, range, run, saw, sequence, silence, sine, slowcat_prime, square, stack, time,
    timecat, tri, Pattern,
};
pub use state::{ControlValue, State};
pub use timespan::TimeSpan;
pub use lisp::{run_lisp, Env, Expr, LispError, Token, Value};

// Re-export scheduler data types (always available)
pub use scheduler::{SchedulerConfig, ScheduledEvent};

// Re-export scheduler functions (native only)
#[cfg(not(target_arch = "wasm32"))]
pub use scheduler::{SchedulerHandle, start_scheduler, play_blocking};

// Re-export audio (when feature enabled)
#[cfg(feature = "audio")]
pub use audio::{AudioEngine, SoundParams, Waveform};

// Re-export player (when feature enabled)
#[cfg(feature = "audio")]
pub use player::{Player, PlayerConfig, OutputMode, PlayHandle};

// Re-export OSC (when feature enabled)
#[cfg(feature = "osc")]
pub use osc::{OscSender, DirtParams, SUPERDIRT_PORT};

/// Prelude module for convenient imports.
pub mod prelude {
    pub use crate::fraction::Fraction;
    pub use crate::hap::{Context, Hap};
    pub use crate::pattern::{
        app_both, app_left, app_right, cat, cosine, euclid, euclid_rot, fastcat, gap, interleave,
        iota, pure, range, run, saw, sequence, silence, sine, slowcat_prime, square, stack, time,
        timecat, tri, Pattern,
    };
    pub use crate::state::State;
    pub use crate::timespan::TimeSpan;
    pub use crate::lisp::{run_lisp, Value};
    pub use crate::scheduler::SchedulerConfig;

    #[cfg(not(target_arch = "wasm32"))]
    pub use crate::scheduler::play_blocking;

    #[cfg(feature = "audio")]
    pub use crate::player::{Player, PlayerConfig};
}
