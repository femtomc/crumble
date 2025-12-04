//! Pattern scheduler for real-time playback.
//!
//! The scheduler runs a background thread that queries patterns at regular
//! intervals and triggers events at the appropriate times.
//!
//! Note: The threading-based scheduler is only available on native platforms.
//! For WASM, scheduling should be handled in JavaScript using requestAnimationFrame
//! or Web Audio's timing system.

use crate::hap::Hap;

/// An event ready to be triggered, with absolute timing information.
#[derive(Debug, Clone)]
pub struct ScheduledEvent<T> {
    /// The original hap from the pattern.
    pub hap: Hap<T>,
    /// Absolute time when this event should trigger (seconds from start).
    pub trigger_time: f64,
    /// Duration of the event in seconds.
    pub duration: f64,
    /// Current cycles per second (tempo).
    pub cps: f64,
    /// The cycle number this event belongs to.
    pub cycle: f64,
}

/// Configuration for the scheduler.
#[derive(Debug, Clone)]
pub struct SchedulerConfig {
    /// Cycles per second (tempo). Default: 0.5 (one cycle every 2 seconds).
    pub cps: f64,
    /// How often to tick, in seconds. Default: 0.05 (50ms).
    pub tick_interval: f64,
    /// How far ahead to look for events, in seconds. Default: 0.1.
    pub lookahead: f64,
    /// Latency buffer before triggering, in seconds. Default: 0.1.
    pub latency: f64,
}

impl Default for SchedulerConfig {
    fn default() -> Self {
        SchedulerConfig {
            cps: 0.5,
            tick_interval: 0.05,
            lookahead: 0.1,
            latency: 0.1,
        }
    }
}

// Native-only scheduler implementation using threads and channels
#[cfg(not(target_arch = "wasm32"))]
mod native {
    use super::*;
    use crate::fraction::Fraction;
    use crate::pattern::Pattern;
    use crate::state::State;
    use crate::timespan::TimeSpan;
    use crossbeam_channel::{bounded, Receiver, Sender};
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::sync::Arc;
    use std::thread;
    use std::time::{Duration, Instant};

    /// Messages sent to the scheduler thread.
    enum SchedulerMessage<T: Clone + Send + 'static> {
        /// Set a new pattern to play.
        SetPattern(Pattern<T>),
        /// Update the tempo (cycles per second).
        SetCps(f64),
        /// Stop playback.
        Stop,
    }

    /// Handle to control a running scheduler.
    pub struct SchedulerHandle<T: Clone + Send + 'static> {
        sender: Sender<SchedulerMessage<T>>,
        running: Arc<AtomicBool>,
    }

    impl<T: Clone + Send + 'static> SchedulerHandle<T> {
        /// Set a new pattern to play.
        pub fn set_pattern(&self, pattern: Pattern<T>) {
            let _ = self.sender.send(SchedulerMessage::SetPattern(pattern));
        }

        /// Update the tempo.
        pub fn set_cps(&self, cps: f64) {
            let _ = self.sender.send(SchedulerMessage::SetCps(cps));
        }

        /// Stop the scheduler.
        pub fn stop(&self) {
            self.running.store(false, Ordering::SeqCst);
            let _ = self.sender.send(SchedulerMessage::Stop);
        }

        /// Check if the scheduler is still running.
        pub fn is_running(&self) -> bool {
            self.running.load(Ordering::SeqCst)
        }
    }

    /// Start a scheduler that sends events to the provided callback.
    ///
    /// Returns a handle to control the scheduler and a receiver for events.
    pub fn start_scheduler<T, F>(
        config: SchedulerConfig,
        initial_pattern: Pattern<T>,
        mut on_event: F,
    ) -> SchedulerHandle<T>
    where
        T: Clone + Send + Sync + 'static,
        F: FnMut(ScheduledEvent<T>) + Send + 'static,
    {
        let (tx, rx) = bounded::<SchedulerMessage<T>>(64);
        let running = Arc::new(AtomicBool::new(true));
        let running_clone = running.clone();

        thread::spawn(move || {
            scheduler_loop(config, initial_pattern, rx, running_clone, &mut on_event);
        });

        SchedulerHandle { sender: tx, running }
    }

    /// Start a scheduler that sends events to a channel.
    ///
    /// Returns a handle and a receiver to consume events.
    pub fn start_scheduler_with_channel<T>(
        config: SchedulerConfig,
        initial_pattern: Pattern<T>,
    ) -> (SchedulerHandle<T>, Receiver<ScheduledEvent<T>>)
    where
        T: Clone + Send + Sync + 'static,
    {
        let (event_tx, event_rx) = bounded::<ScheduledEvent<T>>(256);
        let (msg_tx, msg_rx) = bounded::<SchedulerMessage<T>>(64);
        let running = Arc::new(AtomicBool::new(true));
        let running_clone = running.clone();

        thread::spawn(move || {
            scheduler_loop(
                config,
                initial_pattern,
                msg_rx,
                running_clone,
                &mut |event| {
                    let _ = event_tx.send(event);
                },
            );
        });

        let handle = SchedulerHandle {
            sender: msg_tx,
            running,
        };

        (handle, event_rx)
    }

    fn scheduler_loop<T, F>(
        mut config: SchedulerConfig,
        mut pattern: Pattern<T>,
        rx: Receiver<SchedulerMessage<T>>,
        running: Arc<AtomicBool>,
        on_event: &mut F,
    ) where
        T: Clone + Send + Sync + 'static,
        F: FnMut(ScheduledEvent<T>),
    {
        let start_time = Instant::now();
        let mut last_tick_end = 0.0_f64; // End of last queried range in cycles

        while running.load(Ordering::SeqCst) {
            // Check for messages (non-blocking)
            while let Ok(msg) = rx.try_recv() {
                match msg {
                    SchedulerMessage::SetPattern(p) => pattern = p,
                    SchedulerMessage::SetCps(cps) => config.cps = cps,
                    SchedulerMessage::Stop => {
                        running.store(false, Ordering::SeqCst);
                        return;
                    }
                }
            }

            // Calculate current time
            let now = start_time.elapsed().as_secs_f64();

            // Query window: from last_tick_end to now + lookahead (in cycles)
            let query_end = (now + config.lookahead) * config.cps;

            if query_end > last_tick_end {
                let query_begin = last_tick_end;

                // Query the pattern
                let span = TimeSpan::new(
                    Fraction::from(query_begin),
                    Fraction::from(query_end),
                );
                let state = State::new(span);
                let haps = pattern.query(&state);

                // Process each hap
                for hap in haps {
                    // Only trigger events with an onset in this window
                    if hap.has_onset() {
                        let onset_cycles = hap.part.begin.to_f64();

                        // Check if onset is in our query window
                        if onset_cycles >= query_begin && onset_cycles < query_end {
                            // Calculate absolute trigger time
                            let trigger_time = onset_cycles / config.cps + config.latency;

                            // Calculate duration
                            let duration_cycles = if let Some(ref whole) = hap.whole {
                                (whole.end - whole.begin).to_f64()
                            } else {
                                (hap.part.end - hap.part.begin).to_f64()
                            };
                            let duration = duration_cycles / config.cps;

                            let event = ScheduledEvent {
                                hap,
                                trigger_time,
                                duration,
                                cps: config.cps,
                                cycle: onset_cycles,
                            };

                            on_event(event);
                        }
                    }
                }

                last_tick_end = query_end;
            }

            // Sleep until next tick
            thread::sleep(Duration::from_secs_f64(config.tick_interval));
        }
    }

    /// A simple blocking player that plays a pattern for a given number of cycles.
    pub fn play_blocking<T, F>(
        pattern: Pattern<T>,
        cycles: f64,
        config: SchedulerConfig,
        mut on_event: F,
    ) where
        T: Clone + Send + Sync + 'static,
        F: FnMut(ScheduledEvent<T>) + Send + 'static,
    {
        let duration_secs = cycles / config.cps;
        let start = Instant::now();

        let handle = start_scheduler(config, pattern, move |event| {
            on_event(event);
        });

        // Wait for the duration
        while start.elapsed().as_secs_f64() < duration_secs {
            thread::sleep(Duration::from_millis(10));
        }

        handle.stop();
    }
}

// Re-export native scheduler functions
#[cfg(not(target_arch = "wasm32"))]
pub use native::{play_blocking, start_scheduler, start_scheduler_with_channel, SchedulerHandle};

#[cfg(all(test, not(target_arch = "wasm32")))]
mod tests {
    use super::*;
    use crate::pattern::{pure, sequence};
    use std::sync::{Arc, Mutex};

    #[test]
    fn test_scheduler_basic() {
        let events: Arc<Mutex<Vec<i32>>> = Arc::new(Mutex::new(Vec::new()));
        let events_clone = events.clone();

        let pat = sequence(vec![pure(1), pure(2), pure(3), pure(4)]);

        let config = SchedulerConfig {
            cps: 2.0, // Two cycles per second (faster to ensure complete cycles)
            tick_interval: 0.01,
            lookahead: 0.1,
            latency: 0.0,
        };

        // Play for 2 full cycles to ensure we capture at least one complete cycle
        play_blocking(pat, 2.0, config, move |event| {
            events_clone.lock().unwrap().push(event.hap.value);
        });

        let collected = events.lock().unwrap();
        // Should have at least 4 events from one complete cycle
        assert!(collected.len() >= 4, "Expected at least 4 events, got {}", collected.len());
        // Check that we got all distinct values at least once
        assert!(collected.contains(&1), "Missing value 1");
        assert!(collected.contains(&2), "Missing value 2");
        assert!(collected.contains(&3), "Missing value 3");
        assert!(collected.contains(&4), "Missing value 4");
    }

    #[test]
    fn test_scheduler_with_channel() {
        use std::thread;
        use std::time::Duration;

        let pat = sequence(vec![pure("a"), pure("b")]);

        let config = SchedulerConfig {
            cps: 4.0,
            tick_interval: 0.01,
            lookahead: 0.05,
            latency: 0.0,
        };

        let (handle, rx) = start_scheduler_with_channel(config, pat);

        // Collect events for half a cycle
        thread::sleep(Duration::from_millis(150));
        handle.stop();

        let mut events = Vec::new();
        while let Ok(event) = rx.try_recv() {
            events.push(event.hap.value);
        }

        assert!(!events.is_empty());
    }
}
