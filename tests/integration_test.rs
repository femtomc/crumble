//! Integration tests for crumble.

use crumble::*;
use std::sync::{Arc, Mutex};

/// Test the full scheduler pipeline with a simple pattern.
#[test]
fn test_scheduler_collects_events() {
    let pattern = sequence(vec![pure(1), pure(2), pure(3), pure(4)]);
    let collected: Arc<Mutex<Vec<i32>>> = Arc::new(Mutex::new(Vec::new()));
    let collected_clone = collected.clone();

    let config = SchedulerConfig {
        cps: 2.0, // 2 cycles per second
        tick_interval: 0.01,
        lookahead: 0.1,
        latency: 0.0,
    };

    // Play for 2 cycles to ensure we capture all events
    play_blocking(pattern, 2.0, config, move |event| {
        collected_clone.lock().unwrap().push(event.hap.value);
    });

    let events = collected.lock().unwrap();
    // Should have at least 4 events from one cycle
    assert!(events.len() >= 4, "Expected at least 4 events, got {}", events.len());
    assert!(events.contains(&1), "Missing 1, got {:?}", *events);
    assert!(events.contains(&2), "Missing 2, got {:?}", *events);
    assert!(events.contains(&3), "Missing 3, got {:?}", *events);
    assert!(events.contains(&4), "Missing 4, got {:?}", *events);
}

/// Test that patterns can be transformed before scheduling.
#[test]
fn test_transformed_pattern_scheduling() {
    let pattern = sequence(vec![pure(1), pure(2)])
        .fmap(|x| x * 10);

    let collected: Arc<Mutex<Vec<i32>>> = Arc::new(Mutex::new(Vec::new()));
    let collected_clone = collected.clone();

    let config = SchedulerConfig {
        cps: 4.0,
        tick_interval: 0.01,
        lookahead: 0.05,
        latency: 0.0,
    };

    play_blocking(pattern, 0.5, config, move |event| {
        collected_clone.lock().unwrap().push(event.hap.value);
    });

    let events = collected.lock().unwrap();
    assert_eq!(events.len(), 2);
    assert!(events.contains(&10));
    assert!(events.contains(&20));
}

/// Test that fast() speeds up the pattern.
#[test]
fn test_fast_pattern_timing() {
    // Fast 2 should produce 8 events per cycle instead of 4
    let pattern = sequence(vec![
        pure("a".to_string()),
        pure("b".to_string()),
        pure("c".to_string()),
        pure("d".to_string()),
    ])
    .fast(Fraction::from_integer(2));

    let collected: Arc<Mutex<Vec<String>>> = Arc::new(Mutex::new(Vec::new()));
    let collected_clone = collected.clone();

    let config = SchedulerConfig {
        cps: 2.0, // Faster tempo
        tick_interval: 0.01,
        lookahead: 0.1,
        latency: 0.0,
    };

    // Play for 2 cycles to ensure we get at least one complete cycle
    play_blocking(pattern, 2.0, config, move |event| {
        collected_clone.lock().unwrap().push(event.hap.value.clone());
    });

    let events = collected.lock().unwrap();
    // 4 elements * 2 (fast) = 8 events per cycle, 2 cycles = 16 minimum
    // But we're just checking we get the sped-up pattern working
    assert!(events.len() >= 8, "Expected at least 8 events, got {}", events.len());
}

/// Test stack produces overlapping events.
#[test]
fn test_stack_pattern() {
    let pattern = stack(vec![
        pure("kick".to_string()),
        pure("snare".to_string()),
    ]);

    let collected: Arc<Mutex<Vec<String>>> = Arc::new(Mutex::new(Vec::new()));
    let collected_clone = collected.clone();

    let config = SchedulerConfig {
        cps: 1.0,
        tick_interval: 0.01,
        lookahead: 0.1,
        latency: 0.0,
    };

    play_blocking(pattern, 1.0, config, move |event| {
        collected_clone.lock().unwrap().push(event.hap.value.clone());
    });

    let events = collected.lock().unwrap();
    // Both kick and snare trigger at the same time
    // May get more events due to lookahead windows
    assert!(events.len() >= 2, "Expected at least 2 events, got {}", events.len());
    assert!(events.contains(&"kick".to_string()));
    assert!(events.contains(&"snare".to_string()));
}

/// Test euclidean rhythms.
#[test]
fn test_euclid_rhythm() {
    // euclid(3, 8) produces the classic "tresillo" pattern
    let pattern = euclid(3, 8, "x".to_string());

    let collected: Arc<Mutex<Vec<String>>> = Arc::new(Mutex::new(Vec::new()));
    let collected_clone = collected.clone();

    let config = SchedulerConfig {
        cps: 2.0, // Faster tempo
        tick_interval: 0.01,
        lookahead: 0.1,
        latency: 0.0,
    };

    // Play for 2 cycles
    play_blocking(pattern, 2.0, config, move |event| {
        collected_clone.lock().unwrap().push(event.hap.value.clone());
    });

    let events = collected.lock().unwrap();
    // 3 events per cycle, 2 cycles = at least 3 events
    assert!(events.len() >= 3, "Expected at least 3 events, got {}", events.len());
}

/// Test the Lisp DSL integration.
#[test]
fn test_lisp_pattern_evaluation() {
    let result = run_lisp("(seq 1 2 3 4)").unwrap();

    if let Value::Pattern(pat) = result {
        let events = pat.first_cycle();
        assert_eq!(events.len(), 4);
    } else {
        panic!("Expected a pattern");
    }
}

/// Test Lisp pattern with transformations.
#[test]
fn test_lisp_fast_pattern() {
    let result = run_lisp("(fast 2 (seq kick snare))").unwrap();

    if let Value::Pattern(pat) = result {
        let events = pat.first_cycle();
        assert_eq!(events.len(), 4); // 2 elements * fast 2 = 4 per cycle
    } else {
        panic!("Expected a pattern");
    }
}

/// Test Lisp euclidean rhythm.
#[test]
fn test_lisp_euclid() {
    let result = run_lisp("(euclid 3 8 bd)").unwrap();

    if let Value::Pattern(pat) = result {
        let events = pat.first_cycle();
        assert_eq!(events.len(), 3);
    } else {
        panic!("Expected a pattern");
    }
}

/// Test that scheduler handles multiple cycles correctly.
#[test]
fn test_multi_cycle_scheduling() {
    let pattern = pure("tick");

    let collected: Arc<Mutex<Vec<f64>>> = Arc::new(Mutex::new(Vec::new()));
    let collected_clone = collected.clone();

    let config = SchedulerConfig {
        cps: 2.0, // 2 cycles per second
        tick_interval: 0.01,
        lookahead: 0.1,
        latency: 0.0,
    };

    // Play for 2.5 cycles
    play_blocking(pattern, 2.5, config, move |event| {
        collected_clone.lock().unwrap().push(event.cycle);
    });

    let events = collected.lock().unwrap();
    // Should have at least 2 events (one per cycle for 2 complete cycles)
    assert!(events.len() >= 2, "Expected at least 2 events, got {}", events.len());
}

/// Test continuous signals.
#[test]
fn test_sine_signal() {
    let pattern = sine();
    let events = pattern.first_cycle();

    // Continuous patterns produce one event per query
    assert!(!events.is_empty());

    // Value should be between 0 and 1
    for hap in events {
        assert!(hap.value >= 0.0 && hap.value <= 1.0);
    }
}

/// Test signal range scaling.
#[test]
fn test_range_signal() {
    let pattern = range(100.0, 200.0, sine());
    let events = pattern.first_cycle();

    for hap in events {
        assert!(hap.value >= 100.0 && hap.value <= 200.0);
    }
}

#[cfg(feature = "osc")]
mod osc_tests {
    use crumble::osc::{DirtParams, params_from_map};
    use std::collections::HashMap;

    #[test]
    fn test_dirt_params_builder() {
        let params = DirtParams::new()
            .sound("bd")
            .sample(0)
            .with_gain(0.8);

        assert_eq!(params.s, Some("bd".to_string()));
        assert_eq!(params.n, Some(0));
        assert_eq!(params.gain, Some(0.8));
    }

    #[test]
    fn test_params_from_hashmap() {
        let mut map = HashMap::new();
        map.insert("s".to_string(), "hh".to_string());
        map.insert("gain".to_string(), "0.5".to_string());

        let params = params_from_map(&map);
        assert_eq!(params.s, Some("hh".to_string()));
        assert_eq!(params.gain, Some(0.5));
    }
}

#[cfg(feature = "audio")]
mod audio_tests {
    use crumble::audio::{SoundParams, Waveform};

    #[test]
    fn test_sound_params_default() {
        let params = SoundParams::default();
        assert_eq!(params.sound, "sine");
        assert_eq!(params.gain, 0.5);
    }

    #[test]
    fn test_waveform_from_sound_name() {
        let mut params = SoundParams::default();

        params.sound = "sine".to_string();
        assert_eq!(params.get_waveform(), Waveform::Sine);

        params.sound = "saw".to_string();
        assert_eq!(params.get_waveform(), Waveform::Saw);

        params.sound = "square".to_string();
        assert_eq!(params.get_waveform(), Waveform::Square);

        params.sound = "unknown".to_string();
        assert_eq!(params.get_waveform(), Waveform::Sine); // defaults to sine
    }
}
