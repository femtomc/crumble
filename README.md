# crumble

A Rust implementation of the [Strudel](https://strudel.cc) pattern language for algorithmic music.

Strudel is a live coding environment that implements the TidalCycles pattern language. This crate provides the core pattern engine in Rust.

## Core Concepts

- **Pattern**: A function from time to events. Patterns can be transformed, combined, and queried to produce musical events.
- **Hap**: An event (happening) with a value active during a timespan.
- **Fraction**: Rational numbers for precise timing within cycles.
- **TimeSpan**: An arc of time with begin and end points.

## Usage

```rust
use crumble::prelude::*;

// Create a simple sequence
let pat = sequence(vec![pure(1), pure(2), pure(3), pure(4)]);

// Query the first cycle
let events = pat.first_cycle();
assert_eq!(events.len(), 4);

// Transform patterns
let fast_pat = pat.fast(Fraction::from_integer(2));  // Play twice as fast
let slow_pat = pat.slow(Fraction::from_integer(2));  // Play half as fast

// Stack patterns (play simultaneously)
let stacked = stack(vec![
    pure("kick"),
    sequence(vec![pure("hihat"), pure("hihat")]),
]);

// Euclidean rhythms
let rhythm = euclid(3, 8, "snare");  // 3 hits over 8 steps

// Signal patterns
let lfo = sine();  // 0-1 sine wave over each cycle
let scaled = range(200.0, 800.0, lfo);  // Scale to 200-800
```

## Pattern Constructors

- `pure(value)` - A single value that repeats once per cycle
- `silence()` - Empty pattern (no events)
- `sequence(pats)` / `fastcat(pats)` - Concatenate patterns within one cycle
- `cat(pats)` / `slowcat(pats)` - Concatenate patterns, one per cycle
- `stack(pats)` - Play patterns simultaneously
- `euclid(pulses, steps, value)` - Euclidean rhythm generator

## Pattern Transformations

- `.fast(n)` - Speed up by factor n
- `.slow(n)` - Slow down by factor n
- `.early(t)` - Shift earlier in time
- `.late(t)` - Shift later in time
- `.rev()` - Reverse within each cycle
- `.fmap(f)` - Apply function to values
- `.every(n, f)` - Apply transformation every n cycles
- `.ply(n)` - Repeat each event n times
- `.superimpose(f)` - Layer transformation on top

## Signal Patterns

Continuous patterns for modulation:

- `saw()` - Sawtooth wave (0 to 1)
- `sine()` - Sine wave (0 to 1)
- `cosine()` - Cosine wave (0 to 1)
- `tri()` - Triangle wave (0 to 1)
- `square()` - Square wave (0 or 1)
- `range(min, max, pat)` - Scale signal to range

## License

AGPL-3.0 (same as Strudel)
