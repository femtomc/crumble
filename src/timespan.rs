//! TimeSpan represents an arc of time within the pattern system.
//!
//! A TimeSpan has a begin and end time, both represented as Fractions.
//! It provides methods for cycle-based operations like splitting across
//! cycle boundaries and computing intersections.

use crate::fraction::Fraction;

/// A span of time with a begin and end point.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TimeSpan {
    pub begin: Fraction,
    pub end: Fraction,
}

impl TimeSpan {
    /// Create a new TimeSpan.
    pub fn new(begin: Fraction, end: Fraction) -> Self {
        TimeSpan { begin, end }
    }

    /// Create a TimeSpan from integer begin and end.
    pub fn from_integers(begin: i64, end: i64) -> Self {
        TimeSpan {
            begin: Fraction::from_integer(begin),
            end: Fraction::from_integer(end),
        }
    }

    /// Returns the duration of this timespan.
    pub fn duration(&self) -> Fraction {
        self.end - self.begin
    }

    /// Returns the midpoint of this timespan.
    pub fn midpoint(&self) -> Fraction {
        self.begin + self.duration() / Fraction::new(2, 1)
    }

    /// Split this timespan into a list of timespans, one per cycle.
    /// This is essential for the cycle-based evaluation model.
    pub fn span_cycles(&self) -> Vec<TimeSpan> {
        let mut spans = Vec::new();
        let mut begin = self.begin;
        let end = self.end;
        let end_sam = end.sam();

        // Support zero-width timespans
        if begin == end {
            return vec![TimeSpan::new(begin, end)];
        }

        while end > begin {
            // If begin and end are in the same cycle, we're done.
            if begin.sam() == end_sam {
                spans.push(TimeSpan::new(begin, self.end));
                break;
            }
            // Add a timespan up to the next sam
            let next_begin = begin.next_sam();
            spans.push(TimeSpan::new(begin, next_begin));

            // Continue with the next cycle
            begin = next_begin;
        }
        spans
    }

    /// Shifts this timespan to one of equal duration that starts within cycle zero.
    /// The output timespan probably does not start *at* Time 0 --
    /// that only happens when the input Arc starts at an integral Time.
    pub fn cycle_arc(&self) -> TimeSpan {
        let b = self.begin.cycle_pos();
        let e = b + self.duration();
        TimeSpan::new(b, e)
    }

    /// Apply a function to both the begin and end time.
    pub fn with_time<F>(&self, f: F) -> TimeSpan
    where
        F: Fn(Fraction) -> Fraction,
    {
        TimeSpan::new(f(self.begin), f(self.end))
    }

    /// Apply a function to just the end time.
    pub fn with_end<F>(&self, f: F) -> TimeSpan
    where
        F: Fn(Fraction) -> Fraction,
    {
        TimeSpan::new(self.begin, f(self.end))
    }

    /// Apply a function relative to the cycle (i.e., relative to the sam of the start).
    pub fn with_cycle<F>(&self, f: F) -> TimeSpan
    where
        F: Fn(Fraction) -> Fraction,
    {
        let sam = self.begin.sam();
        let b = sam + f(self.begin - sam);
        let e = sam + f(self.end - sam);
        TimeSpan::new(b, e)
    }

    /// Compute the intersection of two timespans, returns None if they don't intersect.
    pub fn intersection(&self, other: &TimeSpan) -> Option<TimeSpan> {
        let intersect_begin = self.begin.max(other.begin);
        let intersect_end = self.end.min(other.end);

        if intersect_begin > intersect_end {
            return None;
        }

        if intersect_begin == intersect_end {
            // Zero-width (point) intersection - doesn't intersect if it's at the end of a
            // non-zero-width timespan.
            if intersect_begin == self.end && self.begin < self.end {
                return None;
            }
            if intersect_begin == other.end && other.begin < other.end {
                return None;
            }
        }

        Some(TimeSpan::new(intersect_begin, intersect_end))
    }

    /// Like intersection, but panics if the timespans don't intersect.
    pub fn intersection_e(&self, other: &TimeSpan) -> TimeSpan {
        self.intersection(other)
            .expect("TimeSpans do not intersect")
    }

    /// Check if two timespans are equal.
    pub fn equals(&self, other: &TimeSpan) -> bool {
        self.begin == other.begin && self.end == other.end
    }

    /// Format for display.
    pub fn show(&self) -> String {
        format!("{} -> {}", self.begin, self.end)
    }
}

impl std::fmt::Display for TimeSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} -> {}", self.begin, self.end)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_span_cycles_single() {
        let span = TimeSpan::new(Fraction::new(0, 1), Fraction::new(1, 1));
        let cycles = span.span_cycles();
        assert_eq!(cycles.len(), 1);
        assert_eq!(cycles[0], span);
    }

    #[test]
    fn test_span_cycles_multiple() {
        let span = TimeSpan::new(Fraction::new(0, 1), Fraction::new(2, 1));
        let cycles = span.span_cycles();
        assert_eq!(cycles.len(), 2);
        assert_eq!(
            cycles[0],
            TimeSpan::new(Fraction::new(0, 1), Fraction::new(1, 1))
        );
        assert_eq!(
            cycles[1],
            TimeSpan::new(Fraction::new(1, 1), Fraction::new(2, 1))
        );
    }

    #[test]
    fn test_span_cycles_partial() {
        let span = TimeSpan::new(Fraction::new(1, 2), Fraction::new(3, 2));
        let cycles = span.span_cycles();
        assert_eq!(cycles.len(), 2);
        assert_eq!(
            cycles[0],
            TimeSpan::new(Fraction::new(1, 2), Fraction::new(1, 1))
        );
        assert_eq!(
            cycles[1],
            TimeSpan::new(Fraction::new(1, 1), Fraction::new(3, 2))
        );
    }

    #[test]
    fn test_intersection() {
        let a = TimeSpan::new(Fraction::new(0, 1), Fraction::new(1, 1));
        let b = TimeSpan::new(Fraction::new(1, 2), Fraction::new(3, 2));
        let intersection = a.intersection(&b);
        assert_eq!(
            intersection,
            Some(TimeSpan::new(Fraction::new(1, 2), Fraction::new(1, 1)))
        );
    }

    #[test]
    fn test_no_intersection() {
        let a = TimeSpan::new(Fraction::new(0, 1), Fraction::new(1, 2));
        let b = TimeSpan::new(Fraction::new(3, 4), Fraction::new(1, 1));
        assert_eq!(a.intersection(&b), None);
    }

    #[test]
    fn test_duration() {
        let span = TimeSpan::new(Fraction::new(1, 4), Fraction::new(3, 4));
        assert_eq!(span.duration(), Fraction::new(1, 2));
    }
}
