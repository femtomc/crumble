//! Hap represents an event in a pattern.
//!
//! A "Hap" (happening) is an event with a value active during a timespan.
//! It has both a "whole" timespan (the full duration of the event) and a
//! "part" timespan (the portion currently being queried).
//!
//! The name "Hap" is used instead of "Event" because Event is a reserved
//! concept in many environments (like JavaScript).

use crate::fraction::Fraction;
use crate::timespan::TimeSpan;
use std::collections::HashMap;

/// Context information attached to a Hap.
/// This can include source locations for debugging, tags, etc.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Context {
    /// Source code locations that caused this event.
    pub locations: Vec<Location>,
    /// Tags for categorizing events.
    pub tags: Vec<String>,
    /// Additional metadata.
    pub meta: HashMap<String, String>,
}

/// A source code location.
#[derive(Debug, Clone, PartialEq)]
pub struct Location {
    pub start: usize,
    pub end: usize,
}

impl Context {
    /// Create an empty context.
    pub fn new() -> Self {
        Context::default()
    }

    /// Combine this context with another, merging locations.
    pub fn combine(&self, other: &Context) -> Context {
        let mut locations = self.locations.clone();
        locations.extend(other.locations.clone());

        let mut tags = self.tags.clone();
        for tag in &other.tags {
            if !tags.contains(tag) {
                tags.push(tag.clone());
            }
        }

        let mut meta = self.meta.clone();
        meta.extend(other.meta.clone());

        Context {
            locations,
            tags,
            meta,
        }
    }
}

/// A Hap represents a value active during a timespan.
///
/// - `whole`: The full timespan of the event. This might be `None` for
///   continuous (signal-like) patterns.
/// - `part`: The portion of the event that's currently being queried.
///   This must never extend outside of the `whole`.
/// - `value`: The actual value of the event.
/// - `context`: Metadata about the event's origin.
#[derive(Debug, Clone)]
pub struct Hap<T> {
    /// The full timespan of this event. None for continuous patterns.
    pub whole: Option<TimeSpan>,
    /// The portion of the event being queried.
    pub part: TimeSpan,
    /// The value of this event.
    pub value: T,
    /// Context information (source locations, etc.)
    pub context: Context,
}

impl<T> Hap<T> {
    /// Create a new Hap with a whole timespan.
    pub fn new(whole: Option<TimeSpan>, part: TimeSpan, value: T) -> Self {
        Hap {
            whole,
            part,
            value,
            context: Context::new(),
        }
    }

    /// Create a new Hap with context.
    pub fn with_context(
        whole: Option<TimeSpan>,
        part: TimeSpan,
        value: T,
        context: Context,
    ) -> Self {
        Hap {
            whole,
            part,
            value,
            context,
        }
    }

    /// Returns true if this hap has an onset (the beginning of the part
    /// is the same as the beginning of the whole).
    pub fn has_onset(&self) -> bool {
        match &self.whole {
            Some(w) => w.begin == self.part.begin,
            None => false,
        }
    }

    /// Returns the whole if it exists, otherwise the part.
    pub fn whole_or_part(&self) -> TimeSpan {
        self.whole.unwrap_or(self.part)
    }

    /// Apply a function to the timespan(s) of this hap.
    pub fn with_span<F>(self, f: F) -> Self
    where
        F: Fn(TimeSpan) -> TimeSpan,
    {
        Hap {
            whole: self.whole.map(&f),
            part: f(self.part),
            value: self.value,
            context: self.context,
        }
    }

    /// Apply a function to the value of this hap.
    pub fn with_value<U, F>(self, f: F) -> Hap<U>
    where
        F: FnOnce(T) -> U,
    {
        Hap {
            whole: self.whole,
            part: self.part,
            value: f(self.value),
            context: self.context,
        }
    }

    /// Map over the value of this hap (alias for with_value).
    pub fn fmap<U, F>(self, f: F) -> Hap<U>
    where
        F: FnOnce(T) -> U,
    {
        self.with_value(f)
    }

    /// Set the context of this hap.
    pub fn set_context(self, context: Context) -> Self {
        Hap {
            whole: self.whole,
            part: self.part,
            value: self.value,
            context,
        }
    }

    /// Combine this hap's context with another hap's context.
    pub fn combine_context<U>(&self, other: &Hap<U>) -> Context {
        self.context.combine(&other.context)
    }

    /// Check if this hap has a specific tag.
    pub fn has_tag(&self, tag: &str) -> bool {
        self.context.tags.contains(&tag.to_string())
    }

    /// Get the duration of this hap (based on whole if available).
    pub fn duration(&self) -> Fraction {
        match &self.whole {
            Some(w) => w.end - w.begin,
            None => self.part.end - self.part.begin,
        }
    }
}

impl<T: Clone> Hap<T> {
    /// Check if two haps have the same timespan.
    pub fn span_equals(&self, other: &Hap<T>) -> bool {
        match (&self.whole, &other.whole) {
            (None, None) => true,
            (Some(a), Some(b)) => a.equals(b),
            _ => false,
        }
    }
}

impl<T: PartialEq + Clone> Hap<T> {
    /// Check if two haps are equal (same span and value).
    pub fn equals(&self, other: &Hap<T>) -> bool {
        self.span_equals(other) && self.part.equals(&other.part) && self.value == other.value
    }
}

impl<T: std::fmt::Display> Hap<T> {
    /// Format this hap for display.
    pub fn show(&self) -> String {
        let spans = match &self.whole {
            None => format!("~{}", self.part),
            Some(whole) => {
                let is_whole = whole.begin == self.part.begin && whole.end == self.part.end;
                if is_whole {
                    self.part.show()
                } else {
                    format!("({}) in {}", self.part.show(), whole.show())
                }
            }
        };
        format!("[ {} | {} ]", spans, self.value)
    }
}

impl<T: std::fmt::Display> std::fmt::Display for Hap<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.show())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_has_onset() {
        let whole = TimeSpan::new(Fraction::new(0, 1), Fraction::new(1, 1));
        let part = TimeSpan::new(Fraction::new(0, 1), Fraction::new(1, 2));
        let hap: Hap<i32> = Hap::new(Some(whole), part, 42);
        assert!(hap.has_onset());

        let part2 = TimeSpan::new(Fraction::new(1, 2), Fraction::new(1, 1));
        let hap2: Hap<i32> = Hap::new(Some(whole), part2, 42);
        assert!(!hap2.has_onset());
    }

    #[test]
    fn test_with_value() {
        let whole = TimeSpan::new(Fraction::new(0, 1), Fraction::new(1, 1));
        let hap: Hap<i32> = Hap::new(Some(whole), whole, 42);
        let mapped = hap.with_value(|v| v * 2);
        assert_eq!(mapped.value, 84);
    }

    #[test]
    fn test_duration() {
        let whole = TimeSpan::new(Fraction::new(0, 1), Fraction::new(1, 2));
        let hap: Hap<i32> = Hap::new(Some(whole), whole, 42);
        assert_eq!(hap.duration(), Fraction::new(1, 2));
    }
}
