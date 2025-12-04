//! State represents the query state passed to patterns.
//!
//! When querying a pattern, we pass a State containing the timespan
//! we're interested in, along with any control values.

use crate::timespan::TimeSpan;
use std::collections::HashMap;

/// The query state passed to patterns.
#[derive(Debug, Clone)]
pub struct State {
    /// The timespan being queried.
    pub span: TimeSpan,
    /// Control values that can modify pattern behavior.
    pub controls: HashMap<String, ControlValue>,
}

/// A control value that can be stored in the state.
#[derive(Debug, Clone, PartialEq)]
pub enum ControlValue {
    Float(f64),
    Int(i64),
    String(String),
    Bool(bool),
}

impl State {
    /// Create a new State with the given timespan.
    pub fn new(span: TimeSpan) -> Self {
        State {
            span,
            controls: HashMap::new(),
        }
    }

    /// Create a new State with controls.
    pub fn with_controls(span: TimeSpan, controls: HashMap<String, ControlValue>) -> Self {
        State { span, controls }
    }

    /// Returns a new State with a different span.
    pub fn set_span(&self, span: TimeSpan) -> Self {
        State {
            span,
            controls: self.controls.clone(),
        }
    }

    /// Apply a function to the span.
    pub fn with_span<F>(&self, f: F) -> Self
    where
        F: FnOnce(TimeSpan) -> TimeSpan,
    {
        self.set_span(f(self.span))
    }

    /// Returns a new State with added controls.
    pub fn set_controls(&self, controls: HashMap<String, ControlValue>) -> Self {
        let mut new_controls = self.controls.clone();
        new_controls.extend(controls);
        State {
            span: self.span,
            controls: new_controls,
        }
    }

    /// Add a single control value.
    pub fn add_control(&self, key: &str, value: ControlValue) -> Self {
        let mut new_controls = self.controls.clone();
        new_controls.insert(key.to_string(), value);
        State {
            span: self.span,
            controls: new_controls,
        }
    }

    /// Get a control value.
    pub fn get_control(&self, key: &str) -> Option<&ControlValue> {
        self.controls.get(key)
    }
}

impl From<TimeSpan> for State {
    fn from(span: TimeSpan) -> Self {
        State::new(span)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fraction::Fraction;

    #[test]
    fn test_state_creation() {
        let span = TimeSpan::new(Fraction::new(0, 1), Fraction::new(1, 1));
        let state = State::new(span);
        assert_eq!(state.span, span);
        assert!(state.controls.is_empty());
    }

    #[test]
    fn test_state_with_controls() {
        let span = TimeSpan::new(Fraction::new(0, 1), Fraction::new(1, 1));
        let state = State::new(span).add_control("gain", ControlValue::Float(0.5));
        assert_eq!(
            state.get_control("gain"),
            Some(&ControlValue::Float(0.5))
        );
    }
}
