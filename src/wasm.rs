//! WebAssembly bindings for crumble.
//!
//! This module provides JavaScript-friendly APIs for the pattern engine,
//! allowing crumble to be used in web browsers.

use wasm_bindgen::prelude::*;

use crate::fraction::Fraction;
use crate::lisp::{get_widget_registry, run_lisp_with_locations as eval_lisp, Value};
use crate::pattern::{self, Pattern};
use crate::state::State;
use crate::timespan::TimeSpan;

/// Initialize panic hook for better error messages in the browser console.
#[wasm_bindgen(start)]
pub fn init() {
    console_error_panic_hook::set_once();
}

/// A pattern handle that can be manipulated from JavaScript.
#[wasm_bindgen]
pub struct JsPattern {
    inner: Pattern<Value>,
}

#[wasm_bindgen]
impl JsPattern {
    /// Query the pattern for events in a time range.
    /// Returns an array of event objects.
    #[wasm_bindgen(js_name = query)]
    pub fn query(&self, start: f64, end: f64) -> Result<JsValue, JsError> {
        let span = TimeSpan::new(Fraction::from(start), Fraction::from(end));
        let state = State::new(span);
        let haps = self.inner.query(&state);

        let js_haps: Vec<serde_json::Value> = haps
            .into_iter()
            .map(|hap| {
                let value = value_to_json(&hap.value);
                // Convert context.meta to JSON object
                let meta: serde_json::Map<String, serde_json::Value> = hap
                    .context
                    .meta
                    .iter()
                    .map(|(k, v)| (k.clone(), serde_json::json!(v)))
                    .collect();
                serde_json::json!({
                    "start": hap.part.begin.to_f64(),
                    "end": hap.part.end.to_f64(),
                    "whole_start": hap.whole.as_ref().map(|w| w.begin.to_f64()),
                    "whole_end": hap.whole.as_ref().map(|w| w.end.to_f64()),
                    "value": value,
                    "has_onset": hap.has_onset(),
                    "meta": meta,
                    "tags": hap.context.tags,
                })
            })
            .collect();

        serde_wasm_bindgen::to_value(&js_haps).map_err(|e| JsError::new(&e.to_string()))
    }

    /// Query the first cycle (0 to 1).
    #[wasm_bindgen(js_name = firstCycle)]
    pub fn first_cycle(&self) -> Result<JsValue, JsError> {
        self.query(0.0, 1.0)
    }

    /// Get all unique source locations from the pattern.
    /// This queries a few cycles to collect locations for highlighting setup.
    #[wasm_bindgen(js_name = getAllLocations)]
    pub fn get_all_locations(&self) -> Result<JsValue, JsError> {
        // Query a few cycles to collect all unique locations
        let span = TimeSpan::new(Fraction::from(0.0), Fraction::from(4.0));
        let state = State::new(span);
        let haps = self.inner.query(&state);

        // Collect unique locations
        let mut seen = std::collections::HashSet::new();
        let mut locations: Vec<serde_json::Value> = Vec::new();

        for hap in haps {
            for loc in &hap.context.locations {
                let key = (loc.start, loc.end);
                if !seen.contains(&key) {
                    seen.insert(key);
                    locations.push(serde_json::json!({
                        "start": loc.start,
                        "end": loc.end,
                    }));
                }
            }
        }

        serde_wasm_bindgen::to_value(&locations).map_err(|e| JsError::new(&e.to_string()))
    }

    /// Query events with onsets only in a time range.
    #[wasm_bindgen(js_name = queryOnsets)]
    pub fn query_onsets(&self, start: f64, end: f64) -> Result<JsValue, JsError> {
        let span = TimeSpan::new(Fraction::from(start), Fraction::from(end));
        let state = State::new(span);
        let haps = self.inner.query(&state);

        let js_haps: Vec<serde_json::Value> = haps
            .into_iter()
            .filter(|hap| hap.has_onset())
            .map(|hap| {
                let value = value_to_json(&hap.value);
                // Convert context.meta to JSON object for effects
                let meta: serde_json::Map<String, serde_json::Value> = hap
                    .context
                    .meta
                    .iter()
                    .map(|(k, v)| (k.clone(), serde_json::json!(v)))
                    .collect();
                // Convert context.locations to JSON array for highlighting
                let locations: Vec<serde_json::Value> = hap
                    .context
                    .locations
                    .iter()
                    .map(|loc| serde_json::json!({
                        "start": loc.start,
                        "end": loc.end,
                    }))
                    .collect();
                serde_json::json!({
                    "start": hap.part.begin.to_f64(),
                    "end": hap.part.end.to_f64(),
                    "whole_start": hap.whole.as_ref().map(|w| w.begin.to_f64()),
                    "whole_end": hap.whole.as_ref().map(|w| w.end.to_f64()),
                    "value": value,
                    "meta": meta,
                    "locations": locations,
                })
            })
            .collect();

        serde_wasm_bindgen::to_value(&js_haps).map_err(|e| JsError::new(&e.to_string()))
    }

    /// Speed up the pattern by factor n.
    #[wasm_bindgen]
    pub fn fast(&self, n: f64) -> JsPattern {
        JsPattern {
            inner: self.inner.clone().fast(Fraction::from(n)),
        }
    }

    /// Slow down the pattern by factor n.
    #[wasm_bindgen]
    pub fn slow(&self, n: f64) -> JsPattern {
        JsPattern {
            inner: self.inner.clone().slow(Fraction::from(n)),
        }
    }

    /// Shift the pattern earlier in time.
    #[wasm_bindgen]
    pub fn early(&self, t: f64) -> JsPattern {
        JsPattern {
            inner: self.inner.clone().early(Fraction::from(t)),
        }
    }

    /// Shift the pattern later in time.
    #[wasm_bindgen]
    pub fn late(&self, t: f64) -> JsPattern {
        JsPattern {
            inner: self.inner.clone().late(Fraction::from(t)),
        }
    }

    /// Reverse the pattern within each cycle.
    #[wasm_bindgen]
    pub fn rev(&self) -> JsPattern {
        JsPattern {
            inner: self.inner.clone().rev(),
        }
    }

    /// Repeat each event n times.
    #[wasm_bindgen]
    pub fn ply(&self, n: i64) -> JsPattern {
        JsPattern {
            inner: self.inner.clone().ply(n),
        }
    }

    /// Create a palindrome of the pattern.
    #[wasm_bindgen]
    pub fn palindrome(&self) -> JsPattern {
        JsPattern {
            inner: self.inner.clone().palindrome(),
        }
    }
}

/// Evaluate a Lisp expression and return a pattern.
#[wasm_bindgen(js_name = evalLisp)]
pub fn eval_lisp_js(code: &str) -> Result<JsPattern, JsError> {
    match eval_lisp(code) {
        Ok(Value::Pattern(pat)) => Ok(JsPattern { inner: pat }),
        Ok(_) => Err(JsError::new("Expression did not evaluate to a pattern")),
        Err(e) => Err(JsError::new(&format!("Lisp error: {}", e))),
    }
}

/// Get registered widgets after evaluation.
/// Returns an array of widget configs with type, id, start, and end positions.
#[wasm_bindgen(js_name = getWidgets)]
pub fn get_widgets_js() -> Result<JsValue, JsError> {
    let widgets = get_widget_registry();
    let js_widgets: Vec<serde_json::Value> = widgets
        .into_iter()
        .map(|w| {
            serde_json::json!({
                "type": w.widget_type,
                "id": w.id,
                "start": w.start,
                "end": w.end,
            })
        })
        .collect();
    serde_wasm_bindgen::to_value(&js_widgets).map_err(|e| JsError::new(&e.to_string()))
}

/// Create a pattern from a single value (string).
#[wasm_bindgen(js_name = pure)]
pub fn pure_js(value: &str) -> JsPattern {
    JsPattern {
        inner: pattern::pure(Value::String(value.to_string())),
    }
}

/// Create a pattern from a number.
#[wasm_bindgen(js_name = pureNum)]
pub fn pure_num(value: f64) -> JsPattern {
    JsPattern {
        inner: pattern::pure(Value::Float(value)),
    }
}

/// Create a sequence pattern from an array of strings.
#[wasm_bindgen(js_name = seq)]
pub fn seq_js(values: Vec<JsValue>) -> Result<JsPattern, JsError> {
    let patterns: Vec<Pattern<Value>> = values
        .into_iter()
        .map(|v| {
            if let Some(s) = v.as_string() {
                pattern::pure(Value::String(s))
            } else if let Some(n) = v.as_f64() {
                pattern::pure(Value::Float(n))
            } else {
                pattern::pure(Value::String("?".to_string()))
            }
        })
        .collect();

    Ok(JsPattern {
        inner: pattern::sequence(patterns),
    })
}

/// Create a stack (simultaneous) pattern from an array of patterns.
#[wasm_bindgen(js_name = stack)]
pub fn stack_js(patterns: Vec<JsPattern>) -> JsPattern {
    let inner_patterns: Vec<Pattern<Value>> = patterns.into_iter().map(|p| p.inner).collect();
    JsPattern {
        inner: pattern::stack(inner_patterns),
    }
}

/// Create a Euclidean rhythm pattern.
#[wasm_bindgen(js_name = euclid)]
pub fn euclid_js(pulses: i64, steps: i64, value: &str) -> JsPattern {
    JsPattern {
        inner: pattern::euclid(pulses, steps, Value::String(value.to_string())),
    }
}

/// Create a sine wave pattern (0 to 1).
#[wasm_bindgen(js_name = sine)]
pub fn sine_js() -> JsPattern {
    JsPattern {
        inner: pattern::sine().fmap(|v| Value::Float(v)),
    }
}

/// Create a saw wave pattern (0 to 1).
#[wasm_bindgen(js_name = saw)]
pub fn saw_js() -> JsPattern {
    JsPattern {
        inner: pattern::saw().fmap(|v| Value::Float(v)),
    }
}

/// Create a triangle wave pattern (0 to 1).
#[wasm_bindgen(js_name = tri)]
pub fn tri_js() -> JsPattern {
    JsPattern {
        inner: pattern::tri().fmap(|v| Value::Float(v)),
    }
}

/// Create a square wave pattern (0 or 1).
#[wasm_bindgen(js_name = square)]
pub fn square_js() -> JsPattern {
    JsPattern {
        inner: pattern::square().fmap(|v| Value::Float(v)),
    }
}

/// Create a silence pattern.
#[wasm_bindgen(js_name = silence)]
pub fn silence_js() -> JsPattern {
    JsPattern {
        inner: pattern::silence(),
    }
}

/// Helper to convert Value to JSON.
fn value_to_json(value: &Value) -> serde_json::Value {
    match value {
        Value::Integer(n) => serde_json::json!(n),
        Value::Float(f) => serde_json::json!(f),
        Value::String(s) => serde_json::json!(s),
        Value::Fraction(f) => serde_json::json!(f.to_f64()),
        Value::Pattern(_) => serde_json::json!("<pattern>"),
        Value::Function(name) => serde_json::json!(format!("<function:{}>", name)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pure_pattern() {
        let pat = pure_js("kick");
        // Just verify it doesn't panic
        let _ = pat.first_cycle();
    }

    #[test]
    fn test_eval_lisp() {
        let pat = eval_lisp_js("(seq kick snare)").unwrap();
        let _ = pat.first_cycle();
    }
}
