//! A Lisp-like DSL for constructing patterns.
//!
//! This module provides a simple S-expression based language for building
//! patterns in a more expressive, composable way.
//!
//! ## Syntax
//!
//! ```text
//! ; Literals
//! 42              ; integer
//! 3.14            ; float
//! "hello"         ; string
//! foo             ; symbol (becomes string value)
//!
//! ; Pattern constructors
//! (pure 42)                    ; single value pattern
//! (silence)                    ; empty pattern
//! (seq 1 2 3 4)                ; sequence (fastcat)
//! (cat a b c)                  ; slowcat
//! (stack kick snare hihat)     ; simultaneous patterns
//! (euclid 3 8 snare)           ; euclidean rhythm
//! (run 4)                      ; 0 1 2 3
//! (iota 4)                     ; 1 2 3 4
//!
//! ; Transformations
//! (fast 2 (seq 1 2 3))         ; speed up
//! (slow 2 (pure x))            ; slow down
//! (rev (seq a b c d))          ; reverse
//! (early 0.25 pat)             ; shift earlier
//! (late 0.5 pat)               ; shift later
//! (ply 2 (seq a b))            ; repeat each event
//! (palindrome (seq a b c))     ; forward then backward
//!
//! ; Signals
//! (sine)                       ; sine wave 0-1
//! (saw)                        ; sawtooth 0-1
//! (tri)                        ; triangle 0-1
//! (square)                     ; square wave 0/1
//! (range 200 800 (sine))       ; scale signal to range
//!
//! ; Higher-order
//! (every 3 rev (seq a b c d))  ; apply rev every 3rd cycle
//! (superimpose rev pat)        ; layer reversed on top
//! ```

use crate::fraction::Fraction;
use crate::pattern::*;
use std::collections::HashMap;
use std::fmt;

/// A token from the lexer.
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    LParen,
    RParen,
    LBracket,  // [ for stack sugar
    RBracket,  // ]
    Comma,     // , separator in brackets
    Integer(i64),
    Float(f64),
    String(String),
    Symbol(String),
}

/// Tokenize a Lisp expression.
pub fn tokenize(input: &str) -> Result<Vec<Token>, LispError> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(&c) = chars.peek() {
        match c {
            ' ' | '\t' | '\n' | '\r' => {
                chars.next();
            }
            ';' => {
                // Comment - skip to end of line
                while let Some(&c) = chars.peek() {
                    chars.next();
                    if c == '\n' {
                        break;
                    }
                }
            }
            '(' => {
                tokens.push(Token::LParen);
                chars.next();
            }
            ')' => {
                tokens.push(Token::RParen);
                chars.next();
            }
            '[' => {
                tokens.push(Token::LBracket);
                chars.next();
            }
            ']' => {
                tokens.push(Token::RBracket);
                chars.next();
            }
            ',' => {
                tokens.push(Token::Comma);
                chars.next();
            }
            '"' => {
                chars.next(); // consume opening quote
                let mut s = String::new();
                while let Some(&c) = chars.peek() {
                    chars.next();
                    if c == '"' {
                        break;
                    }
                    if c == '\\' {
                        if let Some(&escaped) = chars.peek() {
                            chars.next();
                            match escaped {
                                'n' => s.push('\n'),
                                't' => s.push('\t'),
                                '\\' => s.push('\\'),
                                '"' => s.push('"'),
                                _ => s.push(escaped),
                            }
                        }
                    } else {
                        s.push(c);
                    }
                }
                tokens.push(Token::String(s));
            }
            _ if c.is_ascii_digit() || (c == '-' && chars.clone().nth(1).map_or(false, |n| n.is_ascii_digit())) => {
                let mut num = String::new();
                if c == '-' {
                    num.push(c);
                    chars.next();
                }
                while let Some(&c) = chars.peek() {
                    if c.is_ascii_digit() || c == '.' {
                        num.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                if num.contains('.') {
                    tokens.push(Token::Float(num.parse().map_err(|_| {
                        LispError::ParseError(format!("Invalid float: {}", num))
                    })?));
                } else {
                    tokens.push(Token::Integer(num.parse().map_err(|_| {
                        LispError::ParseError(format!("Invalid integer: {}", num))
                    })?));
                }
            }
            _ if is_symbol_char(c) => {
                let mut sym = String::new();
                while let Some(&c) = chars.peek() {
                    if is_symbol_char(c) {
                        sym.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                tokens.push(Token::Symbol(sym));
            }
            _ => {
                return Err(LispError::ParseError(format!(
                    "Unexpected character: {}",
                    c
                )));
            }
        }
    }

    Ok(tokens)
}

fn is_symbol_char(c: char) -> bool {
    c.is_alphanumeric() || matches!(c, '_' | '-' | '+' | '*' | '/' | '!' | '?' | '<' | '>' | '=' | ':' | '.' | '~')
}

/// AST node for Lisp expressions.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Integer(i64),
    Float(f64),
    String(String),
    Symbol(String),
    List(Vec<Expr>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Integer(n) => write!(f, "{}", n),
            Expr::Float(n) => write!(f, "{}", n),
            Expr::String(s) => write!(f, "\"{}\"", s),
            Expr::Symbol(s) => write!(f, "{}", s),
            Expr::List(items) => {
                write!(f, "(")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", item)?;
                }
                write!(f, ")")
            }
        }
    }
}

/// Parse tokens into an AST.
pub fn parse(tokens: &[Token]) -> Result<Expr, LispError> {
    let mut pos = 0;
    parse_expr(tokens, &mut pos)
}

/// Parse multiple expressions (for a program with multiple top-level forms).
pub fn parse_all(tokens: &[Token]) -> Result<Vec<Expr>, LispError> {
    let mut pos = 0;
    let mut exprs = Vec::new();
    while pos < tokens.len() {
        exprs.push(parse_expr(tokens, &mut pos)?);
    }
    Ok(exprs)
}

fn parse_expr(tokens: &[Token], pos: &mut usize) -> Result<Expr, LispError> {
    if *pos >= tokens.len() {
        return Err(LispError::ParseError("Unexpected end of input".to_string()));
    }

    match &tokens[*pos] {
        Token::Integer(n) => {
            *pos += 1;
            Ok(Expr::Integer(*n))
        }
        Token::Float(n) => {
            *pos += 1;
            Ok(Expr::Float(*n))
        }
        Token::String(s) => {
            *pos += 1;
            Ok(Expr::String(s.clone()))
        }
        Token::Symbol(s) => {
            *pos += 1;
            Ok(Expr::Symbol(s.clone()))
        }
        Token::LParen => {
            *pos += 1; // consume (
            let mut items = Vec::new();
            while *pos < tokens.len() && tokens[*pos] != Token::RParen {
                items.push(parse_expr(tokens, pos)?);
            }
            if *pos >= tokens.len() {
                return Err(LispError::ParseError("Unclosed parenthesis".to_string()));
            }
            *pos += 1; // consume )
            Ok(Expr::List(items))
        }
        Token::LBracket => {
            // [a, b, c] is sugar for (stack a b c)
            *pos += 1; // consume [
            let mut items = vec![Expr::Symbol("stack".to_string())];
            while *pos < tokens.len() && tokens[*pos] != Token::RBracket {
                // Skip commas
                if tokens[*pos] == Token::Comma {
                    *pos += 1;
                    continue;
                }
                items.push(parse_expr(tokens, pos)?);
            }
            if *pos >= tokens.len() {
                return Err(LispError::ParseError("Unclosed bracket".to_string()));
            }
            *pos += 1; // consume ]
            Ok(Expr::List(items))
        }
        Token::RParen => Err(LispError::ParseError("Unexpected ')'".to_string())),
        Token::RBracket => Err(LispError::ParseError("Unexpected ']'".to_string())),
        Token::Comma => Err(LispError::ParseError("Unexpected ','".to_string())),
    }
}

/// Error type for Lisp operations.
#[derive(Debug, Clone)]
pub enum LispError {
    ParseError(String),
    EvalError(String),
    TypeError(String),
}

impl fmt::Display for LispError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LispError::ParseError(msg) => write!(f, "Parse error: {}", msg),
            LispError::EvalError(msg) => write!(f, "Eval error: {}", msg),
            LispError::TypeError(msg) => write!(f, "Type error: {}", msg),
        }
    }
}

impl std::error::Error for LispError {}

/// A value that can be the result of evaluation.
#[derive(Clone)]
pub enum Value {
    Integer(i64),
    Float(f64),
    String(String),
    Pattern(Pattern<Value>),
    Fraction(Fraction),
    Function(String), // Built-in function name
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "Integer({})", n),
            Value::Float(n) => write!(f, "Float({})", n),
            Value::String(s) => write!(f, "String(\"{}\")", s),
            Value::Pattern(_) => write!(f, "Pattern(...)"),
            Value::Fraction(fr) => write!(f, "Fraction({}/{})", fr.numer(), fr.denom()),
            Value::Function(name) => write!(f, "Function({})", name),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Fraction(a), Value::Fraction(b)) => a == b,
            (Value::Function(a), Value::Function(b)) => a == b,
            _ => false,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{}", n),
            Value::Float(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
            Value::Pattern(_) => write!(f, "<pattern>"),
            Value::Fraction(fr) => write!(f, "{}/{}", fr.numer(), fr.denom()),
            Value::Function(name) => write!(f, "<fn:{}>", name),
        }
    }
}

/// Environment for evaluation.
#[derive(Clone)]
pub struct Env {
    bindings: HashMap<String, Value>,
}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}

impl Env {
    pub fn new() -> Self {
        let mut bindings = HashMap::new();

        // Register built-in functions
        for name in &[
            "pure", "silence", "seq", "sequence", "cat", "slowcat", "stack",
            "fast", "slow", "early", "late", "rev", "ply", "palindrome",
            "euclid", "run", "iota", "sine", "saw", "tri", "square", "cosine",
            "range", "every", "superimpose", "layer", "first-of",
            "inside", "outside", "off", "linger", "zoom", "compress",
            // Chords
            "chord",
            // Effects (add metadata to events)
            "delay", "delaytime", "delayfeedback", "room", "size",
            "drive", "saturation", "gain", "pan", "lpf", "hpf",
            "lpq", "hpq", "comp", "compressor",
            "+", "-", "*", "/",
        ] {
            bindings.insert(name.to_string(), Value::Function(name.to_string()));
        }

        Env { bindings }
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        self.bindings.get(name)
    }

    pub fn set(&mut self, name: String, value: Value) {
        self.bindings.insert(name, value);
    }
}

/// Evaluate an expression to a Value.
pub fn eval(expr: &Expr, env: &mut Env) -> Result<Value, LispError> {
    match expr {
        Expr::Integer(n) => Ok(Value::Integer(*n)),
        Expr::Float(n) => Ok(Value::Float(*n)),
        Expr::String(s) => Ok(Value::String(s.clone())),
        Expr::Symbol(s) => {
            // Special case: ~ means silence/rest
            if s == "~" {
                return Ok(Value::Pattern(silence()));
            }
            if let Some(val) = env.get(s) {
                Ok(val.clone())
            } else {
                // Treat unknown symbols as string values (like note names)
                Ok(Value::String(s.clone()))
            }
        }
        Expr::List(items) => {
            if items.is_empty() {
                return Err(LispError::EvalError("Empty list".to_string()));
            }

            // Get the function
            let func = eval(&items[0], env)?;
            let args = &items[1..];

            match func {
                Value::Function(name) => eval_builtin(&name, args, env),
                _ => Err(LispError::TypeError(format!(
                    "Expected function, got {:?}",
                    func
                ))),
            }
        }
    }
}

/// Evaluate a built-in function.
fn eval_builtin(name: &str, args: &[Expr], env: &mut Env) -> Result<Value, LispError> {
    match name {
        // Pattern constructors
        "pure" => {
            require_args(name, args, 1)?;
            let val = eval(&args[0], env)?;
            Ok(Value::Pattern(pure(val)))
        }

        "silence" => {
            require_args(name, args, 0)?;
            Ok(Value::Pattern(silence()))
        }

        "seq" | "sequence" => {
            let pats: Result<Vec<Pattern<Value>>, _> = args
                .iter()
                .map(|arg| eval(arg, env).and_then(to_pattern))
                .collect();
            Ok(Value::Pattern(sequence(pats?)))
        }

        "cat" | "slowcat" => {
            let pats: Result<Vec<Pattern<Value>>, _> = args
                .iter()
                .map(|arg| eval(arg, env).and_then(to_pattern))
                .collect();
            Ok(Value::Pattern(slowcat_prime(pats?)))
        }

        "stack" => {
            let pats: Result<Vec<Pattern<Value>>, _> = args
                .iter()
                .map(|arg| eval(arg, env).and_then(to_pattern))
                .collect();
            Ok(Value::Pattern(stack(pats?)))
        }

        "euclid" => {
            require_args(name, args, 3)?;
            let pulses = eval(&args[0], env).and_then(to_i64)?;
            let steps = eval(&args[1], env).and_then(to_i64)?;
            let val = eval(&args[2], env)?;
            Ok(Value::Pattern(euclid(pulses, steps, val)))
        }

        "chord" => {
            // Usage: (chord c4 :minor7) or (chord c4 minor7) or (chord c4)
            if args.is_empty() {
                return Err(LispError::EvalError("chord requires at least a root note".to_string()));
            }

            // Get root note
            let root_val = eval(&args[0], env)?;
            let root_str = match &root_val {
                Value::String(s) => s.clone(),
                _ => return Err(LispError::TypeError("chord root must be a note name".to_string())),
            };

            let root_midi = parse_note(&root_str).ok_or_else(|| {
                LispError::EvalError(format!("Invalid note: {}", root_str))
            })?;

            // Get chord type (default to major)
            let chord_type = if args.len() > 1 {
                let type_val = eval(&args[1], env)?;
                match type_val {
                    Value::String(s) => s.trim_start_matches(':').to_string(),
                    _ => "major".to_string(),
                }
            } else {
                "major".to_string()
            };

            let intervals = chord_intervals(&chord_type).ok_or_else(|| {
                LispError::EvalError(format!("Unknown chord type: {}", chord_type))
            })?;

            // Build patterns for each note in the chord
            let note_patterns: Vec<Pattern<Value>> = intervals
                .iter()
                .map(|&interval| {
                    let note_name = midi_to_note(root_midi + interval);
                    pure(Value::String(note_name))
                })
                .collect();

            Ok(Value::Pattern(stack(note_patterns)))
        }

        "run" => {
            require_args(name, args, 1)?;
            let n = eval(&args[0], env).and_then(to_i64)?;
            // Convert Pattern<i64> to Pattern<Value>
            let pat = run(n).fmap(Value::Integer);
            Ok(Value::Pattern(pat))
        }

        "iota" => {
            require_args(name, args, 1)?;
            let n = eval(&args[0], env).and_then(to_i64)?;
            // iota(n) creates sequence 1..=n
            let pat = iota(1, n).fmap(Value::Integer);
            Ok(Value::Pattern(pat))
        }

        // Transformations
        "fast" => {
            require_args(name, args, 2)?;
            let factor = eval(&args[0], env).and_then(to_fraction)?;
            let pat = eval(&args[1], env).and_then(to_pattern)?;
            Ok(Value::Pattern(pat.fast(factor)))
        }

        "slow" => {
            require_args(name, args, 2)?;
            let factor = eval(&args[0], env).and_then(to_fraction)?;
            let pat = eval(&args[1], env).and_then(to_pattern)?;
            Ok(Value::Pattern(pat.slow(factor)))
        }

        "early" => {
            require_args(name, args, 2)?;
            let offset = eval(&args[0], env).and_then(to_fraction)?;
            let pat = eval(&args[1], env).and_then(to_pattern)?;
            Ok(Value::Pattern(pat.early(offset)))
        }

        "late" => {
            require_args(name, args, 2)?;
            let offset = eval(&args[0], env).and_then(to_fraction)?;
            let pat = eval(&args[1], env).and_then(to_pattern)?;
            Ok(Value::Pattern(pat.late(offset)))
        }

        "rev" => {
            require_args(name, args, 1)?;
            let pat = eval(&args[0], env).and_then(to_pattern)?;
            Ok(Value::Pattern(pat.rev()))
        }

        "ply" => {
            require_args(name, args, 2)?;
            let n = eval(&args[0], env).and_then(to_i64)?;
            let pat = eval(&args[1], env).and_then(to_pattern)?;
            Ok(Value::Pattern(pat.ply(n)))
        }

        "palindrome" => {
            require_args(name, args, 1)?;
            let pat = eval(&args[0], env).and_then(to_pattern)?;
            Ok(Value::Pattern(pat.palindrome()))
        }

        "every" => {
            require_args(name, args, 3)?;
            let n = eval(&args[0], env).and_then(to_i64)?;
            let transform_name = match &args[1] {
                Expr::Symbol(s) => s.clone(),
                Expr::List(items) if !items.is_empty() => {
                    if let Expr::Symbol(s) = &items[0] {
                        s.clone()
                    } else {
                        return Err(LispError::EvalError(
                            "every: second arg must be a transform".to_string(),
                        ));
                    }
                }
                _ => {
                    return Err(LispError::EvalError(
                        "every: second arg must be a transform".to_string(),
                    ));
                }
            };
            let pat = eval(&args[2], env).and_then(to_pattern)?;

            // Apply the named transform
            let transformed = match transform_name.as_str() {
                "rev" => pat.every(n, |p| p.rev()),
                "fast" => {
                    if let Expr::List(items) = &args[1] {
                        if items.len() >= 2 {
                            let factor = eval(&items[1], env).and_then(to_fraction)?;
                            pat.every(n, move |p| p.fast(factor))
                        } else {
                            return Err(LispError::EvalError("fast requires a factor".to_string()));
                        }
                    } else {
                        return Err(LispError::EvalError("fast requires a factor".to_string()));
                    }
                }
                "slow" => {
                    if let Expr::List(items) = &args[1] {
                        if items.len() >= 2 {
                            let factor = eval(&items[1], env).and_then(to_fraction)?;
                            pat.every(n, move |p| p.slow(factor))
                        } else {
                            return Err(LispError::EvalError("slow requires a factor".to_string()));
                        }
                    } else {
                        return Err(LispError::EvalError("slow requires a factor".to_string()));
                    }
                }
                _ => {
                    return Err(LispError::EvalError(format!(
                        "Unknown transform: {}",
                        transform_name
                    )));
                }
            };

            Ok(Value::Pattern(transformed))
        }

        "superimpose" => {
            require_args(name, args, 2)?;
            let transform_name = match &args[0] {
                Expr::Symbol(s) => s.clone(),
                _ => {
                    return Err(LispError::EvalError(
                        "superimpose: first arg must be a transform name".to_string(),
                    ));
                }
            };
            let pat = eval(&args[1], env).and_then(to_pattern)?;

            let transformed = match transform_name.as_str() {
                "rev" => pat.superimpose(|p| p.rev()),
                _ => {
                    return Err(LispError::EvalError(format!(
                        "Unknown transform: {}",
                        transform_name
                    )));
                }
            };

            Ok(Value::Pattern(transformed))
        }

        // Signals
        "sine" => {
            require_args(name, args, 0)?;
            Ok(Value::Pattern(sine().fmap(Value::Float)))
        }

        "cosine" => {
            require_args(name, args, 0)?;
            Ok(Value::Pattern(cosine().fmap(Value::Float)))
        }

        "saw" => {
            require_args(name, args, 0)?;
            Ok(Value::Pattern(saw().fmap(Value::Float)))
        }

        "tri" => {
            require_args(name, args, 0)?;
            Ok(Value::Pattern(tri().fmap(Value::Float)))
        }

        "square" => {
            require_args(name, args, 0)?;
            Ok(Value::Pattern(square().fmap(Value::Float)))
        }

        "range" => {
            require_args(name, args, 3)?;
            let min = eval(&args[0], env).and_then(to_f64)?;
            let max = eval(&args[1], env).and_then(to_f64)?;
            let pat = eval(&args[2], env).and_then(to_pattern)?;

            // Convert pattern values to f64, scale, then back to Value
            let scaled = pat.fmap(move |v| {
                let f = match v {
                    Value::Float(f) => f,
                    Value::Integer(i) => i as f64,
                    _ => 0.0,
                };
                Value::Float(min + (max - min) * f)
            });

            Ok(Value::Pattern(scaled))
        }

        "inside" => {
            require_args(name, args, 3)?;
            let factor = eval(&args[0], env).and_then(to_fraction)?;
            let transform_name = match &args[1] {
                Expr::Symbol(s) => s.clone(),
                _ => return Err(LispError::EvalError("inside: second arg must be transform".to_string())),
            };
            let pat = eval(&args[2], env).and_then(to_pattern)?;

            let result = match transform_name.as_str() {
                "rev" => pat.inside(factor, |p| p.rev()),
                _ => return Err(LispError::EvalError(format!("Unknown transform: {}", transform_name))),
            };
            Ok(Value::Pattern(result))
        }

        "outside" => {
            require_args(name, args, 3)?;
            let factor = eval(&args[0], env).and_then(to_fraction)?;
            let transform_name = match &args[1] {
                Expr::Symbol(s) => s.clone(),
                _ => return Err(LispError::EvalError("outside: second arg must be transform".to_string())),
            };
            let pat = eval(&args[2], env).and_then(to_pattern)?;

            let result = match transform_name.as_str() {
                "rev" => pat.outside(factor, |p| p.rev()),
                _ => return Err(LispError::EvalError(format!("Unknown transform: {}", transform_name))),
            };
            Ok(Value::Pattern(result))
        }

        "linger" => {
            require_args(name, args, 2)?;
            let amount = eval(&args[0], env).and_then(to_fraction)?;
            let pat = eval(&args[1], env).and_then(to_pattern)?;
            Ok(Value::Pattern(pat.linger(amount)))
        }

        "zoom" => {
            require_args(name, args, 3)?;
            let start = eval(&args[0], env).and_then(to_fraction)?;
            let end = eval(&args[1], env).and_then(to_fraction)?;
            let pat = eval(&args[2], env).and_then(to_pattern)?;
            Ok(Value::Pattern(pat.zoom(start, end)))
        }

        "compress" => {
            require_args(name, args, 3)?;
            let start = eval(&args[0], env).and_then(to_fraction)?;
            let end = eval(&args[1], env).and_then(to_fraction)?;
            let pat = eval(&args[2], env).and_then(to_pattern)?;
            Ok(Value::Pattern(pat.compress(start, end)))
        }

        // Effects - add metadata to pattern events
        // These add key-value pairs to the hap's context.meta,
        // which the audio engine reads to apply effects.
        "delay" | "delaytime" => {
            require_args(name, args, 2)?;
            let amount = eval(&args[0], env).and_then(to_f64)?;
            let pat = eval(&args[1], env).and_then(to_pattern)?;
            Ok(Value::Pattern(pat.with_meta("delay".to_string(), amount.to_string())))
        }

        "delayfeedback" => {
            require_args(name, args, 2)?;
            let amount = eval(&args[0], env).and_then(to_f64)?;
            let pat = eval(&args[1], env).and_then(to_pattern)?;
            Ok(Value::Pattern(pat.with_meta("delayfeedback".to_string(), amount.to_string())))
        }

        "room" => {
            require_args(name, args, 2)?;
            let amount = eval(&args[0], env).and_then(to_f64)?;
            let pat = eval(&args[1], env).and_then(to_pattern)?;
            Ok(Value::Pattern(pat.with_meta("room".to_string(), amount.to_string())))
        }

        "size" => {
            require_args(name, args, 2)?;
            let amount = eval(&args[0], env).and_then(to_f64)?;
            let pat = eval(&args[1], env).and_then(to_pattern)?;
            Ok(Value::Pattern(pat.with_meta("size".to_string(), amount.to_string())))
        }

        "drive" | "saturation" => {
            require_args(name, args, 2)?;
            let amount = eval(&args[0], env).and_then(to_f64)?;
            let pat = eval(&args[1], env).and_then(to_pattern)?;
            Ok(Value::Pattern(pat.with_meta("drive".to_string(), amount.to_string())))
        }

        "gain" => {
            require_args(name, args, 2)?;
            let amount = eval(&args[0], env).and_then(to_f64)?;
            let pat = eval(&args[1], env).and_then(to_pattern)?;
            Ok(Value::Pattern(pat.with_meta("gain".to_string(), amount.to_string())))
        }

        "pan" => {
            require_args(name, args, 2)?;
            let amount = eval(&args[0], env).and_then(to_f64)?;
            let pat = eval(&args[1], env).and_then(to_pattern)?;
            Ok(Value::Pattern(pat.with_meta("pan".to_string(), amount.to_string())))
        }

        "lpf" => {
            require_args(name, args, 2)?;
            let freq = eval(&args[0], env).and_then(to_f64)?;
            let pat = eval(&args[1], env).and_then(to_pattern)?;
            Ok(Value::Pattern(pat.with_meta("lpf".to_string(), freq.to_string())))
        }

        "hpf" => {
            require_args(name, args, 2)?;
            let freq = eval(&args[0], env).and_then(to_f64)?;
            let pat = eval(&args[1], env).and_then(to_pattern)?;
            Ok(Value::Pattern(pat.with_meta("hpf".to_string(), freq.to_string())))
        }

        "lpq" => {
            require_args(name, args, 2)?;
            let q = eval(&args[0], env).and_then(to_f64)?;
            let pat = eval(&args[1], env).and_then(to_pattern)?;
            Ok(Value::Pattern(pat.with_meta("lpq".to_string(), q.to_string())))
        }

        "hpq" => {
            require_args(name, args, 2)?;
            let q = eval(&args[0], env).and_then(to_f64)?;
            let pat = eval(&args[1], env).and_then(to_pattern)?;
            Ok(Value::Pattern(pat.with_meta("hpq".to_string(), q.to_string())))
        }

        "comp" | "compressor" => {
            // Compressor with threshold (dB), ratio, attack (s), release (s)
            // Usage: (comp threshold ratio attack release pattern)
            // Or simple: (comp threshold pattern) with defaults
            if args.len() == 2 {
                // Simple form: just threshold
                let threshold = eval(&args[0], env).and_then(to_f64)?;
                let pat = eval(&args[1], env).and_then(to_pattern)?;
                Ok(Value::Pattern(pat.with_meta("comp_threshold".to_string(), threshold.to_string())))
            } else if args.len() == 5 {
                // Full form: threshold, ratio, attack, release, pattern
                let threshold = eval(&args[0], env).and_then(to_f64)?;
                let ratio = eval(&args[1], env).and_then(to_f64)?;
                let attack = eval(&args[2], env).and_then(to_f64)?;
                let release = eval(&args[3], env).and_then(to_f64)?;
                let pat = eval(&args[4], env).and_then(to_pattern)?;
                let pat = pat.with_meta("comp_threshold".to_string(), threshold.to_string());
                let pat = pat.with_meta("comp_ratio".to_string(), ratio.to_string());
                let pat = pat.with_meta("comp_attack".to_string(), attack.to_string());
                let pat = pat.with_meta("comp_release".to_string(), release.to_string());
                Ok(Value::Pattern(pat))
            } else {
                Err(LispError::EvalError(format!(
                    "{} requires 2 or 5 arguments, got {}",
                    name, args.len()
                )))
            }
        }

        // Arithmetic (for integer/float values)
        "+" => {
            require_args(name, args, 2)?;
            let a = eval(&args[0], env)?;
            let b = eval(&args[1], env)?;
            match (a, b) {
                (Value::Integer(x), Value::Integer(y)) => Ok(Value::Integer(x + y)),
                (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x + y)),
                (Value::Integer(x), Value::Float(y)) => Ok(Value::Float(x as f64 + y)),
                (Value::Float(x), Value::Integer(y)) => Ok(Value::Float(x + y as f64)),
                _ => Err(LispError::TypeError("+ requires numbers".to_string())),
            }
        }

        "-" => {
            require_args(name, args, 2)?;
            let a = eval(&args[0], env)?;
            let b = eval(&args[1], env)?;
            match (a, b) {
                (Value::Integer(x), Value::Integer(y)) => Ok(Value::Integer(x - y)),
                (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x - y)),
                (Value::Integer(x), Value::Float(y)) => Ok(Value::Float(x as f64 - y)),
                (Value::Float(x), Value::Integer(y)) => Ok(Value::Float(x - y as f64)),
                _ => Err(LispError::TypeError("- requires numbers".to_string())),
            }
        }

        "*" => {
            require_args(name, args, 2)?;
            let a = eval(&args[0], env)?;
            let b = eval(&args[1], env)?;
            match (a, b) {
                (Value::Integer(x), Value::Integer(y)) => Ok(Value::Integer(x * y)),
                (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x * y)),
                (Value::Integer(x), Value::Float(y)) => Ok(Value::Float(x as f64 * y)),
                (Value::Float(x), Value::Integer(y)) => Ok(Value::Float(x * y as f64)),
                _ => Err(LispError::TypeError("* requires numbers".to_string())),
            }
        }

        "/" => {
            require_args(name, args, 2)?;
            let a = eval(&args[0], env)?;
            let b = eval(&args[1], env)?;
            match (a, b) {
                (Value::Integer(x), Value::Integer(y)) => {
                    if y == 0 {
                        Err(LispError::EvalError("Division by zero".to_string()))
                    } else {
                        Ok(Value::Fraction(Fraction::new(x, y)))
                    }
                }
                (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x / y)),
                (Value::Integer(x), Value::Float(y)) => Ok(Value::Float(x as f64 / y)),
                (Value::Float(x), Value::Integer(y)) => Ok(Value::Float(x / y as f64)),
                _ => Err(LispError::TypeError("/ requires numbers".to_string())),
            }
        }

        _ => Err(LispError::EvalError(format!("Unknown function: {}", name))),
    }
}

/// Get chord intervals (in semitones) for a chord type.
/// Returns intervals relative to root (root is always 0).
fn chord_intervals(chord_type: &str) -> Option<Vec<i64>> {
    Some(match chord_type {
        // Triads
        "major" | "maj" | "" => vec![0, 4, 7],
        "minor" | "min" | "m" => vec![0, 3, 7],
        "dim" | "diminished" => vec![0, 3, 6],
        "aug" | "augmented" | "+" => vec![0, 4, 8],
        "sus2" => vec![0, 2, 7],
        "sus4" => vec![0, 5, 7],

        // Seventh chords
        "7" | "dom7" => vec![0, 4, 7, 10],
        "maj7" | "major7" => vec![0, 4, 7, 11],
        "min7" | "minor7" | "m7" => vec![0, 3, 7, 10],
        "dim7" | "diminished7" => vec![0, 3, 6, 9],
        "hdim7" | "m7b5" | "half-diminished" => vec![0, 3, 6, 10],
        "minmaj7" | "mM7" => vec![0, 3, 7, 11],
        "aug7" | "+7" => vec![0, 4, 8, 10],

        // Extended chords
        "9" | "dom9" => vec![0, 4, 7, 10, 14],
        "maj9" | "major9" => vec![0, 4, 7, 11, 14],
        "min9" | "minor9" | "m9" => vec![0, 3, 7, 10, 14],
        "11" => vec![0, 4, 7, 10, 14, 17],
        "13" => vec![0, 4, 7, 10, 14, 21],

        // Add chords
        "add9" => vec![0, 4, 7, 14],
        "add11" => vec![0, 4, 7, 17],

        // Power chord
        "5" | "power" => vec![0, 7],

        _ => return None,
    })
}

/// Parse a note name like "c4", "d#3", "eb5" into a MIDI note number.
/// Returns None if the string isn't a valid note.
fn parse_note(s: &str) -> Option<i64> {
    let s = s.to_lowercase();
    let mut chars = s.chars().peekable();

    // Get the note letter
    let letter = chars.next()?;
    let base = match letter {
        'c' => 0,
        'd' => 2,
        'e' => 4,
        'f' => 5,
        'g' => 7,
        'a' => 9,
        'b' => 11,
        _ => return None,
    };

    // Check for accidentals
    let mut accidental = 0i64;
    while let Some(&c) = chars.peek() {
        match c {
            '#' | 's' => { accidental += 1; chars.next(); }
            'b' | 'f' => { accidental -= 1; chars.next(); }
            _ => break,
        }
    }

    // Get the octave number
    let octave_str: String = chars.collect();
    let octave: i64 = octave_str.parse().ok()?;

    // MIDI note: C4 = 60
    Some((octave + 1) * 12 + base + accidental)
}

/// Convert a MIDI note number back to a note name.
fn midi_to_note(midi: i64) -> String {
    let octave = (midi / 12) - 1;
    let note_in_octave = midi % 12;
    let note_name = match note_in_octave {
        0 => "c",
        1 => "cs",
        2 => "d",
        3 => "ds",
        4 => "e",
        5 => "f",
        6 => "fs",
        7 => "g",
        8 => "gs",
        9 => "a",
        10 => "as",
        11 => "b",
        _ => "c",
    };
    format!("{}{}", note_name, octave)
}

fn require_args(name: &str, args: &[Expr], expected: usize) -> Result<(), LispError> {
    if args.len() != expected {
        Err(LispError::EvalError(format!(
            "{} requires {} argument(s), got {}",
            name,
            expected,
            args.len()
        )))
    } else {
        Ok(())
    }
}

fn to_pattern(val: Value) -> Result<Pattern<Value>, LispError> {
    match val {
        Value::Pattern(p) => Ok(p),
        // Wrap non-pattern values in pure
        other => Ok(pure(other)),
    }
}

fn to_i64(val: Value) -> Result<i64, LispError> {
    match val {
        Value::Integer(n) => Ok(n),
        Value::Float(f) => Ok(f as i64),
        _ => Err(LispError::TypeError(format!(
            "Expected integer, got {:?}",
            val
        ))),
    }
}

fn to_f64(val: Value) -> Result<f64, LispError> {
    match val {
        Value::Float(f) => Ok(f),
        Value::Integer(n) => Ok(n as f64),
        _ => Err(LispError::TypeError(format!(
            "Expected number, got {:?}",
            val
        ))),
    }
}

fn to_fraction(val: Value) -> Result<Fraction, LispError> {
    match val {
        Value::Integer(n) => Ok(Fraction::from_integer(n)),
        Value::Float(f) => Ok(Fraction::from(f)),
        Value::Fraction(f) => Ok(f),
        _ => Err(LispError::TypeError(format!(
            "Expected number, got {:?}",
            val
        ))),
    }
}

/// Convenience function to parse and evaluate a Lisp expression.
pub fn run_lisp(input: &str) -> Result<Value, LispError> {
    let tokens = tokenize(input)?;
    let expr = parse(&tokens)?;
    let mut env = Env::new();
    eval(&expr, &mut env)
}

/// Get the pattern from a value (convenience for testing).
pub fn to_pattern_result(val: Value) -> Result<Pattern<Value>, LispError> {
    to_pattern(val)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize_simple() {
        let tokens = tokenize("(+ 1 2)").unwrap();
        assert_eq!(tokens, vec![
            Token::LParen,
            Token::Symbol("+".to_string()),
            Token::Integer(1),
            Token::Integer(2),
            Token::RParen,
        ]);
    }

    #[test]
    fn test_tokenize_string() {
        let tokens = tokenize("\"hello world\"").unwrap();
        assert_eq!(tokens, vec![Token::String("hello world".to_string())]);
    }

    #[test]
    fn test_tokenize_nested() {
        let tokens = tokenize("(fast 2 (seq 1 2 3))").unwrap();
        assert_eq!(tokens.len(), 10);
    }

    #[test]
    fn test_parse_atom() {
        let tokens = tokenize("42").unwrap();
        let expr = parse(&tokens).unwrap();
        assert_eq!(expr, Expr::Integer(42));
    }

    #[test]
    fn test_parse_list() {
        let tokens = tokenize("(+ 1 2)").unwrap();
        let expr = parse(&tokens).unwrap();
        assert_eq!(
            expr,
            Expr::List(vec![
                Expr::Symbol("+".to_string()),
                Expr::Integer(1),
                Expr::Integer(2),
            ])
        );
    }

    #[test]
    fn test_eval_arithmetic() {
        let result = run_lisp("(+ 1 2)").unwrap();
        assert_eq!(result, Value::Integer(3));

        let result = run_lisp("(* 3 4)").unwrap();
        assert_eq!(result, Value::Integer(12));
    }

    #[test]
    fn test_eval_pure() {
        let result = run_lisp("(pure 42)").unwrap();
        if let Value::Pattern(pat) = result {
            let haps = pat.first_cycle();
            assert_eq!(haps.len(), 1);
            assert_eq!(haps[0].value, Value::Integer(42));
        } else {
            panic!("Expected pattern");
        }
    }

    #[test]
    fn test_eval_sequence() {
        let result = run_lisp("(seq 1 2 3 4)").unwrap();
        if let Value::Pattern(pat) = result {
            let haps = pat.first_cycle();
            assert_eq!(haps.len(), 4);
        } else {
            panic!("Expected pattern");
        }
    }

    #[test]
    fn test_eval_fast() {
        let result = run_lisp("(fast 2 (pure x))").unwrap();
        if let Value::Pattern(pat) = result {
            let haps = pat.first_cycle();
            assert_eq!(haps.len(), 2);
        } else {
            panic!("Expected pattern");
        }
    }

    #[test]
    fn test_eval_stack() {
        let result = run_lisp("(stack kick snare)").unwrap();
        if let Value::Pattern(pat) = result {
            let haps = pat.first_cycle();
            assert_eq!(haps.len(), 2);
        } else {
            panic!("Expected pattern");
        }
    }

    #[test]
    fn test_eval_euclid() {
        let result = run_lisp("(euclid 3 8 snare)").unwrap();
        if let Value::Pattern(pat) = result {
            let haps = pat.first_cycle();
            assert_eq!(haps.len(), 3);
        } else {
            panic!("Expected pattern");
        }
    }

    #[test]
    fn test_eval_nested() {
        let result = run_lisp("(fast 2 (seq 1 2 3))").unwrap();
        if let Value::Pattern(pat) = result {
            let haps = pat.first_cycle();
            assert_eq!(haps.len(), 6); // 3 * 2
        } else {
            panic!("Expected pattern");
        }
    }

    #[test]
    fn test_eval_sine() {
        let result = run_lisp("(sine)").unwrap();
        if let Value::Pattern(_) = result {
            // Signal patterns are continuous, so just check it's a pattern
        } else {
            panic!("Expected pattern");
        }
    }

    #[test]
    fn test_eval_rev() {
        let result = run_lisp("(rev (seq a b c d))").unwrap();
        if let Value::Pattern(pat) = result {
            let haps = pat.first_cycle();
            assert_eq!(haps.len(), 4);
        } else {
            panic!("Expected pattern");
        }
    }

    #[test]
    fn test_eval_comments() {
        let result = run_lisp("; this is a comment\n(pure 42)").unwrap();
        if let Value::Pattern(pat) = result {
            let haps = pat.first_cycle();
            assert_eq!(haps.len(), 1);
        } else {
            panic!("Expected pattern");
        }
    }

    #[test]
    fn test_eval_linger() {
        let result = run_lisp("(linger 0.25 (seq 0 1 2 3 4 5 6 7))").unwrap();
        if let Value::Pattern(pat) = result {
            let haps = pat.first_cycle();
            // linger(0.25) repeats first quarter, should get 8 events (0,1 repeated 4 times)
            assert_eq!(haps.len(), 8);
        } else {
            panic!("Expected pattern");
        }
    }

    #[test]
    fn test_fraction_literal() {
        // 1/4 as a fraction using division
        let result = run_lisp("(/ 1 4)").unwrap();
        if let Value::Fraction(f) = result {
            assert_eq!(f, Fraction::new(1, 4));
        } else {
            panic!("Expected fraction, got {:?}", result);
        }
    }
}
