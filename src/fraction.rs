//! Fraction type with cycle-related operations for pattern timing.
//!
//! This module provides a wrapper around rational numbers with methods
//! specific to the cycle-based timing system used in Strudel patterns.

use num_integer::Integer;
use num_rational::Rational64;
use num_traits::{One, Signed, Zero};
use std::cmp::Ordering;
use std::ops::{Add, Div, Mul, Neg, Sub};

/// A fraction representing a point in time or duration within the pattern system.
/// Time is measured in cycles, where one cycle is the fundamental repeating unit.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Fraction(Rational64);

impl Fraction {
    /// Create a new fraction from numerator and denominator.
    pub fn new(numer: i64, denom: i64) -> Self {
        Fraction(Rational64::new(numer, denom))
    }

    /// Create a fraction from an integer.
    pub fn from_integer(n: i64) -> Self {
        Fraction(Rational64::from_integer(n))
    }

    /// Returns the start of the cycle containing this time (floor to integer).
    /// In Tidal/Strudel this is called "sam" (start of measure).
    pub fn sam(&self) -> Self {
        Fraction(Rational64::from_integer(self.0.floor().to_integer()))
    }

    /// Returns the start of the next cycle.
    pub fn next_sam(&self) -> Self {
        self.sam() + Fraction::one()
    }

    /// Returns the position within the current cycle (fractional part).
    pub fn cycle_pos(&self) -> Self {
        *self - self.sam()
    }

    /// Returns the whole cycle containing this time as a TimeSpan.
    /// This is useful for discrete events that occupy their whole cycle.
    pub fn whole_cycle(&self) -> (Self, Self) {
        (self.sam(), self.next_sam())
    }

    /// Floor to integer.
    pub fn floor(&self) -> Self {
        Fraction(Rational64::from_integer(self.0.floor().to_integer()))
    }

    /// Ceiling to integer.
    pub fn ceil(&self) -> Self {
        Fraction(Rational64::from_integer(self.0.ceil().to_integer()))
    }

    /// Convert to f64 for audio calculations.
    pub fn to_f64(&self) -> f64 {
        *self.0.numer() as f64 / *self.0.denom() as f64
    }

    /// Get the numerator.
    pub fn numer(&self) -> i64 {
        *self.0.numer()
    }

    /// Get the denominator.
    pub fn denom(&self) -> i64 {
        *self.0.denom()
    }

    /// Returns the underlying rational number.
    pub fn as_rational(&self) -> Rational64 {
        self.0
    }

    /// Returns the minimum of two fractions.
    pub fn min(self, other: Self) -> Self {
        if self < other {
            self
        } else {
            other
        }
    }

    /// Returns the maximum of two fractions.
    pub fn max(self, other: Self) -> Self {
        if self > other {
            self
        } else {
            other
        }
    }

    /// Zero fraction.
    pub fn zero() -> Self {
        Fraction(Rational64::zero())
    }

    /// One (a whole cycle).
    pub fn one() -> Self {
        Fraction(Rational64::one())
    }

    /// Check if this fraction is zero.
    pub fn is_zero(&self) -> bool {
        self.0.is_zero()
    }

    /// Absolute value.
    pub fn abs(&self) -> Self {
        Fraction(self.0.abs())
    }
}

impl Default for Fraction {
    fn default() -> Self {
        Fraction::zero()
    }
}

impl From<i64> for Fraction {
    fn from(n: i64) -> Self {
        Fraction::from_integer(n)
    }
}

impl From<i32> for Fraction {
    fn from(n: i32) -> Self {
        Fraction::from_integer(n as i64)
    }
}

impl From<f64> for Fraction {
    fn from(f: f64) -> Self {
        // Convert float to rational with bounded denominator to avoid overflow
        // Use a maximum denominator of 10000 for reasonable precision without overflow risk
        let max_denom: i64 = 10000;

        if f.is_nan() || f.is_infinite() {
            return Fraction::zero();
        }

        // Handle negative numbers
        let sign = if f < 0.0 { -1 } else { 1 };
        let f_abs = f.abs();

        // Integer part
        let int_part = f_abs.floor() as i64;
        let frac_part = f_abs - int_part as f64;

        if frac_part < 1e-10 {
            return Fraction::from_integer(sign * int_part);
        }

        // Use continued fraction approximation with bounded denominator
        // Stern-Brocot tree / mediant method
        let mut a_num: i64 = 0;
        let mut a_den: i64 = 1;
        let mut b_num: i64 = 1;
        let mut b_den: i64 = 1;

        let target = frac_part;

        for _ in 0..50 {
            let med_num = a_num + b_num;
            let med_den = a_den + b_den;

            if med_den > max_denom {
                break;
            }

            let med_val = med_num as f64 / med_den as f64;

            if (med_val - target).abs() < 1e-10 {
                // Found exact match
                let total_num = sign * (int_part * med_den + med_num);
                return Fraction::new(total_num, med_den);
            } else if med_val < target {
                a_num = med_num;
                a_den = med_den;
            } else {
                b_num = med_num;
                b_den = med_den;
            }
        }

        // Choose the closer approximation
        let a_val = a_num as f64 / a_den as f64;
        let b_val = b_num as f64 / b_den as f64;

        let (best_num, best_den) = if (a_val - target).abs() < (b_val - target).abs() {
            (a_num, a_den)
        } else {
            (b_num, b_den)
        };

        let total_num = sign * (int_part * best_den + best_num);
        Fraction::new(total_num, best_den)
    }
}

impl From<Rational64> for Fraction {
    fn from(r: Rational64) -> Self {
        Fraction(r)
    }
}

impl Add for Fraction {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Fraction(self.0 + other.0)
    }
}

impl Sub for Fraction {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Fraction(self.0 - other.0)
    }
}

impl Mul for Fraction {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        Fraction(self.0 * other.0)
    }
}

impl Div for Fraction {
    type Output = Self;

    fn div(self, other: Self) -> Self {
        Fraction(self.0 / other.0)
    }
}

impl Neg for Fraction {
    type Output = Self;

    fn neg(self) -> Self {
        Fraction(-self.0)
    }
}

impl PartialOrd for Fraction {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Fraction {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.cmp(&other.0)
    }
}

impl std::fmt::Display for Fraction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}/{}", self.numer(), self.denom())
    }
}

/// Compute the GCD of multiple fractions.
pub fn gcd(fractions: &[Fraction]) -> Option<Fraction> {
    if fractions.is_empty() {
        return None;
    }

    let mut result = fractions[0].0;
    for frac in &fractions[1..] {
        // GCD of rationals: gcd(a/b, c/d) = gcd(a*d, c*b) / (b*d)
        let a = result.numer();
        let b = result.denom();
        let c = frac.0.numer();
        let d = frac.0.denom();
        let numer_gcd = (a * d).gcd(&(c * b));
        result = Rational64::new(numer_gcd, b * d);
    }
    Some(Fraction(result))
}

/// Compute the LCM of multiple fractions.
pub fn lcm(fractions: &[Fraction]) -> Option<Fraction> {
    if fractions.is_empty() {
        return None;
    }

    let mut result = fractions[0].0;
    for frac in &fractions[1..] {
        // LCM of rationals: lcm(a/b, c/d) = lcm(a, c) / gcd(b, d)
        let a = *result.numer();
        let b = *result.denom();
        let c = *frac.0.numer();
        let d = *frac.0.denom();
        let numer_lcm = a.lcm(&c);
        let denom_gcd = b.gcd(&d);
        result = Rational64::new(numer_lcm, denom_gcd);
    }
    Some(Fraction(result))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sam() {
        assert_eq!(Fraction::new(0, 1).sam(), Fraction::new(0, 1));
        assert_eq!(Fraction::new(1, 2).sam(), Fraction::new(0, 1));
        assert_eq!(Fraction::new(3, 2).sam(), Fraction::new(1, 1));
        assert_eq!(Fraction::new(5, 2).sam(), Fraction::new(2, 1));
    }

    #[test]
    fn test_next_sam() {
        assert_eq!(Fraction::new(0, 1).next_sam(), Fraction::new(1, 1));
        assert_eq!(Fraction::new(1, 2).next_sam(), Fraction::new(1, 1));
        assert_eq!(Fraction::new(3, 2).next_sam(), Fraction::new(2, 1));
    }

    #[test]
    fn test_cycle_pos() {
        assert_eq!(Fraction::new(0, 1).cycle_pos(), Fraction::new(0, 1));
        assert_eq!(Fraction::new(1, 2).cycle_pos(), Fraction::new(1, 2));
        assert_eq!(Fraction::new(3, 2).cycle_pos(), Fraction::new(1, 2));
        assert_eq!(Fraction::new(7, 4).cycle_pos(), Fraction::new(3, 4));
    }

    #[test]
    fn test_arithmetic() {
        let a = Fraction::new(1, 2);
        let b = Fraction::new(1, 3);
        assert_eq!(a + b, Fraction::new(5, 6));
        assert_eq!(a - b, Fraction::new(1, 6));
        assert_eq!(a * b, Fraction::new(1, 6));
        assert_eq!(a / b, Fraction::new(3, 2));
    }
}
