use core::fmt;
use std::{
    cmp::Ordering,
    ops::{
        Add,
        BitAnd,
        BitOr,
        BitXor,
        Div,
        Mul,
        Shl,
        Shr,
        Sub
    }
};

#[derive(Debug, Clone, Copy, Eq, Ord)]
pub struct U256([u64; 4]);

impl Default for U256 {
    fn default() -> Self {
        U256([0, 0, 0, 0])
    }
}

impl U256 {
    pub const MAX: U256 = U256([u64::MAX, u64::MAX, u64::MAX, u64::MAX]);
    pub const ONE: U256 = U256([1, 0, 0, 0]);
    pub const ZERO: U256 = U256([0, 0, 0, 0]);

    /// Returns true if the number is zero.
    pub fn is_zero(&self) -> bool {
        self.0.iter().all(|&x| x == 0)
    }

    /// Returns true if the number is one.
    pub fn is_one(&self) -> bool {
        self.0[0] == 1 && self.0.iter().skip(1).all(|&x| x == 0)
    }

    /// Create a new U256 from four u64 values (from least significant to most significant)
    pub fn new(lowest: u64, low: u64, high: u64, highest: u64) -> U256 {
        U256([lowest, low, high, highest])
    }

    /// Addition with overflow handling
    pub fn overflowing_add(self, other: U256) -> (U256, bool) {
        let mut result = [0u64; 4];
        let mut carry = 0u64;

        for i in 0..4 {
            let (res, overflow1) = self.0[i].overflowing_add(other.0[i]);
            let (res, overflow2) = res.overflowing_add(carry);
            result[i] = res;
            carry = (overflow1 as u64) + (overflow2 as u64);
        }

        (U256(result), carry != 0)
    }

    /// Subtraction with overflow handling
    pub fn overflowing_sub(self, other: U256) -> (U256, bool) {
        let mut result = [0u64; 4];
        let mut borrow = 0u64;

        for i in 0..4 {
            let (res, overflow1) = self.0[i].overflowing_sub(other.0[i]);
            let (res, overflow2) = res.overflowing_sub(borrow);
            result[i] = res;
            borrow = (overflow1 as u64) | (overflow2 as u64);
        }

        (U256(result), borrow != 0)
    }

    /// Multiplication with overflow handling
    pub fn overflowing_mul(self, other: U256) -> (U256, bool) {
        let mut result = U256([0; 4]);
        let mut overflow = false;

        for i in 0..4 {
            for j in 0..4 {
                if i + j < 4 {
                    let (low, high) = Self::mul_u64(self.0[i], other.0[j]);
                    let (res, overflow1) = result.0[i + j].overflowing_add(low);
                    result.0[i + j] = res;
                    overflow |= overflow1;

                    if i + j + 1 < 4 {
                        let (res, overflow2) = result.0[i + j + 1].overflowing_add(high);
                        result.0[i + j + 1] = res;
                        overflow |= overflow2;
                    } else if high != 0 {
                        overflow = true;
                    }
                } else {
                    overflow = true;
                }
            }
        }

        (result, overflow)
    }

    /// Helper function to multiply two u64 values and return both parts of the result
    fn mul_u64(a: u64, b: u64) -> (u64, u64) {
        let low = a as u128 * b as u128;
        ((low & 0xFFFFFFFFFFFFFFFF) as u64, (low >> 64) as u64)
    }

    /// Division with overflow handling
    /// Panics if the divisor is zero
    pub fn overflowing_div(self, divisor: U256) -> (U256, bool) {
        assert!(!divisor.is_zero(), "U256 division by zero");

        // Check if self is less than the divisor; if so, the result is zero with no overflow
        if self < divisor {
            return (U256([0; 4]), false);
        }

        // If the divisor is 1, the result is self, with no overflow
        if divisor.is_one() {
            return (self, false);
        }

        // Initialize quotient and remainder
        let mut quotient = U256([0; 4]);
        let mut remainder = self;

        // We start by aligning the divisor with the most significant bit of `self`
        let mut shift = 0;
        let mut divisor_shifted = divisor;
        while remainder >= divisor_shifted {
            divisor_shifted = divisor_shifted.shl(1);
            shift += 1;
        }

        // Perform the division bit by bit
        while shift > 0 {
            divisor_shifted = divisor_shifted.shr(1);
            shift -= 1;

            // If the remainder is greater than or equal to divisor_shifted, subtract and update quotient
            if remainder >= divisor_shifted {
                remainder = remainder - divisor_shifted;
                quotient = quotient | (U256::ONE << shift);
            }
        }

        (quotient, false)
    }


    pub fn to_be_bytes(&self) -> [u8; 32] {
        let bytes = [
            self.0[3].to_be_bytes(),
            self.0[2].to_be_bytes(),
            self.0[1].to_be_bytes(),
            self.0[0].to_be_bytes(),
        ].concat();

        bytes.try_into().unwrap()
    }

    pub fn from_be_bytes(bytes: [u8; 32]) -> Self {
        let mut data = [0u64; 4];
        for i in 0..4 {
            data[i] = u64::from_be_bytes(bytes[i * 8..(i + 1) * 8].try_into().unwrap());
        }

        U256(data)
    }

    pub fn to_le_bytes(&self) -> [u8; 32] {
        let bytes = [
            self.0[0].to_le_bytes(),
            self.0[1].to_le_bytes(),
            self.0[2].to_le_bytes(),
            self.0[3].to_le_bytes(),
        ].concat();

        bytes.try_into().unwrap()
    }

    pub fn from_le_bytes(bytes: [u8; 32]) -> Self {
        let mut data = [0u64; 4];
        for i in 0..4 {
            data[i] = u64::from_le_bytes(bytes[i * 8..(i + 1) * 8].try_into().unwrap());
        }

        U256(data)
    }
}

impl Add for U256 {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        let (result, overflow) = self.overflowing_add(rhs);
        debug_assert!(!overflow, "U256 addition overflow");
        result
    }
}

impl Sub for U256 {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {
        let (result, overflow) = self.overflowing_sub(rhs);
        debug_assert!(!overflow, "U256 subtraction overflow");
        result
    }
}

impl Mul for U256 {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self {
        let (result, overflow) = self.overflowing_mul(rhs);
        debug_assert!(!overflow, "U256 multiplication overflow");
        result
    }
}

impl Div for U256 {
    type Output = Self;

    fn div(self, rhs: Self) -> Self {
        let (result, overflow) = self.overflowing_div(rhs);
        debug_assert!(!overflow, "U256 division overflow");
        result
    }
}

impl PartialEq for U256 {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl PartialOrd for U256 {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        // Compare from the most significant part to the least significant
        for i in (0..4).rev() {
            if self.0[i] < other.0[i] {
                return Some(Ordering::Less);
            } else if self.0[i] > other.0[i] {
                return Some(Ordering::Greater);
            }
        }
        Some(Ordering::Equal)
    }
}

impl BitXor for U256 {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self {
        let mut result = [0u64; 4];
        for i in 0..4 {
            result[i] = self.0[i] ^ rhs.0[i];
        }
        U256(result)
    }
}

impl Shr<u32> for U256 {
    type Output = Self;

    fn shr(self, shift: u32) -> Self {
        if shift >= 256 {
            return U256([0; 4]);
        }

        let mut result = [0u64; 4];
        let word_shift = (shift / 64) as usize;
        let bit_shift = shift % 64;

        for i in 0..(4 - word_shift) {
            result[i] = self.0[i + word_shift] >> bit_shift;
            if bit_shift > 0 && i + word_shift + 1 < 4 {
                result[i] |= self.0[i + word_shift + 1] << (64 - bit_shift);
            }
        }

        U256(result)
    }
}

impl Shl<u32> for U256 {
    type Output = Self;

    fn shl(self, shift: u32) -> Self {
        if shift >= 256 {
            return U256([0; 4]);
        }

        let mut result = [0u64; 4];
        let word_shift = (shift / 64) as usize;
        let bit_shift = shift % 64;

        for i in (word_shift..4).rev() {
            result[i] = self.0[i - word_shift] << bit_shift;
            if bit_shift > 0 && i > 0 {
                result[i] |= self.0[i - word_shift - 1] >> (64 - bit_shift);
            }
        }

        U256(result)
    }
}

impl BitOr for U256 {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self {
        let mut result = [0u64; 4];
        for i in 0..4 {
            result[i] = self.0[i] | rhs.0[i];
        }
        U256(result)
    }
}

impl BitAnd for U256 {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self {
        let mut result = [0u64; 4];
        for i in 0..4 {
            result[i] = self.0[i] & rhs.0[i];
        }
        U256(result)
    }
}

impl From<u8> for U256 {
    fn from(value: u8) -> Self {
        U256([value as u64, 0, 0, 0])
    }
}

impl From<u16> for U256 {
    fn from(value: u16) -> Self {
        U256([value as u64, 0, 0, 0])
    }
}

impl From<u32> for U256 {
    fn from(value: u32) -> Self {
        U256([value as u64, 0, 0, 0])
    }
}

impl From<u64> for U256 {
    fn from(value: u64) -> Self {
        U256([value, 0, 0, 0])
    }
}

impl From<u128> for U256 {
    fn from(value: u128) -> Self {
        U256([value as u64, (value >> 64) as u64, 0, 0])
    }
}

impl fmt::Display for U256 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.is_zero() {
            return write!(f, "0");
        }

        let mut result = String::new();
        let mut temp = *self;

        // Use repeated division by 10 to extract each decimal digit
        while temp != U256([0; 4]) {
            let (quotient, remainder) = temp % 10;
            result.push(char::from_digit(remainder as u32, 10).unwrap());
            temp = quotient;
        }

        write!(f, "{}", result.chars().rev().collect::<String>())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_to_string() {
        assert_eq!(U256::ZERO.to_string(), "0");
        assert_eq!(U256::ONE.to_string(), "1");
        assert_eq!(U256::MAX.to_string(), "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF");
    }

    #[test]
    fn test_add_overflow() {
        let a = U256([1, 0, 0, 0]);
        let b = U256([1, 0, 0, 0]);
        let (result, overflow) = a.overflowing_add(b);
        assert_eq!(result, U256([2, 0, 0, 0]));
        assert_eq!(overflow, false);
    }

    #[test]
    fn test_sub_overflow() {
        let a = U256([2, 0, 0, 0]);
        let b = U256([1, 0, 0, 0]);
        let (result, overflow) = a.overflowing_sub(b);
        assert_eq!(result, U256([1, 0, 0, 0]));
        assert_eq!(overflow, false);
    }

    #[test]
    fn test_mul_overflow() {
        let a = U256([1, 0, 0, 0]);
        let b = U256([1, 0, 0, 0]);
        let (result, overflow) = a.overflowing_mul(b);
        assert_eq!(result, U256([1, 0, 0, 0]));
        assert_eq!(overflow, false);
    }

    #[test]
    fn test_and() {
        let a = U256([1, 0, 0, 0]);
        let b = U256([1, 0, 0, 0]);
        assert_eq!(a.bitwise_and(b), U256([1, 0, 0, 0]));
    }

    #[test]
    fn test_or() {
        let a = U256([1, 0, 0, 0]);
        let b = U256([1, 0, 0, 0]);
        assert_eq!(a.bitwise_or(b), U256([1, 0, 0, 0]));
    }

    #[test]
    fn test_shl() {
        let a = U256([1, 0, 0, 0]);
        assert_eq!(a.shl(64), U256([0, 1, 0, 0]));
    }

    #[test]
    fn test_shr() {
        let a = U256([0, 1, 0, 0]);
        assert_eq!(a.shr(64), U256([1, 0, 0, 0]));
    }
}