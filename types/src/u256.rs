use core::fmt;
use std::{
    cmp::Ordering,
    ops::*,
    str::FromStr
};

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, Hash, Eq, Ord)]
pub struct U256([u64; 4]);

impl Default for U256 {
    fn default() -> Self {
        U256([0, 0, 0, 0])
    }
}

impl Serialize for U256 {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(&self.to_string())
    }
}

impl<'a> Deserialize<'a> for U256 {
    fn deserialize<D: serde::Deserializer<'a>>(deserializer: D) -> Result<Self, D::Error> {
        let s = String::deserialize(deserializer)?;
        U256::from_str(&s).map_err(|_| serde::de::Error::custom("Invalid U256 string"))
    }
}

impl U256 {
    pub const ONE: U256 = U256([1, 0, 0, 0]);
    pub const ZERO: U256 = U256([0, 0, 0, 0]);

    pub const MIN: U256 = Self::ZERO;
    pub const MAX: U256 = U256([u64::MAX, u64::MAX, u64::MAX, u64::MAX]);

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

    /// Raises self to the power of exp, using exponentiation by squaring.
    pub fn pow(self, exp: u32) -> U256 {
        let mut result = U256::ONE;
        for _ in 0..exp {
            result *= self;
        }
        result
    }

    /// Create a new U256 from a string and a radix.
    pub fn from_str_radix(s: &str, radix: u32) -> Result<U256, ()> {
        let mut result = U256::ZERO;
        for c in s.chars() {
            if let Some(digit) = c.to_digit(radix) {
                result = result * U256::from(radix) + U256::from(digit as u64);
            } else {
                return Err(());
            }
        }

        Ok(result)
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
        let mut result = [0u64; 4];
        let mut overflow = false;

        // We multiply each pair of 64-bit segments, handling carries appropriately.
        for i in 0..4 {
            let mut carry = 0u128;
            for j in 0..(4 - i) {
                let a = self.0[i] as u128;
                let b = other.0[j] as u128;
                let product = a * b + result[i + j] as u128 + carry;
                result[i + j] = product as u64;
                carry = product >> 64;
            }
            // If there's any carry left and we're outside the 256-bit bounds, we have overflow.
            if carry > 0 {
                if i + 4 < 4 {
                    result[i + 4] = carry as u64;
                } else {
                    overflow = true;
                }
            }
        }

        (U256(result), overflow)
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
            divisor_shifted = divisor_shifted << 1;
            shift += 1;
        }

        // Perform the division bit by bit
        while shift > 0 {
            divisor_shifted = divisor_shifted >> 1;
            shift -= 1;

            // If the remainder is greater than or equal to divisor_shifted, subtract and update quotient
            if remainder >= divisor_shifted {
                remainder = remainder - divisor_shifted;
                quotient = quotient | (U256::ONE << shift);
            }
        }

        (quotient, false)
    }

    /// Remainder with overflow handling
    /// Panics if the divisor is zero
    pub fn overflowing_rem(self, divisor: U256) -> (U256, bool) {
        assert!(!divisor.is_zero(), "U256 division by zero");

        // Check if self is less than the divisor; if so, the remainder is self with no overflow
        if self < divisor {
            return (self, false);
        }

        // If the divisor is 1, the remainder is zero with no overflow
        if divisor.is_one() {
            return (U256::ZERO, false);
        }

        // Initialize remainder
        let mut remainder = self;

        // We start by aligning the divisor with the most significant bit of `self`
        let mut shift = 0;
        let mut divisor_shifted = divisor;
        while remainder >= divisor_shifted {
            divisor_shifted = divisor_shifted << 1;
            shift += 1;
        }

        // Perform the division bit by bit
        while shift > 0 {
            divisor_shifted = divisor_shifted >> 1;
            shift -= 1;

            // If the remainder is greater than or equal to divisor_shifted, subtract
            if remainder >= divisor_shifted {
                remainder = remainder - divisor_shifted;
            }
        }

        (remainder, false)
    }

    /// Checked subtraction with overflow handling
    pub fn checked_sub(self, other: U256) -> Option<U256> {
        let (result, overflow) = self.overflowing_sub(other);
        if overflow {
            None
        } else {
            Some(result)
        }
    }

    /// Checked addition with overflow handling
    pub fn checked_add(self, other: U256) -> Option<U256> {
        let (result, overflow) = self.overflowing_add(other);
        if overflow {
            None
        } else {
            Some(result)
        }
    }

    /// Checked multiplication with overflow handling
    /// Returns None if the multiplication overflows
    pub fn checked_mul(self, other: U256) -> Option<U256> {
        let (result, overflow) = self.overflowing_mul(other);
        if overflow {
            None
        } else {
            Some(result)
        }
    }

    /// Checked division with overflow handling
    /// Returns None if the divisor is zero or the division overflows
    pub fn checked_div(self, divisor: U256) -> Option<U256> {
        if divisor.is_zero() {
            None
        } else {
            let (result, overflow) = self.overflowing_div(divisor);
            if overflow {
                None
            } else {
                Some(result)
            }
        }
    }

    /// Checked remainder with overflow handling
    /// Returns None if the divisor is zero or the remainder overflows
    pub fn checked_rem(self, divisor: U256) -> Option<U256> {
        if divisor.is_zero() {
            None
        } else {
            let (result, overflow) = self.overflowing_rem(divisor);
            if overflow {
                None
            } else {
                Some(result)
            }
        }
    }

    /// Export the data as a big-endian byte array
    pub fn to_be_bytes(&self) -> [u8; 32] {
        let bytes = [
            self.0[3].to_be_bytes(),
            self.0[2].to_be_bytes(),
            self.0[1].to_be_bytes(),
            self.0[0].to_be_bytes(),
        ].concat();

        bytes.try_into().unwrap()
    }

    /// Import the data from a big-endian byte array
    pub fn from_be_bytes(bytes: [u8; 32]) -> Self {
        let mut data = [0u64; 4];
        for i in 0..4 {
            data[i] = u64::from_be_bytes(bytes[i * 8..(i + 1) * 8].try_into().unwrap());
        }

        U256(data)
    }

    /// Export the data as a little-endian byte array
    pub fn to_le_bytes(&self) -> [u8; 32] {
        let bytes = [
            self.0[0].to_le_bytes(),
            self.0[1].to_le_bytes(),
            self.0[2].to_le_bytes(),
            self.0[3].to_le_bytes(),
        ].concat();

        bytes.try_into().unwrap()
    }

    /// Import the data from a little-endian byte array
    pub fn from_le_bytes(bytes: [u8; 32]) -> Self {
        let mut data = [0u64; 4];
        for i in 0..4 {
            data[i] = u64::from_le_bytes(bytes[i * 8..(i + 1) * 8].try_into().unwrap());
        }

        U256(data)
    }

    // Helper method to perform division and remainder with a u64 divisor
    fn div_rem_u64(self, divisor: u64) -> (U256, u64) {
        let mut result = [0u64; 4];
        let mut remainder = 0u128;

        // Perform the division starting from the most significant part
        for i in (0..4).rev() {
            let dividend = (remainder << 64) | (self.0[i] as u128);
            result[i] = (dividend / divisor as u128) as u64;
            remainder = dividend % divisor as u128;
        }

        (U256(result), remainder as u64)
    }

    /// Get the low u64 value
    pub fn low_u64(&self) -> u64 {
        self.0[0]
    }

    /// Try to get a u64 value
    pub fn as_u64(&self) -> Option<u64> {
        for i in 1..4 {
            if self.0[i] != 0 {
                return None;
            }
        }

        Some(self.0[0])
    }

    // Get the high u64 value
    pub fn high_u64(&self) -> u64 {
        self.0[3]
    }

    /// Get the low u128 value
    pub fn low_u128(&self) -> u128 {
        (self.0[0] as u128) | ((self.0[1] as u128) << 64)
    }
}

impl FromStr for U256 {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        U256::from_str_radix(s, 10)
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

impl Rem for U256 {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self {
        let (result, overflow) = self.overflowing_rem(rhs);
        debug_assert!(!overflow, "U256 remainder overflow");
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

impl Shr for U256 {
    type Output = Self;

    fn shr(self, shift: Self) -> Self {
        if shift >= 256u64.into() {
            return U256([0; 4]);
        }

        let mut result = [0u64; 4];
        let v: U256 = 64u64.into();
        let word_shift: u32 = (shift / v).into();
        let bit_shift: u64 = (shift % v).into();

        let word_shift = word_shift as usize;
        for i in 0..(4 - word_shift) {
            result[i] = self.0[i + word_shift] >> bit_shift;
            if bit_shift > 0 && i + word_shift + 1 < 4 {
                result[i] |= self.0[i + word_shift + 1] << (64 - bit_shift);
            }
        }

        U256(result)
    }
}

impl Shl for U256 {
    type Output = Self;

    fn shl(self, shift: Self) -> Self {
        if shift >= 256u64.into() {
            return U256([0; 4]);
        }

        let mut result = [0u64; 4];
        let v: U256 = 64u64.into();
        let word_shift: u32 = (shift / v).into();
        let bit_shift: u64 = (shift % v).into();

        let word_shift = word_shift as usize;
        for i in (word_shift..4).rev() {
            result[i] = self.0[i - word_shift] << bit_shift;
            if bit_shift > 0 && i > 0 {
                result[i] |= self.0[i - word_shift - 1] >> (64 - bit_shift);
            }
        }

        U256(result)
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

impl AddAssign for U256 {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl SubAssign for U256 {
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs;
    }
}

impl MulAssign for U256 {
    fn mul_assign(&mut self, rhs: Self) {
        *self = *self * rhs;
    }
}

impl DivAssign for U256 {
    fn div_assign(&mut self, rhs: Self) {
        *self = *self / rhs;
    }
}

impl RemAssign for U256 {
    fn rem_assign(&mut self, rhs: Self) {
        *self = *self % rhs;
    }
}

impl BitXorAssign for U256 {
    fn bitxor_assign(&mut self, rhs: Self) {
        *self = *self ^ rhs;
    }
}

impl BitOrAssign for U256 {
    fn bitor_assign(&mut self, rhs: Self) {
        *self = *self | rhs;
    }
}

impl BitAndAssign for U256 {
    fn bitand_assign(&mut self, rhs: Self) {
        *self = *self & rhs;
    }
}

impl ShlAssign<u32> for U256 {
    fn shl_assign(&mut self, shift: u32) {
        *self = *self << shift;
    }
}

impl ShrAssign<u32> for U256 {
    fn shr_assign(&mut self, shift: u32) {
        *self = *self >> shift;
    }
}

impl From<bool> for U256 {
    fn from(value: bool) -> Self {
        U256([value as u64, 0, 0, 0])
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

impl Into<u8> for U256 {
    fn into(self) -> u8 {
        self.0[0] as u8
    }
}

impl Into<u16> for U256 {
    fn into(self) -> u16 {
        self.0[0] as u16
    }
}

impl Into<u32> for U256 {
    fn into(self) -> u32 {
        self.0[0] as u32
    }
}

impl Into<u64> for U256 {
    fn into(self) -> u64 {
        self.0[0]
    }
}

impl Into<u128> for U256 {
    fn into(self) -> u128 {
        (self.0[0] as u128) | ((self.0[1] as u128) << 64)
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
        while !temp.is_zero() {
            let (quotient, remainder) = temp.div_rem_u64(10);
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
    fn test_from_str_radix() {
        assert_eq!(U256::from_str_radix("0", 10).unwrap(), U256::ZERO);
        assert_eq!(U256::from_str_radix("1", 10).unwrap(), U256::ONE);
        assert_eq!(U256::from_str_radix("1234567890", 10).unwrap(), U256::from(1234567890u64));
        assert_eq!(U256::from_str_radix("ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff", 16).unwrap(), U256::MAX);
    }

    #[test]
    fn test_string() {
        assert_eq!(U256::ZERO.to_string(), "0");
        assert_eq!(U256::ONE.to_string(), "1");
        assert_eq!(U256::MAX.to_string(), "115792089237316195423570985008687907853269984665640564039457584007913129639935");
        assert_eq!(U256::from(1234567890u64).to_string(), "1234567890");
        assert_eq!(U256::from(u64::MAX).to_string(), u64::MAX.to_string());
        assert_eq!(U256::from(u128::MAX).to_string(), u128::MAX.to_string());

        // From string
        assert_eq!(U256::from_str("0").unwrap(), U256::ZERO);
        assert_eq!(U256::from_str("1").unwrap(), U256::ONE);
        assert_eq!(U256::from_str("1234567890").unwrap(), U256::from(1234567890u64));
        assert_eq!(U256::from_str("115792089237316195423570985008687907853269984665640564039457584007913129639935").unwrap(), U256::MAX);
    }

    #[test]
    fn test_add_overflow() {
        let a = U256([1, 0, 0, 0]);
        let b = U256([1, 0, 0, 0]);
        let (result, overflow) = a.overflowing_add(b);
        assert_eq!(result, U256([2, 0, 0, 0]));
        assert_eq!(overflow, false);

        let max = U256::MAX;
        let (result, overflow) = max.overflowing_add(U256::ONE);
        assert_eq!(result, U256([0, 0, 0, 0]));
        assert_eq!(overflow, true);
    }

    #[test]
    fn test_sub_overflow() {
        let a = U256([2, 0, 0, 0]);
        let b = U256([1, 0, 0, 0]);
        let (result, overflow) = a.overflowing_sub(b);
        assert_eq!(result, U256([1, 0, 0, 0]));
        assert_eq!(overflow, false);

        let zero = U256::ZERO;
        let (result, overflow) = zero.overflowing_sub(U256::ONE);
        assert_eq!(result, U256::MAX);
        assert_eq!(overflow, true);
    }

    #[test]
    fn test_mul_overflow() {
        let a = U256([1, 0, 0, 0]);
        let b = U256([1, 0, 0, 0]);
        let (result, overflow) = a.overflowing_mul(b);
        assert_eq!(result, U256([1, 0, 0, 0]));
        assert_eq!(overflow, false);

        let max = U256::MAX;
        let (_, overflow) = max.overflowing_mul(U256::from(2u64));
        assert_eq!(overflow, true);
    }

    #[test]
    fn test_and() {
        let a = U256([1, 0, 0, 0]);
        let b = U256([1, 0, 0, 0]);
        assert_eq!(a & b, U256([1, 0, 0, 0]));

        let a = U256([1, 0, 0, 0]);
        let b = U256([0, 1, 0, 0]);
        assert_eq!(a & b, U256::ZERO);
    }

    #[test]
    fn test_or() {
        let a = U256([1, 0, 0, 0]);
        let b = U256([1, 0, 0, 0]);
        assert_eq!(a | b, U256([1, 0, 0, 0]));

        let a = U256([1, 0, 0, 0]);
        let b = U256([0, 1, 0, 0]);
        assert_eq!(a | b, U256([1, 1, 0, 0]));
    }

    #[test]
    fn test_shl() {
        let a = U256([1, 0, 0, 0]);
        assert_eq!(a.shl(64), U256([0, 1, 0, 0]));

        let a = U256([1, 0, 0, 0]);
        assert_eq!(a.shl(128), U256([0, 0, 1, 0]));
    }

    #[test]
    fn test_shr() {
        let a = U256([0, 1, 0, 0]);
        assert_eq!(a.shr(64), U256([1, 0, 0, 0]));

        let a = U256([0, 0, 1, 0]);
        assert_eq!(a.shr(128), U256([1, 0, 0, 0]));
    }

    #[test]
    fn test_div() {
        let a = U256([1, 0, 0, 0]);
        let b = U256([1, 0, 0, 0]);
        assert_eq!(a / b, U256::ONE);

        let a = U256([1, 0, 0, 0]);
        let b = U256([2, 0, 0, 0]);
        assert_eq!(a / b, U256::ZERO);

        let a = U256([1, 0, 0, 0]);
        let b = U256([0, 1, 0, 0]);
        assert_eq!(a / b, U256::ZERO);

        let a = U256([1, 0, 0, 0]);
        let b = U256([0, 0, 1, 0]);
        assert_eq!(a / b, U256::ZERO);

        let a = U256([1, 0, 0, 0]);
        let b = U256([0, 0, 0, 1]);
        assert_eq!(a / b, U256::ZERO);

        let a = U256([0, 0, 0, 1]);
        let b = U256([0, 0, 0, 1]);
        assert_eq!(a / b, U256::ONE);

        let a = U256([0, 0, 0, 1]);
        let b = U256([0, 0, 0, 2]);
        assert_eq!(a / b, U256::ZERO);
    }
}