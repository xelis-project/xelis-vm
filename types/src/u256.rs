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
    #[inline]
    pub fn is_zero(&self) -> bool {
        self.0.iter().all(|&x| x == 0)
    }

    /// Returns true if the number is one.
    #[inline]
    pub fn is_one(&self) -> bool {
        self.0[0] == 1 && self.0.iter().skip(1).all(|&x| x == 0)
    }

    /// Create a new U256 from four u64 values (from least significant to most significant)
    #[inline]
    pub fn new(lowest: u64, low: u64, high: u64, highest: u64) -> U256 {
        U256([lowest, low, high, highest])
    }

    /// Reverse the bits of this U256 value
    pub fn reverse_bits(self) -> U256 {
        U256([
            self.0[3].reverse_bits(),
            self.0[2].reverse_bits(),
            self.0[1].reverse_bits(),
            self.0[0].reverse_bits(),
        ])
    }

    /// Returns the number of leading zeros in this U256 value
    #[inline]
    pub fn leading_zeros(&self) -> u32 {
        for i in (0..4).rev() {
            if self.0[i] != 0 {
                return ((3 - i) as u32) * 64 + self.0[i].leading_zeros();
            }
        }
        256
    }

    /// Returns the number of leading ones in this U256 value
    #[inline]
    pub fn leading_ones(&self) -> u32 {
        for i in (0..4).rev() {
            if self.0[i] != u64::MAX {
                return ((3 - i) as u32) * 64 + self.0[i].leading_ones();
            }
        }
        256
    }

    /// Returns the number of bits needed to represent this number
    #[inline]
    pub fn bits(&self) -> u32 {
        256 - self.leading_zeros()
    }

    /// Raises self to the power of exp, using exponentiation by squaring.
    pub fn pow(self, exp: u32) -> U256 {
        if exp == 0 {
            return U256::ONE;
        }
        
        let mut base = self;
        let mut result = U256::ONE;
        let mut exp_remaining = exp;
        
        // Square and multiply algorithm
        while exp_remaining > 0 {
            // If current exponent bit is 1, multiply result by the current base
            if exp_remaining & 1 == 1 {
                result *= base;
            }
            
            // Square the base
            base *= base;
            
            // Move to next bit
            exp_remaining >>= 1;
        }
        
        result
    }

    /// Raises self to the power of `exp`, returning `None` if overflow occurs.
    pub fn checked_pow(self, exp: u32) -> Option<U256> {
        if exp == 0 {
            return Some(U256::ONE);
        }

        let mut base = self;
        let mut result = U256::ONE;
        let mut exp_remaining = exp;

        while exp_remaining > 0 {
            if exp_remaining & 1 == 1 {
                result = result.checked_mul(base)?;
            }

            base = base.checked_mul(base)?;
            exp_remaining >>= 1;
        }

        Some(result)
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
    #[inline]
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
    #[inline]
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
    #[inline]
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
            // If there's any carry left, we have overflow since we've used all 256 bits
            if carry > 0 {
                overflow = true;
            }
        }

        (U256(result), overflow)
    }

    /// Division with overflow handling
    /// Panics if the divisor is zero
    #[inline]
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

        // Short-circuit if divisor fits in a single u64
        if let Some(small_divisor) = divisor.as_u64() {
            let (quotient, _) = self.div_rem_u64(small_divisor);
            return (quotient, false);
        }

        // Initialize quotient and remainder
        let mut quotient = U256([0; 4]);
        let mut remainder = self;

        // Calculate the number of bits needed to represent each number
        let dividend_bits = remainder.bits();
        let divisor_bits = divisor.bits();
        
        // Only shift if dividend has enough bits
        if dividend_bits >= divisor_bits {
            // Compute the initial shift
            let shift = dividend_bits - divisor_bits;
            
            // Perform division bit by bit
            for i in (0..=shift).rev() {
                let divisor_shifted = divisor << i;
                
                if remainder >= divisor_shifted {
                    remainder = remainder - divisor_shifted;
                    quotient = quotient | (U256::ONE << i);
                }
            }
        }

        (quotient, false)
    }

    /// Remainder with overflow handling
    /// Panics if the divisor is zero
    #[inline]
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

        // Short-circuit if divisor fits in a single u64
        if let Some(small_divisor) = divisor.as_u64() {
            let (_, remainder) = self.div_rem_u64(small_divisor);
            return (U256::from(remainder), false);
        }

        // Initialize remainder
        let mut remainder = self;

        // Calculate the number of bits needed to represent each number
        let dividend_bits = remainder.bits();
        let divisor_bits = divisor.bits();
        
        // Only process if dividend has enough bits
        if dividend_bits >= divisor_bits {
            // Compute the initial shift
            let shift = dividend_bits - divisor_bits;
            
            // Perform division bit by bit to calculate the remainder
            for i in (0..=shift).rev() {
                let divisor_shifted = divisor << i;
                
                if remainder >= divisor_shifted {
                    remainder = remainder - divisor_shifted;
                }
            }
        }

        (remainder, false)
    }

    /// Checked subtraction with overflow handling
    #[inline]
    pub fn checked_sub(self, other: U256) -> Option<U256> {
        let (result, overflow) = self.overflowing_sub(other);
        if overflow {
            None
        } else {
            Some(result)
        }
    }

    /// Checked addition with overflow handling
    #[inline]
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
    #[inline]
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
    #[inline]
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
    #[inline]
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

    /// Saturating addition. Returns MAX on overflow.
    #[inline]
    pub fn saturating_add(self, other: U256) -> U256 {
        let (res, overflow) = self.overflowing_add(other);
        if overflow {
            U256::MAX
        } else {
            res
        }
    }

    /// Saturating subtraction. Returns ZERO on underflow.
    #[inline]
    pub fn saturating_sub(self, other: U256) -> U256 {
        let (res, overflow) = self.overflowing_sub(other);
        if overflow {
            U256::ZERO
        } else {
            res
        }
    }

    /// Saturating multiplication. Returns MAX on overflow.
    #[inline]
    pub fn saturating_mul(self, other: U256) -> U256 {
        let (res, overflow) = self.overflowing_mul(other);
        if overflow {
            U256::MAX
        } else {
            res
        }
    }

    /// Saturating left shift. Returns MAX on overflow.
    #[inline]
    pub fn saturating_shl(self, shift: u32) -> U256 {
        match self.checked_shl(shift) {
            Some(v) => v,
            None => U256::MAX,
        }
    }

    /// Saturating right shift. Returns ZERO on overflow.
    #[inline]
    pub fn saturating_shr(self, shift: u32) -> U256 {
        match self.checked_shr(shift) {
            Some(v) => v,
            None => U256::ZERO,
        }
    }

    /// Saturating division. Returns MAX on division by zero.
    #[inline]
    pub fn saturating_div(self, divisor: U256) -> U256 {
        if divisor.is_zero() {
            // Division by zero saturates to MAX
            return U256::MAX;
        }

        let (res, _overflow) = self.overflowing_div(divisor);
        res
    }

    /// Saturating remainder. Returns self on division by zero.
    #[inline]
    pub fn saturating_rem(self, divisor: U256) -> U256 {
        if divisor.is_zero() {
            // Saturating remainder returns the dividend itself
            return self;
        }

        // remainder can be derived from quotient and divisor
        let quotient = self / divisor;
        self - quotient * divisor
    }

    /// Raises self to the power of `exp`, saturating at MAX on overflow.
    #[inline]
    pub fn saturating_pow(self, exp: u32) -> U256 {
        self.checked_pow(exp).unwrap_or(U256::MAX)
    }

    /// Export the data as a big-endian byte array
    #[inline]
    pub fn to_be_bytes(&self) -> [u8; 32] {
        let mut result = [0u8; 32];
        for (i, part) in self.0.iter().enumerate() {
            result[i * 8..(i + 1) * 8].copy_from_slice(&part.to_be_bytes());
        }

        result
    }

    /// Import the data from a big-endian byte array
    #[inline]
    pub fn from_be_bytes(bytes: [u8; 32]) -> Self {
        let mut data = [0u64; 4];
        for i in 0..4 {
            data[i] = u64::from_be_bytes(bytes[i * 8..(i + 1) * 8].try_into().unwrap());
        }

        U256(data)
    }

    /// Export the data as a little-endian byte array
    #[inline]
    pub fn to_le_bytes(&self) -> [u8; 32] {
        let mut result = [0u8; 32];
        for (i, part) in self.0.iter().enumerate() {
            result[i * 8..(i + 1) * 8].copy_from_slice(&part.to_le_bytes());
        }

        result
    }

    /// Import the data from a little-endian byte array
    #[inline]
    pub fn from_le_bytes(bytes: [u8; 32]) -> Self {
        let mut data = [0u64; 4];
        for i in 0..4 {
            data[i] = u64::from_le_bytes(bytes[i * 8..(i + 1) * 8].try_into().unwrap());
        }

        U256(data)
    }

    // Helper method to perform division and remainder with a u64 divisor
    #[inline]
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
    #[inline]
    pub fn low_u64(&self) -> u64 {
        self.0[0]
    }

    /// Try to get a u64 value
    #[inline]
    pub fn as_u64(&self) -> Option<u64> {
        for i in 1..4 {
            if self.0[i] != 0 {
                return None;
            }
        }

        Some(self.0[0])
    }

    // Get the high u64 value
    #[inline]
    pub fn high_u64(&self) -> u64 {
        self.0[3]
    }

    /// Get the low u128 value
    #[inline]
    pub fn low_u128(&self) -> u128 {
        (self.0[0] as u128) | ((self.0[1] as u128) << 64)
    }

    #[inline]
    pub fn checked_shr(self, shift: u32) -> Option<Self> {
        if shift >= 256 {
            return None;
        }

        let mut result = [0u64; 4];
        let word_shift = (shift / 64) as usize;
        let bit_shift = (shift % 64) as u32;

        for i in 0..(4 - word_shift) {
            result[i] = self.0[i + word_shift] >> bit_shift;
            if bit_shift > 0 && i + word_shift + 1 < 4 {
                result[i] |= self.0[i + word_shift + 1] << (64 - bit_shift);
            }
        }

        Some(U256(result))
    }

    #[inline]
    pub fn checked_shl(self, shift: u32) -> Option<Self> {
        if shift >= 256 {
            return None;
        }

        let mut result = [0u64; 4];
        let word_shift = (shift / 64) as usize;
        let bit_shift = (shift % 64) as u32;

        for i in (word_shift..4).rev() {
            result[i] = self.0[i - word_shift] << bit_shift;
            if bit_shift > 0 && i > word_shift {
                result[i] |= self.0[i - word_shift - 1] >> (64 - bit_shift);
            }
        }

        Some(U256(result))
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

    #[inline]
    fn add(self, rhs: Self) -> Self {
        let (result, overflow) = self.overflowing_add(rhs);
        debug_assert!(!overflow, "U256 addition overflow");
        result
    }
}

impl Sub for U256 {
    type Output = Self;

    #[inline]
    fn sub(self, rhs: Self) -> Self {
        let (result, overflow) = self.overflowing_sub(rhs);
        debug_assert!(!overflow, "U256 subtraction overflow");
        result
    }
}

impl Mul for U256 {
    type Output = Self;

    #[inline]
    fn mul(self, rhs: Self) -> Self {
        let (result, overflow) = self.overflowing_mul(rhs);
        debug_assert!(!overflow, "U256 multiplication overflow");
        result
    }
}

impl Div for U256 {
    type Output = Self;

    #[inline]
    fn div(self, rhs: Self) -> Self {
        let (result, overflow) = self.overflowing_div(rhs);
        debug_assert!(!overflow, "U256 division overflow");
        result
    }
}

impl Rem for U256 {
    type Output = Self;

    #[inline]
    fn rem(self, rhs: Self) -> Self {
        let (result, overflow) = self.overflowing_rem(rhs);
        debug_assert!(!overflow, "U256 remainder overflow");
        result
    }
}

impl PartialEq for U256 {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl PartialOrd for U256 {
    #[inline]
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

    #[inline]
    fn shr(self, shift: u32) -> Self {
        self.checked_shr(shift).unwrap_or(U256::ZERO)
    }
}

impl Shl<u32> for U256 {
    type Output = Self;

    #[inline]
    fn shl(self, shift: u32) -> Self {
        self.checked_shl(shift).unwrap_or(U256::ZERO)
    }
}

impl Shr for U256 {
    type Output = Self;

    #[inline]
    fn shr(self, shift: Self) -> Self {
        if shift >= U256::from(256u64) {
            return U256::ZERO;
        }

        self.checked_shr(shift.into()).unwrap_or(U256::ZERO)
    }
}

impl Shl for U256 {
    type Output = Self;

    #[inline]
    fn shl(self, shift: Self) -> Self {
        if shift >= U256::from(256u64) {
            return U256::ZERO;
        }


        self.checked_shl(shift.into()).unwrap_or(U256::ZERO)
    }
}

impl BitXor for U256 {
    type Output = Self;

    #[inline]
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

    #[inline]
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

    #[inline]
    fn bitand(self, rhs: Self) -> Self {
        let mut result = [0u64; 4];
        for i in 0..4 {
            result[i] = self.0[i] & rhs.0[i];
        }
        U256(result)
    }
}

impl AddAssign for U256 {
    #[inline]
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl SubAssign for U256 {
    #[inline]
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs;
    }
}

impl MulAssign for U256 {
    #[inline]
    fn mul_assign(&mut self, rhs: Self) {
        *self = *self * rhs;
    }
}

impl DivAssign for U256 {
    #[inline]
    fn div_assign(&mut self, rhs: Self) {
        *self = *self / rhs;
    }
}

impl RemAssign for U256 {
    #[inline]
    fn rem_assign(&mut self, rhs: Self) {
        *self = *self % rhs;
    }
}

impl BitXorAssign for U256 {
    #[inline]
    fn bitxor_assign(&mut self, rhs: Self) {
        *self = *self ^ rhs;
    }
}

impl BitOrAssign for U256 {
    #[inline]
    fn bitor_assign(&mut self, rhs: Self) {
        *self = *self | rhs;
    }
}

impl BitAndAssign for U256 {
    #[inline]
    fn bitand_assign(&mut self, rhs: Self) {
        *self = *self & rhs;
    }
}

impl ShlAssign<u32> for U256 {
    #[inline]
    fn shl_assign(&mut self, shift: u32) {
        *self = *self << shift;
    }
}

impl ShrAssign<u32> for U256 {
    #[inline]
    fn shr_assign(&mut self, shift: u32) {
        *self = *self >> shift;
    }
}

impl From<bool> for U256 {
    #[inline]
    fn from(value: bool) -> Self {
        U256([value as u64, 0, 0, 0])
    }
}

impl From<i32> for U256 {
    #[inline]
    fn from(value: i32) -> Self {
        U256([value as u64, 0, 0, 0])
    }
}

impl From<u8> for U256 {
    #[inline]
    fn from(value: u8) -> Self {
        U256([value as u64, 0, 0, 0])
    }
}

impl From<u16> for U256 {
    #[inline]
    fn from(value: u16) -> Self {
        U256([value as u64, 0, 0, 0])
    }
}

impl From<u32> for U256 {
    #[inline]
    fn from(value: u32) -> Self {
        U256([value as u64, 0, 0, 0])
    }
}

impl From<u64> for U256 {
    #[inline]
    fn from(value: u64) -> Self {
        U256([value, 0, 0, 0])
    }
}

impl From<u128> for U256 {
    #[inline]
    fn from(value: u128) -> Self {
        U256([value as u64, (value >> 64) as u64, 0, 0])
    }
}

impl Into<u8> for U256 {
    #[inline]
    fn into(self) -> u8 {
        self.0[0] as u8
    }
}

impl Into<u16> for U256 {
    #[inline]
    fn into(self) -> u16 {
        self.0[0] as u16
    }
}

impl Into<u32> for U256 {
    #[inline]
    fn into(self) -> u32 {
        self.0[0] as u32
    }
}

impl Into<u64> for U256 {
    #[inline]
    fn into(self) -> u64 {
        self.0[0]
    }
}

impl Into<u128> for U256 {
    #[inline]
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

impl PartialEq<u64> for U256 {
    #[inline]
    fn eq(&self, other: &u64) -> bool {
        self.as_u64()
            .map(|v| v.eq(other))
            .unwrap_or(false)
    }
}

impl PartialOrd<u64> for U256 {
    #[inline]
    fn partial_cmp(&self, other: &u64) -> Option<Ordering> {
        self.as_u64().map(|v| v.cmp(other))
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

    #[test]
    fn test_from_to_be() {
        let bytes = U256::ONE.to_be_bytes();
        assert_eq!(U256::ONE, U256::from_be_bytes(bytes));
    }

    #[test]
    fn test_from_to_le() {
        let bytes = U256::ONE.to_le_bytes();
        assert_eq!(U256::ONE, U256::from_le_bytes(bytes));
    }

    #[test]
    fn test_leading_zeros() {
        // Test with values in different words
        assert_eq!(U256::ZERO.leading_zeros(), 256);
        assert_eq!(U256::ONE.leading_zeros(), 255);
        assert_eq!(U256::from(2u64).leading_zeros(), 254);
        assert_eq!(U256::from(u64::MAX).leading_zeros(), 256 - 64);
        
        // Test with values in second word
        let a = U256([0, 1, 0, 0]);
        assert_eq!(a.leading_zeros(), 256 - 65);
        
        // Test with values in third word
        let b = U256([0, 0, 1, 0]);
        assert_eq!(b.leading_zeros(), 256 - 129);
        
        // Test with values in fourth word
        let c = U256([0, 0, 0, 1]);
        assert_eq!(c.leading_zeros(), 256 - 193);
        
        // Test with max value
        assert_eq!(U256::MAX.leading_zeros(), 0);
    }

    #[test]
    fn test_bits() {
        assert_eq!(U256::ZERO.bits(), 0);
        assert_eq!(U256::ONE.bits(), 1);
        assert_eq!(U256::from(2u64).bits(), 2);
        assert_eq!(U256::from(3u64).bits(), 2);
        assert_eq!(U256::from(4u64).bits(), 3);
        assert_eq!(U256::from(u64::MAX).bits(), 64);
        
        // Test with values in second word
        let a = U256([0, 1, 0, 0]);
        assert_eq!(a.bits(), 65);
        
        // Test with max value
        assert_eq!(U256::MAX.bits(), 256);
    }

    #[test]
    fn test_division_edge_cases() {
        // Test division with powers of 2
        let a = U256::from(8u64);
        let b = U256::from(2u64);
        assert_eq!(a / b, U256::from(4u64));
        
        // Test division where dividend has same number of bits as divisor
        let a = U256::from(7u64);  // 111 in binary
        let b = U256::from(7u64);  // 111 in binary
        assert_eq!(a / b, U256::ONE);
        
        // Test division where dividend has one more bit than divisor
        let a = U256::from(14u64); // 1110 in binary
        let b = U256::from(7u64);  // 111 in binary
        assert_eq!(a / b, U256::from(2u64));
        
        // Test division with large difference in magnitude
        let a = U256::from(u64::MAX);
        let b = U256::from(1u64);
        assert_eq!(a / b, U256::from(u64::MAX));
        
        // Test remainder
        let a = U256::from(7u64);
        let b = U256::from(4u64);
        assert_eq!(a % b, U256::from(3u64));
        
        // Test division with large numbers
        let a = U256([0, 0, 0, 1]); // 2^192
        let b = U256([1, 0, 0, 0]); // 1
        assert_eq!(a / b, U256([0, 0, 0, 1]));
        
        // Test division with numbers in different words
        let a = U256([0, 0, 1, 0]); // 2^128
        let b = U256([0, 1, 0, 0]); // 2^64
        assert_eq!(a / b, U256([0, 1, 0, 0])); // 2^64
    }

    #[test]
    fn test_full_width_shift() {
        // Test shifting by exactly 256 bits
        let a = U256::ONE;
        assert_eq!(a << 256u32, U256::ZERO);
        assert_eq!(a >> 256u32, U256::ZERO);
        
        let b = U256::MAX;
        assert_eq!(b << 256u32, U256::ZERO);
        assert_eq!(b >> 256u32, U256::ZERO);
        
        // Test shifting with U256 as shift amount
        let shift_256 = U256::from(256u64);
        assert_eq!(a << shift_256, U256::ZERO);
        assert_eq!(a >> shift_256, U256::ZERO);
        
        // Test shifting by more than 256 bits
        let c = U256::from(42u64);
        assert_eq!(c << 300u32, U256::ZERO);
        assert_eq!(c >> 300u32, U256::ZERO);
        
        let shift_300 = U256::from(300u64);
        assert_eq!(c << shift_300, U256::ZERO);
        assert_eq!(c >> shift_300, U256::ZERO);
    }
    
    #[test]
    fn test_division_power_of_two() {
        // Test division where divisor is 2^n
        let a = U256::from(32u64);
        
        for i in 0..6 {
            let b = U256::ONE << i;
            let expected = U256::from(32u64 >> i);
            assert_eq!(a / b, expected);
        }
        
        // Test division where divisor is 2^n - 1 (Mersenne numbers)
        let a = U256::from(32u64);
        
        // Test with divisor = 3 (2^2 - 1)
        let b = U256::from(3u64);
        assert_eq!(a / b, U256::from(10u64));
        assert_eq!(a % b, U256::from(2u64));
        
        // Test with divisor = 7 (2^3 - 1)
        let b = U256::from(7u64);
        assert_eq!(a / b, U256::from(4u64));
        assert_eq!(a % b, U256::from(4u64));
        
        // Test with divisor = 15 (2^4 - 1)
        let b = U256::from(15u64);
        assert_eq!(a / b, U256::from(2u64));
        assert_eq!(a % b, U256::from(2u64));
        
        // Test with divisor = 2^n + 1
        
        // Test with divisor = 5 (2^2 + 1)
        let b = U256::from(5u64);
        assert_eq!(a / b, U256::from(6u64));
        assert_eq!(a % b, U256::from(2u64));
        
        // Test with divisor = 9 (2^3 + 1)
        let b = U256::from(9u64);
        assert_eq!(a / b, U256::from(3u64));
        assert_eq!(a % b, U256::from(5u64));
        
        // Test with divisor = 17 (2^4 + 1)
        let b = U256::from(17u64);
        assert_eq!(a / b, U256::from(1u64));
        assert_eq!(a % b, U256::from(15u64));
    }
    
    #[test]
    fn test_byte_order() {
        // Test big-endian byte order with specific values
        let value = U256::from(0x0123456789ABCDEFu64);
        let bytes = value.to_be_bytes();
        
        // In the current implementation, to_be_bytes puts each u64 section in big-endian order,
        // but keeps the array in the order of least significant to most significant u64
        
        // First 8 bytes (u64[0]) should contain the value in big-endian order
        assert_eq!(bytes[0], 0x01);
        assert_eq!(bytes[1], 0x23);
        assert_eq!(bytes[2], 0x45);
        assert_eq!(bytes[3], 0x67);
        assert_eq!(bytes[4], 0x89);
        assert_eq!(bytes[5], 0xAB);
        assert_eq!(bytes[6], 0xCD);
        assert_eq!(bytes[7], 0xEF);
        
        // The remaining bytes should be zero
        for i in 8..32 {
            assert_eq!(bytes[i], 0);
        }
        
        // Verify roundtrip
        assert_eq!(U256::from_be_bytes(bytes), value);
        
        // Test little-endian byte order with the same value
        let bytes = value.to_le_bytes();
        
        // In the current implementation, to_le_bytes puts each u64 section in little-endian order,
        // and keeps the array in the order of least significant to most significant u64
        assert_eq!(bytes[0], 0xEF);
        assert_eq!(bytes[1], 0xCD);
        assert_eq!(bytes[2], 0xAB);
        assert_eq!(bytes[3], 0x89);
        assert_eq!(bytes[4], 0x67);
        assert_eq!(bytes[5], 0x45);
        assert_eq!(bytes[6], 0x23);
        assert_eq!(bytes[7], 0x01);
        
        // The remaining bytes should be zero
        for i in 8..32 {
            assert_eq!(bytes[i], 0);
        }
        
        // Verify roundtrip
        assert_eq!(U256::from_le_bytes(bytes), value);
        
        // Test with a value spanning multiple words
        let value = U256([
            0x0123456789ABCDEF,
            0xFEDCBA9876543210,
            0x0F1E2D3C4B5A6978,
            0x8796A5B4C3D2E1F0
        ]);
        
        // Test big-endian roundtrip
        let bytes = value.to_be_bytes();
        assert_eq!(U256::from_be_bytes(bytes), value);
        
        // Test little-endian roundtrip
        let bytes = value.to_le_bytes();
        assert_eq!(U256::from_le_bytes(bytes), value);
    }

    #[test]
    fn test_reverse_bits_single_bit() {
        // LSB of entire 256-bit integer set
        let x = U256::new(1, 0, 0, 0);
        let reversed = x.reverse_bits();
        // Should move to MSB of last limb
        assert_eq!(reversed, U256::new(0, 0, 0, 1u64 << 63));
    }

    #[test]
    fn test_reverse_bits_all_ones() {
        let x = U256::MAX;
        let reversed = x.reverse_bits();
        assert_eq!(reversed, U256::MAX);
    }

    #[test]
    fn test_reverse_bits_palindrome() {
        // Palindromic bit pattern: reversing should yield same value
        let x = U256::new(0x8000_0000_0000_0001, 0, 0, 0x8000_0000_0000_0001);
        let reversed = x.reverse_bits();
        assert_eq!(x, reversed);
    }

    #[test]
    fn test_leading_bits() {
        let x = U256::new(0, 0, 0, 1 << 63);
        assert_eq!(x.leading_zeros(), 0); // MSB set
        let y = U256::new(0, 0, 0, 0);
        assert_eq!(y.leading_zeros(), 256); // all zero
        let z = U256::new(0, 0, 0, u64::MAX >> 1);
        assert_eq!(z.leading_ones(), 0); // starts with 0
    }
}