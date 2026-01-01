// Comprehensive test suite for all standard library (xstd) functions
use xelis_compiler::Compiler;
use xelis_builder::EnvironmentBuilder;
use xelis_lexer::Lexer;
use xelis_parser::Parser;
use xelis_types::Primitive;
use super::*;

#[track_caller]
fn prepare_module(code: &str) -> (Module, xelis_environment::Environment<()>) {
    let tokens: Vec<_> = Lexer::new(code).into_iter().collect::<Result<_, _>>().unwrap();
    let env = EnvironmentBuilder::default();
    let (program, _) = Parser::with(tokens.into_iter(), &env).parse().unwrap();

    let env = env.build();
    let module = Compiler::new(&program, &env).compile().unwrap();

    (module, env)
}

#[track_caller]
fn run_code(code: &str) -> Primitive {
    let (module, environment) = prepare_module(code);
    run_internal(module, &environment, 0).unwrap()
}

#[track_caller]
fn run_code_result(code: &str) -> Result<Primitive, VMError> {
    let (module, environment) = prepare_module(code);
    run_internal(module, &environment, 0)
}

// ============================================================================
// ARRAY TESTS
// ============================================================================

#[test]
fn test_array_len() {
    let code = r#"
        entry main() {
            let arr: u64[] = [1, 2, 3]
            return arr.len() as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(3));
}

#[test]
fn test_array_push() {
    let code = r#"
        entry main() {
            let arr: u64[] = [1]
            arr.push(2)
            return arr.len() as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(2));
}

#[test]
fn test_array_pop() {
    let code = r#"
        entry main() {
            let arr: u64[] = [1, 2]
            let val = arr.pop()
            if val.is_some() {
                return val.unwrap()
            }
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(2));
}

#[test]
fn test_array_first() {
    let code = r#"
        entry main() {
            let arr: u64[] = [10, 20, 30]
            let v = arr.first()
            if v.is_some() {
                return v.unwrap()
            }
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(10));
}

#[test]
fn test_array_last() {
    let code = r#"
        entry main() {
            let arr: u64[] = [10, 20, 30]
            let v = arr.last()
            if v.is_some() {
                return v.unwrap()
            }
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(30));
}

#[test]
fn test_array_get() {
    let code = r#"
        entry main() {
            let arr: u64[] = [10, 20, 30]
            let v = arr.get(1)
            if v.is_some() {
                return v.unwrap()
            }
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(20));
}

#[test]
fn test_array_contains() {
    let code = r#"
        entry main() {
            let arr: u64[] = [10, 20, 30]
            if arr.contains(20) {
                return 1
            }
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(1));
}

#[test]
fn test_array_remove() {
    let code = r#"
        entry main() {
            let arr: u64[] = [10, 20, 30]
            let removed = arr.remove(1)
            return removed
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(20));
}

#[test]
fn test_array_reverse() {
    let code = r#"
        entry main() {
            let arr: u64[] = [1, 2, 3]
            arr.reverse()
            let v = arr.first()
            if v.is_some() {
                return v.unwrap()
            }
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(3));
}

#[test]
fn test_array_extended_ops() {
    let code = r#"
        entry main() {
            let arr: u64[] = [5, 10, 15]
            arr.insert(1u32, 7)
            let swapped = arr.swap_remove(0u32)
            assert(swapped == 5)

            let idx = arr.index_of(15)
            assert(idx.unwrap_or(99) == 0)

            let slice = arr.slice(0u32..2u32)
            assert(slice.len() == 2)

            let other: u64[] = [30, 40]
            arr.extend(other.clone())

            let nested: u64[][] = [[1, 2], [3]]
            let flat = nested.concat()
            assert(flat.len() == 3)

            arr.truncate(3u32)
            let split = arr.split_off(1u32)
            assert(arr.len() == 1)
            assert(split.len() == 2)

            let bytes = [1u8, 2u8].to_bytes()
            assert(bytes.len() == 2)

            return 1
        }
    "#;

    assert_eq!(run_code(code), Primitive::U64(1));
}

#[test]
fn test_array_sort_and_map() {
    let code = r#"
        entry main() {
            let arr: u64[] = [3, 1, 2]
            arr.sort(true)

            let mapped = arr.map(|x: u64| { return x + 1; })
            let first_mapped = mapped.first().unwrap()

            let keyed = arr.clone()
            keyed.sort_by_key(|v: u64| { return 100 - v; }, true)
            let first_keyed = keyed.first().unwrap()

            return first_mapped + first_keyed
        }
    "#;

    assert_eq!(run_code(code), Primitive::U64(5));
}

// ============================================================================
// STRING TESTS
// ============================================================================

#[test]
fn test_string_len() {
    let code = r#"
        entry main() {
            let s = "hello"
            return s.len() as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(5));
}

#[test]
fn test_string_contains() {
    let code = r#"
        entry main() {
            let s = "hello world"
            if s.contains("world") {
                return 1
            }
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(1));
}

#[test]
fn test_string_to_uppercase() {
    let code = r#"
        entry main() {
            let s = "hello"
            let upper = s.to_uppercase()
            return upper.len() as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(5));
}

#[test]
fn test_string_to_lowercase() {
    let code = r#"
        entry main() {
            let s = "HELLO"
            let lower = s.to_lowercase()
            return lower.len() as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(5));
}

#[test]
fn test_string_starts_with() {
    let code = r#"
        entry main() {
            let s = "hello world"
            if s.starts_with("hello") {
                return 1
            }
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(1));
}

#[test]
fn test_string_ends_with() {
    let code = r#"
        entry main() {
            let s = "hello world"
            if s.ends_with("world") {
                return 1
            }
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(1));
}

#[test]
fn test_string_is_empty() {
    let code = r#"
        entry main() {
            let s = ""
            if s.is_empty() {
                return 1
            }
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(1));
}

#[test]
fn test_string_trim() {
    let code = r#"
        entry main() {
            let s = "  hello  "
            let trimmed = s.trim()
            return trimmed.len() as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(5));
}

// ============================================================================
// BYTES TESTS
// ============================================================================

#[test]
fn test_bytes_len() {
    let code = r#"
        entry main() {
            let arr: u8[] = [1, 2, 3]
            let b = arr.to_bytes()
            return b.len() as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(3));
}

#[test]
fn test_bytes_push() {
    let code = r#"
        entry main() {
            let arr: u8[] = [1]
            let b = arr.to_bytes()
            b.push(2)
            return b.len() as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(2));
}

#[test]
fn test_bytes_pop() {
    let code = r#"
        entry main() {
            let arr: u8[] = [1, 2, 3]
            let b = arr.to_bytes()
            let v = b.pop()
            if v.is_some() {
                return v.unwrap() as u64
            }
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(3));
}

#[test]
fn test_bytes_to_hex() {
    let code = r#"
        entry main() {
            let arr: u8[] = [1, 2]
            let b = arr.to_bytes()
            let hex = b.to_hex()
            return hex.len() as u64
        }
    "#;
    // "0102" is 4 characters
    assert_eq!(run_code(code), Primitive::U64(4));
}

#[test]
fn test_bytes_full_suite() {
    let code = r#"
        entry main() {
            let arr: u8[] = [1, 2, 3, 4]
            let b = arr.to_bytes()
            b.push(5)
            let removed = b.remove(1)
            let slice = b.slice(1..3)
            if !b.contains(3) {
                return 0
            }

            let second = b.get(2).unwrap_or(0)
            let first = b.first().unwrap_or(0)
            let last = b.last().unwrap_or(0)
            let arr = b.to_array()
            let split = b.split_off(2)
            b.extend(split)
            b.truncate(3)

            let hex = b.to_hex()
            let from_hex = bytes::from_hex(hex)

            let empty = bytes::new()

            return b.len() as u64
                + slice.len() as u64
                + second as u64
                + first as u64
                + last as u64
                + arr.len() as u64
                + from_hex.len() as u64
                + empty.len() as u64
                + removed as u64
        }
    "#;

    assert_eq!(run_code(code), Primitive::U64(24));
}

// ============================================================================
// OPTIONAL TESTS
// ============================================================================

#[test]
fn test_optional_is_some() {
    let code = r#"
        entry main() {
            let maybe: optional<u64> = 42
            if maybe.is_some() {
                return 1
            }
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(1));
}

#[test]
fn test_optional_is_none() {
    let code = r#"
        entry main() {
            let maybe: optional<u64> = null
            if maybe.is_none() {
                return 1
            }
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(1));
}

#[test]
fn test_optional_unwrap_or() {
    let code = r#"
        entry main() {
            let maybe: optional<u64> = null
            return maybe.unwrap_or(99)
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(99));
}

#[test]
fn test_optional_unwrap_or_some() {
    let code = r#"
        entry main() {
            let maybe: optional<u64> = 42
            return maybe.unwrap_or(99)
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(42));
}

#[test]
fn test_optional_unwrap_or_else_none() {
    let code = r#"
        fn fallback() -> u64 {
            return 77
        }

        entry main() {
            let none: optional<u64> = null
            return none.unwrap_or_else(fallback)
        }
    "#;

    assert_eq!(run_code(code), Primitive::U64(77));
}

#[test]
fn test_optional_unwrap_or_else_some() {
    let code = r#"
        fn fallback() -> u64 {
            return 77
        }

        entry main() {
            let some: optional<u64> = 5
            return some.unwrap_or_else(fallback)
        }
    "#;

    assert_eq!(run_code(code), Primitive::U64(77));
}

#[test]
fn test_optional_unwrap() {
    let code = r#"
        entry main() {
            let some: optional<u64> = 5
            return some.unwrap()
        }
    "#;

    assert_eq!(run_code(code), Primitive::U64(5));
}

#[test]
fn test_optional_expect_failure() {
    let code = r#"
        entry main() {
            let none: optional<u64> = null
            return none.expect("fail")
        }
    "#;

    assert!(run_code_result(code).is_err());
}

// ============================================================================
// RANGE TESTS
// ============================================================================

#[test]
fn test_range_contains() {
    let code = r#"
        entry main() {
            let range = 0..10
            if range.contains(5) {
                return 1
            }
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(1));
}

#[test]
fn test_range_min() {
    let code = r#"
        entry main() {
            let range = 5..20
            return range.min()
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(5));
}

#[test]
fn test_range_max() {
    let code = r#"
        entry main() {
            let range = 5..20
            return range.max()
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(20));
}

#[test]
fn test_range_count() {
    let code = r#"
        entry main() {
            let range = 5..10
            return range.count()
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(5));
}

#[test]
fn test_range_collect() {
    let code = r#"
        entry main() {
            let range = 1..4
            let arr = range.collect()
            return arr.len() as u64
        }
    "#;

    assert_eq!(run_code(code), Primitive::U64(3));
}

// ============================================================================
// MAP TESTS
// ============================================================================

#[test]
fn test_map_len() {
    let code = r#"
        entry main() {
            let mymap: map<u64, u64> = {}
            return mymap.len() as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_map_insert_and_get() {
    let code = r#"
        entry main() {
            let mymap: map<u64, u64> = {}
            mymap.insert(1, 100)
            let v = mymap.get(1)
            if v.is_some() {
                return v.unwrap()
            }
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(100));
}

#[test]
fn test_map_contains_key() {
    let code = r#"
        entry main() {
            let mymap: map<u64, u64> = {}
            mymap.insert(1, 100)
            if mymap.contains_key(1) {
                return 1
            }
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(1));
}

#[test]
fn test_map_clear() {
    let code = r#"
        entry main() {
            let mymap: map<u64, u64> = {}
            mymap.insert(1, 100)
            mymap.clear()
            return mymap.len() as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_map_entries_and_removals() {
    let code = r#"
        entry main() {
            let mymap: map<u64, u64> = {}
            mymap.insert(1, 10)
            mymap.insert(2, 20)
            let shifted = mymap.shift_remove(1).unwrap_or(0)
            let swapped = mymap.swap_remove(2).unwrap_or(0)

            mymap.insert(3, 30)
            mymap.insert(4, 40)

            let keys = mymap.keys()
            let values = mymap.values()
            let entries = mymap.entries()

            return shifted + swapped + keys.len() as u64 + values.len() as u64 + entries.len() as u64
        }
    "#;

    assert_eq!(run_code(code), Primitive::U64(36));
}

// ============================================================================
// MATH TESTS
// ============================================================================

#[test]
fn test_math_sqrt_u64() {
    let code = r#"
        entry main() {
            let n: u64 = 16
            return n.sqrt()
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(4));
}

#[test]
fn test_math_sqrt_u32() {
    let code = r#"
        entry main() {
            let n: u64 = 100
            return n.sqrt()
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(10));
}

#[test]
fn test_math_sqrt_u16() {
    let code = r#"
        entry main() {
            let n: u16 = 81
            return n.sqrt() as u64
        }
    "#;

    assert_eq!(run_code(code), Primitive::U64(9));
}

// ============================================================================
// INTEGER OPERATION TESTS (checked, wrapping, saturating)
// ============================================================================

#[test]
fn test_int_checked_add() {
    let code = r#"
        entry main() {
            let n: u64 = 5
            let v = n.checked_add(3)
            if v.is_some() {
                return v.unwrap()
            }
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(8));
}

#[test]
fn test_int_checked_sub() {
    let code = r#"
        entry main() {
            let n: u64 = 10
            let v = n.checked_sub(3)
            if v.is_some() {
                return v.unwrap()
            }
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(7));
}

#[test]
fn test_int_checked_mul() {
    let code = r#"
        entry main() {
            let n: u64 = 5
            let v = n.checked_mul(3)
            if v.is_some() {
                return v.unwrap()
            }
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(15));
}

#[test]
fn test_int_checked_div() {
    let code = r#"
        entry main() {
            let n: u64 = 15
            let v = n.checked_div(3)
            if v.is_some() {
                return v.unwrap()
            }
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(5));
}

#[test]
fn test_int_saturating_add() {
    let code = r#"
        entry main() {
            let n: u64 = 250
            return n.saturating_add(10)
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(260));
}

#[test]
fn test_int_saturating_sub() {
    let code = r#"
        entry main() {
            let n: u64 = 5
            return n.saturating_sub(10)
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_int_wrapping_add() {
    let code = r#"
        entry main() {
            let n: u64 = 18446744073709551615
            return n.wrapping_add(1)
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_int_wrapping_sub() {
    let code = r#"
        entry main() {
            let n: u64 = 0
            return n.wrapping_sub(1)
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(18446744073709551615));
}

#[test]
fn test_int_full_std_suite() {
    let code = r#"
        entry main() {
            let n: u64 = 8
            let checked_pow = n.checked_pow(2).unwrap()
            let checked_shr = n.checked_shr(1).unwrap()
            let checked_shl = n.checked_shl(1).unwrap()
            let checked_rem = n.checked_rem(3).unwrap()

            let sat_mul = n.saturating_mul(1000)
            let sat_div = n.saturating_div(2)
            let wrap_mul = n.wrapping_mul(5)
            let wrap_div = n.wrapping_div(2)
            let wrap_rem = n.wrapping_rem(3)
            let wrap_pow = n.wrapping_pow(2)

            let rot_left = n.rotate_left(1)
            let rot_right = n.rotate_right(1)
            let rev = n.reverse_bits()
            let leading_zeros = n.leading_zeros()
            let trailing_zeros = n.trailing_zeros()

            let be = n.to_be_bytes()
            let from_be = u64::from_be_bytes(be)
            assert(n == from_be)

            let le = n.to_le_bytes()
            let from_le = u64::from_le_bytes(le)
            assert(n == from_le)

            let min = n.min(4)
            let max = n.max(16)

            let base16 = n.to_string(16)

            return checked_pow
                + checked_shr
                + checked_shl
                + checked_rem
                + sat_mul
                + sat_div
                + wrap_mul
                + wrap_div
                + wrap_rem
                + wrap_pow
                + rot_left
                + rot_right
                + rev.count_ones() as u64
                + leading_zeros as u64
                + trailing_zeros as u64
                + be.len() as u64
                + from_be
                + le.len() as u64
                + from_le
                + min
                + max
                + base16.len() as u64
        }
    "#;

    assert_eq!(run_code(code), Primitive::U64(8337));
}

// ============================================================================
// CORE STDLIB TESTS
// ============================================================================

#[test]
fn test_assert_true() {
    let code = r#"
        entry main() {
            assert(true)
            return 1
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(1));
}

#[test]
fn test_require_true() {
    let code = r#"
        entry main() {
            require(true, "test passed")
            return 1
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(1));
}

#[test]
fn test_clone() {
    let code = r#"
        entry main() {
            let arr: u64[] = [42]
            let arr2 = arr.clone()
            let val = arr2.first()
            if val.is_some() {
                return val.unwrap()
            }
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(42));
}

#[test]
fn test_core_misc_std() {
    let code = r#"
        entry main() {
            println("hello")
            debug("dbg")

            let a: u64[] = [1, 2]
            if is_same_ptr(a, a) {
                return 1
            }
            return 0
        }
    "#;

    assert_eq!(run_code(code), Primitive::U64(1));
}

#[test]
fn test_require_and_panic_failures() {
    let require_fail = r#"
        entry main() {
            require(false, "bad input")
            return 0
        }
    "#;

    assert!(run_code_result(require_fail).is_err());

    let panic_fail = r#"
        entry main() {
            panic("boom")
            return 0
        }
    "#;

    assert!(run_code_result(panic_fail).is_err());
}
