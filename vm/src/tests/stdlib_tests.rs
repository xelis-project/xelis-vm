// Comprehensive test suite for all standard library (xstd) functions
use silex_compiler::Compiler;
use silex_builder::EnvironmentBuilder;
use silex_lexer::Lexer;
use silex_parser::Parser;
use silex_types::Primitive;
use super::*;

#[track_caller]
fn prepare_module(code: &str) -> (Module, silex_environment::Environment<()>) {
    let tokens: Vec<_> = Lexer::new(code).into_iter().collect::<Result<_, _>>().unwrap();
    let env = EnvironmentBuilder::default();
    let (program, _) = Parser::with(tokens.into_iter(), &env).parse().unwrap();

    let env = env.build();
    let module = Compiler::new(&program, &env).compile().unwrap();

    (module, env)
}

#[track_caller]
fn run_code_result_id(code: &str, id: u16) -> Result<Primitive, VMError> {
    let (module, environment) = prepare_module(code);
    run_internal(module, &environment, id)
}

#[track_caller]
fn run_code_id(code: &str, id: u16) -> Primitive {
    run_code_result_id(code, id).unwrap()
}

#[track_caller]
fn run_code(code: &str) -> Primitive {
    run_code_id(code, 0)
}

#[track_caller]
fn run_code_result(code: &str) -> Result<Primitive, VMError> {
    run_code_result_id(code, 0)
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

#[test]
fn test_iter_once() {
    let code = r#"
        entry main() {
            let it = Iterator::once(42u64);
            let v: optional<u64> = it.next();
            return v.unwrap()
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(42));
}

#[test]
fn test_iter_empty_count() {
    let code = r#"
        entry main() {
            let it = Iterator::empty();
            return it.count() as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_array_iter_collect() {
    let code = r#"
        entry main() {
            let arr: u64[] = [10, 20, 30];
            let collected: u64[] = arr.iter().collect();
            return collected.len() as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(3));
}

#[test]
fn test_iter_next() {
    let code = r#"
        entry main() {
            let arr: u64[] = [10, 20, 30];
            let it = arr.iter();
            let a: u64 = it.next().unwrap();
            let b: u64 = it.next().unwrap();
            return a + b
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(30));
}

#[test]
fn test_iter_count() {
    let code = r#"
        entry main() {
            let arr: u64[] = [1, 2, 3, 4, 5];
            let it = arr.iter();
            return it.count() as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(5));
}

#[test]
fn test_iter_skip() {
    let code = r#"
        entry main() {
            let arr: u64[] = [1, 2, 3, 4, 5];
            let it = arr.iter().skip(2u32);
            return it.count() as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(3));
}

#[test]
fn test_iter_take() {
    let code = r#"
        entry main() {
            let arr: u64[] = [1, 2, 3, 4, 5];
            let it = arr.iter().take(3u32);
            return it.count() as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(3));
}

#[test]
fn test_iter_skip_take_collect() {
    let code = r#"
        entry main() {
            let arr: u64[] = [10, 20, 30, 40, 50];
            let result: u64[] = arr.iter().skip(1u32).take(3u32).collect();
            return result[0] + result[1] + result[2]
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(90)); // 20 + 30 + 40
}

#[test]
fn test_iter_chain() {
    let code = r#"
        entry main() {
            let a: u64[] = [1, 2];
            let b: u64[] = [3, 4];
            let it = a.iter().chain(b.iter());
            let result: u64[] = it.collect();
            return result[0] + result[1] + result[2] + result[3]
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(10));
}

#[test]
fn test_iter_rev() {
    let code = r#"
        entry main() {
            let arr: u64[] = [1, 2, 3];
            let result: u64[] = arr.iter().rev().collect();
            return result[0] * 100 + result[1] * 10 + result[2]
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(321));
}

#[test]
fn test_iter_enumerate() {
    let code = r#"
        entry main() {
            let arr: u64[] = [10, 20, 30];
            let it = arr.iter().enumerate();
            let (idx, val): (u32, u64) = it.next().unwrap();
            return idx as u64 + val
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(10)); // 0 + 10
}

#[test]
fn test_iter_zip() {
    let code = r#"
        entry main() {
            let a: u64[] = [1, 2, 3];
            let b: u64[] = [10, 20, 30];
            let it = a.iter().zip(b.iter());
            let result: any[] = it.collect();
            let first: u64[] = result[0];
            let second: u64[] = result[1];
            return first[0] + first[1] + second[0] + second[1]
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(33)); // 1+10 + 2+20
}

#[test]
fn test_iter_map() {
    let code = r#"
        entry main() {
            let arr: u64[] = [1, 2, 3];
            let doubled: u64[] = arr.iter().map(|x: u64| { return x * 2 }).collect();
            return doubled[0] + doubled[1] + doubled[2]
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(12)); // 2+4+6
}

#[test]
fn test_iter_filter() {
    let code = r#"
        entry main() {
            let arr: u64[] = [1, 2, 3, 4, 5, 6];
            let evens: u64[] = arr.iter().filter(|x: u64| { return x % 2 == 0 }).collect();
            return evens.len() as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(3));
}

#[test]
fn test_iter_filter_values() {
    let code = r#"
        entry main() {
            let arr: u64[] = [1, 2, 3, 4, 5, 6];
            let evens: u64[] = arr.iter().filter(|x: u64| { return x % 2 == 0 }).collect();
            return evens[0] + evens[1] + evens[2]
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(12)); // 2+4+6
}

#[test]
fn test_iter_map_filter_chain() {
    let code = r#"
        entry main() {
            let arr: u64[] = [1, 2, 3, 4, 5];
            let result: u64[] = arr.iter()
                .map(|x: u64| { return x * 2 })
                .filter(|x: u64| { return x > 4 })
                .collect();
            return result.len() as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(3)); // 6, 8, 10 pass the filter
}

#[test]
fn test_iter_for_each() {
    let code = r#"
        entry main() {
            let arr: u64[] = [1, 2, 3];
            let sum: u64[] = [0];
            arr.iter().for_each(|x: u64| {
                sum[0] += x
            });
            return sum[0]
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(6));
}

#[test]
fn test_iter_find() {
    let code = r#"
        entry main() {
            let arr: u64[] = [1, 2, 3, 4, 5];
            let found: optional<u64> = arr.iter().find(|x: u64| { return x > 3 });
            return found.unwrap()
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(4));
}

#[test]
fn test_iter_find_none() {
    let code = r#"
        entry main() {
            let arr: u64[] = [1, 2, 3];
            let found: optional<u64> = arr.iter().find(|x: u64| { return x > 10 });
            return found.is_none() as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(1));
}

#[test]
fn test_iter_any_true() {
    let code = r#"
        entry main() {
            let arr: u64[] = [1, 2, 3];
            return arr.iter().any(|x: u64| { return x == 2 }) as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(1));
}

#[test]
fn test_iter_any_false() {
    let code = r#"
        entry main() {
            let arr: u64[] = [1, 2, 3];
            return arr.iter().any(|x: u64| { return x == 10 }) as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_iter_all_true() {
    let code = r#"
        entry main() {
            let arr: u64[] = [2, 4, 6];
            return arr.iter().all(|x: u64| { return x % 2 == 0 }) as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(1));
}

#[test]
fn test_iter_all_false() {
    let code = r#"
        entry main() {
            let arr: u64[] = [2, 3, 6];
            return arr.iter().all(|x: u64| { return x % 2 == 0 }) as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_iter_fold() {
    let code = r#"
        entry main() {
            let arr: u64[] = [1, 2, 3, 4, 5];
            let sum: u64 = arr.iter().fold(0u64, |acc: u64, x: u64| { return acc + x });
            return sum
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(15));
}

#[test]
fn test_iter_fold_product() {
    let code = r#"
        entry main() {
            let arr: u64[] = [1, 2, 3, 4, 5];
            let product: u64 = arr.iter().fold(1u64, |acc: u64, x: u64| { return acc * x });
            return product
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(120));
}

#[test]
fn test_iter_sum() {
    let code = r#"
        entry main() {
            let arr: u64[] = [1, 2, 3, 4, 5];
            return arr.iter().sum()
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(15));
}

#[test]
fn test_iter_position() {
    let code = r#"
        entry main() {
            let arr: u64[] = [10, 20, 30, 40];
            let pos: optional<u32> = arr.iter().position(|x: u64| { return x == 30 });
            return pos.unwrap() as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(2));
}

#[test]
fn test_iter_flatten() {
    let code = r#"
        entry main() {
            let nested: u64[][] = [[1, 2], [3, 4], [5]];
            let flat: u64[] = nested.iter().flatten().collect();
            return flat.len() as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(5));
}

#[test]
fn test_iter_complex_pipeline() {
    // sum of squares of even numbers in 1..=10
    let code = r#"
        entry main() {
            let arr: u64[] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
            let result: u64 = arr.iter()
                .filter(|x: u64| { return x % 2 == 0 })
                .map(|x: u64| { return x * x })
                .fold(0u64, |acc: u64, x: u64| { return acc + x });
            return result
        }
    "#;
    // 4 + 16 + 36 + 64 + 100 = 220
    assert_eq!(run_code(code), Primitive::U64(220));
}

#[test]
fn test_iter_chain_and_collect() {
    let code = r#"
        entry main() {
            let a: u64[] = [1, 2, 3];
            let b: u64[] = [4, 5, 6];
            let c = Iterator::once(7u64);
            let result: u64[] = a.iter().chain(b.iter()).chain(c).collect();
            return result.len() as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(7));
}

#[test]
fn test_iter_typed_filter_collect() {
    // filter preserves the element type: collect() should return u64[]
    let code = r#"
        entry main() {
            let arr: u64[] = [1, 2, 3, 4, 5, 6];
            let result: u64[] = arr.iter().filter(|x: u64| { return x > 3 }).collect();
            return result.len() as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(3));
}

#[test]
fn test_iter_typed_skip_take_collect() {
    // skip/take preserve the element type
    let code = r#"
        entry main() {
            let arr: u64[] = [10, 20, 30, 40, 50];
            let result: u64[] = arr.iter().skip(1u32).take(3u32).collect();
            return result[0] + result[1] + result[2]
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(90)); // 20 + 30 + 40
}

#[test]
fn test_iter_typed_rev_collect() {
    // rev preserves the element type
    let code = r#"
        entry main() {
            let arr: u64[] = [1, 2, 3];
            let result: u64[] = arr.iter().rev().collect();
            return result[0] * 100 + result[1] * 10 + result[2]
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(321));
}

#[test]
fn test_iter_typed_chain_collect() {
    // chain with same-type iterators preserves element type
    let code = r#"
        entry main() {
            let a: u64[] = [1, 2];
            let b: u64[] = [3, 4];
            let result: u64[] = a.iter().chain(b.iter()).collect();
            return result[0] + result[1] + result[2] + result[3]
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(10));
}

#[test]
fn test_iter_typed_map_collect() {
    // map returns Iterator<T(1)> where T(1) is inferred from the closure's return type
    let code = r#"
        entry main() {
            let arr: u64[] = [1, 2, 3];
            let iter1: Iterator<u64> = arr.iter();
            let iter2: Iterator<u64> = iter1.map(|x: u64| { return x * 2 });

            let doubled: u64[] = iter2.collect();
            assert(doubled.len() == 3);
            assert(doubled[0] == 2);
            assert(doubled[1] == 4);
            assert(doubled[2] == 6);

            return doubled[0] + doubled[1] + doubled[2]
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(12)); // 2 + 4 + 6
}

#[test]
fn test_iter_typed_next_optional() {
    // next() on Iterator<u64> returns optional<u64>
    let code = r#"
        entry main() {
            let arr: u64[] = [42];
            let it = arr.iter();
            let v: optional<u64> = it.next();
            return v.unwrap()
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(42));
}

#[test]
fn test_iter_typed_find_optional() {
    // find() on Iterator<u64> returns optional<u64>
    let code = r#"
        entry main() {
            let arr: u64[] = [1, 2, 3, 4];
            let result: optional<u64> = arr.iter().find(|x: u64| { return x > 2 });
            return result.unwrap()
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(3));
}

#[test]
fn test_iter_typed_fold() {
    // fold with T(0) = u64 accumulator
    let code = r#"
        entry main() {
            let arr: u64[] = [1, 2, 3, 4, 5];
            let sum: u64 = arr.iter().fold(0u64, |acc: u64, x: u64| { return acc + x });
            return sum
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(15));
}

#[test]
fn test_iter_typed_enumerate_destructure() {
    // enumerate() returns Iterator<(u32, u64)>; destructure with let (i, v)
    let code = r#"
        entry main() {
            let arr: u64[] = [10, 20, 30];
            let it = arr.iter().enumerate();
            let (idx, val): (u32, u64) = it.next().unwrap();
            return idx as u64 + val
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(10)); // 0 + 10
}

#[test]
fn test_iter_typed_enumerate_collect() {
    // enumerate().collect() returns (u32, u64)[]  (tuple array)
    let code = r#"
        entry main() {
            let arr: u64[] = [10, 20, 30];
            let pairs = arr.iter().enumerate().collect();
            let (idx0, val0): (u32, u64) = pairs[0];
            let (idx1, val1): (u32, u64) = pairs[1];
            return idx0 as u64 + val0 + idx1 as u64 + val1
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(31)); // 0+10 + 1+20
}

#[test]
fn test_iter_typed_flatten_array() {
    // flatten on Iterator<u64[]> → Iterator<u64>
    let code = r#"
        entry main() {
            let nested: u64[][] = [[1, 2], [3, 4], [5]];
            let flat: u64[] = nested.iter().flatten().collect();
            return flat.len() as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(5));
}

#[test]
fn test_iter_typed_flatten_iter() {
    // flatten on Iterator<Iterator<u64>> → Iterator<u64>
    let code = r#"
        entry main() {
            let a: u64[] = [1, 2];
            let b: u64[] = [3, 4];
            let nested = [a.iter(), b.iter()];
            let flat: u64[] = nested.iter().flatten().collect();
            return flat[0] + flat[1] + flat[2] + flat[3]
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(10));
}

#[test]
fn test_iter_static_once_collect() {
    // Iterator::once produces a single-element iterator
    let code = r#"
        entry main() {
            let it = Iterator::once(99u64);
            let v: optional<u64> = it.next();
            return v.unwrap()
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(99));
}

#[test]
fn test_iter_static_empty_collect() {
    // Iterator::empty produces an empty iterator
    let code = r#"
        entry main() {
            let it = Iterator::empty();
            return it.count() as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_iter_static_unfold_collect() {
    // Iterator::unfold(seed, f) builds items until f returns null
    let code = r#"
        entry main() {
            let generated: u64[] = Iterator::unfold(1u64, |state: u64| {
                if state <= 5u64 {
                    return (state * 2u64, state + 1u64)
                }
                return null
            }).collect();
            assert(generated == [2, 4, 6, 8, 10]);

            return generated[0] + generated[1] + generated[2] + generated[3] + generated[4]
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(30)); // 2 + 4 + 6 + 8 + 10
}

#[test]
fn test_iter_static_unfold_empty() {
    // unfold can terminate by returning null after yielding values
    let code = r#"
        entry main() {
            let generated: u64[] = Iterator::unfold(1u64, |state: u64| {
                if state <= 3u64 {
                    return (state, state + 1u64)
                }
                return null
            }).collect();
            assert(generated == [1, 2, 3]);

            return generated.len() as u64 + generated[0] + generated[1] + generated[2]
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(9)); // len(3) + 1 + 2 + 3
}

#[test]
fn test_iter_unfold_count() {
    // unfold can terminate by returning null after yielding values
    let code = r#"
        entry main() {
            let count: u64 = Iterator::unfold(1u64, |state: u64| {
                if state <= 3u64 {
                    return (state, state + 1u64)
                }
                return null
            }).count() as u64;

            return count
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(3)); // 3 items yielded, then count returns 3
}

#[test]
fn test_iter_unfold_in_another_function() {
    // unfold can terminate by returning null after yielding values
    let code = r#"
        fn foo(init: u64) -> Iterator<u64> {
            return Iterator::unfold(init, |state: u64| {
                if state <= 3u64 {
                    return (state, state + 1u64)
                }
                return null
            })
        }

        entry main() {
            let count: u64 = foo(1u64).count() as u64;
            return count
        }
    "#;
    assert_eq!(run_code_id(code, 2), Primitive::U64(3));
}

#[test]
fn test_iter_typed_sum() {
    // sum() on Iterator<u64> returns u64
    let code = r#"
        entry main() {
            let arr: u64[] = [1, 2, 3, 4, 5];
            let s: u64 = arr.iter().sum();
            return s
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(15));
}

#[test]
fn test_iter_typed_position() {
    // position() returns optional<u32>
    let code = r#"
        entry main() {
            let arr: u64[] = [10, 20, 30, 40];
            let pos: optional<u32> = arr.iter().position(|x: u64| { return x == 30 });
            return pos.unwrap() as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(2));
}

#[test]
fn test_iter_typed_zip_collect() {
    // zip returns Iterator<(T(0), T(1))>; elements are typed tuples
    let code = r#"
        entry main() {
            let a: u64[] = [1, 2, 3];
            let b: u64[] = [10, 20, 30];
            let zipped: (u64, u64)[] = a.iter().zip(b.iter()).collect();
            let (x, y): (u64, u64) = zipped[0];
            return x + y
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(11)); // 1 + 10
}

#[test]
fn test_iter_filter_map_collect() {
    // chained filter -> map -> collect: map return type is inferred from closure return type
    let code = r#"
        entry main() {
            let arr: u64[] = [1, 2, 3, 4, 5];
            let result: u64[] = arr.iter()
                .filter(|x: u64| { return x % 2 == 0 })
                .map(|x: u64| { return x * 10 })
                .collect();
            return result[0] + result[1]
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(60)); // 20 + 40
}

#[test]
fn test_iter_static_unfold_zero_items() {
    // unfold that immediately returns null produces an empty iterator
    let code = r#"
        entry main() {
            let generated: u64[] = Iterator::unfold(0u64, |state: u64| {
                return null
            }).collect();
            return generated.len() as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_iter_complex_multi() {
    // complex pipeline with multiple iterator adaptors and a closure that captures an outer variable
    let code = r#"
        entry main() {
            let arr: u64[] = [1, 2, 3, 4, 5];
            let threshold: u64 = 10;
            let result: u64 = arr.iter()
                .chain(Iterator::once(6u64)) // add one more element
                .map(|x: u64| { return x * x }) // square each element
                .filter(|x: u64| { return x > threshold }) // keep squares > 10
                .fold(0u64, |acc: u64, x: u64| { return acc + x }) // sum them
                ;
            return result
        }
    "#;
    // squares are [1, 4, 9, 16, 25, 36]; filter >10 gives [16, 25, 36]; sum is 77
    assert_eq!(run_code(code), Primitive::U64(77));
}

#[test]
fn test_iter_map_lazy() {
    // Panic on map closure to verify laziness of iterators
    // (should not panic if map is lazy and we don't consume the iterator)
    let code = r#"
        entry main() {
            let arr: u64[] = [1, 2, 3];
            let it = arr.iter().map(|x: u64| {
                panic("map should not be called yet");
                return x * 2
            });
            // We don't consume the iterator, so the panic should not occur
            return 42
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(42));
}

#[test]
fn test_iter_filter_lazy() {
    // The filter predicate must not be called when the iterator is never consumed.
    let code = r#"
        entry main() {
            let arr: u64[] = [1, 2, 3];
            let it = arr.iter().filter(|x: u64| {
                panic("filter should not be called yet");
                return true
            });
            return 42
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(42));
}

#[test]
fn test_iter_map_then_filter() {
    // map then filter: map is eagerly evaluated when filter is applied (the VM
    // cannot compose nested closure callbacks lazily), then filter runs on collected results.
    let code = r#"
        entry main() {
            let arr: u64[] = [1, 2, 3, 4, 5];
            let result: u64[] = arr.iter()
                .map(|x: u64| { return x * 2u64 })
                .filter(|x: u64| { return x > 4u64 })
                .collect();
            return result.len() as u64
        }
    "#;
    // doubled: [2, 4, 6, 8, 10]; > 4: [6, 8, 10] -> len 3
    assert_eq!(run_code(code), Primitive::U64(3));
}

#[test]
fn test_iter_take_after_map() {
    // take(N) after map: only N results are returned even though the map
    // closure runs on all upstream items (eager collect at the map/take boundary).
    let code = r#"
        entry main() {
            let data: u32[] = [1, 2, 3, 4, 5];
            let result: u32[] = data.iter()
                .map(|x: u32| { return x * 10u32 })
                .take(2u32)
                .collect();
            assert(result.len() == 2);
            return result[0] as u64 + result[1] as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(30)); // 10 + 20
}

#[test]
fn test_iter_skip_after_map() {
    // skip(N) after map: the first N mapped items are dropped.
    let code = r#"
        entry main() {
            let data: u32[] = [10, 20, 30, 40, 50];
            let result: u32[] = data.iter()
                .map(|x: u32| { return x * 2u32 })
                .skip(2u32)
                .collect();
            // mapped: [20, 40, 60, 80, 100]; skip 2 -> [60, 80, 100]
            assert(result.len() == 3);
            return result[0] as u64 + result[1] as u64 + result[2] as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(240)); // 60 + 80 + 100
}

#[test]
fn test_iter_take_after_filter() {
    // take(N) after filter: only N matching items are returned.
    let code = r#"
        entry main() {
            let data: u32[] = [1, 2, 3, 4, 5, 6, 7, 8];
            let result: u32[] = data.iter()
                .filter(|x: u32| { return x % 2u32 == 0u32 })
                .take(2u32)
                .collect();
            // evens: [2, 4, 6, 8]; take 2 -> [2, 4]
            assert(result.len() == 2);
            return result[0] as u64 + result[1] as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(6)); // 2 + 4
}

#[test]
fn test_iter_deep_map_chain() {
    let code = r#"
        entry main() {
            let it: Iterator<u64> = Iterator::once(0u64);
            foreach _ in 0..100_000 {
                it = it.map(|x: u64| { return x + 1u64 });
            }
            return it.next().unwrap()
        }
    "#;
    assert!(run_code_result(code).is_err());
}

#[test]
fn test_iter_infinite_unfold() {
    // An infinite iterator from unfold should not be called
    let code = r#"
        entry main() {
            let it: Iterator<u64> = Iterator::unfold(0u64, |state: u64| {
                return (state, state + 1u64)
            });

            return it.next().unwrap()
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_iter_next_on_empty() {
    // next() on an empty iterator returns None
    let code = r#"
        entry main() {
            let it: Iterator<u64> = Iterator::empty();
            let v: optional<u64> = it.next();
            assert(v.is_none());
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_iter_next_on_non_empty() {
    let code = r#"
        entry main() {
            let it: Iterator<u64> = Iterator::once(42u64);
            let v = it.next();
            assert(v.is_some());

            assert(it.next().is_none());
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_iter_next_multiple() {
    // Calling next() after an iterator is exhausted should continue to return None
    let code = r#"
        entry main() {
            let it: Iterator<u64> = Iterator::once(42u64)
                .chain(Iterator::empty()) // should be ignored
                .chain(Iterator::once(99u64)); // should be reached

            assert(it.next() == 42); // first item
            assert(it.next() == 99); // now exhausted
            assert(it.next().is_none()); // None
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_iter_chain_lazy() {
    let code = r#"
        entry main() {
            let it: Iterator<u64> = Iterator::once(42u64)
                .chain(Iterator::empty()
                    .map(|x: u64| { return panic("second iterator should not be evaluated yet") })
                );

            assert(it.next() == 42);
            return 0
        }
    "#;

    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_iter_chain_map_local() {
    let code = r#"
        entry main() {
            let it: Iterator<u64> = Iterator::once(42u64)
                .chain(
                    Iterator::once(100u64).map(|x: u64| { return x + 1u64 })
                );

            assert(it.next() == 42);
            assert(it.next() == 101);
            return 0
        }
    "#;

    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_iter_map_next() {
    let code = r#"
        entry main() {
            let it: Iterator<u64> = Iterator::once(10u64).map(|x: u64| { return x * 2u64 });
            assert(it.next() == 20);
            assert(it.next().is_none());
            return 0
        }
    "#;

    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_iter_filter_lazy_once() {
    let code = r#"
        entry main() {
            // Verify filter is evaluated lazily: called once per next() call,
            // not eagerly for all items upfront.
            let call = 0;
            let it: Iterator<u64> = [10u64, 20u64].iter()
                .filter(|x: u64| {
                    call += 1;
                    return true
                });
            assert(it.next() == 10);
            assert(call == 1);     // filter called exactly once for first next()
            assert(it.next() == 20);
            assert(call == 2);     // filter called once more for second next()
            assert(it.next().is_none());
            assert(call == 2);     // source exhausted, filter not called again
            return 0
        }
    "#;

    assert_eq!(run_code(code), Primitive::U64(0));
}

// ============================================================================
// ITERATOR LAZINESS — next() call-count verification
// ============================================================================

#[test]
fn test_iter_next_map_call_count() {
    // map closure called exactly once per next(), never for items that aren't consumed.
    let code = r#"
        entry main() {
            let calls = 0;
            let it: Iterator<u64> = [1u64, 2u64, 3u64].iter()
                .map(|x: u64| { calls += 1; return x * 10u64 });
            assert(it.next() == 10);
            assert(calls == 1);
            assert(it.next() == 20);
            assert(calls == 2);
            assert(it.next() == 30);
            assert(calls == 3);
            assert(it.next().is_none());
            assert(calls == 3);  // no extra call on exhaustion
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_iter_next_filter_rejecting() {
    // filter pulls more items from source only when it rejects; total calls equal items tested.
    let code = r#"
        entry main() {
            let calls = 0;
            let it: Iterator<u64> = [1u64, 2u64, 3u64, 4u64].iter()
                .filter(|x: u64| { calls += 1; return x % 2u64 == 0u64 });
            assert(it.next() == 2);
            assert(calls == 2);   // 1 rejected, 2 accepted
            assert(it.next() == 4);
            assert(calls == 4);   // 3 rejected, 4 accepted
            assert(it.next().is_none());
            assert(calls == 4);   // source exhausted, no extra call
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_iter_next_map_then_filter_lazy() {
    // map + filter + next(): map is called only for items pulled from source,
    // filter may reject the mapped result, driving the next source pull.
    let code = r#"
        entry main() {
            let map_calls = 0;
            let filter_calls = 0;
            let it: Iterator<u64> = [1u64, 2u64, 3u64, 4u64].iter()
                .map(|x: u64| { map_calls += 1; return x * 2u64 })
                .filter(|x: u64| { filter_calls += 1; return x > 4u64 });
            // First next(): map(1)=2 filtered out, map(2)=4 filtered out, map(3)=6 passes.
            assert(it.next() == 6);
            assert(map_calls == 3);
            assert(filter_calls == 3);
            // Second next(): map(4)=8 passes immediately.
            assert(it.next() == 8);
            assert(map_calls == 4);
            assert(filter_calls == 4);
            assert(it.next().is_none());
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_iter_next_filter_then_map_lazy() {
    // filter + map + next(): map is NOT called for rejected items.
    let code = r#"
        entry main() {
            let map_calls = 0;
            let it: Iterator<u64> = [1u64, 2u64, 3u64, 4u64].iter()
                .filter(|x: u64| { return x % 2u64 == 0u64 })
                .map(|x: u64| { map_calls += 1; return x * 10u64 });
            assert(it.next() == 20);
            assert(map_calls == 1);  // item 1 rejected without map, item 2 mapped
            assert(it.next() == 40);
            assert(map_calls == 2);  // item 3 rejected without map, item 4 mapped
            assert(it.next().is_none());
            assert(map_calls == 2);
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

// ============================================================================
// ITERATOR LAZINESS — skip / take / enumerate with next()
// ============================================================================

#[test]
fn test_iter_next_skip_then_next() {
    let code = r#"
        entry main() {
            let it: Iterator<u64> = [10u64, 20u64, 30u64, 40u64].iter().skip(2u32);
            assert(it.next() == 30);
            assert(it.next() == 40);
            assert(it.next().is_none());
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_iter_next_take_exhausts_early() {
    // take(n) stops the iterator after n items even if the source has more.
    let code = r#"
        entry main() {
            let it: Iterator<u64> = [1u64, 2u64, 3u64, 4u64, 5u64].iter().take(2u32);
            assert(it.next() == 1);
            assert(it.next() == 2);
            assert(it.next().is_none());
            assert(it.next().is_none());  // stays None
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_iter_next_take_zero() {
    // take(0) means the iterator is empty before it starts.
    let code = r#"
        entry main() {
            let it: Iterator<u64> = [1u64, 2u64, 3u64].iter().take(0u32);
            assert(it.next().is_none());
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_iter_next_enumerate_state() {
    // enumerate() index must increment correctly across multiple next() calls.
    let code = r#"
        entry main() {
            let it = [100u64, 200u64, 300u64].iter().enumerate();
            let (i0, v0): (u32, u64) = it.next().unwrap();
            assert(i0 == 0); assert(v0 == 100);
            let (i1, v1): (u32, u64) = it.next().unwrap();
            assert(i1 == 1); assert(v1 == 200);
            let (i2, v2): (u32, u64) = it.next().unwrap();
            assert(i2 == 2); assert(v2 == 300);
            assert(it.next().is_none());
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

// ============================================================================
// ITERATOR LAZINESS — chain / unfold with next()
// ============================================================================

#[test]
fn test_iter_next_chain_transitions() {
    // chain: when the first source is exhausted, next() seamlessly moves to the second.
    let code = r#"
        entry main() {
            let it: Iterator<u64> = [1u64, 2u64].iter().chain([3u64, 4u64].iter());
            assert(it.next() == 1);
            assert(it.next() == 2);
            assert(it.next() == 3);
            assert(it.next() == 4);
            assert(it.next().is_none());
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_iter_next_chain_filter_both_sides() {
    // A filter placed after chain applies to items from both iterators.
    let code = r#"
        entry main() {
            let it: Iterator<u64> = [1u64, 2u64, 3u64].iter()
                .chain([4u64, 5u64, 6u64].iter())
                .filter(|x: u64| { return x % 2u64 == 0u64 });
            assert(it.next() == 2);
            assert(it.next() == 4);
            assert(it.next() == 6);
            assert(it.next().is_none());
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_iter_next_chain_map_second_lazy() {
    // The map on the chained iterator must not be called until that side is reached.
    let code = r#"
        entry main() {
            let map_calls = 0;
            let it: Iterator<u64> = Iterator::once(1u64)
                .chain(Iterator::once(2u64).map(|x: u64| {
                    map_calls += 1;
                    return x + 10u64
                }));
            assert(it.next() == 1);
            assert(map_calls == 0);  // second iterator not yet touched
            assert(it.next() == 12);
            assert(map_calls == 1);
            assert(it.next().is_none());
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_iter_next_unfold_map_lazy() {
    // unfold generates one state transition per next(); map wraps each generated item.
    let code = r#"
        entry main() {
            let map_calls = 0;
            let it: Iterator<u64> = Iterator::unfold(0u64, |s: u64| {
                if s >= 3u64 { return null }
                return (s, s + 1u64)
            }).map(|x: u64| { map_calls += 1; return x * 10u64 });
            assert(it.next() == 0);    assert(map_calls == 1);
            assert(it.next() == 10);   assert(map_calls == 2);
            assert(it.next() == 20);   assert(map_calls == 3);
            assert(it.next().is_none()); assert(map_calls == 3);
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_iter_next_unfold_filter_lazy() {
    // unfold + filter: the generator is only called as many times as needed to find a match.
    let code = r#"
        entry main() {
            let calls = 0;
            let it: Iterator<u64> = Iterator::unfold(0u64, |s: u64| {
                calls += 1;
                if s >= 5u64 { return null }
                return (s, s + 1u64)
            }).filter(|x: u64| { return x % 2u64 == 0u64 });
            // First next(): s=0 even, passes. calls=1.
            assert(it.next() == 0);
            assert(calls == 1);
            // Second next(): s=1 odd (reject, calls=2); s=2 even (passes, calls=3).
            assert(it.next() == 2);
            assert(calls == 3);
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

// ============================================================================
// ITERATOR LAZINESS — fallback adapters (rev / flatten / zip) with next()
// ============================================================================

#[test]
fn test_iter_next_rev_correct() {
    // rev forces full collection internally, but multiple next() calls still work.
    let code = r#"
        entry main() {
            let it: Iterator<u64> = [1u64, 2u64, 3u64].iter().rev();
            assert(it.next() == 3);
            assert(it.next() == 2);
            assert(it.next() == 1);
            assert(it.next().is_none());
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_iter_next_flatten_correct() {
    // flatten is lazy: next() expands one sub-array at a time.
    let code = r#"
        entry main() {
            let it: Iterator<u64> = [[1u64, 2u64], [3u64]].iter().flatten();
            assert(it.next() == 1);
            assert(it.next() == 2);
            assert(it.next() == 3);
            assert(it.next().is_none());
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_iter_next_zip_correct() {
    // zip forces full collection internally, multiple next() calls still yield pairs.
    let code = r#"
        entry main() {
            let it: Iterator<(u64, u64)> = [1u64, 2u64, 3u64].iter()
                .zip([10u64, 20u64, 30u64].iter());
            let (a, b): (u64, u64) = it.next().unwrap();
            assert(a == 1); assert(b == 10);
            let (c, d): (u64, u64) = it.next().unwrap();
            assert(c == 2); assert(d == 20);
            let (e, f): (u64, u64) = it.next().unwrap();
            assert(e == 3); assert(f == 30);
            assert(it.next().is_none());
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_iter_next_flatten_opaque_iterators() {
    // flatten of Iterator<Iterator<u64>> via next() must yield ALL inner items,
    // not just the first item from each inner iterator.
    let code = r#"
        entry main() {
            let a: u64[] = [1, 2];
            let b: u64[] = [3, 4];
            let it: Iterator<u64> = [a.iter(), b.iter()].iter().flatten();
            assert(it.next() == 1);
            assert(it.next() == 2);
            assert(it.next() == 3);
            assert(it.next() == 4);
            assert(it.next().is_none());
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_iter_skip_large_with_map_no_stackoverflow() {
    // A large skip before a map should not cause stack overflow.
    // Creates a 200-element array, skips 199, maps, and takes the last.
    let code = r#"
        entry main() {
            let arr: u64[] = [];
            foreach i in 0u64..200 {
                arr.push(i);
            }
            let it: Iterator<u64> = arr.iter()
                .skip(199u32)
                .map(|x: u64| { return x * 10u64 });
            return it.next().unwrap()
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(1990));
}

// ============================================================================
// ITERATOR EDGE CASES — boundary conditions
// ============================================================================

#[test]
fn test_iter_next_skip_beyond_length() {
    // skip(n) where n >= source length: every next() returns None.
    let code = r#"
        entry main() {
            let it: Iterator<u64> = [1u64, 2u64].iter().skip(10u32);
            assert(it.next().is_none());
            assert(it.next().is_none());
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_iter_next_take_beyond_length() {
    // take(n) where n > source length: iterator yields at most source.len() items.
    let code = r#"
        entry main() {
            let it: Iterator<u64> = [1u64, 2u64].iter().take(100u32);
            assert(it.next() == 1);
            assert(it.next() == 2);
            assert(it.next().is_none());
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_iter_next_after_exhaustion() {
    // next() called repeatedly on an exhausted iterator always returns None.
    let code = r#"
        entry main() {
            let it: Iterator<u64> = Iterator::once(1u64);
            let _ = it.next();
            assert(it.next().is_none());
            assert(it.next().is_none());
            assert(it.next().is_none());
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_iter_next_skip_then_filter() {
    // skip + filter together: skip discards first n items, filter runs on the rest.
    let code = r#"
        entry main() {
            let it: Iterator<u64> = [1u64, 2u64, 3u64, 4u64, 5u64].iter()
                .skip(2u32)
                .filter(|x: u64| { return x % 2u64 != 0u64 });
            // After skip(2): [3, 4, 5]; odd: [3, 5]
            assert(it.next() == 3);
            assert(it.next() == 5);
            assert(it.next().is_none());
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

// ============================================================================
// ITERATOR COMBINATIONS — collect-based terminals
// ============================================================================

#[test]
fn test_iter_empty_filter_collect() {
    // filter on an empty iterator produces an empty result.
    let code = r#"
        entry main() {
            let result: u64[] = Iterator::empty().filter(|x: u64| { return true }).collect();
            return result.len() as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_iter_filter_all_rejected() {
    // filter predicate that always returns false yields an empty collection.
    let code = r#"
        entry main() {
            let result: u64[] = [1u64, 2u64, 3u64].iter()
                .filter(|x: u64| { return false })
                .collect();
            return result.len() as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_iter_fold_empty() {
    // fold on an empty iterator returns the init value unchanged.
    let code = r#"
        entry main() {
            return Iterator::empty().fold(42u64, |acc: u64, x: u64| { return acc + x })
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(42));
}

#[test]
fn test_iter_any_on_empty() {
    // any on an empty iterator is always false.
    let code = r#"
        entry main() {
            return Iterator::empty().any(|x: u64| { return true }) as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_iter_all_on_empty() {
    // all on an empty iterator is vacuously true.
    let code = r#"
        entry main() {
            return Iterator::empty().all(|x: u64| { return false }) as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(1));
}

#[test]
fn test_iter_position_not_found() {
    // position returns None when the predicate never matches.
    let code = r#"
        entry main() {
            let pos: optional<u32> = [1u64, 2u64, 3u64].iter()
                .position(|x: u64| { return x > 100u64 });
            return pos.is_none() as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(1));
}

#[test]
fn test_iter_take_then_map_collect() {
    // take before map: only the taken slice is transformed.
    let code = r#"
        entry main() {
            let result: u64[] = [1u64, 2u64, 3u64, 4u64].iter()
                .take(2u32)
                .map(|x: u64| { return x * 10u64 })
                .collect();
            assert(result.len() == 2);
            return result[0] + result[1]
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(30)); // 10 + 20
}

#[test]
fn test_iter_chain_then_skip_collect() {
    // skip applied after chain spans across both iterators.
    let code = r#"
        entry main() {
            let result: u64[] = [1u64, 2u64].iter()
                .chain([3u64, 4u64, 5u64].iter())
                .skip(2u32)
                .collect();
            // chain: [1,2,3,4,5]; skip 2: [3,4,5]
            assert(result.len() == 3);
            return result[0] + result[1] + result[2]
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(12)); // 3+4+5
}

#[test]
fn test_iter_unfold_skip_take_collect() {
    // unfold with skip + take: only the needed range of values is generated.
    let code = r#"
        entry main() {
            let result: u64[] = Iterator::unfold(0u64, |s: u64| {
                if s >= 10u64 { return null }
                return (s, s + 1u64)
            })
            .skip(3u32)
            .take(4u32)
            .collect();
            // [0..9]; skip 3 -> [3..9]; take 4 -> [3,4,5,6]
            assert(result.len() == 4);
            return result[0] + result[1] + result[2] + result[3]
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(18)); // 3+4+5+6
}

#[test]
fn test_iter_flatten_then_filter_collect() {
    // flatten (via chain) then filter: filter runs on the individual elements.
    let code = r#"
        entry main() {
            let a: u64[] = [1u64, 2u64, 3u64];
            let b: u64[] = [4u64, 5u64, 6u64];
            let result: u64[] = a.iter()
                .chain(b.iter())
                .filter(|x: u64| { return x % 2u64 == 0u64 })
                .collect();
            assert(result.len() == 3);
            return result[0] + result[1] + result[2]
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(12)); // 2 + 4 + 6
}

#[test]
fn test_iter_zip_different_lengths() {
    // zip stops at the shorter iterator.
    let code = r#"
        entry main() {
            let a: u64[] = [1u64, 2u64, 3u64];
            let b: u64[] = [10u64, 20u64];
            let result: (u64, u64)[] = a.iter()
                .zip(b.iter())
                .collect();
            return result.len() as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(2));
}

#[test]
fn test_iter_zip_heterogeneous() {
    // zip of Iterator<string> with Iterator<u64>: verifies that T(1) in the
    // parameter type `Iterator<T(1)>` is not incorrectly resolved to `string`
    // (the T(0) of the receiver type) during type-checking.
    let code = r#"
        entry main() {
            let names: string[] = ["alice", "bob", "carol"];
            let scores: u64[] = [10u64, 20u64, 30u64];
            let paired: (string, u64)[] = names.iter().zip(scores.iter()).collect();
            assert(paired.len() == 3);
            assert(paired[0].0 == "alice");
            assert(paired[0].1 == 10u64);
            assert(paired[2].0 == "carol");
            assert(paired[2].1 == 30u64);
            return paired.len() as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(3));
}

#[test]
fn test_iter_complex_lazy_pipeline() {
    // unfold → filter → map → collect: verifies the full lazy collect path.
    let code = r#"
        entry main() {
            let result: u64[] = Iterator::unfold(0u64, |s: u64| {
                if s >= 6u64 { return null }
                return (s, s + 1u64)
            })
            .filter(|x: u64| { return x % 2u64 == 0u64 })
            .map(|x: u64| { return x * 100u64 })
            .collect();
            // evens 0..5: 0, 2, 4 -> mapped: 0, 200, 400
            assert(result.len() == 3);
            assert(result[0] == 0);
            assert(result[1] == 200);
            assert(result[2] == 400);
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}
#[test]
fn test_debug_unfold_map_minimal() {
    // Minimal: unfold + map, no capture in map, check first item
    let code = r#"
        entry main() {
            let it: Iterator<u64> = Iterator::unfold(0u64, |s: u64| {
                if s >= 3u64 { return null }
                return (s, s + 1u64)
            }).map(|x: u64| { return x * 10u64 });
            assert(it.next() == 0);
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_debug_unfold_map_nozero() {
    // unfold + map, no capture, but first item is non-zero
    let code = r#"
        entry main() {
            let it: Iterator<u64> = Iterator::unfold(1u64, |s: u64| {
                if s >= 4u64 { return null }
                return (s, s + 1u64)
            }).map(|x: u64| { return x * 10u64 });
            assert(it.next() == 10);
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_debug_unfold_filter_minimal() {
    // Minimal: unfold + filter, no capture in filter
    let code = r#"
        entry main() {
            let it: Iterator<u64> = Iterator::unfold(0u64, |s: u64| {
                if s >= 5u64 { return null }
                return (s, s + 1u64)
            }).filter(|x: u64| { return x % 2u64 == 0u64 });
            assert(it.next() == 0);
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_debug_map_filter_minimal() {
    // Minimal: dequeue + map + filter, no captures
    let code = r#"
        entry main() {
            let it: Iterator<u64> = [1u64, 2u64, 3u64].iter()
                .map(|x: u64| { return x * 2u64 })
                .filter(|x: u64| { return x > 4u64 });
            assert(it.next() == 6);
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_debug_filter_then_map_cap() {
    // filter (no capture) + map (capture): find first even, then map it
    let code = r#"
        entry main() {
            let map_calls = 0;
            let it: Iterator<u64> = [1u64, 2u64, 3u64].iter()
                .filter(|x: u64| { return x % 2u64 == 0u64 })
                .map(|x: u64| { map_calls += 1; return x * 10u64 });
            assert(it.next() == 20);
            assert(map_calls == 1);
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_debug_unfold_map_cap() {
    // unfold (no capture) + map (capture)
    let code = r#"
        entry main() {
            let map_calls = 0;
            let it: Iterator<u64> = Iterator::unfold(1u64, |s: u64| {
                if s >= 4u64 { return null }
                return (s, s + 1u64)
            }).map(|x: u64| { map_calls += 1; return x * 10u64 });
            assert(it.next() == 10);
            assert(map_calls == 1);
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_debug_unfold_cap_filter() {
    // unfold (captures) + filter (no capture)
    let code = r#"
        entry main() {
            let calls = 0;
            let it: Iterator<u64> = Iterator::unfold(1u64, |s: u64| {
                calls += 1;
                if s >= 6u64 { return null }
                return (s, s + 1u64)
            }).filter(|x: u64| { return x % 2u64 == 0u64 });
            assert(it.next() == 2);
            assert(calls == 2);
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_debug_unfold_map_cap_multi() {
    // unfold (no capture) + map (capture), multiple next() calls
    let code = r#"
        entry main() {
            let map_calls = 0;
            let it: Iterator<u64> = Iterator::unfold(1u64, |s: u64| {
                if s >= 4u64 { return null }
                return (s, s + 1u64)
            }).map(|x: u64| { map_calls += 1; return x * 10u64 });
            assert(it.next() == 10); assert(map_calls == 1);
            assert(it.next() == 20); assert(map_calls == 2);
            assert(it.next() == 30); assert(map_calls == 3);
            assert(it.next().is_none()); assert(map_calls == 3);
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_debug_unfold_map_cap_2calls() {
    // unfold + cap map, only 2 next() calls
    let code = r#"
        entry main() {
            let map_calls = 0;
            let it: Iterator<u64> = Iterator::unfold(1u64, |s: u64| {
                if s >= 4u64 { return null }
                return (s, s + 1u64)
            }).map(|x: u64| { map_calls += 1; return x * 10u64 });
            assert(it.next() == 10);
            assert(map_calls == 1);
            assert(it.next() == 20);
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_iter_unfold_filter_collect() {
    // unfold + filter + collect (no map)
    let code = r#"
        entry main() {
            let result: u64[] = Iterator::unfold(0u64, |s: u64| {
                if s >= 4u64 { return null }
                return (s, s + 1u64)
            })
            .filter(|x: u64| { return x % 2u64 == 0u64 })
            .collect();
            return result.len() as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(2)); // evens: 0, 2
}

#[test]
fn test_iter_unfold_map_collect() {
    // unfold + map + collect (no filter)
    let code = r#"
        entry main() {
            let result: u64[] = Iterator::unfold(0u64, |s: u64| {
                if s >= 3u64 { return null }
                return (s, s + 1u64)
            })
            .map(|x: u64| { return x * 100u64 })
            .collect();
            return result.len() as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(3)); // [0, 100, 200]
}

// ============================================================================
// ITERATOR FLATTEN — laziness confirmation tests
// ============================================================================

#[test]
fn test_iter_flatten_lazy_next_sequential() {
    // next() on a flatten iterator advances one element at a time across sub-arrays.
    let code = r#"
        entry main() {
            let nested: u64[][] = [[10, 20], [30]];
            let it: Iterator<u64> = nested.iter().flatten();
            assert(it.next() == 10);
            assert(it.next() == 20);
            assert(it.next() == 30);
            assert(it.next().is_none());
            return 0
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_iter_flatten_lazy_take_stops_early() {
    // take(2) on a flatten of [[1,2],[3,4]] should yield exactly [1,2].
    let code = r#"
        entry main() {
            let nested: u64[][] = [[1, 2], [3, 4]];
            let result: u64[] = nested.iter()
                .flatten()
                .take(2u32)
                .collect();
            assert(result.len() == 2);
            assert(result[0] == 1u64);
            assert(result[1] == 2u64);
            return result.len() as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(2));
}

#[test]
fn test_iter_flatten_empty_sub_arrays() {
    // flatten skips empty sub-arrays and yields elements from non-empty ones.
    let code = r#"
        entry main() {
            let empty: u64[] = [];
            let nested: u64[][] = [empty, [1], empty, [2, 3], empty];
            let flat: u64[] = nested.iter().flatten().collect();
            assert(flat.len() == 3);
            assert(flat[0] == 1u64);
            assert(flat[1] == 2u64);
            assert(flat[2] == 3u64);
            return flat.len() as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(3));
}

#[test]
fn test_iter_flatten_lazy_with_map() {
    // flatten combined with map: map applies to inner elements, not sub-arrays.
    let code = r#"
        entry main() {
            let nested: u64[] = [1, 2, 3];
            let calls = 0;
            let iter: Iterator<u64> = nested.iter()
                .map(|x: u64| { calls += 1; return [x * 10] })
                .flatten();

            assert(iter.next() == 10);
            assert(calls == 1);
            assert(iter.next() == 20);
            assert(calls == 2);
            assert(iter.next() == 30);
            assert(calls == 3);

            return 0u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(0));
}

#[test]
fn test_iter_flatten_lazy_with_filter() {
    // flatten + filter: filter applies to inner elements.
    let code = r#"
        entry main() {
            let nested: u64[][] = [[1, 2, 3], [4, 5]];
            let result: u64[] = nested.iter()
                .flatten()
                .filter(|x: u64| { return x % 2u64 == 0u64 })
                .collect();
            assert(result.len() == 2);
            assert(result[0] == 2u64);
            assert(result[1] == 4u64);
            return result.len() as u64
        }
    "#;
    assert_eq!(run_code(code), Primitive::U64(2));
}
