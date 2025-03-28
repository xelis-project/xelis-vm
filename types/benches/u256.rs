use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, black_box};
use std::str::FromStr;
use xelis_types::U256;

/// Benchmark creation operations
fn bench_creation(c: &mut Criterion) {
    let mut group = c.benchmark_group("U256 creation");
    
    // From primitive types
    group.bench_function("from u64", |b| {
        b.iter(|| {
            black_box(U256::from(black_box(0xDEADBEEFu64)));
        });
    });

    group.bench_function("from u128", |b| {
        b.iter(|| {
            black_box(U256::from(black_box(0xDEADBEEF_DEADBEEFu128)));
        });
    });

    // From string
    group.bench_function("from_str (small)", |b| {
        b.iter(|| {
            black_box(U256::from_str("12345678901234567890").unwrap());
        });
    });

    group.bench_function("from_str (large)", |b| {
        b.iter(|| {
            black_box(U256::from_str("115792089237316195423570985008687907853269984665640564039457584007913129639935").unwrap());
        });
    });

    // From bytes
    let bytes = U256::MAX.to_be_bytes();
    group.bench_function("from_be_bytes", |b| {
        b.iter(|| {
            black_box(U256::from_be_bytes(black_box(bytes)));
        });
    });

    let bytes = U256::MAX.to_le_bytes();
    group.bench_function("from_le_bytes", |b| {
        b.iter(|| {
            black_box(U256::from_le_bytes(black_box(bytes)));
        });
    });

    group.finish();
}

/// Benchmark serialization operations
fn bench_serialization(c: &mut Criterion) {
    let mut group = c.benchmark_group("U256 serialization");

    // Small number
    let small = U256::from(0xDEADBEEFu64);
    group.bench_function("to_string (small)", |b| {
        b.iter(|| {
            black_box(black_box(small).to_string());
        });
    });

    // Large number
    let large = U256::MAX;
    group.bench_function("to_string (large)", |b| {
        b.iter(|| {
            black_box(black_box(large).to_string());
        });
    });

    // To bytes
    group.bench_function("to_be_bytes", |b| {
        b.iter(|| {
            black_box(black_box(large).to_be_bytes());
        });
    });

    group.bench_function("to_le_bytes", |b| {
        b.iter(|| {
            black_box(black_box(large).to_le_bytes());
        });
    });

    group.finish();
}

/// Benchmark arithmetic operations
fn bench_arithmetic(c: &mut Criterion) {
    let mut group = c.benchmark_group("U256 arithmetic");
    
    // Small numbers
    let a_small = U256::from(0xDEADBEEFu64);
    let b_small = U256::from(0x12345678u64);
    
    // Medium numbers
    let a_medium = U256::from(u64::MAX);
    let b_medium = U256::from(0x100u64);

    // Addition
    for (name, a, b) in [
        ("add (small)", a_small, b_small),
        ("add (medium)", a_medium, b_medium),
    ] {
        group.bench_function(name, |bench| {
            bench.iter(|| {
                black_box(black_box(a) + black_box(b));
            });
        });
    }

    // Subtraction
    for (name, a, b) in [
        ("sub (small)", a_small, b_small),
        ("sub (medium)", a_medium, b_medium),
    ] {
        group.bench_function(name, |bench| {
            bench.iter(|| {
                black_box(black_box(a) - black_box(b));
            });
        });
    }

    // Multiplication
    for (name, a, b) in [
        ("mul (small)", a_small, b_small),
        ("mul (medium)", a_small, b_medium),
    ] {
        group.bench_function(name, |bench| {
            bench.iter(|| {
                black_box(black_box(a) * black_box(b));
            });
        });
    }

    // Division
    for (name, a, b) in [
        ("div (small)", a_small, b_small),
        ("div (medium)", a_medium, b_medium),
    ] {
        group.bench_function(name, |bench| {
            bench.iter(|| {
                black_box(black_box(a) / black_box(b));
            });
        });
    }

    // Remainder
    for (name, a, b) in [
        ("rem (small)", a_small, b_small),
        ("rem (medium)", a_medium, b_medium),
    ] {
        group.bench_function(name, |bench| {
            bench.iter(|| {
                black_box(black_box(a) % black_box(b));
            });
        });
    }

    group.finish();
}

/// Benchmark bitwise operations
fn bench_bitwise(c: &mut Criterion) {
    let mut group = c.benchmark_group("U256 bitwise");
    
    let a = U256::new(0xDEADBEEF, 0x12345678, 0, 0);
    let b = U256::new(0x11223344, 0x55667788, 0, 0);
    
    // Basic bitwise operations
    group.bench_function("and", |bench| {
        bench.iter(|| {
            black_box(black_box(a) & black_box(b));
        });
    });
    
    group.bench_function("or", |bench| {
        bench.iter(|| {
            black_box(black_box(a) | black_box(b));
        });
    });
    
    group.bench_function("xor", |bench| {
        bench.iter(|| {
            black_box(black_box(a) ^ black_box(b));
        });
    });
    
    // Shifts with u32 - use small shift values
    for shift in [1u32, 8u32, 16u32, 32u32, 48u32] {
        group.bench_with_input(BenchmarkId::new("shl_u32", shift), &shift, |b, &shift| {
            b.iter(|| {
                black_box(black_box(a) << black_box(shift));
            });
        });
        
        group.bench_with_input(BenchmarkId::new("shr_u32", shift), &shift, |b, &shift| {
            b.iter(|| {
                black_box(black_box(a) >> black_box(shift));
            });
        });
    }
    
    // Shifts with U256 - use small shift values
    let shifts = [
        U256::from(1u64),
        U256::from(8u64),
        U256::from(16u64),
        U256::from(32u64),
        U256::from(48u64),
    ];
    
    for (i, shift) in shifts.iter().enumerate() {
        let shift_amount = 1 << (i * 3); // 1, 8, 16, 32, 48
        group.bench_with_input(BenchmarkId::new("shl_u256", shift_amount), shift, |b, shift| {
            b.iter(|| {
                black_box(black_box(a) << black_box(*shift));
            });
        });
        
        group.bench_with_input(BenchmarkId::new("shr_u256", shift_amount), shift, |b, shift| {
            b.iter(|| {
                black_box(black_box(a) >> black_box(*shift));
            });
        });
    }
    
    group.finish();
}

/// Benchmark comparisons
fn bench_comparison(c: &mut Criterion) {
    let mut group = c.benchmark_group("U256 comparison");
    
    // Different test cases
    let cases = [
        ("equal", U256::from(0x12345678u64), U256::from(0x12345678u64)),
        ("not_equal", U256::from(0x12345678u64), U256::from(0x87654321u64)),
        ("less_than", U256::from(0x12345678u64), U256::from(0x87654321u64)),
        ("greater_than", U256::from(0x87654321u64), U256::from(0x12345678u64)),
        ("large_equal", U256::MAX, U256::MAX),
        ("large_not_equal", U256::MAX, U256::MAX - U256::ONE),
    ];
    
    for (name, a, b) in cases {
        group.bench_function(format!("eq_{}", name), |bench| {
            bench.iter(|| {
                black_box(black_box(a) == black_box(b));
            });
        });
        
        group.bench_function(format!("cmp_{}", name), |bench| {
            bench.iter(|| {
                black_box(black_box(a).partial_cmp(&black_box(b)));
            });
        });
    }
    
    group.finish();
}

/// Benchmark overflowing operations
fn bench_overflowing(c: &mut Criterion) {
    let mut group = c.benchmark_group("U256 overflowing");
    
    let a_small = U256::from(0xDEADBEEFu64);
    let b_small = U256::from(0x12345678u64);
    let a_medium = U256::from(u64::MAX);
    let b_medium = U256::from(0x100u64);
    
    // Addition
    for (name, a, b) in [
        ("add (small)", a_small, b_small),
        ("add (medium)", a_medium, b_medium),
    ] {
        group.bench_function(format!("overflowing_{}", name), |bench| {
            bench.iter(|| {
                black_box(black_box(a).overflowing_add(black_box(b)));
            });
        });
    }
    
    // Subtraction
    for (name, a, b) in [
        ("sub (small, no overflow)", a_small, b_small),
        ("sub (small, overflow)", b_small, a_small),
    ] {
        group.bench_function(format!("overflowing_{}", name), |bench| {
            bench.iter(|| {
                black_box(black_box(a).overflowing_sub(black_box(b)));
            });
        });
    }
    
    // Multiplication
    for (name, a, b) in [
        ("mul (small)", a_small, b_small),
        ("mul (medium)", a_small, a_medium),
    ] {
        group.bench_function(format!("overflowing_{}", name), |bench| {
            bench.iter(|| {
                black_box(black_box(a).overflowing_mul(black_box(b)));
            });
        });
    }
    
    // Division
    group.bench_function("overflowing_div", |bench| {
        bench.iter(|| {
            black_box(black_box(a_medium).overflowing_div(black_box(b_small)));
        });
    });
    
    // Remainder
    group.bench_function("overflowing_rem", |bench| {
        bench.iter(|| {
            black_box(black_box(a_medium).overflowing_rem(black_box(b_small)));
        });
    });
    
    group.finish();
}

/// Benchmark checked operations
fn bench_checked(c: &mut Criterion) {
    let mut group = c.benchmark_group("U256 checked");
    
    let a_small = U256::from(0xDEADBEEFu64);
    let b_small = U256::from(0x12345678u64);
    let a_medium = U256::from(u64::MAX);
    let b_medium = U256::from(0x100u64);
    
    // Addition
    for (name, a, b) in [
        ("add (small)", a_small, b_small),
        ("add (medium)", a_medium, b_medium),
    ] {
        group.bench_function(format!("checked_{}", name), |bench| {
            bench.iter(|| {
                black_box(black_box(a).checked_add(black_box(b)));
            });
        });
    }
    
    // Subtraction
    for (name, a, b) in [
        ("sub (small, no overflow)", a_small, b_small),
        ("sub (small, overflow)", b_small, a_small),
    ] {
        group.bench_function(format!("checked_{}", name), |bench| {
            bench.iter(|| {
                black_box(black_box(a).checked_sub(black_box(b)));
            });
        });
    }
    
    // Multiplication
    for (name, a, b) in [
        ("mul (small)", a_small, b_small),
        ("mul (medium)", a_small, a_medium),
    ] {
        group.bench_function(format!("checked_{}", name), |bench| {
            bench.iter(|| {
                black_box(black_box(a).checked_mul(black_box(b)));
            });
        });
    }
    
    // Division
    for (name, a, b) in [
        ("div (normal)", a_medium, b_small),
        ("div (by zero)", a_small, U256::ZERO),
    ] {
        group.bench_function(format!("checked_{}", name), |bench| {
            bench.iter(|| {
                black_box(black_box(a).checked_div(black_box(b)));
            });
        });
    }
    
    // Remainder
    for (name, a, b) in [
        ("rem (normal)", a_medium, b_small),
        ("rem (by zero)", a_small, U256::ZERO),
    ] {
        group.bench_function(format!("checked_{}", name), |bench| {
            bench.iter(|| {
                black_box(black_box(a).checked_rem(black_box(b)));
            });
        });
    }
    
    group.finish();
}

/// Benchmark utility functions
fn bench_utility(c: &mut Criterion) {
    let mut group = c.benchmark_group("U256 utility");
    
    // Test cases of varying sizes
    let cases = [
        ("zero", U256::ZERO),
        ("one", U256::ONE),
        ("small", U256::from(0x12345678u64)),
        ("medium", U256::from(u64::MAX)),
        ("large_low", U256::new(0, 0, 0xFFFFFFFF, 0)),
        ("large_high", U256::new(0, 0, 0, 0xFFFFFFFF)),
        ("max", U256::MAX),
    ];
    
    for (name, value) in cases {
        group.bench_function(format!("leading_zeros_{}", name), |bench| {
            bench.iter(|| {
                black_box(black_box(value).leading_zeros());
            });
        });
        
        group.bench_function(format!("bits_{}", name), |bench| {
            bench.iter(|| {
                black_box(black_box(value).bits());
            });
        });
        
        group.bench_function(format!("is_zero_{}", name), |bench| {
            bench.iter(|| {
                black_box(black_box(value).is_zero());
            });
        });
        
        group.bench_function(format!("is_one_{}", name), |bench| {
            bench.iter(|| {
                black_box(black_box(value).is_one());
            });
        });
        
        // Only test conversions for numbers that fit
        if name == "zero" || name == "one" || name == "small" {
            group.bench_function(format!("as_u64_{}", name), |bench| {
                bench.iter(|| {
                    black_box(black_box(value).as_u64());
                });
            });
        }
    }
    
    // Power operation
    for exp in [1u32, 5u32, 10u32, 111u32, 999u32] {
        group.bench_with_input(BenchmarkId::new("pow", exp), &exp, |b, &exp| {
            let base = U256::from(2u64);
            b.iter(|| {
                black_box(black_box(base).pow(black_box(exp)));
            });
        });
    }
    
    group.finish();
}

criterion_group!(
    benches,
    bench_creation,
    bench_serialization,
    bench_arithmetic,
    bench_bitwise,
    bench_comparison,
    bench_overflowing,
    bench_checked,
    bench_utility,
);
criterion_main!(benches); 