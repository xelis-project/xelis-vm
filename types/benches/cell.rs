use criterion::{criterion_group, criterion_main, Criterion};
use xelis_types::{Primitive, ValueCell};

fn bench_drop(c: &mut Criterion) {
    let mut group = c.benchmark_group("Cell drop");
    group.bench_function("primitive", |bench| {
        bench.iter(|| {
            let cell = ValueCell::Default(Primitive::U64(42));
            drop(cell);
        });
    });

    group.bench_function("bytes", |bench| {
        bench.iter(|| {
            let cell = ValueCell::Bytes(vec![
                17,
                42,
                42,
                89,
                23
            ]);

            drop(cell);
        });
    });

    group.bench_function("array", |bench| {
        bench.iter(|| {
            let cell = ValueCell::Object(vec![
                ValueCell::Default(Primitive::U64(42)),
                ValueCell::Default(Primitive::U64(42)),
                ValueCell::Default(Primitive::U64(42))
            ]);

            drop(cell);
        });
    });

    group.bench_function("array double depth", |bench| {
        bench.iter(|| {
            // We don't use clone as we bench it too
            let cell = ValueCell::Object(vec![
                ValueCell::Object(vec![
                    ValueCell::Default(Primitive::U64(42)),
                    ValueCell::Default(Primitive::U64(42)),
                    ValueCell::Default(Primitive::U64(42))
                ]),
                ValueCell::Object(vec![
                    ValueCell::Default(Primitive::U64(42)),
                    ValueCell::Default(Primitive::U64(42)),
                    ValueCell::Default(Primitive::U64(42))
                ]),
                ValueCell::Object(vec![
                    ValueCell::Default(Primitive::U64(42)),
                    ValueCell::Default(Primitive::U64(42)),
                    ValueCell::Default(Primitive::U64(42))
                ])
            ]);

            drop(cell);
        });
    });
}


fn bench_clone(c: &mut Criterion) {
    let mut group = c.benchmark_group("Cell clone");
    group.bench_function("primitive", |bench| {
        let cell = ValueCell::Default(Primitive::U64(42));
        bench.iter(|| {
            let _ = cell.clone();
        });
    });

    group.bench_function("bytes", |bench| {
        let cell = ValueCell::Bytes(vec![
            17,
            42,
            42,
            89,
            23
        ]);
        bench.iter(|| {
            let _ = cell.clone();

        });
    });

    group.bench_function("array", |bench| {
        let cell = ValueCell::Object(vec![
            ValueCell::Default(Primitive::U64(42)),
            ValueCell::Default(Primitive::U64(42)),
            ValueCell::Default(Primitive::U64(42))
        ]);
        bench.iter(|| {
            let _ = cell.clone();

        });
    });

    group.bench_function("array double depth", |bench| {
        let cell = ValueCell::Object(vec![
            ValueCell::Object(vec![
                ValueCell::Default(Primitive::U64(42)),
                ValueCell::Default(Primitive::U64(42)),
                ValueCell::Default(Primitive::U64(42))
            ]),
            ValueCell::Object(vec![
                ValueCell::Default(Primitive::U64(42)),
                ValueCell::Default(Primitive::U64(42)),
                ValueCell::Default(Primitive::U64(42))
            ]),
            ValueCell::Object(vec![
                ValueCell::Default(Primitive::U64(42)),
                ValueCell::Default(Primitive::U64(42)),
                ValueCell::Default(Primitive::U64(42))
            ])
        ]);
        bench.iter(|| {
            let _ = cell.clone();
        });
    });
}

criterion_group!(
    benches,
    bench_drop,
    bench_clone
);
criterion_main!(benches);