use criterion::{criterion_group, criterion_main, Criterion};
use silex_types::{PrimitiveInteger, Primitive, OwnedValue};

fn bench_drop(c: &mut Criterion) {
    let mut group = c.benchmark_group("Cell drop");
    group.bench_function("primitive", |bench| {
        bench.iter(|| {
            let cell = OwnedValue::Primitive(Primitive::Integer(PrimitiveInteger::U64(42)));
            drop(cell);
        });
    });

    group.bench_function("bytes", |bench| {
        bench.iter(|| {
            let cell = OwnedValue::Bytes(vec![
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
            let cell = OwnedValue::Object(vec![
                Primitive::Integer(PrimitiveInteger::U64(42)).into(),
                Primitive::Integer(PrimitiveInteger::U64(42)).into(),
                Primitive::Integer(PrimitiveInteger::U64(42)).into()
            ]);

            drop(cell);
        });
    });

    group.bench_function("array double depth", |bench| {
        bench.iter(|| {
            // We don't use clone as we bench it too
            let cell = OwnedValue::Object(vec![
                OwnedValue::Object(vec![
                    Primitive::Integer(PrimitiveInteger::U64(42)).into(),
                    Primitive::Integer(PrimitiveInteger::U64(42)).into(),
                    Primitive::Integer(PrimitiveInteger::U64(42)).into()
                ]).into(),
                OwnedValue::Object(vec![
                    Primitive::Integer(PrimitiveInteger::U64(42)).into(),
                    Primitive::Integer(PrimitiveInteger::U64(42)).into(),
                    Primitive::Integer(PrimitiveInteger::U64(42)).into()
                ]).into(),
                OwnedValue::Object(vec![
                    Primitive::Integer(PrimitiveInteger::U64(42)).into(),
                    Primitive::Integer(PrimitiveInteger::U64(42)).into(),
                    Primitive::Integer(PrimitiveInteger::U64(42)).into()
                ]).into()
            ]);

            drop(cell);
        });
    });
}


fn bench_clone(c: &mut Criterion) {
    let mut group = c.benchmark_group("Cell clone");
    group.bench_function("primitive", |bench| {
        let cell = OwnedValue::Primitive(Primitive::Integer(PrimitiveInteger::U64(42)));
        bench.iter(|| {
            let _ = cell.clone();
        });
    });

    group.bench_function("bytes", |bench| {
        let cell = OwnedValue::Bytes(vec![
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
        let cell = OwnedValue::Object(vec![
            Primitive::Integer(PrimitiveInteger::U64(42)).into(),
            Primitive::Integer(PrimitiveInteger::U64(42)).into(),
            Primitive::Integer(PrimitiveInteger::U64(42)).into()
        ]);
        bench.iter(|| {
            let _ = cell.clone();

        });
    });

    group.bench_function("array double depth", |bench| {
        let cell = OwnedValue::Object(vec![
            OwnedValue::Object(vec![
                Primitive::Integer(PrimitiveInteger::U64(42)).into(),
                Primitive::Integer(PrimitiveInteger::U64(42)).into(),
                Primitive::Integer(PrimitiveInteger::U64(42)).into()
            ]).into(),
            OwnedValue::Object(vec![
                Primitive::Integer(PrimitiveInteger::U64(42)).into(),
                Primitive::Integer(PrimitiveInteger::U64(42)).into(),
                Primitive::Integer(PrimitiveInteger::U64(42)).into()
            ]).into(),
            OwnedValue::Object(vec![
                Primitive::Integer(PrimitiveInteger::U64(42)).into(),
                Primitive::Integer(PrimitiveInteger::U64(42)).into(),
                Primitive::Integer(PrimitiveInteger::U64(42)).into()
            ]).into()
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