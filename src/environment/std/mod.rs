mod array;
mod optional;
mod string;

use super::EnvironmentBuilder;

pub fn register(env: &mut EnvironmentBuilder) {
    array::register(env);
    optional::register(env);
    string::register(env);
}