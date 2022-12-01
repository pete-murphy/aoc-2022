#![feature(binary_heap_into_iter_sorted)]

use std::io::{self, Write};
mod day01;

fn main() {
    day01::run();
    io::stdout().flush();
}
