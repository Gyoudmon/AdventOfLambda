#![allow(unused_macros, dead_code)]

use std::io;

fn read_line() -> String {
    let mut s = String::new();
    io::stdin().read_line(&mut s).unwrap();
    s.trim().to_string()
}

fn read_tokens() -> Vec<String> {
    let mut s = String::new();
    io::stdin().read_line(&mut s).unwrap();
    s.trim().split_whitespace().map(|token| token.to_string()).collect()
}

macro_rules! inst {
    ($e:expr) => { $e.parse().unwrap() };
    ($e:expr, $t:ty) => { $e.parse::<$t>().unwrap() };
}

fn main() {
    let mut pay: i32 = inst!(read_line());
    let faces = [100, 50, 20, 10, 5, 1];

    for face in faces {
        println!("{}", pay / face);
        pay %= face;        
    }
}
