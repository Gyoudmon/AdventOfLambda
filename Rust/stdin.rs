//! 实验：读取一系列整数，一行两个，一行一个

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

struct Scanner {
    tokens: std::vec::IntoIter<String>,
}

impl Scanner {
    fn new() -> Self {
        let mut s = String::new();
        io::stdin().read_line(&mut s).unwrap();
        let tokens = s
            .split_whitespace()
            .map(String::from)
            .collect::<Vec<_>>();
        Scanner { tokens: tokens.into_iter() }
    }

    fn next<T: std::str::FromStr>(&mut self) -> T {
        let token = self.tokens.next().unwrap();
        token.parse().ok().unwrap()
    }

    fn next_vec<T: std::str::FromStr>(&mut self, n: usize) -> Vec<T> {
        (0..n).map(|_| self.next()).collect()
    }
}

fn main() {
    let tokens = read_tokens();
    let t1 = inst!(tokens[0], i32);
    let t2: i32 = inst!(tokens[1]);
    let l1 = inst!(read_line(), i32);
    let l2: i32 = inst!(read_line());

    let mut s = Scanner::new();
    let s1: i32 = s.next();
    
    println!("{} {} {} {} {}", t1, t2, l1, l2, s1);
}
