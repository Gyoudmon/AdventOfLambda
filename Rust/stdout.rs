//! 实验：格式化输出

fn main() {
    let r: f64 = 3.0;
    let pi: f64 = 3.14159;
    let code: u8 = 65; // only u8

    println!("{:.3}", pi * r * r);

    for b in 0..8 {
        println!("{:03b}", b);
    }

    println!("{}", code as char);
}
