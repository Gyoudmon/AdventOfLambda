//! 实验：交换两个数的值

fn main() {
    let mut a: i32 = 20;
    let mut b: i32 = 22;

    println!("a={a} b={b}");
    std::mem::swap(&mut a, &mut b);
    println!("a={a} b={b}");
}
