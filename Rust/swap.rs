//! 实验：交换两个数的值

fn main() {
    let mut a = 251219;
    let mut b = 221730;

    println!("a={a} b={b}");
    std::mem::swap(&mut a, &mut b);
    println!("a={a} b={b}");
}
