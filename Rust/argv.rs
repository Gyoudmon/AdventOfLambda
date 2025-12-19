//! 实验：回显命令行参数

fn main() {
    // WARNING: no whitespace between the variable and the colon(:)
    let args: Vec<String> = std::env::args().collect();

    // Rust infers type info automatically
    println!("program name: {}", args[0]);
    println!("argument count: {}", args.len() - 1);

    println!("argument vector:");
    for (i, arg) in args.iter().skip(1).enumerate() {
        // another way for string interpolation
        println!("  argument[{i}]: {arg}");
    }
}
