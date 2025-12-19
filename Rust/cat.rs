//! 实验：链接并按行输出文件的全部内容

use std::fs;
use std::io::{self, Read, Write};

fn main() -> io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    
    if args.len() == 1 {
        copy_stdin()?;
    } else {
        for filename in &args[1..] {
            if filename == "-" {
                copy_stdin()?;
            } else {
                copy_file(filename)?;
            }
        }
    }
    
    Ok(())
}

fn copy_stdin() -> io::Result<()> {
    let mut stdin = io::stdin();
    let mut stdout = io::stdout();
    io::copy(&mut stdin, &mut stdout)?;
    Ok(())
}

fn copy_file(filename: &str) -> io::Result<()> {
    let content = fs::read_to_string(filename)?;
    print!("{}", content);
    Ok(())
}
