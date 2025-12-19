fn main() {
    let row = 10;
    let col = 18;

    for r in ('A'..='Z').take(row) {
        for rc in ('A'..=r).rev() {
            print!("{}", rc);
        }

        let prefix_size = r as u8 - 'A' as u8 + 1;
        let terminator = char::from('B' as u8 + (col - prefix_size));
        for c in 'B'..terminator {
            print!("{}", c);
        }

        println!();
    }
}
