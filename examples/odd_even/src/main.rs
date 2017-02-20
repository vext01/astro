#![feature(plugin)]
#![plugin(astrox)]
#![inject_block_id]

extern crate rand;

fn main() {
    let mut y = 0;

    loop {
        let x: u8 = rand::random();
        println!("x={}", x);
        if x % 2 == 0 {
            println!("even!");
            break;
        } else {
            println!("odd");
            y += 1;
        }
    }
    println!("{} tries", y);
}
