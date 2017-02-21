#![feature(plugin)]
#![plugin(astrox)]
#![inject_block_id]

extern crate rand;

fn main() {
    let mut flips = 0;
    let mut heads = 10;

    while heads > 0 {
        flips += 1;
        let side: bool = rand::random();
        if side {
            heads -= 1;
            println!("Flip number {}: heads! {} more to go", flips, heads);
        } else {
            println!("Flip number {}: tails, too bad", flips);
        }
    }
    println!("Done in {} flips", flips);
}
