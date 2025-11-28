use std::{fs::File, io::{BufRead, BufReader}};

fn main() {
    println!("part 1 {}", part1(BufReader::new(File::open("input/day02.txt").unwrap()), 12, 13, 14));
    println!("part 2 {}", part2(BufReader::new(File::open("input/day02.txt").unwrap())));
}

fn part1(input : BufReader<File>, r : u32, g : u32, b : u32) -> u32 {
    input.lines().flatten().map(|line| { 
        let mut parser = line.split(&[':', ',', ';', ' ']);
        let id         = parser.nth(1);     
        loop { 
            match parser.next() {
                Some(_) => {
                    let n = parser.next().unwrap().parse::<u32>().unwrap();
                    let invalid = match parser.next().unwrap() {
                        "red"   => n > r,
                        "green" => n > g,
                        "blue"  => n > b,
                        _       => true
                    };

                    if invalid {
                        return 0;
                    }
                }
                None => { return id.unwrap().parse::<u32>().unwrap() },
            }
        }
    }).sum()
}

fn part2(input : BufReader<File>) -> u32 {

    input.lines().flatten().map(|line| { 
        let (mut r, mut g, mut b) = (0, 0, 0);
        let mut parser = line.split(&[':', ',', ';', ' ']);
        let _          = parser.nth(1);     
        loop { 
            match parser.next() {
                Some(_) => {
                    let n = parser.next().unwrap().parse::<u32>().unwrap();
                    match parser.next().unwrap() {
                        "red"   => r = r.max(n),
                        "green" => g = g.max(n),
                        "blue"  => b = b.max(n),
                        _       => {}
                    };
                }
                None => { return r * g * b },
            }
        }
    }).sum() 
}



