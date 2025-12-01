use std::{fs::File, io::{BufRead, BufReader}, ops::Index};

fn main() {
    println!("part 1 {}", part1(BufReader::new(File::open("input/day03.txt").unwrap())));
    //println!("part 2 {}", part2(BufReader::new(File::open("input/day01.txt").unwrap())));
}

fn part1(input : BufReader<File>) -> u32 {
    let map : Vec<Vec<char>> = input.lines().flatten().map(|i| i.chars().collect()).collect();

    fn valid(map : &Vec<Vec<char>>, x: usize, y: usize) -> bool {
        for a in (x.saturating_sub(1))..=(x+1) {
            if let Some(line) = map.get(a){
                for b in (y.saturating_sub(1))..=(y+1) {
                    if let Some(c) = line.get(b) {
                        if !c.is_digit(10) && c != &'.' {
                            return true
                        }
                    }
                }
            }
        }
        return false
    }

    let mut total = 0;

    for (x, line) in map.iter().enumerate() {
        let mut num  = Vec::<char>::new();
        let mut part = false; 

        for (y, v) in line.iter().enumerate() {
            if v.is_digit(10) {
                num.push(*v);
                if !part { part = valid(&map, x, y) }
            }
            else {
                if part && num.len() > 0 {
                    total += num.iter().collect::<String>().parse::<u32>().unwrap();
                }
                part = false;
                num.clear();
            }
        }

        if part && num.len() > 0 {
            total += num.iter().collect::<String>().parse::<u32>().unwrap();
        }
    }
    total
}
