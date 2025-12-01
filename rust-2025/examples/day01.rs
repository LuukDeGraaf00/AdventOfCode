use std::{fs::File, io::{BufRead, BufReader}};

fn main() {
    println!("part 1 {}", part1(BufReader::new(File::open("input/day01.txt").unwrap())));
    println!("part 2 {}", part2(BufReader::new(File::open("input/day01.txt").unwrap())));
}

fn part1(input : BufReader<File>) -> i64 {
    input.lines().flatten().fold((0, 50),|(result, current), line| {
        let mut iter = line.chars();
        let rotate   = if iter.next().unwrap() == 'R' { 1 } else { -1 };
        let amount   = iter.as_str().parse::<u32>().unwrap() as i64;
        let update   = (current + rotate * amount).rem_euclid(100);
        (result + (update == 0) as i64, update)
    }).0
}

fn part2(input : BufReader<File>) -> i64 {
    input.lines().flatten().fold((0, 50),|(mut result, mut current), line| {
        let mut iter = line.chars();
        let rotate   = if iter.next().unwrap() == 'R' { 1 } else { -1 };
        let amount   = iter.as_str().parse::<u32>().unwrap() as i64;
        for _ in 0..amount {
            current = ((current + rotate) as i32).rem_euclid(100);
            if current == 0 {
                result += 1
            }
        }
        (result, current)
    }).0
}