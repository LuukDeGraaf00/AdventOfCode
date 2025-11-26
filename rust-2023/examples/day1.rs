use std::{fs::File, io::{BufRead, BufReader}};

fn main() {
    println!("part 1 {}", part1(BufReader::new(File::open("input/day1.txt").unwrap())));
    println!("part 2 {}", part2(BufReader::new(File::open("input/day1.txt").unwrap())));
}

fn part1(input : BufReader<File>) -> u32 {
    input.lines().map(|line|    
        line.unwrap().chars().fold(None, |acc, c|
            c.to_digit(10).map_or(acc, |n| Some(acc.map_or((n, n), |(a, _)| (a, n))))
        ).map(|(a, b)| a * 10 + b).unwrap()
    ).sum()
}

fn part2(input : BufReader<File>) -> u32 {
    input.lines().map(|line| {
        let value : Vec<char> = line.unwrap().chars().collect();
        (0..value.len()).fold(None, |acc, i| {         
            match &value[i..].iter().collect::<String>() {
                _ if value[i].is_digit(10)  => value[i].to_digit(10),
                x if x.starts_with("zero")  => Some(0),
                x if x.starts_with("one")   => Some(1),
                x if x.starts_with("two")   => Some(2),
                x if x.starts_with("three") => Some(3),
                x if x.starts_with("four")  => Some(4),
                x if x.starts_with("five")  => Some(5),
                x if x.starts_with("six")   => Some(6),
                x if x.starts_with("seven") => Some(7),
                x if x.starts_with("eight") => Some(8),
                x if x.starts_with("nine")  => Some(9),
                _                           => None,
            }.map_or(acc, |n| Some(acc.map_or((n, n), |(a, _)| (a, n))))
        }).map(|(a, b)| a * 10 + b).unwrap()
    }).sum()
}