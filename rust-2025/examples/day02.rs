use std::{collections::HashSet};

fn main() {
    println!("part 1 {}", part1(std::fs::read_to_string("input/day02.txt").unwrap()));
    println!("part 2 {}", part2(std::fs::read_to_string("input/day02.txt").unwrap()));
}

fn solve(input : String, cache : HashSet<u64>) -> u64 {
    input.split(',').fold(0, |acc, row| {
        let (a, b) = row.split_once('-').unwrap();
        (a.parse::<u64>().unwrap()..=b.parse::<u64>().unwrap()).fold(acc, |acc, n| {
            if cache.contains(&n) { acc + n as u64 } else { acc }
        })
    })
}

fn part1(reader : String) -> u64 {
    solve(reader, (1..=(1 << 18)).map(|n| repeat(n as u64, 2)).collect::<HashSet<u64>>())
}

fn part2(reader : String) -> u64 {
    let mut cache = HashSet::<u64>::new();

    for n in 1..=(1 << 18){
        cache.insert(repeat(n as u64, 2));
    }
    for n in 1..=(1 << 12){
        cache.insert(repeat(n as u64, 3));
    }
    for n in 1..=(1 << 9){
        cache.insert(repeat(n as u64, 4));
    }
    for n in 1..=(1 << 7){
        cache.insert(repeat(n as u64, 5));
    }
    for n in 1..=(1 << 6){
        cache.insert(repeat(n as u64, 6));
    }

    solve(reader, cache)
}

fn repeat(n: u64, k: u32) -> u64 {
    let base = 10u64.pow(n.ilog10() + 1);
    n * ((base.pow(k) - 1) / (base - 1))
}