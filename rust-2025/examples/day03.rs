use std::ops::Index;

fn main() {
    let input = parse();
    println!("part 1 {}", input.iter().map(|m| solve(&m, 0, 1)).sum::<u64>());
    println!("part 2 {}", input.iter().map(|m| solve(&m, 0, 11)).sum::<u64>());
}

fn parse() -> Vec<Vec<u32>> {
    std::fs::read_to_string("input/day03.txt").unwrap()
        .lines().map(|line| line.chars().map(|c| c as u32 - 48).collect::<Vec<u32>>()).collect()
}

fn solve(input : &Vec<u32>, initial : usize, remaining : usize) -> u64 {
    let mut highest = 0;
    let mut options = Vec::<usize>::with_capacity(64);

    for i in initial..input.len() - remaining {
        let n = *input.index(i);
        if n > highest {
            highest = n;
            options.clear();
            options.push(i);
        }
        else if n == highest {
            options.push(i);
        }
    }

    if remaining <= 0 {
        highest as u64
    }
    else {
        let current = (highest as u64) * 10_u64.pow(remaining as u32);
        let next    = options.iter().map(|i| solve(&input, i + 1, remaining - 1)).max().unwrap();
        current + next 
    }
}