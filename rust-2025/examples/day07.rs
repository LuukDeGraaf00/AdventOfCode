use std::{collections::{HashMap, HashSet}};

fn main(){

    let input = parse();
    println!("{:?}", part1(&input));
    println!("{:?}", part2(&input));
}

fn parse() -> Vec<HashSet<usize>>{
    std::fs::read_to_string("input/day07.txt").unwrap().lines().map(|line|{   
        line.chars().enumerate().filter_map(|(i,c)|{
            match c {
                '^' => Some(i),
                'S' => Some(i),
                _   => None,
            }
        }).collect()
    }).filter(|s : &HashSet<usize>| !s.is_empty()).collect()
}

fn part1(input : &Vec<HashSet<usize>>) -> u64 {
    let mut splits = 0;
    let mut beam   = input[0].clone();

    for row in input {
        let mut miss : HashSet<usize> = beam.difference(&row).copied().collect();
        for i in beam.intersection(&row).copied() {
            miss.insert(i - 1);
            miss.insert(i + 1);
            splits += 1;
        }
        beam = miss;
    }
    splits
}


fn part2(input : &Vec<HashSet<usize>>) -> u64 {

    let start       = (input[0].iter().last().unwrap().clone(), 0 as usize);
    let mut cache   = HashMap::<(usize, usize), u64>::new();
    let mut queue   = vec![start];

    while let Some((x, y)) = queue.pop() {  

        if let Some(line) = input.get(y + 1) {

            if line.contains(&x) {
                if let Some(l) = cache.get(&(x - 1, y + 1)) && let Some(r) = cache.get(&(x + 1, y + 1)) {
                    cache.insert((x, y), l + r);
                }
                else {
                    queue.push((x, y)); 
                    queue.push((x - 1, y + 1)); 
                    queue.push((x + 1, y + 1));   
                }
            }
            else {
                if let Some(&n) = cache.get(&(x, y + 1)){          
                    cache.insert((x, y), n);
                }
                queue.push((x, y + 1));
            }
        }
        else { cache.insert((x, y), 1); }
    }

    cache.get(&start).unwrap().clone()
}

