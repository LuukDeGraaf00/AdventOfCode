fn main() {
    let (mut fresh, available) = parse();
    println!("part 1 {:?}", part1(&fresh, &available));
    println!("part 2 {:?}", part2(&mut fresh));
}

fn parse() -> (Vec<(u64, u64)>, Vec<u64>) {
    let input         = std::fs::read_to_string("input/day05.txt").unwrap();
    let mut lines     = input.lines();
    let mut fresh     = Vec::<(u64, u64)>::new();
    let mut available = Vec::<u64>::new();

    while let Some(range) = lines.next() && !range.is_empty(){
        let (low, high) = range.split_once('-').unwrap();
        fresh.push((low.parse().unwrap(), high.parse().unwrap()));
    }

    while let Some(value) = lines.next() {
        available.push(value.parse().unwrap());
    }

    (fresh, available)
}

fn part1(fresh : &Vec<(u64, u64)>, available : &Vec<u64>) -> usize {
    available.iter().filter(|&i| fresh.iter().any(|(l, r)| i > l && i <= r)).count()
}

fn part2(fresh : &mut Vec<(u64, u64)>) -> u64 {
    fresh.sort_unstable();
    fresh.iter().fold((0, 0), |(count, max), &(l, r)| 
        (count + if l > max { (r - l) + 1 } else { r.saturating_sub(max) }, r.max(max))
    ).0
}