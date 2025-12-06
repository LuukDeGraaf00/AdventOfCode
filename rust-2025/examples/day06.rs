fn main(){
    println!("part 1 {:?}", part1());
    println!("part 2 {:?}", part2())
}


fn part1() -> u64{
    let input     = std::fs::read_to_string("input/day06.txt").unwrap();
    let mut lines = input.lines();

    let operators = lines.next_back().unwrap().split_whitespace().map(|v| v == "*");
    let mut values : Vec<_> = lines.map(|line| line.split_whitespace().map(|v| v.parse::<u64>().unwrap())).collect();

    operators.map(|b| {
        let problem = values.iter_mut().map(|i| i.next().unwrap());
        if b { problem.product::<u64>() } else { problem.sum() }
    }).sum()
}

fn part2() -> u64{
    let input                  = std::fs::read_to_string("input/day06.txt").unwrap();
    let lines : Vec<Vec<char>> = input.lines().map(|s| s.chars().collect()).collect();
    let values                 = &lines[..lines.len() - 1];
    let operators              = lines.last().unwrap();
    let mut total              = 0;
    let mut result             = Vec::<u64>::new();

    for i in (0..lines[0].len()).rev() {
        if let Ok(value) = values.iter().map(|v| v[i]).collect::<String>().trim().parse::<u64>(){
            result.push(value);
            match operators[i]{
                '*' => { total += result.iter().product::<u64>(); result.clear(); }
                '+' => { total += result.iter().sum::<u64>(); result.clear(); }
                _   => { }
            }
        }
    }

    total
}