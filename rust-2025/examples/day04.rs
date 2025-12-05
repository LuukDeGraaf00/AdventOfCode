fn main() {
    let (indices, width, height) = parse();
    let mut grid = create_grid(&indices, width, height);
    println!("part 1 {:?}", part1(&indices, &grid));
    println!("part 2 {:?}", part2(&indices, &mut grid, width));
}

fn parse() -> (Vec<usize>, usize, usize) {

    let mut indices = Vec::<usize>::new();
    let mut width   = 0;
    let mut height  = 0;
    let mut index   = 0;

    for c in std::fs::read_to_string("input/day04.txt").unwrap().chars() {
        match c {
            '\n' => { if width == 0 { width = index; }; height += 1; },
            '@'  => { indices.push(index); index += 1; }
            '.'  => { index += 1;}
            _    => { }
        }
    }

    (indices, width, height + 1)
}

fn part1(indices : &Vec<usize>, grid : &Vec<i32>) -> usize {
    indices.iter().filter(|&i| grid[*i] < 4).count()
}

fn part2(indices : &Vec<usize>, grid : &mut Vec<i32>, width : usize) -> usize {
    let (accessible, other) : (Vec<usize>, Vec<usize>) = indices.iter().partition(|&i| grid[*i] < 4);
    if accessible.len() > 0 {
        modify_grid(&accessible, grid, width, -1);
        accessible.len() + part2(&other, grid, width)
    }
    else { 0 }
}

fn create_grid(indices : &Vec<usize>, width : usize, height : usize) -> Vec<i32> {
    let mut output = vec![0; width * height];
    modify_grid(indices, &mut output, width, 1);
    output
}
    
fn modify_grid(indices: &Vec<usize>, grid: &mut Vec<i32>, width: usize, change : i32) {
    let (r1, r3) = (-(width as isize), width as isize);
    let offset : [isize; 8] = [r1-1,r1,r1+1,-1,1,r3-1,r3,r3+1];
    
    for i in indices {    
        let x = i % width;
        for d in offset {
            let n = (*i as isize) + d;
            if n > 0 {
                let n = n as usize;
                if n < grid.len() && (n % width).abs_diff(x) <= 1{
                    grid[n as usize] += change;
                }
            }       
        }
    }
}