use std::env;
use std::fs;

static SAMPLE: &str = r#"1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
"#;

fn part1(input: &str) -> i64 {
    let groups = input.split("\n\n");
    let groups_split = groups.map(|group| group.split("\n"));
    let summed = groups_split.map(|lines| {
        lines
            .map(|line| line.parse::<i64>().unwrap_or(0))
            .sum::<i64>()
    });
    let max = summed.max().unwrap_or(0);

    return max;
}

pub fn run() {
    let cwd = env::current_dir().unwrap();
    let input = fs::read_to_string(cwd.join("src").join("day01-input"));
    let input_ = input.expect("Failed to read input");

    print!("{}", part1(input_.as_str()))
}
