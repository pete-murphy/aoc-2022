use std::{env, fs};

static SAMPLE: &str = r#"A Y
B X
C Z
"#;

#[derive(PartialEq, Eq, Clone, Copy)]
enum RPS {
    Rock,
    Paper,
    Scissors,
}

fn parse_line_part1(line: &str) -> Option<(RPS, RPS)> {
    let split: Vec<_> = line.split(" ").collect();
    match split[..] {
        [x, y] => {
            let x_rps = match x {
                "A" => RPS::Rock,
                "B" => RPS::Paper,
                "C" => RPS::Scissors,
                _ => panic!("Not A, B, or C"),
            };
            let y_rps = match y {
                "X" => RPS::Rock,
                "Y" => RPS::Paper,
                "Z" => RPS::Scissors,
                _ => panic!("Not X, Y, or Z"),
            };
            return Some((x_rps, y_rps));
        }

        _ => return None,
    }
}

#[derive(PartialEq, Eq)]
enum WDL {
    Win,
    Draw,
    Loss,
}

fn parse_line_part2(line: &str) -> Option<(RPS, WDL)> {
    let split: Vec<_> = line.split(" ").collect();
    match split[..] {
        [x, y] => {
            let x_rps = match x {
                "A" => RPS::Rock,
                "B" => RPS::Paper,
                "C" => RPS::Scissors,
                _ => panic!("Not A, B, or C"),
            };
            let y_wdl = match y {
                "X" => WDL::Loss,
                "Y" => WDL::Draw,
                "Z" => WDL::Win,
                _ => panic!("Not X, Y, or Z"),
            };
            return Some((x_rps, y_wdl));
        }

        _ => return None,
    }
}

fn score(round: (RPS, RPS)) -> i32 {
    let score_round = match round {
        (RPS::Rock, RPS::Paper) => 6,
        (RPS::Paper, RPS::Scissors) => 6,
        (RPS::Scissors, RPS::Rock) => 6,
        (ref x, ref y) => {
            if x == y {
                3
            } else {
                0
            }
        }
    };

    let score_hand = match round.1 {
        RPS::Rock => 1,
        RPS::Paper => 2,
        RPS::Scissors => 3,
    };

    score_hand + score_round
}
fn part1(input: &str) -> i32 {
    fn unformat(input: &str) -> impl Iterator<Item = (RPS, RPS)> + '_ {
        return input.split("\n").filter_map(parse_line_part1);
    }

    let parsed = unformat(input);

    parsed.map(score).sum()
}

fn part2(input: &str) -> i32 {
    fn unformat(input: &str) -> impl Iterator<Item = (RPS, WDL)> + '_ {
        return input.split("\n").filter_map(parse_line_part2);
    }

    fn get_rps(round: (RPS, WDL)) -> RPS {
        match round {
            (RPS::Rock, WDL::Win) => RPS::Paper,
            (RPS::Paper, WDL::Win) => RPS::Scissors,
            (RPS::Scissors, WDL::Win) => RPS::Rock,
            (x, WDL::Draw) => x,
            (RPS::Paper, WDL::Loss) => RPS::Rock,
            (RPS::Scissors, WDL::Loss) => RPS::Paper,
            (RPS::Rock, WDL::Loss) => RPS::Scissors,
        }
    }

    let parsed = unformat(input);

    parsed.map(|(x, y)| score((x, get_rps((x, y))))).sum()
}

pub fn run() {
    let cwd = env::current_dir().unwrap();
    let input = fs::read_to_string(cwd.join("../input/day02")).expect("Failed to read input");
    let input_str = input.as_str();

    println!("part 1");
    println!("{}", part1(input_str));
    println!("part 2");
    println!("{}", part2(input_str));
}
