use std::{env, fs};

trait IsBetween<A> {
    fn is_between(&self, pair: (A, A)) -> bool;
}
type Section = (u32, u32);

impl<A: Eq + Ord> IsBetween<A> for A {
    fn is_between(&self, pair: (A, A)) -> bool {
        self >= &pair.0 && self <= &pair.1
    }
}

fn parse_line(line: &str) -> (Section, Section) {
    let mut split = line.split(",");
    let x = split
        .next()
        .map(|x| {
            let mut sp = x
                .split("-")
                .map(|n| n.parse::<u32>().expect("Failed to parse int"));
            let x1 = sp.next().expect("Oops y1");
            let x2 = sp.next().expect("Oops y2");
            (x1, x2)
        })
        .expect("Oops");
    let y = split
        .next()
        .map(|x| {
            let mut sp = x
                .split("-")
                .map(|n| n.parse::<u32>().expect("Failed to parse int"));
            let y1 = sp.next().expect("Oops y1");
            let y2 = sp.next().expect("Oops y2");
            (y1, y2)
        })
        .expect("Oops");
    (x, y)
}

fn sections_overlap_entirely(sections: (Section, Section)) -> bool {
    (sections.0 .0 >= sections.1 .0 && sections.0 .1 <= sections.1 .1)
        || (sections.1 .0 >= sections.0 .0 && sections.1 .1 <= sections.0 .1)
}

fn sections_overlap((x, y): (Section, Section)) -> bool {
    x.0.is_between(y) || x.1.is_between(y) || sections_overlap_entirely((x, y))
}

fn part1(input: &str) -> u32 {
    let parsed = input.split("\n").filter(|&l| !l.is_empty()).map(parse_line);

    let mut n = 0;

    for sections in parsed {
        if sections_overlap_entirely(sections) {
            n += 1;
        }
    }

    n
}

fn part2(input: &str) -> u32 {
    let parsed = input.split("\n").filter(|&l| !l.is_empty()).map(parse_line);

    let mut n = 0;

    for sections in parsed {
        if sections_overlap(sections) {
            n += 1;
        }
    }

    n
}

pub fn run() {
    let cwd = env::current_dir().unwrap();
    let input = fs::read_to_string(cwd.join("../input/day04")).expect("Failed to read input");
    let input_str = input.as_str();

    println!("part 1");
    println!("{}", part1(input_str));
    println!("part 2");
    println!("{}", part2(input_str));
}
