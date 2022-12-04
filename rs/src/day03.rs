use itertools::{self, Itertools};
use std::{
    collections::{HashMap, HashSet},
    env, fs,
};

static SAMPLE: &str = r#"vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
"#;

type Rucksacks = (HashMap<char, usize>, HashMap<char, usize>);

fn parse_line_part1(line: &str) -> Rucksacks {
    let split = line.split_at(line.len() / 2);
    return (split.0.chars().counts(), split.1.chars().counts());
}

fn parse_part1(input: &str) -> impl Iterator<Item = Rucksacks> + '_ {
    return input.lines().map(parse_line_part1);
}

fn char_to_priority(char: char) -> u32 {
    let lower_chars = ('a'..='z').into_iter().collect::<Vec<char>>();
    let upper_chars = ('A'..='Z').into_iter().collect::<Vec<char>>();
    let all_chars = [lower_chars, upper_chars].concat();

    return all_chars
        .iter()
        .position(|&c| c == char)
        .unwrap_or(0)
        .try_into()
        .unwrap_or(0)
        + 1;
}

fn overlap_rucksacks(rucksacks: Rucksacks) -> u32 {
    let k0: HashSet<char> = rucksacks.0.keys().cloned().collect();
    let k1: HashSet<char> = rucksacks.1.keys().cloned().collect();

    let intersection = k0.intersection(&k1);

    return intersection.fold(0, |acc, x| acc + char_to_priority(x.clone()));
}

fn part1(input: &str) -> u32 {
    let parsed = parse_part1(input);

    return parsed.map(overlap_rucksacks).sum();
}

fn part2(input: &str) -> u32 {
    return input
        .lines()
        .chunks(3)
        .into_iter()
        .map(|chunk| {
            let v = chunk.collect::<Vec<_>>();
            let ch = v.get(0).expect("Empty wibble").clone();
            let mut intersection: HashSet<char> = ch.clone().chars().collect();

            for chars in v {
                let chars_set: HashSet<char> = chars.chars().collect();
                intersection = intersection.intersection(&chars_set).cloned().collect();
            }

            return intersection
                .iter()
                .next()
                .expect("Empty intersection")
                .clone();
        })
        .map(char_to_priority)
        .sum();
}

pub fn run() {
    let cwd = env::current_dir().unwrap();
    let input = fs::read_to_string(cwd.join("../input/day03")).expect("Failed to read input");
    let input_str = input.as_str();

    println!("part 1");
    println!("{}", part1(input_str));
    println!("part 2");
    println!("{}", part2(input_str));
}
