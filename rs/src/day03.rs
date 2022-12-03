use itertools::{self, Itertools};
use std::{
    collections::{hash_set::Union, HashMap, HashSet},
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

fn parse_line(line: &str) -> Rucksacks {
    let split = line.split_at(line.len() / 2);
    return (split.0.chars().counts(), split.1.chars().counts());
}

fn parse(input: &str) -> impl Iterator<Item = Rucksacks> + '_ {
    return input.lines().map(parse_line);
}

fn char_to_priority(char: char) -> u32 {
    return char as u32 - 48;
}

fn overlap(rucksacks: Rucksacks) -> u32 {
    let k0: HashSet<char> = rucksacks.0.keys().cloned().collect();
    let k1: HashSet<char> = rucksacks.1.keys().cloned().collect();

    let intersection = k0.intersection(&k1);

    return intersection.fold(0, |acc, x| acc + char_to_priority(x.clone()));
}

fn part1(input: &str) -> u32 {
    let parsed = parse(input);

    return parsed.map(overlap).sum();
}

pub fn run() {
    // let cwd = env::current_dir().unwrap();
    // let input =
    //     fs::read_to_string(cwd.join("src").join("day02-input")).expect("Failed to read input");
    // let input_str = input.as_str();
    let input_str = SAMPLE;

    // println!("{:#?}", parse(input_str).collect::<Vec<_>>());

    println!("{} {:#?}", "a", char_to_priority('a'));
    println!("{} {:#?}", "z", char_to_priority('z'));
    println!("{} {:#?}", "A", char_to_priority('A'));

    println!("part 1");
    println!("{}", part1(input_str));
    // println!("part 2");
    // println!("{}", part2(input_str));
}
