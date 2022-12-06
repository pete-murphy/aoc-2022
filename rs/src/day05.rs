use std::{collections::BTreeMap, env, fs};

fn parse_crate_line(input: &str) -> Vec<Option<char>> {
    let mut v = Vec::new();
    let mut chars = input.chars();
    let mut k = true;

    while k {
        match chars.next_chunk::<3>() {
            Ok([' ', ' ', ' ']) => v.push(None),
            Ok(['[', c, ']']) => v.push(Some(c.to_owned())),
            _ => panic!("Bad!"),
        }
        let x = chars.next();
        if x.is_none() {
            k = false
        }
    }

    return v;
}

struct Move {
    n: usize,
    from: usize,
    to: usize,
}

/// input is like "move 1 from 2 to 1"
fn parse_instr_line(input: &str) -> Move {
    let mv = match input.split(" ").collect::<Vec<_>>().as_slice() {
        ["move", n_str, "from", from_str, "to", to_str] => Move {
            n: n_str.parse().unwrap(),
            from: from_str.parse().unwrap(),
            to: to_str.parse().unwrap(),
        },
        _ => panic!("Failed to parse move"),
    };

    mv
}

fn part1(input: &str) -> String {
    let (&crates_lines, &instrs_lines) = match input.split("\n\n").collect::<Vec<_>>().as_slice() {
        [crates, instrs] => (crates, instrs),
        _ => panic!("Failed splitting into crates & instructions sections"),
    };

    let parsed_crates = crates_lines
        .split("\n")
        .take_while(|&x| !x.starts_with(" 1 "))
        .map(parse_crate_line);

    let parsed_crates_vec = parsed_crates.collect::<Vec<_>>();

    // Using BTreeMap just so keys are sorted when we get values out
    let mut m: BTreeMap<usize, Vec<char>> = BTreeMap::new();

    for xs in parsed_crates_vec.iter().rev() {
        for (j, x) in xs.iter().enumerate() {
            match x {
                &Some(c) => {
                    m.entry(j + 1)
                        .and_modify(|v| v.push(c))
                        .or_insert([c].to_vec());
                }
                _ => (),
            }
        }
    }

    let parsed_instrs = instrs_lines
        .split("\n")
        .take_while(|l| !l.is_empty())
        .map(parse_instr_line);

    // TODO: The following is a weird way to modify a map
    for mv in parsed_instrs {
        let mut to_mv = Vec::new();
        m.entry(mv.from)
            .and_modify(|x| {
                let mut n = mv.n;
                while n > 0 {
                    x.pop().map(|popped| to_mv.push(popped));
                    n -= 1;
                }
            })
            .or_default();

        m.entry(mv.to).and_modify(|x| x.append(&mut to_mv));
    }

    let mut str_vec: Vec<_> = Vec::new();

    for val in m.values() {
        let last = val.last();
        last.map(|&c| str_vec.push(c));
    }

    str_vec.into_iter().collect()
}

fn part2(input: &str) -> String {
    let (&crates_lines, &instrs_lines) = match input.split("\n\n").collect::<Vec<_>>().as_slice() {
        [crates, instrs] => (crates, instrs),
        _ => panic!("Failed splitting into crates & instructions sections"),
    };

    let parsed_crates = crates_lines
        .split("\n")
        .take_while(|&x| !x.starts_with(" 1 "))
        .map(parse_crate_line);

    let parsed_crates_vec = parsed_crates.collect::<Vec<_>>();

    // Using BTreeMap just so keys are sorted when we get values out
    let mut m: BTreeMap<usize, Vec<char>> = BTreeMap::new();

    for xs in parsed_crates_vec.iter().rev() {
        for (j, x) in xs.iter().enumerate() {
            match x {
                &Some(c) => {
                    m.entry(j + 1)
                        .and_modify(|v| v.push(c))
                        .or_insert([c].to_vec());
                }
                _ => (),
            }
        }
    }

    let parsed_instrs = instrs_lines
        .split("\n")
        .take_while(|l| !l.is_empty())
        .map(parse_instr_line);

    // TODO: The following is a weird way to modify a map
    for mv in parsed_instrs {
        let mut to_mv = Vec::new();
        m.entry(mv.from)
            .and_modify(|x| {
                let mut n = mv.n;
                while n > 0 {
                    x.pop().map(|popped| to_mv.push(popped));
                    n -= 1;
                }
            })
            .or_default();

        m.entry(mv.to).and_modify(|x| {
            let to_mv_ = {
                to_mv.reverse();
                &mut to_mv
            }; // TODO: This is the only difference from part1
            x.append(to_mv_)
        });
    }

    let mut str_vec: Vec<_> = Vec::new();

    for val in m.values() {
        let last = val.last();
        last.map(|&c| str_vec.push(c));
    }

    str_vec.into_iter().collect()
}

pub fn run() {
    let cwd = env::current_dir().unwrap();
    let input = fs::read_to_string(cwd.join("../input/day05")).expect("Failed to read input");
    let input_str = input.as_str();

    println!("part 1");
    println!("{}", part1(input_str));
    println!("part 2");
    println!("{}", part2(input_str));
}
