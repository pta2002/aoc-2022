use chumsky::prelude::*;
use std::collections::BTreeMap;

#[derive(Debug, Clone)]
enum Op {
    Noop,
    Addx(isize),
}

#[derive(Debug)]
struct Computer {
    current: usize,
    value: isize,
    changes: BTreeMap<usize, isize>,
}

impl Computer {
    fn new() -> Self {
        let mut changes = BTreeMap::new();
        changes.insert(0, 0);
        Self {
            current: 1,
            value: 1,
            changes,
        }
    }

    fn execute(&mut self, op: &Op) {
        match op {
            Op::Noop => {
                self.current += 1;
            }
            Op::Addx(n) => {
                self.current += 2;
                self.value += n;
                self.changes.insert(self.current, self.value);
            }
        }
    }

    fn get_x_at(&self, cycle: usize) -> isize {
        let actual_cycle = self
            .changes
            .keys()
            .take_while(|k| *k <= &cycle)
            .last()
            .unwrap();
        *self.changes.get(actual_cycle).unwrap()
    }

    fn signal_strength(&self, cycle: usize) -> isize {
        (cycle as isize) * self.get_x_at(cycle)
    }

    fn part1(&self) -> isize {
        self.signal_strength(20)
            + self.signal_strength(60)
            + self.signal_strength(100)
            + self.signal_strength(140)
            + self.signal_strength(180)
            + self.signal_strength(220)
    }

    fn part2(&self) {
        for i in 0..240 {
            let x = self.get_x_at(i + 1);
            let row: isize = (i % 40).try_into().unwrap();

            if i != 0 && row == 0 {
                println!()
            }

            if x - 1 <= row && x + 1 >= row {
                print!("#");
            } else {
                print!(" ");
            }
        }
    }
}

fn parser() -> impl Parser<char, Vec<Op>, Error = Simple<char>> {
    let int = just('-')
        .then(text::int(10))
        .map(|(a, b)| format!("{}{}", a, b))
        .or(text::int(10))
        .map(|s: String| s.parse().unwrap());

    choice((
        text::keyword("noop").to(Op::Noop),
        text::keyword("addx")
            .then(just(' '))
            .ignore_then(int)
            .map(Op::Addx),
    ))
    .separated_by(text::newline())
}

fn main() {
    let src = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();

    let ops = parser().parse(src).unwrap();

    let state1 = ops.iter().fold(Computer::new(), |mut acc, op| {
        acc.execute(op);
        acc
    });

    println!("Part 1: {}", state1.part1());
    println!("Part 2:");

    state1.part2();
}
