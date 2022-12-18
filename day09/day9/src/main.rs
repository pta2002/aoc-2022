use chumsky::prelude::*;
use std::collections::hash_set::HashSet;

#[derive(Debug, Clone)]
enum Direction {
    R,
    L,
    U,
    D,
}

impl Direction {
    fn to_vec(&self) -> (i32, i32) {
        match self {
            Direction::R => (1, 0),
            Direction::L => (-1, 0),
            Direction::U => (0, -1),
            Direction::D => (0, 1),
        }
    }
}

#[derive(Debug)]
struct Rope {
    links: Vec<(i32, i32)>,
    visited: HashSet<(i32, i32)>,
}

fn move_parser() -> impl Parser<char, Vec<(Direction, usize)>, Error = Simple<char>> {
    let int = text::int(10)
        .padded()
        .map(|s: String| s.parse::<usize>().unwrap());

    let direction = choice((
        just('R').to(Direction::R),
        just('L').to(Direction::L),
        just('U').to(Direction::U),
        just('D').to(Direction::D),
    ));

    let r#move = direction.then_ignore(just(' ')).then(int);

    r#move.repeated()
}

impl Rope {
    fn new(size: usize) -> Self {
        Self {
            links: vec![(0, 0); size],
            visited: HashSet::new(),
        }
    }

    fn update(&mut self, head: (i32, i32)) {
        self.links[0] = head;
        let mut prev = head;

        for link in self.links.iter_mut().skip(1) {
            let dist = std::cmp::max((prev.0 - link.0).abs(), (prev.1 - link.1).abs());

            if dist >= 2 {
                let x = (prev.0 - link.0).clamp(-1, 1);
                let y = (prev.1 - link.1).clamp(-1, 1);

                *link = (link.0 + x, link.1 + y);
            }

            prev = *link;
        }

        self.visited.insert(*self.links.last().unwrap());
    }

    fn mv(&mut self, mv: &(Direction, usize)) {
        let vec = mv.0.to_vec();

        for _ in 0..mv.1 {
            self.update((self.links[0].0 + vec.0, self.links[0].1 + vec.1))
        }
    }
}

fn main() {
    let src = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();

    let moves = move_parser().parse(src).unwrap();

    // TODO: Have the rope struct keep track of all the tail positions
    let res1 = moves
        .iter()
        .fold(Rope::new(2), |mut acc: Rope, mv| {
            acc.mv(mv);
            acc
        })
        .visited
        .len();

    let res2 = moves
        .iter()
        .fold(Rope::new(10), |mut acc: Rope, mv| {
            acc.mv(mv);
            acc
        })
        .visited
        .len();

    println!("Part 1: {}", res1);
    println!("Part 2: {}", res2);
}
