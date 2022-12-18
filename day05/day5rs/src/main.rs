use chumsky::prelude::*;
use std::cell::RefCell;
use std::fs::read_to_string;

#[derive(Debug)]
struct Move {
    n: usize,
    from: usize,
    to: usize,
}

// thanks SO! https://stackoverflow.com/questions/64498617/how-to-transpose-a-vector-of-vectors-in-rust
fn transpose2<T>(v: Vec<Vec<T>>) -> Vec<Vec<T>> {
    assert!(!v.is_empty());
    let len = v[0].len();
    let mut iters: Vec<_> = v.into_iter().map(|n| n.into_iter()).collect();
    (0..len)
        .map(|_| {
            iters
                .iter_mut()
                .map(|n| n.next().unwrap())
                .collect::<Vec<T>>()
        })
        .collect()
}

fn move_parser() -> impl Parser<char, Vec<Move>, Error = Simple<char>> {
    let int = text::int(10)
        .padded()
        .map(|s: String| s.parse::<usize>().unwrap());

    let r#move = text::keyword("move")
        .ignore_then(int)
        .then_ignore(text::keyword("from"))
        .then(int)
        .then_ignore(text::keyword("to"))
        .then(int)
        .map(|((n, from), to)| Move {
            n,
            from: from - 1,
            to: to - 1,
        });

    let moves = r#move.repeated();
    moves.then_ignore(end())
}

fn stack_parser() -> impl Parser<char, Vec<RefCell<Vec<char>>>, Error = Simple<char>> {
    let container = filter(|c: &char| c.is_ascii_uppercase())
        .delimited_by(just('['), just(']'))
        .map(Some);

    let cell = container.or(just(' ').repeated().exactly(3).map(|_| None));
    let line = cell.separated_by(just(' ')).at_least(1);

    let number_line = text::digits(10).padded().repeated();

    let lines = line.separated_by(text::newline()).then_ignore(number_line);

    lines.map(transpose2).map(|s| {
        s.into_iter()
            .map(|st| RefCell::new(st.into_iter().filter_map(|i| i).rev().collect()))
            .collect()
    })
}

fn parser() -> impl Parser<char, (Vec<RefCell<Vec<char>>>, Vec<Move>), Error = Simple<char>> {
    stack_parser().then(move_parser())
}

fn main() {
    let src = read_to_string(std::env::args().nth(1).unwrap()).unwrap();

    let (towers, moves) = parser().parse(src).unwrap();

    dbg!(&towers);

    let part1 = moves
        .iter()
        .fold(towers.clone(), |acc, mv| {
            for c in (0..mv.n).map(|_| acc[mv.from].borrow_mut().pop().unwrap()) {
                acc[mv.to].borrow_mut().push(c);
            }
            acc
        })
        .into_iter()
        .map(|c| c.borrow().last().unwrap().to_owned())
        .collect::<String>();

    let part2 = moves
        .iter()
        .fold(towers, |acc, mv| {
            let mut to_push = Vec::with_capacity(mv.n);
            for c in (0..mv.n).map(|_| acc[mv.from].borrow_mut().pop().unwrap()) {
                to_push.push(c);
            }

            for c in to_push.into_iter().rev() {
                acc[mv.to].borrow_mut().push(c);
            }

            acc
        })
        .into_iter()
        .map(|c| c.borrow().last().unwrap().to_owned())
        .collect::<String>();

    println!("Part 1: {}", part1);
    println!("Part 2: {}", part2);
}
