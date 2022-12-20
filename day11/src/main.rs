use chumsky::prelude::*;
use std::cell::RefCell;
use std::fs::read_to_string;

#[derive(Debug, Clone)]
enum Operand {
    Num(usize),
    Old,
}

impl Operand {
    fn val(&self, i: usize) -> usize {
        match self {
            Self::Num(v) => *v,
            Self::Old => i,
        }
    }
}

#[derive(Debug, Clone)]
enum Op {
    Add(Operand),
    Mul(Operand),
}

impl Op {
    fn apply(&self, i: usize) -> usize {
        match self {
            Op::Add(v) => i + v.val(i),
            Op::Mul(v) => i * v.val(i),
        }
    }
}

#[derive(Debug, Clone)]
struct Monkey {
    number: usize,
    items: Vec<usize>,
    operation: Op,
    test: usize,
    if_true: usize,
    if_false: usize,
    inspected: usize,
}

impl Monkey {
    fn inspect_items(&mut self, divide: bool, modu: usize) -> Vec<(usize, usize)> {
        let thrown: Vec<(usize, usize)> = self
            .items
            .iter()
            .map(|i| {
                let new_i = (self.operation.apply(*i) / if divide { 3 } else { 1 }) % modu;

                if new_i % self.test == 0 {
                    (self.if_true, new_i)
                } else {
                    (self.if_false, new_i)
                }
            })
            .collect();

        self.items = vec![];
        self.inspected += thrown.len();

        thrown
    }
}

fn number() -> impl Parser<char, usize, Error = Simple<char>> {
    text::int(10).padded().map(|s: String| s.parse().unwrap())
}

fn operand() -> impl Parser<char, Operand, Error = Simple<char>> {
    number()
        .map(Operand::Num)
        .or(just("old").padded().to(Operand::Old))
}

fn monkey() -> impl Parser<char, Monkey, Error = Simple<char>> {
    let num = just("Monkey").ignore_then(number()).then_ignore(just(':'));

    let starting = just("Starting items:")
        .padded()
        .ignore_then(number().separated_by(just(',')));

    let operation = just("Operation: new = old").padded().ignore_then(choice((
        just("+").ignore_then(operand()).map(Op::Add),
        just("*").ignore_then(operand()).map(Op::Mul),
    )));

    let test = just("Test: divisible by").padded().ignore_then(number());

    let if_true = just("If true: throw to monkey")
        .padded()
        .ignore_then(number());
    let if_false = just("If false: throw to monkey")
        .padded()
        .ignore_then(number());

    num.then(starting)
        .then(operation)
        .then(test)
        .then(if_true)
        .then(if_false)
        .map(
            |(((((number, items), operation), test), if_true), if_false)| Monkey {
                number,
                items,
                operation,
                test,
                if_true,
                if_false,
                inspected: 0,
            },
        )
}

#[derive(Debug, Clone)]
struct MonkeyFamily {
    monkeys: Vec<RefCell<Monkey>>,
}

impl MonkeyFamily {
    fn run_rounds(&mut self, divide: bool, n: usize) {
        let modu = self.monkeys.iter().map(|m| m.borrow().test).product();

        for _ in 0..n {
            for monkey in &self.monkeys {
                let new_items = monkey.borrow_mut().inspect_items(divide, modu);
                for item in new_items {
                    self.monkeys[item.0].borrow_mut().items.push(item.1);
                }
            }
        }
    }

    fn monkey_business(&self) -> usize {
        let mut ns: Vec<usize> = self.monkeys.iter().map(|m| m.borrow().inspected).collect();
        ns.sort();

        ns.iter().rev().take(2).product()
    }
}

fn main() {
    let src = read_to_string(std::env::args().nth(1).unwrap()).unwrap();

    let mut monkeys = MonkeyFamily {
        monkeys: monkey()
            .repeated()
            .parse(src)
            .unwrap()
            .iter()
            .map(|m: &Monkey| RefCell::new(m.clone()))
            .collect::<Vec<RefCell<Monkey>>>(),
    };
    let mut monkeys2 = monkeys.clone();

    monkeys.run_rounds(true, 20);
    println!("Part 1: {}", monkeys.monkey_business());

    monkeys2.run_rounds(false, 10000);
    println!("Part 2: {}", monkeys2.monkey_business());
}
