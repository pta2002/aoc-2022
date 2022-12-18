use core::fmt;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use chumsky::prelude::*;

#[derive(Debug, Clone, PartialEq)]
enum Directory {
    Root,
    Parent,
    Named(String),
}

#[derive(Debug)]
enum Operation {
    ChangeDir(Directory),
    List(Vec<Listing>),
}

#[derive(Debug)]
enum Listing {
    Dir(String),
    File(usize, String),
}

enum FsNode {
    File(Rc<RefCell<Self>>, usize, String),
    Dir(
        Rc<RefCell<Self>>,
        HashMap<String, Rc<RefCell<FsNode>>>,
        String,
    ),
    Root(HashMap<String, Rc<RefCell<FsNode>>>),
}

impl FsNode {
    fn size(&self) -> usize {
        match self {
            FsNode::File(_, s, _) => *s,
            FsNode::Dir(_, nodes, _) => nodes.iter().map(|(_, v)| v.borrow().size()).sum(),
            FsNode::Root(nodes) => nodes.iter().map(|(_, v)| v.borrow().size()).sum(),
        }
    }

    fn parent(&self) -> Rc<RefCell<FsNode>> {
        match self {
            FsNode::File(parent, _, _) => parent.clone(),
            FsNode::Dir(parent, _, _) => parent.clone(),
            FsNode::Root(_) => panic!("Root has no parent"),
        }
    }

    fn dirs(&self) -> HashMap<String, Rc<RefCell<FsNode>>> {
        match self {
            FsNode::Dir(_, dirs, _) => dirs.clone(),
            FsNode::Root(dirs) => dirs.clone(),
            _ => panic!("Tried to list contents of a file"),
        }
    }

    fn name(&self) -> &str {
        match self {
            FsNode::Dir(_, _, name) => name,
            FsNode::File(_, _, name) => name,
            FsNode::Root(_) => "/",
        }
    }

    fn push(&mut self, node: FsNode) {
        match self {
            FsNode::Root(dirs) => dirs.insert(node.name().to_owned(), Rc::new(RefCell::new(node))),
            FsNode::Dir(_, dirs, _) => {
                dirs.insert(node.name().to_owned(), Rc::new(RefCell::new(node)))
            }
            _ => panic!("Can't insert on a file"),
        };
    }

    fn is_dir(&self) -> bool {
        match self {
            FsNode::File(_, _, _) => false,
            _ => true,
        }
    }

    fn part1(&self) -> usize {
        let dirs = self.dirs();

        dirs.iter().fold(0, |acc, dir| {
            if dir.1.borrow().is_dir() {
                let size = dir.1.borrow().size();
                let acc = acc + dir.1.borrow().part1();
                if size <= 100000 {
                    acc + size
                } else {
                    acc
                }
            } else {
                acc
            }
        })
    }

    fn part2(&self, free: usize) -> Option<usize> {
        if self.is_dir() {
            let dirs = self.dirs();
            let smallest = dirs
                .values()
                .into_iter()
                .filter_map(|n| n.borrow().part2(free))
                .min();

            let me = self.size();

            smallest.or(if me > free { Some(me) } else { None })
        } else {
            None
        }
    }
}

impl fmt::Debug for FsNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Root(children) => f
                .debug_struct("FsNode")
                .field("name", &"/")
                .field("children", children)
                .finish(),
            Self::Dir(_, children, name) => f
                .debug_struct("FsNode")
                .field("name", name)
                .field("children", children)
                .finish(),
            Self::File(_, size, name) => f
                .debug_struct("FsNode")
                .field("name", name)
                .field("size", size)
                .finish(),
        }
    }
}

fn file_name() -> impl Parser<char, String, Error = Simple<char>> {
    filter(|a| !char::is_whitespace(*a)).repeated().collect()
}

fn directory_name() -> impl Parser<char, Directory, Error = Simple<char>> {
    choice((
        just('/').to(Directory::Root),
        just('.').then(just('.')).to(Directory::Parent),
        text::ident().map(Directory::Named),
    ))
    .padded()
}

fn operation_parser() -> impl Parser<char, Operation, Error = Simple<char>> {
    let cd = text::keyword("cd")
        .padded()
        .ignore_then(directory_name())
        .map(Operation::ChangeDir);

    let list = text::keyword("ls")
        .padded()
        .ignore_then(listing().separated_by(text::newline()))
        .map(Operation::List);

    just('$').padded().ignore_then(cd.or(list))
}

fn listing() -> impl Parser<char, Listing, Error = Simple<char>> {
    let dir = text::keyword("dir")
        .padded()
        .ignore_then(text::ident())
        .map(Listing::Dir);

    let file = text::int(10)
        .padded()
        .from_str()
        .then(file_name())
        .map(|(s, n)| Listing::File(s.unwrap(), n));

    choice((dir, file))
}

fn operations() -> impl Parser<char, Vec<Operation>, Error = Simple<char>> {
    operation_parser().repeated()
}

fn main() {
    let src = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();

    let ops = operations().parse(src).unwrap();

    let root = Rc::new(RefCell::new(FsNode::Root(HashMap::new())));
    let mut cur = root.clone();

    for op in ops {
        match op {
            Operation::ChangeDir(Directory::Root) => {
                cur = root.clone();
            }
            Operation::ChangeDir(Directory::Parent) => {
                let parent = cur.borrow().parent();
                cur = parent;
            }
            Operation::ChangeDir(Directory::Named(name)) => {
                let dirs = cur.borrow().dirs();

                cur = dirs.get(&name).unwrap().clone();
            }
            Operation::List(files) => {
                for file in files {
                    let new = match file {
                        Listing::File(size, name) => FsNode::File(cur.clone(), size, name.clone()),
                        Listing::Dir(name) => FsNode::Dir(cur.clone(), HashMap::new(), name),
                    };
                    cur.borrow_mut().push(new);
                }
            }
        }
    }

    println!("Part 1: {}", root.borrow().part1());
    println!(
        "Part 2: {}",
        root.borrow()
            .part2(30000000 - (70000000 - root.borrow().size()))
            .unwrap()
    );
}

#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn parse_dir() {
        use Directory::*;
        assert_eq!(directory_name().parse("/"), Ok(Root));
        assert_eq!(directory_name().parse(".."), Ok(Parent));
        assert_eq!(directory_name().parse("a"), Ok(Named(String::from("a"))));
    }

    #[test]
    fn parse_dirs() {}
}
