use std::rc::Rc;

pub trait Parser {
    type Out;

    fn parse<'a>(&self, input: &'a str) -> Option<(Self::Out, &'a str)>;

    fn map<B>(&self, transform: Box<dyn Fn(Self::Out) -> B>) -> Map<Self, B> where Self: Sized {
        Map::new(transform, self)
    }
}

struct Success<A> {
    item: Rc<A>,
}

impl<A> Success<A> {
    pub fn new(item: Rc<A>) -> Self {
        Self { item: item }
    }
}

impl<A: Clone> Parser for Success<A> {
    type Out = Rc<A>;

    fn parse<'a>(&self, input: &'a str) -> Option<(Self::Out, &'a str)> {
        Some((self.item.clone(), input))
    }
}

struct Zero<A> {
    phantom: std::marker::PhantomData<A>,
}

impl<A> Zero<A> {
    pub fn new() -> Self {
        Self {
            phantom: std::marker::PhantomData,
        }
    }
}

impl<A> Parser for Zero<A> {
    type Out = A;

    fn parse<'a>(&self, _input: &'a str) -> Option<(Self::Out, &'a str)> {
        None
    }
}


pub struct Satisfy {
    predicate: Box<dyn Fn(char) -> bool>
}


impl Satisfy {
    pub fn new(predicate: Box<dyn Fn(char) -> bool>) -> Self {
        Self {
            predicate: predicate
        }
    }
}

impl Parser for Satisfy {
    type Out = char;

    fn parse<'a>(&self, input: &'a str) -> Option<(Self::Out, &'a str)> {
        input.chars().next().and_then(|it| {
            if (self.predicate)(it) {
                Some((it, &input[1..]))
            } else {
                None
            }
        })
    }
}

pub struct Map<P: Parser, B> {
    transform: Box<dyn Fn(<P as Parser>::Out) -> B>,
    parser: P
}


impl<P: Parser, B> Map<P, B> {
    pub fn new(transform: Box<dyn Fn(<P as Parser>::Out) -> B>, parser: P) -> Self {
        Self {
            transform: transform,
            parser: parser
        }
    }
}

impl<P: Parser, B> Parser for Map<P, B> {
    type Out = B;

    fn parse<'a>(&self, input: &'a str) -> Option<(Self::Out, &'a str)> {
        self.parser
            .parse(input)
            .map(|(item, rest)| ((self.transform)(item), rest))
    }
}



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
