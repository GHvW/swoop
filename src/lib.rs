use std::rc::Rc;

pub trait Parser {
    type Out;

    fn parse<'a>(&self, input: &'a str) -> Option<(Self::Out, &'a str)>;

    // fn map<B>(self, transform: Box<dyn Fn(Self::Out) -> B>) -> Map<Self, B> where Self: Sized {
    fn map<B, F: Fn(Self::Out) -> B>(self, transform: F) -> Map<Self, F> where Self: Sized {
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


pub struct Satisfy<P> {
    // predicate: Box<dyn Fn(char) -> bool>
    predicate: P
}


impl<P> Satisfy<P> {
    pub fn new(predicate: P) -> Self {
        Self {
            predicate: predicate
        }
    }
}

impl<P: Fn(char) -> bool> Parser for Satisfy<P> {
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

pub struct Map<P: Parser, F> {
    transform: F,
    parser: P
}


impl<P: Parser, F> Map<P, F> {
    pub fn new(transform: F, parser: P) -> Self {
        Self {
            transform: transform,
            parser: parser
        }
    }
}

impl<P: Parser, B, F: Fn(<P as Parser>::Out) -> B> Parser for Map<P, F> {
    type Out = B;

    fn parse<'a>(&self, input: &'a str) -> Option<(Self::Out, &'a str)> {
        self.parser
            .parse(input)
            .map(|(item, rest)| ((self.transform)(item), rest))
    }
}



pub struct Char {
    it: char
}

impl Char {
    pub fn new(it: char) -> Char {
        Self { it: it }
    }
}

impl Parser for Char {
    type Out = char;

    fn parse<'a>(&self, input: &'a str) -> Option<(Self::Out, &'a str)> {
        Satisfy::new(|item| item == self.it).parse(input)
    }
}

pub fn whitespace() -> Satisfy<fn(char) -> bool> {
    Satisfy { predicate: char::is_whitespace }
}

#[cfg(test)]
mod tests {
    use std::assert_eq;

    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }

    #[test]
    fn hello_world_satisfies_char_h() {
        let it = "hello world";

        let (result, rest) = Char::new('h').parse(it).unwrap();


        assert_eq!(result, 'h');
        assert_eq!(rest, "ello world");
    }


    #[test]
    fn hello_world_h_mapped() {
        let it = "hello world";


        let (result, rest): (char, &str) = 
            Char::new('h')
                .map(|item| item.to_ascii_uppercase())
                .parse(it)
                .unwrap();


        assert_eq!(result, 'H');
        assert_eq!(rest, "ello world");
    }

    #[test]
    fn whitespace_test() {
        let it = " world";


        let (result, rest): (char, &str) = 
            whitespace()
                .parse(it)
                .unwrap();


        assert_eq!(result, ' ');
        assert_eq!(rest, "world");
    }
}
