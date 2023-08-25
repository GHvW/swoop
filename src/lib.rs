use std::rc::Rc;

pub trait Parser {
    type Out;

    fn parse<'a>(&self, input: &'a str) -> Option<(Self::Out, &'a str)>;

    // fn map<B>(self, transform: Box<dyn Fn(Self::Out) -> B>) -> Map<Self, B> where Self: Sized {
    fn map<B, F: Fn(Self::Out) -> B>(self, transform: F) -> Map<Self, F> where Self: Sized {
        Map::new(transform, self)
    }

    fn map2<Q: Parser, B, F: Fn(Self::Out, <Q as Parser>::Out) -> B>(self, second: Q, transform: F) -> Map2<Self, Q, F> where Self: Sized {
        Map2::new(transform, self, second)
    }

    fn flat_map<Q: Parser, F: Fn(Self::Out) -> Q>(self, transform: F) -> FlatMap<Self, F> where Self: Sized {
        FlatMap::new(transform, self)
    }

    fn or(self, second: Self) -> Or<Self> where Self: Sized {
        Or::new(self, second)
    }

    fn and<P: Parser>(self, second: P) -> And<Self, P> where Self: Sized {
        And::new(self, second)
    }
}

struct Success<A> {
    item: A,
}

impl<A: Clone> Success<A> {
    pub fn new(item: A) -> Self {
        Self { item: item.clone() }
    }
}

impl<A: Clone> Parser for Success<A> {
    type Out = A;

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

pub struct Or<P> {
    first: P,
    second: P
}


impl<P> Or<P> {
    pub fn new(first: P, second: P) -> Self {
        Self {
            first: first,
            second: second
        }
    }
}

impl<P: Parser> Parser for Or<P> {
    type Out = <P as Parser>::Out;

    fn parse<'a>(&self, input: &'a str) -> Option<(Self::Out, &'a str)> {
        self.first.parse(input).or(self.second.parse(input))
    }
}

pub struct And<P, Q> {
    first: P,
    second: Q
}


impl<P, Q> And<P, Q> {
    pub fn new(first: P, second: Q) -> Self {
        Self {
            first: first,
            second: second
        }
    }
}

impl<P: Parser, Q: Parser> Parser for And<P, Q> {
    type Out = (<P as Parser>::Out, <Q as Parser>::Out);

    fn parse<'a>(&self, input: &'a str) -> Option<(Self::Out, &'a str)> {
        self.first.parse(input).and_then(move |(item, rest)| {
            self.second.parse(rest).map(|(next, leftover)| {
                ((item, next), leftover)
            })
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

pub struct Map2<P: Parser, Q, F> {
    transform: F,
    first: P,
    second: Q
}


impl<P: Parser, Q, F> Map2<P, Q, F> {
    pub fn new(transform: F, first: P, second: Q) -> Self {
        Self {
            transform: transform,
            first: first,
            second: second 
        }
    }
}

impl<P: Parser, Q: Parser, B, F: Fn(<P as Parser>::Out, <Q as Parser>::Out) -> B> Parser for Map2<P, Q, F> {
    type Out = B;

    fn parse<'a>(&self, input: &'a str) -> Option<(Self::Out, &'a str)> {
        self.first.parse(input).and_then(|(item, rest)| {
            self.second.parse(rest).map(|(item2, leftover)| {
                ((self.transform)(item, item2), leftover)
            })
        })
    }
}

pub struct FlatMap<P, F> {
    transform: F,
    parser: P
}


impl<P, F> FlatMap<P, F> {
    pub fn new(transform: F, parser: P) -> Self {
        Self {
            transform: transform,
            parser: parser
        }
    }
}

impl<P: Parser, Q: Parser, F: Fn(<P as Parser>::Out) -> Q> Parser for FlatMap<P, F> {
    type Out = <Q as Parser>::Out;

    fn parse<'a>(&self, input: &'a str) -> Option<(Self::Out, &'a str)> {
        self.parser
            .parse(input)
            .and_then(|(item, rest)| ((self.transform)(item)).parse(rest))
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


pub struct Many<P> {
    parser: P
}

impl<P> Many<P> {
    pub fn new(parser: P) -> Self {
        Self { parser: parser }
    }
}

impl<P: Parser> Parser for Many<P> {
    type Out = Vec<<P as Parser>::Out>;

    fn parse<'a>(&self, input: &'a str) -> Option<(Self::Out, &'a str)> {
        let mut results = Vec::new();
        let mut leftover = input;

        while let Some((item, rest)) = self.parser.parse(leftover) {
            results.push(item);
            leftover = rest;
        }

        Some((results, leftover))
    }
}

pub struct AtLeast1<P> {
    parser: P
}

impl<P> AtLeast1<P> {
    pub fn new(parser: P) -> Self {
        Self { parser: parser }
    }
}

impl<P: Parser> Parser for AtLeast1<P> {
    type Out = Vec<<P as Parser>::Out>;

    fn parse<'a>(&self, input: &'a str) -> Option<(Self::Out, &'a str)> {
        self.parser.parse(input).map(|(item, remaining)| {
            let mut results = vec![item];
            let mut leftover = remaining;

            while let Some((item, rest)) = self.parser.parse(leftover) {
                results.push(item);
                leftover = rest;
            }

            (results, leftover)
        })
    }
}



pub fn whitespace() -> Satisfy<fn(char) -> bool> {
    Satisfy { predicate: char::is_whitespace }
}

#[cfg(test)]
mod tests {
    use std::{assert_eq, assert};

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

    #[test]
    fn many_test() {
        let it = "hello world";


        let (result, rest)  = 
            Many::new(Satisfy::new(char::is_alphabetic))
                .parse(it)
                .unwrap();


        assert_eq!(result, vec!['h', 'e', 'l', 'l', 'o']);
        assert_eq!(rest, " world");
    }

    #[test]
    fn many_with_no_match_test() {
        let it = "hello world";


        let (result, rest)  = 
            Many::new(Char::new('X'))
                .parse(it)
                .unwrap();


        assert_eq!(result, vec![]);
        assert_eq!(rest, "hello world");
    }

    #[test]
    fn at_least_1_test() {
        let it = "hello world";


        let (result, rest)  = 
            AtLeast1::new(Satisfy::new(char::is_alphabetic))
                .parse(it)
                .unwrap();


        assert_eq!(result, vec!['h', 'e', 'l', 'l', 'o']);
        assert_eq!(rest, " world");
    }

    #[test]
    fn at_least_1_with_no_match_test() {
        let it = "hello world";


        let result = 
            AtLeast1::new(Char::new('X'))
                .parse(it);


        assert!(result.is_none());
    }

    #[test]
    fn hello_world_h_flat_mapped() {
        let it = "hello world";


        let (result, rest) = 
            Char::new('h')
                .flat_map(|item| Success::new(Rc::new(item.to_ascii_uppercase())))
                .parse(it)
                .unwrap();


        assert_eq!(*result, 'H');
        assert_eq!(rest, "ello world");
    }

    #[test]
    fn or_first_success() {
        let it = "hello world";


        let (result, rest) = 
            Char::new('h')
                .or(Char::new('X'))
                .parse(it)
                .unwrap();


        assert_eq!(result, 'h');
        assert_eq!(rest, "ello world");
    }

    #[test]
    fn or_second_success() {
        let it = "hello world";


        let (result, rest) = 
            Char::new('X')
                .or(Char::new('h'))
                .parse(it)
                .unwrap();


        assert_eq!(result, 'h');
        assert_eq!(rest, "ello world");
    }


}
