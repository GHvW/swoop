use std::rc::Rc;

pub trait Parser {
    type Out;

    fn parse<'a>(&self, input: &'a str) -> Option<(Self::Out, &'a str)>;

    fn map<B>(self, transform: Box<dyn Fn(Self::Out) -> B>) -> Map<Self, B>
    where
        Self: Sized,
    {
        Map::new(transform, self)
    }

    fn map2<Q: Parser, B>(
        self,
        second: Q,
        transform: Box<dyn Fn(Self::Out, <Q as Parser>::Out) -> B>,
    ) -> Map<And<Self, Q>, B>
    where
        Self: Sized,
    {
        self.and(second)
            .map(Box::new(|(item1, item2)| transform(item1, item2)))
    }

    fn flat_map<Q: Parser>(self, transform: Box<dyn Fn(Self::Out) -> Q>) -> FlatMap<Self, Q>
    where
        Self: Sized,
    {
        FlatMap::new(transform, self)
    }

    fn or(self, second: Self) -> Or<Self>
    where
        Self: Sized,
    {
        Or::new(self, second)
    }

    fn and<P: Parser>(self, second: P) -> And<Self, P>
    where
        Self: Sized,
    {
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

pub struct Satisfy {
    predicate: Box<dyn Fn(char) -> bool>, // predicate: P,
}

impl Satisfy {
    pub fn new(predicate: Box<dyn Fn(char) -> bool>) -> Self {
        Self {
            predicate: predicate,
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

pub struct Or<P> {
    first: P,
    second: P,
}

impl<P> Or<P> {
    pub fn new(first: P, second: P) -> Self {
        Self {
            first: first,
            second: second,
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
    second: Q,
}

impl<P, Q> And<P, Q> {
    pub fn new(first: P, second: Q) -> Self {
        Self {
            first: first,
            second: second,
        }
    }
}

impl<P: Parser, Q: Parser> Parser for And<P, Q> {
    type Out = (<P as Parser>::Out, <Q as Parser>::Out);

    fn parse<'a>(&self, input: &'a str) -> Option<(Self::Out, &'a str)> {
        self.first.parse(input).and_then(move |(item, rest)| {
            self.second
                .parse(rest)
                .map(|(next, leftover)| ((item, next), leftover))
        })
    }
}

pub struct Map<P: Parser, B> {
    transform: Box<dyn Fn(<P as Parser>::Out) -> B>,
    parser: P,
}

impl<P: Parser, B> Map<P, B> {
    pub fn new(transform: Box<dyn Fn(<P as Parser>::Out) -> B>, parser: P) -> Self {
        Self {
            transform: transform,
            parser: parser,
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

pub struct FlatMap<P: Parser, Q: Parser> {
    transform: Box<dyn Fn(<P as Parser>::Out) -> Q>,
    parser: P,
}

impl<P: Parser, Q: Parser> FlatMap<P, Q> {
    pub fn new(transform: Box<dyn Fn(<P as Parser>::Out) -> Q>, parser: P) -> Self {
        Self {
            transform: transform,
            parser: parser,
        }
    }
}

impl<P: Parser, Q: Parser> Parser for FlatMap<P, Q> {
    type Out = <Q as Parser>::Out;

    fn parse<'a>(&self, input: &'a str) -> Option<(Self::Out, &'a str)> {
        self.parser
            .parse(input)
            .and_then(|(item, rest)| ((self.transform)(item)).parse(rest))
    }
}

pub struct Char {
    it: char,
}

impl Char {
    pub fn new(it: char) -> Char {
        Self { it: it }
    }
}

impl Parser for Char {
    type Out = char;

    fn parse<'a>(&self, input: &'a str) -> Option<(Self::Out, &'a str)> {
        let the_char = self.it;
        Satisfy::new(Box::new(|item| item == the_char)).parse(input)
    }
}

pub struct Many<P> {
    parser: P,
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
    parser: P,
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

pub fn whitespace() -> Satisfy {
    Satisfy {
        predicate: Box::new(char::is_whitespace),
    }
}

#[cfg(test)]
mod tests {
    use std::{assert, assert_eq};

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

        let (result, rest): (char, &str) = Char::new('h')
            .map(Box::new(|item| item.to_ascii_uppercase()))
            .parse(it)
            .unwrap();

        assert_eq!(result, 'H');
        assert_eq!(rest, "ello world");
    }

    #[test]
    fn whitespace_test() {
        let it = " world";

        let (result, rest): (char, &str) = whitespace().parse(it).unwrap();

        assert_eq!(result, ' ');
        assert_eq!(rest, "world");
    }

    #[test]
    fn many_test() {
        let it = "hello world";

        let (result, rest) = Many::new(Satisfy::new(Box::new(char::is_alphabetic)))
            .parse(it)
            .unwrap();

        assert_eq!(result, vec!['h', 'e', 'l', 'l', 'o']);
        assert_eq!(rest, " world");
    }

    #[test]
    fn many_with_no_match_test() {
        let it = "hello world";

        let (result, rest) = Many::new(Char::new('X')).parse(it).unwrap();

        assert_eq!(result, vec![]);
        assert_eq!(rest, "hello world");
    }

    #[test]
    fn at_least_1_test() {
        let it = "hello world";

        let (result, rest) = AtLeast1::new(Satisfy::new(Box::new(char::is_alphabetic)))
            .parse(it)
            .unwrap();

        assert_eq!(result, vec!['h', 'e', 'l', 'l', 'o']);
        assert_eq!(rest, " world");
    }

    #[test]
    fn at_least_1_with_no_match_test() {
        let it = "hello world";

        let result = AtLeast1::new(Char::new('X')).parse(it);

        assert!(result.is_none());
    }

    #[test]
    fn hello_world_h_flat_mapped() {
        let it = "hello world";

        let (result, rest) = Char::new('h')
            .flat_map(Box::new(|item| {
                Success::new(Rc::new(item.to_ascii_uppercase()))
            }))
            .parse(it)
            .unwrap();

        assert_eq!(*result, 'H');
        assert_eq!(rest, "ello world");
    }

    #[test]
    fn or_first_success() {
        let it = "hello world";

        let (result, rest) = Char::new('h').or(Char::new('X')).parse(it).unwrap();

        assert_eq!(result, 'h');
        assert_eq!(rest, "ello world");
    }

    #[test]
    fn or_second_success() {
        let it = "hello world";

        let (result, rest) = Char::new('X').or(Char::new('h')).parse(it).unwrap();

        assert_eq!(result, 'h');
        assert_eq!(rest, "ello world");
    }
}
