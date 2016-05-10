#![feature(question_mark)]
#![allow(dead_code)]

extern crate itertools;
extern crate rand;

use rand::distributions::{IndependentSample, Range};
use std::{str, iter};
use itertools::Itertools;

pub type CharStream<'a> = iter::Peekable<str::Chars<'a>>;

#[derive(Debug, PartialEq)]
pub enum GSError {
    Parse(String),
    Runtime(String)
}

type GSErr = Result<(), GSError>;

/// An `Item` can exist on the stack.
#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum Item {
    Op(char),
    Num(i64),
    Str(String),
    Array(Box<[Item]>),
    Block(Box<[Item]>),
}

impl Item {
    /// Upcast the specified `Item` into an `Item::Array`
    ///
    /// Accepts: Num, Array
    ///
    /// ### Num
    /// Transforms into a single element array with the number.
    ///
    /// ### Array
    /// Nop
    fn upcast_to_array(self) -> Item {
        match self {
            x @ Item::Num(_) => Item::Array(vec![x].into_boxed_slice()),

            x @ Item::Array(_) => x,

            _ => panic!("upcast_to_array only accepts num, array")
        }
    }

    /// Upcast the specified `Item` into a `Item::Str`
    ///
    /// Accepts: Num, Array, String
    ///
    /// ### Num
    /// Parses the integer as a string. `34 => '34'`.
    ///
    /// ### Array
    /// Converts each element into a string. `Num` is treated as an
    /// ascii value prior to conversion.
    ///
    /// ### Str
    /// Nop
    fn upcast_to_string(self) -> Item {
        unimplemented!()
    }

    /// Upcast the specified `Item` into a `Item::Block`
    ///
    /// Accepts: Num, Array, String, Block
    ///
    /// ### Num
    fn upcast_to_block(self) -> Item {
        unimplemented!()
    }
}

// Coerce the specified items a similar type.
fn coerce((x, y): (Item, Item)) -> (Item, Item) {
    (x, y)
}

pub fn lex_item(mut chars: &mut CharStream) -> Option<Result<Item, GSError>> {
    loop {
        let item = match chars.next() {
            Some('#') => {
                while let Some(&ch) = chars.peek() {
                    chars.next();
                    if ch == '\n' {
                        break;
                    }
                }

                continue;
            }

            Some('"') => {
                let mut string = String::new();
                loop {
                    match chars.next() {
                        Some('\\') => {
                            match chars.next() {
                                Some(ch) => string.push(ch),
                                None => {
                                    return Some(Err(GSError::Parse(
                                        "invalid escape sequence".to_string()
                                    )));
                                }
                            }
                        }

                        Some('"') => break,
                        Some(ch) => string.push(ch),
                        None => {
                            return Some(Err(GSError::Parse(
                                "eof while scanning string literal".to_string()
                            )));
                        }
                    }
                }

                string.replace("\\\\", "\\");
                string.replace("\\\"", "\"");
                Item::Str(string)
            }

            Some('{') => {
                let mut block_items = Vec::new();
                loop {
                    match chars.peek() {
                        Some(&'}') => {
                            chars.next();
                            break;
                        }

                        // We must handle whitespace here else we can read
                        // `None` and skip final '}'
                        Some(&ch) if ch.is_whitespace() => {
                            chars.next();
                            continue
                        }

                        // Handle eof/`None` on `lex_item` call
                        Some(_) | None => {
                            let item = match lex_item(&mut chars) {
                                Some(ch) => ch,
                                None => {
                                    return Some(Err(GSError::Parse(
                                        "eof while scanning for '}'".to_string()
                                    )))
                                }
                            };

                            block_items.push(
                                if item.is_ok() {
                                    item.unwrap()
                                } else {
                                    return Some(item)
                                }
                            );
                        }
                    }
                }

                Item::Block(block_items.into_boxed_slice())
            }

            Some(ch) if ch.is_digit(10) => {
                let mut num = String::new();
                num.push(ch);

                while let Some(&ch) = chars.peek() {
                    if ch.is_digit(10) {
                        num.push(ch);
                        chars.next();
                    } else {
                        break;
                    }
                }

                Item::Num(num.parse::<i64>().unwrap())
            }

            // If we encounter a '-' immediately followed by a number, this
            // is bound to the number instead of treated as an operator.
            Some(ch) if ch == '-' => {
                match chars.peek() {
                    Some(&ch) if ch.is_digit(10) => {
                        match lex_item(&mut chars) {
                            Some(Ok(Item::Num(x))) => Item::Num(-x),
                            Some(Err(e)) => return Some(Err(e)),
                            _ => unreachable!()
                        }
                    }

                    _ => Item::Op('-')
                }
            }

            Some(ch) if ch.is_whitespace() => continue,
            Some(ch) => Item::Op(ch),
            _ => return None
        };

        return Some(Ok(item))
    }
}

pub fn lex(input: &str) -> Result<Box<[Item]>, GSError> {
    let mut chars = input.chars().peekable();
    let mut tokens = Vec::new();

    while let Some(ch) = lex_item(&mut chars) {
        tokens.push(ch?);
    }

    Ok(tokens.into_boxed_slice())
}

#[derive(Debug)]
pub struct Interpreter {
    stack: Vec<Item>,

    /// Store all past stack markers
    marker_stack: Vec<usize>
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            stack: Vec::new(),
            marker_stack: Vec::new()
        }
    }

    /// Push a value onto the stack
    ///
    /// # Panics
    /// panics if the value exceeds the length of a usize.
    fn push(&mut self, value: Item) {
        self.stack.push(value)
    }

    /// Pop a single value off the stack.
    fn pop(&mut self) -> Result<Item, GSError> {
        match self.stack.pop() {
            Some(value) => {
                // Resize all markers that are beyond the edge of the stack
                for mut marker in self.marker_stack.iter_mut() {
                    if *marker > self.stack.len() {
                        *marker = self.stack.len();
                    }
                }

                Ok(value)
            }

            None => Err(GSError::Runtime("stack underflow".to_string()))
        }
    }

    /// Pop the top two values off the stack.
    fn pop2(&mut self) -> Result<(Item, Item), GSError> {
        // Order of execution in ',' values is defined in rust.
        Ok((self.pop()?, self.pop()?))
    }

    /// Execute a string, returning the result of execution.
    pub fn exec(&mut self, input: &str) -> Result<String, GSError> {
        let items = lex(&input)?;
        self.exec_items(&items)
    }

    /// Execute a sequence of items, returning the result of execution.
    pub fn exec_items(&mut self, items: &[Item])-> Result<String, GSError> {
        for item in items.iter().cloned() {
            match item {
                // Can we restructure to allow for rebound variables?
                Item::Op('+') => self.add()?,
                Item::Op('-') => self.sub()?,
                Item::Op('!') => self.not()?,
                Item::Op('@') => self.at()?,
                Item::Op('$') => self.dollar()?,
                Item::Op('*') => self.mul()?,
                Item::Op('/') => self.div()?,
                Item::Op('%') => self.modulo()?,
                Item::Op('|') => self.or()?,
                Item::Op('&') => self.and()?,
                Item::Op('^') => self.xor()?,
                Item::Op('\\') => self.swap()?,
                Item::Op(';') => self.pop_discard()?,
                Item::Op('<') => self.lt()?,
                Item::Op('>') => self.gt()?,
                Item::Op('=') => self.eq()?,
                Item::Op('.') => self.dup()?,
                Item::Op('?') => self.qmark()?,
                Item::Op('(') => self.dec()?,
                Item::Op(')') => self.inc()?,
                Item::Op('[') => self.marker()?,
                Item::Op(']') => self.slice()?,
                Item::Op('~') => self.neg()?,
                Item::Op(',') => self.array()?,
                Item::Op('a') => self.builtin_abs()?,
                Item::Op('i') => self.builtin_if()?,
                Item::Op('r') => self.builtin_rand()?,
                Item::Op('p') => self.builtin_p()?,
                Item::Op('n') => self.builtin_n()?,

                x @ Item::Num(_) | x @ Item::Str(_) | x @ Item::Block(_) => {
                    self.push(x)
                }

                x @ _ => {
                    return Err(GSError::Runtime(
                            format!("invalid token encountered: {:?}", x))
                    );
                }
            }
        }

        Ok(format!("{:?}", self.stack))
    }

    /// +
    fn add(&mut self) -> GSErr {
        match coerce(self.pop2()?) {
            (Item::Num(x), Item::Num(y)) => self.push(Item::Num(x + y)),

            (Item::Str(x), Item::Str(y)) => {
                self.push(Item::Str(y + &x))
            }

            (Item::Array(x), Item::Array(y)) => {
                self.push(Item::Array(
                    y.iter().chain(x.iter())
                            .cloned()
                            .collect_vec()
                            .into_boxed_slice()
                ));

            }

            (Item::Block(x), Item::Block(y)) => {
                self.push(Item::Block(
                    y.iter().chain(x.iter())
                            .cloned()
                            .collect_vec()
                            .into_boxed_slice()
                ));
            }

            _ => unimplemented!()
        }

        Ok(())
    }

    /// -
    fn sub(&mut self) -> GSErr {
        // Handle parsing of numbers which '-' is a unary operator
        // This should be done in the lexer, a number is negative if the
        // '-' symbol immediately precedes the value (special lexer case)
        match coerce(self.pop2()?) {
            (Item::Num(x), Item::Num(y)) => self.push(Item::Num(y - x)),
            _ => unimplemented!()
        }

        Ok(())
    }

    /// !
    fn not(&mut self) -> GSErr {
        match self.pop()? {
            Item::Num(x) => {
                self.push(Item::Num(if x == 0 { 1 } else { 0 }))
            }

            Item::Str(ref x) => {
                self.push(Item::Num(if x == "" { 1 } else { 0 }))
            }

            Item::Array(ref x) | Item::Block(ref x) => {
                self.push(Item::Num(if x.is_empty() { 1 } else { 0 }))
            }

            _ => unimplemented!()
        }

        Ok(())
    }

    /// @
    fn at(&mut self) -> GSErr {
        let (x, y) = self.pop2()?;
        let z = self.pop()?;
        self.push(y);
        self.push(x);
        self.push(z);

        Ok(())
    }

    /// $
    fn dollar(&mut self) -> GSErr {
        match self.pop()? {
            Item::Num(x) => {
                if x >= self.stack.len() as i64 || x < 0 {
                    return Err(GSError::Runtime(
                        "attempting to index beyond stack".to_string()
                    ));
                }

                let os = self.stack.len() - x as usize - 1;
                let value = self.stack[os].clone();
                self.push(value);
            }

            Item::Str(x) => {
                let mut buf: Vec<char> = x.chars().collect();
                buf.sort();
                self.push(Item::Str(buf.into_iter().collect()))
            },

            _ => unimplemented!()
        }

        Ok(())
    }

    /// *
    fn mul(&mut self) -> GSErr {
        match self.pop2()? {
            (Item::Num(x), Item::Num(y)) => self.push(Item::Num(x * y)),

            (Item::Num(y), Item::Str(x)) |
            (Item::Str(x), Item::Num(y)) => {
                if y < 0 {
                    return Err(GSError::Runtime(
                        "repeat string value is negative".to_string()
                    ));
                }
                self.push(Item::Str(iter::repeat(x).take(y as usize).collect()))
            },

            (Item::Num(y), Item::Array(x)) |
            (Item::Array(x), Item::Num(y)) => {
                if y < 0 {
                    return Err(GSError::Runtime(
                        "repeat string value is negative".to_string()
                    ));
                }

                self.push(Item::Array(x.iter().cloned()
                                              .cycle()
                                              .take(x.len() * y as usize)
                                              .collect_vec()
                                              .into_boxed_slice()
                ))
            }

            _ => unimplemented!()
        }
        Ok(())
    }

    /// /
    fn div(&mut self) -> GSErr {
        match self.pop2()? {
            (Item::Num(x), Item::Num(y)) => self.push(Item::Num(y / x)),

            _ => unimplemented!()
        }
        Ok(())
    }

    /// %
    fn modulo(&mut self) -> GSErr {
        match self.pop2()? {
            (Item::Num(x), Item::Num(y)) => self.push(Item::Num(y % x)),

            _ => unimplemented!()
        }
        Ok(())
    }

    /// ~
    fn neg(&mut self) -> GSErr {
        match self.pop()? {
            Item::Num(x) => self.push(Item::Num(!x)),

            Item::Array(x) => {
                for item in x.iter().cloned() {
                    self.push(item)
                }
            }

            Item::Str(ref x) => {
                match self.exec(x) {
                    Err(e) => return Err(GSError::Runtime(
                                  format!("invalid expression statement: {:?}", e)
                              )),
                    _ => ()
                }
            }

            Item::Block(ref x) => {
                match self.exec_items(x) {
                    Err(e) => return Err(GSError::Runtime(
                                  format!("invalid expression statement: {:?}", e)
                              )),
                    _ => ()
                }
            }

            _ => unimplemented!()
        }
        Ok(())
    }

    /// |
    fn or(&mut self) -> GSErr {
        match coerce(self.pop2()?) {
            (Item::Num(x), Item::Num(y)) => self.push(Item::Num(x | y)),

            (Item::Array(x), Item::Array(y)) => {
                self.push(Item::Array(
                    x.iter().cloned()
                            .chain(y.iter().cloned())
                            .unique()
                            .collect_vec()
                            .into_boxed_slice()
                ));
            }

            _ => unimplemented!()
        }
        Ok(())
    }

    /// &
    fn and(&mut self) -> GSErr {
        match coerce(self.pop2()?) {
            (Item::Num(x), Item::Num(y)) => self.push(Item::Num(x & y)),

            (Item::Array(x), Item::Array(y)) => {
                // TODO: Incorrect value
                self.push(Item::Array(
                    x.iter().cloned()
                            .filter(|ref x| !y.contains(x))
                            .unique()
                            .collect_vec()
                            .into_boxed_slice()
                ));
            }

            _ => unimplemented!()
        }
        Ok(())
    }

    /// ^
    fn xor(&mut self) -> GSErr {
        match coerce(self.pop2()?) {
            (Item::Num(x), Item::Num(y)) => self.push(Item::Num(x ^ y)),

            _ => unimplemented!()
        }
        Ok(())
    }

    // \
    fn swap(&mut self) -> GSErr {
        let (x, y) = self.pop2()?;
        self.push(x);
        self.push(y);
        Ok(())
    }

    // ;
    fn pop_discard(&mut self) -> GSErr {
        self.pop()?;
        Ok(())
    }

    // <
    fn lt(&mut self) -> GSErr {
        match self.pop2()? {
            (Item::Num(y), Item::Num(x)) => {
                self.push(Item::Num(
                    if x < y { 1 } else { 0 }
                ));
            }

            (Item::Str(y), Item::Str(x)) => {
                self.push(Item::Num(
                    if x < y { 1 } else { 0 }
                ));
            }

            (Item::Num(x), Item::Array(y)) |
            (Item::Array(y), Item::Num(x)) => {
                self.push(Item::Array(
                    y.iter().cloned()
                            .take(x as usize)
                            .collect_vec()
                            .into_boxed_slice()
                ));
            }

            _ => unimplemented!()
        }

        Ok(())
    }

    // >
    fn gt(&mut self) -> GSErr {
        match self.pop2()? {
            (Item::Num(y), Item::Num(x)) => {
                self.push(Item::Num(
                    if x > y { 1 } else { 0 }
                ));
            }
            (Item::Str(y), Item::Str(x)) => {
                self.push(Item::Num(
                    if x > y { 1 } else { 0 }
                ));
            }

            // Dont implement str specifically, upcast to an array and
            // apply this way

            (Item::Num(x), Item::Array(y)) |
            (Item::Array(y), Item::Num(x)) => {
                self.push(Item::Array(
                    y.iter().cloned()
                            .skip(x as usize)
                            .collect_vec()
                            .into_boxed_slice()
                ));
            }

            _ => unimplemented!()
        }

        Ok(())
    }

    // =
    fn eq(&mut self) -> GSErr {
        match self.pop2()? {
            (Item::Num(x), Item::Num(y)) => {
                self.push(Item::Num(
                    if x == y { 1 } else { 0 }
                ));
            }
            (Item::Str(x), Item::Str(y)) => {
                self.push(Item::Num(
                    if x == y { 1 } else { 0 }
                ));
            }

            (Item::Num(x), Item::Array(y)) |
            (Item::Array(y), Item::Num(x)) => {
                let os = if x < 0 { y.len() as i64 + x } else { x };

                if 0 <= os && os < y.len() as i64 {
                    self.push(y[os as usize].clone());
                }
            }

            _ => unimplemented!()
        }

        Ok(())
    }

    // ,
    fn array(&mut self) -> GSErr {
        match self.pop()? {
            Item::Num(x) => {
                self.push(Item::Array((0..x).map(Item::Num)
                                            .collect_vec()
                                            .into_boxed_slice()
                ));
            },

            Item::Array(x) => {
                self.push(Item::Num(x.len() as i64));
            },

            _ => unimplemented!()
        }
        Ok(())
    }

    // .
    fn dup(&mut self) -> GSErr {
        let x = self.pop()?;
        self.push(x.clone());
        self.push(x);
        Ok(())
    }

    // ?
    fn qmark(&mut self) -> GSErr {
        match self.pop2()? {
            (Item::Num(y), Item::Num(x)) => {
                if y < 0 {
                    return Err(GSError::Runtime(
                        "cannot raise to negative power".to_string()
                    ));
                }

                // Handle overflow somehow (may have to use own power)
                self.push(Item::Num(x.pow(y as u32)))
            }

            (Item::Num(x), Item::Array(y)) |
            (Item::Array(y), Item::Num(x)) => {
                self.push(Item::Num(
                        y.iter().position(|v| v == &Item::Num(x))
                                .map_or_else(|| -1, |x| x as i64)
                ));
            }

            _ => unimplemented!()
        }

        Ok(())
    }

    // (
    fn dec(&mut self) -> GSErr {
        match self.pop()? {
            Item::Num(x) => self.push(Item::Num(x - 1)),

            Item::Array(x) => {
                if !x.is_empty() {
                    let mut buf = x.into_vec();
                    let cons = buf.remove(0);
                    self.push(Item::Array(buf.into_boxed_slice()));
                    self.push(cons);
                }
            }

            _ => unimplemented!()
        }

        Ok(())
    }

    // )
    fn inc(&mut self) -> GSErr {
        match self.pop()? {
            Item::Num(x) => self.push(Item::Num(x + 1)),

            Item::Array(x) => {
                if !x.is_empty() {
                    let mut buf = x.into_vec();
                    let uncons = buf.pop().unwrap();
                    self.push(Item::Array(buf.into_boxed_slice()));
                    self.push(uncons);
                }
            }

            _ => unimplemented!()
        }

        Ok(())
    }

    // [
    fn marker(&mut self) -> GSErr {
        self.marker_stack.push(self.stack.len());
        Ok(())
    }

    // ]
    fn slice(&mut self) -> GSErr {
        let offset = match self.marker_stack.pop() {
            Some(value) => value,
            None => return Err(GSError::Runtime("marker stack underflow".to_string()))
        };

        let array_items = self.stack.split_off(offset).into_boxed_slice();
        self.push(Item::Array(array_items));
        Ok(())
    }

    // a (abs)
    fn builtin_abs(&mut self) -> GSErr {
        match self.pop()? {
            Item::Num(x) => self.push(Item::Num(x.abs())),
            x @ _ => return Err(GSError::Runtime(
                         format!("invalid type for `abs`: {:?}", x)
                     ))
        }
        Ok(())
    }

    // i (if)
    fn builtin_if(&mut self) -> GSErr {
        self.not()?; // Evaluate if top of stack is not, note this requires
                     // a reverse condition check in the following.

        // TODO: consider block case
        match self.pop()? {
            Item::Num(x) => {
                let (a, b) = self.pop2()?;

                self.push(
                    if x == 0 {
                        a
                    } else if x == 1 {
                        b
                    } else {
                        panic!("expected 0 or 1 but found: {:?}", x)
                    }
                );
            },

            x @ _ => panic!("expected number but found: {:?}", x)
        }

        Ok(())
    }

    // r (rand)
    fn builtin_rand(&mut self) -> GSErr {
        match self.pop()? {
            Item::Num(x) => {
                if x == 0 {
                    return Err(GSError::Runtime(
                        "invalid random range: [0, 0)".to_string()
                    ));
                }

                let range = if x < 0 {
                    Range::new(x, 0)
                } else {
                    Range::new(0, x)
                };

                let mut rng = rand::thread_rng();
                self.push(Item::Num(range.ind_sample(&mut rng)))
            }

            x @ _ => panic!("invalid type for `rand`: {:?}", x)
        }
        Ok(())
    }

    // p
    fn builtin_p(&mut self) -> GSErr {
        println!("{:?}", self.pop()?);
        Ok(())
    }

    // n (newline)
    fn builtin_n(&mut self) -> GSErr {
        self.push(Item::Str("\n".to_string()));
        Ok(())
    }
}

#[allow(unused_imports)]
mod tests {
    use super::*;
    use super::Item::*;

    macro_rules! eval {
        ($input:expr) => {
            {
                let mut it = Interpreter::new();
                it.exec(&$input)
            }
        };
    }

    // test~
    #[test]
    fn negate_num() {
        assert_eq!(eval!("5~"), Ok("[Num(-6)]".to_string()));
    }

    #[test]
    fn negate_str() {
        assert_eq!(eval!("\"1 2+\"~"), Ok("[Num(3)]".to_string()));
    }

    #[test]
    fn negate_array() {
        assert_eq!(eval!("[1 2 3]~"), Ok("[Num(1), Num(2), Num(3)]".to_string()));
    }

    #[test]
    fn negate_block() {
        assert_eq!(eval!("{1 2+}~"), Ok("[Num(3)]".to_string()));
    }

    // test`

    // test!
    #[test]
    fn exclaim_num() {
        assert_eq!(eval!("0!"), Ok("[Num(1)]".to_string()));
        assert_eq!(eval!("1!"), Ok("[Num(0)]".to_string()));
    }

    #[test]
    fn exclaim_str() {
        assert_eq!(eval!("\"\"!"), Ok("[Num(1)]".to_string()));
        assert_eq!(eval!("\"asdf\"!"), Ok("[Num(0)]".to_string()));
    }

    #[test]
    fn exclaim_array() {
        assert_eq!(eval!("[]!"), Ok("[Num(1)]".to_string()));
        assert_eq!(eval!("[1 4]!"), Ok("[Num(0)]".to_string()));
    }

    #[test]
    fn exclaim_block() {
        assert_eq!(eval!("{}!"), Ok("[Num(1)]".to_string()));
        assert_eq!(eval!("{5}!"), Ok("[Num(0)]".to_string()));
    }

    // test@
    #[test]
    fn at() {
        assert_eq!(eval!("1 2 3 4@"), Ok("[Num(1), Num(3), Num(4), Num(2)]".to_string()));
    }

    // test#
    #[test]
    fn hash() {
        assert_eq!(eval!("1 # Here is a comment"), Ok("[Num(1)]".to_string()));
    }

    // test$
    #[test]
    fn dollar_num() {
        assert_eq!(eval!("1 2 3 4 5 1$"),
            Ok("[Num(1), Num(2), Num(3), Num(4), Num(5), Num(4)]".to_string()));
    }

    #[test]
    fn dollar_str() {
        assert_eq!(eval!("\"asdf\"$"), Ok("[Str(\"adfs\")]".to_string()));
    }

    // test+
    #[test]
    fn add_num() {
        assert_eq!(eval!("5 7+"), Ok("[Num(12)]".to_string()));
    }

    #[test]
    fn add_str() {
        assert_eq!(eval!("\"a\"\"b\"+"), Ok("[Str(\"ab\")]".to_string()));
    }

    #[test]
    fn add_array() {
        assert_eq!(eval!("[1][2]+"), Ok("[Array([Num(1), Num(2)])]".to_string()));
    }

    #[test]
    fn add_block() {
        assert_eq!(eval!("{1}{2-}+"), Ok("[Block([Num(1), Num(2), Op('-')])]".to_string()));
    }

    // test-
    #[test]
    fn sub_num() {
        assert_eq!(eval!("-1"), Ok("[Num(-1)]".to_string()));
        assert_eq!(eval!("1 2-3+"), Ok("[Num(1), Num(-1)]".to_string()));
        assert_eq!(eval!("1 2 -3+"), Ok("[Num(1), Num(-1)]".to_string()));
        assert_eq!(eval!("1 2- 3+"), Ok("[Num(2)]".to_string()));
    }

    fn sub_array() {
        assert_eq!(eval!("[5 2 5 4 1 1][1 2]-"),
                   Ok("[Num(5), Num(5), Num(4)]".to_string()));
    }

    // test*
    #[test]
    fn mul_num() {
        assert_eq!(eval!("2 4*"), Ok("[Num(8)]".to_string()));
    }

    #[test]
    fn mul_num_str() {
        assert_eq!(eval!("\"asdf\"3*"), Ok("[Str(\"asdfasdfasdf\")]".to_string()));
        assert_eq!(eval!("3\"asdf\"*"), Ok("[Str(\"asdfasdfasdf\")]".to_string()));
    }

    #[test]
    fn mul_num_array() {
        assert_eq!(eval!("[1 2]2*"),
                   Ok("[Array([Num(1), Num(2), Num(1), Num(2)])]".to_string()));

        assert_eq!(eval!("2[1 2]*"),
                   Ok("[Array([Num(1), Num(2), Num(1), Num(2)])]".to_string()));
    }

    fn mul_join() {
    }

    fn mul_fold() {
    }

    // test/
    #[test]
    fn div_num() {
        assert_eq!(eval!("7 3/"), Ok("[Num(2)]".to_string()));
    }

    // test%
    #[test]
    fn mod_num() {
        assert_eq!(eval!("7 3%"), Ok("[Num(1)]".to_string()));
    }

    // test|
    #[test]
    fn or_num() {
        assert_eq!(eval!("5 3|"), Ok("[Num(7)]".to_string()));
    }

    // test&
    #[test]
    fn and_num() {
        assert_eq!(eval!("5 3&"), Ok("[Num(1)]".to_string()));
    }

    // test^
    #[test]
    fn xor_num() {
        assert_eq!(eval!("5 3^"), Ok("[Num(6)]".to_string()));
    }

    // test[]
    #[test]
    fn slice() {
        assert_eq!(eval!("[1 2]"), Ok("[Array([Num(1), Num(2)])]".to_string()));
        assert_eq!(eval!("1 2 [\\]"), Ok("[Array([Num(2), Num(1)])]".to_string()));
    }

    // test\
    #[test]
    fn swap() {
        assert_eq!(eval!("1 2 3\\"), Ok("[Num(1), Num(3), Num(2)]".to_string()));
    }

    // test;
    #[test]
    fn pop_discard() {
        assert_eq!(eval!("1;"), Ok("[]".to_string()));
        assert_eq!(eval!("2 1;"), Ok("[Num(2)]".to_string()));
    }

    // test<
    #[test]
    fn lt_num() {
        assert_eq!(eval!("3 4<"), Ok("[Num(1)]".to_string()));
    }

    #[test]
    fn lt_str() {
        assert_eq!(eval!("\"asdf\"\"asdg\"<"), Ok("[Num(1)]".to_string()));
    }

    #[test]
    fn lt_num_array() {
        assert_eq!(eval!("[1 2 3]2<"), Ok("[Array([Num(1), Num(2)])]".to_string()));
    }

    // test>
    #[test]
    fn gt_num() {
        assert_eq!(eval!("3 4>"), Ok("[Num(0)]".to_string()));
    }

    #[test]
    fn gt_str() {
        assert_eq!(eval!("\"asdf\"\"asdg\">"), Ok("[Num(0)]".to_string()));
    }

    #[test]
    fn gt_num_array() {
        assert_eq!(eval!("[1 2 3]2>"), Ok("[Array([Num(3)])]".to_string()));
    }

    // test=
    #[test]
    fn eq_num() {
        assert_eq!(eval!("3 4="), Ok("[Num(0)]".to_string()));
    }

    #[test]
    fn eq_str() {
        assert_eq!(eval!("\"asdf\"\"asdg\">"), Ok("[Num(0)]".to_string()));
    }

    #[test]
    fn eq_num_array() {
        assert_eq!(eval!("[1 2 3]2="), Ok("[Num(3)]".to_string()));
        assert_eq!(eval!("[1 2 3]-1="), Ok("[Num(3)]".to_string()));
    }

    // test?
    #[test]
    fn qmark_num() {
        assert_eq!(eval!("2 8?"), Ok("[Num(256)]".to_string()));
    }

    #[test]
    fn qmark_num_array() {
        assert_eq!(eval!("5 [4 3 5 1]?"), Ok("[Num(2)]".to_string()));
    }

    // test(
    #[test]
    fn dec_num() {
        assert_eq!(eval!("5("), Ok("[Num(4)]".to_string()));
    }

    #[test]
    fn dec_array() {
        assert_eq!(eval!("[1 2 3]("), Ok("[Array([Num(2), Num(3)]), Num(1)]".to_string()));
    }

    // test)
    #[test]
    fn inc_num() {
        assert_eq!(eval!("5)"), Ok("[Num(6)]".to_string()));
    }

    #[test]
    fn inc_array() {
        assert_eq!(eval!("[1 2 3])"), Ok("[Array([Num(1), Num(2)]), Num(3)]".to_string()));
    }

    // test if
    fn builtin_if() {
        assert_eq!(eval!("1 2 3i"), Ok("[Num(2)]".to_string()));
    }

    // test abs
    fn builtin_abs() {
        assert_eq!(eval!("-2a"), Ok("[Num(2)]".to_string()));
    }
}
