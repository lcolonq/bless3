#![allow(dead_code, unused_variables)]
use std::collections::HashMap;

use wasm_bindgen::prelude::*;
use winnow::Parser;

// Parser
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct Word {
    name: char,
}

#[derive(Debug, Clone)]
enum Literal {
    Integer(i64),
    Float(f64),
    String(String),
    Array(Vec<Literal>),
    Quote(Program),
}

#[derive(Debug, Clone)]
enum Term {
    Word(Word),
    Literal(Literal),
}

#[derive(Debug, Clone)]
struct Program {
    terms: Vec<Term>
}

#[derive(Debug)]
struct Dictionary {
    words: HashMap<Word, Program>,
}

struct Meaningful;
impl winnow::stream::ContainsToken<char> for Meaningful {
    fn contains_token(&self, c: char) -> bool {
        ('𒀀'..='𒎙').contains(&c)
            || ('0'..='9').contains(&c)
            || c == '[' || c == ']'
            || c == '=' || c == ';'
    }
}

struct NotMeaningful;
impl winnow::stream::ContainsToken<char> for NotMeaningful {
    fn contains_token(&self, c: char) -> bool {
        !Meaningful.contains_token(c)
    }
}

fn consume_whitespace(inp: &mut &str) -> winnow::PResult<()> {
    winnow::token::take_while(
        0..,
        NotMeaningful,
    ).void().parse_next(inp)
}

fn parse_word(inp: &mut &str) -> winnow::PResult<Word> {
    winnow::token::one_of('𒀀'..='𒎙')
        .map(|name| Word { name })
        .parse_next(inp)
}

fn parse_literal(inp: &mut &str) -> winnow::PResult<Literal> {
    winnow::combinator::alt((
        winnow::ascii::dec_int
            .map(|i| Literal::Integer(i)),
        winnow::combinator::delimited(
            '"',
            winnow::token::take_till(0.., '"'),
            '"',
        ).map(|s: &str| Literal::String(s.to_owned())),
        winnow::combinator::delimited(
            '[',
            parse_program,
            ']',
        ).map(|p| Literal::Quote(p)),
    )).parse_next(inp)
}

fn parse_term(inp: &mut &str) -> winnow::PResult<Term> {
    consume_whitespace.parse_next(inp)?;
    let res = winnow::combinator::alt(( 
        parse_word.map(Term::Word),
        parse_literal.map(Term::Literal),
    )).parse_next(inp);
    consume_whitespace.parse_next(inp)?;
    res
}

fn parse_program(inp: &mut &str) -> winnow::PResult<Program> {
    winnow::combinator::repeat(0.., parse_term)
        .map(|terms| Program { terms })
        .parse_next(inp)
}

fn parse_dictionary_entry(inp: &mut &str) -> winnow::PResult<(Word, Program)> {
    consume_whitespace.parse_next(inp)?;
    let name = parse_word.parse_next(inp)?;
    consume_whitespace.parse_next(inp)?;
    '='.parse_next(inp)?;
    let prog = parse_program.parse_next(inp)?;
    ';'.parse_next(inp)?;
    winnow::PResult::Ok((name, prog))
}

fn parse_dictionary(inp: &mut &str) -> winnow::PResult<Dictionary>  {
    winnow::combinator::repeat(
        0..,
        parse_dictionary_entry,
    ).map(|ents: Vec<(Word, Program)>| Dictionary { words: HashMap::from_iter(ents) })
    .parse_next(inp)
}

// Runtime
#[derive(Debug, Clone)]
enum NativeRoutine {
    Add,
}

#[derive(Debug, Clone)]
enum Routine {
    Native(NativeRoutine),
    Program(Program),
}

#[derive(Debug)]
struct Environment {
    words: HashMap<Word, Routine>,
}
impl Environment {
    fn new() -> Self {
        Self {
            words: HashMap::from_iter(vec![
                (Word { name: '𒈦' }, Routine::Native(NativeRoutine::Add))
            ]),
        }
    }
    fn lookup(&self, w: &Word) -> Option<Routine> {
        self.words.get(w).cloned()
    }
}

#[derive(Debug, Clone)]
enum ValueSort {
    Integer,
    Float,
    String,
    Array,
    Routine,
}
impl ValueSort {
    fn pretty(&self) -> &'static str {
        match self {
            Self::Integer => "integer",
            Self::Float => "float",
            Self::String => "string",
            Self::Array => "array",
            Self::Routine => "routine",
        }
    }
}

#[derive(Debug, Clone)]
enum Value {
    Integer(i64),
    Float(f64),
    String(String),
    Array(Vec<Value>),
    Routine(Routine),
}
impl Value {
    fn new(l: &Literal) -> Self {
        match l {
            Literal::Integer(x) => Self::Integer(*x),
            Literal::Float(x) => Self::Float(*x),
            Literal::String(x) => Self::String(x.clone()),
            Literal::Array(x) => Self::Array(x.iter().map(Value::new).collect()),
            Literal::Quote(x) => Self::Routine(Routine::Program(x.clone())),
        }
    }
    fn sort(&self) -> ValueSort {
        match self {
            Self::Integer(_) => ValueSort::Integer,
            Self::Float(_) => ValueSort::Float,
            Self::String(_) => ValueSort::String,
            Self::Array(_) => ValueSort::Array,
            Self::Routine(_) => ValueSort::Routine,
        }
    }
}

#[derive(Debug)]
struct Interpreter {
    env: Environment,
    stack: Vec<Value>,
}
impl Interpreter {
    fn new() -> Self {
        Self {
            env: Environment::new(),
            stack: Vec::new(),
        }
    }
    fn push(&mut self, v: Value) {
        self.stack.push(v);
    }
    fn pop(&mut self) -> Result<Value, Error> {
        if let Some(x) = self.stack.pop() {
            Ok(x)
        } else {
            Err(Error::StackUnderflow)
        }
    }
    fn run_term(&mut self, t: &Term) -> Result<(), Error> {
        match t {
            Term::Literal(l) => {
                self.push(Value::new(l));
            },
            Term::Word(w) => {
                if let Some(p) = self.env.lookup(w) {
                    match p {
                        Routine::Native(n) => self.run_native_routine(n)?,
                        Routine::Program(p) => self.run(&p)?,
                    }
                } else {
                    return Err(Error::WordNotFound(w.clone()));
                }
            },
        }
        Ok(())
    }
    fn run_native_routine(&mut self, n: NativeRoutine) -> Result<(), Error> {
        match n {
            NativeRoutine::Add => {
                let x = self.pop()?;
                let y = self.pop()?;
                let res = match (&x, &y) {
                    (Value::Integer(ix), Value::Integer(iy)) => Value::Integer(ix + iy),
                    (Value::Float(ix), Value::Float(iy)) => Value::Float(ix + iy),
                    _ => return Err(Error::SortMismatch { expected: x.sort(), actual: y.sort() }),
                };
                self.push(res);
            },
        }
        Ok(())
    }
    fn run(&mut self, p: &Program) -> Result<(), Error> {
        for t in p.terms.iter() {
            self.run_term(&t)?;
        }
        Ok(())
    }
    fn load(&mut self, d: Dictionary) {
        for (w, p) in d.words.iter() {
            self.env.words.insert(w.clone(), Routine::Program(p.clone()));
        }
    }
}

#[derive(Debug, Clone)]
enum Error {
    WordNotFound(Word),
    StackUnderflow,
    SortMismatch { expected: ValueSort, actual: ValueSort },
}

#[wasm_bindgen]
pub async fn main_js() {
    console_log::init_with_level(log::Level::Debug).unwrap();
    console_error_panic_hook::set_once();
    tracing_wasm::set_as_global_default();
    log::info!("hello computer, starting up...");
    // let mut input = "123";
    // let res = parse_literal.parse_next(&mut input).expect("parse error");
    let mut input = "1 2 𒈦";
    let p = parse_program.parse_next(&mut input).expect("parse error");
    log::info!("parse success: {:?}", p);
    let mut interpreter = Interpreter::new();
    let res = interpreter.run(&p);
    log::info!("run success: {:?}", interpreter);
}
