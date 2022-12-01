#![feature(let_chains)]

use std::{io::Write, iter::Peekable, str::Chars};

#[derive(Debug)]
enum WordPart<'a> {
    String(&'a str),
    CommandSub(Vec<WordPart<'a>>),
    MathSub(Vec<WordPart<'a>>),
    Subshell(Vec<WordPart<'a>>),
    Variable {
        name: &'a str,
        quoted: bool,
    },
    ModifiedVariable {
        name: &'a str,
        modifier: Option<&'a str>,
        length_modifier: bool,
        quoted: bool,
    },
}

#[derive(Debug)]
enum Token<'a> {
    Word { parts: Vec<WordPart<'a>>, is_pattern: bool },
    Reserved(&'a str),
    Newline,
}

#[derive(Debug)]
enum ParseError {
    Unmatched(&'static str),
    UnexpectedCharacter(char),
    ExpectedBefore(&'static str, &'static str),
    ExpectedVariableName,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unmatched(c) => write!(f, "`{c}' unmatched"),
            Self::UnexpectedCharacter(c) => write!(f, "`{c}' unexpected"),
            Self::ExpectedBefore(c, d) => write!(f, "expected `{c}' before `{d}'"),
            Self::ExpectedVariableName => write!(f, "expected variable name"),
        }
    }
}

#[derive(PartialEq, Debug)]
enum ParenKind {
    Subshell,
    CommandSub,
    MathSub,
}

#[derive(Debug)]
struct ParenStackEntry<'a> {
    kind: ParenKind,
    old_word_parts: Vec<WordPart<'a>>,
    is_quoted: bool,
    in_backtick: bool,
}

struct Parser<'a> {
    string: &'a str,
    iter: Peekable<Chars<'a>>,
    index: usize,
    old_index: usize,
    next_token: Option<Token<'a>>,
    next_token_2: Option<Token<'a>>,
    word_parts: Vec<WordPart<'a>>,
    paren_stack: Vec<ParenStackEntry<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(string: &'a str) -> Self {
        Self {
            string,
            iter: string.chars().peekable(),
            index: 0,
            old_index: 0,
            next_token: None,
            next_token_2: None,
            word_parts: Vec::new(),
            paren_stack: Vec::new(),
        }
    }

    /// advance the iterator and increment our current index by one character
    fn advance(&mut self) {
        let increment = self.iter.peek().map(|c| c.len_utf8()).unwrap_or(1);
        self.iter.next();
        self.index += increment;
    }

    /// adds the current slice being processed to the word parts list
    fn append_slice(&mut self) {
        if self.index > self.old_index {
            self.word_parts.push(WordPart::String(&self.string[self.old_index..self.index]));
        }
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = Result<Token<'a>, ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(token) = core::mem::replace(&mut self.next_token, self.next_token_2.take()) {
            return Some(Ok(token));
        }

        // skip any whitespace and comments we may encounter
        let mut is_in_comment = false;
        loop {
            // if we run out of chars we can just return None here
            match self.iter.peek()? {
                ' ' | '\t' => (),
                '\n' => is_in_comment = false,
                '#' => is_in_comment = true,
                _ => {
                    if !is_in_comment {
                        break;
                    }
                }
            }

            self.advance();
        }

        self.old_index = self.index;

        #[derive(PartialEq)]
        enum QuoteState {
            None,
            Single,
            Double,
        }

        let mut quote_state = QuoteState::None;
        let mut in_backtick = false;
        let mut old_word_parts = Vec::new();
        let mut prev_num = false;

        // loop until we find the end of the word, then return a slice encompassing it
        loop {
            let c = self.iter.peek().cloned();

            match c {
                None | Some(' ') | Some('\t') | Some('\n') | Some('#') => {
                    // handle error cases
                    if c.is_none() {
                        match quote_state {
                            QuoteState::None => (),
                            QuoteState::Single => return Some(Err(ParseError::Unmatched("'"))),
                            QuoteState::Double => return Some(Err(ParseError::Unmatched("\""))),
                        }
                        if in_backtick {
                            return Some(Err(ParseError::Unmatched("`")));
                        }
                        match self.paren_stack.pop() {
                            None => (),
                            Some(ParenStackEntry { kind: ParenKind::CommandSub, .. }) | Some(ParenStackEntry { kind: ParenKind::Subshell, .. }) => return Some(Err(ParseError::Unmatched(")"))),
                            Some(ParenStackEntry { kind: ParenKind::MathSub, .. }) => return Some(Err(ParseError::Unmatched("))"))),
                        }
                    }

                    // only end the word if we're not in a quoted string
                    if quote_state == QuoteState::None && !in_backtick && self.paren_stack.is_empty() {
                        self.append_slice();
                        self.advance();

                        match c {
                            Some('\n') => self.next_token = Some(Token::Newline),
                            Some('#') => {
                                // skip to the end of the comment
                                loop {
                                    match self.iter.peek() {
                                        Some('\n') => {
                                            self.next_token = Some(Token::Newline);
                                            break;
                                        }
                                        None => break,
                                        _ => (),
                                    }

                                    self.advance();
                                }
                            }
                            _ => (),
                        }

                        if !self.word_parts.is_empty() {
                            return Some(Ok(Token::Word { parts: std::mem::take(&mut self.word_parts), is_pattern: false }));
                        } else {
                            // we don't have anything to return, so just recurse
                            return self.next();
                        }
                    }
                }
                Some('\\') => {
                    // backslash escapes are ignored in single quoted strings
                    if quote_state != QuoteState::Single {
                        // if there were other characters in the word before this one, take note of them
                        self.append_slice();

                        // skip to the character being escaped
                        self.advance();

                        self.word_parts.push(WordPart::String(&self.string[self.index..=self.index]));
                        self.old_index = self.index + 1;
                    }
                }
                Some('\'') => {
                    if quote_state != QuoteState::Double {
                        if let Some(top) = self.paren_stack.last() && top.is_quoted {
                            match top.kind {
                                ParenKind::Subshell | ParenKind::CommandSub => return Some(Err(ParseError::ExpectedBefore(")", "'"))),
                                ParenKind::MathSub => return Some(Err(ParseError::ExpectedBefore("))", "'"))),
                            }
                        }

                        self.append_slice();

                        self.old_index = self.index + 1;

                        match quote_state {
                            QuoteState::None => quote_state = QuoteState::Single,
                            QuoteState::Single => quote_state = QuoteState::None,
                            QuoteState::Double => unreachable!(),
                        }
                    }
                }
                Some('\"') => {
                    if quote_state != QuoteState::Single {
                        if let Some(top) = self.paren_stack.last() && top.is_quoted {
                            match top.kind {
                                ParenKind::Subshell | ParenKind::CommandSub => return Some(Err(ParseError::ExpectedBefore(")", "\""))),
                                ParenKind::MathSub => return Some(Err(ParseError::ExpectedBefore("))", "\""))),
                            }
                        }

                        self.append_slice();

                        self.old_index = self.index + 1;

                        match quote_state {
                            QuoteState::None => quote_state = QuoteState::Double,
                            QuoteState::Double => quote_state = QuoteState::None,
                            QuoteState::Single => unreachable!(),
                        }
                    }
                }
                Some('<') | Some('>') | Some('|') | Some(';') | Some('&') => {
                    if quote_state == QuoteState::None && !in_backtick && self.paren_stack.is_empty() {
                        match c {
                            Some('<') | Some('>') => {
                                if prev_num {
                                    // decrement index temporarily so that self.append_slice() doesn't take the number preceding this reserved character
                                    // this allows it to be properly included in the reserved token
                                    let old_index_2 = self.index;
                                    self.index -= 1;
                                    
                                    self.append_slice();

                                    self.old_index = self.index;
                                    self.index = old_index_2;
                                } else {
                                    self.append_slice();
                                    self.old_index = self.index;
                                }

                                // match some extra characters if we started on a < or >
                                while let Some('<') | Some('>') | Some('|') | Some(';') | Some('&') | Some('0'..='9') | Some('-') = self.iter.peek() {
                                    self.advance();
                                }
                            }
                            _ => {
                                self.append_slice();
                                self.old_index = self.index;

                                while let Some('|') | Some(';') | Some('&') = self.iter.peek() {
                                    self.advance();
                                }
                            }
                        }

                        // add the reserved characters we found to the next token, since we may have stuff before it that needs to be returned
                        self.next_token = Some(Token::Reserved(&self.string[self.old_index..self.index]));

                        // if a newline comes after this reserved token, handle it properly
                        if let Some('\n') = self.iter.peek() {
                            self.next_token_2 = Some(Token::Newline);
                        }

                        if !self.word_parts.is_empty() {
                            return Some(Ok(Token::Word { parts: std::mem::take(&mut self.word_parts), is_pattern: false }));
                        } else {
                            return self.next_token.take().map(Ok);
                        }
                    }
                }
                Some('`') => {
                    if let Some(top) = self.paren_stack.last() && top.in_backtick && in_backtick {
                        match top.kind {
                            ParenKind::Subshell | ParenKind::CommandSub => return Some(Err(ParseError::ExpectedBefore(")", "`"))),
                            ParenKind::MathSub => return Some(Err(ParseError::ExpectedBefore("))", "`"))),
                        }
                    }

                    self.append_slice();

                    self.old_index = self.index + 1;

                    match in_backtick {
                        true => {
                            in_backtick = false;
                            let parts = std::mem::replace(&mut self.word_parts, std::mem::take(&mut old_word_parts));
                            self.word_parts.push(WordPart::CommandSub(parts));
                        }
                        false => {
                            in_backtick = true;
                            old_word_parts = std::mem::take(&mut self.word_parts);
                        }
                    }
                }
                Some('$') => {
                    // variable substitution is ignored in single quoted strings
                    if quote_state != QuoteState::Single {
                        self.append_slice();
                        self.advance();

                        self.old_index = self.index;

                        match self.iter.peek() {
                            Some('{') => {
                                self.advance();

                                // check for the presence of the length modifier
                                let length_modifier = if let Some('#') = self.iter.peek() {
                                    self.advance();
                                    true
                                } else {
                                    false
                                };

                                self.old_index = self.index;

                                // find the name of the variable
                                match self.iter.peek() {
                                    Some('a'..='z') | Some('A'..='Z') | Some('0'..='9') | Some('_') => {
                                        // just looping over all valid characters should be good enough since the modifiers aren't valid characters
                                        while let Some('a'..='z') | Some('A'..='Z') | Some('0'..='9') | Some('_') = self.iter.peek() {
                                            self.advance();
                                        }
                                    }
                                    Some('!') | Some('#') | Some('$') | Some('-') | Some('?') | Some('*') | Some('@') => {
                                        // single character reserved variable names are always 1 character
                                        self.advance();
                                    }
                                    Some(_) => return Some(Err(ParseError::ExpectedVariableName)),
                                    None => return Some(Err(ParseError::Unmatched("}"))),
                                }
                                let name = &self.string[self.old_index..self.index];

                                // find the modifiers
                                self.old_index = self.index;
                                loop {
                                    match self.iter.peek() {
                                        Some('}') => break,
                                        None => return Some(Err(ParseError::Unmatched("}"))),
                                        _ => self.advance(),
                                    }
                                }
                                let modifier = if self.index > self.old_index { Some(&self.string[self.old_index..self.index]) } else { None };

                                self.old_index = self.index + 1;

                                self.word_parts.push(WordPart::ModifiedVariable {
                                    name,
                                    modifier,
                                    length_modifier,
                                    quoted: quote_state != QuoteState::None,
                                });
                            }
                            Some('a'..='z') | Some('A'..='Z') | Some('_') => {
                                // keep going until we hit a character that doesn't belong in a variable name
                                while let Some('a'..='z') | Some('A'..='Z') | Some('0'..='9') | Some('_') = self.iter.peek() {
                                    self.advance();
                                }
                                let name = &self.string[self.old_index..self.index];

                                self.word_parts.push(WordPart::Variable {
                                    name,
                                    quoted: quote_state != QuoteState::None,
                                });
                                self.old_index = self.index;

                                continue;
                            }
                            Some('!') | Some('#') | Some('$') | Some('-') | Some('?') | Some('*') | Some('@') | Some('0'..='9') => {
                                // all these reserved variable names are single characters, no need to search for the end of the name
                                let name = &self.string[self.index..=self.index];
                                self.old_index = self.index + 1;

                                self.word_parts.push(WordPart::Variable {
                                    name,
                                    quoted: quote_state != QuoteState::None,
                                });
                            }
                            Some('(') => {
                                self.advance();
                                match self.iter.peek() {
                                    Some('(') => {
                                        self.advance();
                                        self.paren_stack.push(ParenStackEntry {
                                            kind: ParenKind::MathSub,
                                            old_word_parts: std::mem::take(&mut self.word_parts),
                                            is_quoted: quote_state != QuoteState::None,
                                            in_backtick,
                                        });
                                    }
                                    Some(_) => self.paren_stack.push(ParenStackEntry {
                                        kind: ParenKind::CommandSub,
                                        old_word_parts: std::mem::take(&mut self.word_parts),
                                        is_quoted: quote_state != QuoteState::None,
                                        in_backtick,
                                    }),
                                    None => return Some(Err(ParseError::Unmatched(")"))),
                                }

                                self.old_index = self.index;

                                continue;
                            }
                            _ => {
                                self.old_index -= 1;
                                continue;
                            }
                        }
                    }
                }
                Some('(') => {
                    if quote_state == QuoteState::None {
                        self.append_slice();

                        let return_val = if !in_backtick && self.paren_stack.is_empty() {
                            if !self.word_parts.is_empty() {
                                Some(Ok(Token::Word { parts: std::mem::take(&mut self.word_parts), is_pattern: false }))
                            } else {
                                self.next_token.take().map(Ok)
                            }
                        } else {
                            None
                        };

                        self.advance();
                        match self.iter.peek() {
                            Some('(') => {
                                self.advance();
                                self.paren_stack.push(ParenStackEntry {
                                    kind: ParenKind::MathSub,
                                    old_word_parts: std::mem::take(&mut self.word_parts),
                                    is_quoted: quote_state != QuoteState::None,
                                    in_backtick,
                                });
                            }
                            Some(_) => self.paren_stack.push(ParenStackEntry {
                                kind: ParenKind::Subshell,
                                old_word_parts: std::mem::take(&mut self.word_parts),
                                is_quoted: quote_state != QuoteState::None,
                                in_backtick,
                            }),
                            None => return Some(Err(ParseError::Unmatched(")"))),
                        }

                        if return_val.is_some() {
                            return return_val;
                        } else {
                            self.old_index = self.index;
                            continue;
                        }
                    }
                }
                Some(')') => {
                    if quote_state == QuoteState::None {
                        self.append_slice();

                        if let Some(mut entry) = self.paren_stack.pop() && entry.in_backtick == in_backtick {
                            if entry.kind == ParenKind::MathSub {
                                self.advance();
                                match self.iter.peek() {
                                    Some(')') => (),
                                    Some(_) | None => return Some(Err(ParseError::Unmatched("))"))),
                                }
                            }

                            let parts = std::mem::replace(&mut self.word_parts, std::mem::take(&mut entry.old_word_parts));
                            match entry.kind {
                                ParenKind::CommandSub => self.word_parts.push(WordPart::CommandSub(parts)),
                                ParenKind::Subshell => self.word_parts.push(WordPart::Subshell(parts)),
                                ParenKind::MathSub => self.word_parts.push(WordPart::MathSub(parts)),
                            }
                        } else {
                            return Some(Err(ParseError::UnexpectedCharacter(')')));
                        }

                        self.old_index = self.index + 1;
                    }
                }
                _ => (),
            }

            prev_num = matches!(self.iter.peek(), Some('0'..='9'));

            self.advance();
        }
    }
}

fn prompt(p: &str, input: &mut String) -> std::io::Result<usize> {
    let stdin = std::io::stdin();
    let mut stdout = std::io::stdout();

    write!(stdout, "{p}")?;
    stdout.flush()?;

    stdin.read_line(input)
}

const PS1: &str = "@ ";
const PS2: &str = "> ";
const PS3: &str = "#? ";

fn main() {
    let mut args = std::env::args().peekable();

    // skip program name
    let program_name = args.next().unwrap();

    if args.peek().is_none() {
        // no script was passed, enter interactive mode
        loop {
            let mut input = String::new();
            if prompt(PS1, &mut input).expect("prompt failed") == 0 {
                break;
            }

            loop {
                input.pop();

                let exec = || -> Result<(), ParseError> {
                    for token in Parser::new(&input) {
                        println!("{:?}", token?);
                    }

                    Ok(())
                };

                if let Err(err) = exec() {
                    match err {
                        ParseError::Unmatched(_) => {
                            if prompt(PS2, &mut input).expect("prompt failed") == 0 {
                                eprintln!("{program_name}: syntax error: {}", err);
                                break;
                            }
                        }
                        _ => {
                            eprintln!("{program_name}: syntax error: {err}");
                            break;
                        }
                    }
                } else {
                    break;
                }
            }
        }

        println!();
    } else {
        let script_name = args.next().unwrap();
        let arguments: Vec<String> = args.collect();

        let script_contents = std::fs::read_to_string(script_name).expect("failed to read file");

        for token in Parser::new(&script_contents) {
            match token {
                Ok(t) => println!("{t:?}"),
                Err(err) => {
                    eprintln!("{program_name}: syntax error: {err}");
                    std::process::exit(1);
                }
            }
        }
    }
}
