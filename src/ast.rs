use super::parser::{ParseError, Token, WordPart};
use std::iter::Peekable;

const STDIN_NUM: u8 = 0;
const STDOUT_NUM: u8 = 1;

#[derive(Debug)]
pub enum Redirection<'a> {
    ToFileOverwrite { name: Vec<WordPart<'a>>, descriptor: u8, force_truncate: bool },
    ToFileAppend { name: Vec<WordPart<'a>>, descriptor: u8 },
    FromFile { name: Vec<WordPart<'a>>, descriptor: u8 },
    ToFromFile { name: Vec<WordPart<'a>>, descriptor: u8 },
    FromHeredoc { descriptor: u8, parts: Vec<WordPart<'a>> },
    CloseInput { descriptor: u8 },
    DupInput { descriptor: u8, from: u8 },
    FromCoprocess { descriptor: u8 },
    CloseOutput { descriptor: u8 },
    DupOutput { descriptor: u8, from: u8 },
    ToCoprocess { descriptor: u8 },
}

#[derive(Debug, PartialEq, Eq)]
pub enum BackgroundStatus {
    Foreground,
    Background,
    Coprocess,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ListKind {
    Normal,
    And,
    Or,
}

#[derive(Debug)]
pub enum ASTLeaf<'a> {
    Command {
        words: Vec<Token<'a>>,
        redirections: Vec<Redirection<'a>>,
    },
    Pipeline {
        commands: Vec<ASTLeaf<'a>>,
        inverted: bool,
    },
    List {
        kind: ListKind,
        leaves: Vec<ASTLeaf<'a>>,
        background_status: BackgroundStatus,
    },
    For {
        variable_name: String,
        terms: Option<Vec<Token<'a>>>,
        body: Option<Box<ASTLeaf<'a>>>,
    },
    While {
        condition: Box<ASTLeaf<'a>>,
        body: Option<Box<ASTLeaf<'a>>>,
    },
}

type Parser<'a> = Peekable<super::parser::Parser<'a>>;

/// checks whether the next token to be parsed is a word, and throws an error if it isn't
fn check_word(parser: &mut Parser) -> Result<(), ParseError> {
    match parser.peek() {
        Some(Ok(Token::Word { .. })) => Ok(()),
        Some(Ok(Token::Heredoc { .. })) => Err(ParseError::Unexpected("heredoc".into())),
        Some(Ok(Token::Reserved(r))) => Err(ParseError::Unexpected((*r).into())),
        Some(Ok(Token::Newline)) | None => Ok(()),
        Some(Err(err)) => Err(err.clone()),
    }
}

/// if the next token to be parsed is a word then its parts are returned, otherwise an error is thrown
fn parse_word<'a>(parser: &mut Parser<'a>) -> Result<Vec<WordPart<'a>>, ParseError> {
    match parser.peek() {
        Some(Ok(Token::Word { .. })) => {
            if let Some(Ok(Token::Word { parts, .. })) = parser.next() {
                Ok(parts)
            } else {
                unreachable!();
            }
        }
        Some(Ok(Token::Heredoc { .. })) => Err(ParseError::Unexpected("heredoc".into())),
        Some(Ok(Token::Reserved(r))) => Err(ParseError::Unexpected((*r).into())),
        Some(Ok(Token::Newline)) => Err(ParseError::Unexpected("newline".into())),
        None => Err(ParseError::Unexpected("EOF".into())),
        Some(Err(err)) => Err(err.clone()),
    }
}

fn parse_single_word_part<'a>(word: Option<&Token<'a>>) -> Option<&'a str> {
    match word {
        Some(Token::Word { parts, .. }) => {
            if parts.len() == 1 && let Some(WordPart::String(s)) = parts.get(0) {
                Some(*s)
            } else {
                None
            }
        }
        _ => None,
    }
}

fn force_parse_single_word_part<'a>(word: Option<&Token<'a>>, after: String) -> Result<&'a str, ParseError> {
    match word {
        Some(Token::Word { parts, .. }) => {
            if parts.len() == 1 && let Some(WordPart::String(s)) = parts.get(0) {
                Ok(*s)
            } else {
                Err(ParseError::ExpectedAfter("name".into(), after))
            }
        }
        Some(Token::Heredoc { .. }) => Err(ParseError::Unexpected("heredoc".into())),
        Some(Token::Reserved(r)) => Err(ParseError::Unexpected((*r).into())),
        Some(Token::Newline) => Err(ParseError::Unexpected("newline".into())),
        None => Err(ParseError::ExpectedWordAfter(after)),
    }
}

fn force_name<'a>(word: Option<&Token<'a>>, name: &str, after: String) -> Result<(), ParseError> {
    match word {
        Some(Token::Word { parts, .. }) => {
            if parts.len() == 1 && let Some(WordPart::String(s)) = parts.get(0) && *s == name {
                Ok(())
            } else {
                Err(ParseError::ExpectedAfter(name.into(), after))
            }
        }
        _ => Err(ParseError::ExpectedAfter(name.into(), after)),
    }
}

/// parses a command from the token stream
fn parse_command<'a>(parser: &mut Parser<'a>) -> Result<Option<ASTLeaf<'a>>, ParseError> {
    // make sure the first token is valid
    check_word(parser)?;

    let mut words = Vec::new();
    let mut redirections = Vec::new();

    loop {
        match parser.peek() {
            Some(Ok(Token::Word { .. })) => words.push(parser.next().unwrap().unwrap()),
            Some(Ok(Token::Heredoc { .. })) => {
                if let Some(Ok(Token::Heredoc { parts, file_descriptor })) = parser.next() {
                    redirections.push(Redirection::FromHeredoc {
                        descriptor: file_descriptor.unwrap_or(STDIN_NUM),
                        parts,
                    });
                } else {
                    unreachable!();
                }
            }
            Some(Ok(Token::Reserved(mut r))) => {
                // check for file descriptor numbers- only really helps for redirects but simplifies parsing a lot
                let file_descriptor_left = if let Some('0'..='9') = r.chars().next() {
                    let val = r[0..=0].parse::<u8>().unwrap();
                    r = &r[1..];
                    Some(val)
                } else {
                    None
                };

                let file_descriptor_right = if let Some('0'..='9') = r.chars().last() {
                    let val = r[r.len() - 1..].parse::<u8>().unwrap();
                    r = &r[..r.len() - 1];
                    Some(val)
                } else {
                    None
                };

                // TODO: speed this up
                match r {
                    ">" => {
                        parser.next();
                        redirections.push(Redirection::ToFileOverwrite {
                            name: parse_word(parser)?,
                            descriptor: file_descriptor_left.unwrap_or(STDOUT_NUM),
                            force_truncate: false,
                        });
                        continue;
                    }
                    ">|" => {
                        parser.next();
                        redirections.push(Redirection::ToFileOverwrite {
                            name: parse_word(parser)?,
                            descriptor: file_descriptor_left.unwrap_or(STDOUT_NUM),
                            force_truncate: true,
                        });
                        continue;
                    }
                    ">>" => {
                        parser.next();
                        redirections.push(Redirection::ToFileAppend {
                            name: parse_word(parser)?,
                            descriptor: file_descriptor_left.unwrap_or(STDOUT_NUM),
                        });
                        continue;
                    }
                    "<" => {
                        parser.next();
                        redirections.push(Redirection::FromFile {
                            name: parse_word(parser)?,
                            descriptor: file_descriptor_left.unwrap_or(STDIN_NUM),
                        });
                        continue;
                    }
                    "<>" => {
                        parser.next();
                        redirections.push(Redirection::ToFromFile {
                            name: parse_word(parser)?,
                            descriptor: file_descriptor_left.unwrap_or(STDIN_NUM),
                        });
                        continue;
                    }
                    "<&" => redirections.push(Redirection::DupInput {
                        descriptor: file_descriptor_left.unwrap_or(STDIN_NUM),
                        from: file_descriptor_right.ok_or_else(|| ParseError::ExpectedAfter("number".into(), "<&".into()))?,
                    }),
                    "<&-" => redirections.push(Redirection::CloseInput {
                        descriptor: file_descriptor_left.unwrap_or(STDIN_NUM),
                    }),
                    "<&p" => redirections.push(Redirection::FromCoprocess {
                        descriptor: file_descriptor_left.unwrap_or(STDIN_NUM),
                    }),
                    ">&" => redirections.push(Redirection::DupOutput {
                        descriptor: file_descriptor_left.unwrap_or(STDOUT_NUM),
                        from: file_descriptor_right.ok_or_else(|| ParseError::ExpectedAfter("number".into(), ">&".into()))?,
                    }),
                    ">&-" => redirections.push(Redirection::CloseOutput {
                        descriptor: file_descriptor_left.unwrap_or(STDOUT_NUM),
                    }),
                    ">&p" => redirections.push(Redirection::ToCoprocess {
                        descriptor: file_descriptor_left.unwrap_or(STDOUT_NUM),
                    }),
                    "&&" | "||" | ";" | "|" | "&" | "|&" => break,
                    _ => return Err(ParseError::Unexpected(r.into())),
                }

                parser.next();
            }
            Some(Ok(Token::Newline)) | None => break,
            Some(Err(err)) => return Err(err.clone()),
        }
    }

    if words.is_empty() && redirections.is_empty() {
        Ok(None)
    } else {
        Ok(Some(ASTLeaf::Command { words, redirections }))
    }
}

/// parses a list of pipelined commands from the token stream, or just one command if the pipe symbol doesn't follow the word
fn parse_pipeline<'a>(parser: &mut Parser<'a>) -> Result<Option<ASTLeaf<'a>>, ParseError> {
    let mut commands = Vec::new();
    // if a pipeline starts with a word containing only "!", the exit status of its last command is inverted
    let inverted = if check_word(parser).is_ok() && let Some(Ok(Token::Word { parts, .. })) = parser.peek() && matches!(parts.get(0), Some(WordPart::String("!"))) && parts.len() == 1 {
        parser.next();
        true
    } else {
        false
    };

    loop {
        // parse a command and push it onto our list
        if let Some(command) = parse_command(parser)? {
            commands.push(command);
        }

        if let Some(Ok(Token::Reserved("|"))) = parser.peek() {
            // if the next token is a pipe, continue parsing
            parser.next();
        } else {
            // otherwise stop parsing
            break;
        }
    }

    // if only 1 command has been parsed and the pipeline isn't inverted, just return that one command
    if !inverted && commands.len() <= 1 {
        if commands.is_empty() {
            Ok(None)
        } else {
            Ok(Some(commands.pop().unwrap()))
        }
    } else {
        Ok(Some(ASTLeaf::Pipeline { commands, inverted }))
    }
}

#[derive(Debug)]
struct IntermediateResult<'a> {
    leaf: Option<ASTLeaf<'a>>,
    next_token: Option<Token<'a>>,
}

/// streamlines parsing leaves and checking the following tokens
struct PipelineParser<'a> {
    parser: Peekable<super::parser::Parser<'a>>,
}

impl<'a> Iterator for PipelineParser<'a> {
    type Item = Result<IntermediateResult<'a>, ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        let leaf = match parse_pipeline(&mut self.parser) {
            Ok(pipeline) => pipeline,
            Err(err) => return Some(Err(err)),
        };
        let next_token = match self.parser.next() {
            Some(Ok(res)) => Some(res),
            Some(Err(err)) => return Some(Err(err)),
            None => None,
        };

        if leaf.is_some() || next_token.is_some() {
            Some(Ok(IntermediateResult { leaf, next_token }))
        } else {
            None
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum BlockStackEntry {
    CompoundStart,
    Compound,
    Do,
    Done,
}

fn parse_list<'a>(parser: &mut Peekable<PipelineParser<'a>>, can_append: bool, block_stack: &mut Vec<BlockStackEntry>) -> Result<Option<IntermediateResult<'a>>, ParseError> {
    let mut first = None;

    // check for reserved words/commands
    match parser.peek() {
        Some(Ok(IntermediateResult {
            leaf: Some(ASTLeaf::Command { ref words, .. }),
            ..
        })) => {
            let first_part = parse_single_word_part(words.get(0));

            match first_part {
                Some("{") => {
                    if block_stack.last() == Some(&BlockStackEntry::CompoundStart) {
                        block_stack.pop();

                        let mut next = parser.next().unwrap()?;

                        if let ASTLeaf::Command { ref mut words, .. } = next.leaf.as_mut().unwrap() && words.len() > 1 {
                            // remove the leading "{" from the command
                            words.remove(0);
                            first = Some(next);
                        }
                    } else {
                        block_stack.push(BlockStackEntry::Compound);
                        block_stack.push(BlockStackEntry::CompoundStart);

                        first = parse_full_list(parser, block_stack)?;
                    }
                }
                Some("case") => eprintln!("case"),
                Some("for") => {
                    // get the terms
                    let variable_name = force_parse_single_word_part(words.get(1), "for".into())?.to_string();
                    let terms = if words.len() > 2 {
                        force_name(words.get(2), "in", variable_name.to_string())?;
                        Some(words[2..].to_vec())
                    } else {
                        None
                    };

                    parser.next();

                    // get the body
                    block_stack.push(BlockStackEntry::Done);
                    block_stack.push(BlockStackEntry::Do);

                    let body = if let Some(body) = parse_full_list(parser, block_stack)? {
                        body
                    } else {
                        return Err(ParseError::ExpectedAfter("done".into(), "do".into()));
                    };

                    // assemble new leaf
                    let leaf = Some(ASTLeaf::For {
                        variable_name,
                        terms,
                        body: body.leaf.map(Box::new),
                    });

                    first = Some(IntermediateResult { leaf, next_token: body.next_token });
                }
                Some("if") => eprintln!("if"),
                Some("select") => eprintln!("select"),
                Some("while") | Some("until") => {
                    // get the condition
                    let mut condition = parser.next().unwrap()?.leaf.unwrap();

                    if let ASTLeaf::Command { ref mut words, .. } = condition && words.len() > 1 {
                        // remove the leading "while" from the command
                        words.remove(0);

                        if words.is_empty() {
                            return Err(ParseError::ExpectedWordAfter(first_part.unwrap().into()));
                        }
                    } else {
                        unreachable!();
                    }

                    if first_part == Some("until") {
                        condition = ASTLeaf::Pipeline {
                            commands: vec![condition],
                            inverted: true,
                        };
                    }

                    // get the body
                    block_stack.push(BlockStackEntry::Done);
                    block_stack.push(BlockStackEntry::Do);

                    let body = if let Some(body) = parse_full_list(parser, block_stack)? {
                        body
                    } else {
                        return Err(ParseError::ExpectedAfter("done".into(), "do".into()));
                    };

                    // assemble new leaf
                    let leaf = Some(ASTLeaf::While {
                        condition: Box::new(condition),
                        body: body.leaf.map(Box::new),
                    });

                    first = Some(IntermediateResult { leaf, next_token: body.next_token });
                }
                Some("function") => eprintln!("function"),

                Some("do") => {
                    let mut next = parser.next().unwrap()?;

                    if block_stack.pop() != Some(BlockStackEntry::Do) {
                        return Err(ParseError::Unexpected("do".into()));
                    } else if let ASTLeaf::Command { ref mut words, .. } = next.leaf.as_mut().unwrap() && words.len() > 1 {
                        // remove the leading "do" from the command
                        words.remove(0);
                        first = Some(next);
                    }
                }
                Some("then") => eprintln!("then"),
                Some("in") => eprintln!("in"),

                Some("}") => {
                    parser.next();

                    match block_stack.pop() {
                        Some(BlockStackEntry::Compound) | Some(BlockStackEntry::CompoundStart) => return Ok(None),
                        _ => return Err(ParseError::Unexpected("}".into())),
                    }
                }
                Some("done") => {
                    parser.next();

                    match block_stack.pop() {
                        Some(BlockStackEntry::Do) | Some(BlockStackEntry::Done) => return Ok(None),
                        _ => return Err(ParseError::Unexpected("done".into())),
                    }
                }
                Some("esac") => eprintln!("esac"),
                Some("fi") => eprintln!("fi"),

                Some("time") => eprintln!("time"),
                _ => (),
            }
        }
        Some(Ok(IntermediateResult {
            leaf: Some(ASTLeaf::Pipeline { ref commands, .. }),
            ..
        })) => {
            if let Some(ASTLeaf::Command { ref words, .. }) = commands.get(0) {
                let first_part = parse_single_word_part(words.get(0));

                match first_part {
                    Some("while") | Some("until") => {
                        let mut condition = parser.next().unwrap()?.leaf.unwrap();

                        // some annoying bullshit because Iterator::peekable() has no way of accessing the original iterator and i don't care enough to write my own
                        if let ASTLeaf::Pipeline { ref mut commands, ref mut inverted } = condition {
                            if first_part == Some("until") {
                                *inverted = !*inverted;
                            }

                            if let Some(ASTLeaf::Command { ref mut words, .. }) = commands.get_mut(0) {
                                words.remove(0);

                                if words.is_empty() {
                                    return Err(ParseError::ExpectedWordAfter(first_part.unwrap().into()));
                                }
                            } else {
                                unreachable!();
                            }
                        } else {
                            unreachable!();
                        }

                        // get the body
                        block_stack.push(BlockStackEntry::Done);
                        block_stack.push(BlockStackEntry::Do);

                        let body = parse_full_list(parser, block_stack)?.ok_or_else(|| ParseError::ExpectedAfter("done".into(), "do".into()))?;

                        // assemble new leaf
                        let leaf = Some(ASTLeaf::While {
                            condition: Box::new(condition),
                            body: body.leaf.map(Box::new),
                        });

                        first = Some(IntermediateResult { leaf, next_token: body.next_token });
                    }

                    _ => (),
                }
            }
        }
        _ => (),
    }

    let mut first = match first {
        Some(first) => first,
        None => match parser.next() {
            Some(pipeline) => pipeline?,
            None => return Ok(None),
        },
    };

    #[derive(Debug)]
    enum NextMode {
        None,
        Recurse,
        Append,
    }

    let mut kind = ListKind::Normal;
    let mut leaves = Vec::new();
    let mut background_status = BackgroundStatus::Foreground;

    while first.leaf.is_none() {
        match first.next_token {
            Some(Token::Newline) => {
                first = match parser.next() {
                    Some(leaf) => leaf?,
                    None => return Ok(None),
                }
            }
            Some(_) => return Err(ParseError::Unexpected("word".into())),
            None => return Ok(None),
        }
    }

    let mut next_token = first.next_token;

    let next_mode = match next_token {
        Some(Token::Reserved("||")) => {
            kind = ListKind::Or;
            NextMode::Recurse
        }
        Some(Token::Reserved("&&")) => {
            kind = ListKind::And;
            NextMode::Recurse
        }
        Some(Token::Reserved("&")) => {
            background_status = BackgroundStatus::Background;
            NextMode::None
        }
        Some(Token::Reserved("|&")) => {
            background_status = BackgroundStatus::Coprocess;
            NextMode::None
        }
        Some(Token::Reserved(";")) | Some(Token::Newline) => NextMode::Append,
        None => NextMode::None,

        Some(Token::Reserved(r)) => return Err(ParseError::Unexpected(r.into())),
        Some(Token::Word { .. }) => return Err(ParseError::Unexpected("word".into())),
        Some(Token::Heredoc { .. }) => return Err(ParseError::Unexpected("heredoc".into())),
    };

    leaves.push(first.leaf.unwrap());

    match next_mode {
        NextMode::None => (),
        NextMode::Recurse => {
            let next = match parse_list(parser, false, block_stack)? {
                Some(res) => res,
                None => return Err(ParseError::Unexpected("EOF".into())),
            };

            leaves.push(next.leaf.unwrap());

            next_token = next.next_token;
        }
        NextMode::Append => {
            if can_append {
                loop {
                    let next = match parse_list(parser, false, block_stack)? {
                        Some(res) => res,
                        None => break,
                    };

                    leaves.push(next.leaf.unwrap());

                    next_token = next.next_token;
                }
            }
        }
    }

    if leaves.len() == 1 && kind == ListKind::Normal && background_status == BackgroundStatus::Foreground {
        Ok(Some(IntermediateResult { leaf: leaves.pop(), next_token }))
    } else {
        Ok(Some(IntermediateResult {
            leaf: Some(ASTLeaf::List { kind, leaves, background_status }),
            next_token,
        }))
    }
}

fn parse_full_list<'a>(parser: &mut Peekable<PipelineParser<'a>>, block_stack: &mut Vec<BlockStackEntry>) -> Result<Option<IntermediateResult<'a>>, ParseError> {
    let mut lists = Vec::new();
    let mut next_token = None;

    // if the first command parse_list() encounters doesn't end in a newline or ;, parse_list() will ignore everything following it
    // the solution is just to keep running parse_list until we run out of things to parse
    while let Some(list) = parse_list(parser, true, block_stack)? {
        let can_continue = matches!(list.leaf.as_ref().unwrap(), ASTLeaf::List {
            kind: ListKind::And | ListKind::Or,
            ..
        });
        next_token = list.next_token;

        match next_token {
            Some(Token::Newline) | Some(Token::Reserved(";")) | None => match list.leaf.unwrap() {
                ASTLeaf::List {
                    kind: ListKind::Normal,
                    mut leaves,
                    background_status: BackgroundStatus::Foreground,
                } => lists.append(&mut leaves),
                l => lists.push(l),
            },

            Some(Token::Reserved(r)) => return Err(ParseError::Unexpected(r.into())),
            Some(Token::Word { .. }) => return Err(ParseError::Unexpected("word".into())),
            Some(Token::Heredoc { .. }) => return Err(ParseError::Unexpected("heredoc".into())),
        }

        if !can_continue {
            break;
        }
    }

    if lists.is_empty() {
        Ok(None)
    } else if lists.len() == 1 {
        Ok(Some(IntermediateResult { leaf: lists.pop(), next_token }))
    } else {
        Ok(Some(IntermediateResult {
            leaf: Some(ASTLeaf::List {
                kind: ListKind::Normal,
                leaves: lists,
                background_status: BackgroundStatus::Foreground,
            }),
            next_token,
        }))
    }
}

pub fn parse_ast(parser: super::parser::Parser) -> Result<Option<ASTLeaf>, ParseError> {
    let mut parser = (PipelineParser { parser: parser.peekable() }).peekable();
    let mut block_stack = Vec::new();

    Ok(parse_full_list(&mut parser, &mut block_stack)?.and_then(|i| i.leaf))
}
