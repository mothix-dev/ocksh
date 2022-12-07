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
    Subshell,
}

#[derive(Debug)]
pub struct Case<'a> {
    pub patterns: Vec<Vec<WordPart<'a>>>,
    pub body: Option<ASTLeaf<'a>>,
}

#[derive(Debug)]
pub enum ASTLeaf<'a> {
    Command {
        words: Vec<Token<'a>>,
    },
    Redirected {
        command: Box<ASTLeaf<'a>>,
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
        variable_name: &'a str,
        terms: Option<Vec<Token<'a>>>,
        body: Option<Box<ASTLeaf<'a>>>,
    },
    While {
        condition: Box<ASTLeaf<'a>>,
        body: Option<Box<ASTLeaf<'a>>>,
    },
    If {
        condition: Box<ASTLeaf<'a>>,
        body: Option<Box<ASTLeaf<'a>>>,
        body_else: Option<Box<ASTLeaf<'a>>>,
    },
    Case {
        word: Vec<WordPart<'a>>,
        cases: Vec<Case<'a>>,
    },
    Select {
        variable_name: &'a str,
        terms: Option<Vec<Token<'a>>>,
        body: Option<Box<ASTLeaf<'a>>>,
    },
    Function {
        name: &'a str,
        is_korn: bool,
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

fn peek_name<'a>(parser: &mut Parser<'a>) -> Option<&'a str> {
    match parser.peek() {
        Some(Ok(Token::Word { parts, .. })) => {
            if parts.len() == 1 && let Some(WordPart::String(first_part)) = parts.get(0) {
                Some(first_part)
            } else {
                None
            }
        }
        Some(Ok(Token::Reserved(r))) => Some(r),
        _ => None
    }
}

/// parses a command from the token stream
fn parse_command<'a>(parser: &mut Parser<'a>, block_stack: &mut Vec<BlockStackEntry>) -> Result<Option<ASTLeaf<'a>>, ParseError> {
    // make sure the first token is valid
    check_word(parser)?;

    let mut words = Vec::new();
    let mut redirections = Vec::new();

    loop {
        match parser.peek().cloned() {
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
            Some(Ok(Token::Reserved(r))) => {
                match r {
                    "&&" | "||" | ";" | ";;" | "|" | "&" | "|&" => break,
                    "()" => if words.len() == 1 && let Some(Token::Word { parts, .. }) = words.pop() && let [ WordPart::String(name) ] = parts[..] {
                        parser.next();

                        // we've found a function definition, parse it appropriately

                        match peek_name(parser) {
                            Some("{") => (),
                            _ => return Err(ParseError::ExpectedAfter("{".into(), "()".into())),
                        }
                        parser.next();

                        let body = parse_block(parser, block_stack, BlockStackEntry::Compound)?.map(Box::new);

                        return Ok(Some(ASTLeaf::Function { name, is_korn: false, body }))
                    } else {
                        return Err(ParseError::Unexpected("()".into()));
                    },
                    _ => {
                        parser.next();
                        redirections.push(parse_redirection(parser, r)?);
                    }
                }
            }
            Some(Ok(Token::Newline)) | None => break,
            Some(Err(err)) => return Err(err.clone()),
        }
    }

    if words.is_empty() {
        Ok(None)
    } else if !redirections.is_empty() {
        Ok(Some(ASTLeaf::Redirected {
            command: Box::new(ASTLeaf::Command { words }),
            redirections,
        }))
    } else {
        Ok(Some(ASTLeaf::Command { words }))
    }
}

fn parse_redirection<'a>(parser: &mut Parser<'a>, mut r: &'a str) -> Result<Redirection<'a>, ParseError> {
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
        ">" => Ok(Redirection::ToFileOverwrite {
            name: parse_word(parser)?,
            descriptor: file_descriptor_left.unwrap_or(STDOUT_NUM),
            force_truncate: false,
        }),
        ">|" => Ok(Redirection::ToFileOverwrite {
            name: parse_word(parser)?,
            descriptor: file_descriptor_left.unwrap_or(STDOUT_NUM),
            force_truncate: true,
        }),
        ">>" => Ok(Redirection::ToFileAppend {
            name: parse_word(parser)?,
            descriptor: file_descriptor_left.unwrap_or(STDOUT_NUM),
        }),
        "<" => Ok(Redirection::FromFile {
            name: parse_word(parser)?,
            descriptor: file_descriptor_left.unwrap_or(STDIN_NUM),
        }),
        "<>" => Ok(Redirection::ToFromFile {
            name: parse_word(parser)?,
            descriptor: file_descriptor_left.unwrap_or(STDIN_NUM),
        }),
        "<&" => Ok(Redirection::DupInput {
            descriptor: file_descriptor_left.unwrap_or(STDIN_NUM),
            from: file_descriptor_right.ok_or_else(|| ParseError::ExpectedAfter("number".into(), "<&".into()))?,
        }),
        "<&-" => Ok(Redirection::CloseInput {
            descriptor: file_descriptor_left.unwrap_or(STDIN_NUM),
        }),
        "<&p" => Ok(Redirection::FromCoprocess {
            descriptor: file_descriptor_left.unwrap_or(STDIN_NUM),
        }),
        ">&" => Ok(Redirection::DupOutput {
            descriptor: file_descriptor_left.unwrap_or(STDOUT_NUM),
            from: file_descriptor_right.ok_or_else(|| ParseError::ExpectedAfter("number".into(), ">&".into()))?,
        }),
        ">&-" => Ok(Redirection::CloseOutput {
            descriptor: file_descriptor_left.unwrap_or(STDOUT_NUM),
        }),
        ">&p" => Ok(Redirection::ToCoprocess {
            descriptor: file_descriptor_left.unwrap_or(STDOUT_NUM),
        }),
        _ => Err(ParseError::Unexpected(r.into())),
    }
}

#[derive(Debug)]
enum PartialParse<'a> {
    Parsed(ASTLeaf<'a>),
    DidntParse,
    EndParsing,
    EndWith(ASTLeaf<'a>),
}

/// parses a list of pipelined commands from the token stream, or just one command if the pipe symbol doesn't follow the word
fn parse_pipeline<'a>(parser: &mut Parser<'a>, block_stack: &mut Vec<BlockStackEntry>) -> Result<PartialParse<'a>, ParseError> {
    let mut commands = Vec::new();
    // if a pipeline starts with a word containing only "!", the exit status of its last command is inverted
    let inverted = if check_word(parser).is_ok() && let Some(Ok(Token::Word { parts, .. })) = parser.peek() && matches!(parts.get(0), Some(WordPart::String("!"))) && parts.len() == 1 {
        parser.next();
        true
    } else {
        false
    };
    let mut ending = false;

    loop {
        let leaf;

        // try parsing reserved commands, then normal commands
        let parse = parse_reserved(parser, block_stack)?;
        match parse {
            PartialParse::Parsed(l) => leaf = l,
            PartialParse::DidntParse => {
                if let Some(l) = parse_command(parser, block_stack)? {
                    leaf = l;
                } else {
                    break;
                }
            }
            PartialParse::EndParsing => {
                ending = true;
                break;
            }
            PartialParse::EndWith(l) => {
                leaf = l;
                ending = true;
            }
        };

        let mut found_pipe = false;
        let mut redirections = Vec::new();

        while let Some(Ok(Token::Reserved(r))) = parser.peek().cloned() {
            match r {
                "|" => {
                    parser.next();
                    found_pipe = true;
                    break;
                }
                "&&" | "||" | ";" | "&" | "|&" => break,
                ";;" => {
                    // duplicate of the ";;" case in parse_reserved, since apparently it's ok to put this on its own line and this was the easiest way to do so
                    parser.next();
    
                    match block_stack.pop() {
                        Some(BlockStackEntry::Case) => {
                            ending = true;
                            break;
                        },
                        _ => return Err(ParseError::Unexpected(";;".into())),
                    }
                }
                _ => {
                    // parse any additional redirections we find, as redirections aren't parsed in parse_reserved
                    parser.next();
                    redirections.push(parse_redirection(parser, r)?);
                }
            }
        }

        if !redirections.is_empty() {
            // if we've found any redirections, return a special leaf to reflect that
            commands.push(ASTLeaf::Redirected {
                command: Box::new(leaf),
                redirections,
            });
        } else {
            commands.push(leaf);
        }

        if !found_pipe || ending {
            // if a pipe character wasn't found, stop parsing the pipeline here
            break;
        }
    }

    // if only 1 command has been parsed and the pipeline isn't inverted, just return that one command
    if !inverted && commands.len() <= 1 {
        if commands.is_empty() {
            if ending {
                Ok(PartialParse::EndParsing)
            } else {
                Ok(PartialParse::DidntParse)
            }
        } else {
            if ending {
                Ok(PartialParse::EndWith(commands.pop().unwrap()))
            } else {
                Ok(PartialParse::Parsed(commands.pop().unwrap()))
            }
        }
    } else if ending {
        Ok(PartialParse::EndWith(ASTLeaf::Pipeline { commands, inverted }))
    } else {
        Ok(PartialParse::Parsed(ASTLeaf::Pipeline { commands, inverted }))
    }
}

#[derive(Debug, PartialEq, Eq)]
enum BlockStackEntry {
    Subshell,
    Compound,
    Done,
    Fi,
    Case,
}

fn parse_block<'a>(parser: &mut Parser<'a>, block_stack: &mut Vec<BlockStackEntry>, entry: BlockStackEntry) -> Result<Option<ASTLeaf<'a>>, ParseError> {
    // if there isn't a command after the starting token of this block, skip the next token
    if let Some(Ok(Token::Reserved(";"))) | Some(Ok(Token::Newline)) = parser.peek() {
        parser.next();
    }

    // get the body
    block_stack.push(entry);

    parse_full_list(parser, block_stack)
}

fn parse_if<'a>(parser: &mut Parser<'a>, block_stack: &mut Vec<BlockStackEntry>) -> Result<Option<ASTLeaf<'a>>, ParseError> {
    // get the condition
    let condition = if let PartialParse::Parsed(condition) | PartialParse::EndWith(condition) = parse_list(parser, block_stack)? {
        Box::new(condition)
    } else {
        return Err(ParseError::ExpectedAfter("list".into(), "if".into()));
    };

    // make sure "then" comes after
    match peek_name(parser) {
        Some("then") => (),
        _ => return Err(ParseError::ExpectedAfter("then".into(), "if".into())),
    }
    parser.next();

    // get the body
    let body = parse_block(parser, block_stack, BlockStackEntry::Fi)?.map(Box::new);

    let body_else = match peek_name(parser) {
        Some("else") => {
            parser.next();
            parse_block(parser, block_stack, BlockStackEntry::Fi)?.map(Box::new)
        }
        Some("elif") => {
            parser.next();
            parse_if(parser, block_stack)?.map(Box::new)
        }
        _ => None,
    };

    // assemble new leaf
    Ok(Some(ASTLeaf::If { condition, body, body_else }))
}

impl<'a> From<Option<ASTLeaf<'a>>> for PartialParse<'a> {
    fn from(t: Option<ASTLeaf<'a>>) -> Self {
        match t {
            Some(l) => Self::Parsed(l),
            None => Self::DidntParse,
        }
    }
}

fn parse_reserved<'a>(parser: &mut Parser<'a>, block_stack: &mut Vec<BlockStackEntry>) -> Result<PartialParse<'a>, ParseError> {
    // check for reserved words
    if let Some(first_part) = peek_name(parser).map(|s| s.to_string()) {
        match first_part.as_str() {
            "(" => {
                parser.next();

                if let Some(Ok(Token::Reserved(";"))) | Some(Ok(Token::Newline)) = parser.peek() {
                    parser.next();
                }

                block_stack.push(BlockStackEntry::Subshell);

                match parse_full_list(parser, block_stack)? {
                    Some(ASTLeaf::List {
                        kind: ListKind::Normal,
                        leaves,
                        background_status,
                    }) => Ok(PartialParse::Parsed(ASTLeaf::List {
                        kind: ListKind::Subshell,
                        leaves,
                        background_status,
                    })),
                    Some(leaf) => Ok(PartialParse::Parsed(ASTLeaf::List {
                        kind: ListKind::Subshell,
                        leaves: vec![leaf],
                        background_status: BackgroundStatus::Foreground,
                    })),
                    None => Ok(PartialParse::Parsed(ASTLeaf::List {
                        kind: ListKind::Subshell,
                        leaves: vec![],
                        background_status: BackgroundStatus::Foreground,
                    })),
                }
            }
            "{" => {
                parser.next();

                if let Some(Ok(Token::Reserved(";"))) | Some(Ok(Token::Newline)) = parser.peek() {
                    parser.next();
                }

                block_stack.push(BlockStackEntry::Compound);
                Ok(parse_full_list(parser, block_stack)?.into())
            }
            "case" => {
                parser.next();

                let word = parse_word(parser)?;

                // make sure "in" comes after
                match peek_name(parser) {
                    Some("in") => (),
                    _ => return Err(ParseError::ExpectedAfter("in".into(), "case".into())),
                }
                parser.next();

                let mut cases = Vec::new();

                loop {
                    // skip newlines
                    while let Some(Ok(Token::Newline)) = parser.peek() {
                        parser.next();
                    }

                    // break out of the loop if we've reached the end of the statement
                    if peek_name(parser) == Some("esac") {
                        parser.next();
                        break;
                    }

                    let mut patterns = Vec::new();

                    loop {
                        // get the pattern
                        match parser.next() {
                            Some(Ok(Token::Word { mut parts, .. })) => {
                                // make sure this is a valid pattern
                                match parts.last_mut() {
                                    Some(WordPart::String(_)) | Some(WordPart::QuotedString(_)) => patterns.push(parts),
                                    Some(_) => return Err(ParseError::InvalidCasePattern),
                                    None => unreachable!(),
                                }
                            }
                            Some(Ok(Token::Heredoc { .. })) => return Err(ParseError::Unexpected("heredoc".into())),
                            Some(Ok(Token::Reserved(r))) => return Err(ParseError::Unexpected(r.into())),
                            Some(Ok(Token::Newline)) => unreachable!(),
                            Some(Err(err)) => return Err(err),
                            None => return Err(ParseError::Unexpected("EOF".into())),
                        };

                        match parser.next() {
                            Some(Ok(Token::Reserved("|"))) => (),
                            Some(Ok(Token::Reserved(")"))) => break,

                            Some(Ok(Token::Word { .. })) => return Err(ParseError::Unexpected("word".into())),
                            Some(Ok(Token::Heredoc { .. })) => return Err(ParseError::Unexpected("heredoc".into())),
                            Some(Ok(Token::Reserved(r))) => return Err(ParseError::Unexpected(r.into())),
                            Some(Ok(Token::Newline)) => return Err(ParseError::Unexpected("newline".into())),
                            Some(Err(err)) => return Err(err),
                            None => return Err(ParseError::Unexpected("EOF".into())),
                        }
                    }

                    if patterns.is_empty() {
                        // no patterns?
                        return Err(ParseError::ExpectedBefore("pattern".into(), ")".into()));
                    }

                    // get the body
                    let body = parse_block(parser, block_stack, BlockStackEntry::Case)?;

                    cases.push(Case { patterns, body });
                }

                // assemble new leaf
                Ok(PartialParse::Parsed(ASTLeaf::Case { word, cases }))
            }
            "for" => {
                parser.next();

                // get the variable name
                let variable_name = match peek_name(parser) {
                    Some(name) => name,
                    None => return Err(ParseError::ExpectedAfter("variable name".into(), "for".into())),
                };
                parser.next();

                // get the terms (if there are any)
                let terms = match peek_name(parser) {
                    Some("in") => {
                        parser.next();

                        let mut terms = Vec::new();

                        loop {
                            let next = parser.next();
                            match next {
                                Some(Ok(Token::Reserved(";"))) | Some(Ok(Token::Newline)) => break,
                                Some(Ok(Token::Word { .. })) => terms.push(next.unwrap().unwrap()),

                                Some(Ok(Token::Heredoc { .. })) => return Err(ParseError::Unexpected("heredoc".into())),
                                Some(Ok(Token::Reserved(r))) => return Err(ParseError::Unexpected(r.into())),
                                Some(Err(err)) => return Err(err),
                                None => return Err(ParseError::Unexpected("EOF".into())),
                            }
                        }

                        Some(terms)
                    }
                    _ => {
                        // since we're not parsing terms, make sure there's a command terminator here
                        match parser.next() {
                            Some(Ok(Token::Reserved(";"))) | Some(Ok(Token::Newline)) => (),
                            Some(Ok(Token::Word { .. })) => return Err(ParseError::Unexpected("word".into())),
                            Some(Ok(Token::Heredoc { .. })) => return Err(ParseError::Unexpected("heredoc".into())),
                            Some(Ok(Token::Reserved(r))) => return Err(ParseError::Unexpected(r.into())),
                            Some(Err(err)) => return Err(err),
                            None => return Err(ParseError::Unexpected("EOF".into())),
                        }

                        None
                    }
                };

                // make sure "do" comes after
                match peek_name(parser) {
                    Some("do") => (),
                    _ => return Err(ParseError::ExpectedAfter("do".into(), "for".into())),
                }
                parser.next();

                // get the body
                let body = parse_block(parser, block_stack, BlockStackEntry::Done)?.map(Box::new);

                // assemble new leaf
                Ok(PartialParse::Parsed(ASTLeaf::For { variable_name, terms, body }))
            }
            "if" => {
                parser.next();
                Ok(parse_if(parser, block_stack)?.into())
            }
            "select" => {
                parser.next();

                // get the variable name
                let variable_name = match peek_name(parser) {
                    Some(name) => name,
                    None => return Err(ParseError::ExpectedAfter("variable name".into(), "select".into())),
                };
                parser.next();

                // get the terms (if there are any)
                let terms = match peek_name(parser) {
                    Some("in") => {
                        parser.next();

                        let mut terms = Vec::new();

                        loop {
                            let next = parser.next();
                            match next {
                                Some(Ok(Token::Reserved(";"))) | Some(Ok(Token::Newline)) => break,
                                Some(Ok(Token::Word { .. })) => terms.push(next.unwrap().unwrap()),

                                Some(Ok(Token::Heredoc { .. })) => return Err(ParseError::Unexpected("heredoc".into())),
                                Some(Ok(Token::Reserved(r))) => return Err(ParseError::Unexpected(r.into())),
                                Some(Err(err)) => return Err(err),
                                None => return Err(ParseError::Unexpected("EOF".into())),
                            }
                        }

                        Some(terms)
                    }
                    _ => {
                        // since we're not parsing terms, make sure there's a command terminator here
                        match parser.next() {
                            Some(Ok(Token::Reserved(";"))) | Some(Ok(Token::Newline)) => (),
                            Some(Ok(Token::Word { .. })) => return Err(ParseError::Unexpected("word".into())),
                            Some(Ok(Token::Heredoc { .. })) => return Err(ParseError::Unexpected("heredoc".into())),
                            Some(Ok(Token::Reserved(r))) => return Err(ParseError::Unexpected(r.into())),
                            Some(Err(err)) => return Err(err),
                            None => return Err(ParseError::Unexpected("EOF".into())),
                        }

                        None
                    }
                };

                // make sure "do" comes after
                match peek_name(parser) {
                    Some("do") => (),
                    _ => return Err(ParseError::ExpectedAfter("do".into(), "select".into())),
                }
                parser.next();

                // get the body
                let body = parse_block(parser, block_stack, BlockStackEntry::Done)?.map(Box::new);

                // assemble new leaf
                Ok(PartialParse::Parsed(ASTLeaf::Select { variable_name, terms, body }))
            }
            "while" | "until" => {
                parser.next();

                // get the condition
                let mut condition = match parse_pipeline(parser, block_stack)? {
                    PartialParse::Parsed(condition) | PartialParse::EndWith(condition) => condition,
                    _ => return Err(ParseError::ExpectedAfter("condition".into(), first_part)),
                };
                parser.next();

                // invert the condition if this is an "until" block
                if first_part == "until" {
                    match condition {
                        ASTLeaf::Pipeline { ref mut inverted, .. } => *inverted = !*inverted,
                        _ => {
                            condition = ASTLeaf::Pipeline {
                                commands: vec![condition],
                                inverted: true,
                            }
                        }
                    }
                }

                // make sure "do" comes after
                match peek_name(parser) {
                    Some("do") => (),
                    _ => return Err(ParseError::ExpectedAfter("do".into(), first_part)),
                }
                parser.next();

                // get the body
                let body = parse_block(parser, block_stack, BlockStackEntry::Done)?.map(Box::new);

                // assemble new leaf
                Ok(PartialParse::Parsed(ASTLeaf::While { condition: Box::new(condition), body }))
            }
            "((" => return Err(ParseError::Unexpected("((".into())),
            "function" => {
                parser.next();

                // get the function name
                let name = match peek_name(parser) {
                    Some(name) => name,
                    None => return Err(ParseError::ExpectedAfter("name".into(), "function".into())),
                };
                parser.next();

                match peek_name(parser) {
                    Some("{") => (),
                    _ => return Err(ParseError::ExpectedAfter("{".into(), "function".into())),
                }
                parser.next();

                // get the body
                let body = parse_block(parser, block_stack, BlockStackEntry::Compound)?.map(Box::new);

                // assemble new leaf
                Ok(PartialParse::Parsed(ASTLeaf::Function { name, is_korn: true, body }))
            }
            "()" => return Err(ParseError::Unexpected("()".into())),

            "do" | "in" | "then" => return Err(ParseError::Unexpected(first_part)),

            ")" => {
                parser.next();

                match block_stack.pop() {
                    Some(BlockStackEntry::Subshell) => return Ok(PartialParse::EndParsing),
                    _ => return Err(ParseError::Unexpected(")".into())),
                }
            }
            "}" => {
                parser.next();

                match block_stack.pop() {
                    Some(BlockStackEntry::Compound) => return Ok(PartialParse::EndParsing),
                    _ => return Err(ParseError::Unexpected("}".into())),
                }
            }
            "done" => {
                parser.next();

                match block_stack.pop() {
                    Some(BlockStackEntry::Done) => return Ok(PartialParse::EndParsing),
                    _ => return Err(ParseError::Unexpected("done".into())),
                }
            }
            "fi" => {
                parser.next();

                match block_stack.pop() {
                    Some(BlockStackEntry::Fi) => return Ok(PartialParse::EndParsing),
                    _ => return Err(ParseError::Unexpected("fi".into())),
                }
            }
            ";;" => {
                parser.next();

                match block_stack.pop() {
                    Some(BlockStackEntry::Case) => return Ok(PartialParse::EndParsing),
                    _ => return Err(ParseError::Unexpected(";;".into())),
                }
            }
            "esac" => match block_stack.pop() {
                Some(BlockStackEntry::Case) => return Ok(PartialParse::EndParsing),
                _ => return Err(ParseError::Unexpected("esac".into())),
            },
            "else" => match block_stack.pop() {
                Some(BlockStackEntry::Fi) => return Ok(PartialParse::EndParsing),
                _ => return Err(ParseError::Unexpected("else".into())),
            },
            "elif" => match block_stack.pop() {
                Some(BlockStackEntry::Fi) => return Ok(PartialParse::EndParsing),
                _ => return Err(ParseError::Unexpected("elif".into())),
            },
            "))" => return Err(ParseError::Unexpected("))".into())),

            "time" => return Err(ParseError::Unexpected("time".into())),

            _ => Ok(PartialParse::DidntParse),
        }
    } else {
        Ok(PartialParse::DidntParse)
    }
}

fn parse_list<'a>(parser: &mut Parser<'a>, block_stack: &mut Vec<BlockStackEntry>) -> Result<PartialParse<'a>, ParseError> {
    // skip any leading newlines
    while let Some(Ok(Token::Newline)) = parser.peek() {
        parser.next();
    }

    // parse the first leaf
    let mut leaf = parse_pipeline(parser, block_stack)?;

    while let PartialParse::DidntParse = leaf {
        match parser.peek() {
            Some(Ok(Token::Newline)) => {
                parser.next();
                leaf = parse_pipeline(parser, block_stack)?;
            }
            Some(Ok(_)) => return Err(ParseError::Unexpected("token".into())),
            Some(Err(err)) => return Err(err.clone()),
            None => return Ok(PartialParse::DidntParse),
        }
    }

    let mut kind = ListKind::Normal;
    let mut leaves = Vec::new();
    let mut background_status = BackgroundStatus::Foreground;

    match leaf {
        PartialParse::EndParsing | PartialParse::EndWith(_) => return Ok(leaf),
        PartialParse::Parsed(leaf) => leaves.push(leaf),
        PartialParse::DidntParse => unreachable!(),
    }

    let can_recurse = match parser.next() {
        Some(Ok(Token::Reserved("||"))) => {
            kind = ListKind::Or;
            true
        }
        Some(Ok(Token::Reserved("&&"))) => {
            kind = ListKind::And;
            true
        }
        Some(Ok(Token::Reserved("&"))) => {
            background_status = BackgroundStatus::Background;
            false
        }
        Some(Ok(Token::Reserved("|&"))) => {
            background_status = BackgroundStatus::Coprocess;
            false
        }
        Some(Ok(Token::Reserved(";"))) | Some(Ok(Token::Newline)) | None => false,

        Some(Ok(Token::Reserved(r))) => return Err(ParseError::Unexpected(r.into())),
        Some(Ok(Token::Word { .. })) => return Err(ParseError::Unexpected("word".into())),
        Some(Ok(Token::Heredoc { .. })) => return Err(ParseError::Unexpected("heredoc".into())),
        Some(Err(err)) => return Err(err),
    };

    let ending = if can_recurse {
        match parse_list(parser, block_stack)? {
            PartialParse::EndParsing => true,
            PartialParse::EndWith(leaf) => {
                leaves.push(leaf);
                true
            }
            PartialParse::Parsed(leaf) => {
                leaves.push(leaf);
                false
            }
            PartialParse::DidntParse => return Err(ParseError::Unexpected("EOF".into())),
        }
    } else {
        false
    };

    let new_leaf = if leaves.len() == 1 && kind == ListKind::Normal && background_status == BackgroundStatus::Foreground {
        leaves.pop().unwrap()
    } else {
        ASTLeaf::List { kind, leaves, background_status }
    };

    if ending {
        Ok(PartialParse::EndWith(new_leaf))
    } else {
        Ok(PartialParse::Parsed(new_leaf))
    }
}

fn parse_full_list<'a>(parser: &mut Parser<'a>, block_stack: &mut Vec<BlockStackEntry>) -> Result<Option<ASTLeaf<'a>>, ParseError> {
    let mut lists = Vec::new();

    loop {
        match parse_list(parser, block_stack)? {
            PartialParse::Parsed(list) => lists.push(list),
            PartialParse::DidntParse | PartialParse::EndParsing => break,
            PartialParse::EndWith(list) => {
                lists.push(list);
                break;
            }
        }
    }

    if lists.is_empty() {
        Ok(None)
    } else if lists.len() == 1 {
        Ok(lists.pop())
    } else {
        Ok(Some(ASTLeaf::List {
            kind: ListKind::Normal,
            leaves: lists,
            background_status: BackgroundStatus::Foreground,
        }))
    }
}

pub fn parse_ast(parser: super::parser::Parser) -> Result<Option<ASTLeaf>, ParseError> {
    let mut parser = parser.peekable();
    let mut block_stack = Vec::new();

    Ok(parse_full_list(&mut parser, &mut block_stack)?)
}
