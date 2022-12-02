use std::{collections::VecDeque, iter::Peekable, str::Chars};

#[derive(Debug, Clone)]
pub enum WordPart<'a> {
    String(&'a str),
    QuotedString(&'a str),
    CommandSub(Vec<WordPart<'a>>),
    MathSub(Vec<WordPart<'a>>),
    Math(Vec<WordPart<'a>>),
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
pub enum Token<'a> {
    Word { parts: Vec<WordPart<'a>>, is_pattern: bool, is_assignment: bool },
    Heredoc { parts: Vec<WordPart<'a>>, file_descriptor: Option<u8> },
    Reserved(&'a str),
    Newline,
}

#[derive(Debug)]
pub enum ParseError {
    Unmatched(&'static str),
    Unexpected(&'static str),
    ExpectedBefore(&'static str, &'static str),
    ExpectedAfter(&'static str, char),
    ExpectedWordAfter(&'static str),
    ExpectedVariableName,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unmatched(i) => write!(f, "`{i}' unmatched"),
            Self::Unexpected(i) => write!(f, "`{i}' unexpected"),
            Self::ExpectedBefore(i, j) => write!(f, "expected `{i}' before `{j}'"),
            Self::ExpectedAfter(i, j) => write!(f, "expected `{i}' after `{j}'"),
            Self::ExpectedWordAfter(i) => write!(f, "expected word after `{i}'"),
            Self::ExpectedVariableName => write!(f, "expected variable name"),
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
enum ParenKind {
    Subshell,
    CommandSub,
    MathSub,
    Math,
    InnerMath,
    Pattern,
}

#[derive(Debug)]
struct ParenStackEntry<'a> {
    kind: ParenKind,
    old_word_parts: Option<Vec<WordPart<'a>>>,
    is_quoted: bool,
    in_backtick: bool,
}

#[derive(PartialEq, Eq, Debug)]
enum HeredocState {
    None,
    Waiting,
    InsideDouble,
    InsideSingle,
}

/// used to parse an input string into a series of words separated by spaces or reserved characters
///
/// words can contain strings (both quoted and non-quoted) and command, math, and variable substitutions
///
/// the parser is implemented as an iterator, so every next() call will return either the next token, an error that occurred when parsing the next token, or None to signify the end of the file
pub struct Parser<'a> {
    string: &'a str,
    iter: Peekable<Chars<'a>>,
    index: usize,
    old_index: usize,
    next_token: Option<Token<'a>>,
    next_token_2: Option<Token<'a>>,
    word_parts: Vec<WordPart<'a>>,
    paren_stack: Vec<ParenStackEntry<'a>>,
    heredoc_state: HeredocState,
    queued_tokens: VecDeque<Token<'a>>,
    strip_leading_tabs: bool,
    heredoc_terminator: Option<&'a str>,
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
            heredoc_state: HeredocState::None,
            queued_tokens: VecDeque::new(),
            strip_leading_tabs: false,
            heredoc_terminator: None,
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

    fn append_slice_quoted(&mut self) {
        if self.index > self.old_index {
            self.word_parts.push(WordPart::QuotedString(&self.string[self.old_index..self.index]));
        }
    }

    fn process_heredoc(&mut self, r: &'a str, terminator: WordPart<'a>) -> Result<Token<'a>, ParseError> {
        self.heredoc_state = HeredocState::Waiting;

        self.strip_leading_tabs = r.chars().nth(2) == Some('-') || r.chars().nth(3) == Some('-');
        let file_descriptor = if matches!(r.chars().next(), Some('0'..='9')) {
            // we can just parse the file descriptor number here, it's convenient and saves a step later
            Some(r[0..=0].parse().expect("failed to parse file descriptor for heredoc"))
        } else {
            None
        };

        self.heredoc_terminator = match terminator {
            WordPart::String(t) | WordPart::QuotedString(t) => Some(t),
            _ => unreachable!(),
        };

        // grab and save tokens until we hit a newline
        loop {
            match self.next() {
                Some(Ok(Token::Newline)) => break,
                Some(Ok(token)) => self.queued_tokens.push_back(token),
                Some(Err(e)) => return Err(e),
                None => {
                    if self.strip_leading_tabs {
                        return Err(ParseError::Unmatched("<<-"));
                    } else {
                        return Err(ParseError::Unmatched("<<"));
                    }
                }
            }
        }

        // we're in the heredoc now, set state to reflect that
        self.heredoc_state = match terminator {
            WordPart::String(_) => HeredocState::InsideDouble,
            WordPart::QuotedString(_) => HeredocState::InsideSingle,
            _ => unreachable!(),
        };

        // grab the contents of the heredoc
        let parts = match self.next() {
            Some(Ok(Token::Word { parts, .. })) => parts,
            Some(Ok(_)) => panic!("Parser::next() on heredoc should only return Token::word()"),
            Some(Err(e)) => return Err(e),
            None => {
                if self.strip_leading_tabs {
                    return Err(ParseError::Unmatched("<<-"));
                } else {
                    return Err(ParseError::Unmatched("<<"));
                }
            }
        };

        // outside of the heredoc now
        self.heredoc_state = HeredocState::None;

        Ok(Token::Heredoc { parts, file_descriptor })
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = Result<Token<'a>, ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.heredoc_state == HeredocState::None && !self.queued_tokens.is_empty() {
            match self.queued_tokens.pop_front() {
                Some(Token::Heredoc { parts, .. }) => {
                    // skip a newline that's left over from the previous heredoc
                    self.advance();

                    return Some(self.process_heredoc(
                        match parts[0] {
                            WordPart::String(r) => r,
                            _ => unreachable!(),
                        },
                        parts[1].clone(),
                    ));
                }
                token => return token.map(Ok),
            }
        }

        // if we've got another token queued up immediately after, return it
        if let Some(token) = core::mem::replace(&mut self.next_token, self.next_token_2.take()) {
            match token {
                Token::Reserved(r) => {
                    if (r.len() >= 2 && &r[0..2] == "<<") || (r.len() >= 3 && &r[1..3] == "<<") {
                        // ...except if it's a heredoc

                        // advance past any whitespace
                        loop {
                            match self.iter.peek()? {
                                ' ' | '\t' => self.advance(),
                                '\n' => return Some(Err(ParseError::Unexpected("newline"))),
                                '#' => return Some(Err(ParseError::Unexpected("#"))),
                                _ => break,
                            }
                        }

                        let old_index = self.index;
                        // loop until we hit whitespace or a newline
                        loop {
                            match self.iter.peek() {
                                Some(' ') | Some('\t') | Some('\n') => break,
                                None => {
                                    if self.strip_leading_tabs {
                                        return Some(Err(ParseError::ExpectedWordAfter("<<-")));
                                    } else {
                                        return Some(Err(ParseError::ExpectedWordAfter("<<")));
                                    }
                                }
                                _ => self.advance(),
                            }
                        }

                        // throw an error if there's no terminator
                        if self.index == old_index {
                            if self.strip_leading_tabs {
                                return Some(Err(ParseError::ExpectedWordAfter("<<-")));
                            } else {
                                return Some(Err(ParseError::ExpectedWordAfter("<<")));
                            }
                        }

                        let mut is_quoted = false;
                        let mut terminator = &self.string[old_index..self.index];

                        // check if the terminator is quoted
                        let mut chars = terminator.chars();
                        let first_char = chars.next();
                        match first_char {
                            Some('\\') => {
                                is_quoted = true;
                                terminator = &terminator[1..];
                            }
                            Some('\'') | Some('"') => {
                                if chars.last() == first_char && terminator.len() > 2 {
                                    is_quoted = true;
                                    terminator = &terminator[1..terminator.len() - 1];
                                }
                            }
                            _ => (),
                        }

                        let terminator = if is_quoted { WordPart::QuotedString(terminator) } else { WordPart::String(terminator) };

                        if self.heredoc_state == HeredocState::None {
                            return Some(self.process_heredoc(r, terminator));
                        } else {
                            // this is a hack, need to get multiple heredocs in a single command line working somehow
                            return Some(Ok(Token::Heredoc {
                                parts: vec![WordPart::String(r), terminator],
                                file_descriptor: None,
                            }));
                        }
                    } else {
                        return Some(Ok(token));
                    }
                }
                _ => return Some(Ok(token)),
            }
        }

        if self.heredoc_state == HeredocState::None {
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
        }

        if (self.heredoc_state == HeredocState::InsideDouble || self.heredoc_state == HeredocState::InsideSingle) && self.strip_leading_tabs {
            while let Some('\t') = self.iter.peek() {
                self.advance();
            }
        }

        self.old_index = self.index;

        #[derive(PartialEq, Eq, Debug)]
        enum QuoteState {
            None,
            Single,
            Double,
        }

        let mut quote_state = match self.heredoc_state {
            HeredocState::None | HeredocState::Waiting => QuoteState::None,
            HeredocState::InsideDouble => QuoteState::Double,
            HeredocState::InsideSingle => QuoteState::Single,
        };
        let mut in_backtick = false;
        let mut old_word_parts = Vec::new();
        let mut prev_num = false;
        let mut is_pattern = false;
        let mut is_assignment = false;

        // loop until we find the end of the word, then return a slice encompassing it
        loop {
            let c = self.iter.peek().cloned();

            match c {
                None | Some(' ') | Some('\t') | Some('\n') | Some('#') => {
                    // handle error cases
                    if c.is_none() {
                        if self.heredoc_state != HeredocState::None {
                            if self.strip_leading_tabs {
                                return Some(Err(ParseError::Unmatched("<<-")));
                            } else {
                                return Some(Err(ParseError::Unmatched("<<")));
                            }
                        }
                        match quote_state {
                            QuoteState::None => (),
                            QuoteState::Single => return Some(Err(ParseError::Unmatched("'"))),
                            QuoteState::Double => return Some(Err(ParseError::Unmatched("\""))),
                        }
                        if in_backtick {
                            return Some(Err(ParseError::Unmatched("`")));
                        }
                        match self.paren_stack.pop().map(|e| e.kind) {
                            None => (),
                            Some(ParenKind::CommandSub) | Some(ParenKind::Subshell) | Some(ParenKind::Pattern) | Some(ParenKind::InnerMath) => return Some(Err(ParseError::Unmatched(")"))),
                            Some(ParenKind::Math) | Some(ParenKind::MathSub) => return Some(Err(ParseError::Unmatched("))"))),
                        }
                    }

                    // only end the word if we're not in a quoted string
                    if quote_state == QuoteState::None && !in_backtick && self.paren_stack.is_empty() && (self.heredoc_state == HeredocState::None || self.heredoc_state == HeredocState::Waiting) {
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
                            return Some(Ok(Token::Word {
                                parts: std::mem::take(&mut self.word_parts),
                                is_pattern,
                                is_assignment,
                            }));
                        } else {
                            // we don't have anything to return, so just recurse
                            return self.next();
                        }
                    }

                    // if we're in a heredoc and reached a newline, check if the first characters after it match the terminator
                    if c == Some('\n') && (self.heredoc_state == HeredocState::InsideDouble || self.heredoc_state == HeredocState::InsideSingle) {
                        self.advance();

                        // strip leading tabs if we're supposed to and if there are any
                        if self.strip_leading_tabs && self.iter.peek() == Some(&'\t') {
                            self.append_slice_quoted();

                            while self.iter.peek() == Some(&'\t') {
                                self.advance();
                            }

                            self.old_index = self.index;
                        }

                        // check if we've reached the terminator
                        let old_index = self.index; // TODO: should the trailing newline be preserved?
                        let mut we_have_it = true;
                        for c in self.heredoc_terminator.unwrap().chars() {
                            match self.iter.peek() {
                                Some(c2) => {
                                    if c == *c2 {
                                        self.advance();
                                    } else {
                                        we_have_it = false;
                                        break;
                                    }
                                }
                                None => {
                                    if self.strip_leading_tabs {
                                        return Some(Err(ParseError::Unmatched("<<-")));
                                    } else {
                                        return Some(Err(ParseError::Unmatched("<<")));
                                    }
                                }
                            }
                        }

                        // if we've reached the terminator, return the contents of the heredoc
                        if we_have_it {
                            let new_index = std::mem::replace(&mut self.index, old_index);
                            self.append_slice_quoted();
                            self.index = new_index;

                            if self.iter.peek() == Some(&'\n') {
                                self.next_token = Some(Token::Newline);
                            }

                            return Some(Ok(Token::Word {
                                parts: std::mem::take(&mut self.word_parts),
                                is_pattern,
                                is_assignment,
                            }));
                        } else {
                            continue;
                        }
                    }
                }
                Some('\\') => {
                    // backslash escapes are ignored in single quoted strings
                    if quote_state != QuoteState::Single && self.heredoc_state != HeredocState::InsideSingle {
                        // if there were other characters in the word before this one, take note of them
                        match quote_state {
                            QuoteState::None => self.append_slice(),
                            QuoteState::Double => self.append_slice_quoted(),
                            QuoteState::Single => unreachable!(),
                        }

                        // skip to the character being escaped
                        self.advance();

                        self.word_parts.push(WordPart::QuotedString(&self.string[self.index..=self.index]));
                        self.old_index = self.index + 1;
                    }
                }
                Some('\'') => {
                    if quote_state != QuoteState::Double && (self.heredoc_state == HeredocState::None || self.heredoc_state == HeredocState::Waiting) {
                        if let Some(top) = self.paren_stack.last() && top.is_quoted {
                            match top.kind {
                                ParenKind::CommandSub | ParenKind::Subshell | ParenKind::Pattern | ParenKind::InnerMath => return Some(Err(ParseError::Unmatched(")"))),
                                ParenKind::Math | ParenKind::MathSub => return Some(Err(ParseError::Unmatched("))"))),
                            }
                        }

                        match quote_state {
                            QuoteState::None => self.append_slice(),
                            QuoteState::Single => self.append_slice_quoted(),
                            QuoteState::Double => unreachable!(),
                        }

                        self.old_index = self.index + 1;

                        match quote_state {
                            QuoteState::None => quote_state = QuoteState::Single,
                            QuoteState::Single => quote_state = QuoteState::None,
                            QuoteState::Double => unreachable!(),
                        }
                    }
                }
                Some('\"') => {
                    if quote_state != QuoteState::Single && (self.heredoc_state == HeredocState::None || self.heredoc_state == HeredocState::Waiting) {
                        if let Some(top) = self.paren_stack.last() && top.is_quoted {
                            match top.kind {
                                ParenKind::CommandSub | ParenKind::Subshell | ParenKind::Pattern | ParenKind::InnerMath => return Some(Err(ParseError::Unmatched(")"))),
                                ParenKind::Math | ParenKind::MathSub => return Some(Err(ParseError::Unmatched("))"))),
                            }
                        }

                        match quote_state {
                            QuoteState::None => self.append_slice(),
                            QuoteState::Double => self.append_slice_quoted(),
                            QuoteState::Single => unreachable!(),
                        }

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
                            return Some(Ok(Token::Word {
                                parts: std::mem::take(&mut self.word_parts),
                                is_pattern,
                                is_assignment,
                            }));
                        } else {
                            return self.next();
                        }
                    }
                }
                Some('`') => {
                    if quote_state != QuoteState::Single {
                        if let Some(top) = self.paren_stack.last() && top.in_backtick && in_backtick {
                            match top.kind {
                                ParenKind::CommandSub | ParenKind::Subshell | ParenKind::Pattern | ParenKind::InnerMath => return Some(Err(ParseError::Unmatched(")"))),
                                ParenKind::Math | ParenKind::MathSub => return Some(Err(ParseError::Unmatched("))"))),
                            }
                        }

                        match quote_state {
                            QuoteState::None => self.append_slice(),
                            QuoteState::Single | QuoteState::Double => self.append_slice_quoted(),
                        }

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
                }
                Some('$') => {
                    // variable substitution is ignored in single quoted strings
                    if quote_state != QuoteState::Single {
                        match quote_state {
                            QuoteState::None => self.append_slice(),
                            QuoteState::Double => self.append_slice_quoted(),
                            QuoteState::Single => unreachable!(),
                        }
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
                                            old_word_parts: Some(std::mem::take(&mut self.word_parts)),
                                            is_quoted: quote_state != QuoteState::None,
                                            in_backtick,
                                        });
                                    }
                                    Some(_) => self.paren_stack.push(ParenStackEntry {
                                        kind: ParenKind::CommandSub,
                                        old_word_parts: Some(std::mem::take(&mut self.word_parts)),
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
                        if let Some(entry) = self.paren_stack.last() {
                            match entry.kind {
                                ParenKind::Math | ParenKind::MathSub => {
                                    self.advance();
                                    self.paren_stack.push(ParenStackEntry {
                                        kind: ParenKind::InnerMath,
                                        old_word_parts: None,
                                        is_quoted: quote_state != QuoteState::None,
                                        in_backtick,
                                    });
                                    continue;
                                }
                                _ => (),
                            }
                        }

                        self.append_slice();

                        let return_val = if !in_backtick && self.paren_stack.is_empty() {
                            if !self.word_parts.is_empty() {
                                Some(Ok(Token::Word {
                                    parts: std::mem::take(&mut self.word_parts),
                                    is_pattern,
                                    is_assignment,
                                }))
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
                                    kind: ParenKind::Math,
                                    old_word_parts: Some(std::mem::take(&mut self.word_parts)),
                                    is_quoted: quote_state != QuoteState::None,
                                    in_backtick,
                                });
                            }
                            Some(_) => self.paren_stack.push(ParenStackEntry {
                                kind: ParenKind::Subshell,
                                old_word_parts: Some(std::mem::take(&mut self.word_parts)),
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
                    if quote_state != QuoteState::Single && let Some(entry) = self.paren_stack.last() && entry.in_backtick == in_backtick && entry.kind != ParenKind::Pattern && entry.kind != ParenKind::InnerMath {
                        if entry.kind == ParenKind::Math || entry.kind == ParenKind::MathSub {
                            let old_index = self.index;

                            self.advance();
                            match self.iter.peek() {
                                Some(')') => (),
                                Some(_) => continue,
                                None => return Some(Err(ParseError::Unmatched("))"))),
                            }

                            let new_index = core::mem::replace(&mut self.index, old_index);
                            self.append_slice();
                            self.index = new_index;
                        } else {
                            self.append_slice();
                        }

                        let entry = self.paren_stack.pop().unwrap();

                        let parts = std::mem::replace(&mut self.word_parts, std::mem::take(&mut entry.old_word_parts.unwrap()));
                        match entry.kind {
                            ParenKind::CommandSub => self.word_parts.push(WordPart::CommandSub(parts)),
                            ParenKind::Subshell => self.word_parts.push(WordPart::Subshell(parts)),
                            ParenKind::MathSub => self.word_parts.push(WordPart::MathSub(parts)),
                            ParenKind::Math => self.word_parts.push(WordPart::Math(parts)),
                            ParenKind::Pattern | ParenKind::InnerMath => unreachable!(),
                        }

                        self.old_index = self.index + 1;
                    }
                }
                Some('=') => {
                    if quote_state == QuoteState::None && self.paren_stack.is_empty() && self.word_parts.is_empty() && self.index > self.old_index {
                        is_assignment = true;

                        // split the word before and after the = to make parsing assigments easier
                        self.append_slice();
                        self.old_index = self.index;
                        self.advance();
                        self.append_slice();
                        self.old_index = self.index;
                        continue;
                    }
                }
                Some('[') | Some(']') => is_pattern = true,
                Some('*') | Some('?') | Some('+') | Some('@') | Some('!') => {
                    is_pattern = true;

                    self.advance();
                    if let Some('(') = self.iter.peek() {
                        self.paren_stack.push(ParenStackEntry {
                            kind: ParenKind::Pattern,
                            old_word_parts: None,
                            is_quoted: quote_state != QuoteState::None,
                            in_backtick,
                        });
                    } else {
                        continue;
                    }
                }
                _ => (),
            }

            prev_num = matches!(self.iter.peek(), Some('0'..='9'));

            self.advance();
        }
    }
}
