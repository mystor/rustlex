use std::char;

use unicode_xid::UnicodeXID;

use span::{offset_line_col, Span, Spanned};
use token::{IntSuffix, FloatSuffix, Lit, Token};
use error::LexResult;

/// An incremental lexer. Consumes the input string and lexes individual tokens
/// whenever `next` is called.
pub struct Lexer<'a> {
    input: &'a str,
    idx: usize,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer, given the input string
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            input: input,
            idx: 0,
        }
    }

    fn byte(&self, idx: usize) -> u8 {
        if idx + self.idx < self.input.len() {
            self.input.as_bytes()[idx + self.idx]
        } else {
            0
        }
    }

    fn opt_byte(&self, idx: usize) -> Option<u8> {
        if idx + self.idx < self.input.len() {
            Some(self.byte(idx))
        } else {
            None
        }
    }

    fn rest(&self) -> &'a str {
        &self.input[self.idx..]
    }

    fn next_char(&self) -> char {
        self.rest().chars().next().unwrap_or('\0')
    }

    fn opt_next_char(&self) -> Option<char> {
        self.rest().chars().next()
    }

    fn skip_whitespace(&mut self) -> LexResult<()> {
        while !self.rest().is_empty() {
            let ch = self.next_char();
            if is_whitespace(ch) {
                self.idx += ch.len_utf8();
                continue;
            }

            let line_comment = self.byte(0) == b'/' &&
                self.byte(1) == b'/' &&
                self.byte(2) != b'!' &&
                (self.byte(2) != b'/' ||
                 self.byte(3) == b'/');

            if line_comment {
                if let Some(len) = self.rest().find('\n') {
                    self.idx += len;
                    continue;
                } else {
                    self.idx = self.input.len();
                    continue;
                }
            }

            let block_comment = self.byte(0) == b'/' &&
                self.byte(1) == b'*' &&
                self.byte(2) != b'!' &&
                (self.byte(2) != b'*' ||
                 self.byte(3) == b'*');

            if block_comment {
                self.block_comment()?;
                continue;
            }

            return Ok(());
        }
        Ok(())
    }

    fn block_comment(&mut self) -> LexResult<Spanned<&'a str>> {
        assert!(self.rest().starts_with("/*"));

        let start = self.idx;
        let mut depth = 0;
        while self.rest().len() >= 2 {
            if self.byte(0) == b'/' && self.byte(1) == b'*' {
                depth += 1;
                self.idx += 2; // eat '*'
                continue;
            }

            if self.byte(0) == b'*' && self.byte(1) == b'/' {
                depth -= 1;
                self.idx += 2;
                if depth == 0 {
                    return Ok(Spanned {
                        node: &self.input[start..self.idx],
                        span: Span {
                            lo: start,
                            hi: self.idx,
                        },
                    });
                }
                continue;
            }

            self.idx += 1;
        }

        fmt_err!(self, "unexpected EOF while parsing block comment");
    }

    fn doc_comment(&mut self) -> LexResult<Option<Spanned<Token>>> {
        let start = self.idx;
        if self.byte(0) != b'/' {
            return Ok(None);
        }

        let (block_byte, inner_byte) = (self.byte(1), self.byte(2));

        let text = if block_byte == b'/' {
            if let Some(len) = self.rest().find('\n') {
                self.idx += len;
            } else {
                self.idx = self.input.len();
            }

            &self.input[start..self.idx]
        } else if block_byte == b'*' {
            self.block_comment()?.node
        } else {
            return Ok(None);
        };

        Ok(Some(Spanned {
            node: if inner_byte == block_byte {
                Token::OuterDoc(text.into())
            } else if inner_byte == b'!' {
                Token::InnerDoc(text.into())
            } else {
                unreachable!()
            },
            span: Span {
                lo: start,
                hi: self.idx,
            },
        }))
    }

    fn symbol(&mut self) -> LexResult<Option<Spanned<Token>>> {
        let start = self.idx;
        let tok = match self.byte(0) {
            b'=' => match self.byte(1) {
                b'=' => {
                    self.idx += 2;
                    Token::EqEq
                }
                b'>' => {
                    self.idx += 2;
                    Token::FatArrow
                }
                _ => {
                    self.idx += 1;
                    Token::Eq
                }
            },
            b'<' => match self.byte(1) {
                b'=' => {
                    self.idx += 2;
                    Token::Le
                }
                b'-' => {
                    self.idx += 2;
                    Token::LArrow
                }
                b'<' => match self.byte(2) {
                    b'=' => {
                        self.idx += 3;
                        Token::ShlEq
                    }
                    _ => {
                        self.idx += 2;
                        Token::Shl
                    }
                },
                _ => {
                    self.idx += 1;
                    Token::Lt
                }
            },
            b'>' => match self.byte(1) {
                b'=' => {
                    self.idx += 2;
                    Token::Ge
                }
                b'>' => match self.byte(2) {
                    b'=' => {
                        self.idx += 3;
                        Token::ShrEq
                    }
                    _ => {
                        self.idx += 2;
                        Token::Shr
                    }
                },
                _ => {
                    self.idx += 1;
                    Token::Gt
                }
            },
            b'!' => match self.byte(1) {
                b'=' => {
                    self.idx += 2;
                    Token::Ne
                }
                _ => {
                    self.idx += 1;
                    Token::Not
                }
            },
            b'&' => match self.byte(1) {
                b'&' => {
                    self.idx += 2;
                    Token::AndAnd
                }
                b'=' => {
                    self.idx += 2;
                    Token::AndEq
                }
                _ => {
                    self.idx += 1;
                    Token::And
                }
            },
            b'|' => match self.byte(1) {
                b'|' => {
                    self.idx += 2;
                    Token::OrOr
                }
                b'=' => {
                    self.idx += 2;
                    Token::OrEq
                }
                _ => {
                    self.idx += 1;
                    Token::Or
                }
            },
            b'~' => {
                self.idx += 1;
                Token::Tilde
            }
            b'+' => match self.byte(1) {
                b'=' => {
                    self.idx += 2;
                    Token::PlusEq
                }
                _ => {
                    self.idx += 1;
                    Token::Plus
                }
            },
            b'-' => match self.byte(1) {
                b'>' => {
                    self.idx += 2;
                    Token::RArrow
                }
                b'=' => {
                    self.idx += 2;
                    Token::MinusEq
                }
                _ => {
                    self.idx += 1;
                    Token::Minus
                }
            },
            b'*' => match self.byte(1) {
                b'=' => {
                    self.idx += 2;
                    Token::StarEq
                }
                _ => {
                    self.idx += 1;
                    Token::Star
                }
            },
            b'/' => match self.byte(1) {
                b'=' => {
                    self.idx += 2;
                    Token::SlashEq
                }
                _ => {
                    self.idx += 1;
                    Token::Slash
                }
            },
            b'%' => match self.byte(1) {
                b'=' => {
                    self.idx += 2;
                    Token::PercentEq
                }
                _ => {
                    self.idx += 1;
                    Token::Percent
                }
            },
            b'^' => match self.byte(1) {
                b'=' => {
                    self.idx += 2;
                    Token::CaretEq
                }
                _ => {
                    self.idx += 1;
                    Token::Caret
                }
            },
            b'@' => {
                self.idx += 1;
                Token::At
            }
            b'.' => match self.byte(1) {
                b'.' => match self.byte(2) {
                    b'.' => {
                        self.idx += 3;
                        Token::DotDotDot
                    }
                    _ => {
                        self.idx += 2;
                        Token::DotDot
                    }
                },
                _ => {
                    self.idx += 1;
                    Token::Dot
                }
            },
            b',' => {
                self.idx += 1;
                Token::Comma
            }
            b';' => {
                self.idx += 1;
                Token::Semi
            }
            b':' => match self.byte(1) {
                b':' => {
                    self.idx += 2;
                    Token::ModSep
                }
                _ => {
                    self.idx += 1;
                    Token::Colon
                }
            },
            b'#' => {
                self.idx += 1;
                Token::Pound
            }
            b'$' => {
                self.idx += 1;
                Token::Dollar
            }
            b'?' => {
                self.idx += 1;
                Token::Question
            }
            b'(' => {
                self.idx += 1;
                Token::LParen
            }
            b'{' => {
                self.idx += 1;
                Token::LBrace
            }
            b'[' => {
                self.idx += 1;
                Token::LBracket
            }
            b')' => {
                self.idx += 1;
                Token::RParen
            }
            b'}' => {
                self.idx += 1;
                Token::RBrace
            }
            b']' => {
                self.idx += 1;
                Token::RBracket
            }
            _ => return Ok(None)
        };

        Ok(Some(Spanned {
            node: tok,
            span: Span {
                lo: start,
                hi: self.idx,
            }
        }))
    }

    fn word(&mut self) -> Option<&'a str> {
        let mut chars = self.rest().char_indices();
        match chars.next() {
            Some((_, ch)) if UnicodeXID::is_xid_start(ch) || ch == '_' => {}
            _ => return None,
        }

        while let Some((i, ch)) = chars.next() {
            if !UnicodeXID::is_xid_continue(ch) {
                let result = &self.rest()[..i];
                self.idx += i;
                return Some(result);
            }
        }

        let result = self.rest();
        self.idx = self.input.len();
        Some(result)
    }

    fn ident(&mut self) -> LexResult<Option<Spanned<Token>>> {
        let start = self.idx;
        let word = if let Some(word) = self.word() {
            word
        } else {
            return Ok(None);
        };

        Ok(Some(Spanned {
            node: {
                match word {
                    "_" => Token::Underscore,
                    "abstract" => Token::Abstract,
                    "alignof" => Token::Alignof,
                    "as" => Token::As,
                    "become" => Token::Become,
                    "box" => Token::Box,
                    "break" => Token::Break,
                    "const" => Token::Const,
                    "continue" => Token::Continue,
                    "crate" => Token::Crate,
                    "do" => Token::Do,
                    "else" => Token::Else,
                    "enum" => Token::Enum,
                    "extern" => Token::Extern,
                    "false" => Token::Lit(Lit::Bool(false)),
                    "final" => Token::Final,
                    "fn" => Token::Fn,
                    "for" => Token::For,
                    "if" => Token::If,
                    "impl" => Token::Impl,
                    "in" => Token::In,
                    "let" => Token::Let,
                    "loop" => Token::Loop,
                    "macro" => Token::Macro,
                    "match" => Token::Match,
                    "mod" => Token::Mod,
                    "move" => Token::Move,
                    "mut" => Token::Mut,
                    "offsetof" => Token::Offsetof,
                    "override" => Token::Override,
                    "priv" => Token::Priv,
                    "proc" => Token::Proc,
                    "pub" => Token::Pub,
                    "pure" => Token::Pure,
                    "ref" => Token::Ref,
                    "return" => Token::Return,
                    "Self" => Token::CapSelf,
                    "self" => Token::LowSelf,
                    "sizeof" => Token::Sizeof,
                    "static" => Token::Static,
                    "struct" => Token::Struct,
                    "super" => Token::Super,
                    "trait" => Token::Trait,
                    "true" => Token::Lit(Lit::Bool(true)),
                    "type" => Token::Type,
                    "typeof" => Token::Typeof,
                    "unsafe" => Token::Unsafe,
                    "unsized" => Token::Unsized,
                    "use" => Token::Use,
                    "virtual" => Token::Virtual,
                    "where" => Token::Where,
                    "while" => Token::While,
                    "yield" => Token::Yield,
                    id => Token::Ident(id.into()),
                }
            },
            span: Span {
                lo: start,
                hi: self.idx,
            }
        }))
    }

    fn num(&mut self) -> LexResult<Option<Spanned<Token>>> {
        let start = self.idx;

        let base = match (self.byte(0), self.byte(1)) {
            (b'0', b'x') => {
                self.idx += 2;
                16
            }
            (b'0', b'o') => {
                self.idx += 2;
                8
            }
            (b'0', b'b') => {
                self.idx += 2;
                2
            }
            (b'0'...b'9', _) => 10,
            _ => return Ok(None),
        };

        let mut value = 0u64;
        let mut has_dot = false;
        let mut has_exp = false;
        loop {
            let b = self.byte(0);
            let digit = match b {
                b'e' | b'E' if base == 10 => {
                    self.idx += 1;
                    has_exp = true;
                    break;
                }
                b'0'...b'9' => (b - b'0') as u64,
                b'a'...b'f' if base > 10 => 10 + (b - b'a') as u64,
                b'A'...b'F' if base > 10 => 10 + (b - b'A') as u64,
                b'_' => {
                    self.idx += 1;
                    continue;
                }
                b'.' if base == 10 => {
                    self.idx += 1;
                    let ignorable =
                        has_dot ||
                        self.byte(0) == b'.' ||
                        UnicodeXID::is_xid_start(self.next_char());
                    if ignorable {
                        self.idx -= 1;
                        break;
                    }

                    has_dot = true;
                    continue;
                }
                _ => break,
            };

            if digit >= base {
                fmt_err!(self, "unexpected digit {:x} out of base range", digit);
            }
            if let Some(v) = value.checked_mul(base).and_then(|v| v.checked_add(digit)) {
                value = v
            } else {
                fmt_err!(self, "overflow while parsing integer literal");
            }
            self.idx += 1;
        }

        if has_exp {
            let mut has_value = false;
            loop {
                match self.byte(0) {
                    b'+' | b'-' if !has_value => {
                        self.idx += 1;
                    }
                    b'0'...b'9' => {
                        self.idx += 1;
                        has_value = true;
                    }
                    b'_' => {
                        self.idx += 1;
                    }
                    _ => if !has_value {
                        fmt_err!(self, "unexpected end of float literal after \
                                        `E` character");
                    } else {
                        break;
                    }
                }
            }
        }

        let can_be_int = !has_dot && !has_exp;
        let can_be_float = base == 10;

        let node = match (self.byte(0), self.byte(1), self.byte(2), self.byte(3), self.byte(4)) {
            (b'f', b'3', b'2', ..) if can_be_float => {
                self.idx += 3;
                Token::Lit(Lit::Float(self.input[start..self.idx].into(), FloatSuffix::F32))
            }
            (b'f', b'6', b'4', ..) if can_be_float => {
                self.idx += 3;
                Token::Lit(Lit::Float(self.input[start..self.idx].into(), FloatSuffix::F64))
            }
            (b'u', b'8', ..) if can_be_int => {
                self.idx += 2;
                Token::Lit(Lit::Integer(value, IntSuffix::U8))
            }
            (b'i', b'8', ..) if can_be_int => {
                self.idx += 2;
                Token::Lit(Lit::Integer(value, IntSuffix::I8))
            }
            (b'u', b'1', b'6', ..) if can_be_int => {
                self.idx += 3;
                Token::Lit(Lit::Integer(value, IntSuffix::U16))
            }
            (b'i', b'1', b'6', ..) if can_be_int => {
                self.idx += 3;
                Token::Lit(Lit::Integer(value, IntSuffix::I16))
            }
            (b'u', b'3', b'2', ..) if can_be_int => {
                self.idx += 3;
                Token::Lit(Lit::Integer(value, IntSuffix::U32))
            }
            (b'i', b'3', b'2', ..) if can_be_int => {
                self.idx += 3;
                Token::Lit(Lit::Integer(value, IntSuffix::I32))
            }
            (b'u', b'6', b'4', ..) if can_be_int => {
                self.idx += 3;
                Token::Lit(Lit::Integer(value, IntSuffix::U64))
            }
            (b'i', b'6', b'4', ..) if can_be_int => {
                self.idx += 3;
                Token::Lit(Lit::Integer(value, IntSuffix::I64))
            }
            (b'u', b's', b'i', b'z', b'e') if can_be_int => {
                self.idx += 3;
                Token::Lit(Lit::Integer(value, IntSuffix::Usize))
            }
            (b'i', b's', b'i', b'z', b'e') if can_be_int => {
                self.idx += 3;
                Token::Lit(Lit::Integer(value, IntSuffix::Isize))
            }
            _ => if can_be_int {
                Token::Lit(Lit::Integer(value, IntSuffix::Unsuffixed))
            } else if can_be_float {
                Token::Lit(Lit::Float(self.input[start..self.idx].into(),
                                      FloatSuffix::Unsuffixed))
            } else {
                unreachable!()
            }
        };

        Ok(Some(Spanned {
            node: node,
            span: Span {
                lo: start,
                hi: self.idx,
            }
        }))
    }

    fn raw_str(&mut self) -> LexResult<String> {
        let mut pounds = 0;
        while self.byte(0) == b'#' {
            pounds += 1;
            self.idx += 1;
        }
        if self.byte(0) != b'"' {
            fmt_err!(self, "unexpected character while parsing raw string literal");
        }
        self.idx += 1;
        let inner_start = self.idx;

        loop {
            match self.byte(0) {
                b'"' => {
                    let end_start = self.idx;
                    self.idx += 1;
                    let mut cl_pounds = 0;
                    while cl_pounds < pounds {
                        if self.byte(0) != b'#' {
                            break;
                        }
                        cl_pounds += 1;
                        self.idx += 1;
                    }
                    if cl_pounds == pounds {
                        return Ok(self.input[inner_start..end_start].into());
                    }
                }
                _ => {}
            }
            self.idx += 1;
        }
    }

    fn backslash_x(&mut self) -> LexResult<u8> {
        let mut ch = 0;
        let b0 = self.byte(0);
        let b1 = self.byte(1);
        ch += 0x10 * match b0 {
            b'0'...b'9' => b0 - b'0',
            b'a'...b'f' => 10 + (b0 - b'a'),
            b'A'...b'F' => 10 + (b0 - b'A'),
            _ => fmt_err!(self, "unexpected non-hex character after \\x"),
        };
        ch += 0x1 * match b1 {
            b'0'...b'9' => b1 - b'0',
            b'a'...b'f' => 10 + (b1 - b'a'),
            b'A'...b'F' => 10 + (b1 - b'A'),
            _ => fmt_err!(self, "unexpected non-hex character after \\x"),
        };
        self.idx += 2;
        Ok(ch)
    }

    fn backslash_u(&mut self) -> LexResult<char> {
        if self.byte(0) != b'{' {
            fmt_err!(self, "expected {{ after \\u");
        }
        self.idx += 1;
        let mut ch = 0;
        for _ in 0..6 {
            let b = self.byte(0);
            match b {
                b'0'...b'9' => {
                    ch *= 0x10;
                    ch += (b - b'0') as u32;
                    self.idx += 1;
                }
                b'a'...b'f' => {
                    ch *= 0x10;
                    ch += (10 + b - b'a') as u32;
                    self.idx += 1;
                }
                b'A'...b'F' => {
                    ch *= 0x10;
                    ch += (10 + b - b'A') as u32;
                    self.idx += 1;
                }
                b'}' => break,
                _ => fmt_err!(self, "unexpected non-hex character after \\u"),
            }
        }
        if self.byte(0) != b'}' {
            fmt_err!(self, "expected }} to terminate \\u unicode hexcode");
        }
        self.idx += 1;
        if let Some(ch) = char::from_u32(ch) {
            Ok(ch)
        } else {
            fmt_err!(self, "character code {:x} is not a valid unicode character", ch);
        }
    }

    fn str_lit(&mut self) -> LexResult<String> {
        let mut s = String::new();
        loop {
            let ch = match self.opt_byte(0) {
                Some(b'"') => {
                    self.idx += 1;
                    return Ok(s.into());
                }
                Some(b'\\') => {
                    match self.opt_byte(1) {
                        Some(b'x') => {
                            self.idx += 2;
                            let byte = self.backslash_x()?;
                            if byte > 0x80 {
                                fmt_err!(self, "invalid \\x byte {:x} in string literal", byte);
                            }
                            char::from_u32(byte as u32).unwrap()
                        }
                        Some(b'u') => {
                            self.idx += 2;
                            self.backslash_u()?
                        }
                        Some(b'n') => {
                            self.idx += 2;
                            '\n'
                        }
                        Some(b'r') => {
                            self.idx += 2;
                            '\r'
                        }
                        Some(b't') => {
                            self.idx += 2;
                            '\t'
                        }
                        Some(b'\\') => {
                            self.idx += 2;
                            '\\'
                        }
                        Some(b'0') => {
                            self.idx += 2;
                            '\0'
                        }
                        Some(b'\'') => {
                            self.idx += 2;
                            '\''
                        }
                        Some(b'"') => {
                            self.idx += 2;
                            '"'
                        }
                        Some(b'\n') => {
                            self.idx += 2;
                            while let Some(ch) = self.opt_next_char() {
                                if ch.is_whitespace() {
                                    self.idx += ch.len_utf8();
                                } else {
                                    break;
                                }
                            }
                            continue;
                        }
                        Some(b) => fmt_err!(self, "unexpected byte {:?} after \\ character \
                                                   in byte literal", b),
                        None => fmt_err!(self, "unexpected Eof after \\ character in byte literal"),
                    }
                }
                Some(_) => {
                    let ch = self.next_char();
                    self.idx += ch.len_utf8();
                    ch
                },
                None => fmt_err!(self, "unexpected Eof while parsing byte literal")
            };
            s.push(ch);
        }
    }

    fn byte_str_lit(&mut self) -> LexResult<Vec<u8>> {
        let mut s = Vec::new();
        loop {
            let byte = match self.opt_byte(0) {
                Some(b'"') => {
                    self.idx += 1;
                    return Ok(s);
                }
                Some(b'\\') => {
                    match self.opt_byte(1) {
                        Some(b'x') => {
                            self.idx += 2;
                            self.backslash_x()?
                        }
                        Some(b'n') => {
                            self.idx += 2;
                            b'\n'
                        }
                        Some(b'r') => {
                            self.idx += 2;
                            b'\r'
                        }
                        Some(b't') => {
                            self.idx += 2;
                            b'\t'
                        }
                        Some(b'\\') => {
                            self.idx += 2;
                            b'\\'
                        }
                        Some(b'0') => {
                            self.idx += 2;
                            b'\0'
                        }
                        Some(b'\'') => {
                            self.idx += 2;
                            b'\''
                        }
                        Some(b'"') => {
                            self.idx += 2;
                            b'"'
                        }
                        Some(b'\n') => {
                            self.idx += 2;
                            while let Some(ch) = self.opt_next_char() {
                                if ch.is_whitespace() {
                                    self.idx += ch.len_utf8();
                                } else {
                                    break;
                                }
                            }
                            continue;
                        }
                        Some(b) => fmt_err!(self, "unexpected byte {:?} after \\ character \
                                                   in byte literal", b),
                        None => fmt_err!(self, "unexpected Eof after \\ character in byte literal"),
                    }
                }
                Some(b) => {
                    self.idx += 1;
                    b
                },
                None => fmt_err!(self, "unexpected Eof while parsing byte literal")
            };
            s.push(byte);
        }
    }

    fn char_lit(&mut self) -> LexResult<Option<char>> {
        let start = self.idx;
        let ch = match self.opt_byte(0) {
            Some(b'\\') => {
                match self.opt_byte(1) {
                    Some(b'x') => {
                        self.idx += 2;
                        let byte = self.backslash_x()?;
                        assert!(byte <= 0x80);
                        char::from_u32(byte as u32).unwrap()
                    }
                    Some(b'u') => {
                        self.idx += 2;
                        self.backslash_u()?
                    }
                    Some(b'n') => {
                        self.idx += 2;
                        '\n'
                    }
                    Some(b'r') => {
                        self.idx += 2;
                        '\r'
                    }
                    Some(b't') => {
                        self.idx += 2;
                        '\t'
                    }
                    Some(b'\\') => {
                        self.idx += 2;
                        '\\'
                    }
                    Some(b'0') => {
                        self.idx += 2;
                        '\0'
                    }
                    Some(b'\'') => {
                        self.idx += 2;
                        '\''
                    }
                    Some(b'"') => {
                        self.idx += 2;
                        '"'
                    }
                    Some(b) => fmt_err!(self, "unexpected byte {:?} after \\ character \
                                               in byte literal", b),
                    None => fmt_err!(self, "unexpected Eof after \\ character in byte literal"),
                }
            }
            Some(_) => {
                let ch = self.next_char();
                self.idx += ch.len_utf8();
                ch
            },
            None => fmt_err!(self, "unexpected Eof while parsing byte literal")
        };

        if self.byte(0) != b'\'' {
            self.idx = start;
            Ok(None)
        } else {
            self.idx += 1;
            Ok(Some(ch))
        }
    }

    fn byte_lit(&mut self) -> LexResult<u8> {
        let byte = match self.opt_byte(0) {
            Some(b'\\') => {
                match self.opt_byte(1) {
                    Some(b'x') => {
                        self.idx += 2;
                        self.backslash_x()?
                    }
                    Some(b'n') => {
                        self.idx += 2;
                        b'\n'
                    }
                    Some(b'r') => {
                        self.idx += 2;
                        b'\r'
                    }
                    Some(b't') => {
                        self.idx += 2;
                        b'\t'
                    }
                    Some(b'\\') => {
                        self.idx += 2;
                        b'\\'
                    }
                    Some(b'0') => {
                        self.idx += 2;
                        b'\0'
                    }
                    Some(b'\'') => {
                        self.idx += 2;
                        b'\''
                    }
                    Some(b'"') => {
                        self.idx += 2;
                        b'"'
                    }
                    Some(b) => fmt_err!(self, "unexpected byte {:?} after \\ character \
                                               in byte literal", b),
                    None => fmt_err!(self, "unexpected Eof after \\ character in byte literal"),
                }
            }
            Some(b) => {
                // NOTE: At this point, self.idx doesn't necessarially point to
                // a byte boundary, however, the below assertion will assert
                // that we are.
                self.idx += 1;
                b
            },
            None => fmt_err!(self, "unexpected Eof while parsing byte literal")
        };

        assert!(self.byte(0) == b'\'');
        self.idx += 1;
        Ok(byte)
    }

    fn stringlike(&mut self) -> LexResult<Option<Spanned<Token>>> {
        let start = self.idx;

        let node = match (self.byte(0), self.byte(1), self.byte(2)) {
            (b'b', b'\'', ..) => {
                self.idx += 2;
                Token::Lit(Lit::Byte(self.byte_lit()?))
            }
            (b'b', b'"', ..) => {
                self.idx += 2;
                Token::Lit(Lit::ByteStr(self.byte_str_lit()?))
            }
            (b'\'', ..) => {
                self.idx += 1;
                if let Some(ch) = self.char_lit()? {
                    Token::Lit(Lit::Char(ch))
                } else {
                    if let Some(word) = self.word() {
                        Token::Lifetime(word.into())
                    } else {
                        fmt_err!(self, "expected identifier while parsing lifetime");
                    }
                }
            }
            (b'"', ..) => {
                self.idx += 1;
                Token::Lit(Lit::Str(self.str_lit()?))
            }
            (b'b', b'r', b'#') => {
                self.idx += 2;
                Token::Lit(Lit::ByteStr(self.raw_str()?.into_bytes()))
            }
            (b'r', b'#', ..) => {
                self.idx += 1;
                Token::Lit(Lit::Str(self.raw_str()?))
            }
            _ => return Ok(None),
        };

        Ok(Some(Spanned {
            node: node,
            span: Span {
                lo: start,
                hi: self.idx,
            }
        }))
    }

    fn err_info(&self) -> (usize, usize, usize) {
        let (line, col) = offset_line_col(self.input, self.idx);
        (self.idx, line, col)
    }

    /// Get the next token from the lexer.
    pub fn next(&mut self) -> LexResult<Option<Spanned<Token>>> {
        self.skip_whitespace()?;

        if let Some(t) = self.doc_comment()? {
            return Ok(Some(t));
        }

        if let Some(t) = self.symbol()? {
            return Ok(Some(t));
        }

        if let Some(t) = self.num()? {
            return Ok(Some(t));
        }

        if let Some(t) = self.stringlike()? {
            return Ok(Some(t));
        }

        if let Some(t) = self.ident()? {
            return Ok(Some(t));
        }

        if self.rest().len() > 0 {
            fmt_err!(self, "unexpected character {:?} does not start any token",
                     self.next_char());
        }

        Ok(None)
    }
}

/// Check if the passed in character is whitespace as far as rustc would be
/// concerned. Rustc considers left-to-right mark and right-to-left mark as
/// whitespace.
fn is_whitespace(c: char) -> bool {
    c.is_whitespace() || c == '\u{200e}' || c == '\u{200f}'
}

/// Create and run a `Lexer` to completion, producing an error if tokenization
/// fails, and a Vec of the resulting tokens if it succeeds.
pub fn tokenize(input: &str) -> LexResult<Vec<Spanned<Token>>> {
    let mut tokens = Vec::new();
    let mut lexer = Lexer::new(input);
    while let Some(token) = lexer.next()? {
        tokens.push(token);
    }
    Ok(tokens)
}

#[cfg(test)]
mod test {
    use super::*;

    fn unspan_tok(input: &str) -> Vec<Token> {
        tokenize(input)
            .unwrap()
            .into_iter()
            .map(|t| t.node)
            .collect()
    }

    #[test]
    fn symbols() {
        let result = unspan_tok("+ - += -= / /= // This is a comment");
        assert_eq!(result, &[
            Token::Plus,
            Token::Minus,
            Token::PlusEq,
            Token::MinusEq,
            Token::Slash,
            Token::SlashEq,
        ]);
    }

    #[test]
    fn comments() {
        let result = unspan_tok(r#"
+ // Line Comment
/
/* Block Comment
Can span
Many /* lines */ And be nested */
*
"#);
        assert_eq!(result, &[
            Token::Plus,
            Token::Slash,
            Token::Star,
        ]);
    }

    #[test]
    fn all_symbols() {
        let result = unspan_tok("= < <= == != >= > && || ! ~ + - * / % ^ \
                               & | << >> += -= *= /= %= ^= &= |= <<= \
                               >>= @ . .. ... , ; : :: -> <- => # $ ? ( \
                               { [ ] } )");
        assert_eq!(result, &[
            Token::Eq,
            Token::Lt,
            Token::Le,
            Token::EqEq,
            Token::Ne,
            Token::Ge,
            Token::Gt,
            Token::AndAnd,
            Token::OrOr,
            Token::Not,
            Token::Tilde,
            Token::Plus,
            Token::Minus,
            Token::Star,
            Token::Slash,
            Token::Percent,
            Token::Caret,
            Token::And,
            Token::Or,
            Token::Shl,
            Token::Shr,
            Token::PlusEq,
            Token::MinusEq,
            Token::StarEq,
            Token::SlashEq,
            Token::PercentEq,
            Token::CaretEq,
            Token::AndEq,
            Token::OrEq,
            Token::ShlEq,
            Token::ShrEq,
            Token::At,
            Token::Dot,
            Token::DotDot,
            Token::DotDotDot,
            Token::Comma,
            Token::Semi,
            Token::Colon,
            Token::ModSep,
            Token::RArrow,
            Token::LArrow,
            Token::FatArrow,
            Token::Pound,
            Token::Dollar,
            Token::Question,
            Token::LParen,
            Token::LBrace,
            Token::LBracket,
            Token::RBracket,
            Token::RBrace,
            Token::RParen,
        ][..]);
    }

    #[test]
    fn doc_comments() {
        let result = unspan_tok(r#"
/// Outer Doc comment
/// Which spans multiple lines

/**
 * Multiline outer doc comment
 */

//! Inner Doc Comment

/*! Inner multiline doc
    comment */
"#);

        assert_eq!(result, &[
            Token::OuterDoc("/// Outer Doc comment".into()),
            Token::OuterDoc("/// Which spans multiple lines".into()),
            Token::OuterDoc("/**\n * Multiline outer doc comment\n */".into()),
            Token::InnerDoc("//! Inner Doc Comment".into()),
            Token::InnerDoc("/*! Inner multiline doc\n    comment */".into()),
        ]);
    }

    #[test]
    fn idents() {
        let result = unspan_tok("apple pear _ _food __apples let for Self");

        assert_eq!(result, &[
            Token::Ident("apple".into()),
            Token::Ident("pear".into()),
            Token::Underscore,
            Token::Ident("_food".into()),
            Token::Ident("__apples".into()),
            Token::Let,
            Token::For,
            Token::CapSelf,
        ]);
    }

    #[test]
    fn all_keywords() {
        let result = unspan_tok("abstract alignof as become box break \
                               const continue crate do else enum extern \
                               false final fn for if impl in let loop \
                               macro match mod move mut offsetof \
                               override priv proc pub pure ref return \
                               Self self sizeof static struct super \
                               trait true type typeof unsafe unsized \
                               use virtual where while yield _");
        assert_eq!(result, &[
            Token::Abstract,
            Token::Alignof,
            Token::As,
            Token::Become,
            Token::Box,
            Token::Break,
            Token::Const,
            Token::Continue,
            Token::Crate,
            Token::Do,
            Token::Else,
            Token::Enum,
            Token::Extern,
            Token::Lit(Lit::Bool(false)),
            Token::Final,
            Token::Fn,
            Token::For,
            Token::If,
            Token::Impl,
            Token::In,
            Token::Let,
            Token::Loop,
            Token::Macro,
            Token::Match,
            Token::Mod,
            Token::Move,
            Token::Mut,
            Token::Offsetof,
            Token::Override,
            Token::Priv,
            Token::Proc,
            Token::Pub,
            Token::Pure,
            Token::Ref,
            Token::Return,
            Token::CapSelf,
            Token::LowSelf,
            Token::Sizeof,
            Token::Static,
            Token::Struct,
            Token::Super,
            Token::Trait,
            Token::Lit(Lit::Bool(true)),
            Token::Type,
            Token::Typeof,
            Token::Unsafe,
            Token::Unsized,
            Token::Use,
            Token::Virtual,
            Token::Where,
            Token::While,
            Token::Yield,
            Token::Underscore,
        ][..]);
    }

    #[test]
    fn numbers() {
        let result = unspan_tok("0 10i32 0x17faed 0. 0.03 4f32 10.3E3_7f64");

        assert_eq!(result, &[
            Token::Lit(Lit::Integer(0, IntSuffix::Unsuffixed)),
            Token::Lit(Lit::Integer(10, IntSuffix::I32)),
            Token::Lit(Lit::Integer(0x17faed, IntSuffix::Unsuffixed)),
            Token::Lit(Lit::Float("0.".into(), FloatSuffix::Unsuffixed)),
            Token::Lit(Lit::Float("0.03".into(), FloatSuffix::Unsuffixed)),
            Token::Lit(Lit::Float("4f32".into(), FloatSuffix::F32)),
            Token::Lit(Lit::Float("10.3E3_7f64".into(), FloatSuffix::F64)),
        ]);
    }

    #[test]
    fn stringlike() {
        let result = unspan_tok(r###"
"apple"
'a'
'a
'apples
b"apple"
b'a'
br#"apple"#
r#"apple"#
r##"apple"##
'\x14'
"###);

        assert_eq!(result, &[
            Token::Lit(Lit::Str("apple".into())),
            Token::Lit(Lit::Char('a')),
            Token::Lifetime("a".into()),
            Token::Lifetime("apples".into()),
            Token::Lit(Lit::ByteStr(b"apple".to_vec())),
            Token::Lit(Lit::Byte(b'a')),
            Token::Lit(Lit::ByteStr(b"apple".to_vec())),
            Token::Lit(Lit::Str("apple".into())),
            Token::Lit(Lit::Str("apple".into())),
            Token::Lit(Lit::Char('\x14')),
        ]);
    }

    #[test]
    fn test_spans() {
        assert_eq!(tokenize("fn apple"), Ok(vec![
            Spanned {
                node: Token::Fn,
                span: Span { lo: 0, hi: 2 },
            },
            Spanned {
                node: Token::Ident("apple".to_owned()),
                span: Span { lo: 3, hi: 8 },
            },
        ]));
    }
}
