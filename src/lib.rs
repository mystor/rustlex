extern crate unicode_xid;
extern crate memchr;

#[macro_use]
mod error;
mod lex;
mod token;
mod span;

pub use error::{LexError, LexResult};
pub use lex::{Lexer, tokenize};
pub use token::{Token, Lit, FloatSuffix, IntSuffix};
pub use span::{Span, Spanned, offset_line_col};

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_line_col() {
        let source = "\
= < > == != >= > && || ! ~
+ - * / % ^ & | << >>
+= -= *= %= ^= &= |= <<= >>=
@ . .. ... , ; : :: -> <- => # $ ?
( [ { } ] )

/*! foo */
//! foo
/** foo */
/// foo

b'a'
'b'
10
10u32
3.3
3.3f32
\"apple\"
b\"pear\"
true
false

'apples

apples
_

abstract";
        let tokens = tokenize(source).unwrap();

        let infos = tokens
            .into_iter()
            .map(|t| {
                     let src = &source[t.span.lo..t.span.hi];
                     let (line, col) = offset_line_col(source, t.span.lo);
                     (src, line, col)
                 })
            .collect::<Vec<_>>();

        let expected = &[("=", 1, 0),
                         ("<", 1, 2),
                         (">", 1, 4),
                         ("==", 1, 6),
                         ("!=", 1, 9),
                         (">=", 1, 12),
                         (">", 1, 15),
                         ("&&", 1, 17),
                         ("||", 1, 20),
                         ("!", 1, 23),
                         ("~", 1, 25),

                         ("+", 2, 0),
                         ("-", 2, 2),
                         ("*", 2, 4),
                         ("/", 2, 6),
                         ("%", 2, 8),
                         ("^", 2, 10),
                         ("&", 2, 12),
                         ("|", 2, 14),
                         ("<<", 2, 16),
                         (">>", 2, 19),

                         ("+=", 3, 0),
                         ("-=", 3, 3),
                         ("*=", 3, 6),
                         ("%=", 3, 9),
                         ("^=", 3, 12),
                         ("&=", 3, 15),
                         ("|=", 3, 18),
                         ("<<=", 3, 21),
                         (">>=", 3, 25),

                         ("@", 4, 0),
                         (".", 4, 2),
                         ("..", 4, 4),
                         ("...", 4, 7),
                         (",", 4, 11),
                         (";", 4, 13),
                         (":", 4, 15),
                         ("::", 4, 17),
                         ("->", 4, 20),
                         ("<-", 4, 23),
                         ("=>", 4, 26),
                         ("#", 4, 29),
                         ("$", 4, 31),
                         ("?", 4, 33),

                         ("(", 5, 0),
                         ("[", 5, 2),
                         ("{", 5, 4),
                         ("}", 5, 6),
                         ("]", 5, 8),
                         (")", 5, 10),

                         ("/*! foo */", 7, 0),
                         ("//! foo", 8, 0),
                         ("/** foo */", 9, 0),
                         ("/// foo", 10, 0),

                         ("b'a'", 12, 0),
                         ("'b'", 13, 0),
                         ("10", 14, 0),
                         ("10u32", 15, 0),
                         ("3.3", 16, 0),
                         ("3.3f32", 17, 0),
                         ("\"apple\"", 18, 0),
                         ("b\"pear\"", 19, 0),
                         ("true", 20, 0),
                         ("false", 21, 0),

                         ("'apples", 23, 0),

                         ("apples", 25, 0),
                         ("_", 26, 0),
                         ("abstract", 28, 0)];

        assert_eq!(&infos[..], &expected[..]);
    }
}
