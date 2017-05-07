use std::error::Error;
use std::fmt::{self, Display};

macro_rules! fmt_err {
    ($lexer:expr, $($rest:expr),*) => {
        return {
            let (offset, line, col) = $lexer.err_info();
            Err($crate::error::LexError {
                reason: format!($($rest),*),
                offset: offset,
                line: line,
                col: col
            })
        }
    }
}

/// The result type used by `rustlex`.
pub type LexResult<R> = Result<R, LexError>;

/// An error triggered during lexing.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LexError {
    /// A human readable string describing the cause of the error
    pub reason: String,
    /// The byte offset in the input string where the lexing error occurred.
    pub offset: usize,
    /// The line in the input string where the lexing error occurred. 1-indexed.
    pub line: usize,
    /// The column in the input string where the lexing error occurred.
    /// 0-indexed.
    pub col: usize,
}

impl Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Lex Error {}:{}: {}",
               self.line, self.col, self.reason)
    }
}

impl Error for LexError {
    fn description(&self) -> &str {
        &self.reason
    }
}
