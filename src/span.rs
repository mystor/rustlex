use memchr::memchr;

/// A region of the source text. This is used to identify where in the source
/// text an individual token came from.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Span {
    /// The byte offset of the first character of the token.
    pub lo: usize,
    /// The byte offset of the last character of the token.
    pub hi: usize,
}

/// A node which has an associated span.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

/// Translate a byte offset into the line/column information given a specific
/// source string.
pub fn offset_line_col(src: &str, offset: usize) -> (usize, usize) {
    assert!(offset < src.len());
    let mut line = 1;
    let mut prev = 0;
    while let Some(len) = memchr(b'\n', &src[prev..].as_bytes()) {
        let new = prev + len + 1;
        if new > offset {
            break;
        }
        prev = new;
        line += 1;
    }
    (line, offset - prev)
}
