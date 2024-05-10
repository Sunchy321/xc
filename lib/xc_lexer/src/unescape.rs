use std::{char::from_u32, ops::Range, str::Chars};

pub enum EscapeError {
    LoneBackslash,
    InvalidEscape,
    BareCarriageReturn,

    NoBraceInUnicodeEscape,
    UnclosedUnicodeEscape,
    EmptyUnicodeEscape,
    InvalidCharInUnicodeEscape,
    OverlongUnicodeEscape,
    OutOfRangeUnicodeEscape,
    LoneSurrogateUnicodeEscape,
}

pub fn scan_escape(chars: &mut Chars<'_>) -> Result<char, EscapeError> {
    let char = match chars.next().ok_or(EscapeError::LoneBackslash)? {
        '0'  => '\0',
        '\'' => '\'',
        '"' => '"',
        '\\' => '\\',
        'a' => '\x07',
        'b' => '\x08',
        'f' => '\x0c',
        'n' => '\n',
        'r' => '\r',
        't' => '\t',
        'v' => '\x0b',
        'u' => return scan_unicode(chars),
        _ => return Err(EscapeError::InvalidEscape),
    };

    Ok(char)
}

pub fn scan_unicode(chars: &mut Chars<'_>) -> Result<char, EscapeError> {
    if chars.next() != Some('{') {
        return Err(EscapeError::NoBraceInUnicodeEscape);
    }

    let mut digit_count = 1;
    let mut value: u32 = match chars.next().ok_or(EscapeError::UnclosedUnicodeEscape)? {
        '}' => return Err(EscapeError::EmptyUnicodeEscape),
        c => c.to_digit(16).ok_or(EscapeError::InvalidCharInUnicodeEscape)?,
    };

    loop {
        match chars.next() {
            None => return Err(EscapeError::UnclosedUnicodeEscape),
            Some('}') => {
                if digit_count > 6 {
                    return Err(EscapeError::OverlongUnicodeEscape);
                }

                break from_u32(value).ok_or(
                    if value > 0x10FFFF {
                        EscapeError::OutOfRangeUnicodeEscape
                    } else {
                        EscapeError::LoneSurrogateUnicodeEscape
                    }
                )
            },

            Some(c) => {
                let digit = c.to_digit(16).ok_or(EscapeError::InvalidCharInUnicodeEscape)?;

                digit_count += 1;

                if digit_count > 6 {
                    continue;
                }

                value = value * 16 + digit;
            }
        }
    }
}

pub fn check_interpolation(chars: &mut Chars<'_>, at_count: Option<u32>) -> Option<EscapeError> {
    None
}

pub fn unescape_string<F>(src: &str, callback: &mut F)
where
    F: FnMut(Range<usize>, Result<char, EscapeError>),
{
    let mut chars = src.chars();

    while let Some(c) = chars.next() {
        let start = src.len() - chars.as_str().len() - c.len_utf8();

        let res = match c {
            '\\' => {
                match chars.clone().next() {
                    Some('(') => {
                        if let Some(err) = check_interpolation(&mut chars, None) {
                            Err(err)
                        } else {
                            Ok('\0')
                        }
                    }
                    _ => scan_escape(&mut chars)
                }
            },

            '\r' => Err(EscapeError::BareCarriageReturn),
            _ => Ok(c),
        };

        let end = src.len() - chars.as_str().len();

        callback(start..end, res);
    }
}

pub fn check_raw_string<F>(src: &str, callback: &mut F)
where
    F: FnMut(Range<usize>, Result<char, EscapeError>),
{
    let mut chars = src.chars();

    while let Some(c) = chars.next() {
        let start = src.len() - chars.as_str().len() - c.len_utf8();

        let res = match c {
            '\r' => Err(EscapeError::BareCarriageReturn),

            _ => Ok(c),
        };

        let end = src.len() - chars.as_str().len();

        callback(start..end, res);
    }
}
