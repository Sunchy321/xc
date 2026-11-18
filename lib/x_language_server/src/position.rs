use tower_lsp_server::ls_types::{Position, Range};
use xc_span::Span;

pub(crate) fn byte_offset_to_position(source: &str, offset: usize) -> Position {
    let mut offset = offset.min(source.len());
    while !source.is_char_boundary(offset) {
        offset -= 1;
    }

    let prefix = &source[..offset];
    let line = prefix.bytes().filter(|byte| *byte == b'\n').count();
    let line_start = prefix.rfind('\n').map_or(0, |index| index + 1);
    let mut line_prefix = &source[line_start..offset];
    if line_prefix.ends_with('\r') {
        line_prefix = &line_prefix[..line_prefix.len() - 1];
    }

    Position {
        line: line.min(u32::MAX as usize) as u32,
        character: line_prefix.encode_utf16().count().min(u32::MAX as usize) as u32,
    }
}

pub(crate) fn position_to_byte_offset(source: &str, position: Position) -> usize {
    let mut line_start = 0;
    for _ in 0..position.line {
        match source[line_start..].find('\n') {
            Some(newline) => line_start += newline + 1,
            None => return source.len(),
        }
    }

    let line_end = source[line_start..]
        .find('\n')
        .map_or(source.len(), |newline| line_start + newline);
    let content_end = if line_end > line_start && source.as_bytes()[line_end - 1] == b'\r' {
        line_end - 1
    } else {
        line_end
    };
    let line = &source[line_start..content_end];
    let target = position.character as usize;
    let mut utf16_offset = 0;

    for (byte_offset, character) in line.char_indices() {
        if utf16_offset >= target {
            return line_start + byte_offset;
        }
        let next_offset = utf16_offset + character.len_utf16();
        if target < next_offset {
            return line_start + byte_offset;
        }
        utf16_offset = next_offset;
    }

    content_end
}

pub(crate) fn span_to_range(source: &str, span: Span) -> Range {
    if span.lo.0 == 0 && span.hi.0 == 0 {
        return Range::default();
    }
    Range {
        start: byte_offset_to_position(source, span.lo.0 as usize),
        end: byte_offset_to_position(source, span.hi.0 as usize),
    }
}
