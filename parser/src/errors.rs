use crate::string_utils::StringPositions;
use crate::{
    Location,
    ParseError,
    ParseErrorInfo,
};

/// Format a verbose error into a debug trace message.
///
/// Inspired by nom internals:
/// https://github.com/Geal/nom/blob/c326e077b83c62f81b717c80a281cb453cb914e7/src/error.rs#L141
pub fn format_debug_error(input: &str, err: ParseError) -> String {
    use std::iter::repeat;

    let mut string_positions = StringPositions::new(input);
    let lines: Vec<_> = input.lines().map(String::from).collect();

    let mut result = String::new();

    match err {
        ParseError::One(ParseErrorInfo { msg, loc }) => {
            let pos = match loc {
                Location::Eof => string_positions.get_last().unwrap(),
                Location::Span(span) => match string_positions.get_pos(span.start) {
                    Some(pos) => pos,
                    None => string_positions.get_last().unwrap(),
                },
            };
            result += &format!("at line {} col {}, {}:\n", pos.line, pos.col, msg);

            result += &lines[pos.line - 1];
            result += "\n";
            if pos.col > 0 {
                result += &repeat(' ').take(pos.col).collect::<String>();
            }
            result += "^\n\n";
        }
        ParseError::Many(infos) => {
            for (i, info) in infos.iter().enumerate() {
                let msg = &info.msg;
                let loc = info.loc;

                let pos = match loc {
                    Location::Eof => string_positions.get_last().unwrap(),
                    Location::Span(span) => match string_positions.get_pos(span.start) {
                        Some(pos) => pos,
                        None => string_positions.get_last().unwrap(),
                    },
                };
                result += &format!("{}: at line {} col {}, {}:\n", i, pos.line, pos.col, msg);

                result += &lines[pos.line - 1];
                result += "\n";
                if pos.col > 0 {
                    result += &repeat(' ').take(pos.col).collect::<String>();
                }
                result += "^\n\n";
            }
        }
    }

    result
}
