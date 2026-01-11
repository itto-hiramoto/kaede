use std::collections::VecDeque;

use kaede_span::{file::FilePath, Span};

use crate::token::{Token, TokenKind};

/// The rules are based on the Go language, thank you very much!
///
/// `Newline` token will be removed
pub fn insert_semi(tokens: impl Iterator<Item = Token>, file: FilePath) -> VecDeque<Token> {
    let mut result = VecDeque::<Token>::new();

    for tok in tokens {
        match tok.kind {
            TokenKind::NewLine | TokenKind::Eoi => {
                if let Some(last) = result.back() {
                    if is_semi_insertable(&last.kind) {
                        let start = last.span.finish;
                        let mut finish = start;
                        finish.increase_column();

                        result.push_back(Token {
                            kind: TokenKind::Semi,
                            span: Span::new(start, finish, file),
                        });
                    }
                }

                // Eoi not discarded
                if tok.kind == TokenKind::Eoi {
                    result.push_back(tok);
                }
            }

            _ => result.push_back(tok),
        }
    }

    result
}

fn is_semi_insertable(token: &TokenKind) -> bool {
    use TokenKind::*;

    matches!(
        token,
        Int(_)
            | Ident(_)
            | Self_
            | StringLiteral(_)
            | ByteStringLiteral(_)
            | ByteLiteral(_)
            | CharLiteral(_)
            | CloseParen
            | CloseBrace
            | CloseBracket
            | Return
            | True
            | False
    )
}
