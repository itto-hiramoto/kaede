use std::path::PathBuf;

use kaede_span::{Location, Span};

use crate::{
    token::{Token, TokenKind},
    Lexer,
};

use TokenKind::*;

fn without_span(it: impl Iterator<Item = Token>) -> Vec<TokenKind> {
    it.map(|t| t.kind).collect()
}

fn lex_test(program: &str, expect: Vec<TokenKind>) {
    assert_eq!(
        without_span(
            Lexer::new(program, PathBuf::from("test").into())
                .run()
                .into_iter()
        ),
        expect
    );
}

#[test]
fn number() {
    lex_test("4810", vec![Int(4810.to_string()), Semi, Eoi]);
}

#[test]
fn skip_whitespaces() {
    lex_test("  \n  ", vec![Eoi]);

    lex_test("\t\r", vec![Eoi]);
}

#[test]
fn multi_numbers() {
    lex_test(
        "48 10 5 8",
        vec![
            Int(48.to_string()),
            Int(10.to_string()),
            Int(5.to_string()),
            Int(8.to_string()),
            Semi,
            Eoi,
        ],
    );
}

#[test]
fn identifier() {
    lex_test(
        "michael jordan",
        vec![
            Ident("michael".to_string()),
            Ident("jordan".to_string()),
            Semi,
            Eoi,
        ],
    );
}

#[test]
fn punct() {
    lex_test(
        "() , {}",
        vec![
            OpenParen, CloseParen, Comma, OpenBrace, CloseBrace, Semi, Eoi,
        ],
    );
}

#[test]
fn span() {
    let mut r = Lexer::new("\n48 + 10;", PathBuf::from("test").into())
        .run()
        .into_iter();

    r.next();

    let t = r.next().unwrap();

    assert_eq!(t.kind, TokenKind::Plus);
    assert_eq!(
        t.span,
        Span::new(
            Location { line: 2, column: 4 },
            Location { line: 2, column: 5 },
            PathBuf::from("test").into(),
        )
    );
}

#[test]
fn auto_insert_semi() {
    lex_test(
        "48 +\n 10\n",
        vec![Int(48.to_string()), Plus, Int(10.to_string()), Semi, Eoi],
    );

    lex_test("return", vec![Return, Semi, Eoi]);
}

#[test]
fn auto_inserted_semi_span() {
    let mut r = Lexer::new("return", PathBuf::from("test").into())
        .run()
        .into_iter();

    r.next();

    let t = r.next().unwrap();

    assert_eq!(t.kind, TokenKind::Semi);
    assert_eq!(
        t.span,
        Span::new(
            Location { line: 1, column: 7 },
            Location { line: 1, column: 8 },
            PathBuf::from("test").into(),
        )
    );
}

#[test]
fn simple_string_literal() {
    lex_test(
        r#""aeiueoao""#,
        vec![StringLiteral("aeiueoao".to_string()), Semi, Eoi],
    );
}

#[test]
fn unicode_string_literal() {
    lex_test(
        r#""🌹🌼あえいうえおあお🥀🌼""#,
        vec![
            StringLiteral("🌹🌼あえいうえおあお🥀🌼".to_string()),
            Semi,
            Eoi,
        ],
    );
}

#[test]
fn string_literal_escape_sequences() {
    lex_test(
        r#""hello\nworld\t\0""#,
        vec![StringLiteral("hello\nworld\t\0".to_string()), Semi, Eoi],
    );
}

#[test]
fn char_literal_escape_sequences() {
    lex_test(r#"'\n'"#, vec![CharLiteral('\n'), Semi, Eoi]);
    lex_test(r#"'\r'"#, vec![CharLiteral('\r'), Semi, Eoi]);
    lex_test(r#"'\t'"#, vec![CharLiteral('\t'), Semi, Eoi]);
    lex_test(r#"'\0'"#, vec![CharLiteral('\0'), Semi, Eoi]);
    lex_test(r#"'\\'"#, vec![CharLiteral('\\'), Semi, Eoi]);
    lex_test(r#"'\''"#, vec![CharLiteral('\''), Semi, Eoi]);
}

#[test]
fn byte_string_literal() {
    lex_test(
        r#"b"hello""#,
        vec![ByteStringLiteral(b"hello".to_vec()), Semi, Eoi],
    );
}

#[test]
fn byte_string_literal_escape_sequences() {
    lex_test(
        r#"b"line1\nline2\t\"quote\"\\backslash\0null""#,
        vec![
            ByteStringLiteral(b"line1\nline2\t\"quote\"\\backslash\0null".to_vec()),
            Semi,
            Eoi,
        ],
    );
}

#[test]
fn byte_literal() {
    lex_test(r#"b'a'"#, vec![ByteLiteral(b'a'), Semi, Eoi]);
    lex_test(r#"b'Z'"#, vec![ByteLiteral(b'Z'), Semi, Eoi]);
    lex_test(r#"b'0'"#, vec![ByteLiteral(b'0'), Semi, Eoi]);
}

#[test]
fn byte_literal_escape_sequences() {
    lex_test(r#"b'\n'"#, vec![ByteLiteral(b'\n'), Semi, Eoi]);
    lex_test(r#"b'\r'"#, vec![ByteLiteral(b'\r'), Semi, Eoi]);
    lex_test(r#"b'\t'"#, vec![ByteLiteral(b'\t'), Semi, Eoi]);
    lex_test(r#"b'\0'"#, vec![ByteLiteral(b'\0'), Semi, Eoi]);
    lex_test(r#"b'\\'"#, vec![ByteLiteral(b'\\'), Semi, Eoi]);
    lex_test(r#"b'\''"#, vec![ByteLiteral(b'\''), Semi, Eoi]);
}
