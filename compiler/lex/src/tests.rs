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
