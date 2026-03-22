use std::path::PathBuf;

use anyhow::Result;
use kaede_ast::expr::ExprKind;
use kaede_parse::Parser;
use kaede_span::file::FilePath;

#[test]
fn parse_channel_recv() -> Result<()> {
    let mut parser = Parser::new("<-ch", FilePath::from(PathBuf::from("test.kd")));
    let expr = parser.expr()?;

    let recv = match expr.kind {
        ExprKind::ChannelRecv(recv) => recv,
        other => panic!("expected channel recv, got {other:?}"),
    };

    assert!(matches!(recv.channel.kind, ExprKind::Ident(_)));
    Ok(())
}

#[test]
fn parse_channel_send_rhs_uses_full_expression() -> Result<()> {
    let mut parser = Parser::new("ch <- 1 + 2 * 3", FilePath::from(PathBuf::from("test.kd")));
    let expr = parser.expr()?;

    let send = match expr.kind {
        ExprKind::ChannelSend(send) => send,
        other => panic!("expected channel send, got {other:?}"),
    };

    assert!(matches!(send.channel.kind, ExprKind::Ident(_)));
    assert!(matches!(send.value.kind, ExprKind::Binary(_)));
    Ok(())
}

#[test]
fn parse_channel_recv_in_call_argument() -> Result<()> {
    let mut parser = Parser::new("foo(<-ch)", FilePath::from(PathBuf::from("test.kd")));
    let expr = parser.expr()?;

    let call = match expr.kind {
        ExprKind::FnCall(call) => call,
        other => panic!("expected fn call, got {other:?}"),
    };

    let arg = call.args.args.front().expect("call arg");
    assert!(matches!(arg.value.kind, ExprKind::ChannelRecv(_)));
    Ok(())
}
