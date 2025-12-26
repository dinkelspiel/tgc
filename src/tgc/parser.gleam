import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import tgc/lexer

pub type AstKind {
  AstImport(path: List(String))
}

pub type AstNode {
  AstNode(kind: AstKind, col: Int, row: Int)
}

pub type ParserContext {
  ParserContext(
    tokens: List(lexer.Token),
    rest: List(lexer.Token),
    token: lexer.Token,
    ast: List(AstNode),
    contents: List(String),
  )
}

pub fn at(in list: List(a), get index: Int) -> Result(a, Nil) {
  case index >= 0 {
    True ->
      list
      |> list.drop(index)
      |> list.first
    False -> Error(Nil)
  }
}

pub fn parse(tokens: List(lexer.Token), contents: List(String)) {
  parse_root(ParserContext(
    tokens:,
    rest: tokens,
    token: lexer.Token(lexer.TokenIdentifier, "", 0, 0),
    ast: [],
    contents:,
  ))
}

fn error_invalid_token(
  ctx: ParserContext,
  token: lexer.Token,
  expected: List(lexer.TokenKind),
) {
  error(
    ctx,
    token,
    "unexpected token, expected "
      <> {
      list.map(expected, fn(e) { lexer.kind_to_string(e) })
      |> string.join(", ")
    }
      <> ", found "
      <> lexer.kind_to_string(token.kind),
  )
}

fn error_unexpected_eof(ctx: ParserContext, token: lexer.Token) {
  error(ctx, token, "unexpected end of file")
}

fn error(ctx: ParserContext, token: lexer.Token, message: String) {
  io.println("error: " <> message)
  io.println("  │")
  io.println(
    int.to_string(token.row) |> string.pad_start(2, "")
    <> " │ "
    <> at(ctx.contents, token.row - 1) |> result.unwrap(""),
  )
  io.println("  │ " <> string.pad_start("^", token.col, " "))
  panic
}

fn parse_root_import(
  ctx: ParserContext,
  acc: List(String),
  root_token: lexer.Token,
) {
  case ctx.rest {
    [token, ..rest] ->
      case token.kind {
        lexer.TokenIdentifier -> {
          case rest {
            [token2, ..rest] ->
              case token2.kind {
                lexer.TokenSlash ->
                  parse_root_import(
                    ParserContext(..ctx, rest:, token: token2),
                    [token.lexeme, ..acc],
                    root_token,
                  )
                lexer.TokenEol ->
                  ParserContext(..ctx, token: token2, rest:, ast: [
                    AstNode(
                      kind: AstImport([token.lexeme, ..acc] |> list.reverse),
                      col: root_token.col,
                      row: root_token.row,
                    ),
                    ..ctx.ast
                  ])
                _ ->
                  error_invalid_token(ctx, token2, [
                    lexer.TokenSlash,
                    lexer.TokenEol,
                  ])
              }
            _ -> error_unexpected_eof(ctx, token)
          }
        }
        _ -> error_invalid_token(ctx, token, [lexer.TokenIdentifier])
      }
    _ -> error_unexpected_eof(ctx, ctx.token)
  }
}

fn parse_root(ctx: ParserContext) {
  echo ctx.ast

  case ctx.rest {
    [token, ..rest] ->
      case token.kind {
        lexer.TokenIdentifier ->
          case token.lexeme {
            "import" ->
              parse_root_import(ParserContext(..ctx, token:, rest:), [], token)
              |> parse_root
            _ -> todo as "unimplemented identifier in root"
          }
        _ -> todo as "unimplemented token type in root"
      }
    _ -> todo as "completed"
  }
}
