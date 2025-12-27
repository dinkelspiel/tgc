import comparator
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import tgc/lexer

pub type AstKind {
  AstImport(path: List(String))
  AstFuncDef(
    public: Bool,
    name: String,
    args: List(#(String, String)),
    body: List(AstNode),
  )
  AstFuncCall(
    module: option.Option(String),
    name: String,
    args: List(#(option.Option(String), lexer.Token)),
  )
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
    token: lexer.Token(lexer.TokenIdentifier(""), "", 0, 0),
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
    int.to_string(token.row + 1) |> string.pad_start(2, "")
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
        lexer.TokenIdentifier(_) -> {
          case rest {
            [token2, ..rest2] ->
              case token2.kind {
                lexer.TokenSlash ->
                  parse_root_import(
                    ParserContext(..ctx, rest: rest2, token: token2),
                    [token.lexeme, ..acc],
                    root_token,
                  )
                _ ->
                  ParserContext(..ctx, token: token2, rest:, ast: [
                    AstNode(
                      kind: AstImport([token.lexeme, ..acc] |> list.reverse),
                      col: root_token.col,
                      row: root_token.row,
                    ),
                    ..ctx.ast
                  ])
              }
            _ -> error_unexpected_eof(ctx, token)
          }
        }
        _ -> error_invalid_token(ctx, token, [lexer.TokenIdentifier("")])
      }
    _ -> error_unexpected_eof(ctx, ctx.token)
  }
}

fn assert_advance(ctx: ParserContext, kind: lexer.TokenKind) {
  case ctx.rest {
    [token, ..rest] -> {
      echo "a"
      echo kind
      echo token.kind
      echo comparator.is_same_kind(token.kind, kind)
      case comparator.is_same_kind(token.kind, kind) {
        True -> #(ParserContext(..ctx, token:, rest:), token)
        False -> error_invalid_token(ctx, token, [kind])
      }
    }
    _ -> error_unexpected_eof(ctx, ctx.token)
  }
}

fn assert_token(ctx: ParserContext, token: lexer.Token, kind: lexer.TokenKind) {
  case comparator.is_same_kind(token.kind, kind) {
    True -> ParserContext(..ctx, token:)
    False -> error_invalid_token(ctx, token, [kind])
  }
}

fn advance(ctx: ParserContext) {
  case ctx.rest {
    [token, ..rest] -> #(ParserContext(..ctx, token:, rest:), token)
    _ -> error_unexpected_eof(ctx, ctx.token)
  }
}

fn peek(ctx: ParserContext) {
  case ctx.rest {
    [token, ..] -> option.Some(token)
    _ -> option.None
  }
}

fn parse_type_args(ctx: ParserContext, args: List(#(String, String))) {
  let #(ctx, label) = advance(ctx)

  case label.kind {
    lexer.TokenParenRight -> #(ctx, args)

    _ -> {
      let #(ctx, _) = assert_advance(ctx, lexer.TokenColon)

      let next = peek(ctx)
      case next {
        option.Some(next) ->
          case next.kind {
            lexer.TokenIdentifier(_) -> {
              let #(ctx, arg_type) = advance(ctx)
              parse_type_args(ctx, [#(label.lexeme, arg_type.lexeme), ..args])
            }
            lexer.TokenComma -> {
              todo
            }
            lexer.TokenParenRight -> {
              todo
            }
            _ ->
              error_invalid_token(ctx, next, [
                lexer.TokenIdentifier(""),
                lexer.TokenComma,
                lexer.TokenParenRight,
              ])
          }
        option.None -> error_unexpected_eof(ctx, ctx.token)
      }
    }
  }
}

fn parse_value_args(
  ctx: ParserContext,
  args: List(#(option.Option(String), lexer.Token)),
) {
  let #(ctx, value) = advance(ctx)
  let #(ctx, next) = advance(ctx)

  case next.kind {
    lexer.TokenComma -> parse_value_args(ctx, [#(option.None, value), ..args])
    lexer.TokenParenRight -> #(ctx, [#(option.None, value), ..args])
    _ ->
      error_invalid_token(ctx, next, [
        lexer.TokenComma,
        lexer.TokenParenRight,
      ])
  }
}

fn parse_block(ctx: ParserContext, body: List(AstNode)) {
  let #(ctx, token1) = advance(ctx)
  case token1.kind {
    lexer.TokenIdentifier(_) -> {
      let #(ctx, token2) = advance(ctx)
      case token2.kind {
        lexer.TokenPeriod -> {
          let module = token1
          let #(ctx, name) = assert_advance(ctx, lexer.TokenIdentifier(""))
          let #(ctx, _) = assert_advance(ctx, lexer.TokenParenLeft)
          let #(ctx, args) = parse_value_args(ctx, [])

          parse_block(ctx, [
            AstNode(
              kind: AstFuncCall(
                module: option.Some(module.lexeme),
                name: name.lexeme,
                args:,
              ),
              col: module.col,
              row: module.row,
            ),
            ..body
          ])
        }
        lexer.TokenParenLeft -> {
          let name = token1
          todo
        }
        _ ->
          error_invalid_token(ctx, token2, [
            lexer.TokenPeriod,
            lexer.TokenParenLeft,
          ])
      }
    }
    lexer.TokenBraceRight -> #(ctx, body)
    _ -> error_invalid_token(ctx, ctx.token, [lexer.TokenIdentifier("")])
  }
}

fn parse_root_fn(ctx: ParserContext, public: Bool) {
  let #(ctx, name) = advance(ctx)
  let #(ctx, _) = assert_advance(ctx, lexer.TokenParenLeft)
  let #(ctx, args) =
    parse_type_args(ctx, [])
    |> echo
  let #(ctx, _) = assert_advance(ctx, lexer.TokenBraceLeft)
  let #(ctx, body) = parse_block(ctx, [])

  ParserContext(..ctx, ast: [
    AstNode(
      kind: AstFuncDef(public:, name: name.lexeme, args:, body:),
      col: name.col,
      row: name.row,
    ),
    ..ctx.ast
  ])
}

fn parse_root(ctx: ParserContext) {
  echo ctx.ast

  case ctx.rest {
    [token, ..rest] ->
      case token.kind {
        lexer.TokenKeyword("import") ->
          parse_root_import(ParserContext(..ctx, token:, rest:), [], token)
          |> parse_root
        lexer.TokenKeyword("pub") ->
          case rest {
            [token, ..rest] ->
              case token.kind {
                lexer.TokenKeyword("fn") -> {
                  parse_root_fn(ParserContext(..ctx, token:, rest:), True)
                  |> parse_root
                }
                _ -> error_invalid_token(ctx, token, [lexer.TokenKeyword("")])
              }
            _ -> error_unexpected_eof(ctx, token)
          }
        _ ->
          error(
            ctx,
            token,
            "unimplemented token type in root "
              <> lexer.kind_to_string(token.kind),
          )
      }
    _ -> list.reverse(ctx.ast)
  }
}
