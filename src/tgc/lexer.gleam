import gleam/bool
import gleam/list
import gleam/option
import gleam/result
import gleam/string

pub type LexerContext {
  LexerContext(
    pointer: String,
    rest: String,
    acc: String,
    contents: String,
    row: Int,
    col: Int,
    tokens: List(Token),
  )
}

pub type TokenKind {
  TokenIdentifier(String)
  TokenKeyword(String)
  TokenString(String)
  TokenBraceRight
  TokenBraceLeft
  TokenParenRight
  TokenParenLeft
  TokenColon
  TokenComma
  TokenPeriod
  TokenEquals
  TokenSlash
  TokenSpaceship
  TokenGreaterThan
  TokenLessThan
  TokenGreaterThanEquals
  TokenLessThanEquals
  TokenArrowLeft
  TokenPlus
  TokenMinus
  TokenPipe
  TokenPatternOr
}

pub fn kind_to_string(kind: TokenKind) {
  case kind {
    TokenIdentifier(_) -> "Identifier"
    TokenKeyword(kw) -> "Keyword " <> kw
    TokenString(_) -> "String"
    TokenBraceRight -> "BraceRight"
    TokenBraceLeft -> "BraceLeft"
    TokenParenRight -> "ParenRight"
    TokenParenLeft -> "ParenLeft"
    TokenColon -> "Colon"
    TokenComma -> "Comma"
    TokenPeriod -> "Period"
    TokenEquals -> "Equals"
    TokenSlash -> "Slash"
    TokenSpaceship -> "Spaceship"
    TokenGreaterThan -> "GreaterThan"
    TokenLessThan -> "LessThan"
    TokenGreaterThanEquals -> "GreaterThanEquals"
    TokenLessThanEquals -> "LessThanEquals"
    TokenArrowLeft -> "ArrowLeft"
    TokenPlus -> "Plus"
    TokenMinus -> "Minus"
    TokenPipe -> "Pipe"
    TokenPatternOr -> "PatternOr"
  }
}

pub type Token {
  Token(kind: TokenKind, lexeme: String, row: Int, col: Int)
}

pub fn lex(contents: String) {
  let ctx =
    LexerContext(
      pointer: "",
      rest: contents,
      contents:,
      acc: "",
      row: 1,
      col: 0,
      tokens: [],
    )

  do_lex(ctx)
}

fn try_advance(
  ctx: LexerContext,
  cb: fn(LexerContext) -> List(Token),
) -> List(Token) {
  case string.to_graphemes(ctx.rest) {
    [pointer, ..rest] ->
      cb(
        LexerContext(
          ..ctx,
          row: ctx.row,
          col: ctx.col + 1,
          pointer: pointer,
          rest: string.join(rest, ""),
          contents: ctx.contents,
        ),
      )
    _ -> list.reverse(ctx.tokens)
  }
}

fn advance(ctx: LexerContext) -> LexerContext {
  case string.to_graphemes(ctx.rest) {
    [pointer, ..rest] ->
      LexerContext(
        ..ctx,
        col: ctx.col + 1,
        pointer:,
        rest: string.join(rest, ""),
        contents: ctx.contents,
      )
    _ -> panic as "advance was called without a valid coming token"
  }
}

fn pop_identifier(ctx: LexerContext) {
  LexerContext(..ctx, acc: "", tokens: [
    Token(
      kind: case ctx.acc {
        "fn" | "case" | "pub" | "use" | "import" -> TokenKeyword(ctx.acc)
        _ -> TokenIdentifier(ctx.acc)
      },
      lexeme: ctx.acc,
      row: ctx.row,
      col: ctx.col - string.length(ctx.acc) - 1,
    ),
    ..ctx.tokens
  ])
}

fn pop_id_if_non_empty(ctx: LexerContext) {
  case ctx.acc == "" {
    True -> ctx
    False -> pop_identifier(ctx)
  }
}

fn add_token(ctx: LexerContext, token: TokenKind, lexeme: String) {
  LexerContext(..ctx, tokens: [
    Token(
      kind: token,
      lexeme:,
      row: ctx.row,
      col: ctx.col - string.length(lexeme) + 1,
    ),
    ..ctx.tokens
  ])
}

fn peek(ctx: LexerContext) {
  case string.to_graphemes(ctx.rest) {
    [peek, ..] -> option.Some(peek)
    _ -> option.None
  }
}

fn lex_string(ctx: LexerContext, acc: String) {
  case string.to_graphemes(ctx.rest) {
    [char, ..rest] ->
      case char {
        "\"" -> {
          let ctx = add_token(ctx, TokenString(acc), "\"" <> acc <> "\"")
          LexerContext(..ctx, rest: string.join(rest, ""), col: ctx.col)
        }
        _ ->
          lex_string(
            LexerContext(..ctx, rest: string.join(rest, ""), col: ctx.col + 1),
            acc <> char,
          )
      }
    [] -> panic as "string did not end with a \""
  }
}

fn do_lex(ctx: LexerContext) {
  use ctx <- try_advance(ctx)

  case ctx.pointer {
    "\n" -> {
      let ctx = pop_id_if_non_empty(ctx)
      do_lex(LexerContext(..ctx, row: ctx.row + 1, col: 0))
    }
    " " -> {
      pop_id_if_non_empty(ctx)
      |> do_lex()
    }
    "(" -> {
      pop_id_if_non_empty(ctx)
      |> add_token(TokenParenLeft, "(")
      |> do_lex()
    }
    ")" -> {
      pop_id_if_non_empty(ctx)
      |> add_token(TokenParenRight, ")")
      |> do_lex()
    }
    "." -> {
      pop_id_if_non_empty(ctx)
      |> add_token(TokenPeriod, ".")
      |> do_lex()
    }
    "{" -> {
      pop_id_if_non_empty(ctx)
      |> add_token(TokenBraceLeft, "{")
      |> do_lex()
    }
    "}" -> {
      pop_id_if_non_empty(ctx)
      |> add_token(TokenBraceRight, "}")
      |> do_lex()
    }
    "/" -> {
      pop_id_if_non_empty(ctx)
      |> add_token(TokenSlash, "/")
      |> do_lex()
    }
    "=" -> {
      pop_id_if_non_empty(ctx)
      |> add_token(TokenEquals, "=")
      |> do_lex()
    }
    "," -> {
      pop_id_if_non_empty(ctx)
      |> add_token(TokenComma, ",")
      |> do_lex()
    }
    ":" -> {
      pop_id_if_non_empty(ctx)
      |> add_token(TokenColon, ":")
      |> do_lex()
    }
    "<" ->
      case peek(ctx) {
        option.Some(">") ->
          add_token(ctx, TokenSpaceship, "<>") |> advance() |> do_lex()
        option.Some("=") ->
          add_token(ctx, TokenLessThanEquals, "<=") |> advance() |> do_lex()
        option.Some("-") ->
          add_token(ctx, TokenArrowLeft, "<-") |> advance() |> do_lex()
        option.None -> add_token(ctx, TokenLessThan, "<").tokens
        _ -> add_token(ctx, TokenLessThan, "<") |> do_lex()
      }

    ">" ->
      case peek(ctx) {
        option.Some("=") ->
          add_token(ctx, TokenGreaterThanEquals, ">=") |> advance() |> do_lex()
        option.None -> add_token(ctx, TokenLessThan, "<").tokens
        _ -> add_token(ctx, TokenGreaterThan, ">") |> do_lex()
      }

    "|" ->
      case peek(ctx) {
        option.Some(">") ->
          add_token(ctx, TokenPipe, "|>") |> advance() |> do_lex()
        option.None -> add_token(ctx, TokenPatternOr, "|").tokens
        _ -> add_token(ctx, TokenPatternOr, "|") |> do_lex()
      }
    "\"" ->
      pop_id_if_non_empty(ctx)
      |> lex_string("")
      |> do_lex
    _ -> {
      do_lex(LexerContext(..ctx, acc: ctx.acc <> ctx.pointer))
    }
  }
}
