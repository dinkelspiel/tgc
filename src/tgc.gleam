import gleam/io
import gleam/string
import simplifile
import tgc/codegen
import tgc/lexer
import tgc/parser

pub fn main() -> Nil {
  let assert Ok(c) = simplifile.read("./examples/add.gleam")
  lexer.lex(c)
  |> echo
  |> parser.parse(string.split(c, "\n"))
  |> echo
  |> codegen.codegen("")
  |> io.println

  Nil
}
