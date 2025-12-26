import gleam/string
import simplifile
import tgc/lexer
import tgc/parser

pub fn main() -> Nil {
  let assert Ok(c) = simplifile.read("./examples/hello_world.gleam")
  { echo lexer.lex(c) } |> parser.parse(string.split(c, "\n"))

  Nil
}
