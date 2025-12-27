import gleam/io
import gleam/list
import gleam/option
import gleam/string
import tgc/lexer
import tgc/parser

fn value_args(
  args: List(#(option.Option(String), lexer.Token)),
  out: List(String),
) {
  case args {
    [arg, ..rest] -> {
      value_args(rest, [
        case { arg.1 }.kind {
          lexer.TokenIdentifier(value) -> value
          lexer.TokenString(value) -> "\"" <> value <> "\""
          _ -> panic as "invalid token"
        },
        ..out
      ])
    }
    [] -> string.join(out, ", ")
  }
}

pub fn codegen(ast: List(parser.AstNode), out: String) {
  case ast {
    [node, ..rest] ->
      case node.kind {
        parser.AstImport(path:) ->
          codegen(
            rest,
            out <> "#include \"" <> string.join(path, ".") <> ".h\"\n",
          )

        parser.AstFuncDef(public: _, name:, args:, body:) ->
          codegen(
            rest,
            out <> "void " <> name <> "() {\n" <> codegen(body, "") <> "}",
          )
        parser.AstFuncCall(module:, name:, args:) -> {
          let temp_out = case module {
            option.Some(module) -> module <> "_"
            option.None -> ""
          }

          codegen(
            rest,
            temp_out <> name <> "(" <> value_args(args, []) <> ");\n",
          )
        }
      }
    [] -> out
  }
}
