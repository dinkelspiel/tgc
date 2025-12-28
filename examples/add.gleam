import gleam/io

fn add(a: Int, b: Int) {
    a + b
}

pub fn main() {
  io.println(int.to_string(add(2, 2)))
}
