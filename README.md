# Kera

An interpreter for kera language, my first attempt to create an interpreter.

## Running the REPL

Using cargo
```shell
cargo run
```
It can now display correct expression precedence.

TODO:
- [x] Lexer
- [x] Parser
    - [x] Statements
    - [x] Expressions
- [x] Evaluation
- [x] REPL
- [ ] Add more data types

## Example

```
misal lima = 5;
misal sepuluh = 10;

misal tambah = fungsi(x, y) {
    kembalikan x + y;
}

misal hasil = tambah(lima, sepuluh);

jika hasil > 10 {
    kembalikan benar;
} lainnya {
    kembalikan salah;
}
```
