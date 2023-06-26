# Kera

An interpreter for kera language, my first attempt to create an interpreter.

## Running the REPL

Using cargo
```shell
cargo run
```
Currently, it can only print the tokens produced by user's input

## Example

```
misal lima = 5;
misal sepuluh = 10;

misal tambah = fungsi(x, y) {
    kembalikan x + y;
}

misal hasil = tambah(lima, sepuluh);

jika (hasil > 10) {
    kembalikan benar;
} lainnya {
    kembalikan salah;
}
```