# monkey.zig

## Summury

We can learn how to make an interpreter in this book.  
====> ☆☆☆ **["Writing An Interpreter in Go"](https://interpreterbook.com/)** ☆☆☆  
That interpreter is called Monkey in the book.
The Monkey is written in Go in the book, but in this repository it is written in Zig.

## Using Zig Version

Zig: v0.9.1

## Supports

- [x] Lexer
- [x] Parser
- [x] Evaluator
- [x] REPL
- [x] Test case
- [ ] Macro
- [ ] Compiler
- [ ] VM

## Example

### REPL

```sh
$ zig build run
>> let a = 5
null
>> a
5
>> a + 10
15
>> let add = fn(x, y) { x + y }
null
>> add(10, 20)
30
>> let closure = fn(x) { a + x }
null
>> closure(10)
15
>> let new_closure = fn(a) { fn() { a; }; };
null
>> let closure = new_closure(99);
null
>> closure();
99
```

### Fibonacchi

```monkey
let fibonacci = fn(x) {
  if (x == 0) {
    return 0;
  } else {
    if (x == 1) {
      return 1;
    } else {
      fibonacci(x - 1) + fibonacci(x - 2);
    }
  }
};
fibonacci(15); #=> 610
```

## Contributors

- [ktanaka101](https://github.com/ktanaka101) - creator, maintainer

## License

MIT
