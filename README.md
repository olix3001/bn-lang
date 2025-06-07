# BN (Bean) Programming language
BN is a prototype programming language that compiles to javascript!

It is _NOT_ intended to be a fully featured programming language as I made it only
to try out some libraries and implementation details for language development.

> [!IMPORTANT]
> This project uses unstable rust features like `try_impl_v2`, thus requiring nightly rust toolchain to compile.

## Syntax
Here is an example of a simple program in this programming language:
```
// This tells the compiler to just assume that
// variable 'console' exists globally.
#[external] const console;

module math {
  export fn add(a, b) => a + b
  export fn sub(a, b) => a - b
}

module utils {
  const GREET_PREFIX = "Hello, ";
  const GREET_SUFFIX = "!";
  // Standard methods with fn name() { } are supported too!
  // But this is a shorthand for fn name() { return expr; }
  export fn combine(text, prefix: GREET_PREFIX, suffix: GREET_SUFFIX) => prefix + text + suffix
}

// Some examples:
import math::{add, sub};
let a = add(sub(3, 1), 2); // a = 4

console.log(utils::combine(
  "User",
  suffix: "!!!"
));
```
