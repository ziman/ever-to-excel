# scheme-xls

Compile Scheme(*) programs to spreadsheets.

(*) The current input language is actually much simpler than Scheme.

## Example

### Input

```scheme
(define (fac acc n)
  (if-zero
    n
    acc
    (fac (* n acc) (- n 1))))

(define (main)
  (display
    (fac 1 6)))
```

### Output

<a href="https://raw.githubusercontent.com/ziman/scheme-xls/master/screenshot.png">
	<img src="https://raw.githubusercontent.com/ziman/scheme-xls/master/screenshot.png" width="300" />
</a>

## Some details

The input language looks like Scheme but it's restricted even more than C.
It can barely express the factorial function.
* You cannot have local variables (there's no `let`).
* You cannot access the parent scope.

However:
* You can have (mutual) recursion.
* Tail recursion is supported (at the moment only for tail calls with the same number of arguments).
* There some spreadsheet-specific optimisations,
  e.g. compiling expressions directly to spreadsheet expressions
  rather than into multiple instructions, whenever possible.
