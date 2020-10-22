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

### Bytecode

```
; -------[ main ] -------------
OP 0 (XStr "ret")
OP 0 (XInt 1)
OP 0 (XInt 6)
LOAD (Addr 3)
PUSHL (PC 8)
LOAD (Addr 2)
STORE (Addr 3)
JMP (PC 13)
LABEL (PC 8)
STORE (Addr 3)
POP 2
PRINT
HALT
; -------[ fac ] --------------
LABEL (PC 13)
OP 0 (XLoc 2)
JZ (PC 22)
OP 0 (XOp "*" (XLoc 2) (XLoc 3))
OP 0 (XOp "-" (XLoc 2) (XInt 1))
LSTORE 2
LSTORE 3
JMP (PC 13)
JMP (PC 24)
LABEL (PC 22)
OP 0 (XLoc 3)
LABEL (PC 24)
LSTORE 4
RET
```

### Output

<a href="https://raw.githubusercontent.com/ziman/scheme-xls/master/screenshot.png">
	<img src="https://raw.githubusercontent.com/ziman/scheme-xls/master/screenshot.png" width="300" />
</a>

## Some details

The input language looks like Scheme but it's restricted even more than C.
* You cannot have local variables (there's no `let`).
* You cannot access the parent scope.

However:
* You can have (mutual) recursion.
* Tail recursion is supported (at the moment only for tail calls with the same number of arguments).
* There are some spreadsheet-specific optimisations,
  e.g. compiling expressions directly to spreadsheet expressions
  rather than into multiple instructions, whenever possible.
