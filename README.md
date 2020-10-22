# ever-to-excel

Compile programs to spreadsheets.

## Example

### Input

The input language is called Ever.
It's a restricted variant of Scheme.

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

Ever compiles to instructions for a stack VM.

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

<a href="https://raw.githubusercontent.com/ziman/ever-to-excel/master/screenshot.png">
	<img src="https://raw.githubusercontent.com/ziman/ever-to-excel/master/screenshot.png" width="300" />
</a>

## Some details

The input language, Ever, looks like Scheme but it's restricted even more than C.
* You cannot have local variables (there's no `let`).
* You cannot access the parent scope.
* There is no heap, no dynamic allocation, no closures.

All of the above can be addressed; it just needs more work. :)

However:
* You can have (mutual) recursion.
* Tail recursion is supported (at the moment only for tail calls with the same number of arguments).
* There are some spreadsheet-specific optimisations,
  e.g. compiling expressions directly to spreadsheet expressions
  rather than into multiple instructions, whenever possible.

More details:
* Every row in the spreadsheet is the same. (Except for the initial row,
  which does not contain any formulas.)
* We could make every _cell_ the same but we use the static knowledge of the column
  number to make some optimisations.
* The compiler runs the program in the interpreter first
  to determine the minimal width and height of the spreadsheet.
  This is not cheating; it's fine to generate more cells than necessary.
