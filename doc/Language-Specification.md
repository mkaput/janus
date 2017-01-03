The Janus Language Specification
================================

# Introduction

Janus is a simple, dynamically and strongly typed, interpreted language aimed at
being as easy as possible to implement.

This document is the primary reference for the Janus programming language
grammar and semantics.

This document has been greatly inspired by and based on
[The Rust Reference](https://doc.rust-lang.org/reference.html) and
[Rust Grammar](https://doc.rust-lang.org/grammar.html) (including ordinary
copy-pasting tedious paragraphs).

# Notation

Janus' grammar is defined over Unicode codepoints, each conventionally denoted
`U+XXXX`, for 4 or more hexadecimal digits `X`. _Most_ of Janus' grammar is
confined to the ASCII range of Unicode, and is described in this document by a
dialect of Extended Backus-Naur Form (EBNF) which can be defined
self-referentially as follows:

```antlr
grammar        : rule+
rule           : nonterminal ':' productionrule
productionrule : production [ '|' production ]*
production     : term+
term           : element repeats
element        : LITERAL | IDENTIFIER | '[' productionrule ']'
repeats        : [ '*' | '+' ] NUMBER? | NUMBER? | '?'
```

Where:

- Whitespace in the grammar is ignored.
- Square brackets are used to group rules.
- `LITERAL` is a single printable ASCII character, or an escaped hexadecimal
  ASCII code of the form `\xQQ`, in single quotes, denoting the corresponding
  Unicode codepoint `U+00QQ`.
- `IDENTIFIER` is a nonempty string of ASCII letters and underscores.
- The `repeats` forms apply to the adjacent `element`, and are as follows:
  - `?` means zero or one repetition
  - `*` means zero or more repetitions
  - `+` means one or more repetitions
  - NUMBER trailing a repeat symbol gives a maximum repetition count
  - NUMBER on its own gives an exact repetition count

This EBNF dialect should hopefully be familiar to many readers.

## Unicode productions

A few productions in Janus' grammar permit Unicode codepoints outside the ASCII
range. We define these productions in terms of character properties specified
in the Unicode standard, rather than in terms of ASCII-range codepoints. The
section [Special Unicode Productions](#special-unicode-productions) lists these
productions.

## String table productions

Some rules in the grammar &mdash; notably [unary
operators](#unary-operator-expressions), [binary
operators](#binary-operator-expressions), and [keywords](#keywords) &mdash; are
given in a simplified form: as a listing of a table of unquoted, printable
whitespace-separated strings. These cases form a subset of the rules regarding
the [token](#tokens) rule, and are assumed to be the result of a
lexical-analysis phase feeding the parser, driven by a DFA, operating over the
disjunction of all such string table entries.

When such a string enclosed in double-quotes (`"`) occurs inside the grammar,
it is an implicit reference to a single member of such a string table
production. See [tokens](#tokens) for more information.

# Lexical structure

## Input format

Janus input is interpreted as a sequence of Unicode codepoints encoded in UTF-8.
Most Janus grammar rules are defined in terms of printable ASCII-range
codepoints, but a small number are defined in terms of Unicode properties or
explicit codepoint lists.

## Special Unicode Productions

The following productions in the Janus grammar are defined in terms of Unicode
properties: `ident`, `non_null`, `non_eol`, `non_single_quote` and
`non_double_quote`.

### Identifiers

The `ident` production is any nonempty Unicode string of the following form:

- The first character has property `XID_start`
- The remaining characters have property `XID_continue`

that does _not_ occur in the set of [keywords](#keywords).

> **Note**: `XID_start` and `XID_continue` as character properties cover the
> character ranges used to form the more familiar C and Java language-family
> identifiers.

### Delimiter-restricted productions

Some productions are defined by exclusion of particular Unicode characters:

- `non_null` is any single Unicode character aside from `U+0000` (null)
- `non_eol` is `non_null` restricted to exclude `U+000A` (`'\n'`)
- `non_single_quote` is `non_null` restricted to exclude `U+0027`  (`'`)
- `non_double_quote` is `non_null` restricted to exclude `U+0022` (`"`)

## Miscellaneous productions

These productions do not have any special Janus grammar meaning, but are
defined in order to simplify definitions of more sophisticated productions.

```antlr
hex_digit   : 'a' | 'b' | 'c' | 'd' | 'e' | 'f'
            | 'A' | 'B' | 'C' | 'D' | 'E' | 'F'
            | dec_digit
oct_digit   : '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7'
dec_digit   : '0' | nonzero_dec
nonzero_dec : '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
```

## Comments

```antlr
comment            : block_comment | line_comment
block_comment      : "/*" block_comment_body "*/"
block_comment_body : [ block_comment | character ]*
line_comment       : "//" non_eol*
```

## Whitespace

```antlr
whitespace_char : '\x20' | '\x09' | '\x0a' | '\x0d'
whitespace      : [ whitespace_char | comment ]+
```

## Tokens

```antlr
token : [ keyword | op | ident | literal | symbol ] whitespace
```

### Keywords

|           |           |           |           |           |
|-----------|-----------|-----------|-----------|-----------|
| and       | as        | break     | case      | class     |
| const     | continue  | do        | else      | enum      |
| False     | fn        | for       | if        | in        |
| let       | loop      | mod       | or        | return    |
| trait     | True      | type      | while     | yield     |


Keywords are case-sensitive. Each of these has special meaning in its grammar,
and all of them are excluded from the `ident` rule.

Not all of these keywords are used by the language as of now. Some of them were
reserved to make space for possible future features.

### Literals

```antlr
literal : string_lit | char_lit | num_lit | bool_lit | unit_lit
```

#### Character and string literals

```antlr
char_lit   : '\x27' char_body '\x27'
string_lit : '"' string_body* '"'

char_body : non_single_quote
          | '\x5c' [ '\x27' | common_escape ]

string_body : non_double_quote
            | '\x5c' [ '"' | common_escape ]

common_escape  : '\x5c' | 'n' | 'r' | 't' | '0'
               | 'x' hex_digit 2
               | 'u' '{' hex_digit+ 6 '}'
```

#### Number literals

```antlr
num_lit : nonzero_dec [ dec_digit | '_' ]* float_suffix?
        | '0' [       [ dec_digit | '_' ]* float_suffix?
              | 'b'   [ '1' | '0' | '_' ]+
              | 'o'   [ oct_digit | '_' ]+
              | 'x'   [ hex_digit | '_' ]+  ]

float_suffix : exponent | '.' dec_lit exponent?

exponent : ['E' | 'e' ] [ '-' | '+' ]? dec_lit

dec_lit : [ dec_digit | '_' ]+
```

#### Boolean literals

```antlr
bool_lit : "True" | "False"
```

The two values of the boolean type are written `True` and `False`.

#### Unit literal

```antlr
unit_lit : "()"
```

### Symbols

```antlr
symbol : '[' | ']' | '(' | ')' | '{' | '}' | ',' | ';'
```

Symbols are a general class of printable [tokens](#tokens) that play structural
roles in a variety of grammar productions. They are cataloged here for
completeness as the set of remaining miscellaneous printable tokens that do not
otherwise appear as [operators](#operators) or [keywords](#keywords).

# Language grammar

The entry rule of Janus source file is called `program`.

```antlr
program : whitespace? stmt*
```

## Statements

```antlr
stmt      : decl_stmt | subst_stmt | expr_stmt | ";"
decl_stmt : let_decl ";" | item ";"?
expr_stmt : expr ";"
```

### Variable bindings

```antlr
let_decl : "let" ident "=" expr
```

Variable binding introduces new subscope with new variable. This prevents
leaking variables before their declaration and helps programmer prevent
unexpected variable value changes (though the latter can be mitigated with
[Substitution statements](#substitution-statements)).

### Substitution statements

```antlr
subst_stmt : lvalue ":=" expr ";"
```

## Items

```antlr
item : fn_item
```

### Functions

```antlr
fn_item   : "fn" ident "(" fn_params? ")" block
fn_params : ident [ "," ident ]*
```

### Blocks

A block is a sequence of statements, possibly ending with an expression.
The return value of the block is the value of the last expression
statement, or `()` otherwise.

```antlr
block      : "{" stmt* "}"
```

## Lvalues

Lvalue is a reference to something in memory (either variable or item).

```antlr
lvalue   : index_lv | path
index_lv : path "[" expr "]"
path     : ident
```

## Expressions

```antlr
expr : literal_expr
     | block_expr
     | op_expr
     | if_expr
     | while_expr
     | loop_expr
     | break_expr
     | continue_expr
     | return_expr
     | lvalue_expr

literal_expr : literal
block_expr   : block
lvalue_expr  : lvalue
```

### Operators

The special `op_expr` production means unary and binary expression
with operator, and for brevity, it is denoted in this document using
following precedence table:

| Precedence | Operator          | Associativity | Operation |
|------------|-------------------|---------------|---|
| 20         | `(...)`           | n/a           | Grouping |
| 19         | -                 | -             | - |
| 18         | `... ( ... )`     | left-to-right | Function call |
| 17         | `... ++`          | n/a           | Postfix increment |
|            | `... --`          | n/a           | Postfix decrement |
| 16         | `! ...`           | right-to-left | Logical NOT |
|            | `~ ...`           | right-to-left | Bitwise NOT |
|            | `+ ...`           | right-to-left | Unary plus |
|            | `- ...`           | right-to-left | Unary minus |
|            | `++ ...`          | n/a           | Prefix increment |
|            | `-- ...`          | n/a           | Prefix decrement |
| 15         | `... ** ...`      | right-to-left | Exponentation |
| 14         | `... * ...`       | left-to-right | Multiplication |
|            | `... / ...`       | left-to-right | Division |
|            | `... mod ...`     | left-to-right | Remainder |
| 13         | `... + ...`       | left-to-right | Addition |
|            | `... - ...`       | left-to-right | Substraction |
| 12         | `... << ...`      | left-to-right | Bitwise left shift |
|            | `... >> ...`      | left-to-right | Bitwise right shift |
| 11         | `... & ...`       | left-to-right | Bitwise AND |
| 10         | `... ^ ...`       | left-to-right | Bitwise XOR |
| 9          | `... | ...`       | left-to-right | Bitwise OR |
| 8          | `... == ...`      | left-to-right | Equality |
|            | `... != ...`      | left-to-right | Inequality |
|            | `... < ...`       | left-to-right | Less than |
|            | `... > ...`       | left-to-right | Greater than or equal |
|            | `... <= ...`      | left-to-right | Less than or equal |
|            | `... >= ...`      | left-to-right | Greater than or equal |
| 7          | -                 | -             | - |
| 6          | `... and ...`     | left-to-right | Logical AND |
| 5          | `... or ...`      | left-to-right | Logical OR |
| 4          | -                 | -             | - |
| 3          | -                 | -             | - |
| 2          | -                 | -             | - |
| 1          | -                 | -             | - |
| 0          | -                 | -             | - |

### If expressions

```antlr
if_expr : "if" expr
          block
          else_tail?

else_tail : "else" [ if_expr | block ]
```

The return value of the `if-else` expression is either the result
of the *if* block, or the *else* one. If the latter one was not
provided, it evaluates to `()`, e.g.:

```rust
let a = if False { 1234 } // a == ()
```

### While loops

```antlr
while_expr : "while" expr
             block
```

The `while` loop is also similar to constructs in other languages, and it also
always returns `()`.

The `while` loop is syntactic sugar for following snippet:

```rust
loop {
    /* body */
    if /* condition */ { break }
}
```

### Infinite loops

```antlr
loop_expr : "loop" block
```

`loop` always returns `()`.

### Break expressions

```antlr
break_expr : "break"
```

`break` does not evaluate as it performs jump, but technically
it evaluates to `()`.

### Continue expressions

```antlr
continue_expr : "continue"
```

`continue` does not evaluate as it performs jump, but technically
it evaluates to `()`.

### Return expressions

```antlr
return_expr : "return" expr
```

`return` does not evaluate as it performs jump, but technically
it evaluates to `()`.
