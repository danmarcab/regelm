# Regelm

Pure elm implementation of regular expressions.

## Quick Start

```elm
> import Regelm

-- Parse a regex
> reg = Regelm.regex "\\d+"
Ok (Regex ...)

-- Test a regex
> Result.map (\r -> Regelm.contains r "abc") reg
Ok False

> Result.map (\r -> Regelm.contains r "123") reg
Ok True
```


## Features

- [x] Check if string matches a regex
- [x] Generate random string starting from a regex
- [ ] Capture matches / submatches
- [ ] Flags (case insensitive...)


### Supported Regular expression syntax

- [x] `^` Matches beggining of string
- [x] `$` Matches beggining of string
- [x] `?` Matches the preceding expression 0 or 1 time
- [x] `*` Matches the preceding expression 0 or more times
- [x] `+` Matches the preceding expression 1 or more times
- [x] `.` Matches any char
- [x] `x|y` Matches either x or y
- [x] `x{n}` Matches x exactly n times
- [x] `x{n,}` Matches x at least n times
- [x] `x{,n}` Matches x at most n times
- [x] `x{n,m}` Matches x between n and m times
- [x] `[xyz]` Matches chars x, y or z
- [x] `[x-z]` Matches chars between x and z
- [x] `[^xyz]` Matches any char but x, y or z
- [x] `[^x-z]` Matches any char but between x and z
- [x] `\d` Matches a digit
- [x] `\D` Matches a non digit
- [x] `\w` Matches a word char
- [x] `\W` Matches a non word char
- [x] `\` Escapes special chars
- [x] `\f` Matches a form feed
- [x] `\n` Matches a newline
- [x] `\r` Matches a carriage return
- [x] `\s` Matches a white space char
- [x] `\S` Matches a non white space char
- [x] `\t` Matches a tab char
- [x] `\v` Matches a vertical tab
- [x] `(?:x)` Matches group and don't capture

### High priority non supported syntax

- [ ] `(x)` Matches and capture

### Low priority non supported syntax

- [ ] `\b` Matches a word boundary
- [ ] `\B` Matches a non-word boundary
- [ ] `[\b]` Matches a backspace (U+0008)
- [ ] `\n` Backreference to tne `nth` match
- [ ] `\0` Matches a NULL (U+0000) character
- [ ] `\xhh` Matches the character with the code hh
- [ ] `\uhhhh` Matches the character with the code hhhh
- [ ] `\cX` Matches the control character X
- [ ] `x(?=y)` Matches x only if followed by y
- [ ] `x(?!y)` Matches x only if not followed by y
