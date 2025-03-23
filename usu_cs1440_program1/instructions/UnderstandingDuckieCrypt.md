# Understanding DuckieCrypt

DuckieCrypt is an encoding of the first 95 printable ASCII values. ASCII values are discussed in depth in the lesson `lsn/0-ASCIIChars`.

*   A DuckieCrypt message is a space-separated sequence of DuckieCrypt characters
*   A DuckieCrypt character is comprised of two parts: the `[flag]` and the `[character_code]`.
*   There is no space between the `[flag]` and the `[character_code]`.
*   Examples of valid DuckieCrypt characters are `_0`, `^9`, `_19`, `+A4`, and `+C2`.
    - DC character `_0` has a `[flag]` of `_` and a `[character_code]` of `0`.
    - DC character `^9` has a `[flag]` of `^` and a `[character_code]` of `9`.
    - DC character `_19` has a `[flag]` of `_` and a `[character_code]` of `19`.
    - DC character `+A4` has a `[flag]` of `+` and a `[character_code]` of `A4`.
    - DC character `+C2` has a `[flag]` of `+` and a `[character_code]` of `C2`.


"Hello World!" in DuckieCrypt looks like this:

```
^7 _4 _11 _11 _14 +A0 ^22 _14 _17 _11 _3 +A1
```


## DuckieCrypt Flags

DuckieCrypt flags are *always* one character.  The three valid flags are `^`, `_`, and `+`.

*   `^` denotes that the translated character is **uppercase**.
*   `_` denotes that the translated character is **lowercase**.
*   `+` denotes that the translation will result in a special character: one
    that is printable but is not alphabetic.
*   Any other character used as a flag is an invalid DuckieCrypt character.
    If your program encounters an invalid DuckieCrypt character, skip over it
    without printing anything.


## DuckieCrypt Character Codes

A `[character_code]` follows a flag and determines which *ASCII* character to
translate the DuckieCrypt character to.

*   For the lowercase and uppercase characters, the `[character_code]` is an
    integer.
*   For special characters a `[character_code]` is composed of two parts:
    *   an alphabetic character
    *   an integer
*   The alphabetic character determines which of the 3 special character groups
    the DuckieCrypt character translates to, while the integer that follows
    denotes which ASCII character it is.
*   If your program encounters an invalid DuckieCrypt character code, skip over
    it without printing anything.

## The DuckieEncrypter

In the [`demo`](../demo) directory you will find `duckieEncrypter.py`. This file was
provided to assist you in understanding the conversion of plain text to
DuckieCrypt. It is a tool to create DuckieCrypt from plain text. When no arguments are given,
the program will ask the user for text to crypt. When an argument is given, the
DuckieEncrypter will *encrypt* the text file, and output DuckieCrypt to the
screen.

# Character Tables

The following tables are provided to assist you in translating DuckieCrypt back
into plain-text. Just a quick heads up: if you are reading this file in plain
text, you may notice some additional characters in the table below. For
example, so I could display the pipe character, `\|`, I had to include a
backslash before it so the pipe character doesn't get interpreted as markdown
syntax. Each DuckieCrypt character will translate to a singular ASCII
character, and a single ASCII character will translate to a "single"
DuckieCrypt character.


## Uppercase Characters

| Character      | DuckieCrypt Translation
| :------------- | :---------------------
| `A`            | `^0`
| `B`            | `^1`
| `C`            | `^2`
| `D`            | `^3`
| `E`            | `^4`
| `F`            | `^5`
| `G`            | `^6`
| `H`            | `^7`
| `I`            | `^8`
| `J`            | `^9`
| `K`            | `^10`
| `L`            | `^11`
| `M`            | `^12`
| `N`            | `^13`
| `O`            | `^14`
| `P`            | `^15`
| `Q`            | `^16`
| `R`            | `^17`
| `S`            | `^18`
| `T`            | `^19`
| `U`            | `^20`
| `V`            | `^21`
| `W`            | `^22`
| `X`            | `^23`
| `Y`            | `^24`
| `Z`            | `^25`


## Lowercase Characters

| Character      | DuckieCrypt Translation
| :------------- | :---------------------
| `a`            | `_0`
| `b`            | `_1`
| `c`            | `_2`
| `d`            | `_3`
| `e`            | `_4`
| `f`            | `_5`
| `g`            | `_6`
| `h`            | `_7`
| `i`            | `_8`
| `j`            | `_9`
| `k`            | `_10`
| `l`            | `_11`
| `m`            | `_12`
| `n`            | `_13`
| `o`            | `_14`
| `p`            | `_15`
| `q`            | `_16`
| `r`            | `_17`
| `s`            | `_18`
| `t`            | `_19`
| `u`            | `_20`
| `v`            | `_21`
| `w`            | `_22`
| `x`            | `_23`
| `y`            | `_24`
| `z`            | `_25`


## Special Characters

### Group A
| Character      | DuckieCrypt Translation
| :------------- | :---------------------
| ` `            | `+A0`
| `!`            | `+A1`
| `"`            | `+A2`
| `#`            | `+A3`
| `$`            | `+A4`
| `%`            | `+A5`
| `&`            | `+A6`
| `'`            | `+A7`
| `(`            | `+A8`
| `)`            | `+A9`
| `*`            | `+A10`
| `+`            | `+A11`
| `,`            | `+A12`
| `-`            | `+A13`
| `.`            | `+A14`
| `/`            | `+A15`
| `0`            | `+A16`
| `1`            | `+A17`
| `2`            | `+A18`
| `3`            | `+A19`
| `4`            | `+A20`
| `5`            | `+A21`
| `6`            | `+A22`
| `7`            | `+A23`
| `8`            | `+A24`
| `9`            | `+A25`
| `:`            | `+A26`
| `;`            | `+A27`
| `<`            | `+A28`
| `=`            | `+A29`
| `>`            | `+A30`
| `?`            | `+A31`
| `@`            | `+A32`

### Group B

| Character | DuckieCrypt Translation
|:----------| :---------------------
| `[`       | `+B0`
| `\`       | `+B1`
| `]`       | `+B2`
| `^`       | `+B3`
| `_`       | `+B4`
| `` ` ``   | `+B5`

### Group C

| Character      | DuckieCrypt Translation
| :------------- | :---------------------
| `{`            | `+C0`
| `\|`           | `+C1`
| `}`            | `+C2`
| `~`            | `+C3`
