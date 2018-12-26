# Changelog

## 0.3

Highlights:

* Adding `Metaparser`, a parser that constructs a parser from a grammar similar to EBNF

### 0.3.0

* Merged `Atomics` and `Transformers` into `Prebuilt`
* Removed deprecated `skipN`
* Renamed `TokenStream` functions to share the names of `Prelude` functions on lists
* Added `Metaparser`, a parser parsing a grammar into a parser
* Added `pfix`, a fixed point combinator for parsers depending on their own result
* Added convenience function `wrap` to `DCont`
* Added `forceParse` function to parse assuming success
* Added `surround` parser transformer for parsing parenthesised expressions

## 0.2

Highlights:

* Switching from list-based parsing to `TokenStream`

### 0.2.3

* Added `nParse` (cParse for single token) and `try` (parse with Maybe) Transformers, `sInteger` (signed integers) Atomic

### 0.2.2

* Added Either instance for `TokenStream`
* Auto-`success` atomic, atomics for splitting an integer into digits

### 0.2.1

* Deprecated `skipN`, replaced with `sDrop` 

### 0.2.0

* Added `TokenStream`, an abstraction of lists
* Used `TokenStream` to reformulate Atomics and Transformers

## 0.1

Highlights:

* Initial Version

### 0.1.4

* Added `digit` and `letter` parsers

### 0.1.3

* Improved testing facilities
* Added `check` function

## 0.1.2

### 0.1.2.0

* Added `peek` function