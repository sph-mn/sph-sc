# sph-sc
* a simpler c syntax. c written as scheme-like s-expressions
* scheme/lisp like macro system
* supports c and its preprocessor
* the output, after formatting, is supposed to be as if originally written in c
* command-line application and guile library
* possibly also useful as an intermediate language for applications that want to generate c code
* status: should work, been around for a while, easy to maintain and extend
* license: gpl3+. does not apply to generated code. generated code has your license
* you can try it out [here](http://sph.mn/dynamic/syntax/sc/c)

# syntax highlights
see [other/syntax-reference.md](other/syntax-reference.md) for more examples.

## declarations
c supports declarations for variables, arrays, structs, types and enums and they all have a different syntax. sc has declare

```
(declare
  a uint32_t
  b (array uint8_t 3)
  c (struct (id int) (name char*))
  d (enum (x y z))
  e (type uint16_t)
  f (type (struct (id int) (name char*))))
```

## preprocessor macros
preprocessor macros are defined similar to variables and functions and dont need escaped newlines or special formatting
```
(pre-define is-included #t)
```
```
(pre-define (mymacro a b)
  (begin
    (set
      a 1
      b 2)))
```
multiple macros can be defined at once
```
(pre-define
  is-included #t
  id-size 4
  (mymacro a b) (set a 1 b 2))
```

## functions
```
(define (myfunction a b) (int char void)
  "a description of this function"
  (return 1))
```

## structs, addresses, pointers, types
* structs: ``aa.bb`` _or_ ``(struct-get aa bb)``
* struct pointers: ``aa:bb:cc`` _or_ ``(struct-pointer-get aa bb cc)``
* addresses: ``&aa`` _or_ ``(address-of aa)``
* pointers: ``*aa`` _or_ ``(pointer-get aa)``
* types: ``(convert-type aa uint8_t)``

in sc, prefixes apply consistently to the whole following expression. for example, ``*aa.bb`` means ``*(aa.bb)``

## function pointers
function pointer syntax is
```
(function-pointer output-type input-types ...)
```
in declarations, this syntax can be used in place of type names - instead of wrapping the identifier like with c.
an int variable would be declared like this
```
(declare b int)
```
a pointer to a function that takes one char argument and returns an int
```
(declare b (function-pointer int char))
```
a pointer to a function that returns a pointer to a function that returns a pointer to a function
```
(declare b (function-pointer (function-pointer (function-pointer int float) double) (long long int)))
```
in c it looks like this, shorter but not necessarily easier to read (notice how it is wrapped outside in)
```
int(*(*(*b)(long long int))(double))(float)
```

## identifiers
the characters "-", "->", "?", "!", which are often used in scheme, are allowed and replaced in identifiers.
the replacements are done like guile does it. "-" becomes "_", "->" becomes ``_to_``, "?" becomes "_p" (predicate) and "!" becomes "_x", some only in the middle or at the end of identifiers

## if expressions
``(set a (if* b 1 2))`` -> ``a = (b ? 1 : 2)``

## goto labels
```
(label exit
  (return status))
```

## string insertion
```
(sc-insert "// free c code string to be included as is")
```

## s-expression macros
sc supports non-hygienic macros with pattern matching.

~~~
(sc-define-syntax (for-each-index index limit body ...)
  (for ((define index size-t 0) (< index limit) (set+ index 1)) body ...))

(for-each-index i 10 (printf "%lu\n" i))
~~~

complex ellipsis patterns are possible

~~~
(sc-define-syntax (test x ((a b) ...) body ...)
  (x ((a ...) (b) ...) body ...))
~~~

sc-define-syntax* uses scheme expressions to generate the expansion and can return strings for plain c or scheme data for sc.
~~~
(sc-define-syntax* (test* a b ...)
  (let ((c 1))
    (cons* a c b)))

(sc-define-syntax* (test* a b ...)
  (quasiquote (if (unquote a) 1 (begin (unquote-splicing b)))))
~~~

# using indent-based syntax
if you prefer writing c using coffeescript- or python-like indented structure, sc allows you to do so using the [wisp (srfi-119)](https://srfi.schemers.org/srfi-119/srfi-119.html) syntax. all file names ending with .scw are automatically parsed as wisp.

example.scw
~~~
pre-include "stdio.h"

define (main argc argv) : int int char**
  declare i int
  printf "the number of program arguments passed is %d\n" argc
  for : (set i 0) (< i argc) (set+ i 1)
    printf "argument %d is %s\n" (+ i 1) (array-get argv i)
  return 0
~~~

# dependencies
* [guile](https://www.gnu.org/software/guile) >= 3

# installation
## download
download the project archive:
* <https://github.com/sph-mn/sph-sc/archive/master.zip>

## unpack
extract the archive:
```
tar -xf sph-sc.tgz
```
## install
run the installer:
```
cd sph-sc
./exe/install
```
this installs:
* modules into your guile load path
* the `sc` executable into your system path

### optional prefix
you can install relative to a custom location:
```
./exe/install /your/prefix
```
## alternative: symlink install
instead of copying files, you can symlink them:
```
cd sph-sc
./exe/install-symlink
```
## test
run the test script:
```
./exe/test
```
if it runs without errors, the installation is complete.
## notes
* if `sc` is not found, ensure your `PATH` includes the install location.
* if modules are not found, ensure your `GUILE_LOAD_PATH` includes the install location.

## arch user repository (aur) package
* [detail page](https://aur.archlinux.org/packages/sph-sc-git)
* installation with [rua](https://github.com/vn971/rua): `rua install sph-sc-git

# command-line application
$ sc --help
```
parameters
  options ... source-path ... destination-path
  options ... source-path
options
  --help | -h
  --interface
  --parents  treat target as directory and recreate the directory structure of source files
```

# usage from scheme
```
(import (sph lang sc))
```

examples
```
(define result-c-string (sc->c (quote (begin (declare a int) (set a 1)))))

(define scheme-value 8)

(define code
  (quasiquote
    (begin
      (declare a int)
      (set a (unquote scheme-value)))))

(sc->c code)
```

custom syntax can be defined from scheme before using sc->c with:
~~~
; symbol list list -> unspecified
(sc-define-syntax-scm id pattern expansion)

; symbol list {any ... -> string:c/any:sc} -> unspecified
(sc-define-syntax-scm id pattern procedure)

(sc-define-syntax-scm (quote test) (quote (a b ...))
  (lambda (a b) (cons* 0 a b)))
~~~

alternatively, the syntax table can be modified directly:
~~~
(hashtable-set! sc-syntax-table (quote myprefix)
  (lambda (a compile state)
    "list:without-prefix procedure:recurse:{a -> string} vector -> string:c/list:sc"
    (list (quote if) (car a) #t #f)))
~~~

## bindings available in sc-define-syntax*
the current environment is:
~~~
(environment (q (guile)) (q (ice-9 match)) (q (sph lang sc eval-environment)))
~~~

custom environments can be passed to sc->c via sc-state, which can be created with sc-state-new. hints:
~~~
(sc-state-new load-paths eval-env)
(sc->c a load-paths state)
~~~

# utilities
this repository includes under other/
* an auto formatter "sc-format". besides formatting using a regular indentation style where depth corresponds to nesting, it also automatically merges and simplifies some expressions
* a documentation extractor "sc-documentor". it displays a list of declared types, enums, routines, macros, and variables as markdown
* an emacs mode "sph-sc-mode.el". example config in other/emacs
* example-macros.sc

# other
* filename extension for source files: ``.sc``
* clang-format is a recommended auto formatter for c that also handles macro code relatively well. unfortunately, it cannot add empty lines between function definitions and does not cleanly format macros without semicolons
* sc only outputs valid c syntax
* finding the source of c errors is usually the same as with plain c, particularly when the c code is formatted before compilation. modern c compilers indicate run-time errors with context, sc mostly corresponds directly to c, and the directly translated c code is available if in doubt
* "sc-include" relative-paths are source-file relative unless they start with a slash. prefer standard pre-include instead of sc-include to not generate big, unwieldy c files
* sc-macros are only included with sc-include
* in sc-define-syntax*, (sc-gensym) and (sc-syntax? identifier) are available. the former returns a new identifier with each call, _t1, _t2 and so on, for temporary variable names
* editor modes for scheme can be used and fast scheme-style structural editing is possible
* indent-syntax (similar to coffeescript or python) can be used. file names ending with .scw are automatically read as wisp. for code read from standard input, the ``--wisp`` option can be used
* square bracket array accessors can be used as long as they parse to scheme identifiers, for example (+ a[0] a[1]). this requires that the []/() parser ambiguity is disabled, which sc does by default.
* the declare and set syntax lets things be grouped nicely
* llm chatbots can convert from c to sc. see [other/llm-c-to-sc-conversion-prompt.txt](other/llm-c-to-sc-conversion-prompt.txt)

# notes
## macro usage and semicolons
in strict c11 mode, semicolons after blocks with braces are forbidden. when macros are used, sc does not know if the macro will expand to something that ends with a block, for example a function definition.
compilers usually arent strict by default and dont even warn about this, but if you want to follow the standard strictly and have such a case, sc-no-semicolon can be used to prevent insertion of a semicolon.
for example: `(sc-no-semicolon (mymacro 1))` will lead to `mymacro(1)` instead of `mymacro(1);`

## slightly different switch-case form
"case" in sc compiles to if/else-if and lets the user specify the equality predicate.
the syntax is:
~~~
(case predicate value-to-compare case-clause ...)
case-clause: ((value ...) consequent ...) / (value consequent ...)
~~~

this way it is possible to match values with =, but alternatively other predicates like custom comparison functions.

# possible enhancements and ideas
* keyword arguments: it would be easy for sc to match guile style #:keywords with the parameter names of function definitions
* module system: exports-form that compiles to nothing; import form that reads export-form from files and rewrites unexported identifiers to have less likely conflicting internal names. option to add prefix to imported bindings. bindings from preprocessor macros should be handled. or syntax for [clang-modules](https://clang.llvm.org/docs/Modules.html)
* translate scheme comments. function and macro docstrings are translated as expected but scheme comments dont appear in c, only with ``(sc-comment "comment string")`` or sc-insert. a scheme reader that parses scheme comments exists via sph-lib but requires a c library that often does not compile
* improve error messages. currently, guile exceptions are displayed. there is an existing syntax check function with example patterns that can be extended, with a better exception printer, errors could be handled nicely by showing both what is wrong and how it can look like.
* support actual switch/case instead of compiling to if/else, which is currently done to add an extra feature that c does not have
* confirm if the full range of array type syntax can be realized without sc-insert
  * ``int(*)[3]``, ``int(*a)[3]``, ``typedef int(*a)[3]``, ``int[][3]``
  * like (array x ...) in declare. for type conversions, function parameters, typedef and dynamic array sizes
* hygienic macros
* try to reduce optional round brackets in the output. this is difficult in the case of arguments to preprocessor macros

# similar projects
* [lispc](https://github.com/eratosthenesia/lispc) - lisp(ish) to c converter (designed for clisp)
* [sxc](https://github.com/busfactor1inc/sxc) - 's-expression c' transpiler for generating c code using macros written in common lisp
