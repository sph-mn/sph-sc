# sph-sc
* a simpler c syntax. c written as scheme-like s-expressions
* the output, after formatting, is supposed to be as if originally written in c
* supports all c and its preprocessor
* command-line application and scheme library
* possibly also useful as an intermediate language for applications that want to generate c code
* status: should work, been around for a while, easy to maintain and extend
* license: gpl3+. does not apply to generated code. generated code has your license
* you can try it out [here](http://sph.mn/dynamic/syntax/sc/c)

# syntax highlights
see the [syntax reference](#syntax-reference) further below for all features

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
* struct pointers: ``aa:bb:cc`` _or_ ``(: aa bb cc)``
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

## scheme macros
sc supports non-hygienic macros with pattern matching.

~~~
(sc-define-syntax (for-i index limit body ...)
  (for ((set index 0) (< index limit) (set+ index 1)) body ...))

(for-i i 10 (printf "%lu\n" i))
~~~

~~~
(sc-define-syntax (test x ((a b) ...) body ...)
  (x ((a ...) (b) ...) body ...))
~~~

sc-define-syntax* uses scheme expressions to generate the expansion and can return strings for plain c or scheme data for sc.
~~~
(sc-define-syntax* (test* a b ...)
  (cons* 0 a b))
~~~

# dependencies
* [guile](https://www.gnu.org/software/guile) >= 2
* [sph-lib](https://github.com/sph-mn/sph-lib)

# installation
## manual
install all dependencies

### download
* [download](https://github.com/sph-mn/sph-sc/archive/master.zip)
* alternatives: [releases](http://files.sph.mn/u/software/releases), git clone git://sph.mn/sph-sc, [github](https://github.com/sph-mn/sph-sc)

### unpack
unpack the downloaded archive. for example
```shell
tar -xf sph-sc.tgz
```

* -x is for extract
* -f is for the source file name

### install
```shell
cd sph-sc
su root
./exe/install
```

the install script has a "--help" and a "--dry-run" option for more information.
the installer should only copy files and set permissions for non-root users.

## pacman
with [aurget](https://github.com/pbrisbin/aurget): ``aurget -S --deps sph-sc-git``

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
(sc-define-syntax-scm* id pattern procedure)

(sc-define-syntax-scm* (quote test) (quote (a b ...))
  (lambda (a b) (cons* 0 a b)))
~~~

alternatively, the syntax table can be modified directly:
~~~
(hashtable-set! sc-syntax-table (quote myprefix)
  (lambda (a compile state)
    "list:without-prefix procedure:recurse:{a -> string} vector -> string:c/list:sc"
    (list (quote if) (car a) #t #f)))
~~~

# utilities
this repository includes under other/
* an auto formatter "sc-format"
* a documentation extractor "sc-documentor". it displays a list of declared types, enums, routines, macros and variables in markdown
* an emacs mode "sph-sc-mode.el". example config in other/emacs
* "scc" which compiles sc files directly with gcc

# other
* filename extension for source files: ``.sc``
* clang-format is a recommended auto formatter for c that also handles macro code well. unfortunately, it cannot add empty lines between function definitions
* sc only outputs valid c syntax
* finding the source of c errors is usually the same as with plain c, particularly when the c code is formatted before compilation. modern c compilers indicate run-time errors with context and the almost like handwritten c code is available
* "sc-include" relative-paths are source-file relative unless they start with a slash. prefer standard pre-include instead of sc-include to not generate big, unwieldy c files
* sc-macros are only included with sc-include
* editor modes for scheme can be used and fast scheme-style structural editing is possible
* indent-syntax (like coffeescript or python) can be used with [wisp](https://www.draketo.de/english/wisp). also see other/wisp2sc
* square bracket array accessors can be used as long as they parse to scheme identifiers, for example (+ a[0] a[1])
* the declare and set syntax lets things be grouped nicely

* example code from projects using sc
  * [sph-sp](http://files.sph.mn/u/software/sourcecode/sph-sp/source)
  * [sph-db](http://files.sph.mn/u/software/sourcecode/sph-db/source)

# notes
## macro usage and semicolons
in strict c11 mode, semicolons after blocks with braces are forbidden. when macros are used, sc does not know if the macro will expand to something that ends with a block, for example a function definition.
compilers usually arent strict by default and dont even warn about this, but if you want to follow the standard strictly and have such a case, sc-no-semicolon can be used to prevent insertion of a semicolon.
for example: `(begin (sc-no-semicolon (mymacro 1)) (set b 2))` will lead to `mymacro(1)` instead of `mymacro(1);`

## slightly different switch-case form
"case" in sc compiles to if/else-if and lets the user specify the equality predicate.
see the section "syntax reference" below for an example.
the syntax is:
~~~
(case predicate value-to-compare case-clause ...)
case-clause: ((value ...) consequent ...) / (value consequent ...)
~~~

this way it is possible to match values with =, but alternatively other predicates like custom comparison functions.

# possible enhancements and ideas
* sc-gensym to generate unique identifiers in macros
* rewrite sph-sc in c or sc to reduce dependencies. needs a good scheme parser mainly
* "scx": c extensions, for example a module system, keyword arguments or anonymous functions
  * module system: exports-form that compiles to nothing; import form that reads export-form and rewrites all unexported identifiers to have internal names. option to add prefix to imported bindings. bindings from preprocessor macros should be handled. alternative: [clang-modules](https://clang.llvm.org/docs/Modules.html)
* translate scheme comments. function and macro docstrings are translated as expected but scheme comments dont appear in c and only ``(sc-comment "comment string")`` (or sc-insert) can be used. a scheme reader that parses scheme comments exists in sph-lib but it depends on another c library
* better support for wisp, for example with a command-line flag. sc in wisp can be simplified if some replacements are made, for example alternated key/value listings (key value key/value ...) to ((key value) ...)
* improve error messages. examples and checks just need to be extended, and a better exception printer installed
* try to reduce round brackets in the output, as there are cases where they are added when it is optional. arguments to preprocessor macros and complex and/or expressions are the perhaps most difficult cases
* hygienic macros

# syntax reference
sc expression and the c result. taken from the automated tests

~~~
(struct (pre-concat a b) (c (struct (pre-concat a b*))))
->
struct a##b{struct a##b* c;}

(declare a (array (struct b) 3))
->
struct b a[3];

(declare a (array (long unsigned int) 3))
->
long unsigned int a[3];

(begin #\newline)
->
'\n';

(begin a--b)
->
a__b;

(begin *a.b)
->
*(a.b);

(struct-get (pointer-get a) b)
->
(*a).b

(struct-get (a b) c)
->
(a(b)).c

(if* #t (set a 1 b 2) 0)
->
(1?((a=1),(b=2)):0)

(*a b)
->
(*a)(b)

(: ab cd)
->
ab->cd

(= a 1)
->
(a==1)

(and (= 1 2 3) (= 1 2))
->
(((1==2)&&(2==3))&&(1==2))

(address-of a-b)
->
&a_b

(and 1 2 3)
->
(1&&2&&3)

(and a (set b (c d)))
->
(a&&(b=c(d)))

(array-get a 1)
->
a[1]

(array-get (array-get a 1) 2)
->
(a[1])[2]

(array-get aaa 3)
->
aaa[3]

(array-get aaa 3 4 5)
->
aaa[3][4][5]

(array-get *a 3)
->
(*a)[3]

(array-literal 1 "2" 3 4)
->
{1,"2",3,4}

(array-set aa 0 11 1 22 3 33)
->
aa[0]=11;aa[1]=22;aa[3]=33;

(begin "\"\"")
->
"\"\"";

(begin "")
->
"";

(begin "a")
->
"a";

(begin a?)
->
a_p;

(begin a-b)
->
a_b;

(begin a->b)
->
a_to_b;

(begin a!)
->
a_x;

(begin a!?<->)
->
a_x_p_less_to_;

(begin <)
->
<;

(begin &a *a a.b)
->
&a;*a;a.b;

(begin #t)
->
1;

(begin -1)
->
-1;

(begin *a.field)
->
*(a.field);

(begin &*a.field)
->
&*(a.field);

(begin &*a:b:c)
->
&*(a->b->c);

(begin 1 (begin 2 3))
->
1;2;3;

(begin (enum (a b c d e)) (declare a int))
->
enum{a,b,c,d,e};int a;

(begin ab:cd:ef)
->
ab->cd->ef;

(begin ab:cd)
->
ab->cd;

(bit-shift-right 1 2)
->
(1>>2)

(bit-shift-left 1 2)
->
(1<<2)

(bit-not a-b)
->
~a_b

(case = myvalue ((3 2) #t) (4 #f) (("a" "b") #t #t) (else #f #f))
->
if((3==myvalue)||(2==myvalue)){1;}else if(4==myvalue){0;}else if(("a"==myvalue)||("b"==myvalue)){1;1;}else{0;0;}

(cond ((= a 1) #t))
->
if(a==1){1;}

(cond ((= a 1) (= b 2)) ((= c 3) #t))
->
if(a==1){(b==2);}else if(c==3){1;}

(cond ((= a 1) (= b 2)) ((= c 3) #t) (else 4))
->
if(a==1){(b==2);}else if(c==3){1;}else{4;}

(cond* ((= a 1) (= b 2)) ((= c 3) #f #t) (else #t #f))
->
((a==1)?(b==2):((c==3)?(0,1):(1,0)))

(convert-type abc int)
->
((int)(abc))

((convert-type abc function-t) d e)
->
((function_t)(abc))(d,e)

(convert-type a (function-pointer void void*))
->
((void(*)(void*))(a))

(declare aa (array size-t (b-b)))
->
size_t aa[b_b];

(declare aa (array size-t (1 2 3) (array-literal (array-literal -4 5 test-c) (array-literal 6 7 8))))
->
size_t aa[1][2][3]={{{-4,5,test_c},{6,7,8}}};

(declare a (struct test))
->
struct test a;

(declare a (struct (test int)))
->
struct a{int test;};

(declare type-name (type (function-pointer type-return type-argument-1 type-argument-2)))
->
typedef type_return(*type_name)(type_argument_1,type_argument_2);

(declare test-t (type (enum (a b c))))
->
typedef enum{a,b,c} test_t;

(declare e (type (struct (a-b (function-pointer b c-e d)) (b i-nt))))
->
typedef struct{b(*a_b)(c_e,d);i_nt b;} e;

(declare a uint32_t (b ba bb) (void uint8_t uint32_t))
->
uint32_t a;void b(uint8_t ba,uint32_t bb);

(declare c (array uint8_t (3 4)) c (array uint8_t 5) c (array uint8_t (2) 0 0))
->
uint8_t c[3][4];uint8_t c[5];uint8_t c[2]={0,0};

(declare e (enum (ea eb ec)) d (struct (da (unsigned int))))
->
enum{ea,eb,ec};struct d{unsigned int da;};

(declare f (type uint8_t) g (type (struct (ga (unsigned int)))))
->
typedef uint8_t f;typedef struct{unsigned int ga;} g;

(declare h (struct-variable ha 0 0))
->
ha h={0,0};

(declare (pre-concat h i) uint32_t)
->
uint32_t h##i;

(define a uint32_t 1)
->
uint32_t a=1

(define a uint32_t 1 b uint64_t 2)
->
uint32_t a=1;uint64_t b=2

(declare a (function-pointer (function-pointer (unsigned int) float) double))
->
unsigned int(*(*a)(double))(float);

(declare a (function-pointer (function-pointer (function-pointer int float) double) (long long int)))
->
int(*(*(*a)(long long int))(double))(float);

(define (a) (function-pointer uint32_t uint64_t) #t)
->
uint32_t(*a())(uint64_t){1;}

(define (a b) ((function-pointer uint32-t uint64_t) (function-pointer uint32-t uint64_t)) #t)
->
uint32_t(*a(uint32_t(*b)(uint64_t)))(uint64_t){1;}

(define (a b) ((function-pointer (function-pointer uint32_t b-16) uint8_t) b-64))
->
uint32_t(*(*a(b_64 b))(uint8_t))(b_16)

(define (pre-concat a b) uint32_t 1)
->
uint32_t a##b=1

(define (abc) uint32_t (return 0))
->
uint32_t abc(){return(0);}

(define (abc d e) (uint32_t uint64_t b16) (return 0))
->
uint32_t abc(uint64_t d,b16 e){return(0);}

(define (abc d e) (uint32_t (pre-concat b 64) b16) (return 0))
->
uint32_t abc(b##64 d,b16 e){return(0);}

(define (a) void "test-docstring")
->
/** test-docstring */
void a()

(define (a b) (c d) "e")
->
/** e */
c a(d b)

(define (a b c) (void void void) "test-docstring" (+ b c))
->
/** test-docstring */
void a(void b,void c){(b+c);}

(do-while #t 1 2 3)
->
do{1;2;3;}while(1)

(enum (a b c d e))
->
enum{a,b,c,d,e}

(enum (a b (c 3) d (e 4)))
->
enum{a,b,c=3,d,e=4}

(enum test (a b c d e))
->
enum test{a,b,c,d,e}

(for ((set index 0) (< index len) (set index (+ 1 index))) #t)
->
for(index=0;(index<len);index=(1+index)){1;}

(for (((set a 0) (set b 1)) (< index len) ((set a (+ 1 a)) (set b (+ 2 b)))) #t)
->
for(a=0,b=1;(index<len);a=(1+a),b=(2+b)){1;}

(for ((begin a b) (< c d) (begin e f)) #t)
->
for(a,b;(c<d);e,f){1;}

(function-pointer void vo-id*)
->
void(*)(vo_id*)

(if (= a 3) (exit 1) (return (bit-or b c)))
->
if(a==3){exit(1);}else{return((b|c));}

(if 1 2 (begin 3 4 (return #t)))
->
if(1){2;}else{3;4;return(1);}

(if* a (if* b c d) e)
->
(a?(b?c:d):e)

(if* a (if* (if* b c) d) e)
->
(a?((b?c:0)?d:0):e)

(if* (= a 3) (begin (set b-c 4) (myproc a b-c)) a)
->
((a==3)?((b_c=4),myproc(a,b_c)):a)

(if* (not a) #t #f)
->
(!a?1:0)

(if* (not 1) a b)
->
(!1?a:b)

(label abc (define a uint32_t 3) (+ a b))
->
abc:uint32_t a=3;(a+b);

(let* ((a size_t 1) (b size_t 2) (c 3)) (set c 7) (return (if* 4 5 6)))
->
{size_t a=1;size_t b=2;c=3;c=7;return((4?5:6));}

(not 1)
->
!1

(pointer-get a-b)
->
*a_b

(pointer-get (a b))
->
*(a(b))

(pointer-get b)
->
*b

(pointer-get b.c)
->
*(b.c)

(pre-concat a b cd e)
->
a##b##cd##e

(pre-cond ((= a b) 1))
->
#if (a==b)
1;
#endif

(pre-cond ((= a b) 1) (c (pre-define a)) (else 2))
->
#if (a==b)
1;
#elif c
#define a
#else
2;
#endif

(pre-cond-defined (a 1) (b 2))
->
#ifdef a
1;
#elif b
2;
#endif

(pre-cond-not-defined (a 1))
->
#ifndef a
1;
#endif

(pre-define a)
->
#define a

(pre-define (my-macro a b) (if* a #t #f))
->
#define my_macro(a,b) (a?1:0)

(pre-define (a) #t)
->
#define a() 1

(pre-define (a b) (begin "test-docstring" (+ b c) 3))
->
/** test-docstring */
#define a(b) (b+c);3

(pre-define ob-ject 3)
->
#define ob_ject 3

(pre-define a 1 (id a b) (= a b))
->
#define a 1
#define id(a,b) (a==b)

(pre-define a 1 (id) b)
->
#define a 1
#define id() b

(pre-define (->test a b) c)
->
#define _to_test(a,b) c

(pre-if (= a b) (begin c d e) (begin f g))
->
#if (a==b)
c;d;e;
#else
f;g;
#endif

(pre-if-not-defined a b c)
->
#ifndef a
b;
#else
c;
#endif

(pre-if-defined a b c)
->
#ifdef a
b;
#else
c;
#endif

(pre-include "./a/b.c")
->
#include "./a/b.c"

(pre-include "../a/b.c")
->
#include "../a/b.c"

(pre-include "a/b.c")
->
#include <a/b.c>

(pre-include "bb.h")
->
#include <bb.h>

(pre-include "a" "b" "./c")
->
#include <a>
#include <b>
#include "./c"

(pre-let* (a 1 b 2) (+ a b))
->
#define a 1
#define b 2
(a+b);
#undef a
#undef b

(pre-let* (a 1) a)
->
#define a 1
a;
#undef a

(pre-let* ((a b) 1) a)
->
#define a(b) 1
a;
#undef a

(pre-let* ((a b) 1 (c d) 2) a)
->
#define a(b) 1
#define c(d) 2
a;
#undef a
#undef c

(pre-pragma once)
->
#pragma once

(pre-stringify abc)
->
#abc

(pre-undefine my-macro)
->
#undef my_macro

(return)
->
return

(return 1 2)
->
return(1,2)

(sc-insert "var a = 3")
->
var a = 3

(set *a *b.c)
->
*a=*(b.c)

(set a 1)
->
a=1

(set a 1 b-2 2 c-3 3)
->
a=1;b_2=2;c_3=3;

(set a:b (: *a b))
->
a->b=(*a)->b

(struct-get a b)
->
a.b

(struct-get a b c d e)
->
a.b.c.d.e

(struct-get (pointer-get a) b)
->
(*a).b

(struct-get **a b)
->
(**a).b

(struct (a (unsigned int)) (b (unsigned char) 3))
->
struct{unsigned int a;unsigned char b:3;}

(struct testname (a (uns-igned int)) (b (unsigned char) 3))
->
struct testname{uns_igned int a;unsigned char b:3;}

(struct (a-b (function-pointer b c-e d)) (b i-nt))
->
struct{b(*a_b)(c_e,d);i_nt b;}

(struct-literal (a 1) (b "2"))
->
{.a=1,.b="2"}

(struct-literal a 1)
->
{a,1}

(struct-set a b 1 c 2)
->
a.b=1;a.c=2;

(struct-pointer-get a b)
->
a->b

(struct-pointer-get a b c d)
->
a->b->c->d

(union (a (unsigned int)) (b (unsigned char) 3))
->
union{unsigned int a;unsigned char b:3;}

(while #t 1 2 3)
->
while(1){1;2;3;}

(while (not (= 0 (set a (b c)))) #t)
->
while(!(0==(a=b(c)))){1;}

(sc-comment "abc")
->
/* abc */

(sc-comment "abc" "def" "ghi")
->
/* abc
def
ghi */

(!= 1 2 3)
->
((1!=2)&&(2!=3))

(begin (sc-no-semicolon (a 1)) (set b 2))
->
a(1)
b=2;

(begin (sc-no-semicolon (a 1) (set b 2)))
->
a(1)
b=2

(begin (pre-define a (begin (define (a) void 1))) (declare b int))
->
#define a void a(){1;}
int b;

(begin (pre-define (a b) (define (c) void 1)) (a "xyz"))
->
#define a(b) void c(){1;}
a("xyz")

(set+ a 1 b 2)
->
a+=1;b+=2;

(set- a 1)
->
a-=1

(set* a 1)
->
a*=1

(set/ a 1)
->
a/=1

(declare a (type (struct (b (array int 3)))))
->
typedef struct{int b[3];} a;

(pre-define-if-not-defined abc 3 def 4)
->
#ifndef abc
#define abc 3
#endif
#ifndef def
#define def 4
#endif

(pre-define (a) (begin 1 (sc-comment "b") 2 3))
->
#define a() 1;\
/* b */\
2;3

(case* = myvalue ((3 2) #t) (4 #f) (("a" "b") #t #t) (else #f #f))
->
(((3==myvalue)||(2==myvalue))?1:((4==myvalue)?0:((("a"==myvalue)||("b"==myvalue))?(1,1):(0,0))))

(for ((set a 1 b 2) #t (set c 3 d 4)) #t)
->
for(a=1,b=2;1;c=3,d=4){1;}

(begin (pre-define (a) (begin "test" b) c d) (declare e f))
->
/** test */
#define a() b
#define c d
f e;
~~~

# similar projects
* [lispc](https://github.com/eratosthenesia/lispc) - lisp(ish) to c converter (designed for clisp)
* [sxc](https://github.com/busfactor1inc/sxc) - 's-expression c' transpiler for generating c code using macros written in common lisp
