# sph-sc
* a simpler c syntax. c written as scheme-like s-expressions
* the output, after formatting, is supposed to be as if originally written in c
* supports all c and its preprocessor
* command-line application and scheme library
* possibly also useful as an intermediate language for applications that want to compile to c
* status: should work, been around for a while, easy to maintain and extend
* license: gpl3+. does not apply to generated code. generated code has your license
* [homepage](http://sph.mn/c/view/me)

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

## macros
macros are defined like variables and functions and dont need escaped newlines or special formatting
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

in sc, prefixes apply consistently to the whole following expression. for example, "*aa.bb" means *(aa.bb)

## function pointers
function pointer syntax is
```
(function-pointer output-type input-types ...)
```
in declarations this syntax can be used in place of type names - instead of wrapping the identifier like with c.
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
(define b (function-pointer (function-pointer (function-pointer int float) double) (long long int)))
```
in c it looks like this, shorter but not necessarily easier to read (notice how it is wrapped outside in)
```
int(*(*(*b)(long long int))(double))(float)
```

## identifiers
the characters "-", "->", "?", "!", which are often used in scheme, are allowed and replaced in identifiers.
the replacements are done like guile does it. for example "-" becomes "_", "?" becomes "_p" (predicate) and "!" becomes "_x"

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

# dependencies
* [guile](https://www.gnu.org/software/guile) >= 2
* [sph-lib](https://github.com/sph-mn/sph-lib)

# installation
## manual
install all dependencies

### download
* [download](http://sph.mn/git/download/sph-sc.stable.tgz)
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

# other
* filename extension for source files: ``.sc``
* clang-format is a recommended auto formatter for c that also handles macro code well
* sc only outputs valid c syntax
* finding the source of c errors is usually not more difficult compared to plain c, especially when the c code is formatted before compilation. modern c compilers indicate run-time errors with context and the c code is available
* a benefit of using sc is that editor modes for scheme syntax and structural editing can be used
* "sc-include" relative-paths are source-file relative unless they start with a slash
* an emacs mode can be found [here](https://github.com/sph-mn/sph-other/tree/master/emacs/mode) and an auto formatter with the same dependencies as sph-sc [here](https://github.com/sph-mn/sph-script/tree/master/1/other)
* example code from projects using sc
  * [sph-db](http://files.sph.mn/sourcecode/sph-db/source)

# syntax reference
sc expression and the c result. taken from the automated tests
```
(begin *a.field)
->
*(a.field);

(begin &*a.field)
->
&*(a.field);

(begin &*a:b:c)
->
&*(a->b->c);

(: ab cd)
->
ab->cd

(= a 1)
->
(a==1)

(= 1 2 3)
->
(1==2)&&(2==3)

(address-of a-b)
->
&a_b

(and 1 2 3)
->
(1&&2&&3)

(and a (set b (c d)))
->
(a&&(b=c(d)))

(array-get aaa 3)
->
aaa[3]

(array-get aaa 3 4 5)
->
aaa[3][4][5]

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
if(((3==myvalue)||(2==myvalue))){1;}else if((4==myvalue)){0;}else if((("a"==myvalue)||("b"==myvalue))){1;1;}else{0;0;}

(case* = myvalue ((3 2) #t) (4 #f) (("a" "b") #t #t) (else #f #f))
->
(((3==myvalue)||(2==myvalue))?1:((4==myvalue)?0:((("a"==myvalue)||("b"==myvalue))?(1,1):(0,0))))

(cond ((= a 1) #t))
->
if((a==1)){1;}

(cond ((= a 1) (= b 2)) ((= c 3) #t))
->
if((a==1)){(b==2);}else if((c==3)){1;}

(cond ((= a 1) (= b 2)) ((= c 3) #t) (else 4))
->
if((a==1)){(b==2);}else if((c==3)){1;}else{4;}

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

(declare (pre-concat h i) uint32_t)
->
uint32_t h##i;

(define a uint32_t 1)
->
uint32_t a=1

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
void a(){}

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
if((a==3)){exit(1);}else{return((b|c));}

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

(array-get a 1)
->
a[1]

(array-get (array-get a 1) 2)
->
(a[1])[2]

(pointer-get a-b)
->
(*a_b)

(pre-concat a b cd e)
->
a##b##cd##e

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
#define a(b) (b+c);\
  3

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

(pre-define-if-not-defined abc 3 def 4)
->
#ifndef abc
#define abc 3
#endif
#ifndef def
#define def 4
#endif

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

(pre-let (a 1 b 2) (+ a b))
->
#define a 1
#define b 2
(a+b);
#undef a
#undef b

(pre-let (a 1) a)
->
#define a 1
a;
#undef a

(pre-let ((a b) 1) a)
->
#define a(b) 1
a;
#undef a

(pre-let ((a b) 1 (c d) 2) a)
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

(sc-insert "var a = 3")
->
var a = 3

(return)
->
return

(return 1 2)
->
return(1,2)

(set a 1)
->
a=1

(set a 1 b-2 2 c-3 3)
->
a=1;b_2=2;c_3=3;

(struct-get a b)
->
a.b

(struct-get a b c d e)
->
a.b.c.d.e

(struct-get (pointer-get a) b)
->
(*a).b

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
```

# possible enhancements and ideas
* translate scheme comments. function and macro docstrings are translated as expected but scheme comments dont appear in c unless ``(sc-comment "comment string")`` is used
* allow users to add syntax like [sescript](https://github.com/sph-mn/sescript) does
* sc-syntax-case and sc-syntax-rules: scheme code or pattern matching to create expansions. implement do-while as an example
* "scx": an extension with a module system.
* more syntax checks
