(define-test-module (test module sph lang sc)
  (import
    (sph test)
    (sph lang sc))

  (define-test (sc->c arguments) (sc->c arguments))

  (test-execute-procedures-lambda
    (sc->c
      (begin "\"\"")
      "\"\\\"\\\"\";"
      (begin "")
      "\"\";"
      (begin "a")
      "\"a\";"
      (begin a?)
      "a_p;"
      (begin a-b)
      "a_b;"
      (begin a+b)
      "a_and_b;"
      (begin a->b)
      "a_to_b;"
      (begin a!)
      "a_x;"
      (begin a!?<->)
      "a_x_p_less_to_;"
      (begin <)
      "<;"
      (if* a (if* b c d) e)
      "(a?(b?c:d):e)"
      (if* a (if* (if* b c) d) e)
      "(a?((b?c:0)?d:0):e)"
      (if* (= a 3) (begin (set b+c 4) (myproc a b+c)) a)
      "((a==3)?((b_and_c=4),myproc(a,b_and_c)):a)"
      (if (= a 3) (exit 1) (return (bit-or b c)))
      "if((a==3)){exit(1);}else{return((b|c));}"
      (set a 1 b+2 2 c-3 3)
      "a=1;b_and_2=2;c_3=3;"
      (if* (not a) #t #f)
      "(!a?1:0)"
      (define a b32 1)
      "b32 a=1"
      (define a b32 b+2 b16 c-3 b0)
      "b32 a;b16 b_and_2;b0 c_3;"
      (define (pre-concat a b) b32 1)
      "b32 a##b=1"
      (define (abc) b32 (return 0))
      "b32 abc(){return(0);}"
      (define (abc d e) (b32 b64 b16) (return 0))
      "b32 abc(b64 d,b16 e){return(0);}"
      (define (abc d e) (b32 (pre-concat b 64) b16) (return 0))
      "b32 abc(b##64 d,b16 e){return(0);}"
      (set a 1)
      "a=1"
      (deref a 1)
      "(*(a+1))"
      (deref (deref a 1) 2)
      "(*((*(a+1))+2))"
      (not 1)
      "!1"
      (if* (not 1) a b)
      "(!1?a:b)"
      (and 1 2 3)
      "(1&&2&&3)"
      (= 1 2 3)
      "(1==2==3)"
      (equal? a 1)
      "(a==1)"
      (begin #t)
      "1;"
      (begin -1)
      "-1;"
      (begin 1 (begin 2 3))
      "1;2;3;"
      (while #t 1 2 3)
      "while(1){1;2;3;}"
      (do-while #t 1 2 3)
      "do{1;2;3;}while(1)"
      (cond ((= a 1) (equal? b 2)) ((equal? c 3) #t))
      "if((a==1)){(b==2);}else{if((c==3)){1;};}"
      (cond* ((= a 1) (equal? b 2)) ((equal? c 3) #f #t) (else #t #f))
      "((a==1)?(b==2):((c==3)?(0,1):(1,0)))"
      (quote "var a = 3")
      "var a = 3"
      (return)
      "return"
      (return 1 2)
      "return(1,2)"
      (if 1 2 (begin 3 4 (return #t)))
      "if(1){2;}else{3;4;return(1);}"
      (deref a-b)
      "(*a_b)"
      (struct-get a b)
      "a.b"
      (struct-get a b c d e)
      "a.b.c.d.e"
      (struct-get (deref a) b)
      "(*a).b"
      (struct-pointer-get a b)
      "(*a).b"
      (struct-pointer-get a b c d)
      "(*a).b.c.d"
      (pre-define (my-macro a b) (if* a #t #f))
      "#define my_macro(a,b) (a?1:0)"
      (pre-define ob-ject 3)
      "#define ob_ject 3"
      (pre-if (equal? a b) (begin c d e) (begin f g))
      "#if (a==b)\nc;d;e;\n#else\nf;g;\n#endif"
      (pre-undefine my-macro)
      "#undef my_macro\n"
      (pre-concat a b cd e)
      "a##b##cd##e"
      (pre-if-not-defined a b c)
      "#ifndef a\nb;\n#else\nc;\n#endif"
      (pre-if-defined a b c)
      "#ifdef a\nb;\n#else\nc;\n#endif"
      (define-type mytype int)
      "typedef int mytype"
      (define-type type-name (function-pointer type-return type-argument-1 type-argument-2))
      "typedef type_return(*type_name)(type_argument_1,type_argument_2)"
      (address-of a-b)
      "&a_b"
      (convert-type abc int)
      "((int)(abc))"
      ((convert-type abc function-t) d e)
      "((function_t)(abc))(d,e)"
      (bit-shift-right 1 2)
      "(1>>2)"
      (bit-shift-left 1 2)
      "(1<<2)"
      (bit-not a-b)
      "~a_b"
      (struct (a (unsigned int)) (b (unsigned char) 3))
      "struct{unsigned int a;unsigned char b:3;}"
      (struct testname (a (uns-igned int)) (b (unsigned char) 3))
      "struct testname{uns_igned int a;unsigned char b:3;}"
      (struct (a-b (function-pointer b c-e d)) (b i-nt))
      "struct{b(*a_b)(c_e,d);i_nt b;}"
      (define-type e (struct (a-b (function-pointer b c-e d)) (b i-nt)))
      "typedef struct{b(*a_b)(c_e,d);i_nt b;} e"
      (union (a (unsigned int)) (b (unsigned char) 3))
      "union{unsigned int a;unsigned char b:3;}"
      (pre-let (a 1 b 2) (+ a b))
      "#define a 1\n#define b 2\n(a+b);\n#undef a\n\n#undef b\n"
      (pre-let (a 1) a)
      "#define a 1\na;\n#undef a\n"
      (pre-let ((a b) 1) a)
      "#define a(b) 1\na;\n#undef a\n"
      (pre-let ((a b) 1 (c d) 2) a)
      "#define a(b) 1\n#define c(d) 2\na;\n#undef a\n\n#undef c\n"
      (let* ((a size_t 1) (b size_t 2) (c 3)) (set c 7) (return (if* 4 5 6)))
      "{size_t a=1;size_t b=2;c=3;c=7;return((4?5:6));}"
      (pre-define (->test a b) c)
      "#define _to_test(a,b) c"
      (define-array aa size-t (1))
      "size_t aa[1]"
      (define-array aa size-t (1 2 3))
      "size_t aa[1][2][3]"
      (define-array aa size-t (b-b))
      "size_t aa[b_b]"
      (define-array aa size-t (2) 3 4)
      "size_t aa[2]={3,4}"
      (array-get aaa 3)
      "(*(aaa+3))"
      (array-get aaa 3 4 5)
      "(*(aaa+((3*4)+5)))"
      (define-array aa size-t
        (1 2 3) (array-literal (array-literal -4 5 test-c) (array-literal 6 7 8)))
      "size_t aa[1][2][3]={{{-4,5,test_c},{6,7,8}}}"
      (array-set aa 0 11 1 22 2 33)
      "(*(aa+0))=11;(*(aa+1))=22;(*(aa+2))=33;"
      (pre-include "./a/b.c")
      "#include \"./a/b.c\"\n"
      (pre-include "../a/b.c")
      "#include \"../a/b.c\"\n"
      (pre-include "a/b.c")
      "#include <a/b.c>\n"
      (pre-include "bb.h")
      "#include <bb.h>\n"
      (pre-include "a" "b" "c")
      "#include <a>\n#include <b>\n#include <c>\n"
      (pre-include-once a "./a.c" b "b.h")
      "#ifndef sc_included_a\n#include \"./a.c\"\n#define sc_included_a \n#endif\n#ifndef sc_included_b\n#include <b.h>\n#define sc_included_b \n#endif"
      (pre-include-once a "./a.c")
      "#ifndef sc_included_a\n#include \"./a.c\"\n#define sc_included_a \n#endif"
      (pre-define-if-not-defined abc 3 def 4)
      "\n#ifndef abc\n\n#define abc 3\n\n#endif\n#ifndef def\n\n#define def 4\n\n#endif\n"
      (case = myvalue ((3 2) #t) (4 #f) (("a" "b") #t #t) (else #f #f))
      "if(((3==myvalue)||(2==myvalue))){1;}else{if((4==myvalue)){0;}else{if(((\"a\"==myvalue)||(\"b\"==myvalue))){1;1;}else{0;0;};};}"
      (case* = myvalue ((3 2) #t) (4 #f) (("a" "b") #t #t) (else #f #f))
      "(((3==myvalue)||(2==myvalue))?1:((4==myvalue)?0:(((\"a\"==myvalue)||(\"b\"==myvalue))?(1,1):(0,0))))"
      (enum test (a b c d e))
      "enum test{a,b,c,d,e}"
      (define-type test-t (enum (a b c)))
      "typedef enum{a,b,c} test_t"
      (enum (a b c d e))
      "enum{a,b,c,d,e}"
      (begin (enum (a b c d e)) (define a int))
      "enum{a,b,c,d,e};int a;"
      (enum (a b (c 3) d (e 4)))
      "enum{a,b,c=3,d,e=4}"
      (pre-stringify abc)
      "#abc"
      (array-literal 1 "2" 3 4)
      "{1,\"2\",3,4}"
      (struct-literal (a 1) (b "2"))
      "{.a=1,.b=\"2\"}"
      (struct-literal a 1)
      "{a,1}"
      (function-pointer void vo-id*)
      "void(*)(vo_id*)"
      (convert-type a (function-pointer void void*))
      "((void(*)(void*))(a))"
      (define a (function-pointer (function-pointer (unsigned int) float) double))
      "unsigned int(*(*a)(double))(float)"
      (define a
        (function-pointer (function-pointer (function-pointer int float) double) (long long int)))
      "int(*(*(*a)(long long int))(double))(float)"
      (define (a) (function-pointer b32 b64) #t)
      "b32(*a())(b64){1;}"
      (define (a b) ((function-pointer b-32 b64) (function-pointer b-32 b64)) #t)
      "b_32(*a(b_32(*b)(b64)))(b64){1;}"
      (define (a b) ((function-pointer (function-pointer b32 b-16) b8) b-64))
      "b32(*(*a(b_64 b))(b8))(b_16)"
      (pre-define-if-not-defined (a b c) #t)
      "\n#ifndef a\n\n#define a(b,c) 1\n\n#endif\n"
      (define (a) b0 "test-docstring")
      "/** test-docstring */\nb0 a(){}"
      (define (a b c) (b0 b0 b0) "test-docstring" (+ b c))
      "/** test-docstring */\nb0 a(b0 b,b0 c){(b+c);}"
      (pre-define (a b) "test-docstring" (+ b c) 3)
      "/** test-docstring */\n#define a(b) (b+c);\\\n  3"
      )))
