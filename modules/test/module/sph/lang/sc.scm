(define-test-module (test module sph lang sc)
  (import
    (sph test)
    (sph lang sc))

  (define-test (sc->c arguments) (sc->c arguments))

  (test-execute-procedures-lambda
    (sc->c
      (= 1 2 3)
      "(1==2==3)"
      (address-of a-b)
      "&a_b"
      (and 1 2 3)
      "(1&&2&&3)"
      (and a (set b (c d)))
      "(a&&(b=c(d)))"
      (array-get aaa 3)
      "(*(aaa+3))"
      (array-get aaa 3 4 5)
      "(*(aaa+((3*4)+5)))"
      (array-literal 1 "2" 3 4)
      "{1,\"2\",3,4}"
      (array-set aa 11 22 33)
      "(*(aa+0))=11;(*(aa+1))=22;(*(aa+2))=33;"
      (array-set-index aa 0 11 1 22 3 33)
      "(*(aa+0))=11;(*(aa+1))=22;(*(aa+3))=33;"
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
      (begin #t)
      "1;"
      (begin -1)
      "-1;"
      (begin 1 (begin 2 3))
      "1;2;3;"
      (begin (enum (a b c d e)) (define a int))
      "enum{a,b,c,d,e};int a;"
      (bit-shift-right 1 2)
      "(1>>2)"
      (bit-shift-left 1 2)
      "(1<<2)"
      (bit-not a-b)
      "~a_b"
      (case = myvalue ((3 2) #t) (4 #f) (("a" "b") #t #t) (else #f #f))
      "if((((3==myvalue))||((2==myvalue)))){1;}else{if((4==myvalue)){0;}else{if((((\"a\"==myvalue))||((\"b\"==myvalue)))){1;1;}else{0;0;};};}"
      (case* = myvalue ((3 2) #t) (4 #f) (("a" "b") #t #t) (else #f #f))
      "((((3==myvalue))||((2==myvalue)))?1:((4==myvalue)?0:((((\"a\"==myvalue))||((\"b\"==myvalue)))?(1,1):(0,0))))"
      (cond ((= a 1) (equal? b 2)) ((equal? c 3) #t))
      "if((a==1)){(b==2);}else{if((c==3)){1;};}"
      (cond* ((= a 1) (equal? b 2)) ((equal? c 3) #f #t) (else #t #f))
      "((a==1)?(b==2):((c==3)?(0,1):(1,0)))"
      (convert-type abc int)
      "((int)(abc))"
      ((convert-type abc function-t) d e)
      "((function_t)(abc))(d,e)"
      (convert-type a (function-pointer void void*))
      "((void(*)(void*))(a))"
      (declare aa (array size-t (b-b)))
      "size_t aa[b_b];"
      (declare aa (array size-t (1 2 3) (array-literal (array-literal -4 5 test-c) (array-literal 6 7 8))))
      "size_t aa[1][2][3]={{{-4,5,test_c},{6,7,8}}};"
      (declare type-name (type (function-pointer type-return type-argument-1 type-argument-2)))
      "typedef type_return(*type_name)(type_argument_1,type_argument_2);"
      (declare test-t (type (enum (a b c))))
      "typedef enum{a,b,c} test_t;"
      (declare e (type (struct (a-b (function-pointer b c-e d)) (b i-nt))))
      "typedef struct{b(*a_b)(c_e,d);i_nt b;} e;"
      (declare a uint32_t (b ba bb) (void uint8_t uint32_t))
      "uint32_t a;void b(uint8_t ba,uint32_t bb);"
      (declare c (array uint8_t (3 4)) c (array uint8_t 5) c (array uint8_t (2) 0 0))
      "uint8_t c[3][4];uint8_t c[5];uint8_t c[2]={0,0};"
      (declare e (enum (ea eb ec)) d (struct (da (unsigned int))))
      "enum{ea,eb,ec};struct d{unsigned int da;};"
      (declare f (type uint8_t) g (type (struct (ga (unsigned int)))))
      "typedef uint8_t f;typedef struct{unsigned int ga;} g;"
      (declare (pre-concat h i) uint32_t)
      "uint32_t h##i;"
      (define a uint32_t 1)
      "uint32_t a=1"
      (define a uint32_t b+2 b16 c-3 void)
      "uint32_t a;b16 b_and_2;void c_3;"
      (define a (function-pointer (function-pointer (unsigned int) float) double))
      "unsigned int(*(*a)(double))(float)"
      (define a
        (function-pointer (function-pointer (function-pointer int float) double) (long long int)))
      "int(*(*(*a)(long long int))(double))(float)"
      (define (a) (function-pointer uint32_t uint64_t) #t)
      "uint32_t(*a())(uint64_t){1;}"
      (define (a b) ((function-pointer uint32-t uint64_t) (function-pointer uint32-t uint64_t)) #t)
      "uint32_t(*a(uint32_t(*b)(uint64_t)))(uint64_t){1;}"
      (define (a b) ((function-pointer (function-pointer uint32_t b-16) uint8_t) b-64))
      "uint32_t(*(*a(b_64 b))(uint8_t))(b_16)"
      (define (pre-concat a b) uint32_t 1)
      "uint32_t a##b=1"
      (define (abc) uint32_t (return 0))
      "uint32_t abc(){return(0);}"
      (define (abc d e) (uint32_t uint64_t b16) (return 0))
      "uint32_t abc(uint64_t d,b16 e){return(0);}"
      (define (abc d e) (uint32_t (pre-concat b 64) b16) (return 0))
      "uint32_t abc(b##64 d,b16 e){return(0);}"
      (define (a) void "test-docstring")
      "\n/** test-docstring */\nvoid a(){}"
      (define (a b c) (void void void) "test-docstring" (+ b c))
      "\n/** test-docstring */\nvoid a(void b,void c){(b+c);}"
      (do-while #t 1 2 3)
      "do{1;2;3;}while(1)"
      (enum (a b c d e))
      "enum{a,b,c,d,e}"
      (enum (a b (c 3) d (e 4)))
      "enum{a,b,c=3,d,e=4}"
      (enum test (a b c d e))
      "enum test{a,b,c,d,e}"
      (equal? a 1)
      "(a==1)"
      (function-pointer void vo-id*)
      "void(*)(vo_id*)"
      (if (= a 3) (exit 1) (return (bit-or b c)))
      "if((a==3)){exit(1);}else{return((b|c));}"
      (if 1 2 (begin 3 4 (return #t)))
      "if(1){2;}else{3;4;return(1);}"
      (if* a (if* b c d) e)
      "(a?(b?c:d):e)"
      (if* a (if* (if* b c) d) e)
      "(a?((b?c:0)?d:0):e)"
      (if* (= a 3) (begin (set b+c 4) (myproc a b+c)) a)
      "((a==3)?((b_and_c=4),myproc(a,b_and_c)):a)"
      (if* (not a) #t #f)
      "(!a?1:0)"
      (if* (not 1) a b)
      "(!1?a:b)"
      (label abc (define a uint32_t 3) (+ a b))
      "abc:uint32_t a=3;(a+b);"
      (let* ((a size_t 1) (b size_t 2) (c 3)) (set c 7) (return (if* 4 5 6)))
      "{size_t a=1;size_t b=2;c=3;c=7;return((4?5:6));}"
      (not 1)
      "!1"
      (pointer-get a 1)
      "(*(a+1))"
      (pointer-get (pointer-get a 1) 2)
      "(*((*(a+1))+2))"
      (pointer-get a-b)
      "(*a_b)"
      (pre-concat a b cd e)
      "a##b##cd##e"
      (pre-define a)
      "#define a"
      (pre-define (my-macro a b) (if* a #t #f))
      "#define my_macro(a,b) (a?1:0)"
      (pre-define (a) #t)
      "#define a() 1"
      (pre-define (a b) (begin "test-docstring" (+ b c) 3))
      "\n/** test-docstring */\n#define a(b) (b+c);\\\n  3\n"
      (pre-define ob-ject 3)
      "#define ob_ject 3"
      (pre-define a 1 (id a b) (= a b))
      "#define a 1\n#define id(a,b) (a==b)"
      (pre-define a 1 (id) b)
      "#define a 1\n#define id() b"
      (pre-define (->test a b) c)
      "#define _to_test(a,b) c"
      (pre-define-if-not-defined abc 3 def 4)
      "\n#ifndef abc\n#define abc 3\n#endif\n#ifndef def\n#define def 4\n#endif\n"
      (pre-if (equal? a b) (begin c d e) (begin f g))
      "#if (a==b)\nc;d;e;\n#else\nf;g;\n#endif"
      (pre-if-not-defined a b c)
      "#ifndef a\nb;\n#else\nc;\n#endif"
      (pre-if-defined a b c)
      "#ifdef a\nb;\n#else\nc;\n#endif"
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
      (pre-let (a 1 b 2) (+ a b))
      "#define a 1\n#define b 2\n(a+b);\n#undef a\n#undef b\n"
      (pre-let (a 1) a)
      "#define a 1\na;\n#undef a\n"
      (pre-let ((a b) 1) a)
      "#define a(b) 1\na;\n#undef a\n"
      (pre-let ((a b) 1 (c d) 2) a)
      "#define a(b) 1\n#define c(d) 2\na;\n#undef a\n#undef c\n"
      (pre-pragma once)
      "#pragma once\n"
      (pre-stringify abc)
      "#abc"
      (pre-undefine my-macro)
      "#undef my_macro\n"
      (quote "var a = 3")
      "var a = 3"
      (return)
      "return"
      (return 1 2)
      "return(1,2)"
      (set a 1)
      "a=1"
      (set a 1 b+2 2 c-3 3)
      "a=1;b_and_2=2;c_3=3;"
      (struct-get a b)
      "a.b"
      (struct-get a b c d e)
      "a.b.c.d.e"
      (struct-get (pointer-get a) b)
      "(*a).b"
      (struct (a (unsigned int)) (b (unsigned char) 3))
      "struct{unsigned int a;unsigned char b:3;}"
      (struct testname (a (uns-igned int)) (b (unsigned char) 3))
      "struct testname{uns_igned int a;unsigned char b:3;}"
      (struct (a-b (function-pointer b c-e d)) (b i-nt))
      "struct{b(*a_b)(c_e,d);i_nt b;}"
      (struct-literal (a 1) (b "2"))
      "{.a=1,.b=\"2\"}"
      (struct-literal a 1)
      "{a,1}"
      (struct-set a b 1 c 2)
      "a.b=1;a.c=2;"
      (struct-pointer-get a b)
      "(*a).b"
      (struct-pointer-get a b c d)
      "(*a).b.c.d"
      (struct-pointer-set a b 1 c 2)
      "(*a).b=1;(*a).c=2;"
      (union (a (unsigned int)) (b (unsigned char) 3))
      "union{unsigned int a;unsigned char b:3;}"
      (while #t 1 2 3)
      "while(1){1;2;3;}"
      )))
