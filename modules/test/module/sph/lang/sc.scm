(define-test-module (test module sph lang sc)
  (import
    (sph test)
    (sph lang sc))

  (define-test (sc->c arguments) (sc->c arguments))

  (test-execute-procedures-lambda
    (sc->c (begin "") "\"\";"
      (begin "a") "\"a\";"
      (begin a?) "a_p;"
      (begin a-b) "a_b;"
      (begin a+b) "a_and_b;"
      (begin a->b) "a_to_b;"
      (begin a!) "a_x;"
      (if* a (if* b c d) e) "(a?(b?c:d):e)"
      (if* a (if* (if* b c) d) e) "(a?((b?c:0)?d:0):e)"
      (if* (= a 3) (begin (set b+c 4) (myproc a b+c)) a) "((a==3)?((b_and_c=4),myproc(a,b_and_c)):a)"
      (if (= a 3) (exit 1) (return (bit-or b c))) "if((a==3)){exit(1);}else{return((b|c));}"
      (set a 1 b+2 2 c-3 3) "a=1;b_and_2=2;c_3=3;"
      (if* (not a) #t #f) "(!a?1:0)"
      (define a b32 1) "b32 a=1"
      (define a b32 b+2 b16 c-3 b0) "b32 a;b16 b_and_2;b0 c_3;"
      (define (abc) b32 (return 0)) "b32 abc(){return(0);}"
      (define (abc d e) (b32 b64 b16) (return 0)) "b32 abc(b64 d,b16 e){return(0);}"
      (define (abc d e) (b32 (pre-concat b 64) b16) (return 0)) "b32 abc(b##64 d,b16 e){return(0);}"
      (set a 1) "a=1"
      (deref a 1) "*(a+1)"
      (deref (deref a 1) 2) "*(*(a+1)+2)"
      (not 1) "!1"
      (if* (not 1) a b) "(!1?a:b)"
      (and 1 2 3) "(1&&2&&3)"
      (= 1 2 3) "(1==2==3)"
      (equal? a 1) "(a==1)"
      (begin #t) "1;"
      (begin -1) "-1;"
      (begin 1 (begin 2 3)) "1;2;3;"
      (cond ((= a 1) (equal? b 2)) ((equal? c 3) #t)) "if((a==1)){(b==2);}else{if((c==3)){1;}}"
      (cond* ((= a 1) (equal? b 2)) ((equal? c 3) #f #t) (else #t #f))
      "((a==1)?(b==2):((c==3)?(0,1):(1,0)))" (quote "var a = 3")
      "var a = 3" (return)
      "return" (return 1 2)
      "return(1,2)" (if 1 2 (begin 3 4 (return #t)))
      "if(1){2;}else{3;4;return(1);}" (deref a-b)
      "(*a_b)" (struct-ref a b)
      "a.b" (struct-ref (deref a) b)
      "(*a).b" (struct-deref a b)
      "(*a).b" (pre-define (my-macro a b) (if* a #t #f))
      "#define my_macro(a,b) (a?1:0)" (pre-define ob-ject 3)
      "#define ob_ject 3" (pre-if (equal? a b) (begin c d e) (begin f g))
      "#if (a==b)\nc;d;e;\n#else\nf;g;\n#endif" (pre-undefine my-macro)
      "#undef my_macro\n" (pre-concat a b cd e)
      "a##b##cd##e" (pre-if-not-defined a b c)
      "#ifndef a\nb;\n#else\nc;\n#endif" (pre-if-defined a b c)
      "#ifdef a\nb;\n#else\nc;\n#endif" (define-type mytype int)
      "typedef int mytype"
      (define-function-pointer-type type-name type-return type-argument-1 type-argument-2)
      "typedef type_return(*type_name)(type_argument_1,type_argument_2)" (address-of a-b)
      "&a_b" (convert-type abc int)
      "((int)(abc))" ((convert-type abc function-t) d e)
      "((function_t)(abc))(d,e)" (bit-shift-right 1 2)
      "(1>>2)" (bit-shift-left 1 2)
      "(1<<2)" (length size_t)
      "(8*sizeof(size_t))" (bit-not a-b)
      "~a_b" (struct (a unsigned int) (b unsigned char 3))
      "struct{unsigned int a;unsigned char b:3;}"
      (struct testname (a unsigned int) (b unsigned char 3))
      "struct testname{unsigned int a;unsigned char b:3;}"
      (struct (function-pointer a b c d) (b int)) "struct{b(*a)(c,d);int b;}"
      (union (a unsigned int) (b unsigned char 3)) "union{unsigned int a;unsigned char b:3;}"
      (function-pointer f int char size_t) "int(*f)(char,size_t)"
      (function-pointer f (unsigned int) (unsigned char) size_t)
      "unsigned int(*f)(unsigned char,size_t)" (pre-let (a 1 b 2) (+ a b))
      "#define a 1\n#define b 2\n(a+b);\n#undef a\n\n#undef b\n" (pre-let (a 1) a)
      "#define a 1\na;\n#undef a\n" (pre-let ((a b) 1) a)
      "#define a(b) 1\na;\n#undef a\n" (pre-let ((a b) 1 (c d) 2) a)
      "#define a(b) 1\n#define c(d) 2\na;\n#undef a\n\n#undef c\n"
      (let* ((a size_t 1) (b size_t 2) (c 3)) (set c 7) (return (if* 4 5 6)))
      "{size_t a=1;size_t b=2;c=3;c=7;return((4?5:6));}" (pre-define (->test a b) c)
      "#define _to_test(a,b) c" (define-array aaa size-t 3)
      "size_t aaa[3]" (define-array aaa size-t size-b)
      "size_t aaa[size_b]" (define-array aaa size-t size-b -1 2 test-c)
      "size_t aaa[size_b]={-1,2,test_c};" (pre-include "./a/b.c")
      "#include \"./a/b.c\"\n" (pre-include "../a/b.c")
      "#include \"../a/b.c\"\n" (pre-include "a/b.c")
      "#include <a/b.c>\n" (pre-include "bb.h")
      "#include <bb.h>\n" (pre-include "a" "b" "c")
      "#include <a>\n#include <b>\n#include <c>\n" (pre-include-once a "./a.c" b "b.h")
      "#ifndef sc_included_a\n#include \"./a.c\"\n#define sc_included_a \n#endif\n#ifndef sc_included_b\n#include <b.h>\n#define sc_included_b \n#endif"
      (pre-include-once a "./a.c")
      "#ifndef sc_included_a\n#include \"./a.c\"\n#define sc_included_a \n#endif"
      (case = myvalue ((3 2) #t) (4 #f) (("a" "b") #t #t) (else #f #f))
      "if(((3==myvalue)||(2==myvalue))){1;}else{if((4==myvalue)){0;}else{if(((\"a\"==myvalue)||(\"b\"==myvalue))){1;1;}else{0;0;}}}"
      (case* = myvalue ((3 2) #t) (4 #f) (("a" "b") #t #t) (else #f #f))
      "(((3==myvalue)||(2==myvalue))?1:((4==myvalue)?0:(((\"a\"==myvalue)||(\"b\"==myvalue))?(1,1):(0,0))))"
      (enum test (a b c d e)) "enum test{a,b,c,d,e};"
      (enum (a b c d e)) "enum{a,b,c,d,e};"
      (enum (a b (c 3) d (e 4))) "enum{a,b,c=3,d,e=4};"
      (pre-stringify abc) "#abc"
      (array-literal 1 "2" 3 4) "{1,\"2\",3,4};"
      (struct-literal (a 1) (b "2")) "{.a=1,.b=\"2\"};" (struct-literal a 1) "{a,1};")))
