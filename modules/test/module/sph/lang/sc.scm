
(define-test-module (test module sph lang sc)
  (import
    (sph test)
    (sph lang sc)
    (ice-9 match))

  (define-test (sc->c arguments) (sc->c arguments))

  (define-test (sc-define-syntax a)
    (match a
      ( (id pattern expansion application)
        (sc-define-syntax-scm id pattern expansion)
        (sc-syntax-expand id application))))

  (define-test (sc-define-syntax-scm a)
    (match a
      ((id pattern expansion application)
        (sc-define-syntax-scm id pattern expansion)
        (sc-syntax-expand id application))))

  (test-execute-procedures-lambda
    (sc->c
      (unless #f 1 2)
      "if(!0){1;2;}"
      (1+ 3)
      "(3+1)"
      (1- 3)
      "(3-1)"
      (when #t 1 2)
      "if(1){1;2;}"
      (/ 2)
      "(1/2)"
      (/ (+ 1 2))
      "(1/(1+2))"
      (- (+ x y))
      "(-(x+y))"
      (+ (+ x y))
      "(+(x+y))"
      (sc-define-syntax (x a (b ...) body) (define a (b ...) body))
      ""
      (declare a (array point-t 2 (1 2) (3 4)))
      "point_t a[2]={{1,2},{3,4}};"
      (array-set* a 2 3 4)
      "a[0]=2;a[1]=3;a[2]=4;"
      (struct (pre-concat a b) (c (struct (pre-concat a b*))))
      "struct a##b{struct a##b* c;}"
      (declare a (array (struct b) 3))
      "struct b a[3];"
      (declare a (array (long unsigned int) 3))
      "long unsigned int a[3];"
      (begin #\newline)
      "'\\n';"
      (begin a--b)
      "a__b;"
      (begin *a.b)
      "*(a.b);"
      (struct-get (pointer-get a) b)
      "(*a).b"
      (struct-get (a b) c)
      "(a(b)).c"
      (*a b)
      "(*a)(b)"
      (: ab cd)
      "ab->cd"
      (= a 1)
      "(a==1)"
      (and (= 1 2 3) (= 1 2))
      "(((1==2)&&(2==3))&&(1==2))"
      (address-of a-b)
      "&a_b"
      (and 1 2 3)
      "(1&&2&&3)"
      (and a (set b (c d)))
      "(a&&(b=c(d)))"
      (array-get a 1)
      "a[1]"
      (array-get (array-get a 1) 2)
      "(a[1])[2]"
      (array-get aaa 3)
      "aaa[3]"
      (array-get aaa 3 4 5)
      "aaa[3][4][5]"
      (array-get *a 3)
      "(*a)[3]"
      (array-literal 1 "2" 3 4)
      "{1,\"2\",3,4}"
      (array-set aa 0 11 1 22 3 33)
      "aa[0]=11;aa[1]=22;aa[3]=33;"
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
      (begin a->b)
      "a_to_b;"
      (begin a!)
      "a_x;"
      (begin a!?<->)
      "a_x_p_less_to_;"
      (begin <)
      "<;"
      (begin &a *a a.b)
      "&a;*a;a.b;"
      (begin #t)
      "1;"
      (begin -1)
      "-1;"
      (begin *a.field)
      "*(a.field);"
      (begin &*a.field)
      "&*(a.field);"
      (begin &*a:b:c)
      "&*(a->b->c);"
      (begin 1 (begin 2 3))
      "1;2;3;"
      (begin (enum (a b c d e)) (declare a int))
      "enum{a,b,c,d,e};int a;"
      (begin ab:cd:ef)
      "ab->cd->ef;"
      (begin ab:cd)
      "ab->cd;"
      (bit-shift-right 1 2)
      "(1>>2)"
      (bit-shift-left 1 2)
      "(1<<2)"
      (bit-not a-b)
      "~a_b"
      (case = myvalue ((3 2) #t) (4 #f) (("a" "b") #t #t) (else #f #f))
      "if((3==myvalue)||(2==myvalue)){1;}else if(4==myvalue){0;}else if((\"a\"==myvalue)||(\"b\"==myvalue)){1;1;}else{0;0;}"
      (cond ((= a 1) #t))
      "if(a==1){1;}"
      (cond ((= a 1) (= b 2)) ((= c 3) #t))
      "if(a==1){(b==2);}else if(c==3){1;}"
      (cond ((= a 1) (= b 2)) ((= c 3) #t) (else 4))
      "if(a==1){(b==2);}else if(c==3){1;}else{4;}"
      (cond* ((= a 1) (= b 2)) ((= c 3) #f #t) (else #t #f))
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
      (declare a (struct test))
      "struct test a;"
      (declare a (struct (test int)))
      "struct a{int test;};"
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
      (declare h (struct-variable ha 0 0))
      "ha h={0,0};"
      (declare (pre-concat h i) uint32_t)
      "uint32_t h##i;"
      (define a uint32_t 1)
      "uint32_t a=1"
      (define a uint32_t 1 b uint64_t 2)
      "uint32_t a=1;uint64_t b=2"
      (declare a (function-pointer (function-pointer (unsigned int) float) double))
      "unsigned int(*(*a)(double))(float);"
      (declare a
        (function-pointer (function-pointer (function-pointer int float) double) (long long int)))
      "int(*(*(*a)(long long int))(double))(float);"
      (define (a) (function-pointer uint32_t uint64_t) #t)
      "uint32_t(*a())(uint64_t){1;}\n"
      (define (a b) ((function-pointer uint32-t uint64_t) (function-pointer uint32-t uint64_t)) #t)
      "uint32_t(*a(uint32_t(*b)(uint64_t)))(uint64_t){1;}\n"
      (define (a b) ((function-pointer (function-pointer uint32_t b-16) uint8_t) b-64))
      "uint32_t(*(*a(b_64 b))(uint8_t))(b_16)"
      (define (pre-concat a b) uint32_t 1)
      "uint32_t a##b=1"
      (define (abc) uint32_t (return 0))
      "uint32_t abc(){return(0);}\n"
      (define (abc d e) (uint32_t uint64_t b16) (return 0))
      "uint32_t abc(uint64_t d,b16 e){return(0);}\n"
      (define (abc d e) (uint32_t (pre-concat b 64) b16) (return 0))
      "uint32_t abc(b##64 d,b16 e){return(0);}\n"
      (define (a) void "test-docstring")
      "\n/** test-docstring */\nvoid a()"
      (define (a b) (c d) "e")
      "\n/** e */\nc a(d b)"
      (define (a b c) (void void void) "test-docstring" (+ b c))
      "\n/** test-docstring */\nvoid a(void b,void c){(b+c);}\n"
      (do-while #t 1 2 3)
      "do{1;2;3;}while(1)"
      (enum (a b c d e))
      "enum{a,b,c,d,e}"
      (enum (a b (c 3) d (e 4)))
      "enum{a,b,c=3,d,e=4}"
      (enum test (a b c d e))
      "enum test{a,b,c,d,e}"
      (for ((set index 0) (< index len) (set index (+ 1 index))) #t)
      "for(index=0;(index<len);index=(1+index)){1;}"
      (for (((set a 0) (set b 1)) (< index len) ((set a (+ 1 a)) (set b (+ 2 b)))) #t)
      "for(a=0,b=1;(index<len);a=(1+a),b=(2+b)){1;}"
      (for ((begin a b) (< c d) (begin e f)) #t)
      "for(a,b;(c<d);e,f){1;}"
      (function-pointer void vo-id*)
      "void(*)(vo_id*)"
      (if (= a 3) (exit 1) (return (bit-or b c)))
      "if(a==3){exit(1);}else{return((b|c));}"
      (if 1 2 (begin 3 4 (return #t)))
      "if(1){2;}else{3;4;return(1);}"
      (if* a (if* b c d) e)
      "(a?(b?c:d):e)"
      (if* a (if* (if* b c) d) e)
      "(a?((b?c:0)?d:0):e)"
      (if* (= a 3) (begin (set b-c 4) (myproc a b-c)) a)
      "((a==3)?((b_c=4),myproc(a,b_c)):a)"
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
      (pointer-get a-b)
      "*a_b"
      (pointer-get (a b))
      "*(a(b))"
      (pointer-get b)
      "*b"
      (pointer-get b.c)
      "*(b.c)"
      (pre-concat a b cd e)
      "a##b##cd##e"
      (pre-cond ((= a b) 1))
      "#if (a==b)\n1;\n#endif"
      (pre-cond ((= a b) 1) (c (pre-define a)) (else 2))
      "#if (a==b)\n1;\n#elif c\n#define a\n#else\n2;\n#endif"
      (pre-cond-defined (a 1) (b 2))
      "#ifdef a\n1;\n#elif b\n2;\n#endif"
      (pre-cond-not-defined (a 1))
      "#ifndef a\n1;\n#endif"
      (pre-define a)
      "#define a"
      (pre-define (my-macro a b) (if* a #t #f))
      "#define my_macro(a,b) (a?1:0)"
      (pre-define (a) #t)
      "#define a() 1"
      (pre-define (a b) (begin "test-docstring" (+ b c) 3))
      "\n/** test-docstring */\n#define a(b) (b+c);3\n"
      (pre-define ob-ject 3)
      "#define ob_ject 3"
      (pre-define a 1 (id a b) (= a b))
      "\n#define a 1\n#define id(a,b) (a==b)\n"
      (pre-define a 1 (id) b)
      "\n#define a 1\n#define id() b\n"
      (pre-define (->test a b) c)
      "#define _to_test(a,b) c"
      (pre-if (= a b) (begin c d e) (begin f g))
      "#if (a==b)\nc;d;e;\n#else\nf;g;\n#endif"
      (pre-if-not-defined a b c)
      "#ifndef a\nb;\n#else\nc;\n#endif"
      (pre-if-defined a b c)
      "#ifdef a\nb;\n#else\nc;\n#endif"
      (pre-include "./a/b.c")
      "#include \"./a/b.c\""
      (pre-include "../a/b.c")
      "#include \"../a/b.c\""
      (pre-include "a/b.c")
      "#include <a/b.c>"
      (pre-include "bb.h")
      "#include <bb.h>"
      (pre-include "a" "b" "./c")
      "#include <a>\n#include <b>\n#include \"./c\""
      (pre-let* (a 1 b 2) (+ a b))
      "#define a 1\n#define b 2\n(a+b);\n#undef a\n#undef b\n"
      (pre-let* (a 1) a)
      "#define a 1\na;\n#undef a\n"
      (pre-let* ((a b) 1) a)
      "#define a(b) 1\na;\n#undef a\n"
      (pre-let* ((a b) 1 (c d) 2) a)
      "#define a(b) 1\n#define c(d) 2\na;\n#undef a\n#undef c\n"
      (pre-pragma once)
      "#pragma once\n"
      (pre-stringify abc)
      "#abc"
      (pre-undefine my-macro)
      "#undef my_macro\n"
      (return)
      "return"
      (return 1 2)
      "return(1,2)"
      (sc-insert "var a = 3")
      "var a = 3"
      (set *a *b.c)
      "*a=*(b.c)"
      (set a 1)
      "a=1"
      (set a 1 b-2 2 c-3 3)
      "a=1;b_2=2;c_3=3;"
      (set a:b (: *a b))
      "a->b=(*a)->b"
      (struct-get a b)
      "a.b"
      (struct-get a b c d e)
      "a.b.c.d.e"
      (struct-get (pointer-get a) b)
      "(*a).b"
      (struct-get **a b)
      "(**a).b"
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
      "a->b"
      (struct-pointer-get a b c d)
      "a->b->c->d"
      (struct-pointer-set a b 3 c 4)
      "a->b=3;a->c=4;"
      (union (a (unsigned int)) (b (unsigned char) 3))
      "union{unsigned int a;unsigned char b:3;}"
      (while #t 1 2 3)
      "while(1){1;2;3;}"
      (while (not (= 0 (set a (b c)))) #t)
      "while(!(0==(a=b(c)))){1;}"
      (sc-comment "abc")
      "\n/* abc */\n"
      (sc-comment "abc" "def" "ghi")
      "/* abc\ndef\nghi */\n"
      (!= 1 2 3)
      "((1!=2)&&(2!=3))"
      (begin (sc-no-semicolon (a 1)) (set b 2))
      "a(1)\nb=2;"
      (begin (sc-no-semicolon (a 1) (set b 2)))
      "a(1)\nb=2\n"
      (begin (pre-define a (begin (define (a) void 1))) (declare b int))
      "\n#define a void a(){1;}\nint b;"
      (begin (pre-define (a b) (define (c) void 1)) (a "xyz"))
      "\n#define a(b) void c(){1;}\na(\"xyz\")\n"
      (set+ a 1 b 2)
      "a+=1;b+=2;"
      (set- a 1)
      "a-=1"
      (set* a 1)
      "a*=1"
      (set/ a 1)
      "a/=1"
      (declare a (type (struct (b (array int 3)))))
      "typedef struct{int b[3];} a;"
      (pre-define-if-not-defined abc 3 def 4)
      "\n#ifndef abc\n#define abc 3\n#endif\n#ifndef def\n#define def 4\n#endif\n"
      (pre-define (a) (begin 1 (sc-comment "b") 2 3))
      "#define a() 1;\\\n/* b */\\\n2;3"
      (case* = myvalue ((3 2) #t) (4 #f) (("a" "b") #t #t) (else #f #f))
      "(((3==myvalue)||(2==myvalue))?1:((4==myvalue)?0:(((\"a\"==myvalue)||(\"b\"==myvalue))?(1,1):(0,0))))"
      ; bug with missing newline after pre-define
      (begin
        (pre-define (a) (begin "test" b) c d)
        (declare e f))
      "\n/** test */\n#define a() b\n#define c d\nf e;"
      ; sub expressions need to be joined with commas in these contexts
      (for ( (set a 1 b 2) #t (set c 3 d 4)) #t)
      "for(a=1,b=2;1;c=3,d=4){1;}"
      (if* #t (set a 1 b 2) 0)
      "(1?(a=1,b=2):0)"
      (define (a b) (void (sc-insert "test")))
      "void a(test b)"
      (define (a b) (void (array-type double 3)))
      "void a(double b[3])"
      (sc-concat type *)
      "type*"
      (pre-include-guard-begin test-h) "#ifndef test_h\n#define test_h\n"
      (pre-include-guard-end) "#endif")
    (sc-define-syntax-scm
      (test* (a b ...) (unquote (lambda (state compile a b) (cons* 0 a b))) (1 2 3))
      (0 1 2 3))
    (sc-define-syntax
      (id (x ((a b) ...) body ...) (x ((a ...) (b) ...) body ...) (1 ((2 3) (4 5)) b1 b2))
      (1 ((2 4) (3) (5)) b1 b2)
      (id ((name data ...) ...) (a (b name data ...) ...) ((c 1 2 3) (d 4 5 6)))
      (a (b c 1 2 3) (b d 4 5 6)))
))
