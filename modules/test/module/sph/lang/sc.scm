(define-test-module (test module sph lang sc) (import (sph test) (sph lang sc))
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
      (if* (= a 3) (begin (set b+c 4) (myproc a b+c)) a)
      "((a==3u)?((b_and_c=4u),myproc(a,b_and_c)):a)" (if (= a 3) (exit 1) (return (bit-or b c)))
      "if((a==3u)){exit(1u);}else{return((b|c));}" (set a 1 b+2 2 c-3 3)
      "a=1u;b_and_2=2u;c_3=3u;" (if* (not a) #t #f)
      "(!a?1u:0u)" (define a b32 1)
      "b32 a=1u" (define a b32 b+2 b16 c-3 b0)
      "b32 a;b16 b_and_2;b0 c_3;" (set a 1)
      "a=1u" (deref a 1)
      "*(a+1u)" (deref (deref a 1) 2)
      "*(*(a+1u)+2u)" (not 1)
      "!1u" (if* (not 1) a b)
      "(!1u?a:b)" (and 1 2 3)
      "(1u&&2u&&3u)" (= 1 2 3)
      "(1u==2u==3u)" (equal? a 1)
      "(a==1u)" (begin #t)
      "1u;" (begin -1)
      "-1;" (begin 1 (begin 2 3))
      "1u;2u;3u;" (cond ((= a 1) (equal? b 2)) ((equal? c 3) #t))
      "if((a==1u)){(b==2u);}else{if((c==3u)){1u;}}"
      (cond* ((= a 1) (equal? b 2)) ((equal? c 3) #f #t) (else #t #f))
      "((a==1u)?(b==2u):((c==3u)?(0u,1u):(1u,0u)))" (quote "var a = 3")
      "var a = 3" (return)
      "return" (return 1 2)
      "return(1u,2u)" (if 1 2 (begin 3 4 (return #t)))
      "if(1u){2u;}else{3u;4u;return(1u);}" (deref a-b)
      "(*a_b)" (struct-ref a b)
      "a.b" (struct-ref (deref a) b)
      "(*a).b" (define-macro (my-macro a b) (if* a #t #f))
      "#define my_macro(a,b) (a?1u:0u)" (define-macro ob-ject 3)
      "#define ob_ject 3u" (pre-if (equal? a b) (begin c d e) (begin f g))
      "#if (a==b)\nc;d;e;\n#else\nf;g;\n#endif" (undefine-macro my-macro)
      "#undef my_macro\n" (pre-concat a b cd e)
      "a##b##cd##e" (pre-ifndef a b c)
      "#ifndef a\nb;\n#else\nc;\n#endif" (define-type mytype int)
      "typedef int mytype" (address-of a-b)
      "&a_b" (bit-shift-right 1 2)
      "(1u>>2u)" (bit-shift-left 1 2)
      "(1u<<2u)" (length size_t)
      "(8*sizeof(size_t))" (bit-not a-b)
      "~a_b" (struct (a unsigned int) (b unsigned char 3))
      "struct{unsigned int a;unsigned char b:3u;}"
      (struct testname (a unsigned int) (b unsigned char 3))
      "struct testname{unsigned int a;unsigned char b:3u;}"
      (struct (function-pointer a b c d) (b int)) "struct{b(*a)(c,d);int b;}"
      (union (a unsigned int) (b unsigned char 3)) "union{unsigned int a;unsigned char b:3u;}"
      (function-pointer f int char size_t) "int(*f)(char,size_t)"
      (function-pointer f (unsigned int) (unsigned char) size_t)
      "unsigned int(*f)(unsigned char,size_t)"
      (let-macro
        (a 1
          b 2)
        (+ a b))
      "#define a 1u\n#define b 2u\n(a+b);\n#undef a\n\n#undef b\n" (let-macro (a 1) a)
      "#define a 1u\na;\n#undef a\n" (let-macro ((a b) 1) a)
      "#define a(b) 1u\na;\n#undef a\n"
      (let-macro
        ( (a b) 1
          (c d) 2)
        a)
      "#define a(b) 1u\n#define c(d) 2u\na;\n#undef a\n\n#undef c\n"
      (let* ((a size_t 1) (b size_t 2) (c 3)) (set c 7) (return (if* 4 5 6)))
      "{size_t a=1u;size_t b=2u;c=3u;c=7u;return((4u?5u:6u));}" (define-macro (->test a b) c)
      "#define _to_test(a,b) c" (define-array aaa size-t 3)
      "size_t aaa[3u]" (define-array aaa size-t size-b)
      "size_t aaa[size_b]" (define-array aaa size-t size-b -1 2 test-c)
      "size_t aaa[size_b]={-1,2u,test_c}")))
