(load "pc.scm")

(define <boolean>
  (new
   (*parser (char #\#))

   (*parser (char-ci #\t))
   (*parser (char-ci #\f))
   (*disj 2)

   (*caten 2)
   (*delayed (lambda () <symbol>))
   *not-followed-by
   (*pack-with (lambda (sh b)
                 (char-ci=? b #\t) ))
   done))

(define <charprefix>
  (new
   (*parser (char #\#))
   (*parser (char #\\))
   (*caten 2)
   done))

(define <visiblesimplechar>
  (const (lambda (c)
           (char>? c #\space))))

(define <whitespace>
  (new
   (*parser <any>)
   (*parser <visiblesimplechar>)
   *diff
   done))

(define <whitespacenonewline>
  (new
   (*parser <any>)

   (*parser <visiblesimplechar>)
   (*parser (char #\newline))
   (*disj 2)
   *diff

   done))

(define <lambda>
  (new
   (*parser (char-ci #\l))
   (*parser (char-ci #\a))
   (*parser (char-ci #\m))
   (*parser (char-ci #\b))
   (*parser (char-ci #\d))
   (*parser (char-ci #\a))
   (*caten 6)
   (*pack (lambda (_) #\λ))
   done))

(define <newline>
  (new
   (*parser (char-ci #\n))
   (*parser (char-ci #\e))
   (*parser (char-ci #\w))
   (*parser (char-ci #\l))
   (*parser (char-ci #\i))
   (*parser (char-ci #\n))
   (*parser (char-ci #\e))
   (*caten 7)
   (*pack (lambda (_) #\newline))
   done))

(define <nul>
  (new
   (*parser (char-ci #\n))
   (*parser (char-ci #\u))
   (*parser (char-ci #\l))
   (*caten 3)
   (*pack (lambda (_) #\nul))
   done))

(define <page>
  (new
   (*parser (char-ci #\p))
   (*parser (char-ci #\a))
   (*parser (char-ci #\g))
   (*parser (char-ci #\e))
   (*caten 4)
   (*pack (lambda (_) #\page))
   done))

(define <return>
  (new
   (*parser (char-ci #\r))
   (*parser (char-ci #\e))
   (*parser (char-ci #\t))
   (*parser (char-ci #\u))
   (*parser (char-ci #\r))
   (*parser (char-ci #\n))
   (*caten 6)
   (*pack (lambda (_) #\return))
   done))

(define <space>
  (new
   (*parser (char-ci #\s))
   (*parser (char-ci #\p))
   (*parser (char-ci #\a))
   (*parser (char-ci #\c))
   (*parser (char-ci #\e))
   (*caten 5)
   (*pack (lambda (_) #\space))
   done))

(define <tab>
  (new
   (*parser (char-ci #\t))
   (*parser (char-ci #\a))
   (*parser (char-ci #\b))
   (*caten 3)
   (*pack (lambda (_) #\tab))
   done))

(define <namedchar>
  (new
   (*parser <lambda>)
   (*parser <newline>)
   (*parser <nul>)
   (*parser <page>)
   (*parser <return>)
   (*parser <space>)
   (*parser <tab>)
   (*disj 7)
   done))

(define <hexchar>
  (new
   (*parser (range #\0 #\9))
   (*parser (range-ci #\a #\f))
   (*disj 2)
   done)) 

(define <hexunicodechar>
  (new
   (*parser (char-ci #\x))

   (*parser <hexchar>)
   *plus

   (*caten 2)
   (*pack-with (lambda (x c)
                 (integer->char (string->number (list->string c) 16))))
   done))

(define <char>
  (new
   (*parser <charprefix>)

   (*parser <namedchar>)
   (*parser <hexunicodechar>)
   (*parser <visiblesimplechar>)
   (*disj 3)

   (*caten 2)
   (*pack cadr)
   done))

(define <natural>
  (new
   (*parser (range #\0 #\9))
   *plus
   done))

(define <integer>
  (new
   (*parser (char #\+))
   (*parser (char #\-))
   (*disj 2)
   *maybe
   (*pack-with (lambda (? s)
                 (if ?
                     (list s)
                     '())))

   (*parser <natural>)

   (*caten 2)
   (*pack-with append)
   done))

(define <fraction>
  (new
   (*parser <integer>)
   (*parser (char #\/))
   (*parser <natural>)
   (*caten 3)
   (*pack-with (lambda (int / nat) `(,@int ,/ ,@nat)))
   done))

(define <number>
  (new
   (*parser <fraction>)
   (*parser <integer>)
   (*disj 2)
   (*pack (lambda (n)
            (string->number (list->string n))))
   (*delayed (lambda () <infixsymbol>))
   *not-followed-by
   done))

(define <stringliteralchar>
  (new
   (*parser <any>)
   (*only-if (lambda (c)
	             (not (or (char=? c #\\)
			                  (char=? c #\")))))
   done))

(define <stringmetachar>
  (new
   (*parser (char #\\))

   (*parser (char #\\))
   (*parser (char #\"))
   (*parser (char-ci #\t))
   (*parser (char-ci #\f))
   (*parser (char-ci #\n))
   (*parser (char-ci #\r))
   (*disj 6)

   (*caten 2)
   (*pack-with (lambda (_ c)
                 (cond ((char-ci=? c #\t) #\tab)
                       ((char-ci=? c #\f) #\page)
                       ((char-ci=? c #\n) #\newline)
                       ((char-ci=? c #\r) #\return)
                       (else c))))
   done))


(define <stringhexchar>
  (new
   (*parser (char #\\))
   (*parser <hexunicodechar>)
   (*parser (char #\;))

   (*caten 3)
   (*pack cadr)
   done))   

(define <stringchar>
  (new
   (*parser <stringhexchar>)
   (*parser <stringmetachar>)
   (*parser <stringliteralchar>)
   (*disj 3)
   done))

(define <string>
  (new
   (*parser (char #\"))

   (*parser <stringchar>)
   *star

   (*parser (char #\"))

   (*caten 3)
   (*pack-with (lambda (fdq str sdq)
		             (list->string str)))
   done))

(define <symbolcharnodigits>
  (new
   (*parser (range #\a #\z))
   (*parser (range #\A #\Z))
   (*pack (lambda (c)
            (integer->char (+ (char->integer c) (char- #\a #\A)))))
   (*parser (const (lambda (e)
                     (member e '(#\: #\! #\$ #\^ #\* #\- #\_ #\= #\+ #\< #\> #\? #\/)))))
   (*disj 3)
   done))

(define <symbolchar>
  (new
   (*parser (range #\0 #\9))
   (*parser <symbolcharnodigits>)
   (*disj 2)
   done))

(define <symbol>
  (new
   (*parser <symbolchar>)
   *plus
   (*pack (lambda (e)
            (string->symbol (list->string e))))
   done))

(define <properlist>
  (new
   (*parser (char #\())
   
   (*parser <whitespace>)
   *star
   
   (*parser (char #\)))

   (*caten 2)
   (*pack (lambda (_) '()))

   (*delayed (lambda () <sexpr>))
   *star

   (*parser (char #\)))

   (*caten 2)
   (*pack car)

   (*disj 2)
   (*caten 2)
   (*pack cadr)
   done))

(define <improperlist>
  (new
   (*parser (char #\())

   (*parser <whitespace>)
   *star

   (*delayed (lambda () <sexprnowhitespace>))

   (*parser <whitespace>)
   *plus

   (*caten 2)
   (*pack car)
   *plus
   (*pack-with (lambda l (apply append l)))

   (*parser (char #\.))
   (*parser <whitespace>)
   (*delayed (lambda () <sexpr>))
   (*parser (char #\)))

   (*caten 7)
   (*pack-with (lambda (l w1 e1 d w2 e2 r) `(,@e1 . ,e2)))
   done))

(define <vector>
  (new
   (*parser (char #\#))
   (*parser <properlist>)
   (*caten 2)
   (*pack-with (lambda (s l) (list->vector l)))
   done))

(define <quoted>
  (new
   (*parser (char #\'))
   (*delayed (lambda () <sexpr>))
   (*caten 2)
   (*pack-with (lambda (q s) (list 'quote s)))
   done))

(define <quasiquoted>
  (new
   (*parser (char #\`))
   (*delayed (lambda () <sexpr>))
   (*caten 2)
   (*pack-with (lambda (q s) (list 'quasiquote s)))
   done))

(define <unquoted>
  (new
   (*parser (char #\,))
   (*delayed (lambda () <sexpr>))
   (*caten 2)
   (*pack-with (lambda (q s) (list 'unquote s)))
   done))

(define <unquoteandspliced>
  (new
   (*parser (char #\,))
   (*parser (char #\@))
   (*delayed (lambda () <sexpr>))
   (*caten 3)
   (*pack-with (lambda (q at s) (list 'unquote-splicing s)))
   done))

(define <cbnamesyntax1>
  (new
   (*parser (char #\@))
   (*delayed (lambda () <sexpr>))
   (*caten 2)
   (*pack cadr)
   done))

(define <cbnamesyntax2>
  (new
   (*parser (char #\{))
   (*delayed (lambda () <sexpr>))
   (*parser (char #\}))
   (*caten 3)
   (*pack cadr)
   done))

(define <cbname>
  (new
   (*parser <cbnamesyntax1>)
   (*parser <cbnamesyntax2>)
   (*disj 2)
   (*pack (lambda (s) `(cbname ,s)))
   done))

(define <infixprefixextensionprefix>
  (new
   (*parser (char #\#))

   (*parser (char #\#))
   (*parser (char #\%))
   (*disj 2)

   (*caten 2)
   done))

(define <infixneg>
  (new
   (*parser (char #\-))
   (*delayed (lambda () <infixexpression>))
   (*caten 2)
   (*pack-with (lambda (_ e)
                 `(- ,e)))
   done))

(define <infixaddandsub>
  (new
   (*parser <whitespace>)
   *star

   (*delayed (lambda () <infixmulanddiv>))
   (*parser <infixneg>)
   *diff

   (*parser <whitespace>)
   *star

   (*parser (char #\+))
   (*parser (char #\-))
   (*disj 2)
   (*pack (lambda (c)
	          (string->symbol (list->string (list c)))))

   (*parser <whitespace>)
   *star

   (*delayed (lambda () <infixmulanddiv>))
   (*parser <infixneg>)
   *diff

   (*caten 4)
   (*pack-with (lambda (w1 op w2 e)
                 (list op e)))
   *star

   (*caten 2)
   (*pack-with (lambda (first rest)
		             (infix->prefix first rest)))

   (*parser <whitespace>)
   *star

   (*caten 3)
   (*pack cadr)
   done))

(define <infixmulanddiv>
  (new
   (*delayed (lambda () <infixpow>))

   (*parser <whitespace>)
   *star

   (*parser (char #\*))
   (*parser (char #\/))
   (*disj 2)
   (*pack (lambda (c)
	    (string->symbol (list->string (list c)))))

   (*parser <whitespace>)
   *star

   (*delayed (lambda () <infixpow>))

   (*caten 4)
   (*pack-with (lambda (w1 op w2 e)
                 (list op e)))
   *star

   (*caten 2)
   (*pack-with (lambda (first rest)
		             (infix->prefix first rest)))
   done))

(define <powersymbol>
  (new
   (*parser (char #\^))
   
   (*parser (char #\*))
   (*times 2)

   (*disj 2)
   (*pack (lambda (_) 'expt))
   done))

(define <infixpow>
  (new
   (*delayed (lambda () <infixarraygetandfuncall>))
   (*delayed (lambda () <infixatomic>))
   (*disj 2)

   (*parser <whitespace>)
   *star

   (*parser <powersymbol>)

   (*parser <whitespace>)
   *star

   (*delayed (lambda () <infixarraygetandfuncall>))
   (*delayed (lambda () <infixatomic>))
   (*disj 2)

   (*caten 4)
   (*pack-with (lambda (w1 op w2 e)
                 (list op e)))
   *star

   (*caten 2)
   (*pack-with (lambda (first rest)
		             (infix->prefix first rest)))
   done))

(define <infixarglist>
  (new
   (*delayed (lambda () <infixexpression>))

   (*parser (char #\,))
   (*delayed (lambda () <infixexpression>))
   (*caten 2)
   (*pack cadr)
   *star

   (*caten 2)
   (*pack-with (lambda (first rest)
                 (cons first rest)))

   (*parser <whitespace>)
   *star
   (*pack (lambda (w) '()))

   (*disj 2)
   done))

(define <infixarraygetandfuncall>
  (new
   (*delayed (lambda () <infixatomic>))

   (*parser (char #\())
   (*delayed (lambda () <infixarglist>))
   (*parser (char #\)))
   (*caten 3)
   (*pack-with (lambda (l e r)
                 (cons '() e)))

   (*parser (char #\[))
   (*delayed (lambda () <infixexpression>))
   (*parser (char #\]))
   (*caten 3)
   (*pack-with (lambda (l e r)
                 `(vector-ref ,e)))

   (*disj 2)
   *plus
   (*caten 2)
   (*pack-with (lambda (e1 e2)
       (infix->prefix e1 e2)))
   done))

(define <infixparen>
  (new
   (*parser (char #\())
   (*delayed (lambda () <infixexpression>))
   (*parser (char #\)))
   (*caten 3)
   (*pack cadr)
   done))

(define <infixsexprescape>
  (new
   (*parser <infixprefixextensionprefix>)
   (*delayed (lambda () <sexpr>))
   (*caten 2)
   (*pack cadr)
   done))

(define <opsymbol>
  (new
   (*parser (const (lambda (e)
                     (member e '(#\^ #\* #\- #\+ #\/)))))
   
   (*parser (char #\*))
   (*times 2)

   (*disj 2)
   done))

(define <infixsymbolchar>
  (new
   (*parser <symbolchar>)
   (*parser <opsymbol>)
   *diff
   done))

(define <infixsymbol>
  (new
   (*parser <infixsymbolchar>)
   *plus
   (*pack (lambda (e)
            (string->symbol (list->string e))))
   done))

(define <infixatomic>
  (new
   (*parser <whitespace>)
   (*delayed (lambda () <infixlinecomment>))
   (*delayed (lambda () <infixexprcomment>))
   (*disj 3)
   *star

   (*parser <infixsexprescape>)
   (*parser <infixparen>)
   (*parser <number>)
   (*parser <infixsymbol>)
   (*disj 4)

   (*parser <whitespace>)
   (*delayed (lambda () <infixlinecomment>))
   (*delayed (lambda () <infixexprcomment>))
   (*disj 3)
   *star

   (*caten 3)
   (*pack cadr)
   done))

(define <linecomment>
  (new
   (*parser (char #\;))

   (*delayed (lambda () <sexprwithwhitespacenonewline>))
   *star

   (*parser (char #\newline))
   (*caten 3)
   done))

(define <sexprcomment>
  (new
   (*parser (char #\#))
   (*parser (char #\;))
   (*delayed (lambda () <sexpr>))
   (*caten 3)
   done))

(define <infixlinecomment>
  (new
   (*parser (char #\;))

   (*delayed (lambda () <infixexpressionwithwhitespacenonewline>))
   *star

   (*parser (char #\newline))
   (*caten 3)
   done))

(define <infixexprcomment>
  (new
   (*parser (char #\#))
   (*parser (char #\;))
   (*delayed (lambda () <infixexpression>))
   (*caten 3)
   done))

(define <infixexpressioncore>
  (new
   (*parser <infixsexprescape>)
   (*parser <infixneg>)
   (*parser <infixaddandsub>)
   (*parser <infixmulanddiv>)
   (*parser <infixpow>)
   (*parser <infixneg>)
   (*parser <infixarraygetandfuncall>)
   (*parser <infixatomic>)
   (*disj 8)
   done))

(define <infixexpressionwithwhitespace>
  (new
   (*parser <whitespace>)
   (*parser <infixexprcomment>)
   (*parser <infixlinecomment>)
   (*disj 3)
   *star

   (*parser <infixexpressioncore>)

   (*parser <whitespace>)
   (*parser <infixexprcomment>)
   (*parser <infixlinecomment>)
   (*disj 3)
   *star

   (*caten 3)
   (*pack cadr)

   done))

(define <infixexpressionwithwhitespacenonewline>
  (new
   (*parser <whitespacenonewline>)
   (*parser <infixexprcomment>)
   (*parser <infixlinecomment>)
   (*disj 3)
   *star

   (*parser <infixexpressioncore>)

   (*parser <whitespacenonewline>)
   (*parser <infixexprcomment>)
   (*parser <infixlinecomment>)
   (*disj 3)
   *star

   (*caten 3)
   (*pack cadr)

   done))

(define <infixexpression> <infixexpressionwithwhitespace>)

(define <infixextension>
  (new
   (*parser <infixprefixextensionprefix>)
   (*parser <infixexpression>)
   (*caten 2)
   (*pack cadr)
   done))

(define <sexprcore>
  (new
   (*parser <boolean>)
   (*parser <char>)
   (*parser <number>)
   (*parser <symbol>)
   (*parser <string>)
   (*parser <properlist>)
   (*parser <improperlist>)
   (*parser <vector>)
   (*parser <quoted>)
   (*parser <quasiquoted>)
   (*parser <unquoteandspliced>)
   (*parser <unquoted>)
   (*parser <cbname>)
   (*parser <infixextension>)
   (*disj 14)
   done))

(define <sexprwithwhitespace>
  (new
   (*parser <whitespace>)
   (*parser <sexprcomment>)
   (*parser <linecomment>)
   (*disj 3)
   *star

   (*parser <sexprcore>)

   (*parser <whitespace>)
   (*parser <sexprcomment>)
   (*parser <linecomment>)
   (*disj 3)
   *star

   (*caten 3)
   (*pack cadr)
   done))

(define <sexprwithwhitespacenonewline>
  (new
   (*parser <whitespacenonewline>)
   (*parser <sexprcomment>)
   (*parser <linecomment>)
   (*disj 3)
   *star

   (*parser <sexprcore>)

   (*parser <whitespacenonewline>)
   (*parser <sexprcomment>)
   (*parser <linecomment>)
   (*disj 3)
   *star

   (*caten 3)
   (*pack cadr)
   done))

(define <sexprnowhitespace>
  (new
   (*parser <sexprcomment>)
   (*parser <linecomment>)
   (*disj 2)
   *star

   (*parser <sexprcore>)

   (*parser <sexprcomment>)
   (*parser <linecomment>)
   (*disj 3)
   *star

   (*caten 2)
   (*pack cadr)
   done))

(define <sexpr> <sexprwithwhitespace>)

(define infix->prefix
  (lambda (first rest)
    (if (null? rest)
        first
        (letrec ((op (caar rest))
                 (second (if (null? (cdar rest)) '() (cadar rest)))
                 (loop (lambda (acc rest)
                         (if (null? rest)
                             acc
                             (let ((op (caar rest))
                                   (second (if (null? (cdar rest)) '() (cadar rest))))
                               (cond ((null? op)
                                      (loop (cons acc (cdar rest)) (cdr rest)))
                                     ((eq? op 'expt)
                                      (list op acc (infix->prefix second (cdr rest))))
                                     (else
                                      (loop (list op acc second) (cdr rest)))))))))
          (cond ((null? op)
                 (loop (cons first (cdar rest)) (cdr rest)))
                ((eq? op 'expt)
                 (list op first (infix->prefix second (cdr rest))))
                (else
                 (loop (list op first second) (cdr rest))))))))
                    
(define flatten
  (lambda (l)
  (cond ((null? l) '())
        ((pair? l) (append (flatten (car l)) (cadr l)))
          (else (list l)))))
