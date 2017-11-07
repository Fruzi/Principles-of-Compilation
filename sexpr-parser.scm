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

(define <symbolchar>
  (new
   (*parser (range #\0 #\9))
   (*parser (range-ci #\a #\z))
   (*parser (const (lambda (e)
                     (member e '(#\! #\$ #\^ #\* #\- #\_ #\= #\+ #\< #\> #\? #\/)))))
   (*disj 3)
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
   
   (*delayed (lambda () <sexpr>))
   *star
   
   (*parser (char #\)))

   (*caten 3)
   (*pack cadr)
   done))

(define <improperlist>
  (new
   (*parser (char #\())

   (*delayed (lambda () <sexpr>))
   *plus

   (*parser (char #\.))
   (*delayed (lambda () <sexpr>))
   (*parser (char #\)))

   (*caten 5)
   (*pack-with (lambda (l ss d s r) `(,@ss . ,s)))
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

(define <infixadd>
  (new
   (*delayed (lambda () <infixexpression>))
   (*parser (char #\+))
   (*delayed (lambda () <infixexpression>))
   (*caten 3)
   (*pack-with (lambda (e1 _ e2)
                 `(+ ,e1 ,e2)))
   done))

(define <infixneg>
  (new
   (*parser (char #\-))
   (*delayed (lambda () <infixexpression>))
   (*caten 2)
   (*pack-with (lambda (_ e)
                 `(- ,e)))
   done))

(define <infixsub>
  (new
   (*delayed (lambda () <infixexpression>))
   (*parser (char #\-))
   (*delayed (lambda () <infixexpression>))
   (*caten 3)
   (*pack-with (lambda (e1 _ e2)
                         `(- ,e1 ,e2)))
   done))

(define <infixmul>
  (new
   (*delayed (lambda () <infixexpression>))
   (*parser (char #\*))
   (*delayed (lambda () <infixexpression>))
   (*caten 3)
   (*pack-with (lambda (e1 _ e2)
                 `(* ,e1 ,e2)))
   done))

(define <infixdiv>
  (new
   (*delayed (lambda () <infixexpression>))
   (*parser (char #\/))
   (*delayed (lambda () <infixexpression>))
   (*caten 3)
   (*pack-with (lambda (e1 _ e2)
                         `(/ ,e1 ,e2)))
   done))

(define <powersymbol>
  (new
   (*parser (char #\^))
   
   (*parser (char #\*))
   (*times 2)

   (*disj 2)
   done))

(define <infixpow>
  (new
   (*delayed (lambda () <infixexpression>))
   (*parser <powersymbol>)
   (*delayed (lambda () <infixexpression>))
   (*caten 3)
   (*pack-with (lambda (e1 _ e2)
                         `(expt ,e1 ,e2)))
   done))

(define <infixarrayget>
  (new
   (*delayed (lambda () <infixexpression>))
   (*parser (char #\[))
   (*delayed (lambda () <infixexpression>))
   (*parser (char #\]))
   (*caten 4)
   (*pack-with (lambda (e1 _ e2 __)
                         `(vector-ref ,e1 ,e2)))
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
   (*pack-with (lambda (first rest) `(,first ,@rest)))
   done))

(define <infixfuncall>
  (new
   (*delayed (lambda () <infixexpression>))
   (*parser (char #\())

   (*parser (char #\)))
   (*pack (lambda (_) '()))

   (*parser <infixarglist>)
   (*parser (char #\)))
   (*caten 2)
   (*pack car)

   (*disj 2)
   (*caten 3)
   (*pack-with (lambda (e l a)
                 (cons e a)))
   done))


(define <infixparen>
  (new
   (*parser (char #\())
   (*delayed (lambda () <infixexpression>))
   (*parser (char #\)))
   (*caten 3)
   done))

(define <infixsexprescape>
  (new
   (*parser <infixprefixextensionprefix>)
   (*delayed (lambda () <sexpr>))
   (*caten 2)
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

(define <mathop>
  (new
   (*parser <infixpow>)
   (*parser <infixmul>)
   (*parser <infixdiv>)
   (*parser <infixadd>)
   (*parser <infixsub>)
   (*disj 5)
   done))

(define <infixexpression>
  (new
   (*parser <whitespace>)
   *star

   (*parser <number>)
   (*parser <infixsymbol>)
   (*parser <mathop>)
   (*parser <infixneg>)
   (*parser <infixarrayget>)
   (*parser <infixfuncall>)
   (*parser <infixparen>)
   (*parser <infixsexprescape>)
   (*disj 8)

   (*parser <whitespace>)
   *star

   (*caten 3)
   (*pack cadr)

   done))

(define <infixextension>
  (new
   (*parser <infixprefixextensionprefix>)
   (*parser <infixexpression>)
   (*caten 2)
   (*pack cadr)
   done))

(define <sexpr>
  (new
   (*parser <whitespace>)
   *star
   
   (*parser <boolean>)
   (*parser <char>)
   (*parser <number>)
   (*parser <string>)
   (*parser <symbol>)
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

   (*parser <whitespace>)
   *star

   (*caten 3)
   (*pack cadr)
   done))
