(require (lib "plt-match.ss"))

;; Playing with patterns
#|
Pattern matching is really quite easy; it sits at the heart of Haskell,
ML, and Erlang. In Scheme, it's a library, but because of the powerful
macro system, it looks like it is part of the language.

In Scheme, the code looks like its own data. So the definition

(define x 3)

which defines 'x' to be 3 in the current environment looks a lot like

'(define x 3)

which is a list with two symbols and a number. This is shorthand for

(list 'define 'x 3)

which makes it a bit more obvious that we're building a list of symbols and 
a number. 

Another thing we can do is use quasiquote to build lists. There is a difference
between

'(+ 1 (+ 2 3))

and

`(+ 1 (+ 2 3))

These are both lists of symbols and numbers. That is, the '+' has no 
meaning---it is just a symbol. So, in some respects, these 
lists are identical. In fact, if we asked

(equal? '(+ 1 (+ 2 3))
        `(+ 1 (+ 2 3)))

The answer would be #t (or true). You can paste that expression into
a DrScheme interaction pane (the bottom half) to test that.

The difference, though, is this:

`(+ 1 ,(+ 2 3))

the comma means "unquote". This evaluates to the list

'(+ 1 5)

and again, you can test this:

(equal? '(+ 1 5)
        `(+ 1 ,(+ 2 3)))

You are familiar with this from PHP; you might write

<html>
<body>
<?php echo("Body text"); ?>
</body>
</html>

The code in <?php ... ?> is evaluated, while the
rest of the document is not. In the same way, we could write

`(html
  (body
    ,(echo "Some body text")))

and the function call to 'echo' would be evaluated, while
the rest of the document would not be.

Quasiquoting matters because we use it to construct patterns, 
as you will see below.

References:

TSPL v3 Index
http://www.scheme.com/tspl3/tspl_1.html#./tspl:h0

Quote
http://www.scheme.com/tspl3/start.html#./start:s47

Quasiquote
http://www.scheme.com/tspl3/objects.html#./objects:s5

Now, to deconstruct a list, we could use functions that extract the head
and tail of the list ('car' and 'cdr'), or we could use a pattern matcher.
I'm going to write (and comment up) a function that matches a number
of different cases.
|#

(define (match-define-and-two-things expr)
  (match expr
    ;; This is a quasiquoted pattern. It says we should
    ;; match a list starting with the symbol 'define,
    ;; and then we will store the next two things (of any type)
    ;; in the variables 'var' and 'num'. The fact that we might
    ;; expect a variable and a number here is not currently 
    ;; guaranteed.
    [`(define ,var ,num)
      ;; The ~a in the print string means 'anything'. I then 
      ;; pass it the two variables to substitute for the two '~a's.
      (printf "Found these things: '~a' and '~a'~n" var num)]
    ;; Really, this case should throw an error, not just print.
    ;; But an error would halt execution... 
    ;; '~s' is like '~a', but shows strings as strings.
    [any (printf "mdatt: Didn't find two things in: ~s~n" any)]))
;; A short form, for testing
(define mdatt match-define-and-two-things)

;; Testing this.
;; These work; in each case, convince youself of why.
(mdatt '(define x 3))
(mdatt '(define a b))
(mdatt '(define "Hi" "there"))
(mdatt '(define (define x 3) (define a b)))
;; These do not
;; In each case, convince yourself of why.
(mdatt '("define" x 3))
(mdatt '(define x 3 5))
(mdatt '(defin x 3))

;; Exercise: Try to extend the pattern matcher
;; above to include each of the three cases that currently
;; do not work. To be fair, the first case is
;; actually a pain, and really requires techniques
;; from later to do this correctly. In other words,
;; don't stress over the one with the string in it.

#|
Now, for parsing, we need to be able to ask questions
about the content of our patterns. For example, in my
running '(define x 3) example, it would be nice to constrain
the first variable slot to a symbol, and the second slot to a 
number.  This is possible with the pattern matching library 
we're using ("plt-match.ss").
|#

(define (with-types expr)
  (match expr
    ;; Here, we say we want to match the symbol 'define'
    ;; followed by two things, which we will store in the variables
    ;; 'var' and 'num'. However, we use one of the syntaxes of the matcher
    ;; (look up 'plt match' in the Help Desk) that allows us to substitute
    ;; the rule
    ;;
    ;; (? <fun> <var>)
    ;;
    ;; anywhere we want to match something. The <fun> is a function
    ;; of one argument that returns true or false. The second is 
    ;; the variable that we will store something in if the condition <fun>
    ;; holds. (Note, this is not the limit to how we can use the (? ...) form,
    ;; but it will do for getting on with.
    ;;
    ;; So, specifically, we say we want to match
    ;; `(define ,var ,num)
    ;; but insist that 'var' is a symbol, and 'num' is a number.
    [`(define
        ,(? symbol? var)
        ,(? number? num))
      (printf "Matched two things, with types: ~a, ~a~n" var num)]
    ;; Again, a non-failing error so you can hit 'Run' and see things happen.
    ;; Note that this is a pattern match, not a magic else; it's just matching
    ;; anything and everything, and storing it in the variable 'otherwise'.
    ;; Note, the '~s' is like '~a', but shows strings as strings.
    [otherwise (printf "wt: Couldn't match in: ~s~n" otherwise)]))
;; Just for now
(define wt with-types)

;;These work
(wt '(define x 3))
(wt '(define y 5))
(wt `(define z ,(+ 3 5)))
(wt `(define ,(begin 3 'what?) 8))

;; These do not
(wt '(define x y))
(wt '(define 3 x))
(wt '(define z "Hi"))
(wt '(define q 0 0))

;; In all cases, convince yourself as to why these 
;; do or do not work. When you feel you understand
;; why these do (or do not) work, extend the pattern
;; matcher to take these into account. The predicate
;; for testing if something is a string is 'string?'.
;; Unlike the previous question, you don't need to
;; know anything other than what you can infer from the example
;; to extend this matcher.

#|
More complex patterns
---
Now, we've worked with a single list of symbols,
strings, and numbers. In truth, the number of 
//types// of data that we'll have to deal with
in the parser is few: almost everything is a symbol.
In fact, everything is an object: a syntax object, to be
precise. What those objects contain---symbols, strings,
or numbers---is another thing entirely.

Put this way (using Java syntax for a moment) the 
line of code

public void foo(int x)

would be three symbols ('public', 'void', and 'foo')
followed by a list of two symbols, 'int' and 'x'. The fact
that some are identifiers, some are types, and some are
formal variables is meaning that we attach to them---but
to the parser, they are just symbols. 

We'll get to dealing with the syntax objects soon enough. They
add a layer of complexity, but no more than any other kind of
object---they're a layer of indirection, that is all.

In this next section, we'll pull apart some lists that are
more complex in structure, then repeating patterns, and then
a combination of the two.
|#

;; Here, we're going to pull apart some lists that
;; contain sublists. In truth, there is nothing more 
;; complex here than has already been seen.

(define (with-sublists expr)
  (match expr
    [`(define ,id ,body)
      (format "Found the ID: ~s, and BODY: ~s" id body)]
    [`(lambda (,formal) ,body)
      (format "Found var: ~s, body: ~s" formal body)]
    [`(let ([,var ,exp]) ,body)
      (format "Binding ~s to ~s in the expression ~s" var exp body)]
    [any (printf "No match found: ~s~n" expr)]))
(define ws with-sublists)

;;These should work
(ws '(define x 3))
(ws '(define y (lambda (x) (+ x x))))
(ws '(define a-longer-name 5))
(ws '(lambda (x) (+ x x)))
(ws '(lambda (x) (let ([y 5]) (+ x y))))
(ws '(lambda (y) y))
(ws '(lambda (x) (x x)))
(ws '(let ([x 3]) (+ x 5)))
;; These should fail.
(ws '(define (foo x) (+ x x)))
(ws '(lambda (x y) (+ x y)))
(ws '(let ([x 3][y 5]) (+ x y)))
(ws '(lambda x (apply + x)))

;; All of the previous expressions should
;; be explored, and you should be convinced why
;; the existing patterns work. You should also
;; be confident why the patterns that fail, fail. 
;; When you're confident you understand the current
;; behavior, extend the matcher to include the
;; tests that fail.

;; Now, you may have been saying to your self 
;; "Wasn't that all Scheme?" The answer is "Yes, it was."
;; Those expressions looked like Scheme, but actually,
;; they were just lists of symbols and numbers. 
;; In this particular case, you were writing a pattern matcher
;; for Scheme expressions.

;; For bonus credit, make changes to with-sublists
;; so that the body of any define, lambda, or let 
;; is processed as well. That is, the following
;; tests should pass. You'll also need
;; to add a pattern for addition.

;; These should all return #t.
(equal? 
 (ws '(define x (lambda (y) (+ y y))))
 "Found the ID: x, and BODY: \"Found var: y, body: \\\"Add y and y\\\"\"")
(equal?
 (ws '(define (foo x) (let ([y 5]) (+ x y))))
 "Found the ID: (foo x), and BODY: \"Binding y to 5 in the expression \\\"Add x and y\\\"\"")
(equal?
 (ws '(define (bar y) (lambda (x) (+ x y))))
 "Found the ID: (bar y), and BODY: \"Found var: x, body: \\\"Add x and y\\\"\"")

;; Matching one or more
;; We may want to match zero, one, or more things,
;; and capture hold of them in a variable. For example,
;; What if I want to capture all the elements of a list?
;; Or all the variables in a lambda declaration?
;; This is easily done, fortunately.
(define (many expr)
  (match expr
    [`(plus ,a ,b) (+ a b)]
    [`(plus ,args ...)
      (apply + args)]
    [`(print ,a1 ,a2 ,rest ...)
      (printf 
       (string-append
        "First arg: ~a~n"
        "Second arg: ~a~n"
        "Rest of args: ~a~n")
       a1 a2 rest)]
    [`(lambda (,formals ...) ,body)
      (format "Formals: ~a~nBODY: ~a~n" formals body)]
    [`(count-me args ..0)
      (printf "Number of args: ~a~n" (length args))]
    [`(count-more ,args ..2)
      (printf "Number (at least two): ~a~n" (length args))]
    [any (printf "No match: ~a~n" any)]))

;; These should all come out as #t or print something
;; reasonable/what might be expected.
(equal? 8 (many '(plus 3 5)))
(equal? 55 (many '(plus 1 2 3 4 5 6 7 8 9 10)))
(many '(print one two and the rest of the arguments))
(printf "~a" (many '(lambda (x y z) (+ x y z))))
(equal? 3 (many '(count-me a b c)))
(equal? 3 (many '(count-me 1 2 3)))
(equal? 3 (many '(count-more do re me)))
(equal? 8 (many '(count-more do re me fa so la ti do)))

;; Determine for yourself that you understand what each of these
;; patterns, and what the expressions that are exercising them, do.
;; Once you've explored this material, write some tests that cause this
;; code to fail. In some cases, you won't be able to break the 
;; pattern (because the ... is a greedy match), but you'll be able
;; to break the code that gets executed as a result of a match.
;; In other cases, you'll be able to break the pattern.
;; The danger of globs like the '...' pattern is that you can
;; sometimes match things that you don't intend to.

;; I'm tired, and think that the last section goes too far, too fast.
;; Perhaps not. Either way, I'm stopping for now.
