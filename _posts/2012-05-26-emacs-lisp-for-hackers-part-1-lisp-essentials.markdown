---
status: published
title: "Emacs Lisp for hackers -- Part 1: Lisp Essentials"
alias: /2012/05/26/emacs-lisp-for-hackers-part-1-lisp-essentials.html
layout: post
---

I [previously wrote]() about the reasons to learn Emacs Lisp. That covered
the _why_ well enough, but the _how_ is still another question.

There is plenty of information out there about Elisp. However, as I
began to learn, I found that none of it took the form that I like when
I learn something. I like to have a basic guide that covers everything
that I need to know, and has pointers on where to find more
information next. Exhaustive manuals are great for reference, but not
good for teaching. I do not want to sit
down and read a manual from front to back, and only then start to
give it some meaning. I cannot remember all of that, so I need to use
the manual as a refernce anyway. I can't prioritize what
I am reading without real world context, as most of it wont make a ton
of sense until I have internalized some of the system.

All a hacker really needs to know is enough to get
comfortable, and some guidance over the various gotchas on the
platform. I personally prefer a "layered" approach to
learning, which alternates between exploration, instruction, and
creation. That is the style that I would like to try to follow.

So, I assume that you are a programmer, and that you know nothing
about functional programming or Lisp. If you do not know how to
program, then I suggest you look into document that would be more
suited to you, [An Introduction to Programming in Emacs
Lisp](http://www.gnu.org/software/emacs/manual/html_node/eintr/index.html)
This first post covers the very basics of lisp.

# Getting Started #

First of all, you need to know how to evaluate Emacs Lisp. For this
tutorial, at first I suggest you use ielm, the interactive Emacs Lisp
interpreter.

To get started, open Emacs. Type `M-x ielm`, and then press
return. A new buffer should appear which looks like:

    *** Welcome to IELM ***  Type (describe-mode) for help.
    ELISP>

Now enter:

    (message "hullo middle earth")

and press return. If your console looks something like below, you are
read to continue.

    *** Welcome to IELM ***  Type (describe-mode) for help.
    ELISP> (message "hullo middle earth")
    "hullo middle earth"
    ELISP>

For now, you use ielm to experiment with Emacs Lisp.


# Introduction to Lisp #

This tutorial is specific to Emacs Lisp, but really is basically applicable to
any lisp in general. As such, I will be referring to "Lisp" for a
little while. If you have already done some Lisp programming,
feel free to skip to the next section (Introduction to Emacs
Lisp). You can always come back here to review.

## Numbers ##

In Lisp, an integer looks like this:

    ELISP> 1
    1
    ELISP> 3
    3
    ELISP> 5435
    5435

That is all you need to know for a long time.

More:

[Emacs Lisp Reference Manual: Numbers](http://www.gnu.org/software/emacs/manual/html_node/elisp/Numbers.html#Numbers)



## Strings ##

Lisp strings are double quoted, only. No single quotes.

    ELISP> "this is a string"
    "this is a string"


More:

[Emacs Lisp Reference Manual: Strings](http://www.gnu.org/software/emacs/manual/html_node/elisp/Strings-and-Characters.html#Strings-and-Characters)


## Symbols ##

A symbol is a name for something. Just as names are
essentially strings, so are symbols just essentially strings.
Symbols are used to reference a variety of things within Lisp,
espeically to values and functions.

    ELISP> my-symbol
    *** Eval error ***  Symbol's value as variable is void: my-symbol
    ELISP> 'my-symbol
    my-symbol

If you notice, the first expression gave an error. It recognized that
"my-symbol" was a symbol, but it tried to evaluate it. Since
"my-symbol" doesn't refer to anything here, evaluating it doesn't make
sense.

Afterwards, I "quoted" my symbol with the single quotation mark and
the symbol name. That meant just return the symbol, and do not
evaluate it. We will cover quoting in a minute, but first, we need to
talk about evaluation

More:

[Emacs Lisp Reference Manual: Symbols](http://www.gnu.org/software/emacs/manual/html_node/elisp/Symbols.html#Symbols)


## Evaluation ##

Evaluation is the process of taking an expression and "evaluating"
it.

We have already seen what happens when we evaluate a number,
a string, a symbol, and a quoted symbol:

    ELISP> 56
    56
    ELISP> "hullo"
    "hullo"
    ELISP> a-symbol
    *** Eval error ***  Symbol's value as variable is void: a-symbol
    ELISP> 'a-symbol
    a-symbol

Evaluation is the process of taking an expression and figuring out
what it "means".

More:
[Emacs Lisp Reference Manual: Evaluation](http://www.gnu.org/software/emacs/manual/html_node/elisp/Evaluation.html#Evaluation)


### calling functions ###

In Lisp, calling a function looks like:

    (function-name arg1 arg2 arg3)

So, the following calls the function "+" with the arguments 1 and 2.

    ELISP> (+ 1 2)
    3

Thus, evaluating a list (a list is indicted by the parentheses) means
"apply these arguments to this function".

[Emacs Lisp Reference Manual: Evaluation of Function Forms](http://www.gnu.org/software/emacs/manual/html_node/elisp/Function-Forms.html#Function-Forms)

### quotation ###

Sometimes it is convenient to _not_ evaluate code. Lets imagine that you
want to create a list of the first three numbers.

    ELISP> (1 2 3)
    *** Eval error ***  Invalid function: 1

That was an error because when emacs evaluates that statement, since
it is a list, it thinks that this is a function call.

Of course, we can create a list using a regular function:

    ELISP> (list 1 2 3)
    (1 2 3)

However, this gets unwieldly:

    ELISP> (list (list 1) (list 2) (list 3))
    ((1)
     (2)
     (3))

So, for convenience, Lisp gives the ability to "quote" an expression,
preventing it from being evaluated. The easiest way to do that is with
the single quotation mark, '.

    ELISP> '((1) (2) (3))
    ((1)
     (2)
     (3))

Quoting makes it much, much easier to reference things within your code.

More:

[Emacs Lisp Reference Manual: Quoting](http://www.gnu.org/software/emacs/manual/html_node/elisp/Quoting.html#Quoting)

---

Next time, we will talk about some specifics of Emacs Lisp, which
should hopefully be enough to enable you to understand most Emacs Lisp
that you come accross. But for now, this covers the essentials of a
Lisp program, and you should already be equipped to "read" an Emacs Lisp
program while relying heavily on a reference.
