---
status: published
layout: post
title: 'Emacs Lisp for Hackers: Hello World'
---

# Emacs Lisp for Hackers: Hello World #

This post is part of an ongoing series to try to make the Emacs Lisp
learning curve less steep, which I introduced [here][elisp introduction].


So far, you should be able to read Elisp and understand how to get
around Emacs' documentation. Now, its time to get into writing real
Emacs Lisp. 


## Saying "Hello, World" in Emacs ##

Lets get right to it: our very first, very minimal function. Run `M-x
elm` to open up an 
elisp interpreter, and evaluate the following code: 

    (defun say-hello ()
        "hello")

Next, run it via `(say-hello)`. Your \*ielm\* buffer should look
something like this:


    ELISP> (defun say-hello ()
             "Heya, World!")
    say-hello
    ELISP> (say-hello)
    "Heya, World!"


This function definition is very simple. 
It declares a function (via `defun`) named
`say-hello`
This function takes no arguments (signified by the empty parameter
list `()`). 
It returns the string "Heya, World!". Easy. 
Lets do the same thing, but *slightly* differently this time: 

    (defun say-hello ()
       "Says hello."
       (interactive)
       (message "Hello, World!"))

Lets slow down a bit and explain these new features. They are still
easy, but do not necessarily directly translate from experience
programming in other language

The first piece we come to is the documentation string, "Says Hello."
Documentation strings are used to document what a function does. The
function `describe-function` displays the documentation string of the
function that is being described.

Other things also get documentation strings, such as variables with
the `defvar` declaration. We will revisit `defvars` later, though. 

The code `(interactive)` signifies that this function should be
considered a *command*; that is, it should be user-invocable. This
command can be invoked via "M-x say-hello", and it can also be bound
to a keyboard key.

We begin to see the power and flexibility of Emacs with interactive
functions. A user can easily create new commands; they need not be
constrained to what the authors thought he would need, or use. More on
this later. 

Finally, we use `message` to say "Hello, World!" The function
`message` is used to send a message to the user. The string passed
 to `message` will be displayed in the echo area and output to the
 `*Messages*` buffer for reference. Its a really handy little function
for telling the user about all sorts of things. 



We're almost done with the basic introduction to Emacs and Emacs
lisp. At this point, you can get really far by understanding how to
build your own functions, look at the way other functions are built,
and experimenting. Next time, we will look at variables, and the
various ways to use and define them. 




[elisp introduction]: /2012/08/28/the-little-emacs-lisp-book-for-hackers.html
