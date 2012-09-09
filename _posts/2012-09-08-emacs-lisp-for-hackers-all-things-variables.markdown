---
layout: post
title: 'Emacs Lisp For Hackers: All Things Variables'
---

# Emacs Lisp for Hackers #
## All Things Variables ##



Time to get into Variables. 

If you are used to most programming languages, setting and updating
variables is the way you operate. There are a few ways this happens in
Emacs Lisp, but the most fundamental way is via `setq`. 

The special form `setq` is *extremely* simple. It sets the value of a
symbol, which can be referenced later: 

    ELISP> (setq x 10)
    10
    ELISP> (+ x x)
    20
    

There is another, similar function: `defvar`. 
The special form `defvar` is similar in spirit to setq.
It allows you to define special variables, give them default values if
they do not already have values, and provide documentation for what
that variable "means". Some examples: 

    (defvar user-name "Joel" 
      "The name of the person currently using Emacs.")
      
    (defvar pomodoro-done-hook nil
      "Hook that gets done whenever a pomodoro is finished.")

    (defvar my-favorite-numbers '(4 8 15 16 23 42))



Use `defvar` whenever you want to "declare" a variable to have some
special, global meaning. 



Emacs Lisp is not the most opinionated of Lisps, so it lets you do all
the imperative programming that you want. However, a cleaner style of
handling variables is `let`:

    ELISP> (let ((x 10))
             (+ x x))
    20

Let takes a list of symbol and value bindings, and then evaluates
body, which represents the remaining arguments to `let`.


Another slightly more difficult example should help clarify the concept

    ELISP> (let ((x 10))
             (list (let ((x "hi"))
                     x)
                   x))
    ("hi" 10)

This example illustrates how `let` changes bindings. There are two
`let` calls in this fragment. The outer call sets the value of the
symbol `x` to 10. After this happens, the body call occurs. Within the
call to `list` is a second `let` form. This `let` binds "hi" to the
symbol `x`, which is then returned from the `let`. This value ("hi")
then gets made into a list, along with the original value of x, which
was 10. Both of these then get returned, which gives us `("hi" 10")`. 

The bindings list for `let` can take multiple bindings, which will all
then be available when the body is executed: 

    ELISP> (let ((a "Heya")
                 (b "worldz"))
             (concat a " " b))
    "Heya worldz"
    

There is another form of `let`, called `let*`, which makes each
binding available to 
the next binding when it is evaluated. This is actually extremely
useful, but when I first encountered this idea the usefulness was not
immediately apparent. So, lets have one final example, shall we?
Lets look at a quick regular expression example: 

    ELISP> (let* ((filename "2012-09-10-a-blog-post.markdown")
                  (file-without-extension-rx "\\(.*\\).markdown")
                  (regular-filename (progn
                                      (string-match file-without-extension-rx filename)
                                      (match-string 1 filename))))
             (message "The filename sans extension was: %s" regular-filename))
    "The filename sans extension was: 2012-09-10-a-blog-post"

Notice how the `regular-filename` value comes from a compound
expression, and that the arguments two both function calls come from
the previous bindings. This is really, really useful in practice. 





Well, that's about it for the basics of Emacs Lisp. With the Info
documentation and the documentation for the various parts of Emacs,
you should be equipped to hack the good hacks.  

Of course, I'm not done helping you learn Emacs Lisp. There are many areas of the Emacs
APIs that can use some simplification. And then, there is the
reorganization thing that will happen when this series
gets rolled into book form. So, stay tuned. 
