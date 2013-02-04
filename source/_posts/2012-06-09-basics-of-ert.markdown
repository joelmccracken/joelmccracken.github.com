---
status: published
layout: post
title: Fast Emacs Lisp Regression Test Framework Tutorial
alias: /2012/06/09/basics-of-ert.html
---

With Emacs 24 comes many changes, and one of the coolest changes is
the addition of ERT, the Emacs Regression Test framework. 

There are other ways of running tests on your Emacs Lisp code, but
many of them were lacking for one reason or another. Mostly, error
reporting was a problem, but ERT handles that excellently. ERT does
have some 
warts, but it provides a great base for writing tests. Plus, it is now
in Emacs proper, so it is basically the standard way to write tests
now.  

# Acquiring and Set Up #

I hope you are using Emacs 24. I have been using it for months, and I
have not had problems with it. If you do have Emacs 24, then you
already have ERT installed. 

However, you may still be on a different version of Emacs. If so, you
can acquire ERT from 
[its old github page](https://github.com/ohler/ert). 
Download these files and ensure that they are located somewhere in
your load-path. 

Either way, assuming you now have ERT, wherever you want to use it,
include

    (require 'ert)

to get Emacs to load it. You can include this in your `.emacs`, for
example. 


# Getting Started #

A basic test that shows the components to a test:

    (ert-deftest my-first-project-test ()
      :tags '(my-first-project)
      
      (message "first test is running"))


Evaluate this and run it via `M-x ert-run-tests-interactively`. Enter `(tag my-first-project)`
at the prompt, and press return. This will run the tests that are
tagged with my-first-project. 

Lets edit this test to include something the we should actually test. 

    (ert-deftest my-first-project-test ()
      :tags '(my-first-project)
      
      (should (equal (+ 1 1)
                     2)))

Here, we introduce the `(should)` macro. Whatever is returned via the
forms within that macro should be non-nil. Otherwise, the test will
fail and raise an error. 


There are a few other should macros for you to use: 

* `should-not` raises an error if the forms do *not* return nil. 
* `should-error` is useful for testing that something does raise an
  erorr when we want it to. `should-error` also accepts a `:type`
  argument which allows us to be specific about the error. 



Basically, that's it. Ert comes with an info file that has more
specifics about what is going on, but this is enough to get you
started writing tests for your Emacs Lisp code. 
