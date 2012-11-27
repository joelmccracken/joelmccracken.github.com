---
status: idea
layout: post
title: 'Lisp\'s dirty secret'
---

I have been a Lisp supporter for half a decade now. Learning Lisp
changed my life as a programmer. Since then, I've looked at the world
with a Lisp point of view, and it has been very satisfying.
There is a rather large problem, though. 
The more I write in Lisp along with other languages
the more obvious this problem becomes. The thing is, this is a problem
that is not really discussed in the Lisp community. It tends to be
scoffed at and dismissed. Before we get into the details of the
problem, we need to set the stage.

If you pay attention to what lispers perspective of the history of
programming languages, you'll see a progression of programming
languages that started off with generations of algol-based programming
languages slowly adopting lisp's features. As of 2012, the only
feature left that makes Lisp systems, well, Lisp, is having homoiconic
syntax -- that is, support for macros. Now, because you are reading
this, I assume you are familiar with 
macros. It is worth quickly going over the concept, just to make sure
we are on the same page.

Programming systems that have macros let the programmer do all sorts
of awesome magic. Basically, whatever syntax the system "supports" can
be transformed, allowing the programmer control over what that syntax
does. The homoiconic property refers to the code being a data type
that is native to the system.

Macros are awesome. They let the programmer extend the language,
giving them first-class access to basically whatever the interpreter
has. Macros are certainly worth having. Any forward-facing language
should have some kind of syntax-rewriting facilities.
These problems really belong in another post, and have been said
elsewhere. The thing is, the Lisp community has thought about this for
a long time. Best practice is to avoid using macros unless there is
some a really compelling reason to use them. In practice, this often
means that in any given program,
you are likely to never create a macro.
This point is important.
The syntax of Lisp -- that is, s-expressions, the odd parenthesis
syntax -- is designed around macro facilities that
many programmers do.not.use. And, when they are used, they are used
infrequently.

This would be acceptable if there was no downside to using
s-expressions. However, there are downsides. And no, they have nothing
to do with lots of irritating, superfluous parentheses. I promise that
I will try to get to the fundamental issues I have with parentheses in
another post, but for the sake of argument just accept that
s-expressions tend to force your application to take a shape that is
contrary to the way you think about it.

The thing is, we don't need s-expressions for a programming system to
support macros. 
As far as I understand it, the value of s-expressions lie in the
obviousness of how they will parse. If you look at a given
s-expression, it is pretty easy to understand how the interpreter
will parse it. However, we have computers that make it easy to
understand how an s-expression will be interpreted.


We can get any
programming language to support macros. It just takes a bit of
effort. For a very basic macro system implementation, we need 
to process the syntax of source code, change the given syntax
according to rules, and then output the 
new code. There are nuances that make macro systems better and
more user-friendly, but we can still create syntax rewriters without
them.

This fact, more than any other, has turned my attentions away from
Lisp. I can't ignore the irritation that
s-expressions cause. Ruby, by contrast, already naturally has a 
form of metaprogramming that makes many kinds of macros
unnecessary. 


This is a good time to mention that I have started developing a Macro
system for Ruby. Some of it is inspired by 
what @raganwald developed, but I am trying to take it down a different
route. 
