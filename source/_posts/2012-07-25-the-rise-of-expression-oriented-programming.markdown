---
status: published
title: The Rise of Expression-Oriented Programming
alias: /2012/07/25/the-rise-of-expression-oriented-programming.html
layout: post
---

I'm going to come right out and say, I love functional
programming. It is an awesome way to write programs. When I first
started to learn how to write programs in a functional way, I was
blown away by how concise, clear, and powerful functional solutions
can be for some problems. 

Rick Minerich, in his recent essay
"[Functional Programming is Dead, Long Live Expression-Oriented Programming](http://richardminerich.com/2012/07/functional-programming-is-dead-long-live-expression-oriented-programming/)", 
makes the point that a good functional language isn't about supporting
anonymous, making it easier. Its about *expressions*. And I couldn't
agree more.

"Functional programming" is really a loaded term. It invokes all sorts
of ideas such as "purity", high-order functions, parallelization, etc.
These topics are all *really* interesting, and exciting. However, for
day-to-day programming, most of these topics are too different than
the way existing systems work. 

Expression-oriented programming is a good middle-ground. 
Everything returns a value, and can be used as a component in an
expression. Each step in a computation is structured in a singular,
composable way. Each component is isolated from other components, and
yet can be seen in context. There is no need to give it more clout
than it naturally deserves.

The truth of this point is best illustrted with Ruby. Ruby feels
extremely functional. However, Ruby doesn't actually have "functions",
as we think of them. And that's totally fine! Ruby's
everything-as-an-expression model makes it really easy to write in a
functional way.

Coffeescript took Javascript -- already a very functional language,
when compared to most -- and turbocharged its "expression-oriented"
nature. Now, functional programming in Coffeescript is much easier and
more productive than in plain Javascript.  

So, I think Rick is onto something, and that the
"expression-oriented" term should be adopted. It simplifies the
discussion about functional programming, exposes flaws in existing
languages that try to glue functional features onto themselves, and
helps guide the discussion for future languages. All-in-all, a major
win for programming languages. 

