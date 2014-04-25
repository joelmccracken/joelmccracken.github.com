---
status: published
layout: post
title: 'The Idea as the Fundamental Unit of Code Composition'
alias: /2012/12/16/the-idea-as-the-fundamental-unit-of-programming.html
---

I love code. Regardless of what happens with my life, I will always
code. If, tomorrow, I found that I was suddenly very independently
wealthy, I would still code. 

This passion pushes me to improve. One of the most frustrating things
is to need to rearchitect something for no good reason. So, I am
always looking for ways to 
improve my code. I read books 
and blogs, and try to actively apply the things that I have learned in
the code that I write. Typically, I love all the advice I have gotten
from various sources over the years, and my code has improved
dramatically. 

There is something that has always bothered me about programming advice,
though. 
This was a topic that I touched on in the 
[second phase of learning to program](http:/2012/08/26/the-two-phases-of-learning-to-program.html).
From my recollection, all advice on how to write good code 
follows the same general principles. A model object might do too much, and
thus need to be split apart. A controller might be doing something
"more" than simply coordinating the models and the views, and needs to
have that logic removed. A function might just get too large, and
should then be split apart. A database might have a denormalized schema,
which makes updating difficult, and thus should be normalized.

These guidelines feel really similar to one another. This is good,
because it probably means they make sense. It would be suspicious if
we were getting wildly different advice from different sources. 
However, this also suggests existence of an underlying, unifying
principle. Does such a principle exist?

Personally, I am not sure. 
But, I do have an idea about what it *might* be. I have been thinking
that programming is fundamentally about the "idea", and the act of
programming is just the act of codifying ideas.

Lets talk about what this means. Really abstract things are hard. 
For these purposes, I mean an idea is a mental state --
something that can be held in your brain. These states, these basic
units of information, are the stuff that our existence is made out
of. Just as there is a limit to how much we can consider at a moment,
there is also a limit to the size and complexity of an idea. 

Regardless of the form that our code takes, we somehow force it our
heads around it. Imagine examining a method that is not trivial to
grasp. We would probably decomposing it, 
possibly by creating a story about its behavior. 
First it "does this", then it "does that". In any case,
this method is now composed of multiple ideas, even though it is still
one method. Our program does not match the structure of our thoughts,
and this creates a hefty brain tax. 

Once we realize this, we can alleviate this pressure by changing the structure
of our code. We could do something as simple as splitting this method
into multiple methods, or we could create a service object. Either
way, we have just brought the code into line with the way we think
about it. 

I have started to try to develop a process for applying this
principle, with the goal of trying to get a sense of whether
or not it works. So far, results the are fruitful. Focusing on
the ideas that need to be expressed has drastically helped me every
time I have applied it.

So, what have we (maybe) gained? We have a framework to
understand how all of this design advice fits together, as well as a
metric to help us think about how to apply it. We also have a general
principle to help us decide how our code should be structured. I think
that's a win, either way. Let me know your thoughts!
