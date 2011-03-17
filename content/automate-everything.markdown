---

layout: page
title: Automate Everything
---

Ever since I started to practice Test Driven Development, it has bothered me. While
it is a really great idea, I always felt like it was a shadow of another idea. There are
many valuable thingss that are similar to TDD, but aren't part of the
testing process. 

For example, I always noticed that testing out the way your function
behaves in an interactive shell is really similar to writing a
test. Indeed, they seem to be almost the same 
thing<sup><a href="#python-for-example">1</a></sup>. Why are they so
similar? 

Or, whenever I am learning a new library, I play around with
it, trying to figure out how it works, and specifically how it should
be useful for the purposes of my project. Here's the thing -- if I
were to change these shell sessions into test suites, it 
*would provide documentation about that library that is suited to my particular
situation*. 
However, *writing tests for external code is considered wrong*. So,
while I see the usefulness as documentation, it is also wrong. Highly
useful, and wrong. This bothers
me. If doing something like that is beneficial, why shouln't I do
it? 

All of this made testing seem like a small piece of a greater,
possibly more important practice, and for the first eight months or so
of my practicing TDD, it bothered me. Until one day, inspiration
struck.[Test Driven Development isn't *really* about *testing*, it is about
*automation*](/content/automatic-test-driven-development.html).
We can automate ourselves, and this
can bring tremendous benefit -- benefit on the level of what test
driven development brings.

Suddenly, these strange issues seemed to melt away. That
library-documenting code that I was talking about? It isn't
testing. But, it is still something that can be really useful if
*automated*.

So, I'm trying it out. It seems like a hunch worth testing. I am
making a strong effort do figure out what 
areas of my life could benefit from automation, and I'm figuring out
what the best way to do it is. I hope to post the results here.

<span class="footnotes">
1. <a id="python-for-example"></a> Check out 
[Python's doctest library](http://docs.python.org/library/doctest.html)
for an interesting look at this idea.
</span>
