---

layout: page
title: Automatic Test Driven Development
---

Within the last decade, the programming community has begun to
embrace *Test Driven Development*.
If you do not already practice it, you at least have heard of it and
know something about it. 
The thing is, many people don't do TDD, and it 
is unclear to them that they should. Other coders who do
practice TDD will fall into non-TDD development[<sup>1</sup>][iag], 
without a good guiding principle about when and why they should
test. Besides that,
occasionally articles on [hacker news](http://news.ycombinator.com) 
appear telling us that testing costs too much for too little return, 
and thus it is not worth doing. 

So, opinions are divided. Many people swear by testing, while others
dismiss it outright. Thus, discussion is necessary. Is there some way
to figure out the benefits of testing versus not testing? I think
there is.

## All Development is Driven by Tests

Before we talk about TDD, we must discuss regular, plain old development.
Testing itself has nothing to
do with any sort of computer program. There are *lots* of
ways that we test our code. We compile our code. We run it. We verify
that it does what we want it to. For a person who hasn't ever
practiced TDD, it doesn't make sense. Really, 
*all development is test driven*. 
Test Driven Development as a *term* confuses TDD as *practice*.
I mean, come on, *we all already test*.

This distinction is very important. Ever since we started to
code, we have written code, 
pressed enter, and then seen what happens. As early programmers,
this is the most natural thing in the world. This is the
bread-and-butter of coding, and it needs to be acknowledged. The
practice of manually testing your code is about as fundamental to the
practice of programming as anything can possibly be. 

The difference is in the *automation*. When we *write* tests, we now
have a way to find out if things happen *automatically*. What does
this automation give you? If you did
your TDD correctly, you can delete or alter any line of code and
immediately see what that change breaks. You become free to test your
assumptions in any way. Without automation, you could not have such freedom.

Thus, I believe we need some new terms for this
discussion. The old way of manually testing the code by itself 
[needs a term](/content/gaining-words.html). 
Lets call it 
**Manual Test Driven Development** 
(MTDD). The newer, trendier method of development, what has been
called TDD, ought to be called 
**Automated Test Driven Development** 
(ATDD). ATDD, as a term, focuses on the true value of TDD, hopefully giving
some insight to people who haven't been doing TDD for
long. Additionally, I think the insight that automation is what makes
TDD so great is [rather important](/content/automate-everything.html). 



1. Yes, I am guilty of this. I began to write
this as a way to overcome my tendency to fall out of the good
habit of writing tests. [#][iag]{: #i-am-guilty }
{:.footnotes}

[iag]: #i-am-guilty
