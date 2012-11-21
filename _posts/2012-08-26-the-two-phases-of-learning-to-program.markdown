---
status: published
layout: post
title: The Two Phases of Learning to Program
---

A while ago, I had an insight that proved intellectually fruitful.
I think that within this idea is a seed of much more to come.  


The insight is this: There are, fundamentally, two separate phases
to learning to program. The first phase is learning the mechanics of
getting a program to work. The second phase is learning to make a
program that is comprehensible, useful, and extensible.

My first programming language was Javascript. The thing that I
remember most about learning it was the difficulty I had in
understanding basically every single idea. Because I had learned
HTML, I understood the idea of syntax. I also understood `if`
statements, but a `for` loop was beyond me. Forget about
prototypical-inheritance -- I was baffled. 

I did, however, get over this stage. At some point I learned PHP,
and began to grasp how this stuff worked. I hit a pretty good point
where I was able to build things without that much difficulty. 

The first program that really threw me was a project I was working
on in C. Segmentation faults were really common, of course, which
made things difficult. However, this program got to the point where
it was incomprehensible. 

This, I think, was my first taste of the second phase of programming
-- that is, making your code make sense. I eventually abandoned that
project (it was really just a learning exercise anyway), but the
lesson stuck with me. 

While this story is specific to me, I have heard similar things from
other programmers in the past. It makes sense: We start out unable
to get the computer to do anything. Then, once we know how to make a
computer do what we want, we quickly build incomprehensible,
unmaintainable piles of code. 

Of course, the first concern never goes away. Just because we have
learned to make computers do things does not mean programming is
always easy. And, obviously, the second phase is still around. Who
can say that they truly always write "good code", code which doesn't
need replacing eventually for sheer wrongness? I don't know of 
anyone who would say that about themselves. 

I am sufficiently confident to say that I do think that this
properly describes the way things /are/, but it does not describe
/why/ they are that way. What is more, we (generally) know how to
make our code more "correct" in the it-does-what-I-want way, but we
still don't know how to structure our code particularly well. 

We have a number of tools available that can make us write code that
works better. The most basic is education. Learning more
about the systems we work with gives us the foresight to anticipate
what problems may arise with the system. Test Driven Development has
done so well over the last decade because it also allows us to have
way more confidence in our code. Potentially, we could even go so
far as to logically prove that our code behaves correctly. 

However, the second area is more difficult. For given problem,
how should you architect its solution?
Would a functional approach, procedural
approach, or object-oriented approach be best? Or, could we go with
an even more less-common, logic-oriented solution?

We do know how to make some judgments though. For example, smaller
classes tend to be better than large classes. Small, direct
functions are better than large ones. Code duplication is bad. These
aren't *always* really right, though. 

So, why is this second task -- the task of structuring code in the
right way -- so very difficult? If you have any insights, let me
know!
