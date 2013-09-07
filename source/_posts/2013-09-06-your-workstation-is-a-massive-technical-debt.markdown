---
layout: post
title: "Your Workstation is a Massive Technical Debt"
date: 2013-09-06 19:39
comments: true
categories: chef open-source automation technical-debt
---

Thinking about
[closed source as technical debt](/entries/closed-source-as-risk/)
forced me to evaluate
my whole computing life, taking a wholeistic look
at all technical debt I have.
I realized that closed source software is not the biggest problem
facing me regarding technical debt.
I realized there was a lurking debt that I hadn't fully
realized before: the state of my workstation.

Think about it. You probably have personal files on your
computer that could get lost or stolen. If you
read my blog, you probably have things heavily customized,
set up the way you like them. What would
happen if your computer died tomorrow? Each computer is a ticking
timebomb, waiting to explode with headache.

Early last year (2012) bought I purchased a Macbook Air, which was to
replace my Macbook Pro from 2007. I loved it and needed it, but it
took me *three weeks* to set everything up the way I needed before it
made sense to start using it full time.

What kind of debt accrues on the computers we use?

1. An unknown set of applications. What applications do you use? What
   would it take to get them back onto the computer? This is not much
   of a problem with large, obvious applications. Smaller programs,
   such as command line utilities installed with homebrew, are much
   easier to miss.

2. An unknown state of applications. What settings do I have? Which
   browser extensions? Where are all of the config files located for
   the unix utilities I use?

3. A lack of backups. Do I have backups? Are they sufficient? Are they
   secure? How could I know?

4. A lack of tests. I dont have a way to tell if all systems are
   working on my laptop. If I update a system gem, does that break
   anything? Without good tests, I can't tell.

This is a problem I feel daily. As I have been trying to
automate myself as much as possible, I hit these problems constantly.
It is too easy to break a helper script that depends
upon a file system structure, a version of RVM, or a specific shell
command.

I also know I am not the only one with this problem. I once worked
with very smart man who wrote many scripts to automate
his work. However, whenever someone else on the team would try to use
what he wrote, there was usually problem with them. These scripts
expect certain applications to be present, certain libraries, and
certain data paths. Moving the script to a different machine broke
the silent expectations built into the scripts.

I have been using Chef to get my workstation under control. Pivotal
has a few projects, such as
[Sprout](https://github.com/pivotal-sprout/sprout),
[Sprout Wrap](https://github.com/pivotal-sprout/sprout-wrap),
and [Soloist](https://github.com/mkocher/soloist)
to help. If you also think this would be worth doing for yourself, you
should take a look.
