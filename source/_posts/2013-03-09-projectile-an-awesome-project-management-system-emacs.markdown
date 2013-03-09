---
layout: post
title: "Projectile is an Awesome Project Management System for Emacs"
date: 2013-03-09 13:02
comments: true
categories: Emacs
---

One of the biggest problems I have with my personal setup is the
context switch that comes from working with multiple projects.
At the moment, I have probably 5-6 projects that are "active" to some
degree. I obviously don't work on each of them every day, or even
every week.

Its hard to get back into a project after you haven't worked on it for
a while. What does each branch mean? How do I run the tests for this
again? What was the thing I was actually trying to do next?

For most of these, I still don't have an answer. Simply keeping
a NOTES file helps, but that only goes so far. 

For me, the key help would be an Emacs package that makes it easy
to switch between projects I work on. It would abstract away common
tasks (such as running tests, searching all files in a project, etc)
and automatically save project locations on my system, making it very
easy to quickly switch to what I want to work on next. Additionally, I
want this system to be friendly and extensible.

Projectile is a project management system for Emacs. There are a few
things that set it apart from others. Primarily, Projectile is
"modern". It has a decent test suite, is hosted on Github, and is in
active development.

The code in Projectile has a quality that I like to call "shallow",
but maybe "transparent" is a better word. When you read its code, it
all makes sense pretty quickly.
It uses
[@magnars](http://twitter.com/magnars)' 
great 
[dash.el](https://github.com/magnars/dash.el) 
list library, which makes most of its operations very clear and
obvious.

A good project management system needs to be adaptable and
lightweight, and Projectile does that very well.
A few years ago, I tried to use some of the different Emacs project
systems that were available. 
At the time, I basically couldn't figure out how to use them. 
Every project is slightly different. Systems that are optimized for
working on a C++ project probably wont work well for a Smalltalk project.

All together, this made projectile *very* friendly to change and
contribution, and I was able to adapt it for my needs very quickly. If
you have ever been frustrated about organizing projects in Emacs, I
whole-heartedly encourage you to try it. 
