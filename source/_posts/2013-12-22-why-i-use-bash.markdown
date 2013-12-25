---
layout: post
title: "Why I Use Bash"
date: 2013-12-22 16:40
comments: true
categories: unix automation
---

I rely on Bash for much of my scripting needs.
I really
[don't like it](/entries/resources-for-learning-bash-scripting/),
though.
So why don't I use something like
[Ruby](/entries/ruby-from-lisp-perspective/)
instead?

There are a few reasons I use Bash. It works very well as a
"bootstrap language". Bash is good at setting up an
environment and running complicated commands. Problems with
Bash come up when you need to do anything slightly complicated, but
for simple tasks it works very well.

Bash is a de-facto standard. It is available by default on all the nix
operating systems I care about. My goal is to develop a stable,
repeatable, and testable computing environment, and for this, Bash works.
It can be relied upon as a platform to launch other software.

Bash is standard among hackers. Shell examples are given in
Bash script. I can expect another programmer to be able to reasonably
comprehend and modify a Bash script. In this way, Bash transcends
cultural differences among programmers. I can use Bash skills on any
type of project. Knowing Bash, then, is also extremely valuable.

I would love to use something else.
[Awk](http://pubs.opengroup.org/onlinepubs/9699919799/utilities/awk.html)
seems like it would be a very good choice, but I don't know it very
well yet.

Ruby *could* be a good choice here, but I don't believe it currently
is. Before I could ever rely on Ruby as a bootstrap language, I need
to believe a Ruby script on one system will work reliably on another
without having to dictate too much about either system's environment.
I need to
see binaries available that can be be installed anywhere.
These binaries should be
able to be placed *anywhere* on the system and still have everything
work. Gems installed on one Ruby should not interfere with gems on any
other. Environment variables from one should not interfere with
environment variables from another.

Bash works well. I wish there were a better choice for standard systems
scripting, but I don't think there is.
