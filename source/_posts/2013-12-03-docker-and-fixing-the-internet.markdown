---
layout: post
title: "Docker and Fixing the Internet"
categories: Fix-the-Internet
---

I've been hearing about LXC and Docker (hereafter "Docker")
from all around the internet
over the past few months. If you pay attention to Linux developments
at all, you have probably heart about them both.

A while ago I outlined a solution
[to fix the internet](/entries/lets-fix-the-internet/),
which would
shift control of the internet to individuals and away from
centralization.

Docker makes this project much easier.
Installing and running many pieces of software on a system
can cause them to
interact in problematic ways. Beyond that, the entire system is made
more complicated by each new component that is installed.

Instead of having configuration, logs, data, and binaries all on the
same system, Docker lets us contain each application in its own
machine. That way, each container much easier to understand, debug,
and can be consider independent of the rest of the system.
Before Docker, we could have simplified the project in the same way by
installing each piece of software on a separate
machine. Realistically, this would cost way too much.

I am pumped. In my mind, Docker removes the majority of the work
blocking us from having such a system today.
