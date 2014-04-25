---
layout: post
title: "Name Emacs Daemons With the '--daemon=' Option"
date: 2014-02-04 15:42
comments: true
categories: emacs
---

Emacs has a great "daemonization" feature which allows the user to
connect to
a currently-running Emacs instance ("the server") in a "client".
The running client looks and feels just
like a regular Emacs instance.

Creating an Emacs daemon is straightforward. The
`emacs --daemon` command will create a new emacs daemon instance.
Running the command `server-start` from within emacs will 'daemonize'
the current emacs instance, allowing new emacsclients to connect to
it.

Multiple daemon instances can be run, each with a unique name.
A client may then specify which emacs server to connect to via the
`-s <servername>` option. That way, you can have as many Emacs
instances running as you want, and connect to them freely.

However, naming Emacs daemon instances is not straightforward.
There is a variable, `server-name`, which controls what the server
will be named, as long as it is set to that at the time of
daemonization.
So, launching a new Emacs instance from
the command line was really awkward.
The easiest method I had found is something like:

    emacs -e '(setq server-name "my-special-server")' --daemon

This code sets the `server-name` variable before daemonization
starts. This works, but is awkward. You also need to deal with
quoting the code, which is also awkward.

One day, it struck me: what if `--daemon` takes a name as an argument,
but this just isn't documented anywhere?

As it turns out, it does. The above may be accomplished by the
following, which is much more attractive:

    emacs --daemon=my-special-server

Suddenly, launching new Emacs servers is much easier.

