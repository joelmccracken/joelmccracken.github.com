---
status: published
layout: post
title: Emacs Lisp for Hackers Part 2 -- Emacs Documentation
---

In the first part of this series, I introduced the
ideas behind Emacs lisp. Since you understand the basics of what lisp code
looks like and means, it makes sense to learn how to get around the
documentation. These methods will be useful to you for as long as you
use emacs.

Emacs has excellent self-documenting features. I cannot recommend 
that you learn about them highly enough. Some of the documentation is
available online, but that is not nearly the same


Help documentation
------------------

The command `help-for-help`, bound to `C-h ?`, is the self-documenting
starting point. However, I myself even found *it* to be confusing
(specifically, it is overly complete when what you may be looking for
is only a guide). So,
look at it quickly, remember it is there, and maybe move on. 

A better place to start is with the command `describe-key`, which is
bound to `C-h k`. The command allows you to type in a key sequence
that you are used to using, and tells you what command that runs,
along with the documentation for that command. 

This is an easy way to start writing emacs lisp scripts. Every
keybinding maps to an elisp function. And, since these are just
regular functions, they can be called just like regular functions. So,
if you know how to do something with a keybinding, you *also* know how
to do it in elisp. 

Another key feature to note is that many of these help windows contain
"links" to the actual function definitions that these
describe. Clicking on the link or moving point to the link and
pressing RET will take you to the source code for that function, which
lets you see how this command is implemented, as long as it was
implemented in elisp (functions that are implemented in C cannot be
browsed in this way). 

You will occasionally come across a function that you do not
understand. The command `describe-function`, bound to `C-h f`, asks
for a function name, and displays the documentation for that function.

Similar to `describe-function` is `describe-variable`, which is bound
to `C-h v`. This command does the same thing as `describe-function`,
except that it works for variables.   


Lastly, the various `apropos-*` commands. These commands search
through every symbol in emacs, returning what matches the string you
provided. The command `apropos` will search everything, such as
functions, variables, and commands, while `apropos-command` and
`apropos-variable` will only search for commands and variables,
respectively. 

This isn't strictly about writing emacs lisp, but my favorite way to
get to know some feature is via `describe-mode`, bound to 
`C-h m`. This command gives you information about the current
modes that a buffer is in, both its major mode and its minor
modes. This is a great way to explore Emacs, and basically provides a
"cheat-sheet" for every mode that you'd use. 


Info manuals: the full documentation
------------------------------------

Emacs' has extensive documentation provided by info files. These are
essentially hypertext documentation files. They're really, really
great. However, it takes a while to get the "feel" for it. 

Access info with the command `info`, which is bound to `C-h i`. This
shows you a directory of info files. 

The key `s` is bound to `Info-search`, which performs a regular-expression
search on the current 
info document you are viewing. The command `clone-buffer` (`M-n`) is
very useful for opening another info buffer to continue
exploring Emacs documentation. 


------

These commands will get you where you need to be. At this point, you
should be familiar enough with using Emacs to be able to answer your
own questions as they come up. 



