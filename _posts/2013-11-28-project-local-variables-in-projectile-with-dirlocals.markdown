---
layout: post
title: "Project Local Variables in Projectile with Dirlocals"
comments: true
categories: emacs
---

When working on a project in Emacs, sometimes it is extremely useful to set
emacs variables on a project-global level.

In fact, there is currently an
[open ticket](https://github.com/bbatsov/projectile/issues/139)
for the the very excellent
[Projectile](https://github.com/bbatsov/projectile)
project requesting this feature.

I have personally been using Emacs' per-directory local variable
facilities to emulate this behavior for some time.
The
[emacs documentation is found here](http://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html),
but lets explain it based upon this use case.

To create custom variables specific to your project, create a
`.dir-locals.el` file that sits in the root of the project. This
file would sit beside the `Projectile` file, for example.

Inside this file contains a very specially crafted lisp expression
that defines the variables we want. I'm just going to quote the manual
here, because I can't think of a better way to describe it:


>   The `.dir-locals.el` file should hold a specially-constructed list,
> which maps major mode names (symbols) to alists (*note Association
> Lists: (elisp)Association Lists.).  Each alist entry consists of a
> variable name and the directory-local value to assign to that variable,
> when the specified major mode is enabled.  Instead of a mode name, you
> can specify `nil', which means that the alist applies to any mode; or
> you can specify a subdirectory name (a string), in which case the alist
> applies to all files in that subdirectory.
>
>    Here's an example of a `.dir-locals.el' file:
>
>      ((nil . ((indent-tabs-mode . t)
>               (fill-column . 80)))
>       (c-mode . ((c-file-style . "BSD")
>                  (subdirs . nil)))
>       ("src/imported"
>        . ((nil . ((change-log-default-name
>                    . "ChangeLog.local"))))))
>
> This sets `indent-tabs-mode` and `fill-column` for any file in the
> directory tree, and the indentation style for any C source file.  The
> special `subdirs` element is not a variable, but a special keyword
> which indicates that the C mode settings are only to be applied in the
> current directory, not in any subdirectories.  Finally, it specifies a
> different `ChangeLog` file name for any file in the `src/imported`
> subdirectory.

On second thought, the manual example is pretty exhaustive. Lets
simplify: In order to specify a variable global to the whole project,
use this form:

    ((nil . ((my-project-global-variable . "the value"))))

Nice and simple.

A nuance here is that the "values" are not actually evaluated whenever
the variable is set for a buffer in a project. What if we require
evaluation? What if we want, for example, to ensure an elisp function
exists whenever working in a project?

For this, we can use the `eval` special form:

    ((nil . ((eval . (defun my-project-function ()
                       ;; your code here
                       )))))

Safety is clearly a big concern here. What if we open a file in a
project that has a maliciously crafted `.dir-locals.el` file?
Well, Emacs has still got our back: By default, it will ask you if you
wish to allow these variables to take effect. It also saves that
setting for the future so it doesn't need to keep asking you forever.
You can find out more
[in the documentation here](http://www.gnu.org/software/emacs/manual/html_node/emacs/Safe-File-Variables.html).

Using `.dirlocals.el` with Projectile has worked very well for me, and
solves a big class of problems.
