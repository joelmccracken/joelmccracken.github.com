---
layout: post
title: "Switching Between term-mode and line-mode in Emacs' Term"
date: 2014-05-05 08:08:01 -0400
comments: true
categories: emacs
---
I've been using term-mode for a while now and just recently decided to
fix something that has been irking me.

See, term-mode allows you to switch between what is called "line mode"
and "char mode". In line mode, emacs treats the shell as a plain
buffer, except that pressing "return" will send the current line to
the shell for execution. This is really cool! It allows for all sorts
of fancy things, because you can treat your shell as a regular emacs buffer.
This behavior is essentially the same behavior as in `comint-mode`,
another emacs shell.

The second mode of `term-mode` is character mode. When in character
mode, each key you type is sent directly to the subshell (except
for a few). Thus, character mode feels like a traditional terminal
emulator like Terminal.app on OS X.

The way you switch between them is with `C-c C-j`/`C-c C-k`. One will
switch the buffer to character mode, and the other will switch
the buffer to line mode.

I can never remember which key does what and where. So, the other day
I finally decided to sit down and fix the problem. I came up with
this:

````
(require 'term)

(defun jnm/term-toggle-mode ()
  "Toggles term between line mode and char mode"
  (interactive)
  (if (term-in-line-mode)
      (term-char-mode)
    (term-line-mode)))

(define-key term-mode-map (kbd "C-c C-j") 'jnm/term-toggle-mode)
(define-key term-mode-map (kbd "C-c C-k") 'jnm/term-toggle-mode)

(define-key term-raw-map (kbd "C-c C-j") 'jnm/term-toggle-mode)
(define-key term-raw-map (kbd "C-c C-k") 'jnm/term-toggle-mode)
````

The idea is that what I actually want is a method to toggle which
mode the buffer is in. I can then overwrite the previous bindings with
this toggle method, and I won't have to deal with this ever again.

Thus, the command `jnm/term-toggle-mode` toggles a buffer between
character mode and line mode. In order to connect the command, we need
to hook it up to the correct keymaps. These maps are `term-mode-map`,
which is active when the buffer is
in line mode, and `term-raw-map`, which is active when the buffer is
in character mode. So, I hook it up to both key bindings in both maps,
and now I no longer have any problem.

I &lt;3 Emacs.
