---
status: published
layout: post
title: 'Emacs Lisp for Hackers: Keys, and Keymaps'
alias: /2012/12/10/emacs-lisp-for-hackers-keys.html
---


The Keyboard: Keys and Keymaps
==============================

Its been a while since I posted in this series -- but don't fret! I
sill am dedicated to providing a guide to Emacs Lisp, the guide that I
wish I had a year ago. I also recently
implied that I prefer Ruby to Lisp, but that is not
important. Emacs is still the best developer environment in the world,
and occupies a really important place in the future of code composition.

<div class='sidebar'>
<p>
<h4>Afraid to change keybindings?</h4>
</p>
<p>
To be honest, I was always a little uneasy about altering the default Emacs
keybindings. I felt that if I overrode them, I would somehow miss out
on something. 
</p>
<p>
I found that my fears unfounded. I should have just jumped in on my
first day, customizing whatever I felt like. I have yet to encounter a
problem because of some key bindings I had previous overridden. So, if you feel some
trepidation about changing keybindings, you should go ahead and try.</p>
</div>


The learning to program in Emacs Lisp can be split into a number of
concerns. The first concern is the language itself. How do we initialize
variables, create functions, generally structure programs? So far, we have
been mostly limited to these types of concerns. We have not yet
touched on manipulating Emacs itself. Which, is basically the goal of
all of this Emacs hacking, right? So, to get started, lets look at
manipulating keybindings. One of Emacs' most "famous" features is the
magic you can do with the keyboard. I use quotes around famous because
the finger stretching it requires is the primary source of criticism.

Either way, keybindings are a good place to start talking about
modifying Emacs.




Setting a Global Keyboard Binding
---------------------------------

On Mac OS X Emacs, the key Command-Q will kill Emacs. I always hated
this. Disabling it was really easy, though: 

    (global-set-key (kbd "s-q") nil)

This code introduces a few things. First, we use the `global-set-key`
function, which sets the key, well, globally. Other commands set keys
for specific keymaps. Which we'll get to in a minute. 

Next, we have the form `kbd`. We could skip using `kbd`, but the
strings we would need to use to specify a key binding would be uglier
and more opaque. Using `kbd` lets us specify these in a much more
consistent, natural way. In the above example, "s-q" is the
"Command-Q" binding. 

Finally, we have `nil`. This means that the key binding now does
nothing, which disables the Command-Q quitting in our personal Emacs. Nice!

We can bind functions to keys. Lets create a binding so that Meta-h
will execute a "hello world" function:

    (global-set-key (kbd "M-h")
                    (lambda ()
                      (interactive)
                      (message "Hello from Emacs!")))


Now, if you press Meta-h, you'll receive a friendly greeting from
Emacs. 


This is the essence of keybindings. If you were to stop reading here,
you would be able to get pretty far. However, there is another feature
that we should discuss: that is, keymaps. 


Keymaps
-------

A keymap is simply a collection of keys to bindings. They allow us to
conveniently group bindings to be collectively added, enabled, or
disabled. 

Not only does a keymap hold keybindings, it can also itself be bound
to a key. In this way, we create a hierarchy of bindings. Lets look at
a quick example:

    (defvar joel-custom-keymap nil "my keymap..")
    
    (setq joel-custom-keymap (make-sparse-keymap))
    (global-set-key (kbd "C-x M-j")  joel-custom-keymap)
    
    (define-key joel-custom-keymap (kbd "s") 'otto/status)
    (define-key joel-custom-keymap (kbd "t") 'twit)
    (define-key joel-custom-keymap (kbd "p") 'plan)

This is actual code that I use. It comes straight out of my dotemacs
file. It binds the keymap `joel-custom-keymap` to the key sequence 
"C-x M-j". When I press those, I can then press `s`, `t`, or `p` to
run the commands `otto/status`, `twit`, or `plan`, respectively. 

I feel like this is straightforward but awkward to explain. In any
case, this is enough to get started. You'll probably want to
get familiar with what 
[the manual entry](http://www.gnu.org/software/emacs/manual/html_node/elisp/Keymaps.html)
contains so that you will know where to look in the future. Happy hacking!
