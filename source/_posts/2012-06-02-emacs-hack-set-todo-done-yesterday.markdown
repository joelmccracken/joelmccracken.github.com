---
status: published
title: "Quick Emacs Hack: Set TODO to done yesterday"
alias: /2012/06/02/emacs-hack-set-todo-done-yesterday.html
layout: post
---

You have probably heard of [org mode](http://orgmode.org/), the tool which helps keep you
organized. It is really great, and you should check it out. One of its
less widely-known features is habit tracking. Normally, org mode
gives you the ability to mark org entires as TODO items, change them
to done, etc. This works really well, but it somewhat falls apart for
things that we want to do regularly. 

The 
[org-habits module](http://orgmode.org/manual/Tracking-your-habits.html#Tracking-your-habits) 
allows you to make TODO items
into habits. When you set one of these items to DONE, a log entry gets
created, and the habit due date gets moved forward in time based on
the repeater interval. 

The problem, though, is that I often don't get around to setting the
TODO item to done when I actually do it. I am not always at my
computer, after all. 

[Dynamic binding to the rescue!](/2012/05/23/why-you-should-learn-elisp.html)
We *could* handle this in a much more
complicated way, but the easiest solution is to just "pretend" 
that it is actually yesterday while the code that changes the TODO is run.  
Because of dynamic binding, we can be make the change really easily.

The code below that implements this uses some rather specific elisp
features, so I heavily commented it to help, but the part that is
important is at the end.


    (defun org-todo-toggle-yesterday ()
      ;; this function is interactive, meaning a "command" that we call
      ;; as an emacs user (allows us to do "M-x org-todo-toggle-yesterday")
      (interactive)
  
      (let ((time-in-question (decode-time))) 
        ;; time-in-question is the current time, decoded into convenient fields
    
        ;; decrease the field by one which represents the day -- make it "yesterday"
        (decf (nth 3 time-in-question))
    
        ;; now, re-encode that time
        (setq time-in-question (apply 'encode-time time-in-question))
        
        (flet ((current-time () time-in-question))
          ;; flet temporarily binds current-time to this version, which
          ;; returns the time from yesterday 
          
          (org-todo)
          ;; toggles the todo heading
          )))

Most of this code just helps with getting a date for the previous
day. The important part is the last `flet`, which temporarily changes
the definition of `current-time` for those let body calls. 

Okay, hope this helps anyone who uses org mode and the habits
module. Dynamic binding lets this solution work well, and so is not
nearly as problematic as could be seen. 





