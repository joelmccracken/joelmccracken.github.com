---
title: 5 Reasons Why you Should Learn Emacs Lisp Today
layout: post
---

Emacs Lisp is an enigmatic hacker programming language. Its weaknesses
are well known. It slow. It is single-threaded. Its variables are
dynamically bound. I can go on, but these are the main objections. 
Despite these perceived weaknesses, Emacs Lisp 
is _still_ the most popular Lisp language on github. Many
programmers swear by it. Myself, I probably average writing between 50
and 100 lines of Emacs Lisp daily.

Of course, I decided to invest my time learning it, and I am more than
happy that I did. I really wish I had done so sooner. The mentioned
problems dissuaded me from investing much into the programming
language. But now, I seriously wish I had taken the time to learn it
years ago. 


What makes it so awesome, despite the mentioned problems? 
Why should anyone bother learning Emacs Lisp?

So, without further ado, 5 reasons to learn Emacs Lisp today:

1. Learning almost any amount of Emacs Lisp makes your life better
   immediately. What you learn can improve your life _right now_. Not
   one year from now. Not after graduation. Not when your team starts
   the next project and you might be able to use this language. 

   Many lisp languages are on the theoretical side of the spectrum. That
   is great, and it certainly has its place. However, Emacs Lisp is much
   more "quick and dirty". Emacs Lisp is very practical, and helps you
   solve your day to day problems.

   If you don't know any Emacs Lisp, your .emacs file is probably full
   with copy-and-paste code that you
   used to install packages. Which is normal. The trouble is, this gets
   ugly, quickly. Soon your .emacs file becomes really hard to
   maintain. Learning Emacs Lisp helps keep your Emacs life sane. 

   If you ever need to get any help about Emacs, there is a high chance
   that the person who decides to help you will include some Lisp
   code. Its pretty hard to tell someone "sorry, I don't know any elisp"
   on ##emacs on freenode. Knowing Emacs Lisp gives you a better grasp
   of the community.   

   Learn Emacs Lisp. It makes your life better, today. 

2. Emacs Lisp and Emacs is a really interesting programming
   platform. You don't have to be a rabid Emacs fan to care; I think
   anyone would agree that Emacs is objectively interesting. 
   
   Chances are that you have some exposure to the ideas behind
   Smalltalk, such as, you know, that whole "objected oriented"
   thing. Well, besides Smalltalk as a programming language, it is also a
   programming environment. That same remarkably powerful spirit lives
   on in Emacs. Just 
   about everything is immediately viewable and inspectable by the
   user.  Emacs is basically a descendant in spirit of those systems. 

   Want to know what a keybinding does? C-h k, and then the
   keybinding. Want to know how that function works? C-h f, then the
   function name. Want to actually see its source code? Click on the
   link that appears in that buffer. Oh, and you can actually modify
   it on the fly if you need to, just by changing the function
   definition and evaluating it with C-x C-e
   
   When programming in Emacs Lisp, you get real work done with a system
   that is our modern day equivalent of the stuff of legends. Bonus: this system
   isn't just some intellectual exercise. It is practical, awesome, and
   immediately useful. 


3. [Dynamic binding is actually pretty awesome](/2012/06/02/emacs-hack-set-todo-done-yesterday.html). 
   Lets go through a quick
   review of what dynamic binding is, exactly, and contrast it with the
   other option, lexical binding.

   First, lets talk about variable binding, specifically, in pseudo-Javascript:

       function createAdder(num_to_add)
       {
         function(new_num)
         {
           return num_to_add + new_num;
         }
       }
   
       add2 = createAdder(2);
   
       add10 = createAdder(10);
   
       
       console.log(add2(6));  //= 8
   
       console.log(add10(13));  //= 23


   Lets discuss this code. createAdder is a function which will return
   another function -- that is, it "creates" an "adder". The function
   that createAdder returns will add whatever was passed to it (the new
   function) and the original number that was passed to createAdder. 
   
   The important point here is that whenever createAdder returns, the
   function that gets returned still has access to the argument that was
   passed, the num_to_add. Whatever variables are within a parent's scope
   when a function gets created remain available to it for its execution.
   
   Lexical scope is great. It enables really elegant solutions to
   problems, and is what makes Ajax and Node.js sane and possible. 
   
   Dynamic scope is different. With dynamic scope, variables are
   bound and evaluated at evaluation time. They get evaluated to whatever
   is above in the call stack. 
   
   An example, in a pseudo, now dynamically-scoped Javascript:

    
       function adder(new_num)
       {   
          return num_to_add + new_num;
       }

       function add2(new_num)
       {
          num_to_add = 2;
          return adder(new_num);
       }

       function add10(new_num)
       {
          num_to_add = 10;
          return adder(new_num);
       }    

       console.log(add2(6));  //= 8

       console.log(add10(13));  //= 23


   The key difference is that within the body of the function adder,
   num_to_add does not seem to refer to anything. However, since this
   language is dynamically bound, its variables are referenced from where
   it gets called. So, the function add2 sets the variable num_to_add,
   and when adder is called, it is able to reference it. 
   
   You are probably thinking that this sounds a lot like global
   variables. And, it is. This is the reason that most modern languages
   are lexically bound, not dynamically. 
   
   In practice it really doesn't tend to
   be that bad. Variables that you are expected to change are well
   documented. Code only uses dynamic scope resolution when it needs
   to, and typically doesn't need to very often. 
   
   Dynamic binding is actually really convenient. For example, it allows you
   to make variable-based "configuration" changes that only last within a
   given scope. 
   
   Besides that, there is also a macro, called lexical-let, which gives
   you the functionality you would get with lexical binding. 
   
   If that isn't enough, Emacs 24 now supports actual lexical scoping. You
   just need to set a variable within the elisp file, saying that you
   want lexical scoping. 
   
   So. Dynamic scoping is actually pretty cool. 


4. Single threading is a good thing. Emacs lisp is single threaded,
   and probably forever will be. This is somewhat limiting, however it
   does greatly simplify things.  
   
   Enough said. If you really want to deal with locks and their ilk, sorry,
   you will have to look elsewhere. 
   
   
5. Everything about Emacs makes more sense once you understand Emacs
   Lisp. You may have noticed that Emacs is extremely self documenting,
   and that the documentation is extremely comprehensive. However, much
   of that documentation only makes sense within the context of Lisp
   code. So, if you don't understand Emacs Lisp, then much of that
   documentation is worthless. 
   
   As an example, to understand the Emacs keybinding system,
   you really need to have a grasp on the way that keybindings are
   constructed. And, that means Lisp. 
   
   Honestly, Emacs lisp is everywhere in Emacs, and while Emacs can be
   really useful without knowing any Lisp, it becomes way more useful
   when you understand the native tongue, Emacs Lisp. 


So, you should learn Emacs lisp. It is a really fascinating
environment to program in and it has the potential to make pieces of
your life much easier. Besides that, to me, the problems of Emacs
Lisp are not significant enough to be truly dissuading -- in fact,
most of them can be considered strengths. 

In all, Emacs Lisp is a delight to work with, and I cannot recommend
learning it highly enough to anyone who considers themselves a serious
programmer. 
