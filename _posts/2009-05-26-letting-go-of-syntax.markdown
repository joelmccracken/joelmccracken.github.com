---
layout: post
title: Letting Go of Syntax
---

{{ page.title}}
===============


I started to program by learning PHP. I made lots of spaghetti code
intertwined with HTML, CSS, and database queries. It was awful. I did love
the string interpolation features, but I didn't know much else about programming. 
Creating my first guest book was a real thrill. 

C taught me bits about how a computer works on a low level. K&R was wonderful. 
I saw how the syntax interacted with the hardware in a much more direct way,
whereas using PHP had seemed like magic. The fact that {% highlight C %}a[n] == *(a + n){% endhighlight %} was
very important.

At University, I learned Java. Basic principles of object oriented
design were taught. I was exposed to the OO method of restriction, 
encapsulation, and method/data organization. 

I also had to learn MIPS assembly. Translating C code into assembly by hand
was extremely instructive. The syntax became even more transparent -- what 
was important was the idea it conveyed. 

I Learned Python. Python's white space block delimiting was the first time
I made heavy use of a language that didn't have C style syntax. It was 
strange but somewhat liberating. Plus, it had a tons of new features that my 
previous languages didn't have. It was the first time I ever experienced an 
REPL and all the joys and productivity increase that it brings. 

Javascript had a few nifty things, like tail recursion and anonymous 
functions. When I learned how to write a function that could recur 
indefinitely, I was hooked. Anonymous functions and closures blew my mind for
a little while, but I was hooked and got better at them. 

Then came Lisp. I went through the second chapter of Practical Common Lisp, and
I was astonished. A fairly functional, very basic implementation of SQL written
and explained within the second chapter of an introductory book? I thought it was 
extraordinary (and it was). I went to Scheme, and the syntax enlightenment was 
complete.

***

Lisp allows you to program as close to the abstract syntax tree as possible.
It changes the way you think about programs. You realize that
learning a new language is not a challenge. You also to realize that a more 
"natural" syntax is simply a crutch. 

Ultimately, what different languages and environments provide are different
behaviors. Syntax just doesn't really matter.
