---
layout: post
title: Jekyll Reactions
---

{{ page.title }}
==============================

Every once in a while, I notice a gaping hole in a markets. One of those holes is free blogging platforms. I know there are many blogging solutions available, but I haven't been able to find one that is feature-rich. Mostly, they are all the same in any way that matters. I am slowly writing more and more, however I am not ready to begin using paid hosting, because I'm not sure what I want exactly I want to blog about. 

As a programmer, I wanted to be able to have syntax highlighting in my code. This is a requirement. It is very hard to read code that is not properly formatted and highlighted. Nothing provided this functionality. Also, and less so, I wanted have the option of using LaTeX math whenever I needed it. Often, ideas are much easier to describe with math notation than via programs.

So, I waited, dragging my feet. If any of you have actually looked through some of [my old blog][] posts on programming, you know that it is awful to read code on wordpress without any plugins. And, I've never even attempted to write anything with math in it, as that would be totally unreadable.

[my old blog]: http://nyaj.wordpress.com

Until I stumbled upon on Jekyll. [Jekyll][] is a static site generator, which has been somewhat optimized for blogging. You write your content, and then Jekyll converts the document to html, adding in headers, footers, and whatever else you desier. 

[Jekyll]: http://github.com/mojombo/jekyll/tree/master

Jekyll virtues:
* Latex! $E = mc^2$
* Syntax highlighting! {% highlight scheme %}(lamdba (f) (lambda(x) (f (x x)))) (lambda (x) (f (x x))) {% endhighlight %}
* Easy to use whatever text editor I want (emacs, for me)
* Git versioning, since it is hosted on github.
* Layout is completely user-definable. No need to be limited by other peoples' templates

Jekyll problems:
* By nature, commenting is impossible. This has made me very hesitant to adopt it completely, however I don't think this will be a huge problem in the long run. 
* It is unwieldy. For example, I currently have not been able to determine why my blog often does not get plublished on the server. I am fairly certain that this is because the server is running a different version of jekyll. To get around this, my solution is to publish the Jekyll code locally, and then push site content to github. The downside to doing this is that I don't get the github copy of my original documents, only the generated content.

So, while Jekyll is very useful and interesting, and certainly occupies a local optima, it does how its drawbacks. For now, it is the best available publishing solution. So far, I am happy with it. 