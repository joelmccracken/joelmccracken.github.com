---
layout: post
title: "The Ruby Top-Level Object: Main"
comments: true
categories: ruby
---

Hey guys. My blog has become pretty lonely, so I want to try coming up
with some smaller, less-refined posts. I hope you don't mind.

One of the few things I still forget in Ruby is how the toplevel
global instance works and how it relates to `main`, `Object`
instances, and kernel. For some reason I always think that the
toplevel object is called `kernel`, which it isn't.

The toplevel object is the context code that is run by default in
Ruby. This toplevel object is referenced by `self`, and is called
'main':

    bash-3.2$ ruby -e 'puts self'
    main

This 'main' object is of class `Object`:

    bash-3.2$ ruby -e 'puts self.class'
    Object

I thought it was intersting that this 'main' object returns the string
'main'; most instance of objects don't look like that:


    bash-3.2$ ruby -e 'puts self.class.new'
    #<Object:0x007f94e3835ae8>
    bash-3.2$ ruby -e 'puts Object.new'
    #<Object:0x007fc2f4035b60>

I wonder if there are any singleton methods on this metaclass that
explains this different behavior?

    bash-3.2$ ruby -e 'puts self.singleton_methods'
    to_s
    public
    private
    include

Cool! Problem figured out.
