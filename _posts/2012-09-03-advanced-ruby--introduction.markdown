---
layout: post
title: Advanced Ruby -- Introduction
---

# Advanced Ruby -- Introduction #


Originally I went through Metaprogramming Ruby about a year ago, but
never really took the time to use it. Recently I've been doing much
more Ruby, so I feel like it is time for a refresher. 

Ruby is a really interesting programming language. Very recently, my
view shifted on it. For a long time, I wasn't sure exactly what to
think about it. Now, Ruby seems like the ideal
scripting language to interact 
with the Unix environment... especially because I am really sick
of dealing with Bash.

So, here is a first in a series of examples that show off the great
dynamic features of Ruby. These examples are 

Most of what
makes Ruby different is only surprising from the standpoint of a
user of "mainstream" languages. To a Lisp programmer, for example,
none of these should be very surprising. 

Eventually, I'm going to investigate adding real syntactic
extensions (macros), but that is for a future date. 
There are obvious ways to approach this,
and libraries that basically do it (rewrite ruby, etc), 
but there doesn't seem to be much action in
this arena. Which is a shame. I think this is important for the
long-term health of 
Ruby. I believe the perception (and reality) of Ruby as an *advanced* language is
important for its long term health.


---

So, here we go: a basic set of minispec specs which demonstrate some
extreme flexibility of Ruby:

    describe "ruby classes" do
    
      describe "definitions are just regular code" do
    
        it "evaluates regular code" do
          $the_secret = nil
    
          class ClassDefSideEffects
            $the_secret = "shh"
          end
    
          $the_secret.must_equal "shh"
        end
    
        it "return what was last evaluated" do
          (class Doot
             1 + 1
           end).must_equal 2
        end
      end
    
    
      describe "classes are objects" do
        it "has methods (such as class) and reports that it has the type class" do
          class Dootz; end
          Dootz.class.must_equal Class
        end
      end
    
      describe "dynamically creating classes" do
        it "can create new classes" do
          x = Class.new
          y = x.new
          z = x.new
    
          y.class.must_equal z.class
    
          y.class.must_equal x
        end
    
        it "is just objects, so can be passed around" do
          class Dootz; end
    
          x = Dootz.class.new.new
    
          x.class.class.must_equal Class
        end
      end
    end



Next time we'll get into blocks. That way, we can cover the rest of
the sweet things that can be done with the ruby object model. 
