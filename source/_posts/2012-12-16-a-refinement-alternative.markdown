---
status: published
layout: post
title: 'A Refinements Alternative'
alias: /2012/12/16/a-refinement-alternative.html
---

If you pay attention to Ruby, you almost certainly know about all of
the refinements drama that happened. I'll assume that you don't
though.

Basically, refinements is a feature that was going to be in Ruby 2.0,
but has since been removed as a feature. There are a bunch of more
thorough
[explanations](http://timelessrepo.com/refinements-in-ruby)
of the idea behind refinements, but basically the idea is that you can
monkeypatch a class, and those monkeypatches only exist within a
certain scope of execution. 

With refinements, you can add functionality to a
class that is specific to the sender,  without adding logic to the
receiver that is not generally applicable.

So, here is something that lets you do this in a library. This doesn't
*completely* replicate native functionality, but it does let you get
90% of the way. Basically, we create classes that wrap other classes.
First, the tests:

    describe Refinery do
      it "ignores classes that have not been refined" do
        ref = Refinery.new
        "lol".class.must_equal ref.refine("lol").class
      end
    
      it "refines" do
        ref = Refinery.new do
          refine String do
            def to_ruby
              "\"#{to_s}\""
            end
          end
        end
    
        ref.refine("hi").to_ruby.must_equal "\"hi\""
        ref.refine("hi").length.must_equal 2
    
        proc do
          "hi".to_ruby
        end.must_raise NoMethodError
      end
    end

And, the code: 

    require 'delegate'
    
    class Refinery
      def initialize(&refine_def)
        @refinement_definition = RefinementDefinition.new
        @refinement_definition.instance_eval &refine_def if block_given?
      end
    
      def refine(raw_object)
        @refinement_definition.perform_refine(raw_object)
      end
    
      class SingleRefinementDefinition
        def initialize(type, &defn_block)
          @type = type
    
          @refined_class = Class.new(SimpleDelegator)
    
          @refined_class.class_eval(&defn_block) if block_given?
        end
    
        def matches?(obj)
          obj.is_a? @type
        end
    
        def perform_refine obj
          @refined_class.new obj
        end
      end
    
      class RefinementDefinition
        def initialize
          @refinements = []
        end
    
        def refine type, &def_blk
          @refinements << SingleRefinementDefinition.new(type, &def_blk)
        end
    
        def perform_refine obj
          selected_ref = @refinements.select do |ref|
            ref.matches? obj
          end
          if selected_ref.first
            selected_ref.first.perform_refine obj
          else
            obj
          end
        end
      end
    end

I guess I think should put this somewhere better, but I'm not sure
where, so I'm open to suggestions!
