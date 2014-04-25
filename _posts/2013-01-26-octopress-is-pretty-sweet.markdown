---
layout: post
title: "Use Jekyll? You really should be using Octopress"
date: 2013-01-26 10:06
comments: true
categories: [Code, Writing]
---

<div class='preamble'>
Do you use Jekyll? Are you interested in static publishing? Then
you should checkout Octopress, a project I personally ignored for too
long. It makes using Jekyll much better.
</div>


Before I learned C, years ago, I was under the impression that it was
a hard language to learn. I wanted to learn more about computers, and
C++ seemed easier. Everyone said it was "a better C", and it seemed
like the right way to go. 

I quickly learned that my initial impression was wrong. You basically
need to understand C in order to be effective at all with C++.
So, I learned C, and life was good. Fortunately, I've never had to
really use C++ again since then.

I still often get first impressions wrong.
[Octopress](http://octopress.org/docs/blogging/)
is a blogging framework for Jekyll. 
I have known about Octopress' existence
for a long time. The thing is, I ignored it. I thought it just
wouldn't be very useful to me. 

My impression was that Octopress was designed primarily to act as a
styling framework. There are lots of Jekyll blogs around that have
the same Octopress style. Writing about creating an "x" plugin
for Octopress was popular for a time, and these almost all seemed to
be very display-related. 

However, when I recently took the time to look it over again, I
noticed that Octopress includes much more than a styling solution. It
includes scripts that help with all sorts of things you need to do
with a Jekyll website. I was amazed at how much it contains. 
It even has solutions to some long-time
frustrations I have had with Jekyll. 

The other thing that struck me was how flexibly Octopress could be
used. Octopress is 
distributed as a git repository, not as a library. All of its
code is really easy to customize to the specific needs of your application.
Although the Jekyll documentation is good, Octopress' sample configuration
files are really much easier to comprehend than by referencing the Jekyll
wiki.
So, it was clear that Octopress would make it much easier to manage
your Jekyll website, not harder.

It was clear to me that I should adopt Octopress.

Adopting Octopress
-----------

So, how do we migrate an existing Jekyll blog to Octopress?

First, 
[follow the documentation](http://octopress.org/docs/setup/), 
to set up an Octopress blog
with a few of my own modifications: 

     cd ~/Journal # the place where I currently store all of my 
                  # journal stuff
     git clone git://github.com/imathis/octopress.git JournalOctopress

     cd JournalOctopress
     
     git remote rename origin imathis
     
     bundle install --binstubs # so I can skip the 'bundle exec'
                               # and I don't use rbenv, I use RVM

     bin/rake install # install default presentation files 

     bin/rake preview # start the server
     
     
After verifying that everything is working by visiting
`localhost:4000` in the browser, 
lets check out what Rakefile goodies we have: 

    bash-3.2$ bin/rake -T
    rake clean                     # Clean out caches: .pygments-cache, .gist-cache, .sass-cache
    rake copydot[source,dest]      # copy dot files for deployment
    rake deploy                    # Default deploy task
    rake gen_deploy                # Generate website and deploy
    rake generate                  # Generate jekyll site
    rake install[theme]            # Initial setup for Octopress: copies the default theme into the path of Jekyll's generator.
    rake integrate                 # Move all stashed posts back into the posts directory, ready for site generation.
    rake isolate[filename]         # Move all other posts than the one currently being worked on to a temporary stash location (stash) so regenerating the site happens much more quickly.
    rake list                      # list tasks
    rake new_page[filename]        # Create a new page in source/(filename)/index.markdown
    rake new_post[title]           # Begin a new post in source/_posts
    rake preview                   # preview the site in a web browser
    rake push                      # deploy public directory to github pages
    rake rsync                     # Deploy website via rsync
    rake set_root_dir[dir]         # Update configurations to support publishing to root or sub directory
    rake setup_github_pages[repo]  # Set up _deploy folder and deploy branch for Github Pages deployment
    rake update_source[theme]      # Move source to source.old, install source theme updates, replace source/_includes/navigation.html with source.old's navigation
    rake update_style[theme]       # Move sass to sass.old, install sass theme updates, replace sass/custom with sass.old/custom
    rake watch                     # Watch the site and regenerate when it changes



Ohhh, so many choices. The most interesting ones to me are:


    rake new_page[filename]        # Create a new page in source/(filename)/index.markdown
    rake new_post[title]           # Begin a new post in source/_posts
    ...
    rake preview                   # preview the site in a web browser

For me, I next started to write this very blog post:

    bin/rake new_post["Octopress is Pretty Sweet"]

This let me record my observations as they were happening.


Okay. Running `bin/rake preview` and visiting `localhost:4000` shows
the new blog post. Lets start migrating the old content over. I have
always hated the default Jekyll URLs, but because I was using Github
to compile Jekyll, there wasn't a good way for me to change the URLs
that already existed. Migrating to Octopress (and also compiling the
entire site on my own machine) makes this really easy. I also wanted
to make sure the old links still directed to the current content. 


A quick search revealed a Jekyll 
[plugin](https://github.com/ts mango/jekyll_alias_generator)
that makes it trivial to set up redirects. The only thing that
remained was creating the redirect information that the alias
generator requires. This seems like it should be pretty easy to
automate, so I added a new task to the `Rakefile` provided by
Octopress:

    task :joel_copy_old_posts do |t|
    
      old_posts_dir = "../joelmccracken.github.com/_posts/*"
      new_posts_dir = "source/_posts/"
    
      Dir[old_posts_dir].grep(/markdown/).each do |post_file|
        post_filename = File.basename(post_file)
        parts = post_filename.match(/(\d+)-(\d+)-(\d+)-(.*)\.markdown/)
        year = parts[1].to_i
        month = parts[2].to_i
        day = parts[3].to_i
        alias_name_to_add = Date.new(year, month, day).strftime("/%Y/%m/%d/#{parts[4]}.html")
    
        new_filename = new_posts_dir+post_filename
    
    
        old_file = File.open(post_file, "r")
        new_file = File.open(new_filename, "w")
    
        old_file.lines do |line|
          if line =~ /^title/
            new_file.puts line
            new_file.puts "alias: #{alias_name_to_add}"
          else
            new_file.puts line
          end
        end
      end
    end

    
Basically, this code just takes every post, generates an alias name,
adds the alias to the YAML frontmatter. So, it converts the YAML
frontmatter from an old post: 

    ---
    status: published
    layout: post
    title: 'An Alternative to "I Am Not My Code"'
    ---

into this: 

    ---
    status: published
    layout: post
    title: 'An Alternative to "I Am Not My Code"'
    alias: /2012/12/24/an-alternative-to-i-am-not-my-code.html
    ---


Easy, and it works well. The code could be a bit better, but since I only
ever need to run it once, this level of sophistication is perfect.

The only thing left is to change the default theme. I found the
[venice](https://github.com/octopress-themes/venice)
theme and installed it:

    git submodule add git://github.com/octopress-themes/venice.git .themes/venice
    git add git add .themes/
    git commit -m 'added venice theme'
    bin/rake install[venice]

Hopefully, I will be changing the venice layout soon to something I
have personalized. 
This, also, seems easier with Octopress. Since its
themes are fairly complete, modifying them in the future looks easier
than starting from scratch.


Finally, deploying. I just followed 
[the provided instructions](http://octopress.org/docs/deploying/github/):

    bin/rake setup_github_pages # used 'git@github.com:joelmccracken/joelmccracken.github.com'
    bin/rake generate
    bin/rake deploy


Overall, my experience with Octopress has been very positive. So far, I
have not hit any problems. So, if you have been holding off because,
like me, you didn't think it would be very useful, I suggest giving it
a try. It might surprise you how nice it is. 
