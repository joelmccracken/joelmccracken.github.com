---
layout: post
title: "How To Develop a Chef Recipe with Chef Solo"
date: 2013-09-16 19:13
comments: true
categories: chef automation
---

Lets say you are working with Chef, and you realize that you need to
start building a recipe of your own. The most obvious way to go about this is add the recipe
to one of your
cookbooks and run chef, watching the output.

There are some problems with this method though. For one, a chef run can take a really long time,
and we want fast results, right?
For another, you might need to commit the changes you are testing
so they can be included in your normal test run. For example, the
librarian-chef tool wants to pull cookbooks from a git repository.

I ran into this exact scenario when I wanted to develop a recipe that would apply
to my laptop, which runs OS X. Since I was also actually working on my laptop, doing a full
chef run to test a recipe could be very frustrating.

The solution I came up with was to use Chef Solo. Chef Solo is a really simple way to use
Chef without requiring a server. All you need is Chef
installed on the system you want to configure and a couple of extra simple files.
These extra files provide the things Chef needs to run, such as where to find cookbooks,
and which recipes to run.


So, how do we do this? First, clone the repository that contains the cookbooks you
are interested in modifying, and then cd into it:

    git clone path/to/cookbook/repository.git

    cd repository

Next we need to install some support software. Install Chef:

    gem install chef

If your project uses librarian-chef like mine does, install it as well, and
then use it to acquire any cookbook dependencies:

    gem install librarian-chef

    librarian-chef install


Now we should be ready to start using chef solo. The first file we need is
the configuration file, `solo.rb`. Within it add the following:

    cookbook_path File.expand_path("./cookbooks")

This tells chef solo where to find the cookbooks for its configuration.

Next we need add an `attrs.json` file which is used to specify any node attributes.
Add the following to it:

    {
      "run_list": ["recipe[the_cookbook_you_are_working_on::the_recipe_you_are_working_on]"]
    }

You'll notice that its only member is `run_list`. This is a list of the recipes
that should be run on the node. Since we are trying to establish a minimalistic
run that will be as fast as possible, the run list should just include the recipe
we are testing.

Finally, we can test out our single recipe by running:

    librarian-chef install && sudo chef-solo -c solo.rb -j attrs.json


The `librarian-chef install` command will pull cookbook changes as we make them into the
`cookbooks` directory so they will be accessible to `chef-solo`, and `chef-solo` will then.

Thats it! You can now work on your recipes and run them with this (relativey) quick command.

