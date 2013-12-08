---
layout: post
title: "Using Custom Rubies in the Shebangs of Executables"
date: 2013-12-08 13:15
comments: true
categories: ruby unix automation
---

I try to automate things.
Ruby scripts as Unix executables work well for many automation tasks.
They sit sits right at the point of both being simple and powerful.
However, the shebangs for such scripts are typically ugly and
brittle.

In Unix terms, a shebang is first line of a script which how the
script should be interpreted.
A typical shebang looks like this:

```
    #!/usr/bin/bash

    echo "hello, world"
```

The first line of the above script -- the `#!/usr/bin/bash` --
is the "shebang" we are talking about.

For a Ruby script, a shebang looks more like this:

```
    #!/usr/bin/ruby

    puts "hello, world"
```

On some systems, this is fine. But what if the `ruby` you would like to
use is not at that location on another system?
The `env` command can be used as an additional level of abstraction
away from the literal path to the interpreter.
The `env` env command will
search your `$PATH` environment variable for the executable you specify
as its first argument. So, the below example would be run with the
first `ruby` that the `env` command finds in your `$PATH`:

    #!/usr/bin/env ruby

    puts "hello, world"

Thus, our script can be run on a computer that
has Ruby in a different location, and
everything will work well.

However, this doesn't completely end the complication. Say, for
example, our computer has Ruby version `1.9.3` and uses `1.9.3`
features, whereas another has `1.8.7`.

If we only specify `ruby` in the script's shebang, our script
won't work on that other computer.

We really want to be able to declaratively specify the *type* of
interpreter our script requires.
We do not care about the location of the interpreter. We just care
that it exists and that it provides the features our script requires.
For example, we need 'ruby, but only version 2.0'.
Some systems provide us
with executables
named 'ruby-<version x.y.z>', but not all.

Fortunately, we can build build *our own* executables and reference
them in our shebangs. We can make the names of those executables as
descriptive as necessary.

Lets say our system uses RVM, and we want to be able to write scripts
that depend upon the fact that they are running in Ruby 2.0. I
assume you have `~/bin` in your `$PATH`:

First, Create the file `~/bin/ruby-2.0`.
Inside it, add the following lines:

```
  #!/usr/bin/env bash
  exec rvm ruby-2.0.0-<your patch level here> do ruby "$@"
```

Then, save the file and mark it as executable.

That's it! You can now use the ruby-2.0 executable in your scripts.
This shows the whole thing, all set up, and how it works together:

```
    bash-3.2$ which ruby-2.0
    /Users/joel/bin/ruby-2.0

    bash-3.2$ cat `which ruby-2.0`
    #!/usr/bin/env bash
    exec rvm ruby-2.0.0-p247 do ruby "$@"

    bash-3.2$ cat test.rb
    #!/usr/bin/env ruby-2.0

    puts RUBY_VERSION

    bash-3.2$ ./test.rb
    2.0.0
```

If we wanted to use our `./test.rb` script on a system that doesn't
use RVM and has a ruby 2.0 version at `/usr/local/ruby-2.0/bin/ruby`
on that system we can create a `ruby-2.0` executable with the
following:

```
    #!/usr/bin/env bash
    exec /usr/local/ruby-2.0/bin/ruby "$@"
```

The two parts of the above scripts that need to be mentioned are
`exec` and `"$@"`. Exec tells the bash script "replace the currently
executing script with the following script". This makes dealing with
IO simpler and the `ruby-2.0` executable won't stay around as a useless
process for the duration of our script's execution.
The formulation `"$@"` tells bash to
pass all of its arguments along to the next process.

This technique can be adapted to all sorts of applications, not just
Ruby. The ability to create your own executables removes a
barrier to automating the stuff in your life.
