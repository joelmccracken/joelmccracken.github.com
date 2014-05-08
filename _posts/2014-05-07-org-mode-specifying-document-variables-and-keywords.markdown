---
layout: post
title: "Org-mode, File Variables, Properties, and Keywords"
date: 2014-05-07 09:42:08 -0400
comments: true
categories: emacs org-mode
---

Org mode is super amazing, as I am sure you are aware. I use it
throughout the day, every day.

As I try to extend it, there are a few concepts that have confused
me, which I want to cover here, because I am sure they have confused
other people, too: keywords, properties, and plain old variables. Each
of these essentially are used to set keys to values.

# Keywords

Keywords look like this:

````
 #+KEYWORD: value
````

Keywords are predefined and used for specific things. They don't seem
to be explicity explained anywhere in the manual, but they are
referenced throughout.

Keywords might be conceptualized as global to the entire file.

More information:
http://orgmode.org/worg/dev/org-syntax.html#Keywords
http://orgmode.org/manual/In_002dbuffer-settings.html#In_002dbuffer-settings


# Properties

Properties look like this:

````
 * header1
 :PROPERTIES:
 :LOCATION: foo
 :STYLE:    scoot
 :END:
 * header2
````

Properties are used to provide settings that are specific to the
structure. Above, `header1` has properties set for location and style,
whereas `header2` does not have any values for these properties.

Default property values may be set with the property keyword, like so:


````
 #+PROPERTY: property-name property-value
````

More Information:

1. http://orgmode.org/manual/Properties-and-Columns.html#Properties-and-Columns
2. http://orgmode.org/manual/Property-syntax.html#Property-syntax
3. http://orgmode.org/manual/In_002dbuffer-settings.html#In_002dbuffer-settings
   (search for "property")


# Variables

Finally, we come to variables. The org mode documentation refers to
variables that control settings on a regular basis, and I have never
been sure what the right solution is.

Now, I'm familiar with Emacs lisp. I know what is meant by variables
in a general sense, but it always seemed like there should be a syntax
to set any variable. I was going to write in this article that I
haven't found it, but I have since found it. Setting arbitrary
variables looks like this:

````
 #+BIND: variable-name variable-value
````

However, since it took me some time to find it, I have used
file variables to achieve the same thing. I'll include a link to the
documentation below about it.


More Information:

1. http://orgmode.org/manual/Export-settings.html#Export-settings
   (search for "BIND")
2. http://www.gnu.org/software/emacs/manual/html_node/emacs/Specifying-File-Variables.html



HTH,

Joel

