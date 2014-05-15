---
title: Reading from stdin and writing to stdout with Emacs batch
layout: post
categories: emacs org-mode
---

So, this is something that has irritated me from time to time about
emacs. I have long thought that it was not possible to read data from stdin
from emacs when in batch mode (I had discovered that `(message "foo")`
would write foo to stdout, so that wasn't an issue). I have tried to
search google for it in the past, but I never found what I was looking
for.

I have again been dealing with this problem, and finally found the
solution, which is documented at
[(info "(elisp) Batch Mode")](http://www.gnu.org/software/emacs/manual/html_node/elisp/Batch-Mode.html#Batch-Mode).

The relvant selection is:

    Any Lisp program output that would normally go to the echo area,
    either using `message', or using `prin1', etc., with `t' as the stream,
    goes instead to Emacs's standard error descriptor when in batch mode.
    Similarly, input that would normally come from the minibuffer is read
    from the standard input descriptor.  Thus, Emacs behaves much like a
    noninteractive application program.  (The echo area output that Emacs
    itself normally generates, such as command echoing, is suppressed
    entirely.)


So, we can use `read-from-minibuffer` to get data from stdin, and
`princ` to write data.

## A Real-World Example

My goal was to write a plugin for Jekyll that will convert org mode
files into blog posts (which is working, but I still need to tweak
things, so that will be saved for a later post).

Basically, Jekyll reads the files that represents your posts, removes
yaml metadata from the head of the document, and hands the content of
the file to the converter plugin. Thus, I have the text contents of an
org file, and need to convert that to an html version. In an ideal
world, I would be able to pipe this text into emacs and have it output
the html I want.

The portion of elisp that implements this:

    (defun compile-org-file ()
      (interactive)
      (let ((org-document-content "")
            this-read)
        (while (setq this-read (ignore-errors
                                 (read-from-minibuffer "")))
          (setq org-document-content (concat org-document-content "\n" this-read)))

        (with-temp-buffer
          (org-mode)
          (insert org-document-content)
          (org-html-export-as-html)
          (princ (buffer-string)))))


There are a number of things to point out, here.

`read-from-minibuffer` was tricky, and needed some special consideration:

- No prompt should be displayed to the user, since data is coming in
  unprompted. Thus, an empty string argument.
- This function reads in a single line at a time. As data is
  read in, it must be appended to what has already been read.
- When EOF is read, an error is thrown, which is useless to
  us. Instead of worrying about the error, we can wrap the function
  call in an `ignore-errors` macro, which will return `nil` when an
  EOF occurs.

Once the entire data is read in, we can convert it org mode.

- A temporary buffer is created, which is converted to `org-mode`
- The contents of the org document are inserted into the buffer.
- `org-html-export-as-html` creates a new buffer, makes it current,
  and inserts the exported html into that buffer.
- `princ` writes the converted html to stdout.

Overall, I am pretty happy with the solution. This is one of the
lesser known areas of Emacs, and I'm really glad that I finally know
how to do it, because it is a problem I have had to deal with on
several occasions. 
