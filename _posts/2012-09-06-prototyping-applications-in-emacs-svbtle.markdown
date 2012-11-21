---
status: published
layout: post
title: 'Prototyping Applications in Emacs: My Own Svbtle'
---

# Prototyping Applications in Emacs: #
## My Own Svbtle ##

I'm always on the lookout for new ways to improve myself.
Emacs is one of the ways that I do this. 
Because all of Emacs' features are extremely accessible, it is easy to
quickly build what you need to get things done. 

I have written a few of these "mini-applications", but one is fresh in
my mind: svbtle. 
When svbtle was announced/released, I had mixed feelings
about it. However, one of the things that I *do* really like is the
application's split between published posts and ideas. 

In case you don't remember, everything in svbtle is either a published
post, or an idea. I think this split works, because pre-publishing,
writing is so... ephemeral. However, once something is out there, it
takes on a completely different character. 

I feel like *draft* has a negative connotation. An unpublished draft
is basically an undone to-do. Even the word draft implies work that
still needs to be done. 

However, labeling drafts as *ideas* changes them. Ideas are
positive. The more ideas you have, the better. And, an unpublished
idea is just, well, still an idea. I think this cognitive
shift encourages the writing process. 

So, I made a mental note, and began thinking about how I could implement this
in my writing process. Over the last few weeks, I spent 20 or so minutes here
and there hacking some code together that lets me think this way. So,
for a total of *at most* two hours, I got most of the 
benefit that I perceive from svbtle. And, that makes me very happy.

Now, time for the code. It isn't pretty or well-factored, but it
does what I need. I want to show it like this because it
works, and is good enough. There's nothing magic here; its straight-up
maneuvering of files, Emacs buffers, and window layouts. It also sits
on top of Jekyll, because that I what I started with. 


    (defvar journal-ideas-directory "~/Journal/joelmccracken.github.com/_posts/ideas")
    (defvar journal-ideas-org-directory "~/Journal/joelmccracken.github.com/_posts/ideas/org")
    (defvar journal-publish-directory "~/Journal/joelmccracken.github.com/_posts")
    
    (defvar journal-notes-archive-directory "~/Journal/notes")
    (defvar journal-site-directory "~/Journal/joelmccracken.github.com")
    
    (defvar journal-idea-magic-date "2099-01-01")
    
    (defvar journal-map (make-sparse-keymap))
    
    (define-key journal-map (kbd "g") 'journal-magit)
    (define-key journal-map (kbd "n") 'journal-idea-new)
    (define-key journal-map (kbd "o") 'journal-idea-open)
    (define-key journal-map (kbd "p") 'journal-idea-publish)
    
    (global-set-key (kbd "C-c j") journal-map)
    
    
    (defun journal-idea-name-from-filename (filename)
      (when 
        (string-match (concat "^" journal-idea-magic-date "-\\(.*\\)\\(.markdown\\|.md\\)") filename)
        (match-string 1 filename)))


    (defun journal-idea-list ()
      (delete nil
              (mapcar (lambda (filename)
                        (journal-idea-name-from-filename filename))
                      (directory-files journal-ideas-directory))))
    
    (defun journal-todays-date ()
      (format-time-string "%Y-%m-%d"
                          (current-time)))
    
    (defun file-namify-string (str)
      (downcase (replace-regexp-in-string
                 " " "-"
                 (replace-regexp-in-string  "\\(^[ ]+\\|[ ]+$\\|[^a-z0-9_ ]+\\)" "" str))))
    
    
    (ert-deftest file-namify-string-tests ()
      (should (equal (file-namify-string "they're here")
                     "theyre-here"))
      
      (should (equal (file-namify-string " they're here")
                     "theyre-here"))
      (should (equal (file-namify-string " they're here ")
                     "theyre-here"))
      (should (equal (file-namify-string "   they're here ")
                     "theyre-here"))
    
      (should (equal (file-namify-string " Rapid Application Prototyping with Emacs  ")
                     "rapid-application-prototyping-with-emacs"))
      (should (equal (file-namify-string "Going to the Bathroom part 78")
                     "going-to-the-bathroom-part-78")))
    
    
    (defun journal-idea-notes-file (idea-slug)
      (concat journal-ideas-org-directory "/" idea-slug ".org"))
    
    (defun journal-archived-notes-file-for-slug (slug)
      (concat journal-notes-archive-directory "/" slug ".org"))
    
    (defun journal-publish-file-name (slug)
      (concat journal-publish-directory "/" (journal-todays-date) "-" slug ".markdown"))
    
    (defun journal-insert-post-skeleton-if-empty (idea-name)
      (when (= 0 (buffer-size))
        (insert "---\n")
        (insert "layout: post\n")
        (insert (concat "title: '" idea-name "'\n"))
        (insert "---\n")))
    
    
    (defun journal-window-setup-for-idea (idea-draft-buffer idea-notes-buffer)
      ""
      (delete-other-windows)
      (switch-to-buffer idea-draft-buffer)
    
      (split-window-horizontally)
      
      (other-window 1)
    
      (switch-to-buffer idea-notes-buffer))
    
    
    
    (defun journal-idea-draft-filename-from-slug (idea-slug)
      (concat "~/Journal/joelmccracken.github.com/_posts/ideas/"
              journal-idea-magic-date
              "-" idea-slug ".markdown"))
    
    
    (defun journal-idea-notes-filename-from-slug (idea-slug)
      (concat journal-ideas-org-directory "/" idea-slug ".org"))
    
    
    (defun journal-find-idea-file (idea-slug file-type)
      (find-file
       (case file-type
         ('draft (journal-idea-draft-filename-from-slug idea-slug))
         ('notes (journal-idea-notes-filename-from-slug idea-slug)))))
    
    
    (defun journal-idea-new (idea-name)
      (interactive "Midea name: ")
      
      (let ((file-slug (file-namify-string idea-name))
            draft-buffer notes-buffer)
        
        (setq draft-buffer (journal-find-idea-file file-slug 'draft))
        ;; current file is draft, so insert skeleton
        (journal-insert-post-skeleton-if-empty idea-name)
        
        (setq notes-buffer (journal-find-idea-file file-slug 'notes))
        
        (journal-window-setup-for-idea draft-buffer notes-buffer)))
    
    
    (defun journal-idea-open ()
      (interactive)
      (let ((file-slug (ido-completing-read "Idea: " (journal-idea-list))))
        (journal-window-setup-for-idea
         (journal-find-idea-file file-slug 'draft)
         (journal-find-idea-file file-slug 'notes))))
    
    
    (defun journal-insert-idea-draft-into-notes (draft-buffer notes-buffer)
      (save-excursion
        (with-current-buffer  notes-buffer
          (goto-char (point-min))
          (insert "* draft at publish time\n")
          (insert  (with-current-buffer draft-buffer
                     (buffer-string)))
          (insert "\n"))))
    
    
    (defun journal-idea-publish ()
      (interactive)
      (let* ((orig-name (buffer-file-name))
             (basename (file-name-nondirectory orig-name))
             (idea-slug (journal-idea-name-from-filename basename))
             (new-notes-name (journal-archived-notes-file-for-slug idea-slug))
             (new-file-name (journal-publish-file-name idea-slug)))
        
        (when (yes-or-no-p (format "really publish file %s as %s?"
                                   orig-name
                                   new-file-name))
          (let ((idea-buffer (current-buffer))
                (notes-buffer (find-file (journal-idea-notes-filename-from-slug idea-slug))))
            (journal-insert-idea-draft-into-notes idea-buffer notes-buffer)
            (with-current-buffer idea-buffer
              (write-file new-file-name)
              (kill-buffer))
            (with-current-buffer notes-buffer
              (write-file new-notes-name)
              (kill-buffer))
            
            )
          (delete-file orig-name)
          (delete-file (journal-idea-notes-file idea-slug)))))
    
    
    (defun journal-magit ()
      (interactive)
      (magit-status journal-site-directory))
    
    
    
    (provide 'journal)
    
