(defvar journal-ideas-directory "~/Journal/joelmccracken.github.com/_posts/ideas")
(defvar journal-ideas-org-directory "~/Journal/joelmccracken.github.com/_posts/ideas/org")
(defvar journal-publish-directory "~/Journal/joelmccracken.github.com/_posts")

(defvar journal-notes-archive-directory "~/Journal/notes/archive")
(defvar journal-site-directory "~/Journal/joelmccracken.github.com")

(defvar journal/idea-magic-date "2099-01-01")

(defvar journal-map (make-sparse-keymap))

(define-key journal-map (kbd "g") 'journal/magit)
(define-key journal-map (kbd "n") 'journal/new-idea)
(define-key journal-map (kbd "s") 'journal/show-ideas)
(define-key journal-map (kbd "p") 'journal/publish-idea)

(global-set-key (kbd "C-c j") journal-map)



(defun journal/start-server ()
  "start the journal's jekyll server in a shell"
  (interactive)
  (shell "journal-server")
  (goto-char (point-max))
  (insert "cd ~/Journal/ \n")
  (comint-send-input)

  ;; we want to run this before running the server just to see if
  ;; there is any errors
  (goto-char (point-max))
  (insert "bin/rake preview \n")
  (comint-send-input))


(defun journal/stop-server ()
  (interactive)
  (kill-buffer "journal-server"))

(defun journal/show-ideas ()
  "dired of the ideas directory"
  (interactive)

  ;; the idea dir buffer might exist currently, so do this, then kill
  (dired journal-ideas-directory)
  (kill-buffer)

  ;; do it again
  (dired journal-ideas-directory)
  
  ;;  finally sort __now__ that we know is fresh
  (dired-sort-toggle-or-edit))

(defun journal/new-idea (idea-name)
  (interactive "Midea name: ")
  
  (let ((file-slug (journal/file-namify-string idea-name))
        draft-buffer notes-buffer)

    (setq draft-buffer
          (find-file (journal/draft-filename file-slug)))

    ;; current file is draft, so insert skeleton
    (journal/insert-post-skeleton-if-empty idea-name)))


(defun journal/draft-filename (idea-slug)
  (concat journal-ideas-directory "/"
          journal/idea-magic-date
          "-" idea-slug ".markdown"))


(defun journal/insert-post-skeleton-if-empty (idea-name)
  (when (= 0 (buffer-size))
    (insert "---\n")
    (insert "status: idea\n")
    (insert "layout: post\n")
    (insert (concat "title: '" idea-name "'\n"))
    (insert "---\n")))



(ert-deftest journal/file-namify-string-tests ()
  (should (equal (journal/file-namify-string "they're here")
                 "theyre-here"))

  (should (equal (journal/file-namify-string " they're here")
                 "theyre-here"))
  (should (equal (journal/file-namify-string " they're here ")
                 "theyre-here"))
  (should (equal (journal/file-namify-string "   they're here ")
                 "theyre-here"))

  (should (equal (journal/file-namify-string " Rapid Application Prototyping with Emacs  ")
                 "rapid-application-prototyping-with-emacs"))
  (should (equal (journal/file-namify-string "Going to the Bathroom part 78")
                 "going-to-the-bathroom-part-78")))


(defun journal/file-namify-string (str)
  (downcase (replace-regexp-in-string
             " " "-"
             (replace-regexp-in-string  "\\(^[ ]+\\|[ ]+$\\|[^a-z0-9_ ]+\\)" "" str))))


(defun journal/todays-date ()
  (format-time-string "%Y-%m-%d"
                      (current-time)))

(defun journal/published-file-name (slug)
  (concat journal-publish-directory "/" (journal/todays-date) "-" slug ".markdown"))



(defun journal/idea-name-from-filename (filename)
  (when
      (string-match (concat "^" journal/idea-magic-date "-\\(.*\\)\\(.markdown\\|.md\\)") filename)
    (match-string 1 filename)))


(defun journal/publish-idea ()
  (interactive)
  (let* ((orig-name (buffer-file-name))
         (basename (file-name-nondirectory orig-name))
         (idea-slug (journal/idea-name-from-filename basename))
         (new-file-name (journal/published-file-name idea-slug)))
    
    (when (yes-or-no-p (format "really publish file %s as %s?"
                               orig-name
                               new-file-name))
      (journal/change-post-status-to-published)
      (write-file new-file-name)
      (delete-file orig-name))))




(defun journal/change-post-status-to-published ()
  (interactive)
  (goto-char (point-min))
  (replace-regexp "^status: idea" "status: published"))



;; data migrations (?)

(defun journal-update-add-status-to-posts ()
  (interactive)
  (dolist (file (identity (remove nil
                                  (mapcar (lambda (file)
                                            (and (file-regular-p file) file))
                                          (directory-files journal-publish-directory t)))))
    (message "trying to update %S" file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (re-search-forward "---")
      (next-line)
      (beginning-of-line)
      (insert "status: published\n")
      (write-file file))))


(defun journal-start ()
  (journal/start-server))

(defun journal-update-add-status-to-drafts ()
  (interactive)
  (dolist (file (identity (remove nil
                                  (mapcar (lambda (file)
                                            (and (file-regular-p file) file))
                                          (directory-files journal-ideas-directory t)))))
    (message "trying to update %S" file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (re-search-forward "---")
      (next-line)
      (beginning-of-line)
      (insert "status: idea\n")
      (write-file file))))




;; depracated stuff below (move stuff up as it is found useful)


(defun journal-idea-list ()
  (delete nil
          (mapcar (lambda (filename)
                    (journal-idea-name-from-filename filename))
                  (directory-files journal-ideas-directory))))


(defun journal-archived-notes-file-for-slug (slug)
  (concat journal-notes-archive-directory "/" slug ".org"))




(defun journal-idea-notes-filename-from-slug (idea-slug)
  (concat journal-ideas-org-directory "/" idea-slug ".org"))


(defun journal-find-idea-file (idea-slug file-type)
  (find-file
   (case file-type
     ('draft (journal-idea-draft-filename-from-slug idea-slug))
     ('notes (journal-idea-notes-filename-from-slug idea-slug)))))





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


(defun journal-magit ()
  (interactive)
  (magit-status journal-site-directory))

(add-hook 'markdown-mode-hook 'orgstruct-mode)



(provide 'journal)
