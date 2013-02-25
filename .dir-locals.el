((nil . ((eval .
               (progn
                 (defun journal-project-started ()
                   (when (string-match "Journal" project-switched)
                     (load (concat project-switched "journal.el"))
                     (journal-start)))
                 (add-to-list 'projectile-switch-project-hook
                              'journal-project-started))))))
