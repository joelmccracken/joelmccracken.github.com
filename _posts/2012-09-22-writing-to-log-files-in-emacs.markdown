---
status: published
layout: post
title: 'Writing to Log files in Emacs Without a Message'
---


# Writing to Log files in Emacs Without a Message #

I think that one of the keys to making scripting manageable is via
good logging. Especially when it comes to personal solutions,
over-engineering a solution can be a huge waste. 

I personally have a function called `emacs-log` that lets me see what
all of my hacks have been doing. It works well. However, one of the
irritations that I have with this rests within the function
`write-file`, which messages the user when a file is written. This can
be irritating when a log file is being written every few seconds.  


The wonderful folks on #emacs suggested `write-region`. This function
is pretty common, so I've seen it before. Apparently, it has an option
that allows you to skip the call 
to `message`, however the documentation is a little odd, so I missed
it myself. Setting the `VISIT` parameter to a symbol will make it skip
the message. Also, it has an `APPEND` parameter, which is ideal for
logs. 

So, I ended up with: 

    (with-temp-buffer
      (insert log-str)
      (write-region (point-min) (point-max) log-file-name t
                    'nomessage))

If you want to know, the full function is: 

    (defun emacs-log (&rest args)
      (let* ((log-buffer-name  "*My-Emacs-Log*")
             (log-buffer (get-buffer-create log-buffer-name))
             (log-file-name "~/Logs/my_emacs_log.log")
             (log-str (concat
                       (current-time-string)
                       " -- "
                       (apply 'format args)
                       "\n")))
        (save-excursion
          (with-current-buffer log-buffer
            (goto-char (point-max))
            (insert log-str))
          (with-temp-buffer
            (insert log-str)
            (write-region (point-min) (point-max) log-file-name t
                          'nomessage)))))
