(require 'org)
(require 'ox)
(require 'ox-html)

;; only supports a single client at a time.

(defun compile-org-file (content)
  (interactive)
    (with-temp-buffer
      (org-mode)
      (insert content)
      (org-html-export-as-html nil nil nil t)
      (buffer-string)))

(defun org-jekyll-daemon-sentinel (process event)
  ;; not doing anything for now
  )

(setq debug-on-error t)

(defvar org-jekyll-convert-string nil)
(defvar org-jekyll-convert-string-size nil)

(defun org-jekyll-str-received (process str)
  (setq org-jekyll-convert-string
        (concat org-jekyll-convert-string
                str))
  (unless org-jekyll-convert-string-size
    (when (string-match "Length: \\([[:digit:]]+\\)\n" org-jekyll-convert-string)
      (setq org-jekyll-convert-string-size
            (string-to-number (match-string 1
                                            org-jekyll-convert-string)))
      (setq org-jekyll-convert-string
            (replace-match "" nil nil
                           org-jekyll-convert-string))))

  (when org-jekyll-convert-string-size
    (when (= (string-bytes org-jekyll-convert-string)
             org-jekyll-convert-string-size)
      (when (> org-jekyll-convert-string-size 0)
        (process-send-string process
                             (compile-org-file org-jekyll-convert-string)))
      (delete-process process)
      (setq org-jekyll-convert-string nil
            org-jekyll-convert-string-size nil))))

(defun org-jekyll-daemon-filter (process string)
  (org-jekyll-str-received process string))

(defvar org-jekyll-server-process nil)

(defun start-compile-server ()
  (setq org-jekyll-server-process
        (make-network-process :name "org-jekyll-daemon"
                              :server t
                              :sentinel 'org-jekyll-daemon-sentinel
                              :filter 'org-jekyll-daemon-filter
                              :service 9876
                              :host 'local)))
