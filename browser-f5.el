;;; browser-f5.el --- Refresh web browser in Emacs


;;; Commentary:

;;; Code:

(require 'cl-lib)

(defgroup browser-f5 nil
  "Browser refresh utility for multi browsers and multi platform"
  :group 'external)

(defcustom browser-f5-auto-save-buffer t
  "Non-nil means saving buffer before browser refresh"
  :type 'boolean
  :group 'browser-f5)

(defvar-local browser-f5--selected-window nil
  "For internal use.
Store the window id of currently selected instance.")

;; ======================================================
;; Tools
;; ======================================================

(defun browser-f5-call-process-to-string (program &rest args)
  "`shell-command-to-string' is too slow for simple task, so use this."
  (message (string-join (cons program args) " "))
  (with-temp-buffer
    (apply #'call-process program (append '(nil t nil) args))
    (buffer-string)))

;; ======================================================
;; Main
;; ======================================================

(defun browser-f5--linux-search-window-ids-by-name (name-pattern)
  (let ((raw (browser-f5-call-process-to-string "xdotool" "search" "--name" name-pattern)))
    (cl-remove-if #'string-empty-p (split-string raw "\n"))))

(defun browser-f5-linux-list-window-by-name (name-pattern type)
  "Return a list contains multiple plist: ((:id WINDOW-ID :type TYPE :title TITLE) ...)
TYPE ::= chrome | chromium | firefox"
  (mapcar (lambda (id) (list
                        :id id
                        :type type
                        :title (string-trim (browser-f5-call-process-to-string "xdotool" "getwindowname" id))))
          (browser-f5--linux-search-window-ids-by-name name-pattern)))

(defun browser-f5-linux-force-select-window ()
  (interactive)
  (let* ((i 0)
         (candidates (mapcar (lambda (pl)
                               (incf i)
                               (cons (format "%02d :: %s \t:: %s" i (plist-get pl :type) (plist-get pl :title))
                                     pl))
                             (append
                              (browser-f5-linux-list-window-by-name ".Google Chrome$" 'chrome)
                              (browser-f5-linux-list-window-by-name ".Chromium$" 'chromium)
                              (browser-f5-linux-list-window-by-name ".Mozilla Firefox$" 'firefox))))
         (selected-str (ido-completing-read "Select a window:" candidates nil t))
         (selected-window (cdr (assoc selected-str candidates))))
    (message "%s" selected-window)
    (setq-local browser-f5--selected-window selected-window)
    selected-window))

;;;###autoload
(defun browser-f5 ()
  (interactive)
  (when (and browser-f5-auto-save-buffer (buffer-modified-p))
    (save-buffer))
  (if (or (null browser-f5--selected-window)
          current-prefix-arg)
      (browser-f5-linux-force-select-window))
  (let ((window-id (plist-get browser-f5--selected-window :id))
        (browser-type (plist-get browser-f5--selected-window :type)))
    (message "%s" browser-f5--selected-window)
    (case browser-type
      ((chrome chromium)
       (let ((current-active-window-id (browser-f5-call-process-to-string "xdotool" "getactivewindow")))
         (sleep-for 0.1)  ; WTF?! sleep solve shit problem again?!
         (browser-f5-call-process-to-string "xdotool"
                                            "windowactivate" "--sync" window-id
                                            "key" "--window" window-id "F5"
                                            "windowactivate" "--sync" current-active-window-id)))
      ((firefox)
       (browser-f5-call-process-to-string "xdotool" "key" "--window" window-id "F5"))
      )))

(provide 'browser-f5)

;;; browser-f5.el ends here
