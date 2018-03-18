;;; wb-refresh.el --- Refresh web browser in Emacs


;;; Commentary:

;;; Code:

(require 'cl-lib)

(defgroup wb-refresh nil
  "Browser refresh utility for multi browsers and multi platform"
  :group 'external)

(defcustom wb-refresh-save-buffer t
  "Non-nil means saving buffer before browser refresh"
  :type 'boolean
  :group 'wb-refresh)

(defvar-local wb-refresh--selected-window nil
  "For internal use.
Store the window id of currently selected instance.")

;;
;; Tool
;;
(defun wb-refresh-call-process-to-string (program &rest args)
  "`shell-command-to-string' is too slow for simple task, so use this."
  (message (string-join (cons program args) " "))
  (with-temp-buffer
    (apply #'call-process program (append '(nil t nil) args))
    (buffer-string)))
;;
;; GNU/Linux
;;

(defun wb-refresh--send-key-with-xdotool (window-id key)
  (message "window-id %s, %s" window-id key)
  (unless (zerop (wb-refresh-call-process-to-string "xdotool" "key" "--window" window-id key))
    (error "Failed: 'xdotool key --window %s %s'" window-id key)))

(defun wb-refresh--linux-search-window-ids-by-class (class)
  (with-temp-buffer
    (unless (zerop (call-process "xdotool" nil t nil "search" "--class" class))
      (error "Failed: 'xdotool search --class %s'" class))
    (goto-char (point-min))
    (cl-loop with window-ids = nil
             until (eobp)
             do
             (progn
               (push (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))
                     window-ids)
               (forward-line 1))
             finally return window-ids)))

(defun wb-refresh--linux-search-window-ids-by-name (name-pattern)
  (let ((raw (wb-refresh-call-process-to-string "xdotool" "search" "--name" name-pattern)))
    (cl-remove-if #'string-empty-p (split-string raw "\n"))))

(defun wb-refresh-linux-list-window-by-name (name-pattern browser-symbol)
  "Return a list: ((WINDOW-ID . WINDOW-NAME) ...)"
  (mapcar (lambda (id) (list
                        :id id
                        :type browser-symbol
                        :title (string-trim (wb-refresh-call-process-to-string "xdotool" "getwindowname" id))))
          (wb-refresh--linux-search-window-ids-by-name name-pattern)))

(defun wb-refresh-linux-force-select-window ()
  (interactive)
  (let* ((i 0)
         (candidates (mapcar (lambda (pl)
                               (incf i)
                               (cons (format "%02d :: %s \t:: %s" i (plist-get pl :type) (plist-get pl :title))
                                     pl))
                             (append
                              (wb-refresh-linux-list-window-by-name ".Google Chrome$" 'chrome)
                              (wb-refresh-linux-list-window-by-name ".Chromium$" 'chromium)
                              (wb-refresh-linux-list-window-by-name ".Mozilla Firefox$" 'firefox))))
         (selected-str (ido-completing-read "Select a window:" candidates nil t))
         (selected-window (cdr (assoc selected-str candidates))))
    (message "%s" selected-window)
    (setq-local wb-refresh--selected-window selected-window)
    selected-window))

;;;###autoload
(defun wb-refresh ()
  (interactive)
  (when (and wb-refresh-save-buffer (buffer-modified-p))
    (save-buffer))
  (if (or (null wb-refresh--selected-window)
          current-prefix-arg)
      (wb-refresh-linux-force-select-window))
  (let ((window-id (plist-get wb-refresh--selected-window :id))
        (browser-type (plist-get wb-refresh--selected-window :type)))
    (message "%s" wb-refresh--selected-window)
    (case browser-type
      ((chrome chromium)
       (let ((current-active-window-id (wb-refresh-call-process-to-string "xdotool" "getactivewindow")))
         (sleep-for 0.1)  ; WTF?! sleep solve shit problem again?!
         (wb-refresh-call-process-to-string "xdotool"
                                            "windowactivate" "--sync" window-id
                                            "key" "--window" window-id "F5"
                                            "windowactivate" "--sync" current-active-window-id)))
      ((firefox)
       (wb-refresh-call-process-to-string "xdotool" "key" "--window" window-id "F5"))
      )))

(provide 'wb-refresh)

;;; wb-refresh.el ends here
