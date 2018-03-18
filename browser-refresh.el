;;; browser-refresh.el --- Broser refresh utility

;; Copyright (C) 2014 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-browser-refresh
;; Version: 0.01
;; Package-Requires: ((eieio "1.3") (cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cl-lib)

(defgroup browser-refresh nil
  "Browser refresh utility for multi browsers and multi platform"
  :group 'external)

(defcustom browser-refresh-save-buffer t
  "Non-nil means saving buffer before browser refresh"
  :type 'boolean
  :group 'browser-refresh)

(defvar-local browser-refresh--selected-window nil
  "For internal use.
Store the window id of currently selected instance.")

;;
;; Tool
;;
(defun browser-refresh-call-process-to-string (program &rest args)
  "`shell-command-to-string' is too slow for simple task, so use this."
  (message (string-join (cons program args) " "))
  (with-temp-buffer
    (apply #'call-process program (append '(nil t nil) args))
    (buffer-string)))
;;
;; GNU/Linux
;;

(defun browser-refresh--send-key-with-xdotool (window-id key)
  (message "window-id %s, %s" window-id key)
  (unless (zerop (browser-refresh-call-process-to-string "xdotool" "key" "--window" window-id key))
    (error "Failed: 'xdotool key --window %s %s'" window-id key)))

(defun browser-refresh--linux-search-window-ids-by-class (class)
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

(defun browser-refresh--linux-search-window-ids-by-name (name-pattern)
  (let ((raw (browser-refresh-call-process-to-string "xdotool" "search" "--name" name-pattern)))
    (cl-remove-if #'string-empty-p (split-string raw "\n"))))

(defun browser-refresh-linux-list-window-by-name (name-pattern browser-symbol)
  "Return a list: ((WINDOW-ID . WINDOW-NAME) ...)"
  (mapcar (lambda (id) (list
                        :id id
                        :type browser-symbol
                        :title (string-trim (browser-refresh-call-process-to-string "xdotool" "getwindowname" id))))
          (browser-refresh--linux-search-window-ids-by-name name-pattern)))

;; (defun activate ((refresher browser-refresh-linux) window-id)
;;   (when (oref refresher :activate)
;;     (unless (zerop (call-process "xdotool" nil nil nil "windowactivate" window-id))
;;       (error "Failed: 'xdotool windowactivate %s'" window-id))))

(defun browser-refresh-linux-force-select-window ()
  (interactive)
  (let* ((i 0)
         (candidates (mapcar (lambda (pl)
                               (incf i)
                               (cons (format "%02d :: %s" i (plist-get pl :title))
                                     pl))
                             (append
                              (browser-refresh-linux-list-window-by-name "Google Chrome$" 'chrome)
                              (browser-refresh-linux-list-window-by-name "Mozilla Firefox$" 'firefox))))
         (selected-str (ido-completing-read "Select a window:" candidates nil t))
         (selected-window (cdr (assoc selected-str candidates))))
    (setq-local browser-refresh--selected-window selected-window)
    selected-window))

;;;###autoload
(defun browser-refresh ()
  (interactive)
  (when (and browser-refresh-save-buffer (buffer-modified-p))
    (save-buffer))
  (if (or (null browser-refresh--selected-window)
          current-prefix-arg)
      (browser-refresh-linux-force-select-window))
  (let ((window-id (plist-get browser-refresh--selected-window :id))
        (browser-type (plist-get browser-refresh--selected-window :type)))
    (case browser-type
      ((chrome)
       (let ((current-active-window-id (browser-refresh-call-process-to-string "xdotool" "getactivewindow")))
         (sleep-for 0.1)  ; WTF?! sleep solve shit problem again?!
         (browser-refresh-call-process-to-string "xdotool"
                                                 "windowactivate" "--sync" window-id
                                                 "key" "--window" window-id "F5"
                                                 "windowactivate" "--sync" current-active-window-id)))
      ((firefox)
       (browser-refresh-call-process-to-string "xdotool" "key" "--window" window-id "F5"))
      )))

(provide 'browser-refresh)

;;; browser-refresh.el ends here
