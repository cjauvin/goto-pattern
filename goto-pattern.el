;; goto-pattern.el --- An easy way to navigate to a predefined pattern
;;                     in a file
;;   
;; Author: Christian Jauvin <cjauvin@gmail.com>
;; Created: 2008-12-09
;; Last updated: 2011-01-26
;;
;; The primary goal of this extension was to create a very simple
;; navigation system for the function/class definitions of different
;; types of source file (matched with regexps that can vary depending
;; on your style). Of course it can be used with other types of
;; pattern as well. A given pattern must be mapped to a file
;; extension, and it's a one-to-one relationship: to one type of file
;; can only be associated one pattern (the trick is of course to use
;; regexp alternation if you have a need for it).
;;
;; By default, goto-pattern knows about a couple of "source file
;; extension -> function/class definition pattern" mappings (for
;; Python, PHP, JS, *Lisp and SQL). You can add a new one easily by
;; using the interactive function:
;;
;;   M-x g2p-add-pattern
;;
;; If a pattern for the file extension you specified already exists,
;; it will get replaced.
;;
;; This extension can be installed by adding it first to your
;; load-path (in your .emacs):
;;
;;   (add-to-list 'load-path "/path/to/")
;;
;; then requiring it:
;;
;;   (require 'goto-pattern)
;;
;; and finally maybe mapping its entry point function to a handy
;; keyboard shortcut, for instance:
;;
;;   (global-set-key "\C-cgp" 'goto-pattern) ; CTRL-c, + g + p
;;

(setq g2p-extension-to-pattern
   '(("py" . "^ *def .*\\|^ *class .*")
     ("php" . "^ *function .*\\|^ *class .*")
     ("js" . "^ *function .*\\|^.*= function.*")
     ("el" . "^ *(defun .*")
     ("lisp" . "^ *(defun .*")
     ("sql" . "^ *create table.*\\|^ *CREATE TABLE.*")))

(defun g2p-get-curr-buffer-extension ()
  (if (string-match ".*[.]\\(.*?\\)\\(<[0-9]+>\\)?$" (buffer-name))
      (match-string 1 (buffer-name))
    nil))

(defun g2p-clear-match-buffer (buf)
  (save-excursion
    (set-buffer buf)
    (kill-region (point-min) (point-max))))

(defun g2p-get-line-number (s) 
  (string-to-number (substring s (string-match "[0-9]+" s))))
  
(defun g2p-return ()
  (interactive)
  (let ((retline (g2p-get-line-number (what-line))))
    (switch-to-buffer g2p-calling-buffer)
    (goto-line (g2p-get-line-number (nth 0 (nth (- retline 1) (reverse g2p-lines)))))
    (recenter)))

(define-derived-mode g2p-mode nil "g2p mode"
  (define-key g2p-mode-map [mouse-1] 'g2p-return)
  (define-key g2p-mode-map (kbd "<RET>") 'g2p-return)
  (define-key g2p-mode-map (kbd "<ESC>") '(lambda () 
                                            (interactive) 
                                            (switch-to-buffer g2p-calling-buffer)))
  (hl-line-mode))

; entry point
(defun goto-pattern ()
  (interactive)
  (setq g2p-calling-buffer (current-buffer))
  (let ((pattern (cdr (assoc (g2p-get-curr-buffer-extension) g2p-extension-to-pattern))))
    (if pattern
        (let ((matches (g2p-find-matches pattern)))
          (if matches
              (progn 
                (switch-to-buffer (get-buffer-create (concat (buffer-name (current-buffer)) " pattern matches")) t)
                (g2p-clear-match-buffer (current-buffer))
                (g2p-print-matches matches)
                (goto-line 1)
                (g2p-mode))
            (message "No match")))
      (message "I have no pattern for this buffer type... (you can add one with M-x g2p-add-pattern)"))))

(defun g2p-find-matches (pattern)
  (save-excursion
    (beginning-of-buffer)
    (setq g2p-lines ())
    (progn 
      (while (re-search-forward pattern nil t)
        (push (list (what-line) (match-string 0)) g2p-lines))
      (reverse g2p-lines))))

(defun g2p-print-matches (matches)
  (interactive)
  (dolist (line matches)
    (insert (nth 0 line))
    (insert ": ")
    (insert (nth 1 line))
    (add-text-properties
     (line-beginning-position) (line-end-position)
     '(mouse-face 'highlight))
    (insert "\n")))

(defun g2p-add-pattern (&optional extension pattern)
  (interactive)
  (if (null extension)      
      (setq extension (read-string "Buffer/file extension (without the dot): " (g2p-get-curr-buffer-extension))))
  (if (null pattern)      
      (setq pattern (read-string "Pattern (regexp): "))) 
  (if (assoc extension g2p-extension-to-pattern)
      (setf (cdr (assoc extension g2p-extension-to-pattern)) pattern)
    (add-to-list 'g2p-extension-to-pattern (cons extension pattern))))
                     
(provide 'goto-pattern)
