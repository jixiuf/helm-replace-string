;;; anything-replace-string.el --- `replace-string' and `query-replace' `anything.el' interface
;; -*- Mode: Emacs-Lisp -*-

;; Copyright (C) 2011 by 101000code/101000LAB

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA

;; Version: 0.8.3
;; Author: k1LoW (Kenichirou Oyama), <k1lowxb [at] gmail [dot] com> <k1low [at] 101000lab [dot] org>
;; Maintainer: k1LoW (Kenichirou Oyama), <k1lowxb [at] gmail [dot] com> <k1low [at] 101000lab [dot] org>
;;             kitokitoki, <mori.dev.asdf [at] gmail [dot] com>
;; URL: http://code.101000lab.org

;;; Install
;; Put this file into load-path'ed directory, and byte compile it if
;; desired.  And put the following expression into your ~/.emacs.
;;
;; (require 'anything-replace-string)
;;
;; and M-x anything-replace-string

;;; Commentary:

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `anything-replace-string'
;;    Replace string from history.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `anything-replace-string-separator'
;;    Replace string pair separator
;;    default = " -> "

;;; Code:

(require 'anything)

(defgroup anything-replace-string nil
  "`replae-string' and `query-replae' `anything.el' interface"
  :group 'lisp
  :prefix "anything-replace-string-")
(defcustom anything-replace-string-separator
  " -> "
  "Replace string pair separator"
  :type 'string
  :group 'anything-replace-string)

(defvar anything-replace-string-history nil
  "Replace history.")

(defvar anything-replace-string-history-candidates nil
  "Replace history.")
;;;###autoload
(defadvice replacen-string (before anything-replace-string-replace-string(from-string to-string &optional delimited start end) activate)
   (anything-replace-string-push-history from-string to-string 'replace-string))

;;;###autoload
(defadvice query-replace (before anything-replace-string-query-replace(from-string to-string &optional delimited start end) activate)
   (anything-replace-string-push-history from-string to-string 'query-string))

;;;###autoload
(defadvice query-replace-regexp (before anything-replace-string-query-replace(from-string to-string &optional delimited start end) activate)
 (anything-replace-string-push-history from-string to-string 'query-regexp))

;;;###autoload
(defadvice replace-regexp (before anything-replace-string-query-replace(from-string to-string &optional delimited start end) activate)
 (anything-replace-string-push-history from-string to-string 'replace-regexp))

;;;###autoload
(defun anything-replace-string-push-history (from-string to-string &optional replace-type)
  "Push replace history."
  (setq anything-replace-string-history (delete  (list from-string to-string replace-type)  anything-replace-string-history))
  (setq anything-replace-string-history-candidates
        (delete (concat from-string anything-replace-string-separator to-string)
                anything-replace-string-history-candidates))
  (push (list from-string to-string replace-type) anything-replace-string-history)
  (push (concat from-string anything-replace-string-separator to-string) anything-replace-string-history-candidates))

(defvar anything-c-source-replace-string
  '((init . (lambda () "init. " (if mark-active
                                    (setq anything-replace-string-region (list t (region-beginning) (region-end)))
                                  (setq anything-replace-string-region (list nil (point-min) (point-max)))
                                  )))
    (name . "Replace string from history")
    (candidates . anything-replace-string-history-candidates)
    (action
     ("Smart Replace" . anything-smart-replace-action)
     ("Replace String" . anything-replace-string-action)
     ("Replace Regexp" . anything-replace-regexp-action)
     ("Query Replace" . anything-query-replace-action)
     ("Query Regexp" . anything-query-replace-regexp-action)
     ("Smart Replace Reverse" . anything-smart-replace-reverse-action)
     ("Replace String Reverse" . anything-replace-string-reverse-action)
     ("Query Replace Reverse" . anything-query-replace-reverse-action))
    (migemo)
    (multiline)))
(defvar anything-replace-string-region nil)
(defvar anything-c-source-replace-string-dummy
  '((init . (lambda () "init. " (if mark-active
                                    (setq anything-replace-string-region (list t (region-beginning) (region-end)))
                                  (setq anything-replace-string-region (list nil (point-min) (point-max)))
                                  )))
    (name . "Replace string")
    (dummy)
    (action
     ("Replace String" . anything-replace-string-dummy-action)
     ("Replace regexp" .   anything-replace-regexp-dummy-action )
     ("Query Replace" . anything-query-replace-dummy-action)
     ("Query regexp" .    anything-query-replace-regexp-dummy-action )
     )))


(defun anything-smart-replace-action (candidate)
  (loop with match = nil
        until match
        for x in anything-replace-string-history
        do (if (equal (concat (car x) anything-replace-string-separator (cadr x)) candidate)
               (progn
                 (cond ((equal 'replace-string (caddr x)) (anything-replace-string-region x))
                       ((equal 'query-string (caddr x)) (anything-query-replace-region x))
                       ((equal 'replace-regexp (caddr x)) (anything-replace-string-region x 'search-forward-regexp))
                        ((equal 'query-regexp (caddr x)) (anything-query-replace-region x t))
                       (t (anything-replace-string-region x)))
                 (setq match t)
                 (return nil)))))

(defun anything-replace-regexp-action (candidate)
  (message "replace regexp")
  (loop with match = nil
        until match
        for x in anything-replace-string-history
        do (if (equal (concat (car x) anything-replace-string-separator (cadr x)) candidate)
               (progn
                 (anything-replace-string-region x 'search-forward-regexp)
                 (setq match t)
                 (return nil)))))

(defun anything-replace-string-action (candidate)
  (message "replace string")
  (loop with match = nil
        until match
        for x in anything-replace-string-history
        do (if (equal (concat (car x) anything-replace-string-separator (cadr x)) candidate)
               (progn
                 (anything-replace-string-region x)
                 (setq match t)
                 (return nil)))))

(defun anything-query-replace-regexp-action (candidate)
  (message "query replace regexp")
  (loop with match = nil
        until match
        for x in anything-replace-string-history
        do (if (equal (concat (car x) anything-replace-string-separator (cadr x)) candidate)
               (progn
                 (anything-query-replace-region x t)
                 (setq match t)
                 (return nil)))))

(defun anything-query-replace-action (candidate)
  (message "query replace string ")
  (loop with match = nil
        until match
        for x in anything-replace-string-history
        do (if (equal (concat (car x) anything-replace-string-separator (cadr x)) candidate)
               (progn
                 (anything-query-replace-region x)
                 (setq match t)
                 (return nil)))))

(defun anything-smart-replace-reverse-action (candidate)
  (loop with match = nil
        until match
        for x in anything-replace-string-history
        do (if (equal (concat (car x) anything-replace-string-separator (cadr x)) candidate)
               (progn
                 (cond ((equal 'replace-string (caddr x)) (anything-replace-string-region (list (cadr x) (car x) (caddr x))))
                       ((equal 'query-string (caddr x)) (anything-query-replace-region (list (cadr x) (car x) (caddr x))))
                       (t (anything-replace-string-region x)))
                 (setq match t)
                 (return nil)))))

(defun anything-replace-string-reverse-action (candidate)
  (message "replace")
  (loop with match = nil
        until match
        for x in anything-replace-string-history
        do (if (equal (concat (car x) anything-replace-string-separator (cadr x)) candidate)
               (progn
                 (anything-replace-string-region (list (cadr x) (car x) (caddr x)))
                 (setq match t)
                 (return nil)))))

(defun anything-query-replace-reverse-action (candidate)
  (message "query")
  (loop with match = nil
        until match
        for x in anything-replace-string-history
        do (if (equal (concat (car x) anything-replace-string-separator (cadr x)) candidate)
               (progn
                 (anything-query-replace-region (list (cadr x) (car x) (caddr x)))
                 (setq match t)
                 (return nil)))))

(defun anything-replace-string-dummy-action (candidate)
  (let ((to-string candidate) (prompt "Replace string in region "))
    (unless (car anything-replace-string-region)
      (setq prompt "Replace string "))
    (setq to-string (read-string (concat prompt candidate " with: ")))
    (anything-replace-string-push-history candidate to-string 'replace-string)
    (anything-replace-string-region (list candidate to-string 'replace-string))))

(defun anything-replace-regexp-dummy-action (candidate)
  (let ((to-string candidate) (prompt "Replace regexp in region "))
    (unless (car anything-replace-string-region)
      (setq prompt "Replace regexp "))
    (setq to-string (read-regexp (concat prompt candidate " with: ")))
    (anything-replace-string-push-history candidate to-string 'replace-regexp)
    (anything-replace-string-region (list candidate to-string 'replace-string) 'search-forward-regexp )))

(defun anything-query-replace-dummy-action (candidate)
  (let ((to-string candidate) (prompt "Query Replace string in region "))
    (unless (region-active-p)
      (setq prompt "Query Replace string "))
    (setq to-string (read-string (concat prompt candidate " with: ")))
    (anything-replace-string-push-history candidate to-string 'query-string)
    (anything-query-replace-region (list candidate to-string 'query-string) )))

(defun anything-query-replace-regexp-dummy-action (candidate)
  (let ((to-string candidate) (prompt "Query Replace regexp in region "))
    (unless (region-active-p)
      (setq prompt "Query Replace regexp "))
    (setq to-string (read-string (concat prompt candidate " with: ")))
    (anything-replace-string-push-history candidate to-string 'query-regexp)
    (anything-query-replace-region (list candidate to-string 'query-string) t )))

(defun anything-replace-string-region (x &optional search-fun)
  "Replace string."
  (let* ((from-string (car x))
         (to-string (cadr x))
         (count 0)
         (beginning (nth 1 anything-replace-string-region))
         (end  (nth 2 anything-replace-string-region)))
    (unless search-fun
      (setq search-fun 'search-forward))
    (goto-char beginning)
    (while (funcall search-fun from-string end t)
      (incf count)
      (replace-match to-string nil t)
      )
    (message (concat "Replaced " (number-to-string count) " occurrences"))
    (setq mark-active nil)))

(defun anything-query-replace-region (x &optional regexp-flag)
  "Query Replace string."
  (let((from-string (car x))
       (to-string (cadr x))
       (beginning (nth 1 anything-replace-string-region))
       (end  (nth 2 anything-replace-string-region))
       )
    (if (car anything-replace-string-region)
        (perform-replace from-string to-string t regexp-flag nil nil nil beginning end)
      (perform-replace from-string to-string t regexp-flag nil))))

;;;###autoload
(defun anything-replace-string()
  "Replace string from history."
  (interactive)
  (let ((prompt "Replace string in region: "))
    (unless (region-active-p)
      (setq prompt "Replace string: "))
    (anything (list anything-c-source-replace-string-dummy anything-c-source-replace-string) nil prompt nil nil)))

(provide 'anything-replace-string)
;;; anything-replace-string.el ends here
