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

(defvar anything-replace-string-history nil
  "Replace history.")
;;init
(setq anything-replace-string-history nil)

(defvar anything-replace-string-history-candidates nil
  "Replace history.")
;;init
(setq anything-replace-string-history-candidates nil)

(defcustom anything-replace-string-separator
  " -> "
  "Replace string pair separator"
  :type 'string
  :group 'anything-replace-string)

(defadvice replace-string (before anything-replace-string-replace-string(from-string to-string &optional delimited start end) activate)
  (anything-replace-string-push-history from-string to-string 'replace-string))

(defadvice query-replace (before anything-replace-string-query-replace(from-string to-string &optional delimited start end) activate)
  (anything-replace-string-push-history from-string to-string 'query-string))

(defun anything-replace-string-push-history (from-string to-string &optional replace-type)
  "Push replace history."
  (push (list from-string to-string replace-type) anything-replace-string-history)
  (push (concat from-string anything-replace-string-separator to-string) anything-replace-string-history-candidates))

(defvar anything-c-source-replace-string
  '((name . "Replace string from history")
    (candidates . anything-replace-string-history-candidates)
    (action
     ("Smart Replace" . anything-smart-replace-action)
     ("Replace String" . anything-replace-string-action)
     ("Query Replace" . anything-query-replace-action)
     ("Smart Replace Reverse" . anything-smart-replace-reverse-action)
     ("Replace String Reverse" . anything-replace-string-reverse-action)
     ("Query Replace Reverse" . anything-query-replace-reverse-action))
    (migemo)
    (multiline)))

(defvar anything-c-source-replace-string-dummy
  '((name . "Replace string")
    (dummy)
    (action
     ("Replace String" . anything-replace-string-dummy-action)
     ("Query Replace" . anything-query-replace-dummy-action))))

(defun anything-smart-replace-action (candidate)
  (loop with match = nil
        until match
        for x in anything-replace-string-history
        do (if (equal (concat (car x) anything-replace-string-separator (cadr x)) candidate)
               (progn
                 (cond ((equal 'replace-string (caddr x)) (anything-replace-string-region x))
                       ((equal 'query-string (caddr x)) (anything-query-replace-region x))
                       (t (anything-replace-string-region x)))
                 (setq match t)
                 (return nil)))))

(defun anything-replace-string-action (candidate)
  (message "replace")
  (loop with match = nil
        until match
        for x in anything-replace-string-history
        do (if (equal (concat (car x) anything-replace-string-separator (cadr x)) candidate)
               (progn
                 (anything-replace-string-region x)
                 (setq match t)
                 (return nil)))))

(defun anything-query-replace-action (candidate)
  (message "query")
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
    (unless (region-active-p)
      (setq prompt "Replace string "))
    (setq to-string (read-string (concat prompt candidate " with: ")))
    (anything-replace-string-push-history candidate to-string 'replace-string)
    (anything-replace-string-region (list candidate to-string 'replace-string))))

(defun anything-query-replace-dummy-action (candidate)
  (let ((to-string candidate) (prompt "Query Replace string in region "))
    (unless (region-active-p)
      (setq prompt "Query Replace string "))
    (setq to-string (read-string (concat prompt candidate " with: ")))
    (anything-replace-string-push-history candidate to-string 'query-string)
    (anything-query-replace-region (list candidate to-string 'query-string))))

(defun anything-replace-string-region (x)
  "Replace string."
  (let* ((from-string (car x))
       (to-string (cadr x))
       (count 0)
       (beginning (if mark-active  (region-beginning) (point-min)))
       (end (if mark-active  (region-end) (point-max))))
      (goto-char beginning)
      (while (search-forward from-string end t)
        (incf count)
        (replace-match to-string nil t)
        )
    (message (concat "Replaced " (number-to-string count) " occurrences"))
    (setq mark-active nil)))

(defun anything-query-replace-region (x)
  "Query Replace string."
  (let((from-string (car x))
       (to-string (cadr x)))
    (if (region-active-p)
        (perform-replace from-string to-string t nil nil nil nil (region-beginning) (region-end))
      (perform-replace from-string to-string t nil nil))))

(defun anything-replace-string()
  "Replace string from history."
  (interactive)
  (let ((prompt "Replace string in region: "))
    (unless (region-active-p)
      (setq prompt "Replace string: "))
    (anything (list anything-c-source-replace-string-dummy anything-c-source-replace-string) nil prompt nil nil)))

(provide 'anything-replace-string)
;;; anything-replace-string.el ends here
