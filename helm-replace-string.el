;;; helm-replace-string.el --- `replace-string' and `query-replace' `helm.el' interface
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
;; (require 'helm-replace-string)
;;
;; and M-x helm-replace-string

;;; Commentary:

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `helm-replace-string'
;;    Replace string from history.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `helm-replace-string-separator'
;;    Replace string pair separator
;;    default = " -> "

;;; Code:

(require 'helm)

(defgroup helm-replace-string nil
  "`replae-string' and `query-replae' `helm.el' interface"
  :group 'lisp
  :prefix "helm-replace-string-")
(defcustom helm-replace-string-separator
  " -> "
  "Replace string pair separator"
  :type 'string
  :group 'helm-replace-string)

(defvar helm-replace-string-history nil
  "Replace history.")

(defvar helm-replace-string-history-candidates nil
  "Replace history.")
;;;###autoload
(defadvice replacen-string (before helm-replace-string-replace-string(from-string to-string &optional delimited start end) activate)
   (helm-replace-string-push-history from-string to-string 'replace-string))

;;;###autoload
(defadvice query-replace (before helm-replace-string-query-replace(from-string to-string &optional delimited start end) activate)
   (helm-replace-string-push-history from-string to-string 'query-string))

;;;###autoload
(defadvice query-replace-regexp (before helm-replace-string-query-replace(from-string to-string &optional delimited start end) activate)
 (helm-replace-string-push-history from-string to-string 'query-regexp))

;;;###autoload
(defadvice replace-regexp (before helm-replace-string-query-replace(from-string to-string &optional delimited start end) activate)
 (helm-replace-string-push-history from-string to-string 'replace-regexp))

;;;###autoload
(defun helm-replace-string-push-history (from-string to-string &optional replace-type)
  "Push replace history."
  (setq helm-replace-string-history (delete  (list from-string to-string replace-type)  helm-replace-string-history))
  (setq helm-replace-string-history-candidates
        (delete (concat from-string helm-replace-string-separator to-string)
                helm-replace-string-history-candidates))
  (push (list from-string to-string replace-type) helm-replace-string-history)
  (push (concat from-string helm-replace-string-separator to-string) helm-replace-string-history-candidates))

(defvar helm-c-source-replace-string
  '((init . (lambda () "init. " (if mark-active
                                    (setq helm-replace-string-region (list t (region-beginning) (region-end)))
                                  (setq helm-replace-string-region (list nil (point-min) (point-max)))
                                  )))
    (name . "Replace string from history")
    (candidates . helm-replace-string-history-candidates)
    (action
     ("Smart Replace" . helm-smart-replace-action)
     ("Replace String" . helm-replace-string-action)
     ("Replace Regexp" . helm-replace-regexp-action)
     ("Query Replace" . helm-query-replace-action)
     ("Query Regexp" . helm-query-replace-regexp-action)
     ("Smart Replace Reverse" . helm-smart-replace-reverse-action)
     ("Replace String Reverse" . helm-replace-string-reverse-action)
     ("Query Replace Reverse" . helm-query-replace-reverse-action))
    (migemo)
    (multiline)))
(defvar helm-replace-string-region nil)
(defvar helm-c-source-replace-string-dummy
  '((init . (lambda () "init. " (if mark-active
                                    (setq helm-replace-string-region (list t (region-beginning) (region-end)))
                                  (setq helm-replace-string-region (list nil (point-min) (point-max)))
                                  )))
    (name . "Replace string")
    (dummy)
    (action
     ("Replace String" . helm-replace-string-dummy-action)
     ("Replace regexp" .   helm-replace-regexp-dummy-action )
     ("Query Replace" . helm-query-replace-dummy-action)
     ("Query regexp" .    helm-query-replace-regexp-dummy-action )
     )))


(defun helm-smart-replace-action (candidate)
  (loop with match = nil
        until match
        for x in helm-replace-string-history
        do (if (equal (concat (car x) helm-replace-string-separator (cadr x)) candidate)
               (progn
                 (cond ((equal 'replace-string (caddr x)) (helm-replace-string-region x))
                       ((equal 'query-string (caddr x)) (helm-query-replace-region x))
                       ((equal 'replace-regexp (caddr x)) (helm-replace-string-region x 'search-forward-regexp))
                        ((equal 'query-regexp (caddr x)) (helm-query-replace-region x t))
                       (t (helm-replace-string-region x)))
                 (setq match t)
                 (return nil)))))

(defun helm-replace-regexp-action (candidate)
  (message "replace regexp")
  (loop with match = nil
        until match
        for x in helm-replace-string-history
        do (if (equal (concat (car x) helm-replace-string-separator (cadr x)) candidate)
               (progn
                 (helm-replace-string-region x 'search-forward-regexp)
                 (setq match t)
                 (return nil)))))

(defun helm-replace-string-action (candidate)
  (message "replace string")
  (loop with match = nil
        until match
        for x in helm-replace-string-history
        do (if (equal (concat (car x) helm-replace-string-separator (cadr x)) candidate)
               (progn
                 (helm-replace-string-region x)
                 (setq match t)
                 (return nil)))))

(defun helm-query-replace-regexp-action (candidate)
  (message "query replace regexp")
  (loop with match = nil
        until match
        for x in helm-replace-string-history
        do (if (equal (concat (car x) helm-replace-string-separator (cadr x)) candidate)
               (progn
                 (helm-query-replace-region x t)
                 (setq match t)
                 (return nil)))))

(defun helm-query-replace-action (candidate)
  (message "query replace string ")
  (loop with match = nil
        until match
        for x in helm-replace-string-history
        do (if (equal (concat (car x) helm-replace-string-separator (cadr x)) candidate)
               (progn
                 (helm-query-replace-region x)
                 (setq match t)
                 (return nil)))))

(defun helm-smart-replace-reverse-action (candidate)
  (loop with match = nil
        until match
        for x in helm-replace-string-history
        do (if (equal (concat (car x) helm-replace-string-separator (cadr x)) candidate)
               (progn
                 (cond ((equal 'replace-string (caddr x)) (helm-replace-string-region (list (cadr x) (car x) (caddr x))))
                       ((equal 'query-string (caddr x)) (helm-query-replace-region (list (cadr x) (car x) (caddr x))))
                       (t (helm-replace-string-region x)))
                 (setq match t)
                 (return nil)))))

(defun helm-replace-string-reverse-action (candidate)
  (message "replace")
  (loop with match = nil
        until match
        for x in helm-replace-string-history
        do (if (equal (concat (car x) helm-replace-string-separator (cadr x)) candidate)
               (progn
                 (helm-replace-string-region (list (cadr x) (car x) (caddr x)))
                 (setq match t)
                 (return nil)))))

(defun helm-query-replace-reverse-action (candidate)
  (message "query")
  (loop with match = nil
        until match
        for x in helm-replace-string-history
        do (if (equal (concat (car x) helm-replace-string-separator (cadr x)) candidate)
               (progn
                 (helm-query-replace-region (list (cadr x) (car x) (caddr x)))
                 (setq match t)
                 (return nil)))))

(defun helm-replace-string-dummy-action (candidate)
  (let ((to-string candidate) (prompt "Replace string in region "))
    (unless (car helm-replace-string-region)
      (setq prompt "Replace string "))
    (setq to-string (read-string (concat prompt candidate " with: ")))
    (helm-replace-string-push-history candidate to-string 'replace-string)
    (helm-replace-string-region (list candidate to-string 'replace-string))))

(defun helm-replace-regexp-dummy-action (candidate)
  (let ((to-string candidate) (prompt "Replace regexp in region "))
    (unless (car helm-replace-string-region)
      (setq prompt "Replace regexp "))
    (setq to-string (read-regexp (concat prompt candidate " with: ")))
    (helm-replace-string-push-history candidate to-string 'replace-regexp)
    (helm-replace-string-region (list candidate to-string 'replace-string) 'search-forward-regexp )))

(defun helm-query-replace-dummy-action (candidate)
  (let ((to-string candidate) (prompt "Query Replace string in region "))
    (unless (region-active-p)
      (setq prompt "Query Replace string "))
    (setq to-string (read-string (concat prompt candidate " with: ")))
    (helm-replace-string-push-history candidate to-string 'query-string)
    (helm-query-replace-region (list candidate to-string 'query-string) )))

(defun helm-query-replace-regexp-dummy-action (candidate)
  (let ((to-string candidate) (prompt "Query Replace regexp in region "))
    (unless (region-active-p)
      (setq prompt "Query Replace regexp "))
    (setq to-string (read-string (concat prompt candidate " with: ")))
    (helm-replace-string-push-history candidate to-string 'query-regexp)
    (helm-query-replace-region (list candidate to-string 'query-string) t )))

(defun helm-replace-string-region (x &optional search-fun)
  "Replace string."
  (let* ((from-string (car x))
         (to-string (cadr x))
         (count 0)
         (beginning (nth 1 helm-replace-string-region))
         (end  (nth 2 helm-replace-string-region)))
    (unless search-fun
      (setq search-fun 'search-forward))
    (goto-char beginning)
    (while (funcall search-fun from-string end t)
      (incf count)
      (replace-match to-string nil t)
      )
    (message (concat "Replaced " (number-to-string count) " occurrences"))
    (setq mark-active nil)))

(defun helm-query-replace-region (x &optional regexp-flag)
  "Query Replace string."
  (let((from-string (car x))
       (to-string (cadr x))
       (beginning (nth 1 helm-replace-string-region))
       (end  (nth 2 helm-replace-string-region))
       )
    (if (car helm-replace-string-region)
        (perform-replace from-string to-string t regexp-flag nil nil nil beginning end)
      (perform-replace from-string to-string t regexp-flag nil))))

;;;###autoload
(defun helm-replace-string()
  "Replace string from history."
  (interactive)
  (let ((prompt "Replace string in region: ")
        (helm-samewindow nil)
        (init-input ""))
    (unless  mark-active
      (setq prompt "Replace string: ")
      )
    (when (and mark-active (< (- (region-end) (region-beginning)) 50))
      (setq prompt "Replace string: ")
      (setq init-input  (buffer-substring-no-properties (region-beginning)  (region-end) ))
      (setq mark-active nil)
      )
    (helm (list helm-c-source-replace-string-dummy helm-c-source-replace-string)
              init-input
              prompt nil nil)))

(provide 'helm-replace-string)
;;; helm-replace-string.el ends here
