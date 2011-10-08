;;; org-email.el --- use org for an email database -*- lexical-binding: t -*-

;; Copyright (C) 2009-2011 Nic Ferrier

;; Parses emails out of org mode files in a very simple way

;;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;;; Maintainer: Nic Ferrier <nferrier@ferrier.me.uk>
;;; Created: 7th October 2011
;;; Version: 0.01
;;; Keywords: lisp

;; This file is NOT part of GNU Emacs.

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
;;
;; This is a simple org-mode add on that allows the use of org mode as
;; an email database.
;;
;; Store emails in org structure like this:
;;
;; * Nic Ferrier
;; ** email
;; *** nferrier@gnu.org

;;; Source code
;;
;; org-email can be found here
;;   http://github.com/nicferrier/org-email

;;; Style note
;;
;; This codes uses the Emacs style of:
;;
;;    org-email--private-function
;;
;; for private functions and for private variables.


;;; Code:

(defgroup org-email nil
  "Options concerning email handling in Org-mode."
  :tag "Org Email"
  :group 'org)

(defcustom org-email-files nil
  "The files that org-email will check for email addresses."
  :group 'org-email
  :type '(choice
	  (repeat :tag "List of files and directories" file)
	  (file :tag "Store list in a file\n" :value "~/.agenda_files")))

(defcustom org-email-add-completion-hook-mode 'message-mode
  "The mode to add a completion keybinding hook to, if at all.

If you want an email completion function for org-email to be
added to your mode."
  :group 'org-email
  :type 'symbol)


(defun org-email--init-hook ()
  "A hook function to map a key to expansion."
  (local-set-key "\C-c " 'org-email-do-insert)
  )


;; Automatically add this hook.  
;;
;; This probably is not the right way to do this... can we
;; auto-configure the hook variable at compile time??
(if org-email-add-completion-hook-mode
    (add-hook 'message-mode-hook 'org-email--init-hook))


(defun org-email--buffer-emails (buffer)
  "Return all the emails in an org BUFFER.

The emails should be indicated in an org structure."
  (let ((res '()))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^\\*\\* email" nil 't)
          (if (save-excursion
                (forward-line)
                (looking-at "^\\(\\*\\*\\* \\)*\\([a-zA-Z0-9_.+-]+@[a-zA-Z0-9_.+-]+\\)"))
              (let ((email (match-string-no-properties 2)))
                (save-excursion
                  (forward-line -1)
                  (if (looking-at "^\\(\\* \\)*\\(.*\\)")
                      (setq res (cons
                                 (cons (match-string-no-properties 2) email)
                                 res)))
                  )))))
      res)))

(defun org-email--all-buffer-emails ()
  "Get emails from *all* the ORG-EMAIL-FILES.

Returns the emails as a list.

This has to read each file so it would be better to cache this
value and check modification times and stuff like that."
  (apply 'nconc
         (mapcar (lambda (file-name)
                   (let ((buf (find-file-noselect file-name)))
                     (org-email--buffer-emails buf)))
                 (apply 'nconc (list org-email-files)))))

(defun org-email-insert (name-or-email &optional buffer at)
  "Insert the specified NAME-OR-EMAIL in the BUFFER.

The NAME-OR-EMAIL is looked up in the ORG-EMAIL-FILES and
inserted in the BUFFER at the point marked by AT.

All these have sensible defaults obtained by completion and the
current buffer and point."
  (interactive (list
                (completing-read
                 "name or email: "
                 (org-email--all-buffer-emails))
                (current-buffer)
                (point)))
  (let* ((emails (org-email--all-buffer-emails))
         (email (assoc name-or-email emails)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char at)
        (insert (format "\"%s\" <%s>" (car email) (cdr email)))))))

(defun org-email-do-insert ()
  "Interactive completion intended to be bound to a keypress."
  (interactive)
  (let* ((thing (bounds-of-thing-at-point 'word))
         (emails (org-email--all-buffer-emails))
         (email (assoc 
                 (try-completion 
                  (buffer-substring-no-properties (car thing) (cdr thing))
                  emails)
                 emails)))
    (delete-region (car thing) (cdr thing))
    (insert (format "\"%s\" <%s>" (car email) (cdr email)))))

;;; org-email.el ends here
