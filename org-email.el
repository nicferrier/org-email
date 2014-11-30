;;; org-email.el --- use org for an email database -*- lexical-binding: t -*-

;; Copyright (C) 2009-2011 Nic Ferrier

;; Parses emails out of org mode files in a very simple way

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Maintainer: Nic Ferrier <nferrier@ferrier.me.uk>
;; Created: 7th October 2011
;; Package-requires: ((shadchen "1.2")(dash "2.9.0")(noflet "0.0.12"))
;; Version: 3.0.1
;; Url: https://github.com/nicferrier/org-email
;; Keywords: lisp

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

;;; Source code
;;
;; org-email can be found here
;;   http://github.com/nicferrier/org-email

;;; Style note
;;
;; This codes uses the Emacs style of:
;;
;;    org-email/private-function
;;
;; for private functions and for private variables.

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
;;
;; or use the `email' tag:
;;
;; * Nic Ferrier
;; ** nferrier@gnu.org                      :email:
;;
;; This code searches through both when making completions.

;;; Code:

(require 'cl-lib) ; we use labels
(require 'org-element)
(require 'dash)
(require 'shadchen) 

(defgroup org-email nil
  "Options concerning email handling in Org-mode."
  :tag "Org Email"
  :group 'org)

(defcustom org-email-files nil
  "The files that org-email will check for email addresses."
  :group 'org-email
  :type '(choice
	  (repeat :tag "List of files and directories" file)
	  (file
           :tag "Store list in a file\n"
           :value "~/.agenda_files")))

(defcustom org-email-add-completion-hook-mode 'message-mode
  "The mode to add a completion keybinding hook to, if at all.

If you want an email completion function for org-email to be
added to your mode."
  :group 'org-email
  :type 'symbol)


(defun org-el/tree-map (org-elements)
  "Map over ORG-ELEMENTS and produce a more compact form."
  (cl-labels
      ((map-elements (nodes)
         (mapcar
          (lambda (element)
            (match
             element
             ((list 'section props (list 'paragraph props text))
              (list (substring-no-properties text 0 (1- (length text))) nil))
             ((list 'section props (list 'comment _))
              nil)
             ((list 'headline (plist :raw-value title :tags tags) (tail nodes))
              (apply 'list title tags (map-elements nodes)))
             ((list 'headline (list :raw-value data (plist :tags tags)))
              (list data tags))))
          nodes)))
    (map-elements (cddr org-elements))))

(defun org-el/struct-map (org-file)
  "Proxy taking ORG-FILE for `org-el/tree-map'."
  (org-el/tree-map
   (with-current-buffer (find-file-noselect org-file)
     (org-element-parse-buffer))))

(defmacro with-escape (escape &rest body)
  "Evaluate BODY, return the value passed to ESCAPE.

This is just like `catch'/`throw' but the returned value of the
BODY is discarded unless there is a non-local exit through
ESCAPE."
  (declare
   (debug (sexp &rest form))
   (indent 1))
  (let ((tag (make-symbol "escapetag"))
        (escape-type (make-symbol "escapetype"))
        (catch-value (make-symbol "catchvalue")))
    `(noflet ((,escape (value)
                (throw (quote ,tag) (cons (quote ,escape-type) value))))
       (let ((,catch-value 
              (catch (quote ,tag)
                ,@body)))
         (and (consp ,catch-value)
              (eq (quote ,escape-type) (car-safe ,catch-value))
              (cdr-safe ,catch-value))))))

;; (with-escape get-out
;;   (dolist (v (number-sequence 1 10))
;;     (when (equal v 9) (get-out v))))

(defun org-email-list (org-file)
  "Make an alist of names and emails from ORG-FILE."
  (cl-labels
      ((tag-search (nodes tag)
           (cl-labels ((tag-test (lst) (member tag lst)))
             (with-escape escape
               (--map
                (match
                 it
                 ((list text (? #'tag-test) (tail _)) (escape text))
                 ((list text (? #'tag-test)) (escape text))
                 ((list text tags (tail nodes)) (tag-search nodes tag)))
                nodes))))
       (branch-search (nodes tag)
           (cl-labels ((tag-test (lst) (member tag lst)))
             (with-escape escape
               (--map
                (match
                 it
                 ((list text (? #'tag-test) (tail _)) (escape text))
                 ((list text (? #'tag-test)) (escape text))
                 ((list text tags (tail nodes)) (tag-search nodes tag)))
                nodes)))))
    (let* ((tree (org-el/struct-map org-file)))
      (-filter (lambda (kv) (stringp (cdr kv)))
               (--map
                (when it
                  (match
                   it
                   ((list text _ (tail nodes))
                    (cons
                     text
                     (or
                      (branch-search nodes "email")
                      (tag-search nodes "email"))))))
                tree)))))


;;;###autoload
(defun org-email/init-hook ()
  "A hook function to map a key to expansion."
  (local-set-key "\C-c " 'org-email-do-insert))

;; Automatically add this hook.  
;;
;; This probably is not the right way to do this... can we
;; auto-configure the hook variable at compile time??
(when org-email-add-completion-hook-mode
  (add-hook 'message-mode-hook 'org-email/init-hook))


(defun org-email/all-buffer-emails ()
  "Get emails from *all* the ORG-EMAIL-FILES.

Returns the emails as a list.

This has to read each file so it would be better to cache this
value and check modification times and stuff like that."
  (->> org-email-files
    (--map (org-email-list it))
    (-flatten)
    ;; Expand the list into one like:
    ;;    (name (email . name))
    ;;    (email (email .name))
    (--map
     (match
      it
      ((cons name email)
       (list (list (downcase name) (cons email name))
             (list (downcase email) (cons email name))))))
    (-flatten)
    (-partition 2)))

(defun org-email/insert (email buffer pt)
  "Insert EMAIL into BUFFER at PT."
  (with-current-buffer buffer
    (save-excursion
      (goto-char pt)
      (let ((addr (car email))
            (name (cdr email)))
        (if (equal addr name)
            (insert (format "<%s>" addr))
            ;; Else
            (insert (format "\"%s\" <%s>" name addr)))))))

(defun org-email-insert (name-or-email &optional buffer at)
  "Insert the specified NAME-OR-EMAIL in the BUFFER.

The NAME-OR-EMAIL is looked up in the ORG-EMAIL-FILES and
inserted in the BUFFER at the point marked by AT.

All these have sensible defaults obtained by completion and the
current buffer and point."
  (interactive (list
                (let ((completion-ignore-case 't)
                      (completions
                       (org-email/all-buffer-emails)))
                  (car 
                   (kva
                    (completing-read "name or email: " completions)
                    completions)))
                (current-buffer)
                (point)))
  (org-email/insert
   ;; Avoid the completion if we've done it interactively
   (if (consp name-or-email)
       name-or-email
       ;; Else do the completion
       (let* ((emails (org-email/all-buffer-emails)))
         (kva name-or-email emails)))
   buffer
   at))

;;;###autoload
(defun org-email-do-insert ()
  "Interactive completion intended to be bound to a keypress."
  (interactive)
  (let* ((thing
          ;; Find thing before point
          ;; Possibly multiple words
          ;; But with no leading space
          (save-excursion
            (save-match-data
              (looking-back "\\([a-zA-Z -]+\\)" (line-beginning-position) t) 
              (let* ((d (match-data))
                     (m (cons (car d) (cadr d))))
                (goto-char (car m))
                (re-search-forward "[^ ]" (cdr m) 't)
                (cons (car (match-data)) (cdr m))))))
         (thingstr (buffer-substring-no-properties (car thing) (cdr thing)))
         (emails (org-email/all-buffer-emails))
         (completed-email (and
                           (let ((completion-ignore-case 't))
                             (try-completion thingstr emails))
                           thingstr))
         (email (car (kva completed-email emails))))
    (if (not email)
        ;; This displays the full completion list in a window that we can later kill
        (with-current-buffer (get-buffer-create "*Email Completions*")
          (let ((standard-output (current-buffer))
                (completion-ignore-case 't))
            (display-completion-list
             (all-completions thingstr emails)
             thingstr)
            (display-buffer (current-buffer))
            ;;(set-window-dedicated-p (get-buffer-window (current-buffer)) 't)
            ))
      (progn
        ;; Kill the completion window if it exists because we now have a full completion
        (if (get-buffer "*Email Completions*")
            (kill-buffer (get-buffer "*Email Completions*")))
        (delete-region (car thing) (cdr thing))
        (org-email/insert email (current-buffer) (point))))))

;;;###autoload
(defun org-email-collect ()
  "Try and get the email at point."
  (interactive)
  (let ((email (thing-at-point 'email)))
    (when email
      (with-current-buffer (find-file-noselect (car org-email-files))
        (goto-char (point-max))
        (newline)
        (insert "* \n** " email " :email:")
        (pop-to-buffer (current-buffer))))))

(provide 'org-email)

;;; org-email.el ends here
