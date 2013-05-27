;;; org-email-tests.el -- test the org-email stuff

(require 'ert)
(require 'org-email)

(ert-deftest org-email-test-structure ()
  "Tests that the expected structure can be navigated properly."
  (with-temp-buffer
   (insert "* bill the buck
** email
*** billbuck@example1.com
** partner
*** Gillie The Girl
* lesley lady
** colleague @ woomedia
*** CSS programmer
** partner
*** Jimmy Screws
** email
*** ll@example10.org")
   (org-mode)
   ;; Have we got the basics right?
   (save-excursion
     (goto-char (point-min))
     (should (equal "* bill the buck" 
                    (buffer-substring-no-properties (point-min)(line-end-position)))))
   ;; Now pull the emails and check two
   (let* ((emails (org-email--buffer-emails (current-buffer)))
          (bill (assoc "bill the buck" emails))
          (lesley (assoc "lesley lady" emails)))
     (should (equal "billbuck@example1.com" (cdr bill)))
     (should (equal "ll@example10.org" (cdr lesley))))))


;;; org-email-tests.el ends here
