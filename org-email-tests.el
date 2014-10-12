;;; org-email-tests.el -- test the org-email stuff

(require 'ert)
(require 'org-email)

(defconst org-email-tests-dir
  (file-name-directory
   (or (buffer-file-name)
       load-file-name
       default-directory)))

(ert-deftest org-email-lists ()
  "Test the parsing of org files with emails."
  (should
   (equal
    (org-email-list
     (expand-file-name "test.org" org-email-tests-dir))
    '(("nic ferrier" . "nic@ferrier.me.uk")
      ("J Love" . "j.luv@somemail.com")
      ("Bob Towney" . "B.towney@digital.place.email.uk")
      ("Some Guy" . "guy@some.org")))))



;;; org-email-tests.el ends here
