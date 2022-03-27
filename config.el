;; Mail Configuration

(setq notmuch-show-logo nil)
(setq notmuch-always-prompt-for-sender 't)

;; SMTP Settings
(setq message-sendmail-f-is-evil 't)  ;; Remove unnecessary commands
(setq sendmail-program "~/bin/msmtp") ;; Set to own shell for notify-send
(setq mail-specify-envelope-from 't) 
(setq mail-envelope-from 'header)
(setq message-sendmail-envelope-from 'header)

;; Setting proper from, fixes i-did-not-set--mail-host-address--so-tickle-me
(setq mail-host-address "smachmeier.de")
(setq user-full-name "Stefan Machmeier")

;; Compose Mail Settings
;; report problems with the smtp server
(setq smtpmail-debug-info t)
;; add Cc and Bcc headers to the message buffer
(setq message-default-mail-headers "Cc: \nBcc: \n")
;; postponed message is put in the following draft directory
(setq message-auto-save-directory "~/mails/draft")
(setq message-kill-buffer-on-exit t)
;; change the directory to store the sent mail
(setq message-directory "~/mails/")
;; Saving sent mail in folders depending on from
(setq notmuch-fcc-dirs '(("stefan.machmeier@urz.uni-heidelberg.de" . "work/Sent")
                         ("stefan-machmeier@outlook.com" . "private/Sent")))

;; Sign messages by default.
;; (add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)
;; (setq mm-sign-option 'guided)

;; Encryption
(setq notmuch-crypto-process-mime t)

;; S/MIME Configuration, not working atm
(require 'epa-file)
(epa-file-enable)
(setq epg-debug t)
(setq password-cache t) ; default is true, so no need to set this actually
(setq password-cache-expiry 86400); default is 16 seconds

(setq mm-decrypt-option 'always)
(setq mm-verify-option 'always)
(setq gnus-buttonized-mime-types '("multipart/encrypted" "multipart/signed"))
(setq mml-smime-use 'epg)

(setq mew-protect-privacy-always t)
(setq mew-protect-privacy-always-type 'smime-signature)

(add-hook 'message-send-hook 'mml-secure-message-encrypt-smime)
(add-hook 'message-send-hook 'mml-secure-message-sign-smime)

(defun message-recipients ()
  "Return a list of all recipients in the message, looking at TO, CC and BCC. Each recipient is in the format of `mail-extract-address-components'."
  (mapcan (lambda (header)
            (let ((header-value (message-fetch-field header)))
              (and
               header-value
               (mail-extract-address-components header-value t))))
          '("To" "Cc" "Bcc")))

(defun message-all-epg-keys-available-p ()
  "Return non-nil if the pgp keyring has a public key for each recipient."
  (require 'epa)
  (let ((context (epg-make-context epa-protocol)))
    (catch 'break
      (dolist (recipient (message-recipients))
        (let ((recipient-email (cadr recipient)))
          (when (and recipient-email (not (epg-list-keys context recipient-email)))
            (throw 'break nil))))
      t)))

(defun message-sign-encrypt-if-all-keys-available ()
  "Add MML tag to encrypt message when there is a key for each recipient. Consider adding this function to `message-send-hook' to systematically send encrypted emails when possible."
  (when (message-all-epg-keys-available-p)
    (mml-secure-message-sign-encrypt)))

;; (add-hook 'message-send-hook 'message-sign-encrypt-if-all-keys-available)
