;; Mail Configuration
(require 'notmuch)

(setq mml-secure-openpgp-encrypt-to-self t)
(setq mml-secure-smime-encrypt-to-self t)

(setq notmuch-show-logo nil)
(setq notmuch-always-prompt-for-sender 't)

(autoload 'gnus-alias-determine-identity "/home/stefan/.doom.d/gnus-alias/gnus-alias.el" "" t)
(add-hook 'message-setup-hook 'gnus-alias-determine-identity)
;; Define two identities, "home" and "work"
(setq gnus-alias-identity-alist
      '(("private"
         nil ;; Does not refer to any other identity
         "Stefan Machmeier <stefan-machmeier@outlook.com>" ;; Sender address
         nil ;; No organization header
         nil ;; No extra headers
         nil ;; No extra body text
         "~/.signature.prv")
        ("work"
         nil
         "Stefan Machmeier <stefan.machmeier@urz.uni-heidelberg.de>"
         "Universitätsrechenzentrum"
         nil
         nil
         "~/.signature.work")))
;; Use "work" identity by default
(setq gnus-alias-default-identity "work")
;; Define rules to match private identity
(setq gnus-alias-identity-rules
     '(("private" ("any" "stefan.machmeier@\\(outlook\\.com\\)" both) "private")))
;; Determine identity when message-mode loads
(add-hook 'message-setup-hook 'gnus-alias-determine-identity)

;; SMTP Settings
(setq message-sendmail-f-is-evil 't)  ;; Remove unnecessary commands
(setq sendmail-program "~/bin/msmtp") ;; Set to own shell for notify-send
(setq mail-specify-envelope-from 't) 
(setq mail-envelope-from 'header)
(setq message-sendmail-envelope-from 'header)

;; Setting proper from, fixes i-did-not-set--mail-host-address--so-tickle-me
(setq mail-host-address "smachmeier.de")
(setq user-full-name "Stefan Machmeier")

(setq notmuch-maildir-use-notmuch-insert nil)
(setq notmuch-fcc-dirs '(
        ("Stefan Machmeier <stefan-machmeier@outlook.com>" . "private/Sent")
        ("Stefan Machmeier <stefan.machmeier@urz.uni-heidelberg.de>" . "work/Sent Items")))

;This constructs a path, concatenating the content of the variable
;"message-directory" and the second part in the alist:
(defun my-fcc-header-setup ()
  (let ((subdir (cdr (assoc (message-fetch-field "From") notmuch-fcc-dirs))))
    (message-add-header (concat "Fcc: " message-directory subdir))))

(add-hook 'message-send-hook 'my-fcc-header-setup)

(setq message-fcc-handler-function
  '(lambda (destdir)
      (notmuch-maildir-fcc-write-buffer-to-maildir destdir t)))

;; Compose Mail Settings
;; report problems with the smtp server
(setq smtpmail-debug-info t)
;; add Cc and Bcc headers to the message buffer
(setq message-default-mail-headers "Cc: \nBcc: \n")
;; postponed message is put in the following draft directory
(setq message-auto-save-directory "~/mails/draft")
(setq message-kill-buffer-on-exit t)
;; change the directory to store the sent mail
(setq message-directory "~/mails")

(defun message-recipients ()
  "Return a list of all recipients in the message, looking at TO, CC and BCC. Each recipient is in the format of `mail-extract-address-components'."
  (mapcan (lambda (header)
            (let ((header-value (message-fetch-field header)))
              (and
               header-value
               (mail-extract-address-components header-value t))))
          '("To" "Cc" "Bcc")))

;; Guided Encryption and sign settings, sometimes helpful
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

;; Add to custom

;; Sign messages by default.
;; (add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)
;; Decrypt messages by default
;; (add-hook 'message-encrypt-hook 'mml-secure-message-encrypt-pgpmime)
;; (add-hook 'message-send-hook 'message-sign-encrypt-if-all-keys-available)

;; (add-hook 'message-send-hook 'mml-secure-message-encrypt-smime)
;; (add-hook 'message-send-hook 'mml-secure-message-sign-smime)


;;(load "/home/stefan/.doom.d/defaultencrypt/jl-encrypt.el")
;;(load "/home/stefan/.doom.d/defaultencrypt/jl-smime.el")
;; (setq epa-file-encrypt-to "")
;; (setq epg-debug t)
;; Allow automatic LDAP queries for certificates within my domain.
;;(setq jl-smime-permit-ldap "@\\(.+\\.\\)?uni-heidelberg\\.de$")

;; I'm searching for S/MIME certificates via LDAPS at DFN-Verein.
;; Note that ldap.el in Emacs requires a minor workaround to perform
;; encrypted connections via LDAPS.  In fact, ldapsearch is being invoked
;; to use unencrypted plaintext LDAP communication with the parameter "-h".
;; Maybe I'm doing something wrong but I only got LDAPS to work with the
;; parameter "-H ldaps://ldap.pca.dfn.de".  To get rid of the default
;; parameter -h, I'm passing the empty string as hostname, setting
;; smime-ldap-host-list to '("").  Finally, ldapsearch aborts the
;; connection if it is not told where to find the CA certificate for the
;; LDAPS server (which is a Good Thing).
;; I created ~/.ldaprc with a single line pointing to that CA certificate:
;; TLS_CACERT /path/to/server/cert
;;(require 'ldap)
;;(setq smime-ldap-host-list '(""))
;;(setq ldap-default-base "O=DFN-Verein,C=DE"
;;      ; -x: no SASL authentication, -tt: store result in file
;;      ; -H: connect to specified URI.
;;      ldap-ldapsearch-args '("-x" "-tt" "-H ldaps://ldap.pca.dfn.de")
;;      )