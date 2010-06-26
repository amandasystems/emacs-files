;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mail client/reader settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq mm-text-html-renderer 'w3m) ;; Render html mail with w3m

(require 'secrets) ;; SMTP auth info etc.
(require 'mime-w3m)

(setq mime-w3m-safe-url-regexp nil) ;; Don't bother.
(setq mime-w3m-display-inline-images t) ;; Yes, do render inline images

;; Mu-cite, for less ugly citations.
(autoload 'mu-cite-original "mu-cite" nil t)
(setq mu-cite-prefix-format '("> "))
(setq mu-cite-top-format '(full-name " wrote:\n\n"))
(add-hook 'mail-citation-hook (function mu-cite-original))

;;;;;;;;;;
;; BBDB ;;
;;;;;;;;;;

(setq bbdb-file "~/.emacs.d/bbdb")           ;; keep ~/ clean; set before loading
(require 'bbdb) 
(bbdb-initialize)
(setq 
 bbdb-offer-save 1                        ;; 1 means save-without-asking
 bbdb-use-pop-up t                        ;; allow popups for addresses
 bbdb-electric-p t                        ;; be disposable with SPC
 bbdb-popup-target-lines  1               ;; very small 
 bbdb-dwim-net-address-allow-redundancy t ;; always use full name
 bbdb-quiet-about-name-mismatches 2       ;; show name-mismatches 2 secs
 bbdb-always-add-address t                ;; add new addresses to existing...
                                          ;; ...contacts automatically
 bbdb-canonicalize-redundant-nets-p t     ;; x@foo.bar.cx => x@bar.cx
 bbdb-completion-type nil                 ;; complete on anything
 bbdb-complete-name-allow-cycling t       ;; cycle through matches
                                          ;; this only works partially
 bbbd-message-caching-enabled t           ;; be fast
 bbdb-use-alternate-names t               ;; use AKA
 bbdb-elided-display t                    ;; single-line addresses
                                          ;; auto-create addresses from mail
 bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook
 bbdb-ignore-some-messages-alist          ;; don't ask about fake addresses
 ;; NOTE: there can be only one entry per header (such as To, From)
 ;; http://flex.ee.uec.ac.jp/texi/bbdb/bbdb_11.html

 '(( "From" . "no.?reply\\|DAEMON\\|daemon\\|facebookmail\\|twitter")))

(add-hook 'mail-mode-hook 'mail-abbrevs-setup)

;; Notmuch code:
(require 'notmuch)

(setq mail-user-agent 'message-user-agent)

;; add Cc and Bcc headers to the message buffer
(setq message-default-mail-headers "Cc: \nBcc: \n")

;; postponed message is put in the following draft file
;;(setq message-auto-save-directory "~/inmail/Main/Drafts")

(setq message-send-mail-function 'message-smtpmail-send-it)
(setq send-mail-function 'smtpmail-send-it)

;;(add-hook 'message-mode-hook 'auto-fill-mode)
(add-hook 'message-mode-hook (lambda () (guillemets-mode 1)))
(add-hook 'message-mode-hook 'flyspell-mode)

;;sign messages by default
(add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)

(setq notmuch-saved-searches '(("personal" . "tag:personal and tag:inbox")
                               ("feeds" . "tag:feeds and tag:inbox")
                               ("facebook" . "tag:facebook and tag:inbox")
                               ("identica" . "tag:identica and tag:inbox")
                               ("local" . "tag:local and tag:inbox")
                               ("list" . "tag:list and tag:inbox")
                               ("todo" . "tag:todo")))
