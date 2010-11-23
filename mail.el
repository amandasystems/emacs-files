;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mail client/reader settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq mm-text-html-renderer 'w3m) ;; Render html mail with w3m

(require 'mime-w3m)

(setq user-mail-address "albin@eval.nu"
      user-full-name "Albin Stjerna")

;; SMTP

(setq smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials (expand-file-name "~/.authinfo")
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info nil ; change to nil once it works
      smtpmail-debug-verb nil)

(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it)

(require 'starttls)

;; END SMTP

(setq mime-w3m-safe-url-regexp nil) ;; Don't bother.
(setq mime-w3m-display-inline-images t) ;; Yes, do render inline images
(setq mm-inline-text-html-with-images t
      mm-w3m-safe-url-regexp nil)
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

(setq mail-user-agent 'message-user-agent
      message-directory "~/inmail/Gmail/")

;; notmuch-fcc-dirs (quote (("Sent")))

;; add Cc and Bcc headers to the message buffer
(setq message-default-mail-headers "Cc: \nBcc: \n")

;; postponed message is put in the following draft file
;;(setq message-auto-save-directory "~/inmail/Main/Drafts")

(setq message-send-mail-function 'message-smtpmail-send-it)
(setq send-mail-function 'smtpmail-send-it)

(add-hook 'message-mode-hook (lambda () (visual-line-mode 1)))
(add-hook 'message-mode-hook 'turn-off-auto-fill)
(add-hook 'message-mode-hook (lambda () (guillemets-mode 1)))
(add-hook 'message-mode-hook 'flyspell-mode)
(add-hook 'message-mode-hook 'footnote-mode)

;;(add-hook 'notmuch-show-hook 'offlineimap)

;;sign messages by default
(add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)

(setq notmuch-saved-searches '(("personal" . "tag:personal and tag:inbox")
                               ("feeds" . "tag:feeds and tag:inbox")
                               ("facebook" . "tag:facebook and tag:inbox")
                               ("identica" . "tag:identica and tag:inbox")
                               ("local" . "tag:local and tag:inbox")
                               ("list" . "tag:list and tag:inbox")
                               ("todo" . "tag:todo")))



;;(execute-kbd-macro (symbol-function 'notmuch-todo)) ;; bind this to
;;T i notmuch-search-mode-map

(defun notmuch-search-todo ()
  (interactive)
  (notmuch-search-add-tag "todo")
  (notmuch-search-archive-thread))

(defun notmuch-search-untodo ()
  (interactive)
  (notmuch-search-remove-tag "todo")
  (notmuch-search-archive-thread))

(defun notmuch-show-todo ()
  (interactive)
  (notmuch-show-add-tag "todo")
  (notmuch-show-archive-thread))

(defun notmuch-show-untodo ()
  (interactive)
  (notmuch-show-remove-tag "todo")
  (notmuch-show-archive-thread))

(defun notmuch-display-trusted-images ()
  (interactive)
  (if (member "feeds" (notmuch-show-get-tags))
      (progn
        (make-variable-buffer-local 'mm-inline-text-html-with-images)
        (make-variable-buffer-local 'mm-w3m-safe-url-regexp)
        (setq mm-inline-text-html-with-images t
              mm-w3m-safe-url-regexp nil))))

(define-key notmuch-search-mode-map "T" 'notmuch-search-todo)
(define-key notmuch-search-mode-map "U" 'notmuch-search-untodo)
(define-key notmuch-show-mode-map "T" 'notmuch-show-todo)
(define-key notmuch-show-mode-map "U" 'notmuch-show-untodo)
(define-key notmuch-show-mode-map "\C-c\C-o" 'w3m-view-url-with-external-browser)

;; Integrate notmuch with org-mode's agenda view:
(add-hook 'org-finalize-agenda-hook (lambda ()
                                      (notmorg-write-file "/home/albin/org/notmorg.org" '("todo" t) "sched")))

(setq ks-monthnames-lastfm "")

(add-hook 'message-mode-hook 'tach-minor-mode)

(eudc-set-server "localhost" 'bbdb t)
(eudc-protocol-set 'eudc-inline-expansion-format 
		   '("%s %s <%s>" firstname lastname net)
		   'bbdb)
(eudc-set-server "localhost" 'notmuch t)
(setq eudc-server-hotlist '(("localhost" . bbdb)
			    ("localhost" . notmuch)))
(setq eudc-inline-expansion-servers 'hotlist)

(setq notmuch-addr-query-command "/home/albin/.bin/addrlookup")


