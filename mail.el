;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mail client/reader settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; wanderlust
(require 'wl)
;;(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

(setq mm-text-html-renderer 'w3m ;; Render html mail with w3m
      ;;wl-spam-auto-check-folder-regexp-list '(".*")
      )
(autoload 'wl-user-agent-compose "wl-draft" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))

(require 'secrets) ;; SMTP auth info etc.
(require 'mime-w3m)

(setq wl-folder-check-async t
      elmo-imap4-default-stream-type 'ssl
      elmo-imap4-use-modified-utf7 t
      elmo-imap4-debug nil) 



(setq wl-summary-weekday-name-lang "en")

;; Prefetch everything:
(setq wl-message-buffer-prefetch-threshold nil)	; fetch everything 1
(setq wl-prefetch-confirm nil)			; fetch everything 2
(setq wl-summary-incorporate-marks '("N" "U" "!" "A" "F"))
(setq wl-prefetch-threshold nil)

(setq mime-w3m-safe-url-regexp nil) ;; Don't bother.
(setq mime-w3m-display-inline-images t)

(add-hook 'wl-mail-setup-hook 'auto-fill-mode)
(add-hook 'wl-mail-setup-hook 'guillemets-mode)

(require 'bbdb-wl)
(bbdb-wl-setup)

;; i don't want to store addresses from my mailing folders
(setq 
  bbdb-wl-folder-regexp    ;; get addresses only from these folders
  "^\.*Main.*Inbox$\\|^.*Sent\\|^.*Gmail.*")    ;; 


(define-key wl-draft-mode-map (kbd "<tab>") 'bbdb-complete-name)

(setq
  wl-forward-subject-prefix "Fwd: " )

;; from a WL-mailinglist post by David Bremner

;; Invert behaviour of with and without argument replies.
;; just the author
;; (setq wl-draft-reply-without-argument-list
;;   '(("Reply-To" ("Reply-To") nil nil)
;;      ("Mail-Reply-To" ("Mail-Reply-To") nil nil)
;;      ("From" ("From") nil nil)))


;; ;; bombard the world
;; (setq wl-draft-reply-with-argument-list
;;   '(("Followup-To" nil nil ("Followup-To"))
;;      ("Mail-Followup-To" ("Mail-Followup-To") nil ("Newsgroups"))
;;      ("Reply-To" ("Reply-To") ("To" "Cc" "From") ("Newsgroups"))
;;      ("From" ("From") ("To" "Cc") ("Newsgroups"))))


(defun djcb-wl-draft-subject-check ()
  "check whether the message has a subject before sending"
  (if (and (< (length (std11-field-body "Subject")) 1)
        (null (y-or-n-p "No subject! Send current draft?")))
      (error "Abort.")))

(add-hook 'wl-mail-send-pre-hook 'djcb-wl-draft-subject-check)

(setq
 wl-stay-folder-window t                       ;; show the folder pane (left)
 wl-folder-window-width 25                     ;; toggle on/off with 'i'
 wl-fcc ".~/Main/Sent"
 wl-fcc-force-as-read t               ;; mark sent messages as read
 wl-draft-folder ".~/Main/Drafts"            ;; store drafts in 'postponed'
 wl-trash-folder ".~/Main/Trash"             ;; put trash in 'trash'
 wl-spam-folder ".~/Main/spam"              ;; ...spam as well

 wl-biff-check-folder-list '(".~/Main/INBOX")
 wl-message-ignored-field-list '("^.*:")
 wl-message-visible-field-list
 '("^\\(To\\|Cc\\):"
   
   "^Subject:"
   "^\\(From\\|Reply-To\\):"
   "^Organization:"
   "^Message-Id:"
   "^\\(Posted\\|Date\\):"
   )
 wl-message-sort-field-list
 '("^From"

   "^Organization:"
   "^X-Attribution:"
   "^Subject"
   "^Date"
   "^To"
   "^Cc")
)

(add-hook 'wl-auto-check-folder-hook 'wl-folder-open-all-unread-folder)

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
 bbdb-ignore-some-messages-alist ;; don't ask about fake addresses
 ;; NOTE: there can be only one entry per header (such as To, From)
 ;; http://flex.ee.uec.ac.jp/texi/bbdb/bbdb_11.html

 '(( "From" . "no.?reply\\|DAEMON\\|daemon\\|facebookmail\\|twitter")))

;; Convert markdown syntax to multipart MIME HTML mail
;; (defun mimedown ()
;;   (interactive)
;;   (save-excursion
;;     (message-goto-body)
;;     (let* ((sig-point (save-excursion (message-goto-signature) (forward-line -1) (point)))
;;            (orig-txt (buffer-substring-no-properties (point) sig-point)))
;;       (shell-command-on-region (point) sig-point "markdown" nil t)
;;       (insert "<#multipart type=alternative>\n")
;;       (insert orig-txt)
;;       (insert "<#part type=text/html>\n< html>\n< head>\n< title> HTML version of email</title>\n</head>\n< body>")
;;       (exchange-point-and-mark)
;;       (insert "\n</body>\n</html>\n<#/multipart>\n"))))

;; Add signatures:
;; (add-hook 'mime-edit-translate-hook 'mime-edit-insert-signature)
;; (setq signature-file-name "~/.signature")
;; (setq signature-insert-at-eof t)
;; (setq signature-delete-blank-lines-at-eof t)

(add-hook 'mail-mode-hook 'mail-abbrevs-setup)
