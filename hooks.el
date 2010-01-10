(add-hook 'diary-display-hook 'fancy-diary-display)

(add-hook 'cal-tex-hook 'my-calendar-a4)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(add-hook 'inferior-scheme-mode-hook 
          (lambda ()
            (define-key
              inferior-scheme-mode-map [tab]
              'scheme-complete-or-indent)))

(add-hook 'hen-mode-hook
 	  (lambda ()
 	    (make-local-variable 'eldoc-documentation-function)
 	    (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
 	    (eldoc-mode)))

(add-hook 'jabber-post-connect-hooks 'jabber-keepalive-start)

(add-hook 'jabber-post-connect-hooks 'jabber-autoaway-start)

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

;; Neat quotes:
(add-hook 'jabber-chat-mode-hook 'guillemets-mode)
(add-hook 'weblogger-entry-mode-hook 'guillemets-mode)
(add-hook 'w3m-form-input-textarea-mode-hook 'guillemets-mode)

(add-hook 'weblogger-start-edit-entry-hook 
          (lambda()  
            (flyspell-mode 1) 
            (ispell-change-dictionary 'svenska)
            (flyspell-buffer)))


;;(add-hook 'kill-emacs-hook 'net-stop)

;; Hooks for Erc:

;; Auto-save logs on every inserted line:
;;erc-save-buffer-in-logs to
(add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)

(add-hook 'erc-mode-hook 'guillemets-mode)

;; Auto-change Erc filling to fit window:
(add-hook 'window-configuration-change-hook 
 	  '(lambda ()
 	     (setq erc-fill-column (abs (- (window-width) 1)))))

;; (defun my-notify-erc (match-type nickuserhost message)
;;   "Notify when a message is received."
;;   (notify (format "%s in %s"
;;                   ;; Username of sender
;;                   (car (split-string nickuserhost "!"))
;;                   ;; Channel
;;                   (or (erc-default-target) "#unknown"))
;;           ;; Remove duplicate spaces
;;           (replace-regexp-in-string " +" " " message)
;;           :icon "emacs"
;;           :timeout -1))
;; (add-hook 'erc-text-matched-hook 'my-notify-erc)

(require 'erc-match)


;; Notify my when someone mentions my nick.
(defun erc-global-notify (matched-type nick msg)
  (interactive)
  (when (eq matched-type 'current-nick)
    (shell-command
     (concat "notify-send --icon /usr/share/xfm/pixmaps/emacs.xpm -t 4000 -c \"im.received\" \""
             (car (split-string nick "!"))
             " mentioned your nick\" \""
             msg
             "\""))))

(add-hook 'erc-text-matched-hook 'erc-global-notify)

(provide 'hooks)
