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

(provide 'hooks)
