
(add-hook 'jabber-chat-mode-hook 'guillemets-mode)
(add-hook 'weblogger-entry-mode-hook 'guillemets-mode)

(add-hook 'w3m-form-input-textarea-mode-hook 'guillemets-mode)

(add-hook 'weblogger-start-edit-entry-hook 
          (lambda()  
            (flyspell-mode 1) 
            (ispell-change-dictionary 'svenska)
            (flyspell-buffer)))


(provide 'hooks)