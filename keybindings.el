(provide 'keybindings)

;; Use C-x C-m for M-x:
(global-set-key "\C-x\C-m" 'execute-extended-command)
;;(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Use C-w to backword-kill word and rebind kill-region to C-x C-k.
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
;;(global-set-key "\C-i" 'kill-ring-save)

(global-set-key "\C-h" 'delete-backward-char)

(global-set-key "\C-cp" 'delicious-post)

(global-set-key "\C-cip" 'identica-update-status-interactive)
(global-set-key "\C-cid" 'identica-direct-message-interactive)

(global-set-key "\C-cgi" 'ido-goto-symbol)
;; Shorten url by C-c u s
;;(global-set-key "\C-cus" 'get-tinyurl)

;; Browse url by C-c u f
(global-set-key "\C-cuf" 'browse-url-at-point)

(global-set-key (kbd "C-c S") 
  (lambda()(interactive)
    (ispell-change-dictionary "svenska")
    (flyspell-buffer))) 

(global-set-key (kbd "C-x C-b") 'ibuffer)
