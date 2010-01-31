

;;(add-hook 'kill-emacs-hook 'net-stop)

;; Hooks for Erc:

;; Auto-save logs on every inserted line:
;;erc-save-buffer-in-logs to
;;(add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)



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



;;(provide 'hooks)
