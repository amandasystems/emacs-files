(require 'w3m-load)
;;(autoload 'doc-view "doc-view")

;;(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/projects/repos/identica-mode/")
(add-to-list 'load-path "~/projects/repos/lagn/")
(add-to-list 'load-path "~/projects/repos/git-emacs/")
(add-to-list 'load-path "~/projects/repos/elim/elisp/")
(add-to-list 'Info-default-directory-list "~/.info/")

(require 'secrets)

(setq printer-name "laserjet")

;; Put those pesky auto-save and back-up files in ONE, SEPARATE directory: 
(defvar autosave-dir "~/.emacs_autosaves/")

(make-directory autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
   (if buffer-file-name
      (concat "#" (file-name-nondirectory buffer-file-name) "#")
    (expand-file-name
     (concat "#%" (buffer-name) "#")))))


;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
;;(defvar backup-dir "~/.emacs_backups/")
;;(setq backup-directory-alist (list (cons "." backup-dir)))

;; Indent with spaces in stead.
;;(setq-default indent-tabs-mode nil) 

;; Copy/paste to xbuffer (for X interaction Zen):
;;(setq x-select-enable-clipboard t)

(setq-default ispell-program-name "aspell")
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(cal-tex-24 t)
 '(fill-column 80)
 '(flyspell-default-dictionary "sv")
 '(inhibit-startup-screen t)
 '(newsticker-html-renderer (quote w3m-region))
 '(org-export-author-info nil)
 '(org-export-creator-info nil)
 '(scheme-program-name "csi")
 '(term-input-autoexpand (quote input))
 '(tramp-default-user "albin")
 '(w3m-broken-proxy-cache nil)
 '(w3m-coding-system (quote utf-8))
 '(w3m-confirm-leaving-secure-page nil)
 '(w3m-default-display-inline-images t)
 '(w3m-file-coding-system (quote utf-8))
 '(w3m-file-name-coding-system (quote utf-8))
 '(w3m-key-binding (quote info))
 '(w3m-resize-images t)
 '(w3m-terminal-coding-system (quote utf-8))
 '(w3m-use-cookies t)
 '(w3m-use-filter nil)
 '(w3m-use-title-buffer-name t)
 '(w3m-use-toolbar nil))

;; Browse with emacs-w3m:
(setq browse-url-browser-function 'w3m-browse-url
      browse-url-new-window-flag t)

(defun hgr-post ()
  (interactive)
  (browse-url "http://handgranat.org/posta/Tussilago/"))

;; WORD:
(autoload 'no-word "no-word" "word to txt")
(add-to-list 'auto-mode-alist '("\\.doc\\'" . no-word))

(setq inferior-lisp-program "sbcl") 

(setq calendar-date-style 'europeian)
(setq calendar-week-start-day 1
      calendar-day-name-array
      ["Söndag" "Måndag" "Tisdag" 
       "Onsdag" "Torsdag" "Fredag" "Lördag"]
      calendar-month-name-array
      ["Januari" "Februari" "Mars" "April"
       "Maj" "Juni" "Juli" "Augusti" "September"
       "Oktober" "November" "December"])
;;(setq cal-html-directory "~/pandora/public_html/cal")
(setq tex-dvi-print-command "dvips -f * | lp -d laserjet -o media=a4 -o fitplot -")

(defun my-calendar-a4 ()
  "Replace all occurences of 18cm with 17cm."
  (goto-char (point-min))
  (while (search-forward "18cm" nil t)
    (replace-match  "17cm")))

(setq cal-tex-diary t)

(setq cal-tex-preamble-extra "\\usepackage[utf8]{inputenc}\n")

;;(autoload 'scheme-complete "scheme-complete" nil t)

;; (eval-after-load 'hen
;;   '(progn (define-key hen-mode-map "\t" 'scheme-complete-or-indent)))

;;(autoload 'scheme-get-current-symbol-info "scheme-complete" nil t)
;;(autoload 'scheme-complete-or-indent "scheme-complete" nil t)

;;(setq auto-mode-alist (cons '("\\.scm$" . hen-mode) auto-mode-alist))

;;(require 'hen)

(require 'delicious)

(setq auto-mode-alist
   (cons '("\\.mdwn" . markdown-mode) auto-mode-alist))

(require 'tramp)
(tramp-parse-shosts "~/.ssh/known_hosts")

;;(setq wikipedia-default-language-domain "en")


;; (add-to-list 'load-path "/usr/local/share/distel/elisp")
;; (require 'distel)
;;   (distel-setup)

;; (autoload 'babel "babel"
;;   "Use a web translation service to translate the message MSG." t)
;; (autoload 'babel-region "babel"
;;   "Use a web translation service to translate the current region." t)
;; (autoload 'babel-as-string "babel"
;;   "Use a web translation service to translate MSG, returning a string." t)
;; (autoload 'babel-buffer "babel"
;;   "Use a web translation service to translate the current buffer." t)

(require 'smart-quotes)

;;; Identi.ca mode
(require 'identica-mode)
(setq identica-username "tuss")
;; End Identi.ca mode

(require 'keybindings)
;;(require 'hooks)
(require 'git-emacs)

;;(setq ido-enable-flex-matching t) 

(defun pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t) 
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))


(defadvice kill-ring-save (before slick-copy activate compile) "When called
  interactively with no active region, copy a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end)) (message
  "Copied line") (list (line-beginning-position) (line-beginning-position
  2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))

(add-hook 'diary-display-hook 'fancy-diary-display)

(add-hook 'cal-tex-hook 'my-calendar-a4)

;; (add-hook 'inferior-scheme-mode-hook 
;;           (lambda ()
;;             (define-key
;;               inferior-scheme-mode-map [tab]
;;               'scheme-complete-or-indent)))

;; (add-hook 'hen-mode-hook
;;  	  (lambda ()
;;  	    (make-local-variable 'eldoc-documentation-function)
;;  	    (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
;; 	    (eldoc-mode)))

;; (add-hook 'weblogger-entry-mode-hook 'guillemets-mode)
(add-hook 'w3m-form-input-textarea-mode-hook 'guillemets-mode)

;; (add-hook 'weblogger-start-edit-entry-hook 
;;           (lambda()  
;;             (flyspell-mode 1) 
;;             (ispell-change-dictionary 'svenska)
;;            (flyspell-buffer)))


;; utf8 / input-method
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(setq default-input-method 'swedish-postfix)
(set-input-method 'swedish-postfix)

