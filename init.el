(require 'w3m-load)

;;(add-to-list 'load-path "~/projects/repos/identica-mode/")
;;(add-to-list 'load-path "~/projects/repos/lagn/") ;; Seems dead.
;;(add-to-list 'load-path "~/projects/repos/git-emacs/")

(defvar repo-dir "/home/albin/projects/repos/emacs/")

(add-to-list 'load-path (concat repo-dir "identica-mode"))
(add-to-list 'load-path (concat repo-dir "delicious-el"))
(add-to-list 'load-path (concat repo-dir "37emacs"))
(add-to-list 'load-path (concat repo-dir "git-emacs"))
(add-to-list 'load-path (concat repo-dir "ii-mode"))
;;(add-to-list 'load-path (concat repo-dir "weblogger-el"))

;; clojure-mode
(add-to-list 'load-path  (concat repo-dir "clojure-mode"))
(require 'clojure-mode)

;; swank-clojure
(add-to-list 'load-path (concat repo-dir "swank-clojure/src/emacs"))

(setq swank-clojure-jar-path "~/.clojure/clojure.jar"
      swank-clojure-extra-classpaths (list
				      (concat repo-dir "swank-clojure/src/main/clojure")
				      "~/.clojure/clojure-contrib.jar"))

;;(require 'swank-clojure-autoload)

;; slime
(eval-after-load "slime" 
  '(progn (slime-setup '(slime-repl))))

(add-to-list 'load-path (concat repo-dir "slime"))
(require 'slime)
(slime-setup) 



(require 'midnight)


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

(setq-default ispell-program-name "aspell")
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(cal-tex-24 t)
;; '(fill-column 65)
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
;; (autoload 'no-word "no-word" "word to txt")
;; (add-to-list 'auto-mode-alist '("\\.doc\\'" . no-word))

;; (setq inferior-lisp-program "sbcl") 

;;;;;;;;;;;;;;;;;;;;;;;
;; Calendar settings ;;
;;;;;;;;;;;;;;;;;;;;;;;

;;(setq calendar-date-style 'europeian)
(require 'calendar)
(calendar-set-date-style 'european)

;; Swedish calendar:
(setq calendar-week-start-day 1
      calendar-day-name-array
      ["söndag" "måndag" "tisdag" 
       "onsdag" "torsdag" "fredag" "lördag"]
      calendar-month-name-array
      ["januari" "februari" "mars" "april"
       "maj" "juni" "juli" "augusti" "september"
       "oktober" "november" "december"])

(defun my-calendar-a4 ()
  "Replace all occurences of 18cm with 17cm."
  (goto-char (point-min))
  (while (search-forward "18cm" nil t)
    (replace-match  "17cm")))

(setq cal-tex-diary t
      cal-tex-preamble-extra "\\usepackage[utf8]{inputenc}\n"
      calendar-intermonth-text ;; Display ISO week numbers in calendar
        '(propertize
          (format "%2d"
                  (car
                   (calendar-iso-from-absolute
                    (calendar-absolute-from-gregorian (list month day year)))))
          'font-lock-face 'font-lock-function-name-face))

(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'cal-tex-hook 'my-calendar-a4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End of calendar settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'delicious)

(setq auto-mode-alist
   (cons '("\\.mdwn" . markdown-mode) auto-mode-alist))

(require 'tramp)
(tramp-parse-shosts "~/.ssh/known_hosts")

(require 'smart-quotes)

;;; Identi.ca mode
(require 'identica-mode)
;; End Identi.ca mode

(require 'keybindings)
(require 'git-emacs)

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


(add-hook 'w3m-form-input-textarea-mode-hook 'guillemets-mode)

;; utf8 / input-method
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(setq default-input-method 'swedish-postfix)
(set-input-method 'swedish-postfix)

(defun ido-goto-symbol ()
    "Will update the imenu index and then use ido to select a symbol to navigate to"
    (interactive)
    (imenu--make-index-alist)
    (let ((name-and-pos '())
          (symbol-names '()))
      (flet ((addsymbols (symbol-list)
                         (when (listp symbol-list)
                           (dolist (symbol symbol-list)
                             (let ((name nil) (position nil))
                               (cond
                                ((and (listp symbol) (imenu--subalist-p symbol))
                                 (addsymbols symbol))
   
                                ((listp symbol)
                                 (setq name (car symbol))
                                 (setq position (cdr symbol)))
   
                                ((stringp symbol)
                                 (setq name symbol)
                                 (setq position (get-text-property 1 'org-imenu-marker symbol))))
   
                               (unless (or (null position) (null name))
                                 (add-to-list 'symbol-names name)
                                 (add-to-list 'name-and-pos (cons name position))))))))
        (addsymbols imenu--index-alist))
      (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
             (position (cdr (assoc selected-symbol name-and-pos))))
        (cond
         ((overlayp position)
          (goto-char (overlay-start position)))
         (t
          (goto-char position))))))

(require 'ibuffer) 
(setq ibuffer-saved-filter-groups
  (quote (("default"      
            ("Org" ;; all org-related buffers
              (mode . org-mode))  
            ("Mail"
              (or  ;; mail-related buffers
               (mode . message-mode)
               (mode . mail-mode)
               ;; etc.; all your mail related modes
               ))
            ("Jabber"
             (mode . jabber-chat))
            ;; ("MyProject1"
            ;;   (filename . "src/myproject1/"))
            ;; ("MyProject2"
            ;;   (filename . "src/myproject2/"))
            ("Programming" ;; prog stuff not already in MyProjectX
              (or
                (mode . c-mode)
                (mode . perl-mode)
                (mode . python-mode)
                (mode . emacs-lisp-mode)
                ;; etc
                )) 
            ("ERC"   (mode . erc-mode))))))

(add-hook 'ibuffer-mode-hook
  (lambda ()
    (ibuffer-switch-to-saved-filter-groups "default")))

;; Org-mode:
;; Add all files ending with ".org" in ~/org/ to agenda, do not sort them.
(setq org-agenda-files (directory-files "~/org" t ".*\.org" t))


;; Bind a yank-menu to C-cy:
(global-set-key "\C-cy" '(lambda ()
   (interactive)
   (popup-menu 'yank-menu)))


(server-start)

;; compensate for (add-hook 'text-mode-hook 'turn-on-flyspell)
(remove-hook 'text-mode-hook 'turn-on-flyspell)
(remove-hook 'text-mode-hook 'turn-on-auto-fill)

;; Use smart quotes everywhere!
(add-hook 'text-mode-hook (lambda () (guillemets-mode 1)))

(setq org-startup-indented t)

(add-hook 'org-mode-hook (lambda () (visual-line-mode t)))

(setq journal-file "~/org/journal.org")

(require 'org)

(defun start-journal-entry ()
  "Start a new journal entry."
  (interactive)
  (find-file journal-file)
  ;;  (goto-char (point-min))
  (goto-char (point-max))
  (org-insert-heading)
  (org-insert-time-stamp (current-time) t)
;;  (open-line 2)
  (insert " "))

(global-set-key (kbd "C-c j") 'start-journal-entry)

(require 'eshell)

(defun xmms2-run-or-goto () ; FIXME -- make less ugly
  "start a new session of nyxmms2"
  ;; in future version use start-process and process-send-string
  (interactive)
  (unless (bufferp (get-buffer "*xmms2*"))
    (pop-to-buffer "*xmms2*")
    (eshell-mode)
    (goto-char (point-max))
    (insert "nyxmms2")
    (eshell-send-input))
  (pop-to-buffer "*xmms2*"))

(global-set-key (kbd "C-x x") 'xmms2-run-or-goto)

(add-hook 'text-mode-hook (lambda () (visual-line-mode t)))

(setq project-root "~/projects/")

(defun new-project (name)
  "Create a new project"
  (interactive "sEnter project name: ")
  (defvar project-root "~/projects/" "Root directory of projects")
  (let ((project-path (concat project-root name)))
    (make-directory project-path t)
    (message "Project created: %s" project-path))
  (find-file (concat project-root "README.org")))

(require 'emms-setup)
(emms-standard)
(emms-default-players)
(push 'emms-player-mplayer emms-player-list)

(setq emms-source-file-default-directory "/var/storage/downloads/")

(setq org-agenda-include-diary t)
