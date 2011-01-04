;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fetch code using el-get ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'el-get)

(setq el-get-sources
      '((:name identica-mode
               :type git
               :url "http://git.savannah.gnu.org/cgit/identica-mode.git"
               :features identica-mode)
        (:name ii-mode
               :type git
               :url "http://github.com/krl/ii-mode.git"
               :features: ii-mode)
        (:name google-weather
               :type git
               :url "git://git.naquadah.org/google-weather-el.git"
               :features: google-weather)
        (:name offlineimap
               :type git
               :url "git://git.naquadah.org/offlineimap-el.git"
               :features offlineimap)
        (:name notmorg
               :type git
               :url "http://github.com/krl/notmorg.git"
               :features notmorg)
        (:name git-emacs
               :type git
               :url "git://github.com/tsgates/git-emacs.git"
               :features git-emacs
               :build ("make"))
        (:name tea-time
               :type git
               :url "git://github.com/krick/tea-time.git")
        (:name tach
               :type git
               :url "http://jkr.acm.jhu.edu/git/tach.git"
               :features tach)
        (:name 37emacs
               :type git
               :url "git://github.com/hober/37emacs.git"
               :build ("make")
               :features rest-api
               :load  ("./rest-api.el"))
        (:name el-get
               :type git
               :url "git://github.com/dimitri/el-get.git"
               :features el-get)
        (:name notmuch-eudc
               :type git
               :url "http://jkr.acm.jhu.edu/git/notmuch_eudc.git")
        (:name planner-el         :type apt-get)
        (:name delicious-el
               :type git
               :url "http://git.wjsullivan.net/delicious-el.git"
               :features delicious)
        (:name xml-rpc          :type elpa)
        (:name bbdb             :type apt-get)
        (:name auctex           :type apt-get)
        (:name org-mode         :type apt-get)
        (:name debian-el        :type apt-get)
        (:name w3m-el-snapshot  :type apt-get)
        (:name emacs-goodies-el :type apt-get)))

(el-get)

;;;;;;;;;;;;;;;;;;;;;;;;
;; End of el-get code ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(require 'w3m-load)
(require 'midnight)

(setq diary-file "~/org/diary")

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
 '(w3m-use-toolbar nil)
 '(org-default-notes-file "~/org/notes.org")
 '(org-agenda-ndays 7)
 '(org-deadline-warning-days 0)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-reverse-note-order t)
 '(org-remember-store-without-prompt t)
 '(org-remember-templates
   (quote ((116 "* TODO %?\n  %u" "~/org/todo.org" "Tasks")
           (110 "* %u %?" "~/org/notes.org" "Notes"))))
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-handler-functions (quote (org-remember-handler))))

;; Browse with emacs-w3m:
;; (setq browse-url-browser-function 'w3m-browse-url
;;       browse-url-new-window-flag t)

;; Browse with Conkeror:
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "/usr/bin/conkeror")

;;;;;;;;;;;;;;;;;;;;;;;
;; Calendar settings ;;
;;;;;;;;;;;;;;;;;;;;;;;

(require 'calendar)
(calendar-set-date-style 'european)

;; Do not show any holidays:
(setq calendar-holidays nil)

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

(setq auto-mode-alist
      (cons '("\\.mdwn" . markdown-mode) auto-mode-alist))

(require 'tramp)
(tramp-parse-shosts "~/.ssh/known_hosts")
(require 'smart-quotes)

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
                (or (mode . org-mode)
                    (filename . "~/org/*")))
               ("Mail"
                (or  ;; mail-related buffers
                 (mode . message-mode)
                 (mode . mail-mode)
                 (mode . notmuch-show-mode)
                 (mode . notmuch-search-mode)
                 (mode . notmuch-hello-mode)))
               ("Jabber"
                (or
                 (mode . jabber-chat-mode)
                 (mode . jabber-roster-mode)))
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
               ("IRC"   (mode . ii-mode))))))

(setq ibuffer-show-empty-filter-groups nil)

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;;;;;;;;;;;;;;;;
;; Org config ;;
;;;;;;;;;;;;;;;;

(require 'org-protocol)
(require 'org-capture)
;; ;; the 'w' corresponds with the 'w' used before as in:
;;   emacsclient \"org-protocol:/capture:/w/  [...]

(setq org-capture-templates
  '(
     ("w" "" entry ;; 'w' for 'org-protocol'
       (file+headline "~/org/www.org" "Bokmärken")
       "* %c %^g \n:DATE: %T \n%^{Description}")
     ))


(setq org-agenda-files '("/home/albin/org/todo.org"
                         "/home/albin/org/projekt.org"
                         "/home/albin/org/notmorg.org"
                         "/home/albin/org/weather.org"
                         "/home/albin/org/skolan.org"))

(setq org-agenda-custom-commands
      '(("w" todo "WAITING" nil)
        ("s" todo "SOMEDAY" nil)
        ("t" todo "TODO" nil)))

(setq org-agenda-include-diary t)
(define-key mode-specific-map [?a] 'org-agenda)

(setq org-todo-keywords (quote ((sequence "TODO" "|" "DONE")
                                (sequence "WAITING" "|" "CANCELLED" "|" "DONE")
                                (sequence "SOMEDAY" "|" "CANCELLED"))))

(setq org-todo-keyword-faces
      (quote (("TODO"      :foreground "red"          :weight bold)
              ("DONE"      :foreground "forest green" :weight bold)
              ("WAITING"   :foreground "violet"       :weight bold)
              ("SOMEDAY"   :foreground "goldenred"    :weight bold)
              ("CANCELLED" :foreground "orangered"    :weight bold))))

(add-hook 'org-mode-hook (lambda () (visual-line-mode t)))
(add-hook 'org-mode-hook (lambda () (org-indent-mode t)))
(add-hook 'org-mode-hook (lambda () (guillemets-mode t)))

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

(setq org-log-done t)

(setq org-export-default-language "sv")

;; (add-to-list 'org-export-latex-classes
;;              '("article"
;;                "\\documentclass[12pt,a4paper]{article}"
;;                ("\\section{%s}" . "\\section*{%s}")))  

;; (add-to-list 'org-export-latex-packages-alist
;;              '("" "tgschola" t))

;; (add-to-list 'org-export-latex-packages-alist
;;              '("left=3.18cm,top=2.54cm,bottom=2.54cm,right=3.18cm"
;;  "geometry" t))

(org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
        (R . t)
        (sh . t)
        (python . t)))

(require 'google-weather)
(require 'org-google-weather)

;;;;;;;;;;;;;;;;;;;;
;; End Org config ;;
;;;;;;;;;;;;;;;;;;;;

;; Bind a yank-menu to C-cy:
(global-set-key "\C-cy" '(lambda ()
                           (interactive)
                           (popup-menu 'yank-menu)))

;; compensate for (add-hook 'text-mode-hook 'turn-on-flyspell)
(remove-hook 'text-mode-hook 'turn-on-flyspell)
(remove-hook 'text-mode-hook 'turn-on-auto-fill)

;; Use smart quotes everywhere!
;; On a second thought -- don't
;;(add-hook 'text-mode-hook (lambda () (guillemets-mode 1)))





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



;; Numbered links for w3m:
;; courtesy of http://emacs.wordpress.com/2008/04/12/numbered-links-in-emacs-w3m/,
(require 'w3m-lnum)
(require 'w3m)

(add-hook 'w3m-mode-hook (lambda () (let ((active w3m-link-numbering-mode))
                                      (when (not active) (w3m-link-numbering-mode)))))
;; End numbered links

(type-break-mode)

(set-frame-font "DejaVu Sans Mono-12")

;; Use C-x C-m for M-x:
(global-set-key "\C-x\C-m" 'execute-extended-command)

;; Use C-w to backword-kill word and rebind kill-region to C-x C-k.
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-cp" 'delicious-post)
(global-set-key "\C-cip" 'identica-update-status-interactive)
(global-set-key "\C-cid" 'identica-direct-message-interactive)
(global-set-key "\C-cgi" 'ido-goto-symbol)

;; Browse url by C-c u f
(global-set-key "\C-cuf" 'browse-url-at-point)

(global-set-key (kbd "C-c S")
                (lambda()(interactive)
                  (ispell-change-dictionary "svenska")
                  (flyspell-buffer)))

(global-set-key (kbd "C-x C-b") 'ibuffer)


(defun count-words (&optional begin end)
  "count words between BEGIN and END (region); if no region defined, count words in buffer"
  (interactive "r")
  (let ((b (if mark-active begin (point-min)))
      (e (if mark-active end (point-max))))
    (message "Word count: %s" (how-many "\\w+" b e))))

(require 'quack)
(require 'printing)
(setq warning-suppress-types nil) ;; workaround compile errors
(server-start)
