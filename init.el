;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fetch code using el-get ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; El-get is actually maintained by el-get. However, we need to
;; bootstrap it.
(add-to-list 'load-path "~/.emacs.d/el-get/el-get/") 

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
        (:name google-weather)
        (:name offlineimap)
        (:name quack)
        (:name emms)
;;        (:name org-mode)
;;        (:name org-contacts)
        ;; (:name notmorg
        ;;        :type git
        ;;        :url "http://github.com/krl/notmorg.git"
        ;;        :features notmorg)
        (:name git-emacs)
        (:name tea-time
               :type git
               :url "git://github.com/krick/tea-time.git")
        (:name 37emacs
               :type git
               :url "git://github.com/hober/37emacs.git"
               :build ("make")
               :features rest-api
               :load  ("./rest-api.el"))
        (:name el-get)
        (:name geiser)
        (:name auto-complete)
        (:name anything)
        (:name org2blog
               :type git
               :url "http://github.com/punchagan/org2blog.git"
               :features org2blog)
        (:name xml-rpc          :type elpa)
        (:name emacs-jabber)
        (:name bbdb             :type apt-get)
        (:name pymacs)
        (:name auctex           :type apt-get)
        (:name debian-el        :type apt-get)
        (:name org-mode         :type apt-get)
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

;; Put all customizations in this file.
(setq custom-file "~/.emacs.d/albin/custom.el")
(load custom-file)

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
  '(("w" "www" entry ;; 'w' for 'org-protocol'
       (file+headline "~/org/www.org" "Bokmärken")
       "* %c %^g \n:DATE: %T \n%^{Description}")
     ("j" "Journal" entry (file+datetree "~/org/journal.org.gpg")
      "* %?\nEntered on %U\n")
     ("r" "read/review" entry (file "~/org/read-review.org")
      "* TODO läs %c"
      :immediate-finish t)))

(define-key global-map "\C-cc" 'org-capture)


(setq org-agenda-files '("/home/albin/org/todo.org"
                         "/home/albin/org/projekt.org"
                         ;; "/home/albin/org/notmorg.org"
                         "/home/albin/org/weather.org"
                         "/home/albin/org/skolan.org"
                         "/home/albin/org/1:5.org"))

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

(require 'org)
(setq org-log-done t)

(setq org-export-default-language "sv")

(org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
        (R . t)
        (sh . t)
        (python . t)))

(require 'google-weather)
(require 'org-google-weather)

;; org2blog begin here

(require 'org2blog)

(setq org2blog/wp-blog-alist
       '(("wordpress"
          :url "http://eval.nu/xmlrpc.php"
          :username "admin"   
          :default-categories ("på svenska")
          :tags-as-categories nil
          :wp-latex t
          :wp-code t
          :track-posts nil)))


;; End org2blog


(defun insert-new-todo (description)
  (interactive "sTODO: ")
  (save-excursion
    ;;    (set-buffer "*scratch*")
    (find-file "~/org/todo.org")
    (org-insert-todo-heading t)
    (insert description)
    (save-buffer)
    (bury-buffer)))
(global-set-key (kbd "C-c t") 'insert-new-todo)


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
    (git-init project-path)
    (message "Project created: %s" project-path)
    (find-file (concat project-path "/README.org"))))

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
;;(global-set-key "\C-cp" 'delicious-post)
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

(add-hook 'identica-mode-hook (lambda () (identica-icon-mode t)))

(defun count-words (&optional begin end)
  "count words between BEGIN and END (region); if no region defined, count words in buffer"
  (interactive "r")
  (let ((b (if mark-active begin (point-min)))
      (e (if mark-active end (point-max))))
    (message "Word count: %s" (how-many "\\w+" b e))))


(require 'printing)
(setq warning-suppress-types nil) ;; workaround compile errors

(require 'auto-complete)

(defvar ac-source-python '((candidates .
		(lambda ()
		  (mapcar '(lambda (completion)
			     (first (last (split-string completion "\\." t))))
			  (python-symbol-completions (python-partial-symbol)))))))


(add-hook 'python-mode-hook
	  (lambda() (setq ac-sources '(ac-source-python))))


;; LaTeX (Auctex) code begins here

(require 'tex)

;;(TeX-global-PDF-mode nil) ;; Nobody uses .dvi files anymore.

;;set xetex mode in tex/latex
(add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))

(add-hook 'LaTeX-mode-hook
          (lambda() (setq TeX-command-default "XeLaTeX")))


;; End Auctex code

;; Make scripts executable:
(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)

(defun mail-web-page-url (url &rest ignore)
      "mail web page using mail-web-page"
      (interactive (browse-url-interactive-arg "URL: "))
      (shell-command (concat "/home/albin/.bin/mail-web-page " url))
      (setq truncate-lines t))

(global-set-key "\C-cum" 'mail-web-page-url)

(server-start)
