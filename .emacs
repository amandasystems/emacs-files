(require 'w3m-load)
(autoload 'doc-view "doc-view")

(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/projects/repos/emacsweblogs/lisp/")
(add-to-list 'load-path "~/projects/repos/identica-mode/")
(add-to-list 'load-path "~/projects/repos/lagn/")
(add-to-list 'load-path "~/projects/repos/git-emacs/")
(add-to-list 'load-path "~/projects/repos/erc/")
(add-to-list 'Info-default-directory-list "~/.info/")

(require 'secrets)

;; Save screen real estate, kill some decorations:
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(setq printer-name "laserjet")

;; wanderlust
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

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
(defvar backup-dir "~/.emacs_backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))

;; Indent with spaces in stead.
(setq-default indent-tabs-mode nil) 

;; Copy/paste to xbuffer (for X interaction Zen):
(setq x-select-enable-clipboard t)

(setq-default ispell-program-name "aspell")
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(cal-tex-24 t)
 '(erc-modules (quote (autojoin bbdb button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring stamp track)))
 '(fill-column 80)
 '(flyspell-default-dictionary "sv")
 '(inhibit-startup-screen t)
 '(jabber-auto-reconnect t)
 '(jabber-chat-buffer-format "%n")
 '(jabber-chat-time-format "%H:%M:%S")
 '(jabber-debug-keep-process-buffers t)
 '(jabber-history-enable-rotation t)
 '(jabber-history-enabled t)
 '(jabber-roster-buffer "*jabber*")
 '(jabber-roster-line-format "%c %-25n %u %-8s  %S")
 '(jabber-use-global-history nil)
 '(jabber-vcard-avatars-retrieve t)
 '(muse-project-alist (quote (("WikiPlanner" ("~/plans" :default "index" :major-mode planner-mode :visit-link planner-visit-link)))))
 '(newsticker-html-renderer (quote w3m-region))
 '(scheme-program-name "csi")
 '(term-input-autoexpand (quote input))
 '(tramp-default-user "albin")
 '(w3m-confirm-leaving-secure-page nil)
 '(w3m-default-display-inline-images t)
 '(w3m-key-binding (quote info))
 '(w3m-resize-images nil)
 '(w3m-use-cookies t)
 '(w3m-use-filter nil)
 '(w3m-use-title-buffer-name t)
 '(w3m-use-toolbar nil)
 '(weblogger-server-url "http://luftslott.org/xmlrpc.php")
 '(wl-spam-auto-check-folder-regexp-list (quote (".*"))))

;; Render HTML emails with w3m:
(setq mm-text-html-renderer 'w3m)

;; Code to connect and identify with my ZNC bouncer
;; Todo: need serious amounts of refactoring.

(defun irc-bnc ()
  "Connect to ZNC bouncer via ERC, specified by variable znc-accounts"
  (interactive)
  (dolist (account znc-accounts)
    (let ((account-name (car account))
          (account-plist (cadr account)))
      (cond ((plist-get account-plist 'ssl)
             (erc-ssl
              :server (plist-get account-plist 'hostname)
              :port (plist-get account-plist 'port)
              :nick (plist-get account-plist 'username)
              :password (format "%s:%s" 
                                (plist-get account-plist 'username) 
                                (plist-get account-plist 'password))))
            (t 
             (erc
              :server (plist-get account-plist 'hostname)
              :port (plist-get account-plist 'port)
              :nick (plist-get account-plist 'username)
              :password (format "%s:%s" 
                                (plist-get account-plist 'username) 
                                (plist-get account-plist 'password))))))))
      
      
;; Browse with emacs-w3m:
(setq browse-url-browser-function 'w3m-browse-url
      browse-url-new-window-flag t)

(defun hgr-post ()
  (interactive)
  ;;(browse-url (shell-command-to-string "date +'http://handgranat.org/Redigera/Tussilago/%Y-%m-%d-%H.%M'")))
  (browse-url "http://handgranat.org/posta/Tussilago/"))

;; WORD:
(autoload 'no-word "no-word" "word to txt")
(add-to-list 'auto-mode-alist '("\\.doc\\'" . no-word))


(setq inferior-lisp-program "sbcl") 

(defun erc-to-hgr-region (start end)
  "Format irc logs in region to hgr standard" 
  (interactive "r")
  (save-excursion
    (goto-char (min start end))
    (while (re-search-forward "^<\\([^>]+\\)> " (max start end) t)
      (replace-match ";\\1:" nil nil))))

(setq european-calendar-style t)
(setq calendar-week-start-day 1
      calendar-day-name-array
      ["Söndag" "Måndag" "Tisdag" 
       "Onsdag" "Torsdag" "Fredag" "Lördag"]
      calendar-month-name-array
      ["Januari" "Februari" "Mars" "April"
       "Maj" "Juni" "Juli" "Augusti" "September"
       "Oktober" "November" "December"])
(setq cal-html-directory "~/pandora/public_html/cal")
(setq tex-dvi-print-command "dvips -f * | lp -d laserjet -o media=a4 -o fitplot -")

(defun my-calendar-a4 ()
  "Replace all occurences of 18cm with 17cm."
  (goto-char (point-min))
  (while (search-forward "18cm" nil t)
    (replace-match  "17cm")))

(setq cal-tex-diary t)

(setq cal-tex-preamble-extra "\\usepackage[utf8]{inputenc}\n")


(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "unknown" :family "Terminus"))))
 '(w3m-form-face ((((class color) (background light)) (:foreground "darkblue" :underline t))) t))

(autoload 'scheme-complete "scheme-complete" nil t)

(eval-after-load 'hen
  '(progn (define-key hen-mode-map "\t" 'scheme-complete-or-indent)))

(autoload 'scheme-get-current-symbol-info "scheme-complete" nil t)
(autoload 'scheme-complete-or-indent "scheme-complete" nil t)

(setq auto-mode-alist (cons '("\\.scm$" . hen-mode) auto-mode-alist))

(require 'hen)

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

(setq user-mail-address "albin@eval.nu")
(setq user-full-name "Albin Stjerna")

(autoload 'wikipedia-mode "wikipedia-mode.el"
  "Major mode for editing documents in Wikipedia markup." t)

;; Wordpress hacks follows:
(require 'weblogger)


(require 'jabber)
(setq jabber-chat-fill-long-lines nil)

(eval-after-load 'jabber
  '(jabber-keepalive-start))



(require 'delicious)

(setq auto-mode-alist
   (cons '("\\.mdwn" . markdown-mode) auto-mode-alist))

(require 'tramp)
(tramp-parse-shosts "~/.ssh/known_hosts")

(setq wikipedia-default-language-domain "en")

(bbdb-insinuate-w3)

(add-to-list 'load-path "/usr/local/share/distel/elisp")
(require 'distel)
  (distel-setup)

(autoload 'babel "babel"
  "Use a web translation service to translate the message MSG." t)
(autoload 'babel-region "babel"
  "Use a web translation service to translate the current region." t)
(autoload 'babel-as-string "babel"
  "Use a web translation service to translate MSG, returning a string." t)
(autoload 'babel-buffer "babel"
  "Use a web translation service to translate the current buffer." t)
(require 'smart-quotes)

(defun net-start ()
  "Connect to internet-facing services i.e. IRC and Jabber"
  (interactive)
  (irc-bnc)
  (jabber-connect-all))

;; How do we stop ERC?
(defun net-stop ()
  "Disconnect from internet-facing services."
  (interactive)
  (jabber-disconnect)
  (erc-cmd-GQUIT "quitting from IRC"))

(defun stan ()
  (interactive)
  (print (shell-command-to-string "/home/albin/.bin/slclip --from 'byns gård' --to slussen")))

;;; Identi.ca mode
(require 'identica-mode)
(setq identica-username "tuss")


;;;;;;;;;;;;;
;; Tinyurl ;;
;;;;;;;;;;;;;
(require 'mm-url)
(defun get-tinyurl ()
"Grabs the url at point and echos the equivalent tinyurl in the
minibuffer to ease cutting and pasting."
  (interactive)
  (let* ((long-url (thing-at-point 'url))
         (tinyurl
          (save-excursion
            (with-temp-buffer
              (mm-url-insert
               (concat "http://tinyurl.com/api-create.php?url=" long-url))
              (kill-ring-save (point-min) (point-max))
              (buffer-string)))))
    (message tinyurl)))

(require 'keybindings)
(require 'hooks)
(require 'git-emacs)