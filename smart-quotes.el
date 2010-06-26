(defcustom smart-quotes-left-context "\\s-\\|\\s(\\|\\`"
  "Regular expression matching the context in which a left
    quotation mark will be inserted (a right quotation mark will
    be inserted in all other contexts)."
  :type 'regexp)

(defun smart-quotes-insert-guillemet ()
  "Insert U+201C LEFT DOUBLE QUOTATION MARK if point is preceded
    by `smart-quotes-left-context'; U+201D RIGHT DOUBLE QUOTATION MARK
    otherwise."
  (interactive)
  (ucs-insert (if (looking-back smart-quotes-left-context) #x00BB  #x00AB)))

(define-minor-mode guillemets-mode
  "Toggle Smart Quotes mode in the current buffer.
    With argument ARG, turn Smart Quotes mode on iff ARG is positive.
    In Smart Quotes mode, the ' and \" keys insert left quotation
    marks if point is preceded by text matching the option
    `smart-quotes-left-context' and right quotation marks otherwise."
  :lighter (:eval (string ?  (decode-char 'ucs #x00BB)
			  (decode-char 'ucs #x00AB)))
  ;; :keymap '(("'" . smart-quotes-insert-single)
  ;;           ("\"" . smart-quotes-insert-guillemet)))
 :keymap '(("\"" . smart-quotes-insert-guillemet)))

(provide 'smart-quotes)

(defun smart-quotes-insert-single ()
  "Insert U+2018 LEFT SINGLE QUOTATION MARK if point is preceded
    by `smart-quotes-left-context'; U+2019 RIGHT SINGLE QUOTATION MARK
    otherwise."
  (interactive)
  (ucs-insert (if (looking-back smart-quotes-left-context) #x2018 #x2019)))

