;; Filename: eudcb-notmuch.el
;; Copyright (C) 2010 Jesse Rosenthal
;; Author: Jesse Rosenthal <jrosenthal@jhu.edu>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; USAGE:
;;
;; 1. Put notmuch_addresses.py (available by git clone from
;; http://jkr.acm.jhu.edu/git/notmuch_addresses.git ) into your
;; $PATH. Make sure to read the installation instruction in that file,
;; as it requires some libraries still in development.
;;
;; 2. Put this file into your emacs load path. DON"T RENAME IT. EUDC
;; looks for it by name.
;;
;; 3. Put the following in your .emacs:
;;
;; (eudc-set-server "localhost" 'notmuch t)
;; (setq eudc-server-hotlist '(("localhost" . notmuch)))
;; (setq eudc-inline-expansion-servers 'hotlist)
;;
;; if you wish to use BBDB too (probably first), then something like this:
;; 
;; (eudc-set-server "localhost" 'bbdb t)
;; (eudc-protocol-set 'eudc-inline-expansion-format 
;; 		   '("%s %s <%s>" firstname lastname net)
;; 		   'bbdb)
;; (eudc-set-server "localhost" 'notmuch t)
;; (setq eudc-server-hotlist '(("localhost" . bbdb)
;; 			    ("localhost" . notmuch)))
;; (setq eudc-inline-expansion-servers 'hotlist)


(require 'eudc)

(defvar notmuch-addr-query-command "notmuch_addresses.py")

(eudc-protocol-set 'eudc-query-function 'eudc-notmuch-simple-query-internal
		   'notmuch)

(eudc-protocol-set 'eudc-default-return-attributes
		   '(name email)
		   'notmuch)

(eudc-protocol-set 'eudc-strict-return-matches
		   nil
		   'notmuch)

(eudc-protocol-set 'eudc-query-form-attributes
		   '(name)
		   'notmuch)

(eudc-protocol-set 'eudc-inline-expansion-format 
		   '("%s <%s>" name email)
		   'notmuch)


(defun notmuch-addr-search (query)
  (let* ((output-string (shell-command-to-string (concat
						  notmuch-addr-query-command
						  " "
						  query)))
	 (output-list (split-string output-string "\n" t))
	 (parsed-list (mapcar 'mail-header-parse-address output-list)))
    (mapcar (lambda (cons-cell)
	      (list 
	       (cons 'email (car cons-cell))
	       (cons 'name (cdr cons-cell))))
	    parsed-list)))

(defun eudc-notmuch-simple-query-internal (query &optional return-attrs)
  "Query the notmuch with simple QUERY.  

\'return-attrs\' doesn't do anything right now, but is in there
to maintain compatibility."
  (notmuch-addr-search (cdr (car query))))


