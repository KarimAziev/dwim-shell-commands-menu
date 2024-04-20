;;; dwim-shell-commands-menu.el --- Transient menu for dwim-shell-commands -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/dwim-shell-commands-menu
;; Version: 0.1.0
;; Keywords: tools
;; Package-Requires: ((emacs "29.1") (dwim-shell-command "0.61") (transient "0.6.0"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Transient menu for dwim-shell-commands

;;; Code:

(require 'dwim-shell-command)
(require 'dwim-shell-commands)
(require 'transient)

(defun dwim-shell-commands-menu--shared-start (s1 s2)
  "Find common prefix of strings S1 and S2.

Argument S1 is a string to compare.

Argument S2 is another string to compare against S1."
  (declare (pure t)
           (side-effect-free t))
  (let ((search-length (min (length s1)
                            (length s2)))
        (i 0))
    (while (and (< i search-length)
                (= (aref s1 i)
                   (aref s2 i)))
      (setq i (1+ i)))
    (substring s1 0 i)))

(defun dwim-shell-commands-menu--capitalize-variants (word)
  "Generate capitalized variants of WORD.

Argument WORD is a string to generate capitalized variants from."
  (let ((cands)
        (parts (split-string word "" t)))
    (dotimes (i (length parts))
      (let ((val (string-join (remove nil (list
                                           (when (> i 0)
                                             (string-join (seq-take parts i) ""))
                                           (upcase (nth i parts))
                                           (string-join (seq-drop parts (1+ i))
                                                        "")))
                              "")))
        (push val
              cands)))
    (reverse cands)))

(defun dwim-shell-commands-menu--safe-substring (len word)
  "Extract a substring without properties up to length LEN.

Argument LEN is the maximum number of characters to include in the substring.

Argument WORD is the string from which the substring is extracted."
  (if (> (length word) len)
      (substring-no-properties word 0 len)
    word))

(defun dwim-shell-commands-menu--get-all-key-strategies (word len)
  "Generate key strategies from a given WORD and length.

Argument WORD is a string to be processed.

Argument LEN is an integer representing the desired length of the output
strings."
  (let* ((parts (append (split-string word "[^a-z]" t)
                        (list (replace-regexp-in-string "[^a-z]" "" word))))
         (parts-len (length parts))
         (finalize (lambda (short)
                     (while (> len (length short))
                       (setq short (concat short (number-to-string (random 10)))))
                     (dwim-shell-commands-menu--safe-substring len short)))
         (vars
          (mapcar finalize (dwim-shell-commands-menu--capitalize-variants
                            (dwim-shell-commands-menu--safe-substring len
                                                                      (replace-regexp-in-string
                                                                       "[^a-z]"
                                                                       ""
                                                                       word))))))
    (seq-sort-by
     (lambda (it)
       (cond ((string-match-p "[0-9]" it)
              -2)
             ((member it vars)
              -1)
             (t (length (dwim-shell-commands-menu--shared-start word it)))))
     #'>
     (seq-uniq (append
                vars
                (mapcar
                 (lambda (n)
                   (funcall finalize (mapconcat
                                      (apply-partially
                                       #'dwim-shell-commands-menu--safe-substring
                                       n)
                                      parts)))
                 (number-sequence 1 (min len parts-len)))
                (mapcar
                 (lambda (n)
                   (funcall finalize (mapconcat
                                      (apply-partially
                                       #'dwim-shell-commands-menu--safe-substring
                                       n)
                                      (reverse parts))))
                 (number-sequence 1 (min len parts-len))))))))

(defun dwim-shell-commands-menu--generate-shortcuts (items &optional key-fn
                                                           value-fn used-keys)
  "Generate shortcuts for ITEMS using optional KEY-FN and VALUE-FN.

Argument ITEMS is a list of items to generate shortcuts for.

Optional argument KEY-FN is a function that takes an item and returns a string
to be used as the key.

Optional argument VALUE-FN is a function that takes a key and a value, and
returns a new value to be associated with the key.

Optional argument USED-KEYS is a list of strings representing keys that are
already in use and should not be generated again."
  (let* ((value-fn (or value-fn (lambda (key value)
                                  (if (proper-list-p value)
                                      (append (list key) value)
                                    (cons key value)))))
         (total (length items))
         (random-variants (append
                           (mapcar #'char-to-string
                                   (number-sequence (string-to-char
                                                     "a")
                                                    (string-to-char
                                                     "z")))
                           (mapcar #'char-to-string
                                   (number-sequence (string-to-char
                                                     "A")
                                                    (string-to-char
                                                     "Z")))))
         (min-len
          (let ((max-used-key-len (length (car (seq-sort-by #'length #'>
                                                            used-keys)))))
            (max 1 max-used-key-len (ceiling (log total (length
                                                         random-variants)))))))
    (let ((shortcuts used-keys)
          (used-words '())
          (all-keys (mapcar (lambda (def)
                              (if key-fn
                                  (funcall key-fn def)
                                (if (symbolp def)
                                    (symbol-name def)
                                  def)))
                            items))
          (result))
      (dotimes (i (length items))
        (when-let* ((def (nth i items))
                    (word (if key-fn
                              (funcall key-fn def)
                            (if (symbolp def)
                                (symbol-name def)
                              def))))
          (when (not (member word used-words))
            (push word used-words)
            (let ((short
                   (downcase
                    (substring-no-properties word 0
                                             (min min-len
                                                  (length word))))))
              (setq short (replace-regexp-in-string "[^a-z]" "" short))
              (setq short
                    (seq-find
                     (lambda (it)
                       (not
                        (seq-find (apply-partially
                                   #'string-prefix-p it)
                                  shortcuts)))
                     (append
                      (dwim-shell-commands-menu--get-all-key-strategies word
                                                                        min-len)
                      (when (= min-len 1)
                        (or (seq-remove (lambda (key)
                                          (seq-find (apply-partially
                                                     #'string-prefix-p
                                                     (downcase key))
                                                    all-keys))
                                        random-variants)
                            random-variants)))))
              (while (and
                      (< (length short) min-len))
                (setq short (concat short (number-to-string (random 10)))))
              (push short shortcuts)
              (push
               (cond ((functionp value-fn)
                      (funcall value-fn short def))
                     (t (cons short def)))
               result)))))
      (reverse result))))

(defun dwim-shell-commands-menu--get-all-commands ()
  "Collect all dwim-shell-commands not specific to menus or macOS/swift."
  (let ((prefix "dwim-shell-commands-"))
    (let ((vals)
          (prefix-len (length prefix))
          (macosp (eq system-type 'darwin)))
      (mapatoms
       (lambda (sym)
         (when-let* ((value
                      (ignore-errors
                        (commandp
                         (symbol-function
                          sym))))
                     (name (symbol-name sym)))
           (when (and (string-prefix-p prefix name)
                      (not (string-prefix-p "dwim-shell-commands-menu"
                                            name))
                      (or macosp
                          (and (not (string-match-p "-macos-" name))
                               (not (string-match-p "-swift-" name)))))
             (push (list (substring-no-properties name prefix-len) sym)
                   vals)))))
      vals)))

(defvar dwim-shell-commands-menu--cmds nil
  "List of shell commands for DWIM (Do What I Mean) functionality.")

(defun dwim-shell-commands-menu--drop-last-while (pred lst)
  "Remove elements from end of LIST while PRED is true, then return updated LIST.

Argument PRED is a predicate function to test each element.

Argument LST is the list to process."
  (nreverse (seq-drop-while pred (reverse lst))))

(defun dwim-shell-commands-menu--split-options (options)
  "Split OPTIONS into thirds, removing trailing strings from each part.

Argument OPTIONS is a list of options to be split."
  (mapcar
   (apply-partially #'dwim-shell-commands-menu--drop-last-while #'stringp)
   (seq-split options (/ (length options) 3))))

(defun dwim-shell-commands-menu--get-groupped-commands ()
  "Group and sort dwim-shell-commands for menu display."
  (let* ((items
          (dwim-shell-commands-menu--generate-shortcuts
           (seq-sort-by #'car #'string>
                        (dwim-shell-commands-menu--get-all-commands))
           'car))
         (all-items
          (seq-reduce (lambda (acc it)
                        (let* ((descr (car (split-string (cadr
                                                          it)
                                                         "-" t)))
                               (prev (car acc))
                               (prev-str (and (listp prev)
                                              (cadr prev)
                                              (car (split-string
                                                    (cadr
                                                     prev)
                                                    "-" t))))
                               (enabled (and prev-str
                                             (not (string-prefix-p
                                                   prev-str
                                                   descr)))))
                          (if (not enabled)
                              (progn
                                (setq acc (push it acc)))
                            (let ((prevs (seq-take-while #'listp
                                                         acc)))
                              (when (length> prevs 0)
                                (mapc
                                 (lambda (it)
                                   (let* ((tail (cdr it))
                                          (str
                                           (replace-regexp-in-string
                                            "^[-]+"
                                            ""
                                            (substring-no-properties
                                             (car
                                              tail)
                                             (length
                                              prev-str)))))
                                     (setcdr it
                                             (list (if (string-empty-p
                                                        str)
                                                       (car tail)
                                                     str)
                                                   (cadr tail)))))
                                 prevs)
                                (setq acc (push prev-str acc))))
                            (setq acc (push "" acc))
                            (setq acc (push it acc)))))
                      items '()))
         (groupped (seq-split all-items
                              (/ (length all-items) 4))))
    (nreverse (seq-reduce
               (lambda (acc it)
                 (let* ((prev (car acc))
                        (prev-tail (car (last prev))))
                   (when (stringp prev-tail)
                     (push prev-tail it)
                     (nbutlast prev 1))
                   (setq acc (push it acc))))
               groupped '()))))

;;;###autoload (autoload 'dwim-shell-commands-menu "dwim-shell-commands-menu" nil t)
(transient-define-prefix dwim-shell-commands-menu ()
  "Display a menu of shell commands grouped into columns."
  [[:setup-children
    (lambda (_args)
      (transient-parse-suffixes
       (oref transient--prefix command)
       (nth 0 dwim-shell-commands-menu--cmds)))
    :class transient-column]
   [:setup-children
    (lambda (_args)
      (transient-parse-suffixes
       (oref transient--prefix command)
       (nth 1 dwim-shell-commands-menu--cmds)))
    :class transient-column]
   [:setup-children
    (lambda (_args)
      (transient-parse-suffixes
       (oref transient--prefix command)
       (nth 2 dwim-shell-commands-menu--cmds)))
    :class transient-column]
   [:setup-children
    (lambda (_args)
      (transient-parse-suffixes
       (oref transient--prefix command)
       (nth 3 dwim-shell-commands-menu--cmds)))
    :class transient-column]
   [:setup-children
    (lambda (_args)
      (transient-parse-suffixes
       (oref transient--prefix command)
       (nth 4 dwim-shell-commands-menu--cmds)))
    :class transient-column]]
  (interactive)
  (setq dwim-shell-commands-menu--cmds
        (dwim-shell-commands-menu--get-groupped-commands))
  (transient-setup #'dwim-shell-commands-menu))

(provide 'dwim-shell-commands-menu)
;;; dwim-shell-commands-menu.el ends here
