;;; diacriticalize.el --- constructive diacriticism

;; Copyright (C) 2002 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Keywords: extensions
;; Created: 2002-04-27

;; $Id: diacriticalize.el,v 1.1 2002/04/28 04:05:34 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; These commands let one pepper regions of text with gratuitous
;; diacritical marks (and remove them).  There is no particularly good
;; reason for this, it's just for fun.

;; This package currently only works with the ISO-8859-1 character set.

;; Updates of this program may be available via the URL
;; http://www.splode.com/~friedman/software/emacs-lisp/

;;; Code:

(defvar diacriticalize-char-alist
  '((?A   . [?\300 ?\301 ?\302 ?\303 ?\304 ?\305])
    (?a   . [?\340 ?\341 ?\342 ?\343 ?\344 ?\345])
    ;;("AE" . [?\306])
    ;;("ae" . [?\346])
    (?B   . [?\337])
    (?C   . [?\307])
    (?c   . [?\347])
    (?D   . [?\320])
    (?d   . [?\360])
    (?E   . [?\310 ?\311 ?\312 ?\313])
    (?e   . [?\350 ?\351 ?\352 ?\353])
    (?I   . [?\314 ?\315 ?\316 ?\317])
    (?i   . [?\354 ?\355 ?\356 ?\357])
    (?N   . [?\321])
    (?n   . [?\361])
    (?O   . [?\322 ?\323 ?\324 ?\325 ?\326 ?\330])
    (?o   . [?\362 ?\363 ?\364 ?\365 ?\366 ?\370])
    (?P   . [?\336])
    (?p   . [?\376])
    (?U   . [?\331 ?\332 ?\333 ?\334])
    (?u   . [?\371 ?\372 ?\373 ?\374])
    (?Y   . [?\335 ?\245])
    (?y   . [?\375 ?\377])))

(defalias 'diacriticalize-char-to-int
  (if (fboundp 'char-to-int)
      'char-to-int
    'identity))

;; Vector of vectors.  Each slot in this table corresponds to a 7-bit ascii
;; character; the values in the vector at that position represent possible
;; alternative characters to use in replacements.
(defvar diacriticalize-char-table
  (let ((tbl (make-vector 128 nil))
        (map diacriticalize-char-alist))
    (while map
      (aset tbl (diacriticalize-char-to-int (car (car map))) (cdr (car map)))
      (setq map (cdr map)))
    tbl))

(defvar diacriticalize-char-table
  (let ((tbl (make-vector 128 nil))
        (map diacriticalize-char-alist))
    (while map
      (aset tbl (diacriticalize-char-to-int (car (car map))) (cdr (car map)))
      (setq map (cdr map)))
    tbl))

(defvar diacriticalize-inverse-char-table
  (let ((tbl (make-vector 256 nil))
        (map diacriticalize-char-alist)
        c i vec)
    (while map
      (let ((c   (car (car map)))
            (vec (cdr (car map)))
            (i   0))
        (setq map (cdr map))
        (while (< i (length vec))
          (aset tbl (diacriticalize-char-to-int (aref vec i)) c)
          (setq i (1+ i)))))
    tbl))

;;;###autoload
(defun diacriticalize-markup-region (beg end)
  "Put random diacritical marks on characters in region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let (tbl c)
        (while (not (eobp))
          (setq c (diacriticalize-char-to-int (char-after (point))))
          (cond ((and (< c (length diacriticalize-char-table))
                      (setq tbl (aref diacriticalize-char-table c)))
                 (insert (aref tbl (random (length tbl))))
                 (delete-char 1))
                (t (forward-char 1))))))))

;;;###autoload
(defun diacriticalize-unmarkup-region (beg end)
  "Remove random diacritical marks from characters in region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let (c r)
        (while (not (eobp))
          (setq c (diacriticalize-char-to-int (char-after (point))))
          (cond ((and (< c (length diacriticalize-inverse-char-table))
                      (setq r (aref diacriticalize-inverse-char-table c)))
                 (insert r)
                 (delete-char 1))
                (t (forward-char 1))))))))

;;;###autoload
(defun diacriticalize-remarkup-region (beg end)
  "Put random diacritical marks on characters in region, even if already marked up."
  (interactive "r")
  (diacriticalize-unmarkup-region beg end)
  (diacriticalize-markup-region beg end))

(provide 'diacriticalize)

;;; diacriticalize.el ends here
