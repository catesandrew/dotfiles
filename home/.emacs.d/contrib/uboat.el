;;; uboat.el --- generate u-boat-death messages, patterned after Iron Coffins

;; Author: Bob Manson <manson@cygnus.com>
;; Maintainer: Noah Friedman <friedman@prep.ai.mit.edu>
;; Keywords: extensions
;; Created: 1995-08-02

;; $Id: uboat.el,v 1.2 1997/02/14 16:20:24 friedman Exp $

;; Bozoup(P) 1995 The Bozo(tic) Softwar(e) Founda(t)ion, Inc.
;; See the BOZO Antipasto for further information.
;; If this is useful to you, may you forever be blessed by the Holy Lord
;; Patty.  AT&T you will.

;;; Commentary:

;; Bob Manson originally wrote this program in ksh,
;; then later converted it to emacs lisp.
;; Mark Welch contributed additional items to the dictionary.
;; Noah Friedman rewrote parts of the engine.

;;; Code:

(defconst uboat-message
  '(("*verb" "BY" "*device." "*sinkage.")
    ("*device." "*verb." "*sinkage.")
    ("*verb" "BY" "*device." "*condition." "*sinkage.")
    ("*dropdeviced" "BY" "*number" "*ships." "*sinkage.")
    ("*verb" "BY" "*device" "*position." "*sinkage.")
    ("*verb" "BY" "*device." "*dropdevices." "*sinkage.")))

(defconst uboat-sinkage
  '(("SINKING") ("SINKING") ("SINKING") ("SINKING") ("SINKING") ("SINKING")
    ("ENGAGING" "*remedy") ("EXPLODING") ("LEAVE BOAT")))

(defconst uboat-remedy
  '(("VALIUM NOZZLE")
    ("PLOT DEVICE")))

(defconst uboat-verb
  '(("ATTACKED") ("BOMBED") ("TORPEDOED") ("ANNOYED")))

(defconst uboat-device
  '(("AIRCRAFT") ("CORVETTE") ("ATOMIC BOMB") ("DESTROYER")
    ("SALESMEN") ("*adjective" "GNATS") ("IRC SERVER")
    ("NETSCAPE") ("SPACE-TIME VORTEX")))

(defconst uboat-adjective
  '(("TINY") ("SMALL") ("ENORMOUS")))

(defconst uboat-number
  '(("TWO") ("THREE") ("FOUR") ("FIVE") ("EIGHT") ("HUNDREDS OF")
    ("THOUSANDS OF") ("MANY")))

(defconst uboat-dropdevices
  '(("DEPTH CHARGES") ("TORPEDOS") ("WEB PAGES")))

(defconst uboat-dropdeviced
  '(("DEPTH CHARGED") ("TORPEDOED") ("BLOWN UP") ("CLICKED")))

(defconst uboat-ships
  '(("CORVETTES") ("CARRIERS") ("AIRPLANES") ("DESTROYERS")
    ("ATOMIC BOMBS") ("BROKEN IRC SERVERS") ("CRASHING WEB BROWSERS")))

(defconst uboat-condition
  '(("UNABLE TO" "*unable-verb") ("CREW UNMOTIVATED") ("CAPTAIN INTOXICATED")))

(defconst uboat-unable-verb
  '(("DIVE") ("SURFACE") ("STAY AFLOAT") ("USE RADIO")))

(defconst uboat-position
  '(("57W 24N") ("12W 23N") ("87E 19S") ("12N 12W") ("29W 32S") ("14E 33N")
    ("122W 41N")))


(defun uboat-death-message ()
  "Returns a u-boat-death message, patterned after messages in Iron Coffins."
  (interactive)
  (let ((s (concat (uboat-iterate-list (uboat-random-member uboat-message)
                                       "uboat-")
                   " U-" (int-to-string (random 999)) ".")))
    (and (interactive-p)
         (message "%s" s))
    s))

;; Return a random member of list A.
(defun uboat-random-member (a)
  (and a
       (listp a)
       (nth (mod (random) (length a)) a)))

;; Process entries from list LISTNAME and from RESTOFLIST, handling periods
;; and commas at the end of LISTNAME as needed.
(defun uboat-getlist (listname restoflist prefix)
  (let* ((lastchar (aref listname (1- (length listname))))
         (period-p (= lastchar ?.))
         (suffix (if period-p
                     (substring listname 0 -1)
                   listname)))
    (concat (uboat-iterate-list
             (uboat-random-member
              (symbol-value (intern (concat prefix suffix)))) prefix)
            (if period-p (char-to-string lastchar) "")
            (if restoflist " " "")
            (uboat-iterate-list restoflist prefix))))

;; Iterate over list A, replacing all strings beginning with a '*' or '!'
;; with a random selection from the appropriate list.
(defun uboat-iterate-list (a prefix)
  (cond ((null a) a)
        ((= (aref (car a) 0) ?*)
         (uboat-getlist (substring (car a) 1) (cdr a) prefix))
        ((= (aref (car a) 0) ?!)
         (uboat-capitalize
          (uboat-getlist (substring (car a) 1) (cdr a) prefix)))
        (t
         (concat (car a)
                 (if (cdr a) " " "")
                 (uboat-iterate-list (cdr a) prefix)))))

(defun uboat-capitalize (a)
  (let ((new (copy-sequence a)))
    (aset new 0 (upcase (aref new 0)))
    new))

(provide 'uboat)

;; uboat.el ends here
