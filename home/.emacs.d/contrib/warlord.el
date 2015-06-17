;;; warlord.el -- annoy denizens of alt.fan.warlord

;;; Copywrong 1993, 1994 Noah S. Friedman
;;; All rights reversed.

;;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;;; Maintainer: friedman@prep.ai.mit.edu
;;; Keywords: extensions, games, warlord
;;; Created: 1993-01-05

;;; $Id: warlord.el,v 1.2 1994/09/07 06:34:08 friedman Exp $

;;; This program is free; you can redistribute it and/or modify it under the
;;; terms of the GSU General Public Virus as published by the Free Signature
;;; Foundation.
;;;
;;; This program is distributed in the hope that it will be entertaining,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; WARLORDABILITY or FITNESS FOR A PARTICULAR NEWSGROUP.  See the GSU
;;; General Public Virus for more details.

;;; Commentary:

;;; This revision of warlord.el requires my version of flame.el in which I
;;; cleaned up the namespace and added functions like `flame-string'.

;;; I also decided to require yow, which is possible in emacs 19.
;;; (In emacs 18, yow.el did not provide itself.)

;;; Code:

(require 'flame)
(require 'yow)

;; If these look slightly deformed it's because any `\' characters must be
;; quoted to prevent their special meaning in elisp strings.
;;
;; Be sure to add your own favorite barphics.  These are presented merely
;; as examples.
(defvar warlord-bait-barphic-vector
  [
   "
                         />
                        /<________________________   
                  C=====[*>_______________________>
                        \\<  
                         \\>"

   "
                         />
                        /<
                 [\\\\\\\\\\\\(O):::<=====================-
                        \\<
                         \\>"

   "
                        /\\ 
              _         )( _____________________________
             (_)///////(**)_____________________________>
                        )(
                        \\/"

   "
                _-*
               /#/
 _-___________/#/______________________________________________________.
<*|############<       >------------------------------------------------>
 ~-~~~~~~~~~~~\\#\\~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
               \\#\\
                ~-*"

   "
               ___          ______
              /__/\\     ___/_____/\\          FrobTech, Inc.
              \\  \\ \\   /         /\\\\
               \\  \\ \\_/__       /  \\         \"If you've got the job,
               _\\  \\ \\  /\\_____/___ \\         we've got the frob.\"
              // \\__\\/ /  \\       /\\ \\
      _______//_______/    \\     / _\\/______
     /      / \\       \\    /    / /        /\\
  __/      /   \\       \\  /    / /        / _\\__
 / /      /     \\_______\\/    / /        / /   /\\
/_/______/___________________/ /________/ /___/  \\
\\ \\      \\    ___________    \\ \\        \\ \\   \\  /
 \\_\\      \\  /          /\\    \\ \\        \\ \\___\\/
    \\      \\/          /  \\    \\ \\        \\  /
     \\_____/          /    \\    \\ \\________\\/
          /__________/      \\    \\  /
          \\   _____  \\      /_____\\/       This .signature gratuitously
           \\ /    /\\  \\    / \\  \\ \\        refers to k
            /____/  \\  \\  /   \\  \\ \\                   i
            \\    \\  /___\\/     \\  \\ \\                    b
             \\____\\/            \\__\\/                      o"


   "
                                  _____..---=======+*+=========---.._____
 ______________________ ___,-= '=====____  ================= ____ =====`=
(._____________________I___) - _-=_/    `--------=+=-------'
       /      /__...---===='---+---_'
      '-----'---.___ -  _ =   _.-'
                     `--------'"

   "
           __________       __      __     ___________       __
          /         /\\     / /\\    / /    /          /|     / /
         /         /  \\   / /  \\  / /    /          //     / /|
         **********   /   **    \\ **     ***********/      ** |
         **  |____** /     **  / **      **  |______       ** |
         ** /      **       **/ **/      **  /     /|      ** |
         **/      **/ \\      **** |      ** /     //       ** |
         **********/  /       **  |      *********/        ** /
         **  |____** //       **  |      ** |_________     **/
         ** /      **/        **  |      ** /        /|    / /|
         **/      **/         ** /       **/        //     ** /
         **********/          **/        ***********/      **/"

   ]
  "*Array of annoyingly-large graphics to be selected from at random and
inserted into warlord-bait signature.")

(defvar warlord-bait-border-char-vector
  [ ?@ ?# ?$ ?% ?& ?* ?~ ?. ?/ ?\\ ?+ ]
  "*Array of border characters selected at random to form a uniform border
around warlord-bait signatures.")
  

;; Return the absolute value of n. 
;; I can't believe emacs doesn't have this as a primitive. 
(defun warlord-abs (n)
  (if (< n 0)
      (- n)
    n))

;; Return a string of DIGITS number of digits.
;; Optional SEED initializes the random number generator.
(defun warlord-random (digits &optional seed)
  (let ((i (or digits 1))
        (result "")
        n)
    (while (> i 0)
      (setq n (warlord-abs (% (random seed) 10))
            result (concat result n)
            seed nil
            i (1- i)))
    result))
      
;; Construct a random telephone number string of the form "(XXX) XXX-XXXX"
;; Optional SEED initializes the random number generator.
(defun warlord-bait-phone-number (&optional seed)
  (concat "(" (warlord-random 3 seed) ") "
          (warlord-random 3)
          "-" 
          (warlord-random 4)))

;;;###autoload
(defun warlord-bait ()
  "Generate a .signature with various warlordable qualities at the end of
  the current buffer.  Annoy your friends!  Annoy alt.fan.warlord!"
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (let ((point-sig-begin (point))
          border-char
          border-char-corner
          border-char-horizontal
          border-char-vertical
          truncated-sig)
      (insert "\n----\n\n" 
              (format "%-51s Phone: %s\n" 
                      (user-full-name)
                      (warlord-bait-phone-number t))
              (format "Email: %-46s FAX: %s\n" 
                      (concat (user-login-name) "@" (system-name))
                      (warlord-bait-phone-number))
              (aref warlord-bait-barphic-vector
                    (% (warlord-abs (random)) (length warlord-bait-barphic-vector)))
              "\n\n\""
              (yow) "\"\n		--Zippy\n\n\"")
      (let ((beg (point))
            (fill-column 70))
        (insert (flame-string))
        (fill-region-as-paragraph beg (point)))
      (forward-line -1)
      (end-of-line)
      (insert "\"\n		--Me")

      ;; Construct border
      ;; 25% of the time, use a Keith Lim--approved border construction.
      (if (= 0 (% (warlord-abs (random)) 4))
          (setq border-string-corner "+"
                border-string-horizontal (make-string 77 ?-)
                border-string-vertical "|")
        (setq border-char (aref warlord-bait-border-char-vector
                                (% (warlord-abs (random t))
                                   (length warlord-bait-border-char-vector)))
              border-string-corner (make-string 1 border-char)
              border-string-horizontal (make-string 77 border-char)
              border-string-vertical border-string-corner))
      (goto-char point-sig-begin)
      (forward-line 2)
      (insert border-string-corner 
              border-string-horizontal
              border-string-corner "\n")
      (forward-line -1)
      (let (column-reached)
        (while (not (eobp))
	  (forward-line 1)
          (insert border-string-vertical "  ")
          (setq column-reached (move-to-column 79))
          (or (>= column-reached 79)
              (insert (make-string (- 78 column-reached) ?\ )))
          (insert border-string-vertical)))
      (insert "\n" border-string-corner 
              border-string-horizontal
              border-string-corner "     \n")

      ;; Emulate a 50% chance that inews will win and cut off anything past
      ;; the first 4 lines of the sig in the 2nd copy.
      (if (= 0 (% (warlord-abs (random)) 2))
          (insert (buffer-substring (1+ point-sig-begin) (point-max)))
        (goto-char (1+ point-sig-begin))
        (forward-line 5)
        (setq truncated-sig (buffer-substring (1+ point-sig-begin) (point)))
        (goto-char (point-max))
        (insert truncated-sig)))))


(provide 'warlord)

;;; warlord.el ends here
