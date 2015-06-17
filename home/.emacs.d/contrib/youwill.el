;;; youwill.el --- generate meaningless marketing hype

;; Copyright (C) 1984 American Telephone & Telegraph, Inc.

;; Author: Bob Manson <manson@cygnus.com>
;; Maintainer: Noah Friedman <friedman@prep.ai.mit.edu>
;; Keywords: extensions
;; Created: 1997-03-01

;; $Id: youwill.el,v 1.5 1997/03/03 11:36:21 friedman Exp $

;; This software is is guaranteed to do nothing useful, except when it
;; does.  You may sell it, burn it, use it, modify it, or give it away, at
;; your leisure.  You may even require that other people use it.  You may
;; also require that people not use it, as you see fit.  Government
;; agencies are encouraged to integrate this software into weapons control
;; systems and other instruments of destruction.  AT&T YOU WILL!

;;; Commentary:

;; Bob Manson originally wrote this program in ksh.
;; Noah Friedman converted it to emacs lisp.

;;; Code:


(defconst youwill-paragraph
  '(("Have you ever" "*sentence?" "Or" "*sentence?"
     "Or" "*sentence?" "!trailer.")
    ("Have you ever" "*sentence?" "Or" "*sentence?" "!trailer.")))

(defconst youwill-sentence
  '(("*alternatereality" "where" "*discovery")
    ("*thingverb" "*thing" "through the use of" "*psychicactivity")
    ("been called by your" "*relationship" "*place")
    ("been" "*attacked" "by" "*attacker" "*place")
    ("been forced by your" "*bodypart" "to eat" "*vilefood")
    ("been given" "*number" "*pluralthing" "made out of" "*thing")
    ("had" "*number" "*pluralthing" "eat your" "*bodypart")
    ("had" "*thing" "*actionverb" "*place")
    ("had" "*thing" "tell you that" "*discovery")
    ("joined a church where everyone but you can" "*communicate"
     "with your" "*noun")
    ("seen" "*thing" "*graphicverb" "*number" "times in a row")
    ("tasted" "*vilefood" "and thought it was" "*dumbfood")
    ("thought it was" "*value" "to have" "*number" "*pluralthing"
     "to defend against the" "*evilperson")
    ("tried to make" "*thing" "generate" "*psychicactivity")
    ("tried to" "*communicate" "with" "*thing" "*place")
    ("used a conference call to" "*communicate" "with characters from"
     "*inanetvshow")
    ("used" "*vehicle" "to travel" "*place")
    ("wondered if it was possible to have" "*vehicle" "*graphicverb")))

(defconst youwill-trailer
  '(("you will.  And the company that will bring it to you: AT&T")))

(defconst youwill-place
  '(("in a garbage dump")
    ("while being chopped to pieces by a rotor blade")
    ("while being constipated")
    ("while being operated on for club foot")
    ("while drinking Sterno straight from the can")
    ("while driving down the road at 120mph")
    ("while joining in an Iroquois ritual")
    ("while living on a planet where" "*discovery")
    ("while sitting on the toilet")
    ("while sitting quietly with your sexual toy")
    ("while staring at lightbulbs")
    ("while turning yourself inside out")
    ("while visiting" "*planet")
    ("while" "*doingverb" "*dumbfood")
    ("while watching" "*inanetvshow")))

(defconst youwill-inanetvshow
  '(("Barney and Friends")
    ("Iron Chef")
    ("Lamb Chop's Play-Along")
    ("Thundercats")
    ("an AT&T commercial")
    ("Xena")))

(defconst youwill-communicate
  '(("become one") ("psychically link") ("sing") ("talk") ("emote verbally")
    ("hum God Save the Queen madly")))

(defconst youwill-doingverb
  '(("becoming") ("eating") ("abusing") ("calling") ("lifting")
    ("carrying") ("following") ("having sex with") ("throwing")
    ("buying") ("watching") ("operating on")))

(defconst youwill-actionverb
  '(("become pregnant") ("eat your mail") ("give you good advice")
    ("give you pennies") ("hit you") ("kiss you") ("lift weights")
    ("make you feel warm and fuzzy") ("share intimate feelings")
    ("sit next to you on" "*vehicle") ("spew gibberish") ("splode")
    ("write AT&T commercials") ("hurl insults at your" "*relationship")
    ("throw up")))

(defconst youwill-graphicverb
  '(("eat lemons") ("glow")
    ("make annoying phone calls to" "*thing")
    ("show you obscene movies on" "*vehicle")
    ("talking with your" "*bodypart")
    ("throw sparks at you")
    ("turn putrid")))

(defconst youwill-vehicle
  '(("a bus") ("a subway") ("an airplane") ("the toilet") ("your car")))

(defconst youwill-psychicactivity
  '(("ESP") ("bad karma") ("force fields") ("a miasma") ("radio waves")
    ("psychokinetic energy") ("weird AT&T scenarios")))

(defconst youwill-pluralthing
  '(("Ann Arbor Ambassador terminals")
    ("Teletypes(tm)") ("cars") ("cats") ("dogs") ("lanterns")
    ("milk cartons") ("modems") ("telegraphs") ("telephones")
    ("insults") ("empty deodorant cans") ("large bananas")
    ("televisions") ("toilets")))

(defconst youwill-vilefood
  '(("bananas") ("brussel sprouts") ("peaches") ("spinach")))

(defconst youwill-thing
  '(("a small midget") ("an infant") ("large lymph nodes") ("boxes of Borax")
    ("moldy milk") ("your dog") ("your neighbor")
    ("someone you saw at a party") ("your telephone")))

(defconst youwill-dumbfood
  '(("an explosion in your mouth")
    ("broccoli") ("cauliflower") ("sand")
    ("Doritos") ("tasty roadkill")
    ("stuff you found on the ground")))

(defconst youwill-bodypart
  '(("TV") ("Teletype(tm)") ("arm") ("foot") ("leg") ("telephone")
    ("finger") ("mouse") ("head") ("nose") ("ear")
    ("toilet") ("toe") ("fist")))

(defconst youwill-alternatereality
  '(("been transported to an alternate reality")
    ("gotten trapped in a virtual reality")
    ("been forced to play a character on" "*inanetvshow")
    ("gotten dropped on" "*planet" "and shown a play")))

(defconst youwill-thingverb
  '(("bathed") ("burped") ("destroyed") ("eaten") ("excommunicated")
    ("fondled") ("kicked") ("kissed") ("massaged") ("opened up") ("petted")
    ("removed") ("repaired") ("replaced") ("rewired") ("rubbed")
    ("scrubbed") ("turned")))

(defconst youwill-number
  '(("two") ("three") ("four") ("five") ("six") ("seven") ("eight") ("29")))

(defconst youwill-discovery
  '(("your" "*relationship" "was responsible for the"
     "overthrow of Communist Russia")
    ("Michael Jackson is" "*doingverb" "your" "*noun")
    ("Nazi Germany is your country of birth")
    ("several Conspiracy members are following you")
    ("someone thinks of you as Liza Minelli")
    ("the wives are catching colds")
    ("using a" "*noun" "as a sex toy is" "*value")
    ("you are Murphy Brown's child")
    ("you are married")
    ("you lost your faith because your" "*noun" "has" "*alternatereality")
    ("you do not have a pet")
    ("you have two left feet")
    ("you have no room")
    ("your" "*noun" "was caught in a space-time vortex")
    ("you don't have enough mana")
    ("you lost your piglet")
    ("you're originally from" "*planet")
    ("your" "*relationship" "is a" "*evilperson")
    ("your" "*relationship" "is confused")
    ("your long-lost brother is lost")
    ("your" "*relationship" "is" "*planet")
    ("you lost your" "*noun")
    ("you can generate" "*psychicactivity" "without" "*doingverb" "*thing")
    ("your spouse is an alien")))

(defconst youwill-relationship
  '(("child") ("coworker") ("enemy") ("friend") ("mother")
    ("owner") ("parent") ("relative") ("sister") ("slave") ("spouse")))

(defconst youwill-noun
  '(("abode") ("alien") ("anteater") ("cat") ("chicken") ("child")
    ("computer") ("dinoflagellate") ("dog") ("endothermic therapsid")
    ("life insurance") ("piglet") ("rhinocerous") ("sister") ("telephone")
    ("television") ("tetrodotoxin") ("vehicle")))

(defconst youwill-value
  '(("a good idea") ("a positive step in the right direction")
    ("costly") ("futile") ("mysterious") ("quite worthwhile")
    ("unhealthy") ("worthless")))

(defconst youwill-planet
  '(("Mercury") ("Venus") ("Mars") ("Jupiter") ("Saturn") ("Uranus")
    ("Neptune") ("Pluto") ("the Moon")))

(defconst youwill-evilperson
  '(("American") ("Communist") ("Democrat") ("Frenchman") ("German")
    ("Iranian") ("Iraqi") ("Republican") ("Socialist") ("alien from Mars")
    ("alien") ("artist") ("bobbie") ("god-fearing Crustacean")
    ("idiot") ("loon") ("piglet-haters")))

(defconst youwill-attacked
  '(("anally gang raped") ("arrested") ("assaulted")
    ("given lots of drugs") ("hugged warmly") ("jailed")
    ("kissed") ("put up in a nice hotel")
    ("seduced") ("tortured with electricity") ("tortured")))

(defconst youwill-attacker
  '(("*fascistcountry" "*officials")
    ("crazed" "*fascistcountry" "*officials")
    ("irate newspaper salesmen")
    ("my" "*relationship")
    ("the government")
    ("your" "*relationship")))

(defconst youwill-officials
  '(("dog license bureaus") ("government officials") ("net police")
    ("prison camp guards") ("religious fanatics") ("secret police")
    ("traffic cops")))

(defconst youwill-fascistcountry
  '(("American") ("Argentine") ("Australian") ("British") ("Burmese")
    ("Chinese") ("Finnish") ("French") ("Guatemalan") ("Indonesian")
    ("Italian") ("Korean") ("Singaporean") ("Sri Lankan")))


(defun youwill ()
  "Generate a random AT&T commercial.
If called interactively, display the resulting youwill in a buffer."
  (interactive)
  (let ((s (youwill-iterate-list
            (youwill-random-member youwill-paragraph) "youwill-")))
    (and (interactive-p)
         (let ((temp-buffer-show-function temp-buffer-show-function)
               (temp-buffer-show-hook
                (function (lambda ()
                            (fill-paragraph nil)))))

           ;; Play nice with JBW's winning temp-buffer window height
           ;; minimization hacks
           (and (eq temp-buffer-show-function 'show-temp-buffer)
                (setq temp-buffer-show-function
                      (function (lambda (buf)
                                  (save-excursion
                                    (set-buffer buf)
                                    (fill-paragraph nil))
                                  (show-temp-buffer buf)))))
           (with-output-to-temp-buffer "* AT&T YOU WILL *"
             (princ s))))
    s))

;; Return a random member of list A.
(defun youwill-random-member (a)
  (and a
       (listp a)
       (nth (random (length a)) a)))

;; Process entries from list LISTNAME and from RESTOFLIST, handling periods
;; and commas at the end of LISTNAME as needed.
(defun youwill-getlist (listname restoflist prefix)
  (let* ((lastchar (aref listname (1- (length listname))))
         (punct-char-p (memq lastchar '(?. ?, ??)))
         (period-p (memq lastchar '(?. ??)))
         (suffix (if punct-char-p
                     (substring listname 0 -1)
                   listname)))
    (concat (youwill-iterate-list
             (youwill-random-member
              (symbol-value (intern (concat prefix suffix)))) prefix)
            (cond (period-p
                   (concat (char-to-string lastchar) " "))
                  (punct-char-p
                   (char-to-string lastchar))
                  (t ""))
            (if restoflist " " "")
            (youwill-iterate-list restoflist prefix))))

;; Iterate over list A, replacing all strings beginning with a '*' or '!'
;; with a random selection from the appropriate list.
(defun youwill-iterate-list (a prefix)
  (cond ((null a) a)
        ((= (aref (car a) 0) ?*)
         (youwill-getlist (substring (car a) 1) (cdr a) prefix))
        ((= (aref (car a) 0) ?!)
         (youwill-capitalize
          (youwill-getlist (substring (car a) 1) (cdr a) prefix)))
        (t
         (concat (car a)
                 (if (cdr a) " " "")
                 (youwill-iterate-list (cdr a) prefix)))))

(defun youwill-capitalize (a)
  (let ((new (copy-sequence a)))
    (aset new 0 (upcase (aref new 0)))
    new))

(provide 'youwill)

;; youwill.el ends here
