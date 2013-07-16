;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;;
;;
;;                                  Midi-Score-Interface.lisp
;;
;;                                     © 2001, GRAME.
;;
;;
;;
;;
;;
;;  Fichier definissant les objets score, des sequences ˆ la MidiLogo, parties dependantes Macintosh
;;
;;
;; HISTORIQUE :
;;  14-06-01, Premiere version. SL
;;  27-06-01, Ajout du code correspondant a la version linux
;;  29/07/08 : LispWorks version

;;--------------------------------------------------------------------------
;; Interface for MCL on MacIntosh
;;--------------------------------------------------------------------------

(progn

(cffi:define-foreign-type score-ptr () ':pointer)

(cffi:defcfun ("SClearAll" s-clear-all) :void (score score-ptr))

(defun SClearAll (score)
  (s-clear-all score))

(cffi:defcfun ("SFree" s-free) :void (score score-ptr))

(defun SFree (score)
  (s-free score))

(cffi:defcfun ("SNew" s-new) score-ptr)

(defun SNew ()
  (s-new))

;;...................................................................: information

(cffi:defcfun ("SCurDate" s-cur-date) :long (score score-ptr))

(defun SCurDate (score)
  (s-cur-date score))

(cffi:defcfun ("SPrevPos" s-prev-pos) :long (score score-ptr))

(defun SPrevPos (score)
  (s-prev-pos score))

(cffi:defcfun ("SNextPos" s-next-pos) :long (score score-ptr))

(defun SNextPos (score)
  (s-next-pos score))

(cffi:defcfun ("SPrevEv" s-prev-ev)  midi-ev-ptr (score score-ptr))

(defun SPrevEv (score)
  (s-prev-ev score))

(cffi:defcfun ("SNextEv" s-next-ev)  midi-ev-ptr (score score-ptr))

(defun SNextEv (score)
  (s-next-ev score))

;...................................................................: autres informations

(cffi:define-foreign-type sel-fun-ptr () ':pointer)
(cffi:define-foreign-type trs-fun-ptr () ':pointer)

(cffi:defcfun ("SSize" s-size) :long (score score-ptr) (sel sel-fun-ptr) (dur :long) (len :long))

(defun SSize (score sel dur len)
 (s-size score sel dur len))

;; A VERIFIER

(cffi:defcfun ("SDuration" s-duration) :long (score score-ptr) (sel sel-fun-ptr) (dur :long) (len :long) (a :long))

(defun SDuration (score sel dur len a)
 (s-duration score sel dur len a))

;;...................................................................: deplacements absolus

(cffi:defcfun ("SGoBeginDate" s-go-begin-date) :long (score score-ptr) (len :long))

(defun SGoBegindate (score len)
 (s-go-begin-date score len))

(cffi:defcfun ("SGoEndDate" s-go-end-date) :long (score score-ptr) (len :long))

(defun SGoEnddate (score len)
 (s-go-end-date score len))

(cffi:defcfun ("SGoBeforePos" s-go-before-pos) :long (score score-ptr) (sel sel-fun-ptr) (len :long))

(defun SGoBeforePos (score sel len)
 (s-go-before-pos score sel len))

(cffi:defcfun ("SGoAfterPos" s-go-after-pos) :long (score score-ptr) (len :long))

(defun SGoAfterPos (score len)
 (s-go-after-pos score len))

(cffi:defcfun ("SMoveAtDate" s-move-at-date) :long (score score-ptr) (sel sel-fun-ptr) (len :long))

(defun SMoveAtDate (score sel len)
 (s-move-at-date score sel len))

;;...................................................................: modifications

(cffi:defcfun ("SForwardRead" s-forward-read)  midi-ev-ptr (score score-ptr))

(defun SForwardRead (score)
 (s-forward-read score))

(cffi:defcfun ("SBackwardRead" s-backward-read)  midi-ev-ptr (score score-ptr))

(defun SBackwardRead (score)
 (s-backward-read score))

(cffi:defcfun ("SForwardWrite" s-forward-write) :long (score score-ptr) (ev midi-ev-ptr))

(defun SForwardWrite (score ev )
 (s-forward-write score ev))

(cffi:defcfun ("SBackwardWrite" s-backward-write) :long (score score-ptr) (ev midi-ev-ptr))

(defun SBackwardWrite (score ev)
 (s-backward-write score ev))

(cffi:defcfun ("SRemoveEv" s-remove-ev)  midi-ev-ptr (score score-ptr))

(defun SRemoveEv (score)
 (s-remove-ev score))

;;...................................................................: couper, copier,...

(cffi:defcfun ("SCopy" s-copy) :long (score score-ptr) (destscore score-ptr) (sel sel-fun-ptr) (dur :long) (len :long))

(defun SCopy (score dest sel dur len)
  (s-copy score dest sel dur len))

(cffi:defcfun ("SCut" s-cut) :long (score score-ptr) (destscore score-ptr) (sel sel-fun-ptr) (dur :long) (len :long))

(defun SCut (score dest sel dur len)
 (s-cut score dest sel dur len))

(cffi:defcfun ("SSplice" s-splice) :long (score score-ptr) (destscore score-ptr) (dur :long) (len :long))

(defun SSplice (score dest dur len)
 (s-splice score dest dur len))

(cffi:defcfun ("SMix" s-mix) :long (score score-ptr) (dest score-ptr) (sel sel-fun-ptr) (trs trs-fun-ptr)  (dur :long) (len :long))

(defun SMix (score dest sel trs dur len)
 (s-mix score dest sel trs dur len))

(cffi:defcfun ("SInsert" s-insert) :long (score score-ptr) (dest score-ptr) (sel sel-fun-ptr) (trs trs-fun-ptr)  (dur :long) (len :long))

(defun SInsert (score dest sel trs dur len)
 (s-insert score dest sel trs dur len))

;;...................................................................: transformations musicales

(cffi:defcfun ("STransform" s-transform) :long (score score-ptr) (sel sel-fun-ptr) (trs trs-fun-ptr)  (dur :long) (len :long))

(defun STransform (score sel trs dur len)
 (s-transform score sel trs dur len))

(cffi:defcfun ("SSort" s-sort) :void (score score-ptr))

(defun SSort (score)
 (s-sort score))

(cffi:defcfun ("SMatchKeyOff" s-match-keyoff) :void  (score score-ptr))

(defun SMatchKeyOff (score)
 (s-match-keyoff score))

;;...................................................................: c-select

(defvar *selection*)

(cffi:defcallback c-select-int :long ((ev midi-ev-ptr))
  (restart-case
      (handler-bind ((error
                      #'(lambda (c)
                          (declare (ignore c))
                          (invoke-restart 'callback-error-exit))))   
        (if (or (null *selection*) (funcall *selection* ev)) 1 0))
    (callback-error-exit () 
      (format
       t "~&Caught error under c-select callback~%")
      (values))))

(defvar c-select (cffi:callback c-select-int))

;;...................................................................: c-transform

(defvar *transformation*)

(cffi:defcallback c-transform-int :void ((ev midi-ev-ptr))
  (restart-case
      (handler-bind ((error
                      #'(lambda (c)
                          (declare (ignore c))
                          (invoke-restart 'callback-error-exit))))
        
        (funcall *transformation* ev))
    (callback-error-exit () 
      (format
       t "~&Caught error under c-transform callback~%")
      (values))))

(defvar c-transform (cffi:callback c-transform-int))

)

