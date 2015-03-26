;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;;
;;
;;                                 	 Tracks-Interface.lisp
;;
;;                                      1994, GRAME.
;;
;;
;;
;;
;;
;;  Interface de base avec les tracks musicales
;;
;; HISTORIQUE :
;;  06-12-94, Reprise de la version utilisee dans Silice 
;;  20-06-01  La fonction trackp est mise dans tracks-interface.lisp 
;;  21-06-01  Les fonctions trkaddev et trkaddat sonts mise dans tracks-tools.lisp 
;;  27-06-01  Ajout du code correspondant a la version linux
;;  30-07-08 : LispWorks version
;;
;;	      
;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/


;;--------------------------------------------------------------------------
;; Interface for MCL on Macintosh
;;--------------------------------------------------------------------------


(progn

(cffi:define-foreign-type track-ptr () ':pointer)

(cffi:defcfun ("TrkNew" trk-new) track-ptr)

(defun TrkNew ()
  (trk-new))

(cffi:defcfun ("TrkFree" trk-free) track-ptr (track track-ptr))

(defun TrkFree (track)
  (trk-free track))

(cffi:defcfun ("TrkClear" trk-clear) track-ptr (track track-ptr))

(defun TrkClear (track)
  (trk-clear track))

(cffi:defcfun ("TrkCopy" trk-copy) track-ptr (track track-ptr))

(defun TrkCopy (track)
  (trk-copy track))

(cffi:defcfun ("TrkResize" trk-resize) :long (track track-ptr) (len :long))

(defun TrkResize (track len)
  (trk-resize track len))

(cffi:defcfun ("TrkGet" trk-get) track-ptr (track track-ptr) (len :long))

(defun TrkGet (track len)
  (trk-get track len))

(cffi:defcfun ("TrkRem" trk-rem) track-ptr (track track-ptr) (len :long))

(defun TrkRem (track len)
  (trk-rem track len))

(cffi:defcfun ("TrkAdd" trk-add) track-ptr (track track-ptr) (len :long) (ev midi-ev-ptr))

(defun TrkAdd (track len ev)
  (trk-add track len ev))

(cffi:defcfun ("TrkAddDate" trk-add-date) track-ptr (track track-ptr) (len :long) (ev midi-ev-ptr))

(defun TrkAddDate (track len ev)
  (trk-add-date track len ev))

(cffi:defcfun ("TrkLen" trk-len) :long (track track-ptr))

(defun TrkLen (track)
  (trk-len track))

(cffi:defcfun ("TrkSize" trk-size) :long (track track-ptr))

(defun TrkSize (track)
  (trk-size track))

(cffi:defcfun ("TrkFind" trk-find) :long (track track-ptr) (len :long))

(defun TrkFind (track len)
  (trk-find track len))

(cffi:defcfun ("Trk2Seq" trk-2seq)  midi-seq-ptr (track track-ptr))

(defun Trk2Seq (track)
  (trk-2seq track))

(cffi:defcfun ("Seq2Trk" seq-2trk) track-ptr (seq midi-seq-ptr))

(defun Seq2Trk (seq)
  (seq-2trk seq))

(defmacro trackp (trk)
  `(typep ,trk 'FLI::POINTER))

)


