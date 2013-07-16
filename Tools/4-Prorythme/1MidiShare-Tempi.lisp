 
;;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;;
;;
;;                                MidiShare-Tempi.lisp
;;
;;                                     © 1991, GRAME.
;;
;;
;;
;;
;;
;;  Ce fichier ajoute des fonstions de transformation des scores pour tenir compte des
;;  changements de tempo.
;;
;;
;;  CONVENTION D'ECRITURE : 
;;  De même que dans MidiShare toutes les fonctions ont un nom de la forme "midixxyy",
;;  toutes les fonctions définis ici auront un nom de la forme "midi-xx-yy".
;;
;;
;;  HISTORIQUE :
;;  25-01-93, Première version. -Yo-
;;  Version 3 corrigé en septembre 1996 (place-tempo)
;;  Corrigé le 22 12 96 par YO (midi-music2pseudo-time)
;;
;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

;; Commentaires: le 9 octobre 97
;;(eval-when (:compile-toplevel :load-toplevel :execute)
;;  (require :midishare-score))

;; Commentaires: le 9 octobre 97
;;(in-package :midishare)

;;(export '( midi-real2music-time midi-music2real-time convert-tempo  place-tempo))




(defun midi-real2music-time (self &key (quarter 5040) (tempo 500000))
  (let ((sc (my score))
        (t0 0)
        (d0 0)
        (t1 0)
        (d1 0)
        (d2 0))
    (sgobegindate sc 0)
    (do ((e (sforwardread sc) (sforwardread sc)))
        ((%null-ptr-p e))
      (setq t1 (date e))
      (setq d1 (+ d0 (floor (* 1000 quarter (- t1 t0)) tempo)))
      (date e d1)
      (cond ((= (evtype e) typeNote)
             (setq d2 (+ d0 (floor (* 1000 quarter (- (+ t1 (dur e)) t0)) tempo)))
             (dur e (- d2 d1)))
            ((= (evtype e) typeTempo)
             (setq t0 t1)
             (setq d0 d1)
             (setq tempo (tempo e)))))
    (sgobegindate sc 0)))



(defun midi-music2real-time   (self &key (quarter 5040) (tempo 500000))
  (let ((sc (my score))
        (t0 0)
        (d0 0)
        (t1 0)
        (t2 0)
        (d1 0))
    (sgobegindate sc 0)
    (do ((e (sforwardread sc) (sforwardread sc)))
        ((%null-ptr-p e))
      (setq d1 (date e))
      (setq t1 (+ t0 (floor (* tempo (- d1 d0)) (* 1000 quarter))))
      (date e t1)
      (cond ((= (evtype e) typeNote)
             (setq t2 (+ t0 (floor (* tempo (- (+ (dur e) d1) d0)) (* 1000 quarter))))
             (dur e (- t2 t1)))
            ((= (evtype e) typeTempo)
             (setq t0 t1)
             (setq d0 d1)
             (setq tempo (tempo e)))))
    (sgobegindate sc 0)))

(defun midi-music2pseudo-time   (self &key (quarter 5040) (tempo 500000))
  (let ((sc (my score))
        (t0 0)
        (d0 0)
        (t1 0)
        (t2 0)
        (d1 0))
    (sgobegindate sc 0)
    (do ((e (sforwardread sc) (sforwardread sc)))
        ((%null-ptr-p e))
      (setq d1 (date e))
      (setq t1 (+ t0 (floor (* tempo (- d1 d0)) (* 1000 quarter))))
      (date e t1)
      (cond ((= (evtype e) typeNote)
             (setq t2 (+ t0 (floor (* tempo (- (+ (dur e) d1) d0)) (* 1000 quarter))))
             (dur e (- t2 t1)))
            ))
    (sgobegindate sc 0)))







(defun convert-tempo (te)
 (floor 60000000 te) )


;;========================================================================================
;; Ajout de PAJ-TOOLS le 18 Février 2005:
;; Initialisation des variables NOI et *OUT*
;; Création des fonctions PLACE-TEMPO et VAR-TEMPO
;;========================================================================================

(defvar noi 5040)     ;Cette valeur correspond à quarter
                      ;du fichier Midi-Share-Tempi2.lisp
                      ;Elle définie la valeur universelle de la noire
                      
(defvar *out* (new 'midi-score :name "*out*"))    ;sequence de travail



;; *********** Nouveau place-tempo le 18 Février 2005 **************
;; Les paramètres ne sont pas des générateurs
;; dat = date exprimée en noire
;; temp = tempo
;; Exemple:
;;(defun exemple ()
;;  (place-tempo 0 120)
;;  ( prorythme °1 °0 °8 (gnotes)
;;              °0
;;              °(1) (s °(60 61 64 65)) °120)) 
;; Tempo de 120 à la date 0
;; ******************************************************************

(defun place-tempo (date temp &key (sequence *out*))
  (midi-move sequence :date (* noi date))
  (midi-send-im sequence (tempo-change :tempo (convert-tempo temp))))




;; ********************* var-tempo *********************************
;; Nouveauté du 18 Février 2005
;; deb et fin= début et fin de variation de tempo
;; temp: générateur de variation de tempo 
;;
;; Exemple:
;;(defun exemple ()
;;  (var-tempo °0 °8 (i °60 °300))
;;  ( prorythme °1 °0 °8 (gnotes)
;;              °0
;;              °(1) (s °(60 61 64 65)) °120)) 
;; Accelerando de 60 à 120 entre les dates 0 et 8 noires
;; *****************************************************************

(defun var-tempo (deb fin temp &key (sequence *out*))
  (let* ((stime (* (funcall deb 0 0 1 nil) noi))
         (etime (* (funcall fin 0 1 1 nil) noi))
         (ctime stime)
         (reverse nil))
    (midi-move sequence :date stime)
    (midi-send-im sequence (tempo-change :tempo (convert-tempo §temp)))
    (vartempo temp sequence (floor (/ (+ stime etime) 2)) stime etime reverse stime etime)))
         
(defun vartempo (temp sequence ctime stime etime reverse a1 a2)
  (let ((delta (- a2 a1))
        (tempo §temp))
    (if (> delta noi)
      (progn
        (midi-move sequence :date ctime)
        (midi-send-im sequence (tempo-change :tempo (convert-tempo tempo)))
        (vartempo temp sequence (/(+ a2 (* 3 a1)) 4) stime etime reverse a1 (/(+ a1 a2)2))
        (vartempo temp sequence (/(+ a1 (* 3 a2)) 4) stime etime reverse (/(+ a1 a2)2) a2)))))


;;==============================================================================
;;                    UTILITAIRES AVEC MASQUAGE DE SEQUENCE
;;==============================================================================

; Ces fonctions servent à cacher le paramètre séquence à l'utilisateur
; dans le but de simplifier l'approche et du fait que seule la séquence *out*
; soit utilisée.

(defun midiplay (&key dur (date 0) pos (len most-positive-fixnum) sel)
  (midi-play *out* :dur dur :date date :pos pos :len len :sel sel))

(defun midistop ()
  (midi-stop *out*))

(defun midiclear (&key (sel nil) (date 0) pos (dur most-positive-fixnum))
  (midi-clear *out* :sel sel :date date :pos pos :dur dur))

(defun midiprint ( &key (sel nil) (date 0) pos
                        (dur most-positive-fixnum) (len most-positive-fixnum))
  (midi-print *out* :sel sel :date date :pos pos :dur dur :len len))

(defun midiformat (&optional format)
  (midi-set-format *out* format))

(defun midisave (&optional name)
  (midiformat :midifile)
  (midi-save *out* name))

(defun midiload (&optional name)
  (midi-load *out* name))



#|
(midiclear)
(midiplay)
(midistop)
(midiclear)
(midiprint)
(midiprint :len 2)
(midiprint :date 10000)
(player)
(midisave :ask)
(midiload :ask)

|#