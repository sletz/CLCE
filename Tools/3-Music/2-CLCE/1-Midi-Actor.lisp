;;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;;
;;
;;                                  MidiShare-Actor.lisp
;;
;;                                     © 1991, GRAME.
;;
;;
;;  Fichier reprend et etend l'ensemble des fonctions de MidiShare liees aux
;;  applications.
;;
;;
;; HISTORIQUE :
;;  13-09-91, classe actor definie dans le fichier midishare-actor.lisp
;;  07-11-91  status devient un slot de actor, midi-get-status definie ici
;;  05-03-92  nouvelle fonction cat : (cat 'midi-score), ... ou (cat t)
;;  ----------------------------------------------------------------------
;;  04-12-94, Adaptation au nouveau mode de chargement, suppression du 
;;	      package MidiShare, simplification.
;;
;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/



;;                              Classe des acteurs MidiShare
;;========================================================================================

(defclass midi-actor ()
  ((Count        :documentation "incremented every time an instance is created" 
                 :initform 0  :type integer    :allocation :class)
   (Order        :documentation "order number of the part / instant value of Count"
                 :reader order)
   (name         :documentation "name of the part"
                 :initarg :name          :initform nil)
   (status       :initform :idle)
   (sources      :initform nil :reader midi-get-sources)
   (destinations :initform nil :reader midi-get-destinations)
   ))

;; Initialize-instance                                        (method) (interne)
;;...................................................................: midi-actor
(defmethod initialize-instance :after ((self midi-actor) &rest initargs)
  (declare (ignore initargs))
  (incf (my Count))
  (my Order (my Count))
  (unless (my name)
    (my name (concatenate 'string 
                       "Untitled-" (write-to-string (my Count)))))
)


;;                                     Gestion des connexions
;;========================================================================================

;;...................................................................: midi-connect
(defmethod midi-connect ((src midi-actor) (dst midi-actor) (state t))
  (if state
    (progn (pushnew src (slot-value dst 'sources))
           (pushnew dst (slot-value src 'destinations)))
    (progn (setf (slot-value dst 'sources) (remove src (slot-value dst 'sources)))
           (setf (slot-value src 'destinations) (remove dst (slot-value src 'destinations)))) ))
           

;;...................................................................: midi-is-connected
(defmethod midi-is-connected ((src midi-actor) (dst midi-actor))
  (member dst (slot-value src 'destinations)))



;; Quelques outils generaux

;;...................................................................: midi-get-name
(defmethod midi-get-name ((self midi-actor))
  (my name))


;;...................................................................: midi-get-status
(defmethod midi-get-status ((self midi-actor))
  (my status))


;;...................................................................: print-date
(defun print-date (univ-date)
    (multiple-value-bind (ss mn hh dd mo yyyy) 
         (decode-universal-time univ-date)
         (format nil  "~2D/~2D/~2D ~2D:~2,'0D:~2,'0D" dd mo yyyy hh mn ss)))





