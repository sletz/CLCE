;;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;;
;;
;;                                  MidiShare-Appl.lisp
;;
;;                                     © 1991, GRAME.
;;
;;
;;
;;
;;
;;  Fichier reprend et etend l'ensemble des fonctions de MidiShare liees aux
;;  applications.
;;
;;  CONVENTION D'ECRITURE : 
;;  De même que dans MidiShare toutes les fonctions ont un nom de la forme "midixxyy",
;;  toutes les fonctions definis ici auront un nom de la forme "midi-xx-yy".
;;
;; HISTORIQUE :
;;  01-09-91, Premiere version. -Yo-
;;  12-09-91, appel de la methode refresh-midi-window
;;  13-09-91, classe actor definie dans le fichier midishare-actor.lisp
;;  01-11-91, macros after & at     
;;
;;ß 05/12/91, midi-count-evs : ajout des keywords (sel date pos dur len),
;;            pour être compatible avec la methode des scores
;;            midi-get-ev prend les keywords (sel date pos dur len)
;;  03/02/92  methode midi-clear pour vider le buffer = midi-flush-ev
;;  05-03-92  move-to remplace par clce-move-to (conflit avec QuickDraw)
;;            cat-appls appelle cat (midi-actor)
;;  ----------------------------------------------------------------------
;;  04-12-94, Adaptation au nouveau mode de chargement, suppression du 
;;	      package MidiShare, simplification.
;;  22-06-01, Renommage de slot en slot1 pour resoudre un confit de nom sous CmuLisp
;;  25-06-01  Macptr remplace par t pour faciliter le portage Linux
;;            midi-xxx-fermeture-action et midi-xxx-ouverture-action changees en functions
;;  16-07-01  without remplace par une macro multi-platforme : disable-interrupts
;;
;;
;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/



;;                             packages, importations et exportations
;;========================================================================================


;;...................................................................: variables globales
(defvar *midi-max-appls* 128)
(defvar *midi-appls-count* 0)
(defvar *midi-opened-appls* (make-array *midi-max-appls* :initial-element nil))
(defvar *midi-appls-list* nil)


;;...................................................................: macros utiles
(defmacro tbl-appl (n)
  `(svref *midi-opened-appls* ,n))

(defmacro if-self (refnum then else)
  `(let ((self (tbl-appl ,refnum)))
     (if self ,then ,else)))


;;                              Classe des applications MidiShare
;;========================================================================================


;;...................................................................: midi-appl
(defclass midi-appl (midi-actor)
  (
   (refnum   :initform -1)
   (filter   :initform (nullptr))
   (rcvalarm :initform nil :initarg :rcv-alarm)
   (tasktbl  :initform (make-task-tbl 256))
   (freetbl  :initform 0)
   (applalarm :initform nil)
   ))


(defmethod midi-set-appl-alarm ((self midi-appl) alarm)
  (if alarm 
    (progn 
      (my applalarm alarm)
      (set-appl-alarm  (my refnum) call-alarm))
    (remove-appl-alarm (my refnum)))
  (my applalarm alarm))


(defmethod midi-get-appl-alarm ((self midi-appl) )
  (my applalarm))


;;...................................................................: initialize-instance
(defmethod initialize-instance :after ((self midi-appl) &rest initargs)
  (declare (ignore initargs))
  (pushnew self *midi-appls-list*))


;;...................................................................: update-instance-for-different-class
(defmethod update-instance-for-different-class :before ((previous midi-appl) (self dead) &rest initargs)
  (declare (ignore initargs))
  (disable-interrupts
   (setq *midi-appls-list* (delete self *midi-appls-list* :count 1))
   (midi-close previous)
   ))


;;...................................................................: print-object
(defmethod print-object ((self midi-appl) (out stream))
  (if (< (my refnum) 0)
    (format out 
            "<MIDI-APPL~2A ~15A  :state closed>" 
            (my order)
            (my name))
    (format out 
            "<MIDI-APPL~2A ~15A  :state open     :refnum ~D.>" 
            (my order)
            (my name)
            (my refnum))))



;;                                   Gestion generale
;;========================================================================================


;; Utilitaires pour la gestion des midi-appl
;;------------------------------------------

;;...................................................................: midi-get-lisp-appl
(defmethod midi-get-lisp-appl ((refnum integer))
  (tbl-appl refnum))


;;...................................................................: midi-get-lisp-appl
(defmethod midi-get-lisp-appl ((self midi-appl))
  self)

;;...................................................................: midi-list-lisp-appl
(defun midi-list-lisp-appl ()
  (let (l)
    (dotimes (i *midi-max-appls* (nreverse l))
      (if (midi-get-lisp-appl i) (push (aref *midi-opened-appls* i) l)))))

;;...................................................................: midi-get-ind-appl
(defmethod midi-get-ind-lisp-appl ((index integer))
  (let ((refnum (midigetindappl index)))
    (if-self refnum
             self
             refnum)))

;;...................................................................: midi-get-named-appl
(defmethod midi-get-named-lisp-appl ((name string))
  (let ((refnum (midi-get-named-appl name)))
    (if-self refnum
             self
             refnum)))


;;...................................................................: midi-get-refnum
(defmethod midi-get-refnum ((self midi-appl))
  (my refnum))


;;                                     Gestion du nom
;;========================================================================================

;;...................................................................: midi-set-name
(defmethod midi-set-name ((self midi-appl) (name string))
  (my name name)
  (when (> (my refnum) 0)
    (midi-set-name (my refnum) name)))



;;                                     Gestion du filtre
;;========================================================================================

;;...................................................................: midi-get-filter
(defmethod midi-get-filter ((self midi-appl))
  (my filter))


;;...................................................................: midi-set-filter
(defmethod midi-set-filter ((self midi-appl) (filter t))
  (my filter filter)
  (when (> (my refnum) 0)
    (midisetfilter (my refnum) filter)))


;;...................................................................: midi-modify-filter
(defmethod midi-modify-filter ((self midi-appl) &key accept chan port type)
  (midi-modify-filter (my filter) :accept accept :chan chan :port port :type type))



;;                                Gestion des alarmes de reception
;;========================================================================================

;;...................................................................: midi-set-rcv-alarm
(defmethod midi-set-rcv-alarm ((self midi-appl) (foo function))
  (my rcvalarm foo))


;;...................................................................: midi-get-rcv-alarm
(defmethod midi-get-rcv-alarm ((self midi-appl))
  (my rcvalarm))



;;                   Gestion des transmissions et receptions d'evenements
;;========================================================================================

;;...................................................................: midi-send-im
(defmethod midi-send-im ((self midi-appl) (ev t))
  (if (> (my refnum) 0)
    (midi-send-im (my refnum)  ev)
    (midi-free-ev ev)))


;;...................................................................: midi-send
(defmethod midi-send ((self midi-appl) (ev t))
  (if (> (my refnum) 0)
    (midi-send (my refnum)  ev)
    (midi-free-ev ev)))


;;...................................................................: midi-send-at
(defmethod midi-send-at ((self midi-appl) (ev t) (date integer))
  (if (> (my refnum) 0)
    (midi-send-at (my refnum)  ev date)
    (midi-free-ev ev)))


;;...................................................................: midi-count-evs
(defmethod midi-count-evs ((self midi-appl) &key sel date pos dur len)
  ; ajout des keyword, pour être compatible avec la methode des scores
  (declare (ignore sel date pos dur len))
  (if (> (my refnum) 0)
    (midi-count-evs (my refnum))
    0))


;;...................................................................: midi-get-ev
(defmethod midi-get-ev ((self midi-appl) &key sel date pos len dur)
  ;ignore les mots cles
  (declare (ignore sel date pos len dur))
  (if (> (my refnum) 0)
    (midi-get-ev (my refnum))
    (nullptr)))


;;...................................................................: midi-avail-ev
(defmethod midi-avail-ev ((self midi-appl))
  (if (> (my refnum) 0)
    (midi-avail-ev (my refnum))
    (nullptr)))


;;...................................................................: midi-flush-evs
(defmethod midi-flush-evs ((self midi-appl))
  (midi-flush-evs (my refnum)))


;;                                   Gestion des tâches
;;========================================================================================

;;...................................................................: make-task-tbl
(defun make-task-tbl (n)
  (let ((tbl (make-array n)))
    (dotimes (i n)
      (setf (aref tbl i) (+ i 1)))
    (setf (aref tbl (- n 1)) nil)
    tbl))

;;...................................................................: midi-init-tasktbl
(defmethod midi-init-tasktbl ((self midi-appl))
  (let ((tbl (slot-value self 'tasktbl)))
    (dotimes (i (array-total-size tbl))
      (setf (aref tbl i) (+ i 1)))
    (setf (aref tbl (- (array-total-size tbl) 1)) nil)
    (setf (slot-value self 'freetbl) 0)))
      

;;...................................................................: midi-funcall
(defmethod midi-funcall ((self midi-appl) (date integer) &rest exp)
  (disable-interrupts
   (with-slots (tasktbl freetbl refnum) self
     (if freetbl
       (let ((tasknum freetbl))
         (setf freetbl (aref tasktbl tasknum))
         (setf (aref tasktbl tasknum) exp)
         (mididtask call-task date refnum tasknum 0 0))
       (error "Not enough room for task in ~S" self)))))


;;...................................................................: midi-call
(defmethod midi-call ((foo function) (date integer) (self midi-appl) &rest largs)
  (disable-interrupts
   (with-slots (tasktbl freetbl refnum) self
     (if freetbl
       (let ((tasknum freetbl))
         (setf freetbl (aref tasktbl tasknum))
         (setf (aref tasktbl tasknum) (cons foo (cons date (cons self largs))))
         (mididtask call-task date refnum tasknum 0 0))
       (error "Not enough room for task in ~S" self)))))


;;...................................................................: midi-task
(defmethod midi-task ((foo function) (date integer) (self midi-appl) &rest largs)
  (disable-interrupts
   (with-slots (tasktbl freetbl refnum) self
     (if freetbl
       (let ((tasknum freetbl))
         (setf freetbl (aref tasktbl tasknum))
         (setf (aref tasktbl tasknum) (cons foo (cons date (cons self largs))))
         (mididtask call-task date refnum tasknum 0 0))
       (error "Not enough room for task in ~S" self)))))


;;...................................................................: midi-dtask
(defmethod midi-dtask ((foo function) (date integer) (self midi-appl) &rest largs)
  (disable-interrupts
   (with-slots (tasktbl freetbl refnum) self
     (if freetbl
       (let ((tasknum freetbl))
         (setf freetbl (aref tasktbl tasknum))
         (setf (aref tasktbl tasknum) (cons foo (cons date (cons self largs))))
         (mididtask call-task date refnum tasknum 0 0))
       (error "Not enough room for task in ~S" self)))))


;;...................................................................: midi-forget-task
(defmethod midi-forget-task ((ev t))
  (disable-interrupts
   (let ((refnum (ref ev))
         (tasknum (midigetfield ev 1))
         self)
     (midiforgettask ev)
     (if (setq self (midi-get-lisp-appl refnum))
       (with-slots (tasktbl freetbl) self
         (unless (numberp (aref tasktbl tasknum))
           (setf (aref tasktbl tasknum) freetbl)
           (setf freetbl tasknum)))) )))


;;...................................................................: midi-count-dtasks
(defmethod midi-count-dtasks ((self midi-appl))
  (midicountdtasks (my refnum)))


;;...................................................................: midi-exec1-dtask
(defmethod midi-exec1-dtask ((self midi-appl))
  (midiexec1dtask (my refnum)))


;;...................................................................: midi-flush-dtasks
(defmethod midi-flush-dtasks ((self midi-appl))
  (midiflushdtasks (my refnum)))

;;                         Animation des midi-appl en tâche de fond
;;========================================================================================


;;...................................................................: midi-background
(defmethod midi-background ((self midi-appl))
  (dotimes (i (midi-count-dtasks self))
    (midi-exec1-dtask self))
  (if (> (midi-count-evs self) 0) 
    (if (my RcvAlarm)
      (funcall (my RcvAlarm) self)
      (midi-flush-evs self)) ))


;;...................................................................: midi-distribute-evs
(defmethod midi-distribute-evs ((self midi-appl))
  (let ((dl (my destinations)))
    (if dl
      (do ((e (midi-get-ev self) (midi-get-ev self)))
          ((nullptrp e))
        (dolist (d (cdr dl))
          (midi-send d (midi-copy-ev e)))
        (midi-send (car dl) e))
      (midi-flush-evs self))))


(defun process-events-or-tasks-p ()
  (mapc #'midi-background  *midi-appls-list*)
  nil)


;;                             Ouverture et fermeture des midi-appl
;;========================================================================================


;;...................................................................: variables globales
(defvar *midi-ouverture-actions* nil)
(defvar *midi-fermeture-actions* nil)


;;...................................................................: midi-add-fermeture-action
(defun midi-add-fermeture-action (fun)
  (pushnew fun *midi-fermeture-actions*))


;;...................................................................: midi-rem-fermeture-action
(defun midi-rem-fermeture-action (fun)
  (setq *midi-fermeture-actions* (remove fun *midi-fermeture-actions*)))


;;...................................................................: midi-add-ouverture-action
(defun midi-add-ouverture-action (fun)
  (pushnew fun *midi-ouverture-actions*))


;;...................................................................: midi-rem-ouverture-action
(defun midi-rem-ouverture-action (fun)
  (setq *midi-ouverture-actions* (remove fun *midi-ouverture-actions*)))


;;...................................................................: midi-open
(defmethod midi-open ((self midi-appl))
  (unless (> (my refnum) 0)
    (my refnum (midi-open (my name)))
    (cond ((> (my refnum) 0)
           (midi-set-filter (my refnum) (my filter))
           (setf (aref *midi-opened-appls* (my refnum)) self)
           (incf *midi-appls-count*)
           (if (= *midi-appls-count* 1) (mapc #'funcall (reverse *midi-ouverture-actions*)))
           self)
          (t
           (error "MidiShare failed to open ~S" self))) ))


;;...................................................................: midi-close
(defmethod midi-close ((self midi-appl))
  (unless (< (my refnum) 0)
    (disable-interrupts 
     (if (= *midi-appls-count* 1) (mapc #'funcall *midi-fermeture-actions*))
     (setf (aref *midi-opened-appls* (my refnum)) nil)
     (decf *midi-appls-count*)
     (midiclose (my refnum))
     (my refnum -1)))
  self)


;;...................................................................: midi-connect
(defmethod midi-connect ((src midi-appl) (dst integer) (state t))
  (midi-connect (slot1 src refnum) dst state))

(defmethod midi-connect ((src midi-appl) (dst midi-appl) (state t))
  (midi-connect (slot1 src refnum) (slot1 dst refnum) state))

(defmethod midi-connect ((src integer) (dst midi-appl) (state t))
  (midi-connect src (slot1 dst refnum) state))


;;...................................................................: midi-is-connected
(defmethod midi-is-connected ((src midi-appl) (dst integer))
  (midi-is-connected (slot1 src refnum) dst))

(defmethod midi-is-connected ((src midi-appl) (dst midi-appl))
  (midi-is-connected (slot1 src refnum) (slot1 dst refnum)))

(defmethod midi-is-connected ((src integer) (dst midi-appl))
  (midi-is-connected src (slot1 dst refnum)))


;;...................................................................: midi-close-all
(defun midi-close-all ()
  (mapc #'midi-close (midi-list-lisp-appl)))


;;...................................................................: after
(defmacro after (date &body func)
  `(midi-funcall clce (now ,date) #'(lambda nil ,@func)))

;;...................................................................: at
(defmacro at (date &body func)
  `(midi-funcall clce ,date #'(lambda nil ,@func)))


;;                             Installation et desinstallation
;;========================================================================================

(eval-when (:load-toplevel :execute)
  ;(midi-add-ouverture-action #'install-background)
  (midi-add-fermeture-action #'remove-background)
  (add-quit-action #'midi-close-all)
)

;;                             Creation de CLCE
;;========================================================================================

(defvar clce)

(defun install-clce ()
  (midishare:midishare-framework) ;; load mishare
  (clce-framework) ;; load CLCE
  (setq clce (new 'midi-appl  :name "clce" :rcv-alarm #'midi-distribute-evs))
  (midi-set-filter clce (midi-new-filter :chan t :port t :type t))
  (midi-modify-filter clce :accept nil :type (list typesysex typeActiveSens))
  (midi-open clce)
  (midi-connect clce 0 t)
  (midi-connect 0 clce t)
)

(defun remove-clce ()
  (midi-close clce)
  (midi-free-filter (midi-get-filter clce))
  (midi-set-filter clce (nullptr))
  )

(eval-when (:load-toplevel :execute)
  (add-startup-action #'install-clce)
  (add-quit-action #'remove-clce)
  (install-clce)
)
  

