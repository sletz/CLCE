;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;;
;;
;;                                  MidiShare-Appl-Interface.lisp
;;
;;                                     © 2001, GRAME.
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
;;  14-06-01, Premiere version. SL
;;  27-06-01, Ajout du code relatif a la version linux (non termine)
;;  23-09-08, Version LispWorks
;;  08-05-10, Pour LispWorks v.6 (suppression de without-interrupts) : remplacement par
;;            mp:allowing-block-interrupts t (DLO)

;; Conservee pour avantage multi-plateforme meme si equivalente a mp:with-interrupts-blocked
;; de LispWorks v.6 (DLO)
(defmacro disable-interrupts (&body body)
  `(mp:allowing-block-interrupts t ,@body))

;;...................................................................: set-appl-alarm
(defun set-appl-alarm (ref callback)
  (declare (ignore ref callback))
  (error "set-appl-alarm not supported")
  ) 	

;;...................................................................: remove-appl-alarm
(defun remove-appl-alarm (ref)
  (declare (ignore ref))
  (error "remove-appl-alarm not supported")
  )     	

(cffi:defcallback call-alarm-int :void ((refnum :short) (code :long))
  (mp:allowing-block-interrupts t
     (if-self refnum
            (if (my applalarm)
              (funcall (my applalarm) self code))
            ())
))

(defvar call-alarm (cffi:callback call-alarm-int))


;;                                   Gestion des taches
;;========================================================================================

;;...................................................................: call-task

(cffi:defcallback call-task-int :void ((date :long) (refnum :short)  (tasknum :long)   (arg2 :long)   (arg3 :long ))
  (declare (ignore date arg2 arg3))
  (mp:allowing-block-interrupts t
   (let ((self (midi-get-lisp-appl refnum))
         task)
     (when self
       (with-slots (tasktbl freetbl) self
         (setq task (aref tasktbl tasknum))
         (unless (numberp task)
           (setf (aref tasktbl tasknum) freetbl)
           (setf freetbl tasknum)
           (apply (car task) (cdr task))))))
))


(defvar call-task (cffi:callback call-task-int))

;;                         Animation des midi-appl en tache de fond
;;========================================================================================

;; essai pour Robert Pascal (YO 19/05/98)
;;---------------------------------------
;; le principe est le suivant : plutot que de mettre les traitements dans un process, 
;; ils sont mis dans la fonction appelee par le system pour savoir s'il faut reveiller le 
;; process. Le process lui meme est donc toujours bloque !!!

(defvar *midishare-process*)

(defun midishare-process ()
  (loop (mp:process-wait "midishare background process" #'process-events-or-tasks-p)))

(defun install-background ()
  (setq *idle-sleep-ticks* 0)
  (setq *foreground-sleep-ticks* 0)
  (setq *background-sleep-ticks* 0)
  (setf *midishare-process* (mp:process-run-function "midishare-process" '(:priority 2) #'midishare-process ))
  )

;;...................................................................: remove-background
(defun remove-background ()
  (if *midishare-process*
    (mp:process-kill *midishare-process*))
  (setf *midishare-process* nil))

