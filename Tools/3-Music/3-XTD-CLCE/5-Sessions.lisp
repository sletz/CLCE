;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;;
;;
;;                                  Sessions.lisp
;;
;;                                     © 2001, GRAME.
;;
;;
;;
;;
;;
;; Memorisation des sessions de travail
;;
;; HISTORIQUE :
;; 
;;  06-12-2000 : adaptation a la librairie Player
;;  24-07-2003 : Modification de la gestion fenetre : plus de bonton close, le Pleyer est désallou lorsque CLCE quite.
;;  07-07-04 Utilisation du package MidiShare, evtype à la place de type.
;;
;;
;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/


(use-package :midishare)

(progn 

(defparameter *session-num* 0)
(defparameter *session-output* *standard-output*)

(defun open-session ()
  (multiple-value-bind (ss mn hh jj mo an) (get-decoded-time )
    (declare (ignore hh mn ss))
    (let ((fname (format nil "ccl:music-sessions;~2,'0D-~2,'0D-~2,'0D-session" an mo jj)))
      (setq *session-output* (open fname :direction :output :if-exists :append :if-does-not-exist :create)))))

(defun close-session ()
  (close *session-output*)
  (setq *session-output* *standard-output*))

(defun print-to-session (exp)
  (multiple-value-bind (ss mn hh jj mo an) (get-decoded-time )
    (format *session-output* "~%~%~%~%;==========================================================")
     (format *session-output* "~%; NUM : ~5D, DATE : ~2D/~2D/~2D, TIME : ~2D:~2D:~2D~%"
            *session-num* jj mo an hh mn ss)
    (pprint `(progn 
               (setq *random-state* ,*random-state*)
               ,exp) 
            *session-output*))
  (finish-output *session-output*)
  (incf *session-num*))

(defvar ! nil)
(defvar !! nil)
(defvar !!! nil)

(defvar *session-mf-player* nil)

(defmacro play (exp)
  `(let ((*temp-tracks* nil))
     (print-to-session (list 'play ',exp))
     (when ( < (current-stack-length) 20000)
       (extend-current-stack 1000))
     (def-track !!! !!)
     (def-track !! !)
     (def-track ! ,exp)
     (when (null *session-mf-player*)
       (setq *session-mf-player* (open-msh-player "Lisp_Player")))
     (mf-player-stop-seq *session-mf-player*)
     (set-mf-player-seq *session-mf-player* (track2seq !) 500)
     ;;(mf-player-start-seq *session-mf-player*)
     (start-callback)
     (gc-temp-tracks)))


(defun kill-mf-player ()
  (when *session-mf-player*
    (close-msh-player *session-mf-player*)
    (setq *session-mf-player* nil)))

)    ;; End of MCL interface

;;===========================================================================================
;;
;; INSTALLATION DU MODULE
;; 
;;
;;===========================================================================================

(eval-when (:load-toplevel :execute)
  ;;(add-startup-action #'open-session)
  (add-quit-action #'kill-mf-player)
  ;;(add-quit-action #'close-session)
  ;;(open-session)
)
