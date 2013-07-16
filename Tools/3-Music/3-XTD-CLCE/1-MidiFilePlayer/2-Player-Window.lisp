
;;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;;
;;
;;                                 	Player-Window.lisp
;;
;;                                      2001, GRAME.
;;
;;
;;
;;
;;
;; 
;;
;; HISTORIQUE :
;;
;; petites modifications pour cause de conflit de nom avec d'autres modules :
;;  open-player --> open-mf-player
;; en outre si le nom de fichier n'est pas fourni un dialogue apparait. (YO)
;; 23/07/96 : suppression definition de new, my, slot car definis ailleurs (YO)
;; 06/12/00 : utilisation des points d'entree de la librairie PlayerSharedPPC (SL) 
;; 22-06-01, Renommage de slot en slot1 pour resoudre un confit de nom sous CmuLisp
;; 16/07/01 : Ajout du code relatif a la version CMUCL / linux
;; 16/07/01 : Ajout du code relatif a la version CMUCL/Linux
;; 24/07/03 : Modification de la gestion fenetre : plus de bouton close. Le player est desalloue
;;           lorsque CLCE quitte.
;; 09/02/05 : Adapdation OSX
;; 14/05/09 : Adapdation pour LispWorks

;;--------------------------------------------------------------------------
;; Interface for MCL on MacIntosh
;;--------------------------------------------------------------------------

;;(in-package "CL-USER")

(progn

(defvar *player-state* (MidiNewPlayerState))
(defvar *player-window* nil)
(defvar *player-timer* (mp:make-timer 'update-pos *player-window*))
(defvar *player-pos* (MidiNewPos))
 
;;===============
;; general tools
;;===============

(defun convert-denom (denom)
  (let ((r (- denom 2))
        (res 4))
    (if (> r 0)
      (dotimes (i r) (setq res (* res 2)))
      (dotimes (i (- r)) (setq res (/ res 2))))
      res))

(defun c-tempo (tempo)
  (if (= tempo 0)
      1
      (round (/ (* 60000 1000) tempo))))

;;=======================================================
;; open a MidiFile PLAYER with its an associated dialog
;;=======================================================

(export '(open-mf-player open-msh-player get-mf-player-loop-content set-mf-player-seq msh-seq-duration msh-seq-all-duration))

(defun open-msh-player (name)
  (player-framework)
  (let ((ref (Open-Player name)))
    (midi-connect ref 0 T)
    (SetAllTrackPlayer ref (midi-new-seq) 500)
    ;;(init-player ref name)
    (setq *player-window* (make-instance 'player-interface))
    (capi:display *player-window*)
    ref
    ))
  
(defun close-msh-player (player)
  (ClosePlayer player))

(defun set-mf-player-seq (player seq tpq)
 (SetAllTrackPlayer player seq tpq))

(defun mf-player-stop-seq (player)
 (StopPlayer player))
 
(defun mf-player-start-seq (player)
 (StartPlayer player))

(defun msh-seq-duration ()
  (let (date length)
    (GetStatePlayer *session-mf-player* *player-state*)
    (setq date (s-date *player-state*))
    (GetEndScorePlayer *session-mf-player* *player-state*)
    (setq length (s-date *player-state*))
    (max 0 (- length date))))

(defun msh-seq-all-duration ()
  (GetEndScorePlayer *session-mf-player* *player-state*)
  (s-date *player-state*))
  
;;=======================================================
;; Dialog
;;=======================================================

(defvar *max-chars* 20)

(capi:define-interface player-interface ()
  ()
  (:panes
   (play-button
    capi:push-button
    :text "Start"
    :callback-type :interface
    :callback 'start-callback)
   (stop-button
    capi:push-button
    :text "Stop"
    :callback-type :interface
    :callback 'stop-callback)
   (cont-button
    capi:push-button
    :text "Cont"
    :callback-type :interface
    :callback 'cont-callback)
   (bar-pane
    capi:text-input-pane
    :accessor bar-pane
    :min-height '(character 1)
    :max-width nil)
   (beat-pane
    capi:text-input-pane
    :accessor beat-pane
    :min-height '(character 1)
    :max-width nil)
   (div-pane
    capi:text-input-pane
    :accessor div-pane
    :min-height '(character 1)
    :max-width nil)
   (tempo-pane
    capi:text-input-pane
    :accessor tempo-pane
    :min-height '(character 1)
    :max-width nil
    :enabled :no)
   )
  (:layouts
    (button-layout
     capi:row-layout
     '(play-button stop-button cont-button))
    (pos-layout
     capi:row-layout
     '(bar-pane beat-pane div-pane)
     :title "Position")
    (tempo-layout
     capi:row-layout
     '(tempo-pane)
     :title "Tempo")
     (main-layout
      capi:column-layout
       '(pos-layout tempo-layout button-layout)))
  (:default-initargs
   :title "MidiShare Player"
   :X 100 :Y 50 :VISIBLE-WIDTH 200 :VISIBLE-HEIGHT 300  
   :WIDTH 200 :HEIGHT 100
   :layout 'main-layout))

(defun update-pos (self)
   (GetStatePlayer *session-mf-player* *player-state*)
   (when (= kIdle (s-state *player-state*))
     (stop-timer))
   (let ((curbar (s-bar *player-state*) )
         (curbeat (s-beat *player-state*))
         (curdivision (s-unit *player-state*)))
     (setf (capi:text-input-pane-text (bar-pane self)) (write-to-string curbar)
            (capi:text-input-pane-text (beat-pane self)) (write-to-string curbeat)
            (capi:text-input-pane-text (div-pane self)) (write-to-string curdivision)
            (capi:text-input-pane-text (tempo-pane self)) (write-to-string (c-tempo (s-tempo *player-state*))))))

(defun read-pos (self)
  (GetEndScorePlayer *session-mf-player* *player-state*)
  (let ((curbar (read-from-string (capi:text-input-pane-text (bar-pane self))))
        (curbeat (read-from-string (capi:text-input-pane-text (beat-pane self)))))
     (p-bar *player-pos* (min curbar (s-bar *player-state*)))
     (p-beat *player-pos* curbeat)
     (p-unit *player-pos* 1)
     (SetPosBBUPlayer *session-mf-player*  *player-pos*)))

(defun start-timer ()
  (stop-timer)
  (setq *player-timer* (mp:make-timer 'update-pos *player-window*))
  (mp:schedule-timer-relative *player-timer* 0.1 0.5))

(defun stop-timer()
   (when *player-timer* (mp:unschedule-timer *player-timer*)))

(defun start-callback (&rest args)
  (start-timer)
  (StartPlayer *session-mf-player*))

(defun stop-callback (&rest args)
  (stop-timer)
  (StopPlayer *session-mf-player*))
 
(defun cont-callback (&rest args)
  (start-timer)
  (read-pos *player-window*)
  (ContPlayer *session-mf-player*))

)     ;; End of MCL interface


