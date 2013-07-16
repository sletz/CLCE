;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;;
;;
;;                                  midi-notepad.lisp
;;
;;                                     © 1994, GRAME.
;;
;;
;;
;;
;;
;;  Pour prendre des "notes musicale" et les utiliser par la suite
;;
;; HISTORIQUE :
;;  01-12-94, Premiere version. -Yo-
;;  06-12-94, Reprise du fichier pour nouvelle config de chargement 
;;  13-06-01  Remplacement de %null-ptr-p par nullptrp pour rendre le code independant platforme
;;  25-06-01  Macptr remplace par t pour faciliter le portage Linux
;;
;;
;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/



(defclass midi-note-pad (midi-actor)
  (
   (time-window	:initform 1500 :initarg :time-window)
   (max-seq	:initform 10 :initarg :max-seq)
   (start-date	:initform nil)
   (last-date	:initform nil)
   (cur-seq	:initform (snew))
   (open-notes	:initform 0)
   (seq-count	:initform 0)
   (seq-list	:initform nil)
  )
)


;;                                 Creation et destruction
;;========================================================================================

(defmacro until (c &body b)
  `(loop (if ,c (return)) ,@b))

;;...................................................................: initialize-instance
(defmethod initialize-instance :after ((self midi-note-pad) &rest initargs)
  (declare (ignore initargs))
  (midi-connect clce self t)
  (check-break-task (midi-get-time) clce self)
  )

;;...................................................................: update-instance-for-different-class
(defmethod update-instance-for-different-class :before ((previous midi-note-pad) (self dead) &rest initargs)
  (declare (ignore initargs))
  (dolist (s (slot-value previous 'seq-list)) 
    (sfree s))
  (if (slot-value previous 'cur-seq) 
    (sfree (slot-value previous 'cur-seq))))


;;                   Gestion des transmissions et receptions d'evenements
;;========================================================================================

;;...................................................................: midi-send-im
(defmethod midi-send-im ((self midi-note-pad) (e t))
  (date e (midi-get-time))
  (midi-send self e))


;;...................................................................: midi-send-at
(defmethod midi-send-at ((self midi-note-pad) (e t) (date integer))
  (date e date)
  (midi-send self e))


;;...................................................................: midi-send
(defmethod midi-send ((self midi-note-pad) (e t))
  (if (member (evtype e) '(0 1 2))
    (let ((s (my cur-seq)))
      (when (or (null (my cur-seq)) (nullptrp (my cur-seq))) 
        (error "curseq est null"))
      (unless (my start-date) (my start-date (date e)))
      (my last-date (date e))
      (date e (- (date e) (my start-date)))
      (sgoenddate s (date e))
      (sforwardwrite s e)
      (cond ((and (= (evtype e) typekeyon) (> (vel e) 0))
             (my open-notes (+ 1 (my open-notes))))
            ((or (= (evtype e) typekeyoff) (and (= (evtype e) typekeyon) (= (vel e) 0)))
             (my open-notes (- (my open-notes) 1)))))
    (midi-free-ev e)))

(defmethod check-break ((self midi-note-pad))
  (without-interrupts 
   (when (and (my start-date)
              (> (- (Midi-get-time) (my last-date)) (my time-window))
              (zerop (my open-notes)))
     (if (>= (my seq-count) (my max-seq))
       (progn
         (sfree (car (last (my seq-list))))
         (smatchkeyoff (my cur-seq))
         (my seq-list (cons (my cur-seq) (butlast (my seq-list)))))
       (progn
         (my seq-count (+ 1 (my seq-count)))
         (smatchkeyoff (my cur-seq))
         (my seq-list (cons (my cur-seq) (my seq-list)))))
     (my cur-seq (snew))
     (my start-date nil))))

(defun check-break-task (d master self)
  (unless (is-dead self)
    (check-break self)
    (midi-task #'check-break-task (+ d (my time-window)) master self)))

(defmethod item ((self midi-note-pad) (item fixnum))
  (when (< item (my seq-count))
    (let ((src (nth item (my seq-list)))
          (dst (new-temp-track)))
      (sgobegindate src 0)
      (until (nullptrp (snextev src))
        (add-track  dst (midi-copy-ev (SForwardRead src))))
      dst)))

(defun moy (l)
  (let ((tot 0)
        (n 0))
    (dolist (x l) (incf tot x) (incf n))
    (if l (floor tot n) 0)))


(defun track-notation (trk &optional (stream t))
  (labels ((check-list (l) (if (cdr l) l (car l))))
    (let* ((*max-chord-delta* 50)
           (lchords (track-chords trk))
           (nchords 0)
           (lpitch nil)
           (lvel nil)
           (ldur nil))
      (dolist (chord lchords)
        (incf nchords)
        (let (lp lv ld)
          (dotrack (i e chord)
            (push (dur e) ld)
            (push (pitch e) lp)
            (push (vel e) lv))
          (if (cdr lp) (push (sort lp #'<) lpitch) (push (car lp) lpitch))
          (push (moy lv) lvel)
          (push (moy ld) ldur)))
      (format stream 
              "(play (ct ~S (gn :dur (s ¡~S) :pitch (s ¡~S) :vel (s ¡~S))))"
              nchords (nreverse ldur) (nreverse lpitch) (nreverse lvel))
      )))

(defun track-notation2 (trk &optional (stream t))
  (let* ((*max-chord-delta* 50)
         (lchords (track-chords trk)))
    (format stream "(play (ct ~S (alt " (length lchords))
    (dolist (chord lchords)
      (let (lp lv)
        (dotrack (i e chord)
          (push (pitch e) lp)
          (push (vel e) lv))
        (format stream 
                "(gn :dur ¡~S :pitch (s ¡~S) :vel ¡~S)"
                (trksize chord) 
                (if (cdr lp) (sort lp #'<) (car lp))
                (moy lv))))
    (format stream ")))~%" )))


;; 				Mise en oeuvre simplifiee
;;===========================================================================================

(defvar *notepad*)

(defun rejoue (&optional (n 0))
  (play (item *notepad* n)))

(defun decris (&optional (n 0))
  (track-notation (item *notepad* n)))

;;				Installation et Desinstallation
;;===========================================================================================

(defun install-notepad ()
  (setq *notepad* (new 'midi-note-pad))
  (midi-connect 0 0 t))

(defun remove-notepad ()
  (if *notepad* (free *notepad*))
  (setq *notepad* nil))
  
(eval-when (:load-toplevel :execute)
  (midi-add-ouverture-action #'install-notepad)
  (midi-add-fermeture-action #'remove-notepad)
  (install-notepad)
)
