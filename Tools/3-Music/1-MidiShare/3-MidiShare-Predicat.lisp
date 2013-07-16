;;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;;
;;
;;                                  MidiShare-Predicat.lisp
;;
;;                                     © 1991, GRAME.
;;
;;
;;
;;
;;
;;  Ensemble de generateurs de lambda fonctions pour les selections et les regles
;;
;;
;; HISTORIQUE :
;;  04-09-91 Premiere version. -Yo-
;;  06-12-94 Adaptation au nouveau mode de chargement, suppression du package MidiShare 
;;  07-07-04 Utilisation du package MidiShare, type remplacŽ par evtype
;;  26-05-05 Macro val remplace par valint
;;
;;
;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/


(use-package :midishare)

;;                                      meta-macro
;;========================================================================================
 

(defmacro def-is-macro (name ltype lvar)

  (cond ((null ltype)
         `(defmacro ,name (&rest lcond)
            (if (null lcond) 
              #'(lambda (e1 &optional e2 d1 d2 (n 1))
                  (declare (ignore e1 e2 d1 d2))
                  (if (= n 0) 
                    'cont
                    (= n 1)))
              (progn
                (setq lcond (if (> (length lcond) 1) (cons 'and lcond) (car lcond))) 
                (let ((decl (mapcar #'(lambda(v) `(,v (,v e1))) 
                                      (list-var lcond ',lvar))))
                  `(function (lambda (e1 &optional e2 d1 d2 (n 1))
                               (declare (ignore e2 d1 d2))
                               (if (= n 0)
                                 'cont
                                 (and (= n 1)
                                      (let ,decl ,lcond)))) ))))) )
        ((atom ltype)
         `(defmacro ,name (&rest lcond)
              (if (null lcond) 
                #'(lambda (e1 &optional e2 d1 d2 (n 1))
                    (declare (ignore e2 d1 d2))
                    (if (= n 0)
                      'cont
                      (and (= n 1) 
                           (= (evtype e1) ,ltype))))
                (progn
                  (setq lcond (if (> (length lcond) 1) (cons 'and lcond) (car lcond))) 
                  (let ((decl (mapcar #'(lambda(v) `(,v (,v e1))) 
                                      (list-var lcond ',lvar))))
                    `(function (lambda (e1 &optional e2 d1 d2 (n 1))
                                 (declare (ignore e2 d1 d2))
                                 (if (= n 0)
                                   'cont
                                   (and (= n 1)
                                        (= (evtype e1) ,',ltype) 
                                        (let ,decl ,lcond)))) ))))) )

        ((consp ltype)
         (setq ltype (mapcar #'eval ltype))
         `(defmacro ,name (&rest lcond)
              (if (null lcond) 
                #'(lambda (e1 &optional e2 d1 d2 (n 1))
                    (declare (ignore e2 d1 d2))
                    (if (= n 0)
                      'cont
                      (and (= n 1) (member (evtype e1) ',ltype))))
                (progn
                  (setq lcond (if (> (length lcond) 1) (cons 'and lcond) (car lcond))) 
                  (let ((decl (mapcar #'(lambda(v) `(,v (,v e1))) 
                                      (list-var lcond ',lvar))))
                    `(function (lambda (e1 &optional e2 d1 d2 (n 1))
                                 (declare (ignore e2 d1 d2))
                                 (if (= n 0)
                                   'cont
                                   (and (= n 1)
                                        (member (evtype e1) ',',ltype) 
                                        (let ,decl ,lcond)))) )))))) ))         





;;                    macros pour les differentes categories d'evenements
;;========================================================================================
 

;;...................................................................: list-var
(defun list-var (l1 l2 &optional res)
  (if (consp l1) 
    (dolist (v (cdr l1)) (setq res (list-var v l2 res)))
    (if (member l1 l2) (pushnew l1 res)) )
  res)
 

;;...................................................................: SPECIFICATIONS
(def-is-macro is-event () (type chan port date type))

(def-is-macro is-note typeNote (pitch vel dur type chan port date ref))

(def-is-macro is-key-on typeKeyOn (pitch vel type chan port date ref))

(def-is-macro is-key-off typeKeyOff (pitch vel type chan port date ref))

(def-is-macro is-key (typeNote typeKeyOn typeKeyOff) (pitch vel type chan port date ref))

(def-is-macro is-key-press typeKeyPress (pitch kpress type chan port date type))

(def-is-macro is-ctrl-change typeCtrlChange (ctrl valint type chan port date type))

(def-is-macro is-prog-change typeProgChange (pgm type chan port date type))

(def-is-macro is-chan-press typeChanPress (cpress type chan port date type))

(def-is-macro is-pitch-bend typePitchWheel (bend type chan port date type))

(def-is-macro is-song-pos typeSongPos (clknum type chan port date type))

(def-is-macro is-song-sel typeSongSel (song type chan port date type))

(def-is-macro is-prog-change typeProgChange (pgm type chan port date type))

(def-is-macro is-clock typeClock (type chan port date type))

(def-is-macro is-start typeStart (type chan port date type))

(def-is-macro is-stop typeStop (type chan port date type))

(def-is-macro is-cont typeContinue (type chan port date type))

(def-is-macro is-active-sens typeActiveSens (type chan port date type))

(def-is-macro is-tune typeTune (type chan port date type))

(def-is-macro is-reset typeReset (type chan port date type))

(def-is-macro is-sys-ex typeSysEx (fields type chan port date type))

(def-is-macro is-stream typeStream (fields type chan port date type))

(def-is-macro is-qframe typeQFrame (frame-type frame-count type chan port date type))


;;                    macros de composition de predicats 
;;========================================================================================
 
;; ATTENTION : non utilisable comme regles, seulement pour les selections

(defmacro ev-or (&rest lcond)
  `#'(lambda (e) (or ,@(mapcar #'(lambda(c) `(funcall ,c e)) lcond))))

(defmacro ev-and (&rest lcond)
  `#'(lambda (e) (and ,@(mapcar #'(lambda(c) `(funcall ,c e)) lcond))))

(defmacro ev-not (cond)
  `#'(lambda (e) (not (funcall ,cond e))))






