;;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;;
;;
;;                                 	 Tracks-Rewrite.lisp
;;
;;                                     © 2001, GRAME.
;;
;;
;;
;;
;;
;;  Outils plus evolues pour les tracks musicales
;;
;; HISTORIQUE :
;;
;;
;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/


;;;=======================================================================================
;;; Normalisation d'un pattern : 
;;;	<*x> -> (* x), 
;;;	<(F p1 p2 * p3 p4)> -> (F <p1> <p2> (* <p3>) <p4>)
;;;=======================================================================================

(defun normalize-pattern (p)
  (cond ((null p) 
         (error "pattern vide"))
        ((symbolp p)
         (if (starp p) (list '* (unstar p)) p))
        ((numberp p) 
         p)
        ((listp p)
         (cons (car p) (normalize-list-pattern (cdr p))))))

(defun starp (sym)
  (eq (char (symbol-name sym) 0) #\*))

(defun unstar (sym)
  (intern (string-left-trim "*" (symbol-name sym))))

(defun normalize-list-pattern (lp)
  (cond ((null lp) 
         nil)
        ((eq '* (first lp))
         (cons (list '* (normalize-pattern (second lp)))
               (normalize-list-pattern (cddr lp))))
        (t
         (cons (normalize-pattern (car lp))
               (normalize-list-pattern (cdr lp))))))



;;;=======================================================================================
;;; Liste des variables d'un pattern normalise : 
;;;	<x> 		-> (x), 
;;;	<-> 		-> (), 
;;;	<(* p)> 	-> <p>, 
;;;	<(F p1 ... pn)> -> <p1> u ... u <pn>
;;;=======================================================================================

(defun list-pattern-vars (pat)
  (cond  ((null pat) nil)
         ((eq '- pat) nil)
         ((symbolp pat) (list pat))
         ((listp pat) (reduce #'union (mapcar #'list-pattern-vars (cdr pat))))))


;;;=======================================================================================
;;; Projection et Construction : 
;;;=======================================================================================

(defmacro projection (sym &optional fun)
  (if fun
    `(setf (get ,sym 'projection), fun)
    `(get ,sym 'projection)))

(defmacro construction (sym &optional fun)
  (if fun
    `(setf (get ,sym 'construction) ,fun)
    `(get ,sym 'construction)))



;;;=======================================================================================
;;; Meccanisme de pattern matching : 
;;;=======================================================================================

(defun pattern-bind (pat trk env)
  (if (consp pat) 
    (seq-match (construction (first pat)) 
               (rest pat) 
               (funcall (projection (first pat)) trk (length (rest pat)))
               env)
    (if (and (symbolp pat) (neq '- pat)) 
      (cons (cons pat trk) env)
      env)))

(defun seq-match (f lp lt env)
  (if (null lp)
    env
    (if (and (consp (car lp)) (eq '* (caar lp)))
      (rev-seq-match f (reverse lp) (reverse lt) env)
      (if (null lt)
        (seq-match f (cdr lp) nil (pattern-bind (car lp) (new-temp-track) env))
        (seq-match f (cdr lp) (cdr lt) (pattern-bind (car lp) (car lt) env))))))

(defun rev-seq-match (f lp lt env)
  (if (null (cdr lp))
    (pattern-bind (second (car lp)) (cons f (reverse lt)) env)
    (if (null lt)
      (rev-seq-match f (cdr lp) nil (pattern-bind (car lp) (new-temp-track) env))
      (rev-seq-match f (cdr lp) (cdr lt) (pattern-bind (car lp) (car lt) env)))))

(defun build-pattern-env (pat exp)
  (pattern-bind pat exp nil))

(defun search-env (sym env)
  (let ((b (assoc sym env)))
    (if b
      (cdr b)
      (symbol-value sym))))

(defun translate-track-exp (exp env)
  (cond ((null exp) nil)
        ((numberp exp) exp)
        ((symbolp exp) (search-env exp env))
        ((consp exp) (cons (construction (car exp))
                           (mapcar #'(lambda (e) (translate-track-exp e env)) (cdr exp))))
        (t (error "wrong expression ~S" exp))))

(defun eval-track-exp (exp env)
  (let ((lt (translate-track-exp exp env)))
    (if (consp lt)
      (apply (car lt) (cdr lt))
      lt)))

(defun rwrt (grule gtrack)
  (let (lout env)
  #'(lambda (stime ctime etime reverse)
      (when (null lout)
        (let* ((rule (normalize-list-pattern §grule)))
          (setq env nil)
          (loop
            (when (eq '-> (car rule))
              (setq lout (cdr rule))
              (return))
            (setq env (pattern-bind (pop rule) §gtrack env)))))
      (eval-track-exp (pop lout) env))))
  

(defmacro pattern (pat exp &body forms)
  (let* ((pat (normalize-pattern pat))
         (vars (list-pattern-vars pat))
         (env (gensym)))
    
    `(let ((,env (pattern-bind ',pat ,exp nil)))
       (declare (special ,@vars))
       (progv (mapcar #'car ,env) (mapcar #'cdr ,env)
         ,@forms))))


;;;=======================================================================================
;;; exemples de projections et de constructions
;;;=======================================================================================

(projection 'seq #'track-chords)
(construction 'seq #'seq-tracks)

(projection 'par #'(lambda (trk n) (make-list n :initial-element trk)))
(construction 'par #'par-tracks)

(projection 'epar #'(lambda (trk n) (make-list n :initial-element trk)))
(construction 'epar #'epar-tracks)

(projection 'chan #'track-chans)
(construction 'chan #'chan-tracks)

(projection 'port #'track-ports)
(construction 'port #'port-tracks)

(projection 'key #'track-keys)
(construction 'key #'key-tracks)

(projection 'oct #'track-octs)
(construction 'oct #'oct-tracks)

(projection 'vel #'track-vels)
(construction 'vel #'vel-tracks)




