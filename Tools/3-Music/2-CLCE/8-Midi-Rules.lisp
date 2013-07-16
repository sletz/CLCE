;;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;;
;;  midi-rule.lisp
;;
;;  Copyright (c) 1991, GRAME.  All rights reserved.
;;
;;  Rules are actors with recognition behavior
;;
;;
;;  HISTORIQUE :
;;  31-Jul-91 Nouvelle version avec certaines fonctionnalites des Parts (selection ....)
;;
;;            29-Aug-91 SIMPLIFICATION seule la fonction search-rule subiste (recherche d'une regle)
;;            03-Sep-91 adaptation aux scores,disparition de la methode RCV remplacee par MIDI-SEND
;;            mot clef :amount       
;;            06-Sep-91 slot evt-count  (nbr d'evenements en attente)
;;            modification de basic-send remove-tr 

;;  USAGE :  (setq rr (new 'midi-rule :action '((print 'toto))))
;;
;;           (midi-search-rule rr  (is-and (is-seq (is-note (= pitch 60))
;;                                                 (is-note (= pitch 64))
;;                                                 (is-note (= pitch 67))
;;                                         (is-dur :max 1000))))  selectionne une sequence
;;
;;           (midi-record rr)
;;           (midi-stop rr)
;;  12-09-91 Methode de rafraichissement : refresh-midi-window
;;           Methode print-object, cat-rules
;;
;;  17-09-91 recadrage des dates dans Eval-action, initialisation de count dans 
;;           Midi-clear, Midi-clear dans Midi-search-rule -SL-
;;           correction action par defaut
;;  19-09-91 correction de eval-action
;;
;;  05/12/91 print-object : midi-count-evs remplace midi-get-size
;;           sclear s'appelle desormais sclearall (basic-midi-send)
;;           sforwardremove s'appelle desormais sremoveev (remove-tr)
;;
;;  24-02-92 La partie action d'un reconnaisseur est ecrite soit:
;;           - sous la forme d'une fonction compilee sans arguments, par expemple (midi-set-action rule1 #'action1)
;;           - sous la forme d'une liste où la reference à l'objet est faite par le symbole SELF
;;           (on ecrira (midi-set-action rule1 '((print self) (midi-move self :len 1 ) ... etc...)))
;;  05-03-92 Move-to remplace par clce-move-to (conflit avec QuickDraw)
;;           cat-rules appelle cat (midi-actor)
;;  17-02-02 Nettoyage pour insertion dans CVS
;;  07-07-04 Renommage, evtype à la place de type.
;;  17-05-10 Suppression de (declare (ignore-if-unused self)) dans initialize-instance et midi-set-action
;;  28-08-10 declaration IGNORE-IF-UNUSED pas disponible LispWorks => ajoute evaluation bidon du
;;           symbole SELF dans macro-expansions de initialize-instance et midi-set-action, pour
;;           eviter eventuels Warnings (DLO)

;;...................................................................: variables globales
(defvar *midi-active-rule-list* nil)
(defvar *midi-rule-list* nil)      
(defvar *midi-rule-count* 0)
 

;;...................................................................: midi-rule
(defclass midi-rule (midi-score)
  ((rulefct   :documentation "lambda fonction de selection de patterns" 
              :initarg :rule
              :initform  #'(lambda (d1 d2 p1 p2 n) (declare (ignore d1 d2 p1 p2 n)) nil))
   (amount    :documentation "nombre de recherche de pattern"  
              :initform nil)
   (evt-count :documentation "nbr d'evenements en attente"  
              :initform 0 )
   (action    :documentation "action effectuee lorsque la regle matche"  
              :initform nil
              :initarg :action)
))


;;...................................................................
;; evaluation bidon de self pour eviter eventuels Warnings
(defmethod initialize-instance :after ((self midi-rule) &rest initargs)
  (declare (ignore initargs))
  (pushnew self  *midi-rule-list*)
  (incf *midi-rule-count*)
  (cond ((consp (my action))
        (my action (compile nil `(lambda (self) self ,@(my action) )))) ;ici eval. bidon
        ;;(my action (compile nil `(lambda (self) ,@(my action) ))))
        ((functionp (my action))
         (my action (compile nil `(lambda (self)(declare (ignore self)) (funcall ,(my action) ) ))))
        (t (my action #'(lambda (self)  (declare (ignore self)) nil)))))



;;...................................................................: update-instance-for-different-class
(defmethod update-instance-for-different-class :before ((previous midi-rule) (self dead) &rest initargs)
  (declare (ignore initargs))
  (decf *midi-rule-count*)
  (setq *midi-rule-list* (delete self *midi-rule-list* :count 1))
)




;;                   Gestion des transmissions et receptions d'evenements
;;========================================================================================
(defmethod  basic-midi-send ((self midi-rule) )
  (let ((last-ev (sprevev (my score))))
    (incf (my evt-count ))
    (sgobegindate (my score ) 0)
    (multiple-value-bind (res1 res2) 
                         (basic-midi-search-rule self (my rulefct) last-ev (my evt-count ))
      (cond  ((eq res1 t)
              (eval-action self)
              (sclearall (my score)) 
              (my evt-count  0)
              
              (when (numberp (my amount))
                (decf (my amount))
                (when (= (my amount) 0)
                  (midi-stop self))))
             ((null res1 )
              (sclearall (my score))
              (my evt-count  0))
             (t (if res2 (remove-tr self res2)))))))

;;...................................................................: midi-send-im
(defmethod midi-send-im ((self midi-rule) e)
  (call-next-method)
  (basic-midi-send self ))

;;...................................................................: midi-send
(defmethod midi-send ((self midi-rule) e)
  (call-next-method)
  (basic-midi-send self ))

;;...................................................................: midi-send-at
(defmethod midi-send-at ((self midi-rule) e (date integer))
  (call-next-method)
  (basic-midi-send self ))
  
;;...................................................................
(defmethod eval-action ((self midi-rule) )
  (sgobegindate (my score ) 0)
  (ssplice (my score) (nullptr) (date (snextev (my score))) most-positive-fixnum)
  (funcall (my action) self))

       
;; utilitaires
;;-------------

;;...................................................................
(defmethod  remove-tr ((self midi-rule) p) 
  (let ((date (scurdate (my score)))
        (s (my score)))
    (sgobegindate (my score ) 0)
    (do ((cur (snextev s) (snextev s)))
        ((or (nullptrp cur) (eql cur p)))
      (midifreeev (sremoveev s))
      (decf (my evt-count )))
    (sgobegindate (my score)  date)))


;;...................................................................: midi-search-rule
;; applique une nouvelle regle, mot clef amount si on veut preciser le nombre de fois où la regle reste active
(defmethod midi-search-rule ((self midi-rule) rule &key (dur most-positive-fixnum) date  amount)
  (declare (ignore date dur))
  (when rule
    (my amount amount)
    (midi-clear self)
    (my rulefct (is-sub-tr rule))
))
  
;...................................................................: midi-learn-rule
(defmethod midi-learn-rule ((self midi-rule))
  (declare (special sc ))
  (if (eq (my status) :recording)
    (midi-stop self))
  (setq sc (new 'midi-score))
  (midi-record sc :key-off nil)
  (my status :learning))


;;...................................................................: midi-set-action
;; evaluation bidon de self pour eviter eventuels Warnings
(defmethod midi-set-action ((self midi-rule) fun)
  "change la partie action de la rule"
   (if (consp fun)
       (my action (compile nil `(lambda (self) self ,@fun ))) ;ici eval. bidon
       (my action (compile nil `(lambda (self) (declare (ignore self)) (funcall ,fun )))))
)
;;       (my action (compile nil `(lambda (self) ,@fun )))
;;       (my action (compile nil `(lambda (self)(declare (ignore self)) (funcall ,fun ))))))

;;...................................................................: midi-record
(defmethod midi-record ((self midi-rule) &key src dur (key-off :no) (sync :full))
  (declare (ignore sync key-off dur src))
  (if (eq (my status) :learning)
    (midi-stop self))
  (call-next-method)
  (pushnew self *midi-active-rule-list*))

;;...................................................................: midi-stop
(defmethod midi-stop ((self midi-rule))
  (declare (special sc ))
  (when (eq (my status) :learning)
    (my status :idle)
    (midi-stop sc)
    (unless (midi-empty-p sc)
      (midi-search-rule  self (midi-make-rule sc )))
    (free sc))
  (call-next-method)
  (setq *midi-active-rule-list* (delete self *midi-active-rule-list*)))

;;...................................................................: midi-clear
(defmethod midi-clear ((self midi-rule) &key sel date pos dur len dst)
  (declare (ignore sel)(ignore date)(ignore pos)(ignore dur)(ignore len)(ignore l dst))
  (call-next-method)
  (my evt-count 0))

;;...................................................................: midi-stop-all-rule
(defun midi-stop-all-rule ()
  (mapc #'midi-stop  *midi-active-rule-list*))


;                             Recherche de  regles
;;========================================================================================

;; cherche le premier pattern qui matche la regle
;; place la position courante au debut du pattern reconnu
;; rend la duree du pattern

;;...................................................................: midi-search-rule
(defmethod midi-search-rule ((self midi-score) r &key (dur most-positive-fixnum) date  amount)
  (declare (ignore amount))
  (when date (sgobegindate (my score) date))
  (when r
    (multiple-value-bind (lastev count) (midi-search-lastev self dur)
      (let ((res (basic-midi-search-rule self  (is-sub-td r) lastev count)))
        (if (consp res)
          (progn
            (let* ((first (car res))
                   (d1 (date first)))
              (sgobegindate (my score) d1)
              (do ((cur (snextev (my score))(snextev (my score))))
                  ((eql cur first))
                (sforwardread (my score)))
              (caddr res))))))))
 

;;...................................................................
(defmethod basic-midi-search-rule ((self midi-score) r p2 n)
  (let ((p1 (snextev (my score))))
     (unless (nullptrp p1)
      (funcall r  p1 p2 (date p1) (date p2) n))))


;;...................................................................
(defmethod midi-search-lastev ((self midi-score) dur)
  (let* ((s (my score))
         (pos (snextpos s))
         (count 0)
         lastev )
    (sgoenddate s (min most-positive-fixnum (+ (scurdate s) dur)))
    (setq count (+ 1 (- (sprevpos s) pos)))
    (if (= 0 count) 
      (setq lastev (snextev s))
      (setq lastev (sprevev s)))
    (sgobeforepos s (nullptr) pos)
    (values lastev count)))
    
;;                             Synthese de  regles
;;========================================================================================

;;...................................................................
(defun make-key (ty p v)
  (cond ((= ty typenote)
        `(is-note (= pitch ,p) (> vel 0)))
        ((= ty typekeyon)
         (if (> v 0)
           `(is-key-on (= pitch ,p) (> vel 0))
           `(is-key-on (= pitch ,p) (= vel 0))))
        ((= ty typekeyoff)
         `(is-key-off (= pitch ,p) (> vel 0)))
        (t nil)))
  

;;............................
(defun make-note (h)
  `(is-note (= pitch ,h)))

;;.........................................................
(defun make-dur (&key (min 0) (max most-positive-fixnum))
  `(is-dur :min ,min  :max ,max))

;;.........................................................
(defun make-size (&key (min 0) (max most-positive-fixnum))
  `(is-size :min ,min  :max ,max))

;;............................
(defun make-and ( &rest r)
  `(is-and @,r))

;;............................
(defun make-or ( &rest r)
  `(is-or ,@r))

;;............................
(defun make-not ( r)
  `(is-not ,@r))

;;............................
(defun make-seqs ( &rest r)
  `(is-seq ,@r))

;;............................
(defun make-chord (&rest r)
  `(is-and (is-chord ,@r) (is-dur :max 100)))

;; analyse de la sequence et creation du code de la regle
;;---------------------------------------------------------
;; la sequence est decoupees en une sequence d'accords (notes dans une fenêtre temporelle < 100)
;; et de sequence de notes


;;...................................................................: midi-code-rule
(defmethod midi-code-rule  ((self midi-score) &key real-time)
  (unless (midi-empty-p self)
    (let ((sc (new 'midi-score))
          (res t) 
          code)
      (midi-copy self :dst sc :date 0)
      (when real-time (midi-note-to-keyon sc))
      (midi-move sc :date 0)
      (do ( (cur-pos 1 (midi-get-pos sc)))
          ((null res) (progn (free sc)
                             (if (= 1 (length code)) (car (reverse code)) 
                                 (apply #'make-seqs (reverse code)))))
        (setq res (midi-search-rule sc (is-max (is-and (is-size :min 2)
                                                       (is-dur  :max 100)))))
        (let ((next-pos (midi-get-pos sc)))
          (unless (= cur-pos next-pos)
            (midi-move sc :pos cur-pos)
            (setq code (cons (constructor-seq sc (- next-pos cur-pos ))  code))
            (midi-move sc :pos next-pos)))
        (if res
          (progn 
            (setq code (cons (constructor-chord sc res) code))
            (midi-move sc :len res))    ; (midi-forward-date s res)
          (unless (midi-end-p sc)
            (setq code (cons (constructor-seq sc) code))))))))

;;...................................................................: midi-make-rule
(defmethod midi-make-rule ((self midi-score) &key real-time)
  (eval (midi-code-rule self :real-time real-time)))

;;...................................................................
(defun constructor-seq (score &optional (len most-positive-fixnum))
  (let ((s (mapcar #'(lambda (s) 
                       (apply #'make-key s))
                   (extract-list score  '(type pitch  vel) len ))))
    (if (= (length s) 1)
      (car s)
      (apply #'make-seqs  s))))

;;...................................................................
(defun constructor-chord (score &optional (len most-positive-fixnum))
  (let ((s (mapcar #'(lambda (s) 
                       (apply #'make-key s))
                   (extract-list score  '( type pitch  vel) len ))))
    (if (= (length s) 1)
      (car s)
      (apply #'make-chord  s))))


;;...................................................................
(defmethod midi-note-to-keyon ((self midi-score))
  (let ((s (new 'midi-score)))
    (midi-transform self #'(lambda (e)
                             (if (=  0 (evtype e))
                               (progn 
                                 (midi-write-ev s (key-on :pitch (pitch e) :vel (vel e)) :date (date e))
                                 (midi-write-ev s (key-on :pitch (pitch e) :vel 0) :date (+ (date e) (dur e))))
                               (midi-write-ev s e :date (date e)))))
    (midi-copy s :dst self :date 0)
    (free s)
    self))

                              
;; utilitaire d'extraction des parametres
;;---------------------------------------------
(defmethod extract-list ((self midi-score) fun &optional (len most-positive-fixnum) date)
  (let (res1 res2)
    (midi-transform self #'(lambda (e)
                             (dolist (s fun)
                               (setq res1 (cons (eval (list s e)) res1)))
                             (setq res2 (cons (reverse res1) res2))
                             (setq res1 nil))
                    :len len :date date)
    (reverse res2)))


