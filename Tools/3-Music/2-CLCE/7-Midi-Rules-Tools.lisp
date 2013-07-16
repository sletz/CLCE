;;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;;
;;  rulestools.lisp
;;
;;  Copyright (c) 1991, GRAME.  All rights reserved.
;;
;;  This file contains functions for CLCE user
;;
;;  History :
;;   
;;
;;     21-Jun-91 implementation des regles
;;     24-Jun-91 Adaptation MCL2.0 (pb avec les macptr)
;;               correction isseq2 (bug pour une seq avec 1 ev)
;;     25-Jun-91 issub rend cont
;;              
;;     2-Jui-91 version allegee
;;
;;     fonctions : isev isdur issize isand isor isnot isseq ischord issub
;;
;;     5-Jui-91 utilisation des mecanismes d'union, d'intersection, de difference de selection
;;     31-Jul-91 correction bug dans ISNOT 
;;     02-Aug-91 nouveau combinateur de regle ISSERIE
;;     28-Aug-91 simplification : les selections sont constituees de segments, la regle ISCHORD
;;               reconnait les accords de regles strict sans chevauchement (les selections sont contigues)
;;     30-Aug-91 fonction ISSEARCH et ISSEARCH-INT pour avoir une comportement identique en temps reel et en temps differe
;;     03-Sep-91 version adaptee aux objets score 
;;     04-Sep-91 disparition de ISEV : macro IS-NOTE IS-KEY-ON ...., ISDUR devient IS-DUR.... essai de suppression des selections
;;     05-Sep-91 les regles rendent t en cas de succes (et non plus 'ok)
;;     06-Sep-91 optimisation de IS-SUB-TR et IS-SEQ2
;;     09-Sep-91 correction IS-SUB-TR 
;;     11-Sep-91 combinateur IS-MAX qui recherche le segment de longueur maximum matchant une regle
;;     24-02-92 is-sub-td rend la longueur du segment -SL-
;;     17-02-02 nettoyage pour insertion dans CVS
;;     11-05-10 protection contre les pointeurs null dans is-sub-tr, correction de is-max

;; PRINCIPE : les regles rendent - 'fail si le segment passe ne matche pas la regle
;;                               - 'cont si le segment passe 'commence' ˆ matcher
;;                               - une selection (liste de couples de ptr) lorsque la regle matche
;;                               - la selection vide est representee par (nil)
         


;;-------------
;; utilitaires
;;-------------

;; rend t si p1 < p2
;;-------------------

(defun inf (p1 p2)
  (if (eql p1 p2)
    nil
    (unless (nullptrp p1)
      (let ((d1 (date p1))
            (d2 (date p2)))
        (if (= d1 d2)
          (do ((pj p1 (link pj)))
              ((or (nullptrp pj)
                   (> (date pj) d1))
               nil)
            (if (eql pj p2)
              (return t)))
          (< d1 d2))))))


;;----------------
;; regles simples
;;----------------

;; si la regle matche, la selection est le segment entier

(defun is-dur (&key (min 0) (max most-positive-fixnum))
  #'(lambda (p1 p2 d1 d2 n)
      (declare (ignore p1 p2))

     ; (PRINT 'DUR1) (PRINT (LIST D1 D2 N)) 
      (cond  ((= n 0) 
             (if (= min 0)
               t
               'cont))
            ((< (- d2 d1) min ) 'cont)
            ((> (- d2 d1) max ) nil)
            (t  t))))

;; si la regle matche, la selection est le segment entier

(defun is-size (&key (min 0) (max most-positive-fixnum))
  #'(lambda (p1 p2 d1 d2 n)
      (declare (ignore d1 d2 p1 p2))
       (cond ((= n 0) 
             (if (= min 0)
               t
               'cont))
            ((< n min) 'cont)
            ((> n max) nil)
            (t  t))))


;;-----------------------
;; combinaison de regles
;;-----------------------


;; AND
;;-----

(defun is-and2 (r1 r2)
  #'(lambda (p1 p2 d1 d2 n)
      (let ((res1 (funcall r1  p1 p2 d1 d2 n)))
            (cond ((null res1 ) nil)
              ((eq res1 t) (funcall r2  p1 p2 d1 d2 n))
              ((null (funcall r2  p1 p2 d1 d2 n)) nil)
              (t 'cont)))))


(defun is-and (&rest r)
   (if (= (length r) 2)
    (apply #'is-and2 r)
    (is-and2 (car r) (apply  #'is-and  (cdr r)))))


;; OR
;;---


(defun is-or2 (r1 r2)
  #'(lambda (p1 p2 d1 d2 n)
      (let ((res1 (funcall r1  p1 p2 d1 d2 n)))
        (cond ((null res1) (funcall r2  p1 p2 d1 d2 n))
              (( eq res1 t) t)
              ((eq t  (funcall r2  p1 p2 d1 d2 n)) t)
              (t 'cont)))))
 
  
(defun is-or(&rest r)
   (if (= (length r) 2)
    (apply #'is-or2 r)
    (is-or2 (car r) (apply  #'is-or(cdr r)))))


;; NOT
;;----

(defun is-not (r)
  #'(lambda (p1 p2 d1 d2 n)
      (let ((res (funcall r  p1 p2 d1 d2 n)))
      (cond ((eq res t)
             nil)
            ((null res )
              t)
            (t 'cont)))))

;; ACCORD
;;--------
   
(defun is-chord (&rest r)
  (apply #'istab1 (mapcar #'is-sub r)))


    ;      cont   ok   fail
    ;      ---------------
    ; cont  cont  cont  cont
    ; ok    cont   ok   cont
    ; fail  cont  cont  fail

(defun istab12 (r1 r2)                          
  #'(lambda (p1 p2 d1 d2 n)                  
      (let ((res1 (funcall r1 p1 p2 d1 d2 n)))
        (if (eq res1 'cont)
          'cont
          (let ((res2 (funcall r2 p1 p2 d1 d2 n)))
            (cond ((and (null res1)
                        (null res2))
                   nil)
                  ((and (eq res1 t )
                        (eq res2 t))
                   t)
                  (t 'cont)))))))

(defun istab1 (&rest r)
   (if (= (length r) 2)
    (apply  #'istab12 r)
    (istab12 (car r) (apply  #'istab1  (cdr r)))))

;; SEQUENCE
;;----------

(defun is-seq2 (r1 r2)  
  #'(lambda (p1 p2 d1 d2 n)
      (let ((result nil)
            (fin (+ 1 n)))
        (do*  ((count 0 (+ count 1))
               ( pj  p1 (if (= count 1) p1 (link pj))) 
               ( pk  pj (if (= count 1) (link pj) (link pk)))
               ( dj  d1 (if (= count 1) d1 (date pj)))
               ( dk  (date pj) (if (= count 1) (date pj) (date pk))))
              ((= count  fin) 'cont)

         ;( PRINT (LIST P1 PJ PK P2 count n ))
          (let ((res1 (funcall r1  p1 pj d1 dj count)))
            (cond ((eq res1 t)
                   (let ((res2 (funcall r2  pk p2 dk d2 (- n count))))
                     (cond ((eq res2 t)
                            (return t))
                           ((eq res2 'cont)
                            (setq result 'cont))
                           (t ))))
                  ((null res1 )
                   (return result))
                  (t )))))))


(defun is-seq (&rest r)
  (if (= (length r) 2)
    (apply  #'is-seq2 r)
    (is-seq2 (car r) (apply #'is-seq (cdr r)))))


;; la regle is-sub rend -  une selection si elle trouve un sous segment qui matche
;;                      - 'fail si aucun sous segment ne matche   
;;                      - 'cont si elle trouve un sous segment (etendu ˆ droite) qui rend cont 
;;                        (et aucun qui rend 'ok)


;; si la regle matche, la selection du is-sub est la selection de la regle sur le sous segment qui matche
;; rend TRUE
;;-------------------------------------------------------------------------------------------------

(defun is-sub (r) 
  #'(lambda (p1 p2 d1 d2 n)
      (declare (ignore d2 n))
      (let ((result nil))
        (catch 'fin (do* ((pj p1 (link pj)) 
                          (dj d1 (date pj)))
                         ((eql  pj (link p2)) result)
                      (do* ((count 0 (+ count 1))
                            (pk pj (if (= count 1) pj (link pk)))
                            (dk dj (if (= count 1) dj (date pk))))
                           ((eql  pk (link p2)) (setq result 'cont))
                        (let ((res (funcall r  pj pk dj dk count)))
                          (cond ((eq res t)
                                 (throw 'fin res))
                                ((eq res 'cont))
                                (t (return))))))))))


;; TEMPS REEL rend un ptr de fin de sous segment ˆ liberer dans le cas du resultat 'cont
;;---------------------------------------------------------------------------------------

(defun is-sub-tr (r)
  #'(lambda (p1 p2 d1 d2 n)
      (let ((result nil)
            (ptr nil))
        (do* ((count n (- count 1))
              (pj p1 (link pj))
              (dj d1 (if (nullptrp pj) 0 (date pj))))
             ((nullptrp pj) (values result ptr))
          (let ((res (funcall r pj p2 dj d2 count)))
            (cond ((eq res t)
                   (return (values res pj)))
                  ((null res))
                  (t (unless ptr         ; on garde le resultat favorable
                     (setq result 'cont
                           ptr pj)))))))))
 

;; TEMPS DIFFERE rend la selection (couple de ptr) et la taille du segment
;;--------------------------------------------------------------------------

(defun is-sub-td (r) 
  #'(lambda (p1 p2 d1 d2 n)
      (declare (ignore d2 ))
      (let ((result nil))
        (catch 'fin (do* ((pj p1 (link pj)) 
                          (dj d1 (date pj)))
                         ((eql  pj (link p2)) result)
                      (do* ((count 0 (+ count 1))
                            (pk pj (if (= count 1) pj (link pk)))
                            (dk dj (if (= count 1) dj (date pk))))
                           ((or (> count n)(eql  pk (link p2))) (setq result 'cont))
                        (let ((res (funcall r  pj pk dj dk count)))
                          (cond ((eq res t)
                                 (throw 'fin (list pj pk count)))
                                ((eq res 'cont))
                                (t (return))))))))))



;; cherche le segment de longueur maximum qui matche la regle
;;------------------------------------------------------------

(defun is-max (r)
  #'(lambda (p1 p2 d1 d2 n)
      (let ((res (funcall r p1 p2 d1 d2 n)))
        (if (eq res t)
          (let ((next (link p2)))
            (if (nullptrp next)
              res
              (if (eq t (funcall r p1 next d1 (date next) (+ n 1)))
                'cont
                res)))
          res))))
