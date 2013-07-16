;;=============================================================================
;;
;;                           RYTHMES PRÉDÉFINIS
;;
;;                         OPÉRATEURS SUR RYTHMES
;;
;;=============================================================================


;; Pour tester les durées des expressions rythmiques:
;; (dur-midi (prefixplus (code-rythme  '(4/5dc (1/4 -1))))) --> 1
;; gi-rythme et gi-rythme2 nouveau générateurs de rythme 12 - 04 -05


;; ------------
(defvar dc1)
(defvar dc2)
(defvar dc3)
(defvar dc4)

(setq dc1 '(1/4 -3/4))
(setq dc2 '(-1/4 1/4 -1/2))
(setq dc3 '(-1/2 1/4 -1/4))
(setq dc4 '(-3/4 1/4))
;; ------------
(defvar trio1)
(defvar trio2) 
(defvar trio3)

(setq trio1 '(2/3c (1/2 -1)))
(setq trio2 '(2/3c (-1/2 1/2 -1/2)))
(setq trio3 '(2/3c (-1 1/2)))

;; ------------
(defvar quin1)
(defvar quin2)
(defvar quin3)
(defvar quin4)
(defvar quin5)

(setq quin1 '(4/5dc (1/4 -1)))
(setq quin2 '(4/5dc (-1/4 1/4 -3/4)))
(setq quin3 '(4/5dc (-1/2 1/4 -1/2)))
(setq quin4 '(4/5dc (-3/4 1/4 -1/4)))
(setq quin5 '(4/5dc (-1 1/4)))

;; ------------
(defvar sy0)
(defvar sy1)
(defvar sy2)
(defvar sy3)
(defvar sy4)
(defvar sy5)
(defvar sy6)
(defvar sy7)
(defvar sy8)
(defvar sy9)

(setq sy0 '(2/3n (1 1 1)))
(setq sy1 '(1))
(setq sy2 '(1/2 1/2))
(setq sy3 '(2/3c (1/2 1/2 1/2)))
(setq sy4 '(1/4 1/4 1/4 1/4))
(setq sy5 '(4/5dc (1/4 1/4 1/4 1/4 1/4)))
(setq sy6 '(4/6dc (1/4 1/4 1/4 1/4 1/4 1/4)))
(setq sy7 '(4/7dc (1/4 1/4 1/4 1/4 1/4 1/4 1/4)))
(setq sy8 '(1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8 ))

;; ------------
(defvar ttrio1)
(defvar ttrio2)
(defvar ttrio3)

(setq ttrio1 '(2/3c (1/2 1)))
(setq ttrio2 '(2/3c (1/2 1/2 1/2)))
(setq ttrio3 '(2/3c (1 1/2)))

;; ------------
(defvar ddc1)
(defvar ddc2)
(defvar ddc3)
(defvar ddc4)
(defvar ddc5)
(defvar ddc6)

(setq ddc1 '(1/4 3/4))
(setq ddc2 '(1/4 1/2 1/4))
(setq ddc3 '(1/4 1/4 1/2))
(setq ddc4 '(1/4 1/4 1/4 1/4))
(setq ddc5 '(1/2 1/4 1/4))
(setq ddc6 '(3/4 1/4))

;; ------------
(defvar qquin1)
(defvar qquin2)
(defvar qquin3)
(defvar qquin4)
(defvar qquin5)
(defvar qquin6)
(defvar qquin7)
(defvar qquin8)

(setq qquin1 '(4/5dc (1/4 1/2 1/2)))
(setq qquin2 '(4/5dc (1/2 1/4 1/2)))
(setq qquin3 '(4/5dc (1/2 1/2 1/4)))
(setq qquin4 '(4/5dc (1/4 1/4 3/4)))
(setq qquin5 '(4/5dc (1/4 3/4 1/4)))
(setq qquin6 '(4/5dc (3/4 1/4 1/4)))
(setq qquin7 '(4/5dc (1/2 3/4)))
(setq qquin8 '(4/5dc (3/4 1/2)))




;; =========================================================
;;           RETROGRADATION D'UN RYTHME: RETRO (R)
;; =========================================================

;; Exemples de renversement d'un rythme:

;; (l 1 (retro °(1 /12 1/2))) --> ((1/2 /12 1))

(defun retro (a)
#'(lambda (stime ctime etime reverse)
        (reverse §a)))








;; =========================================================
;; FUSION DE RYTHMES: FUSION, NFUSION
;; =========================================================

;; Exemples de fusion de rythmes:

;; (l 1 (fusion °(1 -1) °(-2 -3) °(0) °(-5 10 10)))
;; ((1 1 + 2 3 + 5 10 10))

;; (l 4 (fusion °(1 -1) (alt °(-2 -3) °(0) °(-5 10 10))))
;; ((1 1 + 2 -3) (1 -1) (1 1 + 5 10 10) (1 1 + 2 -3))

;; (l 1 (nfusion °4 °(1)))
;; ((1 + 1 + 1 + 1))




(defun chg-fin (l)
  (cond ((numberp l) (abs l))
        ((not (listp l)) l)
        ((cdr l) (cons (car l) (chg-fin (cdr l))))
        (t (list (chg-fin (car l))))))




(defun chg-debut (l)
  (cond ((numberp l) (abs l))
        ((and (not (listp (car l))) (not (numberp (car l)))) (cons  (car l) (chg-debut (cdr l))))
        ((cdr l) (cons (chg-debut (car l)) (cdr l)))
        (t (list (chg-debut (car l))))))




(defun zerop-rythm (l)
  (if (numberp l) 
    (zerop l)
    (if (consp l)
      (and (zerop-rythm (car l))
           (zerop-rythm (cdr l)))
      t)))



;; =========================================================
;;         CONCATENATION DE RYTHMES: CONCAT (R1 R2 R3 ...)
;; =========================================================

;; Exemples de concaténation de rythmes:

;; (l 1 (concat °ry1 °ry1)) -->
;; ((2/3DC (1/4 1/2) 1/2 + 2/3C (1/2 -1) 2/3DC (1/4 1/2) 1/2 + 2/3C (1/2 -1)))

;; (l 1 (concat °(1 1) °(2 4))) --> ((1 1 2 4))

;; (l 1 (concat °(1 2 3) °(10 20) °(77 88 99 66))) --> ((1 2 3 10 20 77 88 99 66))
;; ---------------------------------------------------
(defun concat ( &rest l)
(appl °append (apply #'lst l)))


;; --------------------------------------------------- concatene n fois rythm
(defun nconcat (n rytm)
  #'(lambda (stime ctime etime reverse) 
      (let (l) 
        (dotimes  (i §n) (push  rytm l)) 
        §(apply #'concat l))))

;; (l 3 (nconcat °3 (h °((1) (1/2) (1/4))))) --> ((1 1/4 1/2) (1/2 1/4 1/4) (1/4 1/2 1))


;; --------------------------------------------------- concatene n fois rythm (qui est évalué une seule fois)

(defun nconcat-spe (n rytm)
  #'(lambda (stime ctime etime reverse) 
      (let (l (r §rytm)) 
        (dotimes  (i §n) (setq l (append l r)))
        l)))

;; (l 3 (nconcat-spe °3 (h °((1) (1/2) (1/4))))) --> ((1/2 1/2 1/2) (1/2 1/2 1/2) (1 1 1))


; ---------------------------------------------------- concatene n fois rythm et complete à l'entier supérieur

(defun nconcat-spe2 (n r)
  #'(lambda (stime ctime etime reverse) 
      (let (l (r §r)) 
        (dotimes  (i §n) (setq l (append l r)))
        (let ((d (dur-midi (prefixplus (code-rythme l)))))
        (if (= d (truncate d))
            l
             (append l (list (- (+ 1 (truncate d)) d))))))))

; (l 1 (nconcat-spe2 °9 °(1/4))) -----------> ((1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4 3/4))



; ---------------------------------------------------- décompose un rythme r1 en éléments r2 

(defun nconcat-spe3 (r1 r2)
  #'(lambda (stime ctime etime reverse) 
      (let* ((rrr2 §r2)
             (rr1 (dur-midi (prefixplus (code-rythme §r1))))
             (rr2 (dur-midi (prefixplus (code-rythme rrr2))))
             (q (truncate (/ rr1 rr2)))
             (rest (- rr1 (* q rr2)))
             (l))
     
        (dotimes  (i q) (setq l (append l rrr2)))
        (if (> rest 0)
            (append l (list rest)) l))))

;(l 1 (nconcat-spe3 °(2) °(1/4))) ------> ((1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4)) 
;(l 1 (nconcat-spe3 °(2 + 1/8) °(1/4))) ------> ((1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/8)) 



;; --------------------------------------------------- concatene une liste de listes (ou de rythmes)
(defun concat-list ( l)
(appl °append l))

;; (l 1 (concat-list (lst °(1 2 3) °(10 20)))) --> ((1 2 3 10 20))


;; =========================================================
;;           FUSION DE RYTHMES: FUSION (R1 R2 R3 ...)
;; La fusion concatene les rythmes en les liant
;; =========================================================


;; --------------------------------------------------- fusionne une suite de rythmes
(defun fusion ( &rest l)
  (if (not (third l))
    (fusion2 (first l) (second l))
    (fusion2 (first l) (apply #'fusion (cdr l)))))

;; (l 1 (fusion °(1) °(2/3n (1 -1 1)) °(1/2))) --> ((1 + 2/3N (1 -1 1) + 1/2))

;; --------------------------------------------------- fusionne n fois rythm - corrigé le 14 08 07
(defun nfusion (n rytm)
  #'(lambda (stime ctime etime reverse) 
      (let ((nn §n) l) 
        (cond ((<= nn 0) nil)
              ((= nn 1) §rytm)
              (t
               (dotimes  (i nn) (push  rytm l)) 
               §(apply #'fusion l))))))

;; (l 1 (nfusion °3 (h °((1) (1/2) (1/4))))) --> ((1/2 + 1/4 + 1/4) (1 + 1/4 + 1))

;; --------------------------------------------------- fusionne deux rythmes
(defun fusion2 (gr1 gr2)
  #'(lambda (stime ctime etime reverse)
        (let ((r1 §gr1)
               (r2 §gr2))
          (if (zerop-rythm r1)
            r2
            (if (zerop-rythm r2)
              r1
              (append (chg-fin r1) '(+) (chg-debut r2)))))))

(defun afusion2 (r1 r2)
  (if (zerop-rythm r1)
    r2
    (if (zerop-rythm r2)
      r1
      (append (chg-fin r1) '(+) (chg-debut r2)))))

;; (l 1 (fusion2 °(1) °(2/3n (1 -1 1)))) --> ((1 + 2/3N (1 -1 1)))





;; =========================================================
;;                    COUF
;; fusionne ou concat une liste de rythmes (a probabilité égale)
;; =========================================================

;; (l 4 (couf °((1) (2)))) --> ((1 + 2) (1 2) (1 2) (1 + 2))

(defun couff (l)
  (let ((x (proba 0.5)))
    (if (not (third l))
      (if x (afusion2 (first l) (second l)) (append (first l) (second l)))
      (if x (afusion2 (first l) (couff (cdr l))) (append (first l) (couff (cdr l)))))))

(defun couf (l)
  (call °couff l))


;; =========================================================
;;                    COUF1
;; fusionne ou concat une suite de rythmes (a probabilité égale)
;; =========================================================

;;(l 3 (couf1  °(2) °(4) °(8))) --> ((2 + 4 8) (2 + 4 8) (2 + 4 8))

(defun couf1 ( &rest l)
  (let ((x (proba 0.5)))
    (if (not (third l))
      (if x (fusion2 (first l) (second l)) (concat (first l) (second l)))
      (if x (fusion2 (first l) (apply #'couf1 (cdr l))) (concat (first l) (apply #'couf1 (cdr l)))))))

(defun couf1 (&rest l) (call °couff (apply #'lst l)))


;; =========================================================
;;                    COUF2
;; fusionne ou concat avec une prob gp, gn fois un rythme gr
;; =========================================================

;;(l 2 (couf2 (alt °0.4 °0.6)  °3 (sel-al °2 °(2) °(4) °(8))))
;;((4 + 2 + 4 8) (8 8 4 + 2) (2 + 2 + 2 + 2) (4 2 2 + 2))

(defun fusion-rythmes (r1 r2)
  (if (zerop-rythm r1)
    r2
    (if (zerop-rythm r2)
      r1
      (append (chg-fin r1) '(+) (chg-debut r2)))))

;; ------------------------------- FBLK : fonctionnel BLK
;; (l 5 (fblk (alt °cons °append) °4 °(6))) --->
;; (((NIL 6 6) 6 6) ((NIL 6 6) 6 6) ((NIL 6 6) 6 6) ((NIL 6 6) 6 6) ((NIL 6 6) 6 6))

(defun fblk (gf g1 g2)
  #'(lambda (stime ctime etime reverse)
      (let (l)
        (dotimes (i (floor §g1))
          (setq l (funcall §gf l §g2))
          )
        l)))


(defun couf2 (gp gn gr)
  (fblk (gpgate gp °fusion-rythmes °append) gn gr))

#|
|#

;; =========================================================
;;                    COUF2
;; fusionne exclusivement ou concat exclusivement avec 
;; une prob gp, gn fois un rythme gr
;; =========================================================

;;(l 4 (coufexclusif (alt °0.4 °0.6)  °3 (sel-al °2 °(2) °(4) °(8))))
;; --> ((4 4 8) (2 + 4 + 8) (4 2 2) (2 4 2))


(defun coufexclusif (gp gn gr)
  (rdg (gpgate gp °fusion-rythmes °append) (blk gn gr)))

   









;; =========================================================
;; APPLIQUE UNE FORMULE
;; =========================================================

;; (l 2 (gform °(x y -> 1/2 1/3 (y 1/2 y) 3/4 x) (s °(1 2))))
;; --> ((1/3 1/6) 3/4)

(defun gform (gf gv)
  (let (l)
    #'(lambda (stime ctime etime reverse)
        (when (null l)
          (setq l (calc-formule §gf gv stime ctime etime reverse)))
        (pop l))))

(defun calc-formule (f gv stime ctime etime reverse)
  (multiple-value-bind (vars body) (split-formule f)
    (instancie-formule body (calc-env vars gv stime ctime etime reverse))))

(defun instancie-formule (body env &optional (metacoef 1))
  (let ((r nil) (coef metacoef))
    (dolist (x body)
      (cond ((numberp x)
             (setq coef (* coef x)))
            ((symbolp x)
             (push (* (cdr (assoc x env)) coef) r)
             (setq coef metacoef))
            ((listp x)
             (push (instancie-formule x env coef) r)
             (setq coef metacoef))))
    (nreverse r)))

(defun calc-env (vars gv stime ctime etime reverse)
  (let (env)
    (dolist (v vars)
      (setq env (acons v §gv env)))
    env))

(defun split-formule (f &optional vars)
  (if (eq (car f) '->)
    (values (nreverse vars) (cdr f))
    (split-formule (cdr f) (cons (car f) vars))))


;; =========================================================
;; MET A PLAT UNE LISTE
;; =========================================================

;; (l 10 (gflat °(1 (2 3 (4 5))))) --> (1 2 3 4 5 1 2 3 4 5)
    

(defun flat (l)
  (if (listp l)
    (fflat l nil)
    l))

(defun fflat (l ll)
  (let ((lx))
    (if (null l)
      (nreverse ll)
      (progn
        (setq lx (car l))
        (if (listp lx)
          (setq ll (append (nreverse (flat lx)) ll))
          (setq ll (cons lx ll)))
        (fflat (cdr l) ll)))))

(defun gflat (l)
  (s (call °flat l)))


;; =========================================================
;; REND UN RYTHME
;; =========================================================

;; (l 1 (grythme °(x -> 2/3 (1/4 x 1/2 x) x -1/2 x) °2))
;; --> ((1/3 2/3 2 -1))

(defun grythme (gf gv)
    #'(lambda (stime ctime etime reverse)
        (calc-rythme §gf gv stime ctime etime reverse)))

(defun calc-rythme (f gv stime ctime etime reverse)
  (multiple-value-bind (vars body) (split-formule f)
    (instancie-rythme body (calc-env vars gv stime ctime etime reverse))))

(defun instancie-rythme (body env &optional (metacoef 1))
  (let ((r nil) (coef metacoef))
    (dolist (x body)
      (cond ((numberp x)
             (setq coef (* coef x)))
            ((symbolp x)
             (push (* (cdr (assoc x env)) coef) r)
             (setq coef metacoef))
            ((listp x)
             (setq r (revappend (instancie-rythme x env coef) r))
             (setq coef metacoef))))
    (nreverse r)))


;; =========================================================
;; gi-rythme et gi-rythme2 DECOMPOSE UN RYTHME EN 2 PARTIES
;; =========================================================


;; --------------- cr-inv = codage inverse de cr -----------------------

(defun traduit-dur-symb-inv (ds)
  (let ((v (assoc ds '(((fdur 2 3 d-c) (2/3c 3/2))
                       ((fdur 3 5 d-c) (3/5c 5/2))
                       ((fdur 2 3 d-dc) (2/3dc 3/4))
                       ((fdur 3 5 d-dc) (3/5dc 5/4))
                       ((fdur 2 3 d-tc) (2/3tc 3/8))
                       ((fdur 2 3 d-qc) (2/3qc 3/16))
                       ((fdur 2 3 d-n) (2/3n 3))
                       ((fdur 2 3 d-b) (2/3b 6))
                       ((fdur 3 5 d-n) (3/5n 5))
                       ((fdur 4 5 d-n) (4/5n 5))
                       ((fdur 4 6 d-n) (4/6n 6))
                       ((fdur 4 5 d-c) (4/5c 5/2))
                       ((fdur 4 5 d-dc) (4/5dc 5/4))
                       ((fdur 4 5 d-tc) (4/5tc 5/8))
                       ((fdur 4 5 d-qc) (4/5qc 5/16))
                       ((fdur 4 6 d-dc) (4/6dc 6/4))
                       ((fdur 4 6 d-c) (4/6c 6/2))
                       ((fdur 4 6 d-tc) (4/6tc 6/8))
                       ((fdur 4 6 d-qc) (4/6qc 6/16))
                       ((fdur 4 7 d-dc) (4/7dc 7/4))
                       ((fdur 4 7 d-n) (4/7n 7))
                       ((fdur 4 7 d-c) (4/7c 7/2))
                       ((fdur 4 7 d-tc) (4/7tc 7/8))) :test #'equal)))
    (if v
      (second v)
      (error "undefined symbolic duration : ~S" ds))))

(defun prefixplus-inv (l)
  (cond ((null l) l)
        ((eq (car (first l)) '+) (append (list (second (first l)) '+ (third (first l))) (prefixplus-inv (cdr l))))
        (t (cons (first l) (prefixplus-inv (cdr l))))))

(defun cr-inv (l)
  (code-inv (prefixplus-inv l)))

(defun code-inv (l)
  (cond ((null l) l)
        ((or (numberp (car l)) (eq (car l) '+)) (cons (car l) (code-inv (cdr l))))
        ((listp (car l))
         (let* ((x (second (car l)))
                (xx (traduit-dur-symb-inv x))
                (symb (first xx))
                (totdur (second xx))
                (res (code-nolet l totdur)))
           (append (list symb) (list (first res)) (code-inv (second res)))))  ; double n-olet
        (t (print "ERREUR dans code-inv"))))

(code-inv '(1 + 3))
(traduit-dur-symb-inv '(fdur 2 3 d-c))

(defun code-nolet (l totdur &optional (tot 0) (res nil))
  (if (< tot totdur)
    (let ((x (car l)))
      (cond
       ((equal x '+) (code-nolet (cdr l) totdur tot (cons x res)))
       ((and (listp x) (= 3 (length x))) (code-nolet (cdr l) totdur (+ tot (abs (third x))) (cons (third x) res)))
       ((and (listp x) (= 4 (length x)))
        (let* ((y (third (car l)))
               (yy (traduit-dur-symb-inv y))
               (symb2 (first yy))
               (totdur2 (second yy))
               (res2 (code-sd-nolet l totdur2)))
          (code-nolet (second res2) totdur (+ tot (* (eval (second x)) totdur2)) (append  (list (first res2)) (list symb2) res))))))
    (list (reverse res) l)))

(defun code-sd-nolet (l totdur &optional (tot 0) (res nil))
  (if (< tot totdur)
    (let ((x (car l)))
      (cond
       ((equal x '+) (code-sd-nolet (cdr l) totdur tot (cons x res)))
       (t (code-sd-nolet (cdr l) totdur (+ tot (abs (fourth x))) (cons (fourth x) res)))))
    (list (reverse res) l)))
 
#|


(code-inv '(1 + (* (FDUR 2 3 D-N) 2) (* (FDUR 2 3 D-N) 1)))
(code-inv '(1 + (* (FDUR 2 3 D-N) 2) (* (FDUR 2 3 D-N) 1) + 2))


(cr '(1 + 2/3c (1/2 2/3dc (1/2 + 1/4) 1/2) + 2/3n (2 1)))
(prefixplus-inv '((+ 1 (* (FDUR 2 3 D-C) 1/2)) (* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) 1/2) (* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) 1/4) 
                  (+ (* (FDUR 2 3 D-C) 1/2) (* (FDUR 2 3 D-N) 2)) (* (FDUR 2 3 D-N) 1)))
--> (1 + (* (FDUR 2 3 D-C) 1/2) (* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) 1/2) (* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) 1/4) 
     (* (FDUR 2 3 D-C) 1/2) + (* (FDUR 2 3 D-N) 2) (* (FDUR 2 3 D-N) 1))

(cr '(1 + 2/3C (1/2 2/3DC (1/2 + 1/4) 1/2) + 2/3N (2 1)))
(prefixplus-inv '((+ 1 (* (FDUR 2 3 D-C) 1/2)) (+ (* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) 1/2) (* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) 1/4)) 
                  (+ (* (FDUR 2 3 D-C) 1/2) (* (FDUR 2 3 D-N) 2)) (* (FDUR 2 3 D-N) 1)))
-->(1 + (* (FDUR 2 3 D-C) 1/2) (* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) 1/2) + (* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) 1/4) 
    (* (FDUR 2 3 D-C) 1/2) + (* (FDUR 2 3 D-N) 2) (* (FDUR 2 3 D-N) 1))

|#



(defun coupe-rythme (l n &optional (tot 0) (lie nil) (res nil)) ;retourne nil si pas possible
  (if l
    (let* ((y (car l))
           (x (if (listp y) (car (last y)) y))) 
      (cond
       ((equal x '+) (coupe-rythme (cdr l) n tot t (cons y res)))
       (t (cond
           (lie (coupe-rythme (cdr l) n tot nil (cons y res)))
           ((> x 0) (if (< tot n) (coupe-rythme (cdr l) n (1+ tot) nil (cons y res)) (list (reverse res) l)))
           ((< x 0) (coupe-rythme (cdr l) n tot nil (cons y res)))))))))


#|
(coupe-rytm '(1 + 2 3 -1 2) 2)

(1 + 2/3C (1/2 2/3DC (1/2 + 1/4) 1/2) + 2/3N (2 1))
(coupe-rytm '(1 + (* (FDUR 2 3 D-C) 1/2) (* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) 1/2) + (* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) 1/4) 
    (* (FDUR 2 3 D-C) 1/2) + (* (FDUR 2 3 D-N) 2) (* (FDUR 2 3 D-N) 1)) 2)
|#

(defun rend-silence (l &optional (res nil))
  (if l
    (rend-silence (cdr l) (if (equal (car l) '+) res (cons (change-der (car l)) res)))
    (reverse res)))

(defun change-der (x)
  (cond
   ((numberp x) (- 0 (abs x)))
   ((listp x) (change-der-list x))))

(defun change-der-list (x)
  (let ((der (car (last x))))
    (reverse (cons (- 0 (abs der)) (cdr (reverse x))))))

; (change-der'(* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) -1/2))
; (change-der'(* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) 1/2))
; (rend-silence '(1 + (* (FDUR 2 3 D-C) 1/2) (* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) 1/2) + (* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) 1/4)))
   

; (-1 -2 (* (FDUR 2 3 D-C) -1/2) (* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) -1/2) (* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) -1/4))


(defun type-contiguite (x y)
  (cond ((and (numberp x) (numberp y)) 1)
        ((and (listp x) (listp y) (= 3 (length x)) (= 3 (length y)) (equal (second x) (second y))) 2)
        ((and (listp x) (listp y) (= 4 (length x)) (= 4 (length y)) (equal (third x) (third y))) 3)
        (t 0)))


   
; ------------------- cumul les silences d'un rythme fait que de silences ----------------------------
(defun compact-silence (l)
  (if l (compact-silence0 l) nil))

(defun compact-silence0 (l &optional (tot 0) (res nil) (precedent nil) (premier? t))
  (if l
    (let ((x (car l)))
      (cond
       ((numberp x) (if (or premier? (= (type-contiguite x precedent) 1)) (compact-silence0 (cdr l) (+ tot x) res x nil)
                        (compact-silence0 (cdr l) x (cons (cumul tot precedent) res) x nil)))
       ((and (listp x) (= 3 (length x))) (let ((dur (second (traduit-dur-symb-inv (second x)))))
                                           (if (and (< (abs tot) dur) (or premier? (= (type-contiguite x precedent) 2))) (compact-silence0 (cdr l) (+ tot (car (last x))) res x nil)
                                             (compact-silence0 (cdr l) (car (last x)) (cons (cumul tot precedent) res) x nil))))
       ((and (listp x) (= 4 (length x))) (let ((dur (second (traduit-dur-symb-inv (third x)))))
                                           (if (and (< (abs tot) dur) (or premier? (= (type-contiguite x precedent) 3))) (compact-silence0 (cdr l) (+ tot (car (last x))) res x nil)
                                             (compact-silence0 (cdr l) (car (last x)) (cons (cumul tot precedent) res) x nil))))))
    (reverse (cons (cumul tot precedent) res))))


  


(defun cumul (n a)
  (cond
   ((numberp a) n)
   ((listp a) (reverse (cons n (cdr (reverse a)))))))

#|
(compact-silence '(-1 -2 (* (FDUR 2 3 D-C) -1/2) (* (FDUR 2 3 D-C) -3/2) (* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) -1/2) 
                   (* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) -1/4) (* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) -1/4) 100))

(2/3n (1 -1 1) 2/3n (2 1))
(compact-silence '((* (FDUR 2 3 D-N) -1) (* (FDUR 2 3 D-N) -1) (* (FDUR 2 3 D-N) -1) (* (FDUR 2 3 D-N) -2) (* (FDUR 2 3 D-N) -1)))

;(2/3dc (1/2 1/4) 2/3dc (1/4 1/4 1/4))
(compact-silence '((* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) -1/2) (* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) -1/4) 
                   (* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) -1/4) (* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) -1/4) (* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) -1/4)))
|#

; ------------ remplace les nolet totalement silence par leurs équivalents non nolet ---------------
; ---- ou remplace les double nolet totalement silence par leurs équivalent simple nolet -----------
(defun transf-noletsilence (l &optional (res nil))
  (if l
    (let ((x (car l)))
      (cond
       ((numberp x) (transf-noletsilence (cdr l) (cons x res))) 
       ((and (listp x) (= 3 (length x))) (let ((dur (second (traduit-dur-symb-inv (second x))))) 
                                           (if (= dur (abs (third x))) (transf-noletsilence (cdr l) (cons (- 0 (abs(* dur (eval (second x))))) res))
                                               (transf-noletsilence (cdr l) (cons x res)))))
       ((and (listp x) (= 4 (length x))) (let ((dur (second (traduit-dur-symb-inv (third x)))))
                                           (if (= dur (abs (fourth x))) (transf-noletsilence (cdr l) (cons (list (first x) (second x) (- 0 (abs (* dur (eval (third x)))))) res))
                                               (transf-noletsilence (cdr l) (cons x res)))))))
    (reverse res)))
#|
(transf-noletsilence '( -3 (* (FDUR 2 3 D-N) -3) (* (FDUR 2 3 D-N) -1/2))) --> (-3 -2 (* (FDUR 2 3 D-N) -1/2))
(transf-noletsilence '( -3 (* (FDUR 2 3 D-N) -3) (* (FDUR 2 3 D-C) -3/2) (* (FDUR 2 3 D-N) -1/2))) --> (-3 -2 -1 (* (FDUR 2 3 D-N) -1/2))
|#

; ----- Rend négatives (silence) les i premières durées positives d'un rythme ------


(defun i-rythme (l i)
  (let ((long (long-rythme l))
        (dur (eval-dur-rythme l)))
    (if (< i long)
  (let ((r (coupe-rythme (code-rythme l) i)))
    (code-inv 
     (append 
      
      (compact-silence 
       (transf-noletsilence 
        (compact-silence 
         (rend-silence 
          (first r)))))
      
      (second r))))
  (list (- 0 dur)))))




#|
(i-rythme '(1 + 2/3c (2/3dc (1/2 1/4) -1/2 1/2) + 2/3n (2 1)) 2)   -->   (-1 2/3C (-1 1/2) + 2/3N (2 1))
(i-rythme '(1 + 2/3c (2/3dc (1/2 1/4) -1/2 1/2) + 2/3n (2 1)) 0)
(i-rythme '(1 + 2/3c (2/3dc (1/2 1/4) -1/2 1/2) + 2/3n (2 1)) 10)
(i-rythme '(1 2 -3 4 -5 6 7) 3)   -->   (-15 6 7)
|#


; ------------------ Additionne si possible les durées positives et négatives d'un rythme -----------------------



(defun compact-rythme (l)
  (reverse (compact-rythm (reverse l))))

(defun compact-rythm (l &optional (tot 0) (res nil) (precedent nil) (premier? t)) ;(print (list (cdr l) res))
       (if l
         (let ((x (car l)))
           (cond
            ((numberp x) (if (or premier? (= (type-contiguite x precedent) 1)) (compact-rythm (cdr l) (+ tot (abs x)) res (abs x) nil)
                             (compact-rythm (cdr l) x (cons (cumul tot precedent) res) (abs x) nil)))
            ((and (listp x) (= 3 (length x))) (let ((dur (abs(second (traduit-dur-symb-inv (second x))))))
                                                (if (and (< tot dur) (or premier? (= (type-contiguite x precedent) 2))) (compact-rythm (cdr l) (+ tot (abs (car (last x)))) res x nil)
                                                    (compact-rythm (cdr l) (abs (car (last x))) (cons (cumul tot precedent) res) x nil))))
            ((and (listp x) (= 4 (length x))) (let ((dur (second (traduit-dur-symb-inv (third x)))))
                                                (if (and (< tot dur) (or premier? (= (type-contiguite x precedent) 3))) (compact-rythm (cdr l) (+ tot (abs (car (last x)))) res  x nil)
                                                    (compact-rythm (cdr l) (abs (car (last x))) (cons (cumul tot precedent) res) x nil))))))
         (reverse (cons (cumul tot precedent) res))))



#|
;(compact-rythme '(1 1 3 -5)) --> (10)
;(compact-rythme (ote '+ '(1 + (* (FDUR 2 3 D-C) 1/2) (* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) 1/2) + (* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) 1/4))))
;(code-rythme '(1 + 2 + 3 + 2/3n (-1 2) 2/3n (2/3c (1/2 -1/2 1/2) 2/3c (1 1/2) + 2/3c (1/2 -1))))
;(compact-rythme (ote '+ '(1 + 2 + 3 + 
                          (* (FDUR 2 3 D-N) -1) (* (FDUR 2 3 D-N) 2) 
                          (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) 1/2) (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) -1/2) (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) 1/2) 
                          (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) 1) (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) 1/2) + 
                          (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) 1/2) (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) -1))))

(compact-rythme '(1 2 3 4)) --> 10
(compact-rythme '(1 + 4/5dc (1/4 -1))) --> nil!!!!!!!!!



|#

; ------------------ Remplace un rythme par un rythme de sa durée  -----------------------

(defun compact (l)
    #'(lambda (stime ctime etime reverse)
        (list(dur-midi (prefixplus (code-rythme §l))))))

;;(dur-midi (prefixplus (code-rythme '(1 + 4/5dc (1/4 -1)))))

;;(l 1 (compact °(1 + 4/5dc (1/4 -1)))) --> ((2))



; -------------------------------------------------------------------
; ------------------ Remplace les n-olets "complet" d'un rythme par la durée équivalente -----------------------
; -------------------------------------------------------------------

(defun ote-nolet-tout-silence (l &optional (res nil))
  (if l
    (let ((x (car l)))
      (cond
       ((numberp x) (ote-nolet-tout-silence (cdr l) (cons x res)))
       ((and (listp x) (= 3 (length x))) (let ((y (second (traduit-dur-symb-inv (second x)))))
                                           (if (= y (third x))
                                             (ote-nolet-tout-silence (cdr l) (cons (* (eval (second x)) y) res))
                                             (ote-nolet-tout-silence (cdr l) (cons x res)))))
       ((and (listp x) (= 4 (length x))) (let ((y (second (traduit-dur-symb-inv (third x)))))
                                           (if (= y (fourth x))
                                             (ote-nolet-tout-silence (cdr l) (cons (list (first x) (second x) (* (eval (third x)) y)) res))
                                             (ote-nolet-tout-silence (cdr l) (cons x res)))))))
    (reverse res)))


#|
(ote-nolet-tout-silence (code-rythme '(2/3n (3)))) --> (2)
(ote-nolet-tout-silence '((* (FDUR 2 3 D-N) 3)))  --> (2)

(ajout '+ (ote-nolet-tout-silence 
 (compact-rythme 
  (ote-nolet-tout-silence 
   (compact-rythme (ote '+ '((* (FDUR 2 3 D-N) 2) 1 + 2 + 3 + 
                          (* (FDUR 2 3 D-N) -1) (* (FDUR 2 3 D-N) 2) 
                          (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) 1/2) (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) -1/2) (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) 1/2) 
                          (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) 1) (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) 1/2) + 
                          (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) 1/2) (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) -1))))))))
|#

; ------------------ Intercalle un atome a entre les éléments d'une liste -----------------------
(defun ajout (a l &optional (res nil))
  (if l
    (ajout a (cdr l) (append (list a (car l)) res))
    (reverse (cdr res))))

; (ajout '+ '(1 2 3 4 5)) --> (1 + 2 + 3 + 4 + 5)


; -------------------------------------------------------------------
; ----- Rend négatives (silence) les i premières durées positives d'un rythme ------
; ------------------ et compact toutes les durées restantes en une seule -----------
; -------------------------------------------------------------------


(defun i-rythme2 (l i)
  (let ((long (long-rythme l))
        (dur (eval-dur-rythme l)))
    (if (< i long)
  (let ((r (coupe-rythme (code-rythme l) i)))
    (code-inv 
     (append 
      
      (compact-silence 
       (transf-noletsilence 
        (compact-silence 
         (rend-silence 
          (first r)))))
      
      (ajout '+ (ote-nolet-tout-silence 
                 (compact-rythme 
                  (ote-nolet-tout-silence 
                   (compact-rythme (ote '+ (second r))))))))))
  (list (- 0 dur)))))




;(i-rythme2 '(1 + 2/3c (2/3dc (1/2 1/4) -1/2 1/2) + 2/3n (2 1)) 2) --> (-1 2/3C (-1 1/2) + 2)
;(i-rythme2 '(2/3N (1 1 1) 2/3N (1 1 1)) 1)
;(i-rythme2 '(1 2 -3 4 -5 6 7) 3)
;(i-rythme2 '(1 + 2/3c (2/3dc (1/2 1/4) -1/2 1/2) + 2/3n (2 1)) 10)
;(i-rythme2 '(1 + 2/3c (2/3dc (1/2 1/4) -1/2 1/2) + 2/3n (2 1)) 0)
;(i-rythme2 '(1 + 2/3c (2/3dc (1/2 1/4) -1/2 1/2) + 2/3n (2 1)) 4)


; -------------------------------------------------------------------
; CONTRAIRE: rend 1 si 0, 0 pour toute autre valeur
; -------------------------------------------------------------------
(defun contraire (l)
    #'(lambda (stime ctime etime reverse)
        (if (= §l 0)  1  0)))

(l 3 (contraire (s °(0 0 1 2 3))))


; -------------------------------------------------------------------
; MODIFRYTHME: retourne: si n=0 le rythme r compacté, si n=-1 le rythme r compacté en silence, sinon r
; -------------------------------------------------------------------

(defun modifrytm (r n)
                  (cond ((= 0 n) (list(dur-midi (prefixplus (code-rythme r)))))
                        ((= -1 n) (list (- 0 (dur-midi (prefixplus (code-rythme r))))))
                        (t r)))

(defun modifrythme (r n)
    #'(lambda (stime ctime etime reverse)
        (modifrytm §r §n)))




;(l 1 (modifrythme °(2 2 + 2/3n (1 1 1) -10) °-1)) --------> (-16)
;(l 1 (modifrythme °(2 2 + 2/3n (1 1 1) -10) °0)) ---------> (16)
;(l 1 (modifrythme °(2 2 + 2/3n (1 1 1) -10) °6)) ---------> (2 2 + 2/3n (1 1 1) -10)


; -------------------------------------------------------------------
; -- Rend négatives (silence) les i premières durées positives d'un rythme ---
; -------------------------------------------------------------------

(defun gi-rythme (l i)
  #'(lambda (stime ctime etime reverse) 
      (i-rythme §l §i)))

; -------------------------------------------------------------------
; -- Rend négatives (silence) les i premières durées positives d'un rythme l --
; ---------- et compact toutes les durées restantes en une seule ---------------
; -------------------------------------------------------------------

(defun gi-rythme2 (l i)
  #'(lambda (stime ctime etime reverse) 
      (i-rythme2 §l §i)))

#|
(l 1 (gi-rythme °(1 2 -3 4 -5 6 7) °3)) --> ((-15 6 7))
(l 1 (gi-rythme2 °(1 2 -3 4 -5 6 7) °3)) --> ((-15 13))

(l 1 (gi-rythme °(1 + 2/3c (2/3dc (1/2 1/4) -1/2 1/2) + 2/3n (2 1)) °2)) --> ((-1 2/3C (-1 1/2) + 2/3N (2 1)))
(l 1 (gi-rythme2 °(1 + 2/3c (2/3dc (1/2 1/4) -1/2 1/2) + 2/3n (2 1)) °2)) --> ((-1 2/3C (-1 1/2) + 2))
|#     




