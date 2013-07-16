
;;==============================================================================
;;
;;                          GENERATEURS PARTICULIERS (paj)
;;
;;==============================================================================

;; GAUSS, TRIANGLE, GMODE, GMODALEA, SEL-AL, PASSE, PASSEP, PASSE2, PASSEP2,
;; PASSIP, PROBA01, GSOM, REDUIT-G, REDUIT-D, AUGMENTE-G, AUGMENTE-D
;; de Letz le 4 juillet 2006 ajout de share-gen et use-gen


(defmacro share-gen (symb g)
  `(let ((gen ,g))
     (setf (get ',symb 'generator) gen)
     gen))

(defmacro use-gen (symb)
  `(let ((value-index 0))
     #'(lambda (stime ctime etime reverse)
         (let ((value-list (get ',symb 'value-list))
               (gen (get ',symb 'generator)))
           ;; (print value-list)
           (if (>= value-index (length value-list))
             (setf (get ',symb 'value-list) (push (funcall gen stime ctime etime reverse) value-list)))
           (incf value-index)
           (elt value-list (- (length value-list) value-index))))))


(defmacro free-gen (symb)
  `(setf (get ',symb 'value-list) nil))





;;;;;;;;;;;

;(share-gen x (h °(1 2 3 5 6 8 9 1 2)))
;(share-gen y (rnd °50 °60))
;(share-gen z (floo (i °50 °60)))

;(share-gen a (sel (h °(0 1 1 0 0 1)) (rnd °0 °10) (rnd °100 °200)))
;(share-gen b (sel (h °(0 1 1 0 0 1)) (rnd °0 °10) (rnd °100 °200)))


;(l 50 (alt (use-gen x) (freq °2 (use-gen x)) (use-gen x)))

;(l 50 (alt (use-gen z) (freq °2 (use-gen z)) (use-gen z)))

;(l 50 (alt (use-gen b) (freq °3 (use-gen b)) (use-gen b)))
;(l 50 (alt (use-gen b) (freq °2 (use-gen b)) (use-gen b)))

;; pour liberer la liste de valeurs associée à un symbole:
;(free-gen x)






(defun gaussienne (x)
  (exp (-  (/ ( *  x x)  20))))


(defun f (x  t1 t2)
  ( - (/ (* 20 (- x t1)) ( - t2 t1))  10))


(defun alpha ( x t1 t2)
  (gaussienne (f x t1 t2)))


;;===============================================
;; GAUSS (LMIN LMAX)

;; Distribution gaussienne sur l'intervalle 
;; stime etime avec au centre un pic = lmax et
;; la valeur lmin aux bornes.
;;===============================================

(defun gauss (lmin lmax)
  #'(lambda (stime ctime etime reverse)
      (let ((la §lmax)
            (li §lmin))
        (if reverse (round (+ li (* (alpha  (- etime ctime) stime etime) (- la  li))))
            (round (+ li (* (alpha  ctime stime etime) (- la  li))))))))


;; (l 10 (gauss °1 °10))
;; (1 1 2 5 8 10 8 5 2 1)

;; (l 10 (gauss (i °1 °5) °10))
;; (1 2 3 6 9 10 9 7 5 5)


;;===============================================
;; TRIANGLE (LMIN LMAX)

;; Distribution "triangulaire" sur
;; l'intervalle entre les valeurs lmin lmax puis lmin

;; exemples
;;(l 10 (triangle °0 °10))
;;(0 2 4 6 8 10 8 6 4 2)

;;(l 10 (quantif (triangle °1000 °2000)))
;;(1000 1167 1396 1584 1834 2000 1834 1584 1396 1167)
;;===============================================

(defun triangle (lmin lmax)
  #'(lambda (stime ctime etime reverse)
      (let ((a (/ (+ stime etime) 2))
            (e (/ (- etime stime) 2))
            (la §lmax)
            (li §lmin))
        (cond ((< ctime stime) li)
              ((> ctime etime) li)
              (t
               (round (+ la  (* (/ (abs (- ctime a)) e) (- li la)))))))))






;;===============================================
;; GMODE (V1 V2)

;; Génére une liste de valeurs entre v1 et v2
;; suivant le modèle fournit par la liste l

;; exemple
;; (l 1 (gmode °60 °72 °maj))
;; ((60 62 64 65 67 67 69 71 72))

;; (l 1 (gmode °60 °72 ))
;; ((60 61 62 63 64 65 66 67 68 69 70 71 72))

;; (l 1 (gmode °60 °72 °(1) :rev °t))
;; ((72 71 70 69 68 67 66 65 64 63 62 61 60))

;; (l 2 (gmode °0 °12 (blk °12 (h °(1 2 3)))))
;; ((0 1 3 4 7 9 10 11) (0 1 3 5 6 8 11))
;;===============================================




(defun gmode (v1 v2 &optional (l °(1)) &key (rev °nil))
#'(lambda (stime ctime etime reverse)
(let ((ll §l))
  (if (numberp ll) (setq ll (list ll)))
  (do ((res nil)
       (l2 ll)
       (vc §v1))
      ((> vc §v2) (if §rev res (nreverse res)))
    (unless l2 (setq l2 ll))
    (push vc res)
    (incf vc (pop l2))))))


  
;;===============================================
;; GMODALEA (BMIN BMAX L)

;; Générateur aléatoire de valeurs appartenant à un mode 
;; majeure de base bmin allant jusqu'à bmax inclus.

;; exemple
;; (l 10 (gmodalea °60 °72 °maj))
;; (72 64 65 64 71 67 71 64 69 62)

;; (l 10 (floo(gmodalea °60 (i °72 °60) °maj)))
;; (65 66 71 65 66 70 67 68 71 70)
;;===============================================
 


(defun gmodalea (bmin bmax l)
  (h (gmode bmin bmax l)))





;;===============================================
;; SEL-AL (N G1 G2 G3 ....)

#| Selectionne aléatoirement sur une longueur donnée
 (l 10 (sel-al °1 °10 °20 °30))
 (20 10 20 20 20 20 20 20 10 10)

 ( prorythme °0 °0 °8 (gnotes)
              °0
              (sel-al °1 °(2/3dc(1/4 1/4 1/4) + 2/3dc(1/4 1/4 1/4))
                      °(1 + 1/2 1/2 2))
              (s °( 60 61 62 63))
              °100)
|# 
;;===============================================


(defun sel-al (g1 &rest lg)
  #'(lambda (stime ctime etime reverse)
      §(elt lg (random (1+ §g1)))))




;;==============================================================================
;;
;;                              GENERATEUR TEMPORELS      
;; L'évaluation d'un générateur temporel dépend de la date courante qui n'est pas passée
;; explicitement. Un générateur peut dépendre de bornes temporelles t1 et t2
;;
;;==============================================================================




;;==============================================================================
;;  GENERATEURS TEMPOREL DE PASSAGE (ou de transition) PASSE et PASSEP


;; PASSE = Passage en interpolation
;; PASSE (T1 T2 G1 G2) où g1 et g2 sont des générateurs de valeurs numériques
;; Si la date courante <= T1 --> G1
;; Si la date courante > T1 et < T2 --> valeur interpolée entre G1 et G2
;; Si la date courante >= T2 --> G2

;; Ex (l 30 (passe °10 °20 °0 °10)) --> (0 0 0 0 0 0 0 0 0 0 0 1 2 3 4 5 6 7 8 9 10 10 10 10 10 10 10 10 10 10)


;; PASSEP = Passage en probabilité
;; PASSEP (T1 T2 G1 G2) où g1 et g2 sont des générateurs quelconque
;; Si la date courante <= T1 --> G1
;; Si la date courante > T1 et < T2 --> probabilité croissante avec le temp de valeurs de G2 au détriment de G1
;; Si la date courante >= T2 --> G2

;; Ex (l 30 (passep °10 °20 °0 °10)) --> (0 0 0 0 0 0 0 0 0 0 0 0 0 10 0 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10)

;;==============================================================================

(defun constraintspe (x min max)
  ;; (cond ((< x min) (- min 1))
  (cond ((< x min) min)
        ((> x max) max)
        ;;(t (round x))
        (t x)
        ))

#| ancien probint et passe

(defun probint (t1 t2)
  #'(lambda (stime ctime etime reverse)  
      (let ((tt1 (* noi §t1))
            (tt2 (* noi §t2)))
        (if (proba (/(- (constraintspe ctime tt1 tt2) tt1) (- tt2 tt1 1))) 1 0))))

(defun passe (t1 t2 g1 g2)
  #'(lambda (stime ctime etime reverse)
      (let* ((tt1 (* noi §t1))
             (tt2 (* noi §t2))
             (coef (/ (- ctime tt1) (- tt2 tt1 1)))
             (a1 §g1)
             (a2 §g2)) 
        (constraint (round(+ a1 (* coef (- a2 a1))))(min a1 a2)(max a1 a2)))))
|#

(defun probint (t1 t2)
  #'(lambda (stime ctime etime reverse)  
      (let* ((c (if (< etime 5039) 1 noi)) ; pour pouvoir tester avec l au lieu de lnoi (11 03 05)
             (tt1 (* c §t1))
             (tt2 (* c §t2)))
        (if (proba (/(- (constraintspe ctime tt1 tt2) tt1) (- tt2 tt1 1))) 1 0))))

(defun passe (t1 t2 g1 g2)
  #'(lambda (stime ctime etime reverse)
     (let* ((c (if (< etime 5039) 1 noi)) ; pour pouvoir tester avec l au lieu de lnoi (11 03 05)
            (tt1 (* c §t1))
            (tt2 (* c §t2))
            (coef (/ (- ctime tt1) (- tt2 tt1 1)))
            (a1 §g1)
            (a2 §g2)) 
       (constraint (round(+ a1 (* coef (- a2 a1))))(min a1 a2)(max a1 a2)))))

(defun passep (t1 t2 g1 g2)
  (sel (probint t1 t2) g1 g2))


; passe de la valeur g1 à la valeur g2 à la date dat
; (l 10 (change °4 °10 °20)) --> (10 10 10 10 20 20 20 20 20 20)
;;----------------------------------------------------------------
(defun change (dat g1 g2)
  #'(lambda (stime ctime etime reverse)
      (let* ((c (if (< etime 5039) 1 noi)) ; pour pouvoir tester avec l au lieu de lnoi (11 03 05)
             (tt (* c §dat))
             (a1 §g1)
             (a2 §g2)) 
        (if (< ctime tt) a1 a2))))



;;----------------------------------------------------------------
;; Comme l mais avec des temps normalisés (noi)
;;----------------------------------------------------------------

(defun lnoi (n g)
  (let (l) 
    (dotimes (i n)
      (push (funcall g 0 (* i noi) (* n noi) nil) l))
    (nreverse l)))




;;==============================================================================
;; PASSIP (G1 G2)
;; Passe en probabilité de G1 à G2 sur tout l'interval de temps

;; Ex: (l 20 (passip °0 °30)) --> (0 0 0 30 0 30 0 30 0 30 30 0 0 0 30 30 30 30 30 30)
;;==============================================================================
#|
(defun probit ()
  #'(lambda (stime ctime etime reverse)
      (declare (ignore reverse))
      (let ((tt1  stime)
            (tt2  etime))
        (if (proba (/(- (constraintspe ctime tt1 tt2) tt1) (- tt2 tt1 1))) 1 0))))
|#
(defun probit ()
  #'(lambda (stime ctime etime reverse) 
      (declare (ignore reverse))
      (let* ((c (if (< etime 5039) 1 noi))  ; pour pouvoir tester avec l au lieu de lnoi (11 03 05)
             (tt1  (* c stime))
             (tt2  (* c etime)))
        (if (proba (/(- (constraintspe (* c ctime) tt1 tt2) tt1) (- tt2 tt1 1))) 1 0))))

(defun passip (g1 g2)
  (sel (probit) g1 g2))






;;==============================================================================
;;             PROBA01 (G1 G2)  
;; Génére des 0 avec la probabilité G1/(G1+G2)
;; et des 1 avec la probabilité G2/(G1 + G2)

;; Ex: (l 30 (prob01 °1 °2)) --> (0 1 0 0 1 0 1 0 1 1 1 1 1 0 0 1 1 1 0 1 0 1 1 1 0 1 1 0 1 0)
;; ici génére des 1 avec une probabilité 2 fois plus grande que pour les 0

;; Equivalent à (h °(§g1 fois 0 et §g2 fois 1)
;;==============================================================================

(defun prob01 (g1 g2)
  (h(concat (blk g1 °0) (blk g2 °1))))






;;==============================================================================
;;                     DIVERS GENERATEURS
;;==============================================================================



;;===============================================
;; GSOM (G)

;; Retourne cycliquement la somme cumulée d'une liste
;; exemple:
;; (l 2 (gsom °(1 2 3)))
;; (6 6)
;;===============================================

(defun gsom (g)
  (let ((i 0))
    #'(lambda (stime ctime etime reverse)
        (let ((s 0) (l §g))
          (dotimes (ii (length l) )
            (progn (setq s (+ s (car l)))
                   (setq l (cdr l))))
          (setq i (+ i 1))
          s))))


;;===============================================
;; REDUIT-G (T1 T2 L NB)

;; Réduit par la gauche la liste jusqu'à ce qu'elle
;; ne contienne plus que nb éléments.

;; exemple
;; (l 20 (reduit-g °6 °8 °(1 2 3 4 5 6) °2)) --> 
;; ((1 2 3 4 5 6) (1 2 3 4 5 6) (1 2 3 4 5 6) (1 2 3 4 5 6) 
;; (1 2 3 4 5 6) (1 2 3 4 5 6) (1 2 3 4 5 6) (3 4 5 6) 
;; (5 6) (5 6) (5 6) (5 6) (5 6) (5 6) (5 6) (5 6) (5 6) 
;; (5 6) (5 6) (5 6))
;;===============================================


(defun reduit-g (t1 t2 l nb)
  #'(lambda (stime ctime etime reverse)
      (let* ((c (if (< etime 5039) 1 noi)) ; pour pouvoir tester avec l au lieu de lnoi (11 03 05)
            (tt1 (* c §t1))
            (tt2 (* c §t2))
             (ll §l)
             (long (length ll))
             (d ctime)
             (reduc))
        (cond ((<= d tt1) (setq d tt1))
              ((>= d tt2) (setq d tt2))
              (t))
        (setq reduc (round(/(*(- long §nb)(- tt1 d))(- tt1 tt2))))
        (cond ((= 0 reduc) ll)
              ((>= reduc long)  (nthcdr  (- long 1) ll))
              ((< reduc 0) (append ll (make-list (- reduc) 
                                                 :initial-element (car (last ll)))) )
              (t (nthcdr  reduc ll))))))



;;===============================================
;; REDUIT-D (T1 T2 L NB)

;; Réduit par la droite la liste jusqu'à ce qu'elle
;; ne contienne plus que nb éléments.

;; exemple
;; (l 20 (reduit-d °6 °8 °(1 2 3 4 5 6) °2))  --> 
;; ((1 2 3 4 5 6) (1 2 3 4 5 6) (1 2 3 4 5 6) (1 2 3 4 5 6) 
;; (1 2 3 4 5 6) (1 2 3 4 5 6) (1 2 3 4 5 6) (1 2 3 4) 
;; (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2) 
;; (1 2) (1 2) (1 2))

;;===============================================

(defun reduit-d (t1 t2 l nb)
  #'(lambda (stime ctime etime reverse)
      (let* ((c (if (< etime 5039) 1 noi)) ; pour pouvoir tester avec l au lieu de lnoi (11 03 05)
            (tt1 (* c §t1))
            (tt2 (* c §t2))
             (ll (reverse §l))
             (long (length ll))
             (d ctime)
             (reduc))
        (cond ((<= d tt1) (setq d tt1))
              ((>= d tt2) (setq d tt2))
              (t))
        (setq reduc (round(/(*(- long §nb)(- tt1 d))(- tt1 tt2))))
        (cond ((= 0 reduc) (reverse ll))
              ((>= reduc long)  (reverse (nthcdr  (- long 1) ll)))
              ((< reduc 0) (reverse (append ll (make-list (- reduc) 
                                                 :initial-element (car (last ll)))) ))
              (t (reverse(nthcdr  reduc ll)))))))




;;===============================================
;; AUGMENTE-G (T1 T2 L NB)

;; Augmente par la gauche la liste à partir des
;; nb premiers éléments de droite.

;; exemple
;; (l 10 (augmente-g °0 °10 °(1 2 3 4 5 6) °2))
;; ((5 6) (5 6) (4 5 6) (4 5 6) (3 4 5 6) (3 4 5 6)
;; (3 4 5 6) (2 3 4 5 6) (2 3 4 5 6) (1 2 3 4 5 6))
;;===============================================

(defun augmente-g (t1 t2 l nb)
  #'(lambda (stime ctime etime reverse)
      (let* ((c (if (< etime 5039) 1 noi)) ; pour pouvoir tester avec l au lieu de lnoi (11 03 05)
            (tt1 (* c §t1))
            (tt2 (* c §t2))
             (ll §l)
             (long (length ll))
             (d ctime)
             (reduc))
        (cond ((<= d tt1) (setq d tt1))
              ((>= d tt2) (setq d tt2))
              (t))
        (setq reduc (round(/(*(- long §nb)(- tt2 d))(- tt2 tt1))))
        (cond ((= 0 reduc) ll)
              ((>= reduc long)  (nthcdr  (- long 1) ll))
              ((< reduc 0) (append ll (make-list (- reduc) 
                                                 :initial-element (car (last ll)))) )
              (t (nthcdr  reduc ll))))))




;;===============================================
;; AUGMENTE-D (T1 T2 L NB)

;; Augmente par la droite la liste à partir des
;; nb premiers éléments de gauche. 
;; exemple
;; (l 10 (augmente-d °0 °10 °(1 2 3 4 5 6) °2))
;; ((1 2) (1 2) (1 2 3) (1 2 3) (1 2 3 4) (1 2 3 4) 
;; (1 2 3 4) (1 2 3 4 5) (1 2 3 4 5) (1 2 3 4 5 6))

;;===============================================

(defun augmente-d (t1 t2 l nb)
  #'(lambda (stime ctime etime reverse)
      (let* ((c (if (< etime 5039) 1 noi)) ; pour pouvoir tester avec l au lieu de lnoi (11 03 05)
            (tt1 (* c §t1))
            (tt2 (* c §t2))
             (ll (reverse §l))
             (long (length ll))
             (d ctime)
             (reduc))
        (cond ((<= d tt1) (setq d tt1))
              ((>= d tt2) (setq d tt2))
              (t))
        (setq reduc (round(/(*(- long §nb)(- tt2 d))(- tt2 tt1))))
        (cond ((= 0 reduc) (reverse ll))
              ((>= reduc long)  (reverse (nthcdr  (- long 1) ll)))
              ((< reduc 0) (reverse (append ll (make-list (- reduc) 
                                                 :initial-element (car (last ll)))) ))
              (t (reverse(nthcdr  reduc ll)))))))



;;==============================================================================
;;                            DIVERS PROBABILITÉ
;;==============================================================================

(defun passec (gp)
  #'(lambda (stime ctime etime reverse)
      (let* ((tt1  stime)
             (tt2  etime)
             (p (/(- (constraintspe ctime tt1 tt2) tt1) (- tt2 tt1))))
        (if (proba (+ 0.1 (expt p §gp))) 1 0))))

;(l 20 (passec °100))

(defun passed (gp)
  #'(lambda (stime ctime etime reverse)
      (let* ((tt1  stime)
             (tt2  etime)
             (p (/(- tt2 (constraintspe ctime tt1 tt2)) (- tt2 tt1))))
        (if (proba (+ 0.1 (expt p §gp))) 1 0))))

;(l 20 (passed °100))


(defun gproba2 (g )
  (call °proba (mult  (i °1 g) (i °1 g) (i °1 g))))

(defun gexpt (gv gp)
  (call °expt gv gp))

;;(l 30 (gpgate (gexpt (i °0.9 °0.25) °4) °1 °0))

(defun passexpd (g1 g2)
(gpgate (seq °0.95 °0.4 °0.25 °0.18 °0.12 °0.08) g1 g2))

(defun passexpc (g1 g2)
(gpgate (seq °0.9 °0.85 °0.75 °0.6 °0.1 °0.05) g1 g2))




;;==============================================================================
;;                EXEMPLES D'UTILISATION DE GENERATEURS
;;==============================================================================


#|
(l 20 (gbrown °0 °1 °2 °1))
(l 20 (gbrown °5 °10 °20 (gbrown °0 °1 °2 °1)))
(l 1 (gmode °60 °72 °maj))
(l 10 (sel (rnd °0 °1) °0 °1))
(l 10 (sel (rnd °0 °1) (gmode °60 °72 °maj) (gmode °20 °32 °maj)))
(l 10 (sel °1 °0 °1))
(l 10 (gmode °0 °10 °(1 2)))
(l 10 (h (lst (g 1))))
(l 10 (rnd °10 °100))
(l 10 (gbrown °60 °48 (osc °40 °72 °49) (h °(2 -3))))
(l 10 (h °(1 2 5 0.5)))
(l 12 (s °(1 2 5)))
(l 5 (lst °(1  2)))

(l 5 (rdv °max (h °(1 2 3 4 5 6 7 8 9 10 11 12 13 14))))
(l 5 (rdv °max (h (l 30 (rdv °+ °1)))))
(l 35 (mem (h °(1 2 3 4 5)) °2 °7))
(l 10 (osc °2 °0 °10))
(l 20 (sinus °6))


(l 10 (h (g maj)))
(l 10 (h (g maj)))
(l 10 (add °1 (s (g maj))))
(l 10 (add °0 (h (g maj))))
(l 10 (add °60 (pf °0 (rdv °+ (s °( 1 1 1 2))))))

|#

