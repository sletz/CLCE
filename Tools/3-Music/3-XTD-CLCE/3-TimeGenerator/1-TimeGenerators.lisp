;;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;;
;;
;;                          GENERATEURS FONCTIONNELS TEMPORELS
;;
;;                               YO/GRAME/FEVRIER 93
;;
;;
;; HISTORIQUE :
;;  21-02-93, premiere version d'apres Generator.lisp
;;  23-02-93, refonte complete
;;  13-10-94, modification de rond-reader
;;  02-11-94, rendu "compatible ascendant" avec l'ancien Generator.lisp
;;  04-11-94, rajoute FLOO
;;  06-12-94, Reprise du fichier TimeGenerators-3.lisp 
;;  23-07-96, Suppression de GNRPARAM, GNOTE, GCHORD, GVOL car definis ailleurs 
;;  13-12-96, Ajout gosc et gosc2 de PAJ
;;  20-01-00, Ajout de la fonction freeze qui genere tjrs la premiere valeur d'un generateur
;;  30-11-01, Correction d'un bug dans mem YO
;;
;;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\


;;                                   initialisations
;;========================================================================================


(eval-when (:compile-toplevel :load-toplevel :execute)

  ;; ------------------------------- § : macro caractere 
  
  (defun funcall-reader (stream char)
    (declare (ignore char))
    (list 'funcall (read stream t nil t) 'stime 'ctime 'etime 'reverse))
  
  (set-macro-character #\§ #'funcall-reader)
  
  ;; ------------------------------- ° : macro caractere 
  
  (defun rond-reader (stream char)
    (declare (ignore char))
    (let ((l (read stream t nil t)))
      (if (listp l)
        (list 'G (list 'quote l))
        (list 'G (rond-val l)))))
  
  (defun rond-val (x)
    (if (and (symbolp x) (fboundp x))
      (list 'symbol-function (list 'quote x))
      x))
  
  (set-macro-character #\° #'rond-reader)

)

;;________________________________________________________
;;                    COLLECTEURS
;;________________________________________________________


;; ------------------------------- L : cree une liste de n valeurs 

(defun L (n g)
  (let (l) 
    (dotimes (i n)
      (push (funcall g 0 i n nil) l))
    (nreverse l)))



;; ------------------------------- TAKE : prend le nieme 

(defun take (n g)
  (dotimes (i (- n 1)) 
    (funcall g 0 i n nil))
  (funcall g 0 (- n 1) n nil))

;;________________________________________________________
;;                    GENERATEURS SIMPLES
;;________________________________________________________

;; ------------------------------- G : cree une generateur de constantes 

(defun G (x)
  #'(lambda (stime ctime etime reverse) 
      (declare (ignore stime ctime etime reverse)) 
      x))

;; ------------------------------- LST : combine en une liste

(defun lst (&rest lg)
  #'(lambda (stime ctime etime reverse)
      (mapcar #'(lambda (g) §g) lg)))

;; ------------------------------- APPL : applique comme apply

(defun appl (g1 g2)
  #'(lambda (stime ctime etime reverse)
      (apply §g1 §g2)))


;; ------------------------------- CALL : applique comme funcall

(defun call (g &rest lg)
  (appl g (apply #'lst lg)))


;;________________________________________________________
;;                  OPERATEURS SUR LES NOMBRES
;;________________________________________________________


;; ------------------------------- PROBA : donne t avec une probabilite <n>
;;                                         entre 0 et 1

(defun proba (n)
  (> (* n 1000) (random 1000)))


;; ------------------------------- ALEA : ajoute un petit alea <e> 
;;                                        a une valeur <n>

(defun alea (nn e)
  (if (zerop e)
    nn
    (+ nn (- e) (random (+ (* 2 e) 1)))))


;; ------------------------------- CONTRAINT : contraint une valeur <x> 
;;                                             entre deux valeurs <min> et <max>

(defun constraint (x min max)
  (cond ((< x min) min)
        ((> x max) max)
        (t (round x))))


;; ------------------------------- REPLIE : replie une valeur <x> 
;;                                          entre deux valeurs <min> et <max>

(defun replie (x min max)
  (cond ((< x min) (replie (- (+ min min) x) min max))
        ((> x max) (replie (- (+ max max) x) min max))
        (t (round x))))


;; ------------------------------- SISYPHE : replace une valeur <x> 
;;                                           entre deux valeurs <min> et <max>

(defun sisyphe (x min max ecart)
  (cond ((< x min) (sisyphe (+ x ecart) min max ecart))
        ((> x max) (sisyphe (- x ecart) min max ecart))
        (t (round x))))


;; ------------------------------- TORE : replace une valeur <x> 
;;                                        entre deux valeurs <min> et <max>

(defun tore (x min max)
  (if (= min max) 
    min
    (let* ((d (- max min))
           (d2 (+ d d))
           (vv (mod (- x min) d2)))
      (if (> vv d) (- max (- d2 vv)) (+ min vv)))))


;;________________________________________________________
;;                    GENERATEURS TEMPORELS DE BASE
;;________________________________________________________


;; ------------------------------- sync : synchronise un generateur

(defun sync (gst gct get grev g)
  #'(lambda (stime ctime etime reverse)
      (funcall g §gst §gct §get §grev)))

;; ------------------------------- stime : date de debut

(defun stime ()
  #'(lambda (stime ctime etime reverse)
      (declare (ignore ctime etime reverse))
      stime))

;; ------------------------------- ctime : date courante

(defun ctime ()
  #'(lambda (stime ctime etime reverse)
      (declare (ignore stime etime reverse))
      ctime)) ;; a revoir


;; ------------------------------- etime : date de fin

(defun etime ()
  #'(lambda (stime ctime etime reverse)
      (declare (ignore stime ctime reverse))
      etime))


;; ------------------------------- isreverse? : date de fin

(defun isreverse? ()
  #'(lambda (stime ctime etime reverse)
      (declare (ignore stime ctime etime))
      reverse))


;;________________________________________________________
;;                    GENERATEURS TEMPORELS DERIVES
;;________________________________________________________

;; ------------------------------- boucle : boucle un generateur

(defun boucle (g)
  #'(lambda (stime ctime etime reverse)
      (funcall g stime (+ stime (mod (- ctime stime) (- etime stime))) etime reverse)))

;; ------------------------------- ntime : temps normalise

(defun ntime ()
  (i °0 °1))

;; ------------------------------- i : interpolation

(defun i (g1 g2)
  #'(lambda (stime ctime etime reverse)
      (let ((v1 (if reverse §g2 §g1))
            (v2 (if reverse §g1 §g2)))
        (if (= etime stime) v1
            (/ (+ (* v1 (- etime ctime)) 
                  (* v2 (- ctime stime))) 
               (- etime stime))))))

;; ------------------------------- rev : renversement du temps

(defun rev (g)
  #'(lambda (stime ctime etime reverse)
      (funcall g stime ctime etime (not reverse))))

;; ------------------------------- trep : repetition du temps

(defun trep (gr g)
  (setq g (boucle g))
  #'(lambda (stime ctime etime reverse)
      (funcall g stime (* ctime §gr) etime reverse)))

;; ------------------------------- seq : sequence

(defun seq (&rest lg)
  (let ((nn (length lg)))
    #'(lambda (stime ctime etime reverse)
        (let* ((dt (/ (- etime stime) nn))
               (p (if reverse 
                    (- nn 1 (floor (- ctime stime) dt)) 
                    (floor (- ctime stime) dt)))
               (ct (+ stime (mod (- ctime stime) dt))))
        (funcall (nth p lg) stime ct (+ stime dt) reverse))) ))


;;________________________________________________________
;;                       COMBINATEURS NON TEMPORELS
;;________________________________________________________


;; ------------------------------- S : serialise des listes

(defun s (g)
  (let (l)
    (if (typep g 'function)
      #'(lambda (stime ctime etime reverse)
          (unless l (setq l §g))
          (pop l))
      #'(lambda (stime ctime etime reverse)
          (declare (ignore stime ctime etime reverse))
          (unless l (setq l g))
          (pop l)) )))

;; ------------------------------- BORNE : borne un nombre entre 2 valeurs

(defun borne (gb1 gb2 gval)
  #'(lambda (stime ctime etime reverse) 
        (let ((b1 §gb1)(b2 §gb2)(val §gval))
          (cond ((< val b1) (+ b1 b1 (- val)))
                ((> val b2) (+ b2 b2 (- val)))
                (t val)))))

(defun ecrete (gb1 gb2 gval)
  #'(lambda (stime ctime etime reverse) 
        (let ((b1 §gb1)(b2 §gb2)(val §gval))
          (cond ((< val b1) b1)
                ((> val b2) b2)
                (t val)))))
        
;; ------------------------------- H : fonction aleatoire

(defun h (g)
  (if (typep g 'function)
    #'(lambda (stime ctime etime reverse) 
        (let* ((l §g)
               (n (length l)))
          (elt l (random n))))
    (let ((n (length g)))
      #'(lambda (stime ctime etime reverse) 
          (declare (ignore stime ctime etime reverse))
          (elt g (random n))) )))
        
;; ------------------------------- HS : Epuise aleatoirement une liste

(defun hs (g)
  (let (l nn)
    #'(lambda (stime ctime etime reverse)
        (unless l 
          (setq l §g) 
          (setq nn (length l)))
        (let ((vv (elt l (random nn))))
          (setq l (remove vv l :count 1))
          (decf nn)
          vv))))
        

;; ------------------------------- CB : combinaisons d'une liste
(defun combinaisons (l)
  (if (null l)
    (list nil)
    (let ((p (combinaisons (cdr l)))
          (rr))
      (dolist (e p)
        (push e rr)
        (push (cons (car l) e) rr))
      rr)))
      
(defun cb (g)
  #'(lambda (stime ctime etime reverse)
      (combinaisons §g)))
        

;; ------------------------------- NONIL : enleve les valeurs nil d'un generateur
(defun nonil (g)
  #'(lambda (stime ctime etime reverse)
      (do ((vv §g §g))
          ((not (null vv)) vv)
        )))
        
;; ------------------------------- RND : fonction aleatoire

(defun rnd (g1 g2)
  #'(lambda (stime ctime etime reverse) 
        (let* ((v1 §g1)
               (v2 §g2))
          (+ (min v1 v2) (random (+ 1 (floor (abs (- v1 v2)))))))))

;; ------------------------------- SEL : selection un generateur

(defun sel (g1 &rest lg)
  #'(lambda (stime ctime etime reverse)
      §(elt lg (round §g1))))

;; ------------------------------- SELR : selection d'un resultat de generateur

(defun selr (g1 &rest lg)
  #'(lambda (stime ctime etime reverse)
      (elt (mapcar #'(lambda (g) §g) lg) (round §g1))))

;; ------------------------------- ALT : alterne les generateurs

(defun alt (&rest lg)
  (let (l)
    #'(lambda (stime ctime etime reverse)
        (unless l (setq l lg))
        §(pop l))))

;; ------------------------------- RALT : RALT(x::l) <=> ALT(x, RALT(l))

(defun ralt (&rest lg)
  (if (< (length lg) 2)
    (car lg)
    (alt (car lg) (apply #'ralt (cdr lg)))))

;; ------------------------------- LALT : LAT(x::l) <=> ALT(LALT(l), x)

(defun lalt (&rest lg)
  (if (< (length lg) 2)
    (car lg)
    (alt (apply #'lalt (cdr lg)) (car lg))))

;; ------------------------------- RDG : reduit par la gauche

(defun rdg (g1 g2)
  #'(lambda (stime ctime etime reverse)
        (reduce §g1 §g2)))

;; ------------------------------- RDD : reduit par la droite

(defun rdd (g1 g2)
  #'(lambda (stime ctime etime reverse)
        (reduce §g1 §g2 :from-end t)))

;; ------------------------------- RDV : reduit verticalement

(defun rdv (g1 g2)
  (let ((start t)
        (prev))
  #'(lambda (stime ctime etime reverse)
      (if start
        (setq start nil prev §g2)
        (setq prev (funcall §g1 prev §g2)))
      prev)))

;; ------------------------------- dif : evite deux valeurs identiques de suite

(defun dif (g)
  (let ((prev (gensym)))
    #'(lambda (stime ctime etime reverse)
        (let (curr)
          (loop 
            (setq curr §g)
            (if (not (equal curr prev))
              (return)))
          (setq prev curr)
          curr))))

;; ------------------------------- PF : prefix les valeurs de g2 par une valeur de g1

(defun pf (g1 g2)
  (let ((start t))
  #'(lambda (stime ctime etime reverse) 
      (if start
        (progn 
          (setq start nil)
          §g1)
        §g2))))

(defun freeze (g)
  (let ((start t)
        val)
  #'(lambda (stime ctime etime reverse) 
      (when start
        (setq start nil)
        (setq val §g))
        val)))

;; ------------------------------- PL : posfix les valeurs de g1 par une valeur de g2

(defun pl (g1 g2)
  #'(lambda (stime ctime etime reverse) 
      (if (= (+ 1 ctime) etime)
        §g2
        §g1)))

;; ------------------------------- BLK : liste verticalement

(defun blk (g1 g2)
  #'(lambda (stime ctime etime reverse)
      (let (l)
        (dotimes (i (floor §g1))
          (push §g2 l))
        (nreverse l))))

;; ------------------------------- REP : repetition de valeurs

(defun rep (gval grep)
  (let ((vv nil)
        (rr 0))
    #'(lambda (stime ctime etime reverse)
        (when (<= rr 0)
          (setq rr §grep)
          (setq vv §gval))
        (decf rr)
        vv)))

;; ------------------------------- MEM : source, taille de boucle, vit. renouvlmnt

(defun putlast (e l)
  (rplacd (last l) (list e))
  l)

(defun mem (src size chg)
  (let ((n1 0) 		;; nombre d'échantillons accumulés dans l1
        (n2 0)		;; nombre d'échantillons accumulés dans l2
        (l1 nil)	;; premiere liste d'accumulation
        (l2 nil)	;; seconde liste d'accumulation
        (decompte 0)	;; quand à zero chercher une valeur de la source
        )
    
    #'(lambda (stime ctime etime reverse)
        (let ((y) (d (- (floor §size) 1)))
          (decf decompte)
          (if (or (<= decompte 0) (>= d (+ n1 n2)))
            (progn
              ;; cas ou l'on joue la source directe
              (setq y §src)
              (if (<= decompte 0) (setq decompte §chg)) )
            (progn 
              ;; cas ou l'on joue la valeur mémorisée
              (setq y (if (< d n1) (nth d l1) (nth (- d n1) l2))) )
            )
          (push y l1)
          (incf n1)
          (when (> n1 64)
            (setq n2 n1)
            (setq l2 l1)
            (setq n1 0)
            (setq l1 nil))
          y))))

;; ------------------------------- ADD : addition

(defun add (&rest lg)
  (appl °+ (apply #'lst lg)))

;; ------------------------------- SUB : soustraction

(defun sub (&rest lg)
  (appl °- (apply #'lst lg)))

;; ------------------------------- MULT : multiplication

(defun mult (&rest lg)
  (appl °* (apply #'lst lg)))

;; ------------------------------- DIV : division

(defun div (&rest lg)
  (appl °/ (apply #'lst lg)))

;; ------------------------------- FLOO : rend entier

(defun floo (val)
  #'(lambda (stime ctime etime reverse)
      (floor §val)))

;; ------------------------------- FRAG : fragmentation par division

(defun frag (val div)
  (let ((cc 0) (vv 0))
    #'(lambda (stime ctime etime reverse)
        (when (< cc 1)
          (setq cc §div)
          (setq vv (/ §val cc)))
        (decf cc)
        vv)))

;; ------------------------------- SHIFT : decale les valeurs d'un generateur (max 255)

(defun shift (g1 g2)
  (let ((tab (make-array 256))
        (head 0)
        (startp t))
    #'(lambda (stime ctime etime reverse)
        (when startp
          (dotimes (i 256)
            (setf (aref tab i) §g2))
          (setq startp nil))
        (let ((res (aref tab (logand 255 (+ head §g1)))))
          (setf (aref tab head) §g2)
          (setq head (logand 255 (+ head 1)))
          res))))

;; ------------------------------- FREQ : change la frequence d'1 gen.

(defun freq (g1 g2)
  (let ( (cc 0)  (vv) (startp t))
    #'(lambda (stime ctime etime reverse)
        (when startp 
          (setq startp nil)
          (setq vv §g2))
        (when (<= cc 1)
          (incf cc §g1))
        (do ()
            ((<= cc 1))
          (decf cc)
          (setq vv §g2) ) 
        vv)))

;; ------------------------------- MINI : minimum

(defun mini (&rest lg)
  (appl °min (apply #'lst lg)))

;; ------------------------------- MAXI : maximum

(defun maxi (&rest lg)
  (appl °max (apply #'lst lg)))

;; ------------------------------- GOR : ou logique

(defun gor (&rest lg)
  #'(lambda (stime ctime etime reverse)
      (some #'(lambda (x) x) (mapcar #'(lambda (g) §g) lg))))

;; ------------------------------- GAND : et logique

(defun gand (&rest lg)
  #'(lambda (stime ctime etime reverse)
      (every #'(lambda (x) x) (mapcar #'(lambda (g) §g) lg))))


;; ------------------------------- GNOT : non logique

(defun  gnot (g)
  (appl °not g))

(defun  ggnot (g)
  #'(lambda (stime ctime etime reverse)
      (not §g)))

;; ------------------------------- GMOD : modulo

(defun  gmod (g1 g2)
  #'(lambda (stime ctime etime reverse)
      (mod §g1 §g2)))


;; ------------------------------- GATE : vrai pendant n1 faux pendant n2

(defun gate (g1 g2 &optional phase)
  (if phase
    (shift phase (gate g1 g2))
    (s (call °append (blk g1 °t) (blk g2 °nil)))))

;; ------------------------------- GPGATE : porte probabiliste

(defun  gpgate (gp gv1 &optional (gv2 °nil))
  #'(lambda (stime ctime etime reverse)
      (if (> (* §gp 1000) (random 1000))
        §gv1
        §gv2)))

;; ------------------------------- PULSE : vrai tous les n, faux sinon

(defun pulse (g &optional phase)
  (if phase
    (shift phase (pulse g))
    (s (call °cons °t (blk (call °- g °1) °nil)))))

; (l 10 (pulse °3))

;; ------------------------------- GBROWN : generateur de valeurs brownien

(defun gbrown (init min max delta)
  (call °tore (call °+ init (rdv °+ delta)) min max))

(defun gbrown2 (init min max delta)
  (call °tore (rdv °+ (pf init delta)) min max))

;; ------------------------------- GREPLIE : replie des valeurs (cf replie)

(defun greplie (gx gmin gmax)
  #'(lambda (stime ctime etime reverse)
      (let ((min §gmin)
            (max §gmax)
            (x §gx))
        (cond ((< x min) (replie (- (+ min min) x) min max))
              ((> x max) (replie (- (+ max max) x) min max))
              (t (round x))))))

;; ------------------------------- SINUS : generateur de sinus

(defun sinus (period)
  (let ((x 0)
        (2pi (+ pi pi)))
    #'(lambda (stime ctime etime reverse)
        (sin (incf x (/ 2pi §period))))))

;; ------------------------------- OSC : oscillateur sinus

(defun osc (period min max)
  (let ((x 0)
        (2pi (+ pi pi)))
    #'(lambda (stime ctime etime reverse)
        (let ((m §min))
          (+ m (* (- §max m) (/ (+ 1 (sin (incf x (/ 2pi §period)))) 2)))))))

;; ------------------------------- GOSC (PAJ) : oscille entre 2 valeurs

(defun gosc  (init min max delta)
  (greplie (call °+ init (rdv °+ (pf °0 delta))) min max))

;; ------------------------------- GOSC2 (PAJ) : oscille entre 2 valeurs
;;                                                 avec respect du pas

(defun changesigne? (a b)
(if (>= (* a b) 0) nil t))

(defun gosc2 (ginit gmin gmax gpas)
  (let ((start t)
        (prev)
        (monte)
        (ancienpas))
    #'(lambda (stime ctime etime reverse)
        (let ((min §gmin)
              (max §gmax)
              (pas §gpas)
              (prov))
          (if (< max min)
            (setq prov min min max max prov))
          (if start
            (setq start nil prev (replie §ginit min max) ancienpas pas monte (>= pas 0))
            (progn (if (changesigne? pas ancienpas)
                     (setq ancienpas pas monte (>= pas 0)))
                   (if (= min max) (setq prev min)
                       (if monte (if (> (+ prev (abs pas)) max)
                                   (progn (setq monte nil)
                                          (if (= prev max) (setq prev (max (- prev (abs pas)) min))
                                              (setq prev max)))
                                   (setq prev (+ prev (abs pas))))
                           (if (< (- prev (abs pas)) min) 
                             (progn (setq monte t)
                                    (if (= prev min) (setq prev (min (+ prev (abs pas)) max))
                                        (setq prev min)))
                             (setq prev (- prev (abs pas))))))))) prev)))

;; ------------------------------- PBLK : blocks symboliques

(defun pblk (gpat gval)
  (let (l)
    #'(lambda (stime ctime etime reverse)
        (unless l (setq l (instancie gpat gval stime ctime etime reverse)))
        (pop l))))

(defun instancie (gpat gval stime ctime etime reverse &aux env)
  (declare (special gval env stime ctime etime reverse))
  (labels ((foo (x)
                (declare (special gval env stime ctime etime reverse))
                (cond ((null x) x)
                      ((symbolp x)
                       (unless (assoc x env) (setq env (acons x §gval env)))
                       (cdr (assoc x env)))
                      ((atom x) x)
                      ((consp x) (mapcar #'foo x)))) )
    (mapcar #'foo §gpat)))

;; ex : (l 10 (pblk (g '((x y)(y x y))) (rdv °+ °1)))
;; ex : (l 10 (pblk °((x y)(y x y)) (rdv °+ °1)))

;; ------------------------------- PERM : interpolation par permutation 

(defun perm (gl)
  (labels ((permlist (nn)
                 (do ((l nil)
                      (i nn (- i 1)))
                     ((zerop i) l)
                   (do ((x (random nn) (random nn)))
                       ((not (member x l)) (push x l) l))))
           (exch (l1 l2 nn)
                 (if (zerop nn)
                   (cons (car l2) (cdr l1))
                   (cons (car l1) (exch (cdr l1) (cdr l2) (- nn 1))) )))
    (let ((cc nil)
          (s nil)
          (p nil)
          (startp t))
      #'(lambda (stime ctime etime reverse)
          (when startp
            (setq s §gl)
            (setq startp nil))
          (when (null p)
            (setq cc s)
            (setq s §gl)
            (setq p (permlist (length cc))))
          (setq cc (exch cc s (pop p)))) )))

;; (l 10 (perm (alt °(1 2 3 4 5) °(a b c d e))))

(provide 'time-generator)
