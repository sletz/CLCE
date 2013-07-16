;;==================================================================================================================
;;
;;             PROCÉDURES PERMETTANT LE PASSAGE DE LA BARRE DE MESURE PAR UN N-OLET (yo)
;;
;;==================================================================================================================





;; transformation des liaisons dans un rythme
;; mise en notation infix des '+'
;;--------------------------------------------------

(defun infix-liaisons (rythme)
  (cond ((numberp rythme) (list rythme))
        ((eq '+ (car rythme)) (append (infix-liaisons (second rythme)) (cons '+ (infix-liaisons (third rythme)))))
        (t (list rythme))))

;; (infix-liaisons '(+ 2 (+ (* (FDUR 2 3 D-N) 1) (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) 1))))

;; diffusion des attributs
;; passe les attributs de note dans les rythmes
;;--------------------------------------------------

(defun diffuse-attribut1 (att er)
  (cond ((numberp er) `(note ,er ,att))
              ((eq '+ er) er)
              ((eq '* (car er)) (append (butlast er) (list `(note ,(car (last er)) ,att))))))

(defun diffuse-attributs (att lr)
  (mapcar #'(lambda (er) (diffuse-attribut1 att er)) lr))

;; (diffuse-attributs 'att (infix-liaisons '(+ 2 (+ (* (FDUR 2 3 D-N) 1) (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) 1)))))


;; rend les attributs d'une note (tout sauf le rythme)
;;----------------------------------------------------
(defun get-attr (ev)
  `(attributs ,(first ev) ,(second ev) ,(fourth ev) ,(fifth ev) ,(sixth ev) ,(seventh ev) ,(eighth ev) ,(ninth ev)))



;; transformation d'une liste d'événements
;;--------------------------------------------

(defun reorg-lev (lev)
  (apply #'append (mapcar #'(lambda (ev) (diffuse-attributs (get-attr ev) (infix-liaisons (third ev)))) lev)))


;;(reorg-lev '((0 0 (+ 2 (+ (* (FDUR 2 3 D-N) 1) (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) 1))) (84) (255) 100 NIL NIL 1) (28/9 0 (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) 1/2) (85) (255) 110 NIL NIL 1) (10/3 0 (* (FDUR 2 3 D-N) 1) (86) (255) 120 NIL NIL 1)))


;; donne la durée d'un événement nouvelle manière
;;-----------------------------------------------

(defun dur-note (n)
  (cond ((eq '+ n) 0)
        ((eq '% n) 0)
        ((eq 'note (first n)) (abs (second n)))
        ((eq '* (first n)) (* (dur-coef (second n)) (apply #'+ (mapcar #'dur-note (cddr n)))))))

;;(dur-note '(* (FDUR 2 3) (* (FDUR 2 3) (note 10))))

(defun dur-coef (coef)
  (/ (second coef) (third coef)))

(defun dur-coef-list (l)
  ;;(print (list "durcoef list" l))
  (if (null (cdr l))
    (second (car l))
    (* (dur-coef (car l)) (dur-coef-list (cdr l)))))

;; (dur-note '(NOTE 10 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1)))
  

;; factorise les nolets
;;-------------------------


(defun dur-of-note (n)
  (cond ((eq '+ n) 0)
        ((eq 'note (first n)) (abs (second n)))
        ((eq '* (first n)) (dur-coef-list (cdr n)))))

(defun group-dur (du futur &optional past)
  (if (<= du 0) (list (nreverse past) futur)
      (let* ((e (car futur))
             (d (dur-of-note e)))
        (group-dur (- du d) (cdr futur) (cons e past)))))

;; donne la durée totale d'un nolet à partir de son premier élément
;;-----------------------------------------------------------------
(defun dur-tot-nolet (evnm)
  (let ((coef (second evnm)))
    (* (second coef) (eval (fourth coef)))))

;;(dur-tot-nolet '(* (FDUR 2 3 D-C) (NOTE 1 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1))))

;;(group-dur (+ 2 2/3) '((NOTE 2 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1)) + (* (FDUR 2 3 D-N) (NOTE 1 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1))) + (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) (NOTE 1 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1))) (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) (NOTE 1/2 (ATTRIBUTS 28/9 0 (85) (255) 110 NIL NIL 1))) (* (FDUR 2 3 D-N) (NOTE 1 (ATTRIBUTS 10/3 0 (86) (255) 120 NIL NIL 1)))))

;; remonte (dans les nolets) un element de nolet
;;-----------------------------------------------
(defun remonte (e)
  (cond ((eq '+ e) e)
        ((= 3 (length e)) (third e))
        (t (cons '* (cddr e)))))

;;(remonte '+)
;;(remonte '(* (FDUR 2 3 D-N) (NOTE 1 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1))))
;;(remonte '(* (FDUR 2 3 D-N) (FDUR 2 3 D-C) (NOTE 1 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1))))

;;remonte nolet
;;-------------------
(defun remonte-nolet (g)
  (mapcar #'remonte g))



(defun factorise-nolets (lenm &optional past)
  ;;(print (list 'factorise-nolets lenm past))
  (if (null lenm) (nreverse past)
      (let ((e (car lenm)))
        ;;(print (list "e=" e))
        (if (and (listp e) (eq '* (first e)))
          (let* ((dtn (dur-tot-nolet e))
                 (cp (group-dur dtn lenm))
                 (gr (first cp))
                 (lenm2 (second cp))
                 (factor (second (first gr)))
                 )
              ;;(print (list '>>> 'factorise-nolets dtn gr lenm2 factor))
            (factorise-nolets lenm2 (cons (cons '* (cons factor (factorise-nolets (remonte-nolet gr)))) past)))
          (factorise-nolets (cdr lenm) (cons e past))))))


(defun paj2yo (lev)
  (factorise-nolets (reorg-lev lev)))
#|
(reorg-lev '((0 0 (+ 2 (+ (* (FDUR 2 3 D-N) 1) (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) 1))) (84) (255) 100 NIL NIL 1)))

(paj2yo '((0 0 (+ 2 (+ (* (FDUR 2 3 D-N) 1) (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) 1))) (84) (255) 100 NIL NIL 1) 
            (28/9 0 (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) 1/2) (85) (255) 110 NIL NIL 1) 
            (10/3 0 (* (FDUR 2 3 D-N) 1) (86) (255) 120 NIL NIL 1)))


(reorg-lev '((0 0 (+ 2 (+ (* (FDUR 2 3 D-N) 1) (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) 1))) (84) (255) 100 NIL NIL 1) 
             (28/9 0 (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) 1/2) (85) (255) 110 NIL NIL 1) 
             (10/3 0 (* (FDUR 2 3 D-N) 1) (86) (255) 120 NIL NIL 1)))

(factorise-nolets '((NOTE 2 att) 
                    + (* (FDUR 2 3 D-N) (NOTE 1 att)) 
                    + (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) (NOTE 1 att)) 
                    (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) (NOTE 1/2 att)) 
                    (* (FDUR 2 3 D-N) (NOTE 1 att))))


(paj2yo '((0 0 (+ 2 (+ (* (FDUR 2 3 D-C) 1) (+ (* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) 1/2) (+ (* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) 1/4) (* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) 1/4))))) (84) (255) 100 NIL NIL 1) 
            (28/9 0 (* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) 1/2) (85) (255) 110 NIL NIL 1) 
            (10/3 0 (* (FDUR 2 3 D-C) 1) (86) (255) 120 NIL NIL 1)))

;;--> ((NOTE 2 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1)) + (* (FDUR 2 3 D-C) (NOTE 1 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1)) + (* (FDUR 2 3 D-DC) (NOTE 1/2 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1)) + (NOTE 1/4 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1)))) + (* (FDUR 2 3 D-C) (* (FDUR 2 3 D-DC) (NOTE 1/4 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1)) (NOTE 1/2 (ATTRIBUTS 28/9 0 (85) (255) 110 NIL NIL 1))) (NOTE 1 (ATTRIBUTS 10/3 0 (86) (255) 120 NIL NIL 1))))


(factorise-nolets '((* (FDUR 2 3 D-N) (NOTE 1 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1))) 
                    (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) (NOTE 1 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1))) 
                    (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) (NOTE 1/2 (ATTRIBUTS 28/9 0 (85) (255) 110 NIL NIL 1))) 
                    (* (FDUR 2 3 D-N) (NOTE 1 (ATTRIBUTS 10/3 0 (86) (255) 120 NIL NIL 1)))) )

(mapcar #'dur-note '((* (FDUR 2 3 D-N) (NOTE 1 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1))) 
                     (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) (NOTE 1 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1))) 
                     (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) (NOTE 1/2 (ATTRIBUTS 28/9 0 (85) (255) 110 NIL NIL 1))) 
                     (* (FDUR 2 3 D-N) (NOTE 1 (ATTRIBUTS 10/3 0 (86) (255) 120 NIL NIL 1)))) )



(group-dur 2 '((* (FDUR 2 3 D-N) (NOTE 1 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1))) 
                     (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) (NOTE 1 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1))) 
                     (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) (NOTE 1/2 (ATTRIBUTS 28/9 0 (85) (255) 110 NIL NIL 1))) 
                     (* (FDUR 2 3 D-N) (NOTE 1 (ATTRIBUTS 10/3 0 (86) (255) 120 NIL NIL 1)))) )

|#





;; decoupe une duree en deux parties en utilisant des divisions binaires
;;----------------------------------------------------------------------

(defun binary-split (x d  &optional left right)
  ;;(print (list x d left right))
  (cond ((< d 1/16) (list (nreverse (cons x left)) right))
        ((= x (* 2 d)) (list (nreverse (cons d left)) (cons d right)))
        ((< x (* 2 d)) (let ((m (/ x 2))) (binary-split m (- d m) (cons m left) right)))
        ((> x (* 2 d)) (let ((m (/ x 2))) (binary-split m d left (cons m right))))
        ((<= x d) (list (nreverse (cons x left)) right))))

(binary-split 1 3/4)
;;-> ((1/2 1/4) (1/4))


(binary-split 1 1/3)
;;-> ((1/4 1/16 1/16) (1/8 1/2))


;; fabrique des notes liées a partir d'une liste de durées et d'un attribut
;;-------------------------------------------------------------------------
(defun fabrique-liaison (att ld)
  (if (null ld) ld
      (if (null (cdr ld))
        (list `(note ,(car ld) ,att))
        (cons `(note ,(car ld) ,att) (cons '+ (fabrique-liaison att (cdr ld)))))))

;;(fabrique-liaison 'toto '(1/4 1/16 1/16))
;; -> ((NOTE 1/4 TOTO) + (NOTE 1/16 TOTO) + (NOTE 1/16 TOTO))



(defun est-note? (n) (eq 'note (car n)))
(defun est-nolet? (n) (eq '* (car n)))


;; descent tous les coefs d'un nolet (pour apres decoupage)
;;----------------------------------------------------------

(defun descent-nolet-coef (a)
 ;(print (list 'descent-nolet-coef a))
 (cond ((eq '+ a) a)
        ((eq '% a) a)
        ((eq '* (car a)) `(* ,(descent-fdur (second a)) ,@(mapcar #'descent-nolet-coef (cddr a))))
        (t a)))


(defun descent-fdur (fd)
  (append (butlast fd) (list (descent-unite-coef (car (last fd))))))

(defun descent-unite-coef (uc)
  (cond ((eq uc 'd-b) 'd-n)
        ((eq uc 'd-n) 'd-c)
        ((eq uc 'd-c) 'd-dc)
        ((eq uc 'd-dc) 'd-tc)
        ((eq uc 'd-tc) 'd-qc)
        (t (error "dans descent-unite-coef"))))

;; divise un nolet en placant la coupure '% à la vrai date d
;;----------------------------------------------------------

(defun divise-nolet (n d)
  (cond ((est-note? n) 
         (let* ((attr (third n))
                (dd (second n))
                (du (abs dd))
                (cp (binary-split du d))
                (l1 (mapcar  #'(lambda (x) (* x (signum dd)))  (first cp)))
                (l2 (mapcar  #'(lambda (x) (* x (signum dd))) (second cp)))
                )
           (append (fabrique-liaison attr l1) (cons '% (fabrique-liaison attr l2)))))
        ((est-nolet? n)
         `((* ,(second n) ,@(divise-list-nolet (cddr n) (/ d (dur-coef (second n)))))))
        ((eq '+ n) (error "dans divise nolet"))))

(defun divise-list-nolet (ln d &optional (dc 0) past)
  ;(print (list 'divise-list-nolet ln d dc past "*************"))
  (cond ((<= d 0)  (nreverse (cons '% past)))
        ((null ln) (nreverse (cons '% past)))
        ((<= (dur-note (car ln)) d) (divise-list-nolet (cdr ln) (- d (dur-note (car ln))) (+ dc (dur-note (car ln))) (cons (car ln) past)))
        (t (append (nreverse past) (divise-nolet (car ln) d) (cdr ln)))))


;;(divise-nolet  '(* (FDUR 2 3 D-DC) (NOTE 1/2 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1)) + (NOTE 1/4 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1))) 1/2)          



;; algorithme qui coupe les arbres
;;--------------------------------

(defun coupe-arbre (a)
  (cond ((eq '% a) '(coupure nil nil))
        ((eq '+ a) a)
        ((eq 'note (car a)) a)
        ((eq '* (car a)) (groupe-branches (second a) (mapcar #'coupe-arbre (cddr a))))))


(defun groupe-branches (coef lb &optional gauche)
  (if (null lb) 
    `(* ,coef ,@(nreverse gauche))
    (let ((b (car lb)))
      (if (and (listp b) (eq 'coupure (first b)))
        (let ((x (second b))
              (y (third b)))
          ;(print (list "coupe-branches *************************************" x y))
          (cond ((and (null x) (null y)) `(coupure (* ,coef ,@(nreverse gauche)) (* ,coef ,@(cdr lb))))
                ((null x) `(coupure (* ,coef ,@(nreverse gauche)) (* ,coef ,@(cons y (cdr lb)))))
                ((null y) `(coupure (* ,coef ,@(nreverse (cons x gauche))) (* ,coef ,@(cdr lb))))
                (t `(coupure (* ,coef ,@(nreverse (cons x gauche))) (* ,coef ,@(cons y (cdr lb)))))))
        (groupe-branches coef (cdr lb) (cons (car lb) gauche))))))
            
        
;;(coupe-arbre '(* coef1 (note 10)  (note 20)  (* coef2 (note 1) % (note 2)  (note 3))  (note 30)))
;; -> (COUPURE (* COEF1 (NOTE 10) (NOTE 20) (* COEF2 (NOTE 1))) (* COEF1 ((* COEF2 ((NOTE 2) (NOTE 3))) (NOTE 30))))

;;(divise-nolet  '(* (FDUR 2 3 D-DC) (NOTE 1/2 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1)) + (NOTE 1/4 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1))) 1/4)
;;(coupe-arbre (descent-nolet-coef (divise-nolet  '(* (FDUR 2 3 D-DC) (NOTE -1/2 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1)) + (NOTE 1/4 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1))) 1/4)))
;;->(COUPURE (* (FDUR 2 3 D-DC) (NOTE 1/4 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1)) + (NOTE 1/8 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1))) (* (FDUR 2 3 D-DC) ((NOTE 1/8 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1)) + (NOTE 1/4 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1)))))


;; prend une liste d'événements et découpes les nolets suivant les barres de mesure
;;---------------------------------------------------------------------------------


(defun coupe-levnm (lev lmes &optional (dc 0) (past))
(print `(coupe-levnm ,lev ,lmes ,dc ,past))
  (if (or (null lmes) (null lev)) (nreverse past)
      (let* ((e (car lev))
             (m (car lmes))
             (du (dur-note e))
             (df (+ dc du)))
        (cond ((> df m)
               (let ((cp (coupe-arbre (descent-nolet-coef (monitor1 (divise-nolet e (- m dc)))))))
                 (print (list "<<<<<<<<" cp))
                 (coupe-levnm (cons (third cp) (cdr lev)) (cdr lmes) 0 (cons '+ (cons (second cp) past)))))
              ((= df m)
               (coupe-levnm (cdr lev) (cdr lmes) 0 (cons e past)))
              (t 
               (coupe-levnm (cdr lev) lmes df (cons e past)))))))

(defun monitor1 (l) (print (list "monitor1" l))
  ;; objet cense etre un singleton
  (if (cdr l) (error "monitor1") (car l)))

(defun yo2paj (levnm)
  (rythmize (distribuer-coef-fdur (datation levnm))))


;; calcul de dates
;;----------------

(defun datation (levnm &optional (dc 0) (coef 1))
;(print `(datation ,levnm ,dc ,coef))
  (if (null levnm) levnm
      (let ((e (car levnm)))
        (cond ((eq '+ e) (cons e (datation (cdr levnm) dc coef)))
              ((eq '% e) (cons e (datation (cdr levnm) dc coef)))
              ((eq 'note (car e)) (cons `(note ,(second e) (attributs ,dc ,@(cddr (third e)))) (datation (cdr levnm) (+ dc (* coef (abs (second e)))) coef)))
              ((eq '* (car e)) (cons `(* ,(second e) ,@(datation (cddr e) dc (* coef (dur-coef (second e))))) (datation (cdr levnm) (+ dc (* coef (dur-note e))) coef)))))))
#|
;;(datation '((NOTE 1/2 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1)) (* (FDUR 2 3 D-DC) (* (FDUR 2 3 D-DC) (NOTE 1/2 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1)))) (NOTE 1 (ATTRIBUTS 0 0 (12) (255) 100 NIL NIL 1))))

 + (NOTE 1/4 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1)) (NOTE 1/4 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1)))))
|#

(defun distribuer-coef-fdur (levnm)
  (nreverse (distribuer-coef levnm nil nil)))

(defun distribuer-coef (levnm lcoef past)
  (if (null levnm)
    past
    (let ((e (car levnm)))
      (cond ((eq '+ e) 
             (distribuer-coef (cdr levnm) lcoef (cons '+ past)))
            ((eq 'note (car e)) 
             (distribuer-coef (cdr levnm) lcoef (cons (if lcoef (cons '* (revappend lcoef (list e))) e) past)))
            ((eq '* (car e))
             (distribuer-coef 
              (cdr levnm) 
              lcoef 
              (distribuer-coef (cddr e) (cons (second e) lcoef) past)))))))
#|

;;(distribuer-coef-fdur '((note 0) (* A (note x) (* B (note y) (note z)) (note u)) (note v)))
;;(distribuer-coef-fdur '((note 0) (note x) (note y) (note z) (note u) (note v)))
  
            
          
;(defun prefix-liaisons (lev &optional pile past)
;  (if (null lev) (nreverse (cons (addition pile) past))
;      (if (eq '+ (car lev))
;        (prefix-liaisons (cddr lev) (cons (second lev) pile) past)
;        (if pile (prefix-liaisons lev nil (cons (addition pile) past))
;            (if (cdr lev)
;              (if (eq '+ (second lev))
;                (prefix-liaisons (cdr lev) (list (car lev)) past)
;                (prefix-liaisons (cdr lev) nil (cons (car lev) past)))
;              (nreverse (cons (car lev) past)))))))
;
;(defun addition (ln &optional res)
;  (if (null ln) res
;      (if (null res)
;        (addition (cdr ln) (car ln))
;        (addition (cdr ln) `(+ ,(car ln) ,res)))))

;;(addition '(3 2 1))

;;(prefix-liaisons '(1 + 2 ))



(paj2yo '((0 0 (+ 2 (+ (* (FDUR 2 3 D-N) 1) (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) 1))) (84) (255) 100 NIL NIL 1) 
            (28/9 0 (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) 1/2) (85) (255) 110 NIL NIL 1) 
            (10/3 0 (* (FDUR 2 3 D-N) 1) (86) (255) 120 NIL NIL 1)))

;;-> ((NOTE 2 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1)) + (* (FDUR 2 3 D-N) (NOTE 1 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1)) + (* (FDUR 2 3 D-C) (NOTE 1 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1)) (NOTE 1/2 (ATTRIBUTS 28/9 0 (85) (255) 110 NIL NIL 1))) (NOTE 1 (ATTRIBUTS 10/3 0 (86) (255) 120 NIL NIL 1))))

(datation '((NOTE 2 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1)) + (* (FDUR 2 3 D-N) (NOTE 1 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1)) + (* (FDUR 2 3 D-C) (NOTE 1 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1)) (NOTE 1/2 (ATTRIBUTS 28/9 0 (85) (255) 110 NIL NIL 1))) (NOTE 1 (ATTRIBUTS 10/3 0 (86) (255) 120 NIL NIL 1)))))
;;-> ((NOTE 2 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1)) + (* (FDUR 2 3 D-N) (NOTE 1 (ATTRIBUTS 2 0 (84) (255) 100 NIL NIL 1)) + (* (FDUR 2 3 D-C) (NOTE 1 (ATTRIBUTS 8/3 0 (84) (255) 100 NIL NIL 1)) (NOTE 1/2 (ATTRIBUTS 10/3 0 (85) (255) 110 NIL NIL 1))) (NOTE 1 (ATTRIBUTS 11/3 0 (86) (255) 120 NIL NIL 1))))






((NOTE 2 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1)) 
 + (* (FDUR 2 3 D-N) (NOTE 1 (ATTRIBUTS 2 0 (84) (255) 100 NIL NIL 1))) 
 + (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) (NOTE 1 (ATTRIBUTS 8/3 0 (84) (255) 100 NIL NIL 1))) 
 (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) (NOTE 1/2 (ATTRIBUTS 10/3 0 (85) (255) 110 NIL NIL 1))) 
 (* (FDUR 2 3 D-N) (NOTE 1 (ATTRIBUTS 11/3 0 (86) (255) 120 NIL NIL 1))))


(yo2paj (paj2yo '((0 0 (+ 2 (+ (* (FDUR 2 3 D-N) 1) (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) 1))) (84) (255) 100 NIL NIL 1) 
                  (28/9 0 (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) 1/2) (85) (255) 110 NIL NIL 1) 
                  (10/3 0 (* (FDUR 2 3 D-N) 1) (86) (255) 120 NIL NIL 1))))

((0 0 (+ 2 (+ (* (FDUR 2 3 D-N) 1) (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) 1))) (84) (255) 100 NIL NIL 1) 
 (10/3 0 (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) 1/2) (85) (255) 110 NIL NIL 1) 
 (11/3 0 (* (FDUR 2 3 D-N) 1) (86) (255) 120 NIL NIL 1))


;(2 + 2/3n (1 + 2/3c (1 1/2) 1))

(paj2yo '((0 0 (+ 2 (+ (* (FDUR 2 3 D-N) 1) (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) 1))) (84) (255) 100 NIL NIL 1) 
          (28/9 0 (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) 1/2) (85) (255) 110 NIL NIL 1) 
          (10/3 0 (* (FDUR 2 3 D-N) 1) (86) (255) 120 NIL NIL 1)))




((0 1 (+ 2 (+ (* (FDUR 2 3 D-N) 1) (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) 1))) (84) (255) 100 NIL NIL 1) 
(10/3 4 (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) 1/2) (85) (255) 110 NIL NIL 1) 
(11/3 5 (* (FDUR 2 3 D-N) 1) (86) (255) 120 NIL NIL 1))



(coupe-levnm '((NOTE 2 (ATTRIBUTS 0 1 (84) (255) 100 NIL NIL 1)) 
               + 
               (* (FDUR 2 3 D-N) 
                  (NOTE 1 (ATTRIBUTS 0 2 (84) (255) 100 NIL NIL 1)) 
                  + 
                  (* (FDUR 2 3 D-C) 
                     (NOTE 1 (ATTRIBUTS 0 3 (84) (255) 100 NIL NIL 1)) 
                     (NOTE 1/2 (ATTRIBUTS 28/9 4 (85) (255) 110 NIL NIL 1))) 
                  (NOTE 1 (ATTRIBUTS 10/3 5 (86) (255) 120 NIL NIL 1))))
             '(3 4))


(distribuer-coef-fdur '((NOTE 2 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1)) 
                        + (* (FDUR 2 3 D-N) (NOTE 1 (ATTRIBUTS 2 0 (84) (255) 100 NIL NIL 1)) 
                             + (* (FDUR 2 3 D-C) 
                                  (NOTE 1 (ATTRIBUTS 8/3 0 (84) (255) 100 NIL NIL 1)) 
                                  (NOTE 1/2 (ATTRIBUTS 10/3 0 (85) (255) 110 NIL NIL 1))) 
                             (NOTE 1 (ATTRIBUTS 11/3 0 (86) (255) 120 NIL NIL 1)))))
|#
;; nouvelle version de prefix liaison reconstituant le rythme des notes liées
;;---------------------------------------------------------------------------

(defun get-rythm (evi)
  (cond ((eq 'note (car evi)) (second evi))
        ((eq '* (car evi)) (cons '* (append (butlast (cdr evi)) (list (second (car (last evi)))))))
        (t (error "dans get-ryhtm"))))

(defun get-of-attr (evi)
  (cond ((eq 'note (car evi)) (third evi))
        ((eq '* (car evi)) (get-of-attr (car (last evi))))
        (t (print (list "get of attr" evi))(error "dans get-of-attr"))))

(defun make-of-note (attr rythme)
  (cons (second attr) (cons (third attr) (cons rythme (cdddr attr)))))

(defun testxyz (ev)
  (make-of-note (get-attr ev) (get-rythm ev)))

;;(testxyz '(* (FDUR 2 3 D-C) (NOTE 1 (ATTRIBUTS 10 0 (84) (255) 100 NIL NIL 1))))


(defun rythmize (lev)
  ;(print `(rythmize ,lev))
  (if lev
    (let ((e (car lev)))
      (prefix-liaisons (cdr lev) (get-of-attr e) (list (get-rythm e)) nil))))

(defun prefix-liaisons (lev attr pile past)
  ;(print `(prefix-liaisons (,lev ,attr ,pile ,past)))
  (if (null lev) (nreverse (cons (make-of-note attr (addition pile)) past))
      (if (eq '+ (car lev))
        (prefix-liaisons (cddr lev) attr (cons (get-rythm (second lev)) pile) past)
        (let ((e (car lev)))
          (prefix-liaisons (cdr lev) (get-of-attr e) (list (get-rythm e)) (cons (make-of-note attr (addition pile)) past))))))
 
(defun addition (ln &optional res)
  (if (null ln) res
      (if (null res)
        (addition (cdr ln) (car ln))
        (addition (cdr ln) `(+ ,(car ln) ,res)))))

#|
(rythmize '((NOTE 2 (ATTRIBUTS 0 0 (84) (255) 100 NIL NIL 1)) 
            + (* (FDUR 2 3 D-N) (NOTE 1 (ATTRIBUTS 2 0 (84) (255) 100 NIL NIL 1))) 
            + (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) (NOTE 1 (ATTRIBUTS 8/3 0 (84) (255) 100 NIL NIL 1))) 
            (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) (NOTE 1/2 (ATTRIBUTS 28/9 0 (85) (255) 110 NIL NIL 1))) 
            (* (FDUR 2 3 D-N) (NOTE 1 (ATTRIBUTS 10/3 0 (86) (255) 120 NIL NIL 1)))))

;;(addition '(3 2 1))

;;(prefix-liaisons '(1 + 2 ))

(prepare-nolet '((0 0 (+ 2 (+ (* (FDUR 2 3 D-N) 1) (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) 1))) (84) (255) 100 NIL NIL 1) 
                  (28/9 0 (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) 1/2) (85) (255) 110 NIL NIL 1) 
                  (10/3 0 (* (FDUR 2 3 D-N) 1) (86) (255) 120 NIL NIL 1))
               '(3 1))
|#

;;---------------------------------------------------------------------------

(defun prepare-nolet (levof lmes)
  (let* ((levnm (paj2yo levof))
         (levcut (coupe-levnm levnm lmes)))
    (yo2paj levcut)))







