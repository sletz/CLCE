;;============================================================================================================================
;;       CALCUL DU FICHIER ENIGMA A PARTIR DE L'ENSEMBLE DES *LISTE.n*
;;                  PIERRE ALAIN JAFFRENNOU - SEPTEMBRE 1996
;;       Mthode:
;;        1- Rarangement des listes *liste.n*
;;        2- Production des sous fichiers ENIGMA (via DECODE) canal par canal, layer par layer
;;        3- Construction du fichier final ENIGMA par concatnation des fichiers prcdents
;;           avec les fichiers contenus dans le dossier enigma includes du dossier MCL5.0
;;
;;============================================================================================================================
;;
;; Modifi le 16 nov 1996: prepa-liste; decode; finale
;; Modifi en octobre 1997 pour introduire la possibilit de coder les accords
;; Suppression *dur-mesure* le 18 12 97

;; (novembre 97) Cration d'un cinquime champs Enigma compos d'une liste (modif du 27 08 01 pour afficher Gliss dans FINAL):
;; (0): premire et dernire grace note (grace note unique)     
;; (1): premire grace note
;; (2): seconde grace note
;; etc .... 
;; (99): dernire grace note
;; (253 puis arguments: liste des ecars liste des dures): marque de glissando  ; ajout du 27 04 01
;; (254): marque de staccato
;; (255): note normale (non grace note)

;; rajout le 18 12 2000:
;; variable *long-mes* qui rgle la longueur de toute les mesures dans FINALE

;; Modifications de Mai 2001 (Finale2001d):
;; le dossier enigma includes doit se trouver au mme niveau que MCL:
;; (defparameter *enigma-dir* "ccl:enigma includes;")

;; Modifications de 27 08 2001 (Finale2001d):
;; Transformation du 5eme champ des listes en une liste pour introduire la notation des glissandi

;; Pour raison de simplification: Modification en Mai 2001 des fonctions FINALE, DECODE et PREPARE-LISTE (NOM)
;; Pour calculer le fichier ENIGMA il suffit dsormais de faire: (finale "essai")
;; Pour tester le fichier  dcoder en FINALE, il suffit de faire (prepa-liste)
;; La variable intermdiaire t1 qui stockait (prepa-liste), n'est plus utilise
;; re-nouveau prepa-liste
;; Modification de write-enigma-file

;; Modifications de 12 11 2001 (Finale2001d):
;; Introduction d'un paramtre optionnel :layer

;; Modification de Mars 2005
;; Refonte importante pour prendre en compte finale2004
;; Cration de fonctions avec le suffixe 01 (pour Finale 2001) ou 04 (Finale 2004)
;; Cration des dossiers D2001 et D2004 dans le dossier ENIGMA INCLUDES

;; Modification de Avril 2012
;; ajout du parametre optionnel :layersynthe
;; qui permet de piloter les layers de l'chantilloneur machFive
;; et d'crire une expression dans la partition
;;==============================================================================




;;==============================================================================
;;            DFINITION ET INITIALISATION DES VARIABLES
;;==============================================================================



(defparameter *long-mes* 4)
(defvar *noire* 1024)


(defvar *liste-note* nil) 
(defvar *liste-portee* nil)
(defvar *liste-mesures* nil)
(defvar *liste-decription-mesures* nil)
(defvar *liste-de-portee-clef* nil)
(defvar *liste-nuplet* nil)
(defvar *liste-des-GF* nil)          ;; codage des mesures
(defvar *liste-des-IM* nil)          ;; codage des marques d'articulation (signes d'accent, de staccato ...)
(defvar *liste-des-TL* nil)          ;; pour coder les mesures composites pour FINALE
(defvar *liste-des-TU* nil)          ;; pour coder les mesures composites pour FINALE
(defvar *liste-position-portee* nil)
(defvar *liste-des-veloc* nil)
(defvar *liste-des-tete* nil)        ;; code la forme des tte de note dans le cas de micro-interval  31 08 01
(defvar *liste-des-expressions*)     ;; code les expressions (mf, piz ...)
(defvar *liste-barres-mesures* nil)  ;; liste des time-signature de la pice
(defvar *copy-liste-bm* nil)         ;; copie de la liste prcdente pour chaque canal (decode-chan)


(defvar *cur-pos* 0)  ;; position courante  l'intrieur de la mesure
(defvar *cur-code* '8)
(defvar *num-mes-FR* 0) ;; indique par mesure et par layer le num du premier et dernier vnement
;;(defvar *dur-mesure* 0)
(defvar *cur-portee* 0)
(defvar *cur-n* 0)   ;; indice du tableau eE de finale
(defvar *first-ev* 1)
(defvar *last-ev* 1)

(defvar *premier* nil)
(defvar *dernier* nil)
(defvar *vrai-dernier* nil)

(defvar *vrai-dur* nil)
(defvar *dur-symb* nil)
(defvar *silence* nil)
;(defvar *mesure-vide* nil)
(defvar *num-canal-mesure* 0)


(defvar *precedent* nil)
(defvar *suivant* nil)
(defvar *liaison-explic* nil)
(defvar *liaison* nil)

;(defvar *fin-de-dur* nil)
;(defvar *debut-de-dur* nil)

(defvar *n-uplet-1* nil)
(defvar *n-uplet-2* nil)
(defvar *1-en-cours* nil)
(defvar *2-en-cours* nil)
(defvar *total1* 0)
(defvar *total2* 0)
(defvar *limite1* 0)
(defvar *limite2* 0)

(defvar *dur-uplet1* 0)
(defvar *dur-uplet2* 0)

(defvar *codage* nil)   ;; variable locale  code-nuplet

(defvar *end-mesure* nil)




(defvar *num-uplet1*)
(defvar *num-uplet2*)


(defvar d-b 2)
(defvar d-n 1)
(defvar d-c 1/2)
(defvar d-dc 1/4)
(defvar d-tc 1/8)
(defvar d-qc 1/16)

(defun rythm-suivant (r)
  (let ((v (assoc r '((d-b d-n)
                      (d-n d-c)
                      (d-c d-dc)
                      (d-dc d-tc)
                      (d-tc d-qc)
                      (d-qc nil)))))
    (second v)))

(defvar *pos-cour* -1)               ;; reprsente (first item) cad la date du groupe rythmique
(defvar *mes-non-finie* nil)


(defun init-codage()
  (setq
   *liste-des-TL* nil
   *liste-des-TU* nil
   *liste-note* nil
   *liste-portee* nil
   *liste-mesures* nil
   *liste-decription-mesures* nil
   *liste-de-portee-clef* nil
   *liste-position-portee* nil
   *liste-nuplet* nil
   *liste-des-GF* nil
   *liste-des-IM* nil
   *liste-des-veloc* nil
   *liste-des-tete* nil
   *liste-des-expressions* nil
   *cur-pos* 0
   *cur-code* '8
   *num-mes-FR* 0
   *cur-n* 0
   *first-ev* 1
   *last-ev* 1
   d-b 2
   d-n 1
   d-c 1/2
   d-dc 1/4
   *n-uplet-1* nil
   *n-uplet-2* nil
   *1-en-cours* nil
   *2-en-cours* nil
   *total1* 0
   *total2* 0
   *limite1* 0
   *limite2* 0
   *num-uplet1* 1
   *num-uplet2* 1
   *dur-uplet1* 0
   *dur-uplet2* 0
   *codage* nil
   ;;*codage2* nil
   ;;*coeff* 1
   *end-mesure* nil
   *cur-portee* 0
   *silence* nil
   *vrai-dur* nil
   *dur-symb* nil
   ;*mesure-vide* nil
   ;;*inter-portee* -220
   *liaison* nil
   *pos-cour* -1
   *mes-non-finie* nil
   *ref-FR* 1
   
   ))


;;==============================================================================
;;  OUTILS NCESSAIRES POUR LE CALCUL DE LA LISTE DES MESURES  APPLIQUER
;;  Janvier 20001
;;==============================================================================


;; ----------------------------------------------------------------------------------
;; Rend la liste de rythme commune  2 listes de rythmes
;; Fonction utilise pour trouver la liste des longueur de mesure dans FINALE
;;
;; Contrairement  l'habitude un n-olet de dure n est not -n (triolet de noire: -2)
;; et un silence de dure n est not (n)
;;
;; (mixrythm0 '(1 -2 1) '(2 2)) --> (1 -2 1)
;; (mixrythm0 '(1 -2 4) '(2 2 (6))) --> (1 -2 4)
;; ----------------------------------------------------------------------------------


(defun mixrythm0 (l1 l2 &optional (d 0) (si nil))
  ;(print (list "mixrythm0" l1 l2 d si))
  (if (and l1 l2)
    (cond ((or (listp (car l1)) (listp (car l2))) (specialsil l1 l2 d t))
          ((or (< (car l1) 0) (< (car l2) 0)) (if (> d 0) (cons d (nonsecable l1 l2 0)) (nonsecable l1 l2 0)))
          (t (secable l1 l2 d si)))))

;; -----------------------------

(defun specialsil (l1 l2 d si)
  ;(print (list "specialsil" l1 l2 d si))
  (let ((x (car l1))
        (y (car l2)))
    (cond  ((and (listp x) (listp y)) 
            (cond ((< (car y) (car x)) (cons (list (car y)) (mixrythm0 (cons (list (- (car x) (car y))) (cdr l1)) (cdr l2) 0 nil)))
                  ((< (car x) (car y)) (cons (list (car x)) (mixrythm0 (cdr l1) (cons (list (- (car y) (car x))) (cdr l2)) 0 nil)))
                  (t (cons (list (car x)) (mixrythm0 (cdr l1) (cdr l2) 0 nil)))))
           ((and (listp x) (numberp y)) 
            (cond ((> y 0) 
                   (cond ((> (car x) (abs y)) 
                          (cons (+ d (abs y)) (mixrythm0 (cons (list (- (car x) (abs y))) (cdr l1)) (cdr l2) 0 nil)))
                         ((< (car x) (abs y))
                          (mixrythm0 (cdr l1) (cons (- (abs y) (car x)) (cdr l2)) (+ d (car x)) nil))
                         (t (cons (+ d (abs y)) (mixrythm0 (cdr l1) (cdr l2) 0 nil)))))
                  
                  ((< y 0) (if (> (car x) (abs y)) 
                             (cons y (mixrythm0 (cons (list (+ (car x) y)) (cdr l1)) (cdr l2) 0 nil))
                             (nonsecable (cons (car(car l1)) (cdr l1)) l2  d)))))
           (t (specialsil l2 l1 d si)))))

;; -----------------------------

(defun nonsecable (l1 l2 d)
  ;(print (list "nonsecable" l1 l2 d))
  (let ((si (or (listp (car l1)) (listp (car l2))))
        (x (if (numberp (car l1)) (car l1) (car(car l1))))
        (y (if (numberp (car l2)) (car l2) (car(car l2)))))
    (cond  ((and (< x 0) (< y 0)) (cond  ((< x y) (nonsecable (cons (- x y) (cdr l1)) (cdr l2) (+ d y)))
                                         ((< y x) (nonsecable (cdr l1) (cons (- y x) (cdr l2)) (+ d x)))
                                         ((= y x) (cons (+ d x) (mixrythm0 (cdr l1) (cdr l2))))))
           ((and (< x 0) (>= y 0)) (cond ((= (- x) y) (cons (+ d x) (mixrythm0 (cdr l1) (cdr l2))))
                                        ((< (- x) y) (if si (cons (+ d x) (mixrythm0 (cdr l1) (cons (list (+ y x)) (cdr l2))))
                                                         (cons (+ d x) (mixrythm0 (cdr l1) (cons (+ y x) (cdr l2))))))
                                        ((> (- x) y) (nonsecable (cons (+ x y) (cdr l1)) (cdr l2) (- d y)))))
           (t (nonsecable l2 l1 d)))))

;; -----------------------------

(defun secable (l1 l2 d si)
  ;(print (list "secable" l1 l2 d si))
  (let ((x (car l1))
        (y (car l2)))
    (cond  ((> x y) (mixrythm0 (cons (- x y) (cdr l1)) (cdr l2) (+ d y)))
           ((> y x) (mixrythm0 (cdr l1) (cons (- y x) (cdr l2)) (+ d x)))
           ((= y x) (if si (cons (list (+ d x)) (mixrythm0 (cdr l1) (cdr l2)))
                        (cons (+ d x) (mixrythm0 (cdr l1) (cdr l2))))))))


;; -------------------------------------------
;; Complete une liste de rythme l
;; sur une dure d avec du silence
;;
;; (completer 10 '(1 -1 -3)) --> (1 -1 -3 (5))
;; -------------------------------------------

(defun completer (d l)
  (if (null l) 
    (if (> d 0) (list (list d)) nil)
    (if (numberp (car l)) (cons (car l) (completer (- d (abs (car l))) (cdr l)))
        (cons (car l) (completer (- d (abs (car (car l)))) (cdr l))))))
 
(defun abscar (v)
  (cond ((listp v) (abs(car v)))
        ((numberp v) (abs v))
        (t (print "ERREUR DANS ABSCAR"))))

;; -------------------------------------------
;; Complete la dure d'une liste de rythme l
;;
;; (calcdur '(2 -3 (4)))
;; -------------------------------------------

(defun calcdur (l) (reduce #'+ (mapcar #'abscar l)))

;;==============================================================================
;;             MIXER-RYTHME
;; Rend la liste de rythme commune  2 listes de rythmes aprs compltude des listes
;; Fonction utilise pour trouver la liste des longueur de mesure dans FINALE
;;
;; Contrairement  l'habitude un n-olet de dure n est not -n (triolet de noire: -2)
;; et un silence de dure n est not (n)
;;
;; (mixer-rythme '((2) -2) '(1 -2)) --> (1 -3)
;; (mixer-rythme '(-2 (2) -2 2 (2) 1 (2) -1 -2 (2) 2) '(3 -2 2 (2) 1 1 (2) -1 -1 1 (1) 1 1))
;; --> (-2 1 -3 2 (1) 1 1 (2) -1 -2 (1) 1 2)
;;==============================================================================

(defun mixer-rythme (l1 l2)
  (let ((m (max (calcdur l1) (calcdur l2))))
    (mixrythm0 (completer m l1) (completer m l2))))

;;==============================================================================
;;             SUPERMIXRYTHM
;; Comme mixer-rythme mais tendu  une liste de liste de rythmes
;; Fonction utilise pour trouver la liste des longueur de mesure dans FINALE
;;
;; Contrairement  l'habitude un n-olet de dure n est not -n (triolet de noire: -2)
;; et un silence de dure n est not (n)
;;
;; (supermixrythm '((1 -2 1 3 -2) (2 -2 -2 2) (1 1 1 1 1 1 1))) --> (1 -3 -2 1 -2)
;; (supermixrythm '((-2 (2) -2 2 (2) 1 (2) -1 -2 (2) 2) 
;;                    (3 -2 2 (2) 1 1 (2) -1 -1 1 (1) 1 1) (-2)))
;; --> (-2 1 -3 2 (1) 1 1 (2) -1 -2 (1) 1 2)
;;==============================================================================

(defun supermixrythm (ll)
  (reduce #'mixer-rythme ll))

;;==============================================================================
;;             CHERCHE-MESURE
;;
;; 9 janvier 2001
;; retourne la liste des mesures d'une partition
;;  partir d'une liste des valeurs de mesure admises
;; et de la liste cumule des notes, silences et n-uplets
;; 
;;==============================================================================

(defun cherche-mesure0 (lpref l)
  ;(print (list 'cherche-mesure0 lpref l))
  (cond ((null lpref) (print "ERREUR DANS CHERCHE-MESURE0"))
        ((null l) nil)
        (t (cherche-mesure lpref l))))

;; ote les valeurs non entire d'une liste:

(defun ote-non-entier (lpref)
  (cond ((null lpref) nil)
        ((integerp (car lpref)) (cons (car lpref) (ote-non-entier (cdr lpref))))
        (t (ote-non-entier (cdr lpref)))))

(defun cherche-mesure (lpref l)
  ;(print (list 'cherche-mesure lpref l))
  (let ((lnpref (sort (ote-non-entier lpref) #'<)))
    (if (finliste (car (last lnpref)) l)     ; ------- si le total des segments de la liste des dcoupage est <=  la plus gde val entire de la liste des prfrences
      (list (derniere-mesure lnpref (calcdur l)))
      (let ((res (trouve-point lpref lpref l)))
        (if res res
            (coupe lpref l))))))

;;==============================================================================
;;             FINLISTE
;; retourne vrai si la somme des abscar de l <= v, nil sinon
;; (finliste 5 '( 1 (3) -1)) --> t
;; (finliste 5 '( 1 2 1)) --> t 
;; (finliste 5 '( 1 3 1 2)) --> nil
;; (finliste 5 '( 1 6 1)) --> nil  
;;==============================================================================

(defun finliste (v l &optional (d 0))
  ;(print (list 'finliste v l d))
  (if (null l) t
      (if (> (+ d (abscar (car l))) v) nil
          (finliste v (cdr l) (+ d (abscar (car l)))))))

;;==============================================================================
;;             DERNIERE-MESURE
;; traitement de la derniere mesure:
;; retourne la plus petite valeur entire de la liste des prfrences
;; > au total de la liste des dcoupage restant  traiter 
;; (derniere-mesure '(3 4 5) 9/2) --> 5
;; (derniere-mesure '(3 4 5) 7/2) --> 4
;;==============================================================================

(defun derniere-mesure (lref longl)
  ;(print (list 'derniere-mesure lref longl))
  (cond ((null lref) (print "ERREUR DANS DERNIERE-MESURE"))
        ((<= longl (car lref)) (car lref))
        (t (derniere-mesure (cdr lref) longl))))

;; -----------------------------------------------
;; cherche si une borne de la liste l coincide avec
;; une valeur de ln = '(4 3 2 1 5 1/2 3/2 5/2 7/2 9/2) 
;; liste modifie le 28 11 01 '(4 3 2 5 7/2 9/2 5/2 3/2 1 1/2)
;; -----------------------------------------------

(defun trouve-point (ln lnbis l)
  ;(print (list 'trouve-point ln l))
  (if (null ln) nil
      (let ((res (trouve-un-point ln lnbis (car ln) l)))
        (if res res
      (trouve-point (cdr ln) lnbis l)))))

(defun trouve-un-point (ln lnbis n l &optional (d 0))
  ;(print (list 'trouve-un-point n l))
  (if (null l) (print "ERREUR DANS TROUVE-UN-POINT")
      (cond ((< (+ d (abscar (car l))) n) (trouve-un-point ln lnbis n (cdr l) (+ d (abscar (car l)))))
            ((= (+ d (abscar (car l))) n) (cons n (cherche-mesure0 lnbis (cdr l))))
            (t nil))))

;; -----------------------------------------------
;; Il faut couper: priorit aux coupes de silence
;; -----------------------------------------------

(defun coupe (ln l)
  ;(print (list 'coupe ln l))
  (if (null ln) (print "ERREUR DANS COUPE")
      (let ((res (coupe-silence ln l)))
        (if res res
            (let ((res2 (coupe-note ln l)))
              (if res2 res2
                  (grande-mesure ln (maxliste(ote-non-entier ln)) l)))))))

;; -----------------------------------------------
;; on cherche  couper les silences
;; -----------------------------------------------

(defun coupe-silence (ln l &optional (d 0))
  ;(print (list 'coupe-silence ln l d))
  (if (null l) nil
      (cond ((listp (car l)) (coupe-silence1 l ln ln d (+ d (abscar (car l)))))
            ((and (numberp (car l)) (> (car l) 0)) (coupe-silence ln (cdr l) (+ d (car l))))
            ((and (numberp (car l)) (< (car l) 0)) (coupe-silence ln (cdr l) (+ d (abs (car l)))))
            (t (print "ERREUR DANS COUPE-SILENCE")))))

(defun coupe-silence1 (l ln lnbis d1 d2)
  ;(print (list 'coupe-silence1 ln l d1 d2))
  (if (null ln) (coupe-silence lnbis (cdr l) d2) 
      (cond ((and (<= d1 (car ln)) (> d2 (car ln))) 
             (cons (car ln) (cherche-mesure0 lnbis (cons (list (- d2 (car ln))) (cdr l)))))
            (t (coupe-silence1 l (cdr ln) lnbis d1 d2)))))

;; -----------------------------------------------
;; on cherche  couper les notes
;; -----------------------------------------------

(defun maxliste (l)
  (let((res 0))
    (mapc #' (lambda (x) (setq res (max x res))) l)
    res))

(defun coupe-note (ln l &optional (d 0))
  ;(print (list 'coupe-note ln l d))
  (if (null l) nil
      (cond ((listp (car l)) (coupe-note ln (cdr l) (+ d (abscar(car l)))))
            ((and (numberp (car l)) (> (car l) 0)) (coupe-note1 l ln ln d (+ d (abs (car l)))))
            ((and (numberp (car l)) (< (car l) 0)) (coupe-note ln (cdr l) (+ d (abs(car l)))))
            (t (print "ERREUR DANS COUPE-NOTE")))))

(defun coupe-note1 (l ln lnbis d1 d2)
  ;(print (list 'coupe-note1 ln l d1 d2))
  (if (null ln) (coupe-note lnbis (cdr l) d2) 
      (cond ((and (<= d1 (car ln)) (> d2 (car ln))) 
             (cons (car ln) (cherche-mesure0 lnbis (cons (- d2 (car ln)) (cdr l)))))
            (t (coupe-note1 l (cdr ln) lnbis d1 d2)))))

;; -----------------------------------------------
;; toutes les tentatives ont choues
;; on admet des mesures > (maxliste '(4 3 2 5 7/2 9/2 5/2 3/2 1 1/2))
;; -----------------------------------------------

(defun grande-mesure (ln n l &optional (d 0))
  ;(print (list 'grande-mesure l d))
  (if (null l) nil
      (let* ((v (abscar (car l)))
             (dd (+ d v)))
        (cond ((null (cdr l)) (list (ceiling dd)))
              (t (if (>= dd n) (cons dd (cherche-mesure0 ln (cdr l)))
                     (grande-mesure ln n (cdr l) dd)))))))


;;==============================================================================
;;
;;  PREPARATION DE LA LISTE A CODER EN ENIGMA
;;
;;==============================================================================

(defun trie1 (l)
  (sort (copy-list l) #'< :key #'car ))

(defun trie2 (l)
  (sort (copy-list l) #'< :key #'cadr ))

(defun trie3 (l)
  (sort (copy-list l) #'< :key #'caddr ))

;; (trie3'((1 2 3) (20 30 40) (1 5 3) (5 6 8) (5 10 8)))

(defun trie12 (l)
  (trie1 (trie2 l)))

;; (trie12'((1 2 3) (20 30 40) (1 5 3) (5 10 8) (5 6 8)))

(defun trie (l) ;(format t "trie l=~S ~% ~%" l )
  (sort l #'(lambda(a b) (if (= (car a) (car b))
                           (< (car(fifth a)) (car(fifth b)))       ; modifi le 28 08 01
                           (< (car a) (car b))))))

(defun trie4 (l)
  (sort (copy-list l) #'< ))

; -----------------------------                   

(defun make-liste (liste aux)
  (if (cdr liste) 
    (if (= (first (first liste)) (first (second liste)))
      (make-liste (cdr liste) (cons (first liste) aux))
      (cons  (reverse (cons (first liste) aux)) (make-liste (cdr liste) nil)))
     (list (reverse (cons (first liste) aux)))))

(defun is-sil? (item) (< (third item) 0))
(defun is-note? (item) (> (third item) 0))

(defun supprime-sil-item (item)
  (if (find-if #'is-note? item)
    (remove-all-sil item)
    (remove-all-but-one-sil item)))

(defun remove-all-sil (item)
  (remove-if #'is-sil? item))

(defun remove-all-but-one-sil (item)
  (remove-if #'is-sil? item :count (1- (count-if #'is-sil? item)) ))
    
(defun applatit (liste)
  (if liste
    (append (car liste) (applatit (cdr liste)))))

;; supprime les silences inutiles dans les accords splits
;; 29 janvier 98

(defun supprime-sil (liste)
  (if liste  (applatit (mapcar #'supprime-sil-item (make-liste liste nil)))))  

;;==============================================================================
;;             BOUCHE-TROU
;; Ajout le 07 01 2001 pour dbuter les *listen* au temps 0 et boucher les ventuels 
;; trous de temps, de manire  satisfaire la recherche de la liste des barres 
;; de mesure: *liste-barres-mesures*
;;==============================================================================

(defun bouche-trou (l) ;(format t "bouche-trou l=~S  ~% ~%" l )  ;; pour tenir compte des accords 18 01 2001
  (if (null (cdr l)) l
      (let* ((a1 (car l))
             (a2 (cadr l))
             (dat1 (car a1))
             (dat2 (car a2))
             (chan (second a1))
             (layer (ninth a1))
             (dur1 (abs(special-evaluate (third a1))))
             (dur2 (abs(special-evaluate (third a2))))
             ;(grace (fifth a1))
             (grace (car (fifth a1)))      ;; 27 08 01 pour introduire le symbole de glisse dans final (plus '(255) ci-dessous au lieu de 255)
             )
        (cond ((< grace 100) (cons a1 (bouche-trou (cdr l))))
              ((= dat1 dat2) (if (= dur1 dur2) (cons a1 (bouche-trou (cdr l)))
                                 (print (list "ERREUR DANS BOUCHE-TROU: NOTES DE MEME DATE MAIS DE DURES DIFFRENTES" a1 a2))))
              ((= dat2 (+ dat1 dur1)) (cons a1 (bouche-trou (cdr l))))
              ((> dat2 (+ dat1 dur1)) (cons a1 (cons (list (+ dat1 dur1) chan (- (+ dat1 dur1) dat2) '(20) '(255) nil nil nil layer nil) (bouche-trou (cdr l)))))
              (t (print (list "ERREUR DANS BOUCHE-TROU: CHEVAUCHEMENT DE NOTES DANS RECODAGE FINAL" a1 a2)))))))

(defun bouche-debut (l) ;(format t "bouche-debut l=~S  ~% ~%" l )
  (if l
    (let* ((a (car l))
           (dat (car a))
           (chan (second a))
           (layer (ninth a)))
      
      (cond ((> dat 0) (cons (list 0 chan (- 0 dat) '(20) '(255) nil nil nil layer nil) l))
            ((= 0 dat) l)
            (t (print (list "erreur dans BOUCHE-DEBUT: date ngative" a)))))))


 


(defun trie-layer (l n)     ;; modif du 11 11 01 pour introduction des layers FINALE
  ;(format t "trie-layer l=~S ~%" l)
  (if l
    (progn
      (let ((a (ninth (car l)))) 
        (if (= a n) (cons (car l) (trie-layer (cdr l) n))
        (trie-layer (cdr l) n)))) nil))




;;(trie-layer '((10 1 2 3 4 5 6 7 1) (8 1 2 3 4 5 6 7 1) (10 1 2 3 4 5 6 7 2)) 1)
;; -->((10 1 2 3 4 5 6 7 1) (8 1 2 3 4 5 6 7 1))

(defun trie-layer-canal (l) ;; modif du 11 11 01 pour introduction des layers FINALE

  (let ((res nil) tmp)
    (when l 
      (dotimes (i 4) ;(format t "trie-layer-canal l=~S tmp=~S res=~S ~% ~%" l tmp res)
        (when (setq tmp (trie-layer l (1+ i)))
           (setq res (cons (bouche-trou (bouche-debut (reunir (trie  tmp)))) res ))
          )))
    res))

;;==============================================================================
;;             PREPA-LISTE
;; Nouveau PREPA-LISTE qui calcule aussi une liste de rythme
;; dont chaque lments (relatif  chaque layer de chaque canal)
;; est de la forme (date dure); avec - pour nolet et () pour silence.
;; exemple (((0 -2) (2 2) (4 (2)) (6 -2) (8 2) (10 (2))) ((0 -2) (2 4) (6 -2) (8 4))) issue de
;; (prorythme 0 0 16 (gnotes) 0 (2/3n (1 1 1) 2 -2) 60 100)
;; (prorythme 9 0 16 (gnotes) 0 (2/3n (1 -1 1) 4) 60 100))
;;==============================================================================


(defun calclrythme (l) ;(format t "calclrythme l=~S ~% ~%" l)
  (if l (append (calccanal (car l)) (calclrythme (cdr l)))))




;(defun calclrythme2 (l)
;  (if l (append (calccanal2 (car l)) (calclrythme2 (cdr l)))))

(defun calccanal (l) ;(format t "calccanal l=~S ~% ~%" l) 
  (if l
    (cons (calccanallayer (enleve-liaison (reunir (car l)))) (calccanal (cdr l)))))

;(defun calccanal2 (l)
;  (if l
;    (cons (calccanallayer2 (enleve-liaison (reunir (car l)))) (calccanal2 (cdr l)))))

;; l est un liste commencant par + (par ex: (+ 1 (+ (* (FDUR 2 3 D-N) 1) (* (FDUR 2 3 D-C) 1/2))))


  (defun enleve-liaison0 (l) ;(format t "enleve-liaison000000 l=~S ~% ~%" l)
         (cond ((numberp l) (list l))
               ((and (listp l) (equal (car l) '+)) (cons (second l) (enleve-liaison0 (third l))))
               ((and (listp l) (equal (car l) '*)) (list l))
               (t (list "erreur de rythme dans ENLEVE-LIAISON" l))))


;; (enleve-liaison0 '(+ 1 (+ (* (FDUR 2 3 D-N) 1) (* (FDUR 2 3 D-C) 1/2))))
;; -->(1 (* (FDUR 2 3 D-N) 1) (* (FDUR 2 3 D-C) 1/2))



(defun cons-sous-liste (dat chan lr reste)
  (if lr
    (cons (append (list dat chan (car lr)) reste) (cons-sous-liste (+ dat (eval (car lr))) chan (cdr lr) reste))))

;; (cons-sous-liste 0 0 '(1 (* (FDUR 2 3 D-N) 1) (* (FDUR 2 3 D-C) 1/2)) '((60) (255) 100 NIL NIL 1))
;; --> ((0 0 1 (60) (255) 100 NIL NIL 1) (1 0 (* (FDUR 2 3 D-N) 1) (60) (255) 100 NIL NIL 1) 
;; (5/3 0 (* (FDUR 2 3 D-C) 1/2) (60) (255) 100 NIL NIL 1))
 



(defun enleve-liaison (ll) ;(format t "enleve-liaison ll=~S ~% ~%" ll);; transforme la liste d'vnement en supprimant les liaisons --> correction 10 septembre 2012
  (if ll
    (let* ((a (car ll))
           (a1 (car a))
           (a2 (second a))
           (a3 (third a)))
          (if (numberp a3) (cons a (enleve-liaison (cdr ll)))
              (if (equal (car a3) '+) 
                (append (cons-sous-liste a1 a2 (enleve-liaison0 a3) (last a 8)) (enleve-liaison (cdr ll))) ;; (last a 7) au lieu de (last a 6) correction du 10 septembre 2012
                (cons a (enleve-liaison (cdr ll))))))))


  
 



(defun calccanallayer (l)
   ;(format t "calccanallayer l=~S ~% ~%" l) 
  (if l
    (let* ((a (car l))
           ;(a1 (first a))
           (a2 (third a))
           (grace (car (fifth a))))
      
       (if (numberp a2)
        (if (> a2 0) (if (> grace 100) (cons a2 (calccanallayer (cdr l))) (calccanallayer (cdr l)))
            (cons (list (abs a2)) (calccanallayer (cdr l))))
        (if (listp a2)
          (if (equal (car a2) '*)
            (let* ((a3 (second a2))
                   (v (* (second a3) (eval (fourth a3))))) 
              (cons (- 0 v) ( calccanallayer (clc  v l 0))))
            (if (equal a2 nil) (calccanallayer (cdr l))
                (print (list "Erreur de rythme dans CALCCANALLAYER " a)))))))))

; --> (2 -4)

; ********************************************** ??????
; introduction de la coupure des n-olets 6 12 01
#|
(defun calccanallayer (l)
  ;(print (list "calccanallayer" l))
  (if l
    (let* ((a (car l))
           ;(a1 (first a))
           (a2 (third a))
           (grace (car (fifth a))))


      (if (numberp a2)
        (if (> a2 0) (if (> grace 100) (cons a2 (calccanallayer (cdr l))) (calccanallayer (cdr l)))
            (cons (list (abs a2)) (calccanallayer (cdr l))))
        (if (listp a2)
          (if (equal (car a2) '*)
            (let* ((a3 (second a2))
                   (v (* (second a3) (eval (fourth a3))))) 
              (cons (/ (- 0 v) 2) (cons (/ (- 0 v) 2) ( calccanallayer (clc  v l 0))))) ;<---- LIGNE DIFFRENTE!
            (if (equal a2 nil) (calccanallayer (cdr l))
                (print (list "Erreur de rythme dans CALCCANALLAYER " a)))))))))

; --> (2 -2 -2)
; **********************************************
|#

;;================================================================================================= 
;;            PREPA-LISTE
;; Construit une super liste  partir des *liste.n* 
;; et calcule la liste des barres de mesure: *liste-barres-mesures* (NOUVEL ALGORYTHME AU 30 11 2001)
;;=================================================================================================


(defun calclrythme2 (l)
  (if l (append (calccanal2 (car l)) (calclrythme2 (cdr l)))))

(defun calccanal2 (l)
  (if l
    (cons (calccanallayer2 (enleve-liaison (reunir (car l)))) (calccanal2 (cdr l)))))

(defun calccanallayer2 (l)
  ;(print (list "calccanallayer2" l))
  (if l
    (let* ((a (car l))
           (a1 (first a))
           (a2 (third a))
           (grace (car (fifth a)))) 
      (if (numberp a2)
        (if (> a2 0) (if (> grace 100) (cons (list a1 a2) (calccanallayer2 (cdr l))) (calccanallayer2 (cdr l)))
            (cons (list a1 (list (abs a2))) (calccanallayer2 (cdr l))))
        (if (listp a2)
          (if (equal (car a2) '*)
            (let* ((a3 (second a2))
                   (v (* (second a3) (eval (fourth a3))))) 
              (cons (list a1 (- 0 v)) ( calccanallayer2 (clc  v l 0))))
            (if (equal a2 nil) (calccanallayer2 (cdr l))
                (print (list "Erreur de rythme dans CALCCANALLAYER2 " a)))))))))

; --> ((0 2) (2 -4))



(defun clc (v l &optional (compt 0)) ;; retourne la liste l prive des premiers lments dont la somme des dures = v
  (if l
      (let* ((a (car l))
             (a2 (third a))
             (a3 (fifth a))
             (grace (car a3))
             (a5 (if (> grace 100) (abs(eval a2)) 0)) ;; pour ne pas prendre en compte les grace notes
             (valolet (+ compt a5)))
        ;(print (list " dans CLC:  valolet v l ="  valolet v l))
        (cond
         ((= valolet v) (cdr l))
         ((< valolet v) (clc v (cdr l) valolet))
         (t (print (list "Erreur de rythme dans CLC: valolet v (car l) =" valolet v (car l))))))))


#|
; ancienne version 2001
(defun prepa-liste ()

  (let ((ltotal (list (trie-layer-canal *liste0*) (trie-layer-canal *liste1*) (trie-layer-canal *liste2*) (trie-layer-canal *liste3*)
                      (trie-layer-canal *liste4*) (trie-layer-canal *liste5*) (trie-layer-canal *liste6*) (trie-layer-canal *liste7*)
                      (trie-layer-canal *liste8*) (trie-layer-canal *liste9*) (trie-layer-canal *liste10*) (trie-layer-canal *liste11*)
                      (trie-layer-canal *liste12*) (trie-layer-canal *liste13*) (trie-layer-canal *liste14*) (trie-layer-canal *liste15*)
                      (trie-layer-canal *liste16*) (trie-layer-canal *liste17*) (trie-layer-canal *liste18*) (trie-layer-canal *liste19*)
                      (trie-layer-canal *liste20*) (trie-layer-canal *liste21*) (trie-layer-canal *liste22*) (trie-layer-canal *liste23*)
                      (trie-layer-canal *liste24*) (trie-layer-canal *liste25*) (trie-layer-canal *liste26*) (trie-layer-canal *liste27*)
                      (trie-layer-canal *liste28*) (trie-layer-canal *liste29*) (trie-layer-canal *liste30*) (trie-layer-canal *liste31*))))
    ;(setq *liste-canal* (calclcanal ltotal))    ;; calcule la liste des canaux utiliss
    (setq *liste-barres-mesures* (cherche-mesure0 '(4 3 2 1) 
                                  ;;  liste modifie le 17 01 2002 '(4 3 2 5 7/2 9/2 5/2 3/2 1 1/2) 
                                           ;; liste modifie le 28 11 01 '(4 3 2 5 7/2 9/2 5/2 3/2 1 1/2) 
                                                  (supermixrythm (calclrythme ltotal)))) ;; calcule les listes de dures par canal et par layer
   ;(print ltotal)
   ltotal))

|#



;; ************************ pour tester si il y a rellement une liste  afficher dans finale (22 09 2012)
(defun liste-vide? (l)
  (if l
      (if (eq (car l) nil)
          (liste-vide? (cdr l))
        nil) t))


(defun prepa-liste () 
  
  (let ((ltotal (list (trie-layer-canal (copy-tree *liste0*)) (trie-layer-canal (copy-tree *liste1*)) (trie-layer-canal (copy-tree *liste2*)) (trie-layer-canal (copy-tree *liste3*))
                      (trie-layer-canal (copy-tree *liste4*)) (trie-layer-canal (copy-tree *liste5*)) (trie-layer-canal (copy-tree *liste6*)) (trie-layer-canal (copy-tree *liste7*))
                      (trie-layer-canal (copy-tree *liste8*)) (trie-layer-canal (copy-tree *liste9*)) (trie-layer-canal (copy-tree *liste10*)) (trie-layer-canal (copy-tree *liste11*))
                      (trie-layer-canal (copy-tree *liste12*)) (trie-layer-canal (copy-tree *liste13*)) (trie-layer-canal (copy-tree *liste14*)) (trie-layer-canal (copy-tree *liste15*))
                      (trie-layer-canal (copy-tree *liste16*)) (trie-layer-canal (copy-tree *liste17*)) (trie-layer-canal (copy-tree *liste18*)) (trie-layer-canal (copy-tree *liste19*))
                      (trie-layer-canal (copy-tree *liste20*)) (trie-layer-canal (copy-tree *liste21*)) (trie-layer-canal (copy-tree *liste22*)) (trie-layer-canal (copy-tree *liste23*))
                      (trie-layer-canal (copy-tree *liste24*)) (trie-layer-canal (copy-tree *liste25*)) (trie-layer-canal (copy-tree *liste26*)) (trie-layer-canal (copy-tree *liste27*))
                      (trie-layer-canal (copy-tree *liste28*)) (trie-layer-canal (copy-tree *liste29*)) (trie-layer-canal (copy-tree *liste30*)) (trie-layer-canal (copy-tree *liste31*))
                      
                      (trie-layer-canal (copy-tree *liste32*)) (trie-layer-canal (copy-tree *liste33*)) (trie-layer-canal (copy-tree *liste34*)) (trie-layer-canal (copy-tree *liste35*))
                      (trie-layer-canal (copy-tree *liste36*)) (trie-layer-canal (copy-tree *liste37*)) (trie-layer-canal (copy-tree *liste38*)) (trie-layer-canal (copy-tree *liste39*))
                      (trie-layer-canal (copy-tree *liste40*)) (trie-layer-canal (copy-tree *liste41*)) (trie-layer-canal (copy-tree *liste42*)) (trie-layer-canal (copy-tree *liste43*))
                      (trie-layer-canal (copy-tree *liste44*)) (trie-layer-canal (copy-tree *liste45*)) (trie-layer-canal (copy-tree *liste46*)) (trie-layer-canal (copy-tree *liste47*)))))

    ;(print (list "prepa-liste ltotal= " ltotal))

    
    (if (not (liste-vide? ltotal))
        (setq *liste-barres-mesures* (cherche-mesure0 '(4 3 2 1 5/2 7/2 9/2 3/2 1/2 9/4 13/4 17/4 5/4 3/4) (supermixrythm (calclrythme ltotal))))  ;; calcule les listes de dures par canal et par layer
      (progn
        (print (list "liste vide dans prepa-liste: ---> abort"))
        (abort)))


    ;;  liste modifie le 17 01 2002 '(4 3 2 5 7/2 9/2 5/2 3/2 1 1/2) 
    ;; liste modifie le 28 11 01 '(4 3 2 5 7/2 9/2 5/2 3/2 1 1/2) 
                                                  
                                                 
    ;; calcule les listes de dures par canal et par layer

    ;(print (list "prepa-liste: ltotal" ltotal))
    ltotal))


;; ------------------------------------------------------------------------------
;;          CODAGE DES RYTHMES

;;           DE (1 + 2/3c (1/2 + 2/3dc (1/2 1/4) + 1/2))
;;           A ((+ 1 (+ (* (FDUR 2 3 C) 1/2) (* (FDUR 2 3 C) (FDUR 2 3 DC) 1/2)))
;;                   (+ (* (FDUR 2 3 C) (FDUR 2 3 DC) 1/4) (* (FDUR 2 3 C) 1/2)))
;; ------------------------------------------------------------------------------

(defun fdur (num denum note)
  (declare (ignore note))
  (/ num denum))

(defun traduit-dur-symb (ds)
  (let ((v (assoc ds '((2/3c (fdur 2 3 d-c))
                       (3/5c (fdur 3 5 d-c))
                       (2/3dc (fdur 2 3 d-dc))
                       (3/5dc (fdur 3 5 d-dc))
                       (2/3tc (fdur 2 3 d-tc))
                       (2/3qc (fdur 2 3 d-qc))
                       (2/3n (fdur 2 3 d-n))
                       (2/3b (fdur 2 3 d-b))
                       (3/5n (fdur 3 5 d-n))
                       (4/5n (fdur 4 5 d-n))
                       (4/6n (fdur 4 6 d-n))
                       (4/5c (fdur 4 5 d-c))
                       (4/5dc (fdur 4 5 d-dc))
                       (4/5tc (fdur 4 5 d-tc))
                       (4/5qc (fdur 4 5 d-qc))
                       (4/6dc (fdur 4 6 d-dc))
                       (4/6c (fdur 4 6 d-c))
                       (4/6tc (fdur 4 6 d-tc))
                       (4/6qc (fdur 4 6 d-qc))
                       (4/7dc (fdur 4 7 d-dc))
                       (4/7n (fdur 4 7 d-n))
                       (4/7c (fdur 4 7 d-c))
                       (4/7tc (fdur 4 7 d-tc))) :test #'equal)))
    (if v
      (second v)
      (error "undefined symbolic duration : ~S" ds))))

(defun code-rythme (l &optional variation) 
       (cond ((numberp l) (if variation `(* ,@variation ,l) l))
             ((consp l) (if (symbolp (first l))
                          (if (eq (first l) '+) 
                            (cons '+ (code-rythme (rest l) variation))
                            (append (code-rythme (second l) (append variation (list (traduit-dur-symb (first l)))))
                                    (code-rythme (cddr l) variation)))
                          (cons (code-rythme (first l) variation)
                                (code-rythme (rest l) variation))))
             ((null l) ())
             (t (error "wrong rythm : ~S" l))))

(defun prefixplus (l) 
  (cond ((null l) l)
        ((null (cdr l)) l)
        ((eq (second l) '+)
         (let ((temp (prefixplus (cddr l))))
           (cons (list '+ (first l) (first temp)) (rest temp))))
        (t
         (cons (first l) (prefixplus (rest l))))))

(defun cr (r)
  (prefixplus (code-rythme r)))

;;(cr '(1 + 2/3c (1/2 + 2/3dc (1/2 1/4) + 1/2) + 10))

; nbre de notes dans un rythme

(defun long-rythme (r)  ; 29 12 2001
  (length (cr r)))

;(long-rythme '(1 2 3))
;(long-rythme '(1 + 2 + 3))
;(long-rythme '(1 + 2/3b (2 -2 2) + 10 20))

;; ----------------------------------------------------------------------
;; Pour prparer les listes servant  dterminer les mesures
;; ----------------------------------------------------------------------

(defun traduit-dur-symb-mesure (ds)        ;;------------- rajout 4 01 2001
  (let ((v (assoc ds '((2/3c 1)
                       (3/5c 3/2)
                       (2/3dc 1/2)
                       (3/5dc 3/4)
                       (2/3tc 1/4)
                       (2/3qc 1/8)
                       (2/3n 2)
                       (2/3b 4)
                       (3/5n 3)
                       (4/5n 4)
                       (4/6n 4)
                       (4/5c 2)
                       (4/5dc 1)
                       (4/5tc 1/2)
                       (4/5qc 1/4)
                       (4/6dc 1)
                       (4/6c 2)
                       (4/6tc 1/2)
                       (4/6qc 1/4)
                       (4/7dc 1)
                       (4/7c 2)
                       (4/7tc 1/2)))))
    (if v
      (second v)
      (error "undefined symbolic duration : ~S" ds))))
    
;; ------------------------------------------------------------------------------

(defun entre (x a b)
  (let ((xx (car x)))                    ; ajout le 27 08 01
(if (and (<= xx b) (>= xx a)) t nil)))

(defun gracenote? (x)
  (entre x 0 99))

(defun next-mesure (version)
  (setq *end-mesure* t)
  (setq *last-ev* *cur-n*)       
  (incf *num-mes-FR*)
  (incf *num-canal-mesure*)
  ;;(format t "next-mesure *copy-liste-bm*=~S ~%" *copy-liste-bm*)
  ;;(format t "next-mesure *num-canal-mesure*=~S ~%" *num-canal-mesure*)
  ;;(format t "dans next-mesure *end-mesure*=~S ~%" *end-mesure*)
  ;;(if item (setq *dur-mesure* (- (first item) (* 4 *num-canal-mesure*))))
  ;;(if item (setq *dur-mesure* (mod (first item) 4)))
  (cond
   ((= version 2001) (code-mesure01))  
   ((= version 2004) (code-mesure04))
   (t (format nil "ERREUR dans decode-file, pas de version ~S" version)))
  (setq *first-ev* (1+ *cur-n*))
  (setq *cur-pos* 0)
  (setq *copy-liste-bm* (cdr *copy-liste-bm*))
  (unless (null *copy-liste-bm*)
    (setq *long-mes* (car *copy-liste-bm*))))

(defun fin-canal ()
  ;(format t "dans fin-canal ******** *cur-pos*=~S ~%" *cur-pos*)
  ;(format t "dans fin-canal ******** *long-mes*=~S ~%" *long-mes*)
  (setq *end-mesure* t)
  (setq *last-ev* *cur-n*)       
  (incf *num-mes-FR*)
  (incf *num-canal-mesure*)
  (setq *first-ev* (1+ *cur-n*))
  (setq *cur-pos* 0))

(defun fin-layer (n version) 
  (if (not *end-mesure*) 
    (progn (setq *mes-non-finie* t)
           (decode-dur nil (- *cur-pos* *long-mes*) nil '(255) 0 nil nil nil nil version nil n)))  

  ;(format t "dans fin-layer ******** *cur-pos*=~S ~%" *cur-pos*)
  ;(format t "dans fin-layer ******** *long-mes*=~S ~%" *long-mes*)
  (setq *end-mesure* t)
  (setq *last-ev* *cur-n*)       
  (incf *num-mes-FR*)
  (incf *num-canal-mesure*)
  (setq *first-ev* (1+ *cur-n*))
  (setq *cur-pos* 0))

;;================================================================================================= 
;;            DECODE-LAYER
;; Pour un canal donn, onstruit les diffrents sous-fichiers ENIGMA
;; 
;;=================================================================================================

(defun decode-layer (liste version &optional (total-pos 0) (num-expres-en-cours 0) (num-pgchg-en-cours 0))      ;; codification du premier item de la liste
  (if liste
    (let* ((item (first liste))
           (special (fifth item))
           (expression (seventh item))
           (expressionlayer (tenth item))
           (articulation (nth 10 item))  ;; ajout de Mai 2012 (11 me champ)
           (pgchange (eighth item))
           (haut (car (fourth item)))
           ;;(dat (first item))
           ;;(chan (second item))
           ) 

      ;(format t "dans decode-layer ******** liste=~S ~%" liste)
      ;(format t "dans decode-layer ******** item=~S expressionlayer=~S ~%" item expressionlayer)
      ;(format t "dans decode-layer ******** articulation=~S ~%" articulation)

      (setq *premier* t)
      (if (cdr liste)
        (setq *dernier* nil)
        (setq *dernier* t))
      (setq *pos-cour* (first item))
      
      
      (if (and expression (> haut 0)) 
        (let ((b (expression-actuelle expression haut)))
          (cond ((= b 0) (setq expression nil) (setq num-expres-en-cours 0)) ;la hauteur ne figure dans aucun interval
                ((= b num-expres-en-cours) (setq expression nil))            ;deux notes successives avec mme expression
                (t (setq num-expres-en-cours b) (setq expression num-expres-en-cours))))) ;changement d'expression--> expression devient num-expres-en-cours

    ; (if expressionlayer    ; envoi de la note bascule layer de l'chantilloneur
     ;    (progn
    ;       (p-abs (* noi dat))
     ;      (p-write-abs(note :pitch (first expressionlayer)  :dur noi :vel 10 :chan (nchan chan) :port (nport chan)))))
          



      (if (and pgchange (> haut 0))
        (if (= pgchange num-pgchg-en-cours) (setq pgchange nil)
            (setq num-pgchg-en-cours pgchange)))
      
      ;(format t "dans decode-layer ******** item=~S ~%" item)
      ;(format t "dans decode-layer ******** *pos-cour*=~S ~%" *pos-cour*)
      ;(format t "dans decode-layer ******** total-pos=~S ~%" total-pos)
      ;(format t "dans decode-layer *** *pos-cour*=~S *cur-pos*=~S dur==~S total-pos=~S first item=~S ~%" *pos-cour* *cur-pos* (third item) total-pos (first item))
      ;(format t "~%" )
      
      (if (> *pos-cour* total-pos)      
        (let ((x (- total-pos *pos-cour*)))
          (setq total-pos (+ total-pos  (abs x))) 
           (decode-dur (second liste) x nil special (sixth item) expression (second expressionlayer) pgchange version)))
      
      (if (not (gracenote? special))   ;; cas gnral 
        (setq total-pos (+ total-pos (special-evaluate (third item)))))
      
      ;(decode-dur (second liste) (third item) (fourth item) special (sixth item) expression (second expressionlayer) pgchange version)  
      (decode-dur (second liste) (third item) (fourth item) special (sixth item) expression expressionlayer pgchange articulation version)
      (setq *premier* nil)
      (if (cdr liste) 
         ; (decode-layer-suite (cdr liste) total-pos num-expres-en-cours (second expressionlayer) num-pgchg-en-cours version )
        (decode-layer-suite (cdr liste) total-pos num-expres-en-cours expressionlayer num-pgchg-en-cours version )
        total-pos))))





(defun decode-layer-suite (liste total-pos num-expres-en-cours num-expreslayer-en-cours num-pgchg-en-cours version)    ;; codification des autres items de la liste
    (if liste
    (let* ((item (first liste))
           (special (fifth item))
           (expression (seventh item))
           (expressionlayer (tenth item))
           (articulation (nth 10 item))  ;; ajout de Mai 2012 (11 me champ)
           (pgchange (eighth item))
           (haut (car (fourth item)))
           ;;(dat (first item))
           ;;(chan (second item))
           )
      ;;(format t "dans decode-layer-suite *** *pos-cour*=~S *cur-pos*=~S dur==~S total-pos=~S first item=~S ~%" *pos-cour* *cur-pos* (third item) total-pos (first item))
      ;;(format t "dans decode-layer-suite *end-mesure*=~S item=~S ~%" *end-mesure* item)
      ;;(format t "dans decode-layer-suite  liste=~S ~%" liste)
      ;;(format t "dans decode-layer-suite  expressionlayer=~S ~%" expressionlayer)
      ;;(format t "dans decode-layer-suite  articulation=~S ~%" articulation)

      (if (not (entre special 2 99))
        (setq *pos-cour* (first item)))

      (if (and expression (> haut 0)) 
        (let ((b (expression-actuelle expression haut)))
          (cond ((= b 0) (setq expression nil) (setq num-expres-en-cours 0)) ;la hauteur ne figure dans aucun interval
                ((= b num-expres-en-cours) (setq expression nil))            ;deux notes successives avec mme expression
                (t (setq num-expres-en-cours b) (setq expression num-expres-en-cours)))))  ;changement d'expression--> expression devient num-expres-en-cours

#|   
        supression: ce calcul est ralis par noteli via la fonctionn f-layersynth (16-04-2012)


      (if (and expressionlayer (> haut 0))
          (if (= (second expressionlayer) num-expreslayer-en-cours)  
              (setq expressionlayer nil)                       ;on reste dans le mme layer de l'chantilonneur
            (progn
              (p-abs (* noi dat))
              (p-write-abs (note :pitch (first expressionlayer)  :dur noi :vel 10 :chan (nchan chan) :port (nport chan)))
              (setq num-expreslayer-en-cours (second expressionlayer)))) ) ; on a chang de layer et envoi de la note bascule layer de l'chantilloneur
|#



      (if (and pgchange (> haut 0))
        (if (= pgchange num-pgchg-en-cours) (setq pgchange nil)
            (setq num-pgchg-en-cours pgchange)))
      
      ;;(format t "dans decode-layer-suite ******** item=~S ~%" item)
      ;;(format t "dans decode-layer-suite ******** *pos-cour*=~S ~%" *pos-cour*)
      ;;(format t "dans decode-layer-suite ******** total-pos=~S ~%" total-pos)
      ;;(format t "~%" )
      
      (if (> *pos-cour* total-pos)     
        (let ((x (- total-pos *pos-cour*))) 
          (setq total-pos (+ total-pos  (abs x))) 
          (decode-dur (second liste) x nil special (sixth item) expression (second expressionlayer) pgchange version))) 
      
      (if (not (gracenote? special)) 
        (setq total-pos (+ total-pos (special-evaluate (third item))))) 
      
      (if (not (cdr liste)) (setq *dernier* t))
      ;(decode-dur (second liste) (third item) (fourth item) special (sixth item) expression (second expressionlayer) pgchange version) 
      (decode-dur (second liste) (third item) (fourth item) special (sixth item) expression expressionlayer pgchange articulation version)
      (decode-layer-suite (cdr liste) total-pos num-expres-en-cours num-expreslayer-en-cours num-pgchg-en-cours version))

    total-pos))


(defun next-note () 
  (setq *cur-n* (1+ *cur-n*)))

(defun next-pos (code special)    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;(format t "~%" )
  (if (not (gracenote? special))
    (setq  *cur-pos* (+ *cur-pos* code))))

(defun end-mesure? ()
 ;(format t "dans end-mesure --- *long-mes*=~S *cur-pos*=~S ~%" *long-mes* *cur-pos* )
  (= *long-mes* *cur-pos*))  ;; rajout le 18 12 2000


(defun sup-mesure? (v-dur)
  (> (+ *cur-pos* v-dur) *long-mes*))  ;; rajout le 18 12 2000


(defun init-decode-chan (version) ;; 12 11 01 (layer)
  (setq *cur-portee* (+ 1 *cur-portee*))
  (setq *2-en-cours* nil)
  (setq *lprov-GF* nil)
  (code-portee)
  (cond
   ((= version 2001) (code-portee-clef01))  
   ((= version 2004) (code-portee-clef04))
   (t (format nil "ERREUR dans init-decode-chan, pas de version ~S" version))))

(defun init-code-layer ()
  (setq *num-canal-mesure* 0)
  (setq *1-en-cours* nil)
  (setq *2-en-cours* nil)
  (setq *copy-liste-bm* *liste-barres-mesures*)
  (setq *long-mes* (car *liste-barres-mesures*)))

;;================================================================================================= 
;;            DECODE
;; Construit canal par canal les diffrents sous-fichiers ENIGMA
;; 
;;=================================================================================================

;; Mofif Mai 2001 (voir intro)

(defun decode (version)
  ;(setq *liste-barres-mesures* (calcul-liste-mesure))  ;;(append (calcul-liste-mesure) (list 4)))
   (init-codage)
  (decode-file (prepa-liste) version))  ;; 112 11 01  prsent prepa-liste calcule *liste-barres-mesures*



(defun decode-file (liste version) 
;(print (list "decode-file --> liste  " liste))
  (if liste
    (let ((chan-liste (car liste))) ;; codification par canal 
      (when chan-liste 
        (init-decode-chan version)  
        
        (cond
         ((= version 2001) (decode-chan01 chan-liste version))  
         ((= version 2004) (decode-chan04 chan-liste version))
         (t (format nil "ERREUR dans decode-file, pas de version ~S" version))))
        
        (fin-canal)
        (decode-file (cdr liste) version))
      (progn
        (code-description-mesure version)
        (code-divers))))

;;================================================================================================= 
;;            DECODE-CHAN
;; Pour un canal donn, construit les diffrents sous-fichiers ENIGMA
;; 
;;=================================================================================================

(defun expression-actuelle (expression haut)
  (if expression
    (calc-num (car expression) (second expression) haut)
      (print "ERREUR DANS nouvel-num-expres-en-cours")))

(defun calc-num (lint lnum haut)
  (if (or (null lint) (null lnum)) 0
      (let* ((a (car lint))
             (a1 (car a))
             (a2 (if (= (length a) 1) a1 (second a)))
             (num (car lnum))
             (m1 (min a1 a2))
             (m2 (max a1 a2)))
        (if (or (> haut m2) (< haut m1))
          (calc-num (cdr lint) (cdr lnum) haut)
          num))))

;------------------------- version 2001 ----------------------------
(defun decode-chan01 (liste version)
  (when liste
    (let* ((l (car liste))
           (lay (ninth (car l))))
      (if l 
        (progn (setq *layer* lay) 
               (init-code-layer) 
               ;(decode-layer (prepare-nolet l *liste-barres-mesures*))
               (decode-layer l version)
               (fin-layer *layer* version) 
               (decode-chan01 (cdr liste) version)))))
        (code-GF01 (trie12 (reverse *lprov-GF*))))

;--------------------------------------------------------------------
(defun ote (a l &optional (res nil)) ; ote tous les lments a d'une liste l
  (if l
    (if (equal a (car l)) (ote a (cdr l) res) (ote a (cdr l) (cons (car l) res)))
    (reverse res)))

; (ote 5 '(2 3 5 7 5)) --> (2 3 7)

(defun otelist (l1 l2) ; ote les lments de la liste l1 de la liste l2
  (if l1
    (otelist (cdr l1) (ote (car l1) l2)) l2))

; (otelist '(5 6) '(1 2 5 4 5 6 8)) --> (1 2 4 8)
; (otelist '((1 1 (1 7)) (1 1 (4 1))) '((1 1 (4 1)) (1 1 (1 7)) (1 2 (4 3)) (1 2 (1 9)) (1 3 (4 5)) (1 3 (1 11))))
; --> ((1 2 (4 3)) (1 2 (1 9)) (1 3 (4 5)) (1 3 (1 11)))

;; Pour 1 staff et 1 mesure donne, regroupe les n de layer
;; dans une mme liste de type (nStaff nMesure (nLayer refFR) (nLayer refFR) (nLayer refFR) ....)
                                                  
(defun regroupe (l &optional (res (reverse (2pr (car l)))))
  (if l (regroupe (cdr l) (cons (third (car l)) res)) (reverse res)))

; (regroupe '((1 1 (1 7)) (1 1 (4 1)))) --> (1 1 (1 7) (4 1)) ;Ex pour le staf 1, mesure 1 qui possde les layer 1 et 4


;; Pour un canal donn:

(defun transf-lprov-GF (l &optional (res nil)) ;Pour un canal donn, regroupe les infos layer d'une mesure dans 1 mme liste
  (if l
    (let* ((a (2pr (car l)))
           (l1 (extraitl04 a l))
           (l2 (otelist l1 l)))
      (transf-lprov-GF l2 (cons (regroupe l1) res)))
    (reverse res)))

; (transf-lprov-GF '((1 1 (4 1)) (1 1 (1 7)) (1 2 (4 3)) (1 2 (1 9)) (1 3 (4 5)) (1 3 (1 11))))
; --> ((1 1 (1 7) (4 1)) (1 2 (1 9) (4 3)) (1 3 (1 11) (4 5)))

;------------------------- version 2004 ----------------------------
(defun decode-chan04 (liste version) 
  (when liste
    (let* ((l (car liste))
           (lay (ninth (car l)))) 
      (if l 
        (progn (setq *layer* lay) 
               (init-code-layer) 
               (decode-layer l version)
               (fin-layer *layer* version) 
               (decode-chan04 (cdr liste) version)))))
  (code-GF04 (transf-lprov-GF (trie12 (reverse *lprov-GF*)))))
  

;;================================================================================================= 
;;            DECODE-DUR
;; 
;; 
;;=================================================================================================
#|
|#

(defun decode-dur (item2 dur haut special vel expression expressionlayer pgchange articulation &optional version (liaisag nil) (liaisad nil))
  ;(format t "dans decode-dur ******** dur=~S item2=~S ~%" dur item2)
  ;(format t "dans decode-dur ******** item2=~S ~%" item2)
  ;(format t "dans decode-dur ******** special=~S ~%" special)
  ;(format t "dans decode-dur ******** haut=~S ~%" haut)
  ;(format t "dans decode-dur ******** pgchange=~S ~%" pgchange)
  ;(format t "dans decode-dur ******** expressionlayer=~S haut=~S ~%" expressionlayer haut)
  ;(format t "dans decode-dur ******** articulation=~S  ~%" articulation)

  (if (nuplet1? dur special) (traite-n-uplet1 dur special))
  (setq *vrai-dernier* nil)
  
  (let ((ld (liaison? dur)))
    (cond
     
     ((and ld (eq 0 (first-liaison dur)))
      (decode-dur item2 (second-liaison dur) haut special vel expression expressionlayer pgchange articulation version liaisag liaisad))
     
     (ld 
      (setq *liaison* t)
      (decode-dur item2 (first-liaison dur) haut special vel expression expressionlayer pgchange articulation version liaisag t) 
      (setq *liaison* nil)
      (setq *premier* nil)
      (decode-dur item2 (second-liaison dur) haut '(255) vel nil nil nil nil version t liaisad))         ; le 29 08 01
     
     (t
      ;(format t "avant ddnl *cur-pos* = ~S, *long-mes* = ~S,  rythme = ~S ~%" *cur-pos* *long-mes* dur)
      ;(if (> (+ *cur-pos* (special-evaluate dur)) *long-mes*)
       ; (format t ">>>>split de nolet: duree premiere partie = ~S, duree-totale = ~S,  ~%" (- *long-mes* *cur-pos*) (special-evaluate dur)))
      (decode-dur-nonliaison dur haut special vel liaisag liaisad expression expressionlayer pgchange articulation version)))))




(defun init-decode-dur-nonliaison (dur)
  (setq *vrai-dur* (special-evaluate dur))
  ;(format t "dans init-decode-dur-nonliaison ----- *vrai-dur*=~S ~%" *vrai-dur*)
  (setq *dur-symb* (abs (get-dur dur))) 
  (setq *silence* (sign (get-dur dur))))




(defun decode-dur-nonliaison ( dur  haut special vel liaisag liaisad expression expressionlayer pgchange articulation version) 
  
  ;(format t "dans decode-dur-nonliaison ******** articulation =~S ~%" articulation)
  ;(format t "dans decode-dur-nonliaison ******** liaisag =~S ~%" liaisag)
  ;(format t "dans decode-dur-nonliaison ******** liaisad =~S ~%" liaisad)
  ;(format t "dans decode-dur-nonliaison ******** pgchange =~S ~%" pgchange)

  ;;(format t "dans decode-dur-nonliaison ******** special=~S ~%" special)
  ;;(format t "dans decode-dur-nonliaison ******** dur=~S ~%" dur)
  ;;(format t "dans decode-dur-nonliaison ******** *cur-pos*=~S ~%" *cur-pos*)
  ;;(format t "dans decode-dur-nonliaison ******** *dur-mesure*=~S ~%" *dur-mesure*)
  ;;(format t "dans decode-dur-nonliaison ******** expressionlayer=~S ~%" expressionlayer)

  (init-decode-dur-nonliaison dur)                     ; calcul de *vrai-dur* *dur-symb* *silence*
  (cond
   ((and (not (gracenote? special)) (sup-mesure? *vrai-dur*))
    (let ((val-dur (- *long-mes* *cur-pos*)))                    ;; rajout le 18 12 2000
      (decode-split-of-dur1  val-dur haut special vel liaisag nil expression expressionlayer pgchange articulation version)
      ;(format t "dans decode-dur-nonliaison avant split2 *vrai-dur*=~S val-dur=~S ~%" *vrai-dur* val-dur)
      (decode-split-of-dur2 (- *vrai-dur*  (abs val-dur)) haut special vel nil liaisad articulation version))) 
   (t (setq *suivant* nil)
      (setq *precedent* nil)
      (if (and *dernier* (not liaisad)) (setq *vrai-dernier* t))
      (code-note *vrai-dur* *dur-symb* haut special vel liaisag liaisad expression expressionlayer pgchange articulation version))))

;; ------------------------------------------------------------------------------
;;             Passage de la barre de mesure
;; ------------------------------------------------------------------------------

(defun decode-split-of-dur1 ( dur haut special vel liaisag liaisad expression expressionlayer pgchange articulation version) 
  ;(format t "dans decode-split-of-dur1 ******** special=~S ~%" special)
  ;(format t "dans decode-split-of-dur1 ******** dur=~S ~%" dur)
  (setq *suivant* t)
  (setq *precedent* nil)
  ;(format t "dans decode-split-of-dur1 ******** *suivant*=~S ~%" *suivant*)
  (code-note dur dur haut special vel liaisag liaisad expression expressionlayer pgchange articulation version)   ;; on transmet "expression" et pgchange pour la 1re partie de la note 
  (setq *premier* nil))

(defun decode-split-of-dur2  ( dur haut special vel liaisag liaisad articulation version)
  ;(format t "dans decode-split-of-dur2 ******** special=~S ~%" special)
  (if (or (= (car special) 253) (= (car special) 254)) (setq special '(255)))  ;1 09 01 pas d'articulation (stac ou gliss) sur les notes lies
  ;(format t "dans decode-split-of-dur2 ******** dur=~S ~%" dur)
  ;(format t "dans decode-split-of-dur2 ******** *long-mes*=~S ~%" *long-mes*)
  (setq *precedent* t)
  (if (> dur *long-mes*)    ;; rajout le 18 12 2000
    (progn
      (let ((mem-long-mes *long-mes*)) ;; RAJOUT LE 6 01 2001 
        (setq *suivant* t)
      (setq *vrai-dernier* nil)
      (code-note *long-mes* *long-mes* haut special vel liaisag liaisad nil nil nil articulation version)   ;; rajout le 18 12 2000  ;; on transmet "nil" pour l'expression et pgchange de la sde partie de la note 
      ;(format t "dans decode-split-of-dur2 >>>>>>>>>>>>>>>>>>>>>>>>>>dur=~S *long-mes*=~S ~%" dur *long-mes*)
      (decode-split-of-dur2 (- dur mem-long-mes) haut special vel liaisag liaisad articulation version)))   ;; rajout le 18 12 2000
    (progn                    ;; on passe ici
      (setq *suivant* nil)
      (if (not liaisad) (setq *vrai-dernier* t))
      (code-note dur dur haut special vel liaisag liaisad nil nil nil nil version) ;; on transmet "nil" pour l'expression et pgchange et articulation de la sde partie de la note
      (setq *silence* nil) 
      )))

;; ------------------------------------------------------------------------------
;;                  n-uplets
;; ------------------------------------------------------------------------------

(defun fin-n-uplet1 (total limite)  ; 01 09 01 pour les fonctions fin-n-uplet1 et code-debut-note
  (not (< total limite)))

(defun traite-n-uplet1 (dur special)
  ;;(format t "~%")
  ;;(format t "dans traite-n-uplet1 dur=~S ~%" dur)
  ;;(format t "dans traite-n-uplet1 *total1*=~S ~%" *total1*)
  ;;(format t "dans traite-n-uplet1 *limite1*=~S ~%" *limite1*)
  ;;(format t "dans traite-n-uplet1 *limite2*=~S ~%" *limite2*)
  (let ((dur-symb-posit1 (if (gracenote? special) 
                           dur (abs (car (last dur))))))
    (if (not *1-en-cours*)
      (progn 
        (setq *1-en-cours* t)
        (setq *num-uplet1* (1+ *cur-n*)) 
        
        ;;(format t "*num-uplet1*=~S ~%" *num-uplet1*)
        (setq *dur-uplet1* dur)
        (if (not (gracenote? special))
          (setq *limite1* (* (third (second dur)) (eval(car(last (second dur)))))))
        ;;(format t "*limite1*=~S ~%" *limite1*)
        (setq *n-uplet-1* t)
        (setq *total1* 0)
        (if (nuplet2? dur special)
          (traite-n-uplet2 dur special)
          (setq *total1* dur-symb-posit1)))
      (progn 
        (setq *n-uplet-1* nil)
        (if (nuplet2? dur special)
          (traite-n-uplet2 dur special)
          (if (not (gracenote? special))
            (setq *total1* (+ *total1* dur-symb-posit1)))) 
        (if (fin-n-uplet1 *total1* *limite1*)       ; modit 01 09 01
          (progn (setq *1-en-cours* nil)
                 (code-nuplet *dur-uplet1* *num-uplet1* *cur-n* 1)))))))

(defun traite-n-uplet2 (dur special) 
  ;;(format t "~%")
  ;;(format t "dans n-uplet2  ********** *2-en-cours*=~S ~%" *2-en-cours*)
  ;;(format t "dur=~S ~%" dur)
  ;:(format t "*limite2*=~S ~%" *limite2*)
  ;;(format t "*total2*~S ~%" *total2*)
  
  (let ((dur-symb-posit2 (if (gracenote? special) 
                           dur (abs (car (last dur))))))
    (if (not (gracenote? special))
      (setq *total1* (+ *total1* (* (eval (third dur)) dur-symb-posit2))))
    (if (not *2-en-cours*)
      (progn
        ;;(format t "dans  n-uplet2 je passe dans progn1 ===  ~%" )
        (setq *2-en-cours* t)
        (setq *num-uplet2* (1+ *cur-n*))
        ;;(format t "dans n-uplet2   *num-uplet2*=~S ~%" *num-uplet2*)
        (setq *dur-uplet2* dur)
        (if (not (gracenote? special))
          (setq *limite2* (* (third (third dur)) (eval(car(last (third dur)))))))
        (setq *total2* dur-symb-posit2)
        (setq *n-uplet-2* t))
      
      (progn 
        (setq *n-uplet-2* nil)
        (if (not (gracenote? special)) 
          (setq *total2* (+ *total2* dur-symb-posit2)))
        (if (not (< *total2* *limite2*))
          (progn (setq *2-en-cours* nil)
                 (code-nuplet *dur-uplet2* *num-uplet2* *cur-n* 2)))))))

;; ATTENTION PAS DE NOTE LIE A UN SILENCE DU TYPE:
;; (+ (* (FDUR 2 3 C) 1/2) (* (FDUR 2 3 C) (FDUR 2 3 DC) -1/2))
;; PLACER UN MESSAGE D'ERREUR

;; (special-evaluate '(+ (* (FDUR 2 3 D-C) 1/2) (* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) -1/2)))
;; 5/9
 
(defun special-evaluate (sdur) ;(print (list "special-evaluate" sdur))
  (cond ((numberp sdur) (abs sdur))
        ((eq '+ (car sdur)) (+ (special-evaluate (second sdur)) (special-evaluate (third sdur))))
        ((and (eq '* (car sdur)) (= 3 (list-length sdur))) 
         (if (numberp (car (last sdur)))
           (* (special-evaluate (eval(second sdur))) (abs (car (last sdur))))
           (* (special-evaluate (eval(second sdur))) (special-evaluate (last sdur)))))
        ((and (eq '* (car sdur)) (= 4 (list-length sdur)))
         (if (numberp (car (last sdur)))
           (* (special-evaluate (eval(second sdur))) (special-evaluate (eval(third sdur))) (abs (car (last sdur))))
           (* (special-evaluate (eval(second sdur))) (special-evaluate (eval(third sdur))) (special-evaluate (last sdur)))))
        (t (eval (first sdur)))))

    
(defun  first-liaison (dur)
  (second dur))

(defun  second-liaison (dur)
  (third dur))

(defun liaison? (dur)
(and (listp dur) (eq (first dur) '+)))  
 
(defun nuplet1? (dur special)
 (or (and *n-uplet-1* (gracenote? special))
     (and (listp dur)  (eq (first dur) '*) (listp (second dur)))))

(defun nuplet2? (dur special)
  (or (and *n-uplet-2* (gracenote? special))
  (and (listp dur)  (eq (first dur) '*) (listp (third dur)))))

;; ------------------------------------------------------------------------------
;;   Traitement des *listen*: 
;; (0 2 1/2 (62 69) (1)) (0 2 1/2 (63 70) (2)) (0 2 1/2 (60 67) (99)) (0 2 1 (61 68) (255))     ; 27 08 01
;; ------------------------------------------------------------------------------



(defun reunir (l) ;(format t "reunir l=~S ~% ~%" l) ;; transforme l'ensemble des notes de mme date et mme dure (le reste tant identique) en accord
  (nreverse (reunir1 (cdr l) nil (car l))))

;;(reunir '((0 0 1 (60) (255) 100 NIL NIL 1) (1 0 1 (62) (255) 100 NIL NIL 1) (1 0 1 (60) (255) 100 NIL NIL 1)))
;; -->((0 0 1 (60) (255) 100 NIL NIL 1) (1 0 1 (62 60) (255) 100 NIL NIL))




(defun reunir1 (l r x) 
  ;(format t "reunir1 l=~S  ~%" l ) 
  ;(format t "reunir1  r=~S   ~%" r)
  ;(format t "reunir1  x=~S  ~%" x)
  ;(format t "reunir1  (cons x r)=~S ~% ~%" (cons x r))
  (if (null l) 
    (cons x r) 
    (if (compatible x (car l))
        (reunir1 (cdr l) r (reunir-ev x (car l)))
        (reunir1 (cdr l) (cons x r) (car l)))))

(defun compatible (e1 e2)
  (and (= (first e1) (first e2)) (equal (third e1) (third e2)) 
       (equal (second e1) (second e2)) (equal (fifth e1) (fifth e2)) 
       (equal (seventh e1) (seventh e2)) (equal (eighth e1) (eighth e2)) (equal (ninth e1) (ninth e2))))          ;le 29 08 01  le 19 10 01 le 25 10 01 le 12 11 01

(defun reunir-ev (e1 e2)
  (list (first e1) (second e1) (third e1) (sort (reunir-pitch (fourth e1) (fourth e2)) #'<) (fifth e1) 
        (sixth e1) (seventh e1) (eighth e1) (ninth e2) (tenth e2) (nth 10 e2)))                          
;;le 19 10 01 le 25 10 01 le 12 11 01  :: tentn (pour expressionlayer le 11-04-2012
;; (nth 10 e2) le 25 septembre 2012 pour :llayersynthe

(defun reunir-pitch (p1 p2)
  (if (numberp p1)
    (if (numberp p2)
      (list p1 p2)
      (cons p1 p2))
    (if (numberp p2)
      (cons p2 p1)
      (append p1 p2))))

;; a revoir dans le cas de rythme ternaire !!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; cette fonction ne sert plus!

(defun complete (l)  ;; complete les listes avec des silences
  (if (cdr l)
    (let ((item (car l)))
      (if (<  (+ (first item) (abs(eval (third item))))  (first (cadr l)))
        (progn ;(print  "complete")
        (cons item 
              (cons (list (+ (first item) (abs(eval (third item)))) 
                          (second item) 
                          (- (+ (first item) (abs(eval (third item)))) (first (cadr l)))
                          '(20)
                          '(255) 0) (complete (cdr l)))))          ; '(255) le 27 08 01
        (cons item (complete (cdr l)))))
      l))

;; ------------------------------------------------------------------------------
;; code-articulation ---> il faudra remplacer les 2 code ci-dessous par celui-ci. Attention l-artic est une liste
;; ------------------------------------------------------------------------------

(defun code-articulation (n-ev l-artic) ;(print (list "code-articulation n-ev l-artic = " n-ev l-artic))
  (if l-artic
      (progn
        (code-articulation2 n-ev (car l-artic))
        (code-articulation n-ev (cdr l-artic)))))

(defun code-articulation2 (n-ev n-artic)
  (setq *liste-des-IM* (cons (format nil "^IM(0,~S) ~S 8 0 16 0"
                                     n-ev n-artic)
                             *liste-des-IM*)))

;; ------------------------------------------------------------------------------
;; code-staccato
;; ------------------------------------------------------------------------------

; ne sert plus
(defun code-staccato (n-ev)
  (setq *liste-des-IM* (cons (format nil "^IM(0,~S) 1 8 0 16 0"
                                     n-ev)
                             *liste-des-IM*)))

;; ------------------------------------------------------------------------------             28 08 01
;; code-glissando      ; 51= numro de code d'un signe de glissando dans bibal articulations de FINALE
;; ------------------------------------------------------------------------------

;; ne sert plus (décembre 2012)

(defun code-glissando (n-ev)
  (setq *liste-des-IM* (cons (format nil "^IM(0,~S) 51 16 0 172 0"
                                     n-ev)
                             *liste-des-IM*)))

;; ------------------------------------------------------------------------------             31 08 01
;; code-micro-interval   ; code les "articulations finales" + ou - suivant que la hauteur 
                         ; est entre ton et (ton + 1/2 ton) ou (ton - 1/2 ton) et ton
                         ; et code la forme des ttes de note dans *liste-des-IM*     
;; ------------------------------------------------------------------------------

(defun code-micro-interval (haut n dur-symb)
       (let ((h (car haut)))
         (multiple-value-bind (a b) (round h)
           (declare (ignore a))
             (if (sign b)
               (setq *liste-des-IM* (cons (format nil "^IM(0,~S) 6 0 0 48 0" n) *liste-des-IM*))
               (setq *liste-des-IM* (cons (format nil "^IM(0,~S) 24 -2 0 49 0" n) *liste-des-IM*)))))
       (code-tete n dur-symb (length haut)))


(defun code-tete (n dur k &optional (i 0))
       (when (< i k)
         (if (< dur 2)
           (setq *liste-des-tete* (cons (format nil "^cn(0,~S) 0 0 0 0 0 " n) (cons (format nil "^cn(0,~S) ~S 100 0 -48 0 " n (1+ i)) *liste-des-tete*)))
           (setq *liste-des-tete* (cons (format nil "^cn(0,~S) 0 0 0 0 0 " n) (cons (format nil "^cn(0,~S) ~S 100 0 -62 0 " n (1+ i)) *liste-des-tete*))))
         (code-tete n dur k (1+ i))))

;; ------------------------------------------------------------------------------             19 10 01
;; code-expression       ; code les "expressions finales". Utile pour noter les
                         ; modes de jeu ou chgt d'instrument lorsque ceux-ci
                         ; occupent un mme canal MIDI  
;; ------------------------------------------------------------------------------
    
(defun code-expression (numnote numexpres) 
  (setq *liste-des-expressions* (cons (format nil "^ED(0,~S) ~S 19 160 0 -32768 " numnote numexpres) *liste-des-expressions*)))

;; ---------------- nouveaut FINALE2004 --------------------------------

;; ------------------------------------------------------------------------------
;; code-pages            ; code chaque page de la partition
;; ------------------------------------------------------------------------------

(defun code-pages (n)
  (code-pa (+ 5 (floor (/ n 150)))))

;(code-pages 400)

(defun code-pa (n &optional (i 1) (j 1) (l nil))
  (if (<= i n)
    (progn
      (setq l
            (cons (format nil "^PS(~S) -180 144 180 -144 100 0 " i)  
                  (cons (format nil "^PS(~S) 3168 2448 ~S 2 " i (if (= i 1) 1 (mod j (1- (* 2 n))))) l)))
      (code-pa n (1+ i) (+ 2 j) l))
    (reverse l)))

;; ---------------- nouveaut FINALE2004 --------------------------------
;; ------------------------------------------------------------------------------
;; code-BC            ; code chaque page de la partition
;; ------------------------------------------------------------------------------

(defun code-BC (n &optional (i 1) (l nil))
  (if (<= i n)
    (progn
      (setq l (cons (format nil "^BC(~S) 3072 252 336 129 0 " i)
                    (cons (format nil "^BC(~S) 2048 168 252 86 0 " i)
                          (cons (format nil "^BC(~S) 1024 84 168 43 0 " i)
                                (cons (format nil "^BC(~S) 0 0 84 0 0 " i)
                                      (cons (format nil "^BC(~S) 4096 336 172 172 1 " i)
                                            l))))))
      (code-BC n (1+ i) l))
    (reverse l)))

;(code-BC 3)

;; ------------------------------------------------------------------------------
;; code une note au format FINALE
;; ------------------------------------------------------------------------------

(defun code-note ( vrai-dur dur-symb haut special vel liaisag liaisad expression expressionlayer pgchange articulation version)
  
  ; spcial est le 5eme champ des listes qui code: grace note, staccato, glissando
  
  ;;(format t " ~%" )
  ;;(format t "dans code-note  ************************************ ~%" )
  ;;(format t "dbut code note articulation=~S ~%" articulation)
  ;;(format t "dbut code note *liste-note*=~S ~%" *liste-note*)
  ;;(format t "dbut code note *cur-pos*=~S ~%" *cur-pos*)
  ;;(format t "~%dbut code note *total-des-pos*=~S ~%" *total-des-pos*)
  ;;(format t "dbut code note *num-canal-mesure*=~S ~%" *num-canal-mesure*)
  ;;(format t "dbut code note vrai-dur=~S ~%" vrai-dur)
  ;;(format t "dbut code note dur-symb=~S ~%" dur-symb)
  ;;(format t "dbut code note *long-mes*=~S ~%" *long-mes*)
  ;;(format t "dbut code note *cur-n*=~S ~%" *cur-n*)
  ;;(format t "dbut code note *premier*=~S ~%" *premier*)
  ;;(format t "dbut code note *dernier*=~S ~%" *dernier*)
  ;;(format t "dbut code note *vrai-dernier*=~S ~%" *vrai-dernier*)
  ;;(format t "dbut code note *end-mesure*=~S ~%" *end-mesure*)
  ;;(format t "dbut code note *pos-cour*=~S ~%" *pos-cour*)
  ;;(format t "dans code note *silence*=~S ~%" *silence*)
  ;;(format t "dans code note special=~S ~%" special)
  ;;(format t "dans code note *1-en-cours*=~S ~%" *1-en-cours*)
  ;;(format t "dans code note *n-uplet-1*=~S ~%" *n-uplet-1*)
  ;;(format t "dans code note *n-uplet-2*=~S ~%" *n-uplet-2*) 
  ;;(format t "dans code note haut=~S ~%" haut)
  ;;(format t "dans code note expression=~S ~%" expression)
  ;;(format t "dans code note pgchange=~S ~%" pgchange)
  ;;(format t "dans code note expressionlayer=~S ~%" expressionlayer)
  
  (if (not (= 0 vrai-dur))
    (progn
      (setq *cur-pos* (mod (+ *cur-pos* 0) *long-mes*))  ;; rajout le 18 12 2000
      (setq *end-mesure* nil)
      (next-note)
      (code-debut-note dur-symb haut special expression expressionlayer articulation pgchange)
      (unless *silence*  
        (code-suite-note liaisag liaisad haut special vel 1)) 
      (next-pos vrai-dur special)
      (when (end-mesure?) (next-mesure version)))
    (format t "dans code-note note vnement de dure nulle ~%" )))
  
(defun staccato (x)
  (if (= (car x) 254) t nil))    ; (car x) au lieu de x le 27 08 01: t si la note est staccato, nil sinon

(defun glissando? (x)
  (if (= (car x) 253) t nil))   ; le 28 08 01: t si la note est gliss, nil sinon

(defun micro? (x)
       (if x
  (let ((h (car x)))
      (not (= (round h) h))) nil))   ; le 31 08 01: t si la hauteur n'est pas entire (micro-interval),  nil sinon


(defun xor (a b)
  (or (and a (not b)) (and (not a) b)))


(defun articulation? (x)     ; retourne t si la note est dot d'au moins une articulation  01 09 01
  (entre x 100 254))


;; il faut dcomposer les notes d'une dure sup  la palette de note de finale


(defun code-debut-note (dur-symb haut special expression expressionlayer articulation pgchange)
  ;;(format t "dbut code-dbut-note -------------------------------special=~S ~%" special)
  ;(format t "dbut code-dbut-note -------------------------------articulation=~S *cur-n*=~S ~%" articulation *cur-n*)
  ;;(format t "dbut code-dbut-note -------------------------------dur-symb=~S haut=~S ~%" dur-symb haut)
  ;;(format t "dbut code-dbut-note -------------------------------*n-uplet-1*=~S *n-uplet-2*=~S ~%" *n-uplet-1* *n-uplet-2*)
  ;;(format t "dbut code-dbut-note -------------------------------*precedent*=~S *suivant*=~S ~%" *precedent* *suivant*)
  ;;(format t "dbut code-dbut-note -------------------------------*total1*=~S ~%" *total1*)
  ;;(format t "dbut code-dbut-note -------------------------------*limite1*=~S ~%" *limite1*)
  ;; (format t "dbut code-dbut-note -------------------------------expressionlayer=~S haut=~S ~%" expressionlayer haut)
  ;;(format t "dbut code-dbut-note -------------------------------expression=~S ~%" expression)
  ;;(format t "dbut code-dbut-note -------------------------------pgchange=~S ~%" pgchange)


  (let ((cinquieme 0))
    (setq cinquieme (format nil "$C0~S~S0~S0~S"
                            (cond
                             ;((gracenote? special) 8)
                             ((and (gracenote? special) articulation) 'A)
                             ((and (gracenote? special) (not articulation)) 8)
                             ((micro? haut) 6)                           ; tte carre avec forcement l'articulation + ou -
                              ((or (articulation? special) articulation) 2)                  ;prsence d'une articulation de special (staccato) ou d'une articulation du 11 me champ (accent ...)
                             (t 0))
                            (if (or *n-uplet-1* *n-uplet-2*) 8 0)             ;dbut d'un n-uplet1
                            (if (or (entre special 2 99) (not (fin-n-uplet1 *total1* *limite1*))) 0 8)  ;note sous n-uplet1 ou 2eme grace note et suivante (liaison)
                            (if (gracenote? special) 1 0) 
                            ))

    (setq *liste-note*  (cons  (format nil "^eE(~S) ~S ~S ~S 0 ~A ~S ~S "   ;mai 2001
                                     *cur-n* 
                                     (if *premier* 0 (1-  *cur-n* ))
                                     (cond ((and *vrai-dernier* *dernier* *end-mesure*) 0)
                                           (t (1+  *cur-n* )))
                                     (* *noire* (abs dur-symb))
                                     (if *silence*
                                       (cond
                                        (*n-uplet-1* '$81080800)
                                        (*n-uplet-2* '$81080000)
                                        (t '$81000800))
                                       cinquieme)
                                     (if (or expression pgchange expressionlayer) 129 128)          ;; 19 10 01 pour coder les "text expression" ****** rajout "expressionlayer"  11-04-2012
                                     (if *silence* 0 (length haut)))
                             *liste-note*))
  ;(if (staccato special) (code-staccato *cur-n*)) -------> le codage est à présent réalisé en amont par autre-articulion dans noteliste (et par code-articulation)
 ; (if (glissando? special) (code-glissando *cur-n*)) -------> le codage est à présent réalisé en amont par autre-articulion dans noteliste (et par code-articulation)
  (if (micro? haut) (code-micro-interval haut *cur-n* dur-symb))
  (if expression (code-expression *cur-n* expression))
  (if expressionlayer (code-expression *cur-n* expressionlayer))
  (if pgchange (code-expression *cur-n* pgchange))
  (if articulation (code-articulation *cur-n* articulation))))  ;; rajout de Mai 2010 pour coder le 11 me champ (marques d'articulation


;; *n-uplet-1* si la note est le dbut d'un premier n-uplet
;; *n-uplet-2* si la note est le dbut d'un second n-uplet interne au premier


(defun code-suite-note (liaisag liaisad haut special vel n)
  ;(format t "dbut code-suite-note ---liaisag=~S liaisad=~S special=~S haut=~S *silence*=~S ~%" liaisag liaisad special haut *silence*)
  (when (not(null haut))
    (let ((code-hauteur (unless *silence* (calc-hauteur (round (car haut))))))   ; 31 08 01
      (setq *liste-note*  (cons  (format nil "    ~S $~S~S0~S0000 " 
                                         (first code-hauteur)
                                         (cond ((or (= (car special) 99) (= (car special) 0)) 'C)      ; (car special) le 27 04 0
                                               ((and(not liaisag)(not liaisad)(not *suivant*) (not *precedent*)) 8)
                                               ((and (or *suivant* liaisad) (not liaisag) (not *precedent*)) 'C)
                                               ((and (or *precedent* liaisag) (not liaisad) (not *suivant*)) 'A)
                                               (t 'E))
                                         (second code-hauteur)
                                         (cond ((gracenote? special) 1) ;; grace note
                                               (t n)))
                                 *liste-note*))

;; remarque du 04 09 01
;; *silence* est toujours NIL dans cette fonction, car on y passe que pour les vraies notes
;; aussi le (not *silence*) qui suit ne sert  rien
;; toutefois il se trouve qu'un bug? fait passer par cette fonction des silences:  corriger !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      ;(if (and (not (gracenote? special)) (not *silence*) (not (equal haut '(20))))  ce n'est pas une solution!!
        (if (and (not (gracenote? special)) (not *silence*))
        (code-velocite *cur-n* n vel))
      (code-suite-note liaisag liaisad (cdr haut) special vel (1+ n)))))

#|

|#

;; ------------------------------------------------------------------------------
;; code-velocite
;; ------------------------------------------------------------------------------



(defun code-velocite(num naccord vel)
  ;(format t "dans code-velocite --- *pos-cour*=~S ~%" *pos-cour*)
  ;(format t "dans code-velocite --- vel=~S ~%" vel)
   (setq *liste-des-veloc* (cons (format nil "^ac(0,~S) ~S 0 0 0 ~S "
                                      num
                                      naccord
                                      (- vel 64))
                              *liste-des-veloc*)))


;; ------------------------------------------------------------------------------
;; retouner un couple hauteur format FINALE , alteration (0 ou 1)
;; ------------------------------------------------------------------------------

(defun calc-hauteur (haut)
  (multiple-value-bind (q r) (floor (- haut 60) 12)
    (let (( res (alte r))) 
     (list  (+  (if (second res)  1 0) (* 16 (+ (first res) (* 7 q )))) (if (second res)  1 0)))))

(defun alte (r)
  (let ((liste '(( 0 nil)
                 ( 0 t)
                 ( 1 nil)
                 ( 1 t)
                 ( 2 nil)
                 ( 3 nil)
                 ( 3 t)
                 ( 4 nil)
                 ( 4 t)
                 ( 5 nil)
                 ( 5 t)
                 ( 6 nil)
                 ( -1 nil)
                 ( -2 t)
                 ( -2 nil)
                 ( -3 t)
                 ( -3 nil)
                 ( -4 t)
                 ( -4 nil)
                 ( -5 nil)
                 ( -6 t)
                 ( -6 nil)
                 ( -7 t))))
    (elt liste (if (>= r 0) r (+ 11 (- r))) )))
               
(defun sign (n) (if (< n 0) t nil))
                   
;; ------------------------------------------------------------------------------
;; calcule la duree effective  coder
;; ------------------------------------------------------------------------------

(defun get-dur (dur)
  (if (numberp dur) 
    dur
    (get-dur (car (last dur)))))


;; ------------------------------------------------------------------------------
;; code-nuplet
;; ------------------------------------------------------------------------------
;; ---------------- version FINALE98 --------------------------------


(defun code-nuplet (dur premier avant-dernier type )
  
  ;(format t "dur=~S ~%" dur)
  ;(format t "premier=~S ~%" premier)
  ;(format t "avant-dernier=~S ~%" avant-dernier)
  ;(format t "type~S ~%" type)
  (cond ((= type 2)
         (setq *codage* (list
                         (format nil "^TP(0,~S) ~S ~S ~S ~S ~S "
                                 premier
                                 (third (third dur))
                                 (* (eval (car (last (third dur)))) *noire*)
                                 (second (third dur))
                                 (* (eval (car (last (third dur)))) *noire*)
                                 avant-dernier)
                         (format nil "^TP(0,~S) 0 22 0 12 8705 " premier)
                         (format nil "^TP(0,~S) -12 0 -12 0 0 " premier))))
        
        ((= type 1) (setq *liste-nuplet* (cons (format nil "^TP(0,~S) ~S ~S ~S ~S ~S "
                                                       premier
                                                       (third (second dur))
                                                       (* (eval (car (last (second dur)))) *noire*)
                                                       (second (second dur))
                                                       (* (eval (car (last (second dur)))) *noire*)
                                                       avant-dernier)
                                               (cons (format nil "^TP(0,~S) 0 70 0 12 8705 " premier)
                                                     (cons (format nil "^TP(0,~S) -12 0 -12 0 0 " premier)
                                                           
                                                           *liste-nuplet*))))))
  (if *codage* 
    (progn
      (setq *liste-nuplet*  (append *codage* *liste-nuplet*))
      (setq *codage* nil))))

;; ------------------------------------------------------------------------------
;; code-mesure
;; ------------------------------------------------------------------------------


;; ---------------- version FINALE98 --------------------------------
(defun code-mesure()
  ;;(format t "dans code-mesure --- *pos-cour*=~S ~%" *pos-cour*)
  ;;(format t "dans code-mesure --- *vrai-dernier*=~S ~%" *vrai-dernier*)
  ;;(format t "dans code-mesure --- *num-mes-FR*=~S ~%" *num-mes-FR*)
  ;;(format t "dans code-mesure --- *total-des-pos*=~S ~%" *total-des-pos*)
  ;;(format t "dans code-mesure --- *mes-non-finie*=~S ~%" *mes-non-finie*)
  ;;(format t "dans code-mesure --- *first-ev*=~S ~%" *first-ev*)
  ;;(format t *"dans code-mesure --- last-ev*=~S ~%" *last-ev*)
  (setq *liste-mesures* (cons (format nil "^FR(~S) ~S ~S 0 0 "
                                      *num-mes-FR*
                                      *first-ev*
                                      *last-ev*)
                              *liste-mesures*))
  ;(format t "dans code-mesure --- *cur-portee*=~S ~%" *cur-portee*)
  ;(format t "dans code-mesure --- *num-canal-mesure*=~S ~%" *num-canal-mesure*)
  (setq *liste-des-GF* (cons (format nil "^GF(~S,~S) 0 75 0 0 0 "
                                     *cur-portee* *num-canal-mesure*)
                             
                             (cons (format nil "^GF(~S,~S) 0 0 ~S 0 0 "
                                           *cur-portee*
                                           (if *mes-non-finie*
                                             (progn
                                               (setq *mes-non-finie* nil) *num-canal-mesure*)
                                             *num-canal-mesure*)
                                           *num-mes-FR*)
                                   *liste-des-GF*))))

;; ---------------- version FINALE2001  -------------------------------- ^FR
(defun code-mesure01()
  ;;(format t "dans code-mesure --- *pos-cour*=~S ~%" *pos-cour*)
  ;;(format t "dans code-mesure --- *vrai-dernier*=~S ~%" *vrai-dernier*)
  ;;(format t "dans code-mesure --- *num-mes-FR*=~S ~%" *num-mes-FR*)
  ;;(format t "dans code-mesure --- *total-des-pos*=~S ~%" *total-des-pos*)
  ;;(format t "dans code-mesure --- *mes-non-finie*=~S ~%" *mes-non-finie*)
  ;;(format t "dans code-mesure --- *first-ev*=~S ~%" *first-ev*)
  ;;(format t "dans code-mesure --- *last-ev*=~S ~%" *last-ev*)
  (setq *liste-mesures* (cons (format nil "^FR(~S) ~S ~S 0 0 "
                                      *num-mes-FR*
                                      *first-ev*
                                      *last-ev*)
                              *liste-mesures*))
  (prepaGF01 *cur-portee* *mes-non-finie* *num-canal-mesure* *num-mes-FR*))

(defun prepaGF01 (cp mnf ncm nmFR)
  ;(format t "dans prepaGF --- *cur-portee*=~S ~%" *cur-portee*)
  ;(format t "dans prepaGF --- *num-canal-mesure*=~S ~%" *num-canal-mesure*)
  ;(format t "dans prepaGF --- *layer*=~S ~%" *layer*)
  ;(format t "dans prepaGF --- *lprov-GF*=~S ~%" *lprov-GF*)
  (if mnf (setq *mes-non-finie* nil))
  (setq *lprov-GF* (cons (list cp ncm *layer* nmFR) *lprov-GF*)))


;; ---------------- version FINALE2004  -------------------------------- ^FR
(defun code-mesure04()
  ;;(format t "dans code-mesure --- *pos-cour*=~S ~%" *pos-cour*)
  ;;(format t "dans code-mesure --- *vrai-dernier*=~S ~%" *vrai-dernier*)
  ;;(format t "dans code-mesure --- *num-mes-FR*=~S ~%" *num-mes-FR*)
  ;;(format t "dans code-mesure --- *total-des-pos*=~S ~%" *total-des-pos*)
  ;;(format t "dans code-mesure --- *mes-non-finie*=~S ~%" *mes-non-finie*)
  ;;(format t "dans code-mesure --- *first-ev*=~S ~%" *first-ev*)
  ;;(format t "dans code-mesure --- *last-ev*=~S ~%" *last-ev*)
   (setq *liste-mesures* (cons (format nil "^FR(~S) ~S ~S 0 0 "
                                        ;(1+ *ref-FR*)
                                        *ref-FR*
                                        *first-ev*
                                        *last-ev*)
                                
                                *liste-mesures*))
  
  (prepaGF04 *cur-portee* *mes-non-finie* *num-canal-mesure* *ref-FR*)
  (setq *ref-FR* (+ 1 *ref-FR*)))

(defun prepaGF04 (cp mnf ncm refFR)
  ;(format t "dans prepaGF --- *cur-portee*=~S ~%" *cur-portee*)
  ;(format t "dans prepaGF --- *num-canal-mesure*=~S ~%" *num-canal-mesure*)
  ;(format t "dans prepaGF --- *layer*=~S ~%" *layer*)
  ;(format t "dans prepaGF --- *lprov-GF*=~S ~%" *lprov-GF*)
  
  (if mnf (setq *mes-non-finie* nil))
  (setq *lprov-GF* (cons (list cp ncm (list *layer* refFR)) *lprov-GF*)))
(defun 2pr (l)
  (cons (car l) (list (cadr l))))

(defun souslist (a l)
(if (equal a (2pr l)) l))

(defun extraitl04 (a l &optional (res nil)) ; pour 2004
  (let* ((aa (car l))
        (bb (souslist (2pr a) aa)))
    (if l
      (extraitl04 a (cdr l) (if bb (cons bb res) res))
      res)))

;(extraitl '(1 2 30 50) '((1 2 3 4) (1 2 5 6) (3 4 5 6)))
; --> ((1 2 5 6) (1 2 3 4))

(defun extraitl01 (a l &optional (res1 nil) (res2 nil)) ; pour 2001
  (let* ((aa (car l))
         (bb (souslist (2pr a) aa)))
    (if l
      (extraitl01 a (cdr l) (if bb (cons bb res1) res1) (if bb res2 (cons aa res2)))
      (list res1 (reverse res2)))))

(defun tab (l &optional (ta (make-array 4)))
  (if l 
    (progn (setf (aref ta (1- (third (car l)))) (fourth (car l)))
          (tab (cdr l) ta))) ta)

;; ---------------- version 2001 --------------------------------
(defun code-GF01 (l) ;(print (list "code-GF" l));; code dans *lprov-GF* la sous-liste des GF correspondant  un canal de 1 ou plusieurs layers
  (if l
    (let* ((a (first l))
           (b (extraitl01 a l))
           (l1 (trie3 (first b)))
           (l2 (car (last b))))
      (if (> (length l1) 4) (print (list "ERREUR dans CODE-GF: plus de 4 layer" l1)))
      (if l1 (let ((ta (tab l1))
                   (x1 (first a))
                   (x2 (second a)))
               (setq *liste-des-GF* (cons (format nil "^GF(~S,~S) ~S 75 0 0 0 "
                                              x1 x2 (aref ta 3))
                                      (cons (format nil "^GF(~S,~S) 0 0 ~S ~S ~S "
                                                    x1 x2 (aref ta 0) (aref ta 1) (aref ta 2)) *liste-des-GF*)))))
      (code-GF01 l2))))

;; ---------------- version FINALE2004 --------------------------------


;; tableau reoit une liste de liste de type ((num_layer ref_FR) ...)
;; tablau retourne une table de 4. Rang 1 pour layer 1, rang 2 pour layer 2 ...
;; La valeur porte dans le tableau est l'indice (dans FR()) du "staff-mesure-layer" correspondant

(defun tableau (l &optional (ta (make-array 4)))
  (if l 
    (let* ((l1 (car l))
           (layer (car l1))
           (ref (second l1)))
      (if (<= layer 4)
       ; (progn (setf (aref ta (1- layer)) (1+ ref))
               (progn (setf (aref ta (1- layer)) ref)
               (tableau (cdr l) ta))
        (format t "ERREUR dans TABLEAU --- N de Layer=~S suprieur  4~%" layer)))
    ta))

; (tableau '((1 7) (4 1))) --> #(8 0 0 2)

(defun code-GF04 (l)
       (if l
         (let* ((e (car l))
                (n-staff (first e))
                (n-mesure (second e))
                (listlayer (cddr e))
                (ta (tableau listlayer))) ; configure la tableau "tableau" de type (a 0 0 b) si layer 1 et 4
           
           (setq *liste-des-GF* (cons (format nil "^GF(~S,~S) ~S ~S 0 0 0 "
                                              n-staff n-mesure (aref ta 2) (aref ta 3))
                                      (cons (format nil "^GF(~S,~S) 0 0 75 ~S ~S "
                                                    n-staff n-mesure (aref ta 0) (aref ta 1)) *liste-des-GF*)))
           (code-GF04 (cdr l)))))


;; ------------------------------------------------------------------------------
;; code-portee, code-portee-clef, code-description-mesure
;; ------------------------------------------------------------------------------


(defun code-portee ()
(setq *liste-portee* (cons (format nil "^PL(~S) 0 0 0 0 0 0 " *cur-portee*) *liste-portee*))
(setq *liste-portee* (cons (format nil "^PL(~S) 20565 0 0 0 0 0 " *cur-portee*) *liste-portee*)))

;; ---------------- version FINALE2001 --------------------------------
(defun code-portee-clef01 ()
  (setq *liste-de-portee-clef* 
        (cons (format nil "^IS(~S) -772 -772 -4 0 0 0 " *cur-portee*)
              (cons (format nil "^IS(~S) 0 0 5 0 0 0 " *cur-portee*)
                    ;(cons (format nil "^IS(~S) 0 0 0 0 0 0 " *cur-portee*)
                    (cons (format nil "^IS(~S) 0 0 1792 0 0 0 " *cur-portee*) ;; modif 15 mai 2001
                          *liste-de-portee-clef*)))))



;; ---------------- version FINALE2004 --------------------------------
(defun code-portee-clef04 ()
  (setq *liste-de-portee-clef* 
        
        (cons (format nil "^IS(~S) 0 0 0 " *cur-portee*)
              (cons (format nil "^IS(~S) 0 0 0 " *cur-portee*)
                    (cons (format nil "^IS(~S) 1536 -1024 9247 0 " *cur-portee*)
                          (cons (format nil "^IS(~S) -772 -772 -4 0 0 -517 " *cur-portee*)
                                (cons (format nil "^IS(~S) 0 0 5 0 0 0 " *cur-portee*)
                                      (cons (format nil "^IS(~S) 0 0 26368 0 0 0 " *cur-portee*) ;; modif 15 mai 2001
                                            *liste-de-portee-clef*))))))))


(defun numppal (f) 
  ;;(multiple-value-bind (v r) (floor f)
    (max (nump (floor f)) (floor (/ (numerator f) (denominator f))) 3)) 

(defun nump (v &optional (n 1)) 
  (let ((vn (* v n)))
    (cond ((or (< vn 1) (> vn 5)) 0)     ;; 5 = maximum admis du numrateur principal de time-signature
          ((and (>= vn 3) (<= vn 5)) vn)
          (t (nump v (1+ n))))))

(defun denppal2 (f)
  (let ((result (denp (numppal f) f)))
    (if (= result 0) (print "ERREUR DE MESURE DANS DENPPAL")
        result))) 

(defun denppal (a)
  (/ *noire* (denppal2 a)))

(defun denp (a f &optional (n 1))
  (cond ((> n 32) 0)
        ((<= (/ a n) f) n)
        (t (denp a f (1+ n)))))

(defun densecond (f)
  (/ *noire* (denominator f)))

(defun numsecond (f)
  (* (- f (/ (numppal f) (denppal2 f))) (denominator f)))

(defun code-description-mesure (version)
  (cond
   ((= version 2001) (cdm01 *liste-barres-mesures*))  
   ((= version 2004) (cdm04 *liste-barres-mesures*))
   (t (format nil "ERREUR dans init-decode-chan, pas de version ~S" version))))


;; ---------------- version FINALE2001b --------------------------------
(defun cdm01 (l &optional (i 0))
  (let ((maxmes (length *liste-barres-mesures*)))
    (if (< i maxmes)                                  ;;avant: *max-mesure* au lieu de maxmes : cliner *max-mesure* -------- 
      (let ((lm (car l)))
        (cond
         
         
         ((= lm 3/2)
          (setq *liste-decription-mesures*
                (cons (format nil "^MS(~S) 0 0 240 0 0 0 " (+ 1 i))  ;; rajout le 16 05 2001
                      (cons (format nil (concatenate 'string "^MS(~S) 360 0 " "3" " 512 6 16 ")    ;; rajout le 18 12 2000
                                    (+ 1 i)) *liste-decription-mesures*))))
         
         ((= lm 1)
          (setq *liste-decription-mesures*
                (cons (format nil "^MS(~S) 0 0 240 0 0 0 " (+ 1 i))  ;; rajout le 16 05 2001
                      (cons (format nil (concatenate 'string "^MS(~S) 360 0 " "1" " 1024 6 16 ")    ;; rajout le 18 12 2000
                                    (+ 1 i)) *liste-decription-mesures*))))
         
         ((= lm 1/2)
          (setq *liste-decription-mesures*
                (cons (format nil "^MS(~S) 0 0 240 0 0 0 " (+ 1 i))  ;; rajout le 16 05 2001
                      (cons (format nil (concatenate 'string "^MS(~S) 360 0 " "1" " 512 6 16 ")    ;; rajout le 18 12 2000
                                    (+ 1 i)) *liste-decription-mesures*))))
         ((integerp lm)
          (setq *liste-decription-mesures*
                (cons (format nil "^MS(~S) 0 0 240 0 0 0 " (+ 1 i))  ;; rajout le 16 05 2001
                      (cons (format nil (concatenate 'string "^MS(~S) 360 0 " (write-to-string lm) " 1024 6 16 ")    ;; rajout le 18 12 2000
                                    (+ 1 i)) *liste-decription-mesures*))))
         
         (t
          (setq *liste-decription-mesures*
                (cons (format nil "^MS(~S) 0 0 240 0 0 0 " (+ 1 i))   ;; rajout le 16 05 2001
                      (cons (format nil "^MS(~S) 360 0 ~S ~S 198 16" (+ i 1) (+ 3 i) (+ 3 i))  *liste-decription-mesures*)))
          (let ((a1 (denppal lm)) (a2 (densecond lm)) (b1 (numppal lm)) (b2 (numsecond lm)))
            (cond
             ((= lm 5/2) (setq a1 1024) (setq a2 512) (setq b1 2) (setq b2 1)))    ; cas particulier de dure 3/2 --> code 3/8
            
            (setq *liste-des-TL*
                  (cons (format nil "^TL(~S) ~S 1 ~S 1 0 0" (+ 3 i) a1 a2) *liste-des-TL*))
            (setq *liste-des-TU*
                  (cons (format nil "^TU(~S) ~S 0 1 ~S 0 1" (+ 3 i) b1 b2) *liste-des-TU*))
            
            )))
        (cdm01 (cdr l) (1+ i))))))

;; -------------------------------------------------------------------- 

(defun analyse-mesure (lm)
  (multiple-value-bind (x y) (floor lm)
    (let ((numx (numerator x))
          (denx (denominator x))
          (numy (numerator y))
          (deny (denominator y)))
      (list numx (/ 1024 denx) numy (/ 1024 deny)))))

;(analyse-mesure 5/2) --> (2 1024 1 512)

;; ---------------- version FINALE2004 -------------------------------- 
(defun cdm04 (l &optional (i 1))
  (if l                                  
    (let ((lm (car l)))
      (cond
       
       ((or (integerp lm) (< lm 1))                                      ;cas des mesures entires ou < 1noire                    
        (setq *liste-decription-mesures*
              (cons (format nil "^MS(~S) 4 1024 240 0 0 0 " i) 
                    (cons (format nil "^MS(~S) 360 0 ~S ~S 6 16 " i (numerator lm) (/ 1024 (denominator lm))) 
                          *liste-decription-mesures*))))
       
       (t                                                                ;cas des mesures composites
        (setq *liste-decription-mesures*
              (cons (format nil "^MS(~S) 4 1024 240 0 0 0 " i) 
                    (cons (format nil "^MS(~S) 360 0 ~S ~S 196 16 " i (+ 6 (* 3 i)) (* 3 i))
                          *liste-decription-mesures*)))
        (let ((v (analyse-mesure lm)))
          (setq *liste-des-TL*
                (cons (format nil "^TL(~S) ~S 1 ~S 1 0 0 " (* 3 i) (second v) (fourth v)) *liste-des-TL*))
          (setq *liste-des-TU*
                (cons (format nil "^TU(~S) ~S 0 1 ~S 0 1 " (+ 6 (* 3 i)) (first v) (third v)) *liste-des-TU*)))))
      (cdm04 (cdr l) (1+ i)))))


;;-------------- Version Finale98???? --------------
;(defun a (i)
;  (- 0 (+ 150 (* (- i 1) 280))))


;;-------------- Mars 2005 Version Finale2004 --------------
(defun a04 (i)
  (- 0 (* (1- i) 320)))


;; ---------------- version FINALE98 --------------------------------
;(defun code-divers ()
;  (dotimes (i *cur-portee*)
;   (let ((j (1+ i)))
;      (setq *liste-position-portee* (cons (format nil "^IU(0) ~S 0 0 0 ~S" j (a j))
;                                          *liste-position-portee*)))))


;; ---------------- version FINALE2001d --------------------------------


(defun code-divers ()
  (dotimes (i *cur-portee*)
           (let ((j (1+ i)))
             (setq *liste-position-portee* (cons (format nil "^Iu(0) 1 0 32767 2147483647 ")
                                                 (cons (format nil "^Iu(0) ~S 0 0 0 ~S " j (a04 j))
                                                       *liste-position-portee*))))))



;; ------------------------------------------------------------------------------
;; criture du fichier ENIGMA
;; ------------------------------------------------------------------------------

;(defparameter *enigma-dir* "ccl:enigma includes;")                    ; directory o on sauve
;(defparameter *enigma-include-dir01* "ccl:enigma includes;D2001;")    ; directory o on lit (version 2001)
;(defparameter *enigma-include-dir04* "ccl:enigma includes;D2004;")    ; directory o on lit (version 2001)

(defparameter *enigma-dir* (make-pathname :directory (append (butlast (pathname-directory (LISP-IMAGE-NAME)) 3) '("enigma includes"))))
(defparameter *enigma-include-dir01* (make-pathname :directory (append (butlast (pathname-directory (LISP-IMAGE-NAME)) 3) '( "enigma\ includes/D2001"))))    ; directory o on lit (version 2001)
(defparameter *enigma-include-dir04* (make-pathname :directory (append (butlast (pathname-directory (LISP-IMAGE-NAME)) 3) '( "enigma\ includes/D2004"))))   ; directory o on lit (version 2004)


(defun incorporate-file-01 (dst-stream src-filename)
  (with-open-file (f (merge-pathnames *enigma-include-dir01* src-filename)) 
    (do ((l (read-line f nil nil) (read-line f nil nil))) 
        ((null l)) 
      (write-line l dst-stream))))

(defun incorporate-file-04 (dst-stream src-filename)
  (with-open-file (f (merge-pathnames *enigma-include-dir04* src-filename)) 
    (do ((l (read-line f nil nil) (read-line f nil nil))) 
        ((null l)) 
      (write-line l dst-stream))))


(defun incorporate-list (dst-stream list)
  (dolist (l list)
    (write-line l dst-stream)))

;; -------------- version FINALE98 et 2001---------------------------------------

(defun write-enigma-file01 (dst-filename)
  (with-open-file (dst-stream (merge-pathnames *enigma-dir* dst-filename) 
                              :direction :output 
                              :if-exists :supersede)
    (incorporate-file-01 dst-stream "fich1")
    (incorporate-list dst-stream (reverse *liste-mesures*))
    (incorporate-file-01 dst-stream "fich2")
    (incorporate-list dst-stream (reverse *liste-de-portee-clef*))
    (incorporate-file-01 dst-stream "fich3")
    (incorporate-list dst-stream (reverse *liste-position-portee*))   ;;modifi le 15 mai 2001 (finale 2001d)
    (incorporate-file-01 dst-stream "fich3bis")
    (incorporate-list dst-stream (reverse *liste-decription-mesures*))
    (incorporate-file-01 dst-stream "fich4")
    (incorporate-list dst-stream (reverse *liste-portee*))
    (incorporate-list dst-stream (code-pages *last-ev* ))         ;; ajout le 22 03 05 pour FINALE2005 (codage des pages)

    (incorporate-file-01 dst-stream "fich5")
    (incorporate-list dst-stream *liste-des-TL*)
    (incorporate-list dst-stream *liste-des-TU*)
    (incorporate-file-01 dst-stream "fich6")
    (incorporate-list dst-stream (reverse *liste-des-expressions*))   ;;code expressions  19 10 01
    (incorporate-list dst-stream (reverse *liste-des-GF*))
    (incorporate-file-01 dst-stream "fich6bis")                  ;;rajout le 15 mai 2001 (finale 2001d)
    (incorporate-list dst-stream (reverse *liste-des-IM*))
    ;(incorporate-list dst-stream (reverse *liste-crochet*))
    (incorporate-list dst-stream *liste-nuplet*)
    (incorporate-list dst-stream (reverse *liste-des-veloc*))
    (incorporate-list dst-stream (reverse *liste-des-tete*)) ;; 31 08 01 liste des formes de tte de note
    (incorporate-file-01 dst-stream "fich7")
    (incorporate-list dst-stream (reverse *liste-note*))
    (incorporate-file-01 dst-stream "fich8")
    ;;(set-mac-file-type dst-stream "ETF3")  13/05/09
    ))


;; -------------- version FINALE2004 ---------------------------------------

(defvar debTU)
(setq debTU '("^TU(1) 3 0 1 2 0 0 " "^TU(2) 3 0 1 2 0 0 " "^TU(3) 4 0 1 3 0 0 " "^TU(4) 4 0 1 3 0 0 " "^TU(5) 4 0 1 4 0 0 " "^TU(6) 4 0 1 4 0 0 " )) 


(defun write-enigma-file04 (dst-filename) 
  (with-open-file (dst-stream (merge-pathnames *enigma-dir* dst-filename) 
                              :direction :output 
                              :if-exists :supersede)
    (incorporate-file-04 dst-stream "fich1-04")
    (incorporate-list dst-stream (code-BC (length *liste-barres-mesures*)))  ;; ajout le 22 03 05 pour FINALE2005 (codage comlmentaire des mesures)
    (incorporate-file-04 dst-stream "fich1bis-04")
    (incorporate-list dst-stream (reverse *liste-mesures*))
    (incorporate-file-04 dst-stream "fich2-04")
    (incorporate-list dst-stream (reverse *liste-de-portee-clef*))
    (incorporate-file-04 dst-stream "fich3-04")
    (incorporate-list dst-stream (reverse *liste-position-portee*))   ;;modifi le 15 mai 2001 (finale 2001d)
    (incorporate-file-04 dst-stream "fich3bis-04")
    (incorporate-list dst-stream (reverse *liste-decription-mesures*))
    (incorporate-file-04 dst-stream "fich4-04")
      ;********************************

    (incorporate-file-04 dst-stream "fich5-04")
    (incorporate-list dst-stream (reverse *liste-portee*))
    (incorporate-list dst-stream (code-pages *last-ev* ))         ;; ajout le 22 03 05 pour FINALE2005 (codage des pages)
    (incorporate-file-04 dst-stream "fich5bis-04")
    (incorporate-list dst-stream (reverse *liste-des-TL*))
    (incorporate-list dst-stream (append debTU (reverse *liste-des-TU*)))
    (incorporate-file-04 dst-stream "fich6-04")
    (incorporate-list dst-stream (reverse *liste-des-expressions*))   ;;code expressions  19 10 01
    (incorporate-list dst-stream (reverse *liste-des-GF*))
    (incorporate-file-04 dst-stream "fich6bis-04")                  ;;rajout le 15 mai 2001 (finale 2001d)
    (incorporate-list dst-stream (reverse *liste-des-IM*))
    ;(incorporate-list dst-stream (reverse *liste-crochet*))
    (incorporate-list dst-stream *liste-nuplet*)
    (incorporate-list dst-stream (reverse *liste-des-veloc*))
    (incorporate-list dst-stream (reverse *liste-des-tete*)) ;; 31 08 01 liste des formes de tte de note
    (incorporate-file-04 dst-stream "fich7-04")
    (incorporate-list dst-stream (reverse *liste-note*))
    (incorporate-file-04 dst-stream "fich8-04")
    ;;(set-mac-file-type dst-stream "ETF3")  13/05/09
    ))


;; ------------------------------------------------------------------------------
;; sauver le fichier prepa de concatnantion des listes (ne sert plus)
;; ------------------------------------------------------------------------------

#|
(defun write-prepa-file (dst-filename varasauver)
  (with-open-file (dst-stream (merge-pathnames *enigma-dir* dst-filename) 
                              :direction :output 
                              :if-exists :supersede)
    (incorporate-file dst-stream (format nil "~S" varasauver))
    (set-mac-file-type dst-stream "TEXT")))
|#

;; ------------------------------------------------------------------------------
;; lancer l'criture
;; ------------------------------------------------------------------------------

;; Modif Mai 2001 (voir intro)
;; ---------------------------
(defun finale2001 (nom-fich)
  (decode 2001)
  (write-enigma-file01 nom-fich)
  "Enigma 2001")
  
;; Ajout du 16 Mars 2005
;; ---------------------------
(defun finale2004 (nom-fich)
  (decode 2004) 
  (write-enigma-file04 nom-fich)
  "Enigma 2004")


