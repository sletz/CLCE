;;==============================================================================

;;         OUTILS LISP POUR LA COMPOSITION PAR PROCESSUS
;;                   PIERRE ALAIN JAFFRENNOU

;;          VERSION NOVEMBRE 1997
;;          Modification de Glissando le 14-12-2000
;;          Ajout des arpeges le 04 02 2001
;;          Modification en mai 2001
;;          Modification du 18 octobre 2001:
;;              - créationd'un 7ème champ dans les listes de notes en direction de FINALE
;;              - Ce champ code les TEXTES EXPRESSIONS de FINALE
;;          Ajout de make-control le 28 Février 2005: 
;;              - pour la prise en compte du paramètre LCONTROL de GNOTES
;;          Modification 02 03 05: compatibilité de :sil avec les n-olets
;;          Modification 08 03 05: TRANSPOSE ET TRANSPOLISTE. Ajout dans :ltransp du type: (5) = liste de 1
;;==============================================================================

;;         PRODUCTION DES NOTES MIDI ET PRÉ-ENIGMA (PARTITION FINALE)
;;         Ensemble des procédures de calcul de la score MIDI 
;;         et des données permettant la production d'un fichier de type ENIGMA

;;==============================================================================







;;==============================================================================
;;              DEFINITION DES VARIABLES
;;==============================================================================


(defvar maxvol)       ;maximum du volume
(defvar maj)          ;gamme majeure
(defvar ecoute nil)   ;sequence de mixage
(defvar *pas-temps*)  ;pas temporel

(defvar *bend* )           ;echelle courante du pitch-bend
(defvar *bend-demi-ton*)   ;excursion de la molette pour un demi-ton

;;------------------------------------------------------------- 02 03 05
;; Pour la mise en place de :sil hors n-olet dans note-motif
;; stocke la durée réelle cumulée des éléments d'un n-olet au cours de sa mise en place

(defvar *totrythm*)

;;------------------------------------------------------------- 


;;============================================================
;; Une liste par canal généralisé (de 0 à 31)
;; Chaque liste stocke les infos en vue de la partition FINALE
;;============================================================

(defvar *liste0* nil)
(defvar *liste1* nil)
(defvar *liste2* nil)
(defvar *liste3* nil)
(defvar *liste4* nil)
(defvar *liste5* nil)
(defvar *liste6* nil)
(defvar *liste7* nil)
(defvar *liste8* nil)
(defvar *liste9* nil)
(defvar *liste10* nil)
(defvar *liste11* nil)
(defvar *liste12* nil)
(defvar *liste13* nil)
(defvar *liste14* nil)
(defvar *liste15* nil)
(defvar *liste16* nil)
(defvar *liste17* nil)
(defvar *liste18* nil)
(defvar *liste19* nil)
(defvar *liste20* nil)
(defvar *liste21* nil)
(defvar *liste22* nil)
(defvar *liste23* nil)
(defvar *liste24* nil)
(defvar *liste25* nil)
(defvar *liste26* nil)
(defvar *liste27* nil)
(defvar *liste28* nil)
(defvar *liste29* nil)
(defvar *liste30* nil)
(defvar *liste31* nil)
(defvar *liste32* nil)
(defvar *liste33* nil)
(defvar *liste34* nil)
(defvar *liste35* nil)
(defvar *liste36* nil)
(defvar *liste37* nil)
(defvar *liste38* nil)
(defvar *liste39* nil)
(defvar *liste40* nil)
(defvar *liste41* nil)
(defvar *liste42* nil)
(defvar *liste43* nil)
(defvar *liste44* nil)
(defvar *liste45* nil)
(defvar *liste46* nil)
(defvar *liste47* nil)

(defvar *layer* 1)           ;; 14 11 01 mémorise le n° du layer courant (pour FINALE)
(defvar *ref-FR*)            ;; référence des FR() dans code-mesure
(defvar *lprov-GF* nil)      ;; liste provisoire de GF (type de codage ENIGMA)


(defvar *etat-pgchge* (make-array 32))  ;; Fabrication d'un tableau de 32

(defun init-array (a)
  (dotimes (i 32)
      (setf (aref a i) -1)))            ;; Pour initialiser ultérieurement le tableau *etat-pgchge*


(defun init-listes ()                   ;; Procédure d'initialisation des listes et du tableau *etat-pgchge*
  (setq *liste0* nil)
  (setq *liste1* nil)
  (setq *liste2* nil)
  (setq *liste3* nil)
  (setq *liste4* nil)
  (setq *liste5* nil)
  (setq *liste6* nil)
  (setq *liste7* nil)
  (setq *liste8* nil)
  (setq *liste9* nil)
  (setq *liste10* nil)
  (setq *liste11* nil)
  (setq *liste12* nil)
  (setq *liste13* nil)
  (setq *liste14* nil)
  (setq *liste15* nil)
  (setq *liste16* nil)
  (setq *liste17* nil)
  (setq *liste18* nil)
  (setq *liste19* nil)
  (setq *liste20* nil)
  (setq *liste21* nil)
  (setq *liste22* nil)
  (setq *liste23* nil)
  (setq *liste24* nil)
  (setq *liste25* nil)
  (setq *liste26* nil)
  (setq *liste27* nil)
  (setq *liste28* nil)
  (setq *liste29* nil)
  (setq *liste30* nil)
  (setq *liste31* nil)
  (setq *liste32* nil)
  (setq *liste33* nil)
  (setq *liste34* nil)
  (setq *liste35* nil)
  (setq *liste36* nil)
  (setq *liste37* nil)
  (setq *liste38* nil)
  (setq *liste39* nil)
  (setq *liste40* nil)
  (setq *liste41* nil)
  (setq *liste42* nil)
  (setq *liste43* nil)
  (setq *liste44* nil)
  (setq *liste45* nil)
  (setq *liste46* nil)
  (setq *liste47* nil)
  (init-array *etat-pgchge*))

(defun init ()                        ;; Initialisation complète avant calculs
  (init-listes)
  (setq *totrythm* 0)
  (p-abs 0)
  (midi-clear *out*))



;;==============================================================================
;;            GESTION DES POINTEURS TEMPORELS
;;==============================================================================


;;=======================================================
;; Place le pointeur à la date absolue "date"
;;=======================================================
          
(defun p-abs (date)
  (midi-move *out* :date date))

;;=======================================================
;;Déplace le pointeur d'une durée "dur"
;;=======================================================
          
(defun p-rel (dur)
  (midi-move *out* :dur dur))

;;=======================================================
;; Retourne l'événement courant
;;=======================================================
          
(defun ev? ()
  (midi-avail-ev *out* ))

;;=======================================================
;; Rend la date courante
;;=======================================================
          
(defun date? ()
  (midi-move *out* ))




;;==============================================================================
;;                           NOTES: NOTE A HAUTEUR NON ENTIERE
;;
;;                 Configurer le pitch bend sur tous les canaux
;;                       CONFORMÉMENT À LA CONSTANTE *bend*
;;                  définie dans le fichier 4Processus.lisp
;;==============================================================================



; le 19 10 01 (replie h à l'intérieur de la liste l si l est une liste
; sinon ne transpose pas si l est un nombre (voir ltransp dans gnotes)
; CORRECTIONS DU 08 03 05

(defun transpose (l h)
  (if (listp l)
    (cond
     ((= 2 (length l)) (tran l (round h))) ; cas du repliement dans un interval (change MIDI et FINALE)
     ((= 1 (length l)) (+ h (car l)))      ; cas dE LA TRANSPOSITION DU CLAVIER (change MIDI seulement)
     ((eq nil l) h)                        ; cas dE LA TRANSPOSITION D'écriture (change FINALE seulement)
     (t (print "ERREUR DANS TRANSPOSE: LA LISTE DE TRANSPOSITION CONTIENT PLUS DE 2 VALEURS"))) ; cas: pas de transposition
    h))

(defun cherchepgchange (chan l)   ;; recherche si un pgchange est associée au canal chan  - 24 10 01 -
  (if l
    (if (= chan (caar l)) (second (car l)) (cherchepgchange chan (cdr l))) nil))


(defun place-pgchange (chan port pgchange) ;; écrit le cas échéant un pgchange
       (let* ((superchan (+ chan (* port 16)))
              (pgchg (cherchepgchange superchan pgchange))) ;(print (list "place-pgchange **********" pgchg (aref *etat-pgchge* superchan)))
         (if (and pgchg (not (= pgchg (aref *etat-pgchge* superchan))))
           (progn (setf (aref *etat-pgchge* superchan) pgchg)
                  (midi-write-ev *out* (prog-change :pgm pgchg :chan chan :port port))))))


;;--------------------------------------------------------------------- 21 12 2001
;; place le prgchange pgchg à la date dat sur le canal (0-31) superchan

(defun pgchg-at-dat (dat superchan pgchg)
  (let* ((chan (nchan superchan))
         (port (nport superchan)))
    
    (if (and pgchg (not (= pgchg (aref *etat-pgchge* superchan))))
      (progn (setf (aref *etat-pgchge* superchan) pgchg)
             (p-abs (* dat noi))
             (midi-write-ev *out* (prog-change :pgm pgchg :chan chan :port port))))))

#|
(defun exemple ()
  (place-tempo 0 60 ecoute)
  (pgchg-at-dat 1 16 40)
  (pgchg-at-dat 2 16 40)
  (pgchg-at-dat 3 16 41))
|#
;;--------------------------------------------------------------------------  21 12 2001
;; place le prgchange pgchg à la date courante sur le canal (0-31) superchan

(defun pgchg-no-dat (superchan pgchg)
  (let* ((chan (nchan superchan))
         (port (nport superchan)))
    
    (if (and pgchg (not (= pgchg (aref *etat-pgchge* superchan))))
      (progn (setf (aref *etat-pgchge* superchan) pgchg)
             (midi-write-ev *out* (prog-change :pgm pgchg :chan chan :port port))))))


;;--------------------------------------------------------------------------------------------------------  21 12 2001
;; place, à la date courante, les prgchange de la liste lpg sur les canaux correspondant de la liste lchan

(defun pgchg-no-dat-mult (lchan lpg)
  (mapcar #'pgchg-no-dat lchan lpg))

#|
(defun exemple ()
  (place-tempo 0 60 ecoute)
  (p-abs noi)
  (pgchg-no-dat-mult '(0 1) '(45 50))

  (p-abs (* 2 noi))
  (pgchg-no-dat-mult '(0 1) '(45 60)))
|#


(defun notes ( chan dur h1 ltransp vel pgchange port stac) ;(print (list chan (/ (date?) noi) dur h1 (round h1) pgchange "NOTES"))
       (if (> (date?) 0) (p-rel -1))   ; 28 08 01 on ne peut pas reculer le pointeur à la date 0 !
       (midi-write-ev *out* (pitch-bend :chan chan :bend (round (* (/ 8192 *bend*) (- h1 (round h1)))) :port port))
       ;(midi-write-ev *out* (ctrl-change :ctrl 7 :value 127  :chan chan :port port))   ; 28 08 01 ; suppression le 19 Février 2005 à cause des générateurs de volume
       (if (> (date?) 0) (p-rel +1))   ; 28 08 01
       
       (if pgchange (place-pgchange chan port pgchange))  ;; 24 10 01
       
;; le 4 mai 2007 changement de truncate en round ci-dessous       
       
       (if (> dur 32000) 
         (let ((d (date?))) 
           (midi-write-ev *out* (key-on :pitch (transpose ltransp (round h1)) :vel vel :chan chan :port port))
           (midi-write-ev *out* (key-off :pitch (transpose ltransp (round h1))  :vel vel :chan chan :port port) :date (+ (- (floor (* dur stac)) 1) (date?)))
           (p-abs d))
       (midi-write-ev *out* (note :pitch (transpose ltransp (round h1)) :dur (- (floor (* dur stac)) 1) :vel vel :chan chan :port port))))




;;==============================================================================
;;                           NOTESLISTE
;;
;;           CONSTRUIT LA LISTE DE DONNÉES DES NOTES POUR ENIGMA
;;               
;; datenoire: date de la note en fraction de noire: 2; 19/3; 3/4 ...
;; dursymb: durée (positive) symbolique de la note: (2/3c (1/2 -1)); 4; 1/2 3/4 ...
;; liste des hauteurs de l'accord: (48 50 60) ...
;;==============================================================================

(defun split? (x l)
  (let ((min (apply #'min l))
        (max (apply #'max l)))
    (if (and (> x min) (< x max)) t nil)))

(defun noteliste (chan numexpression laccord datenoire dursymb listhaut ltextexpres grace vel layer)
  ;(print (list datenoire dursymb listhaut))
  (notelist chan numexpression laccord datenoire dursymb listhaut ltextexpres grace vel layer (split? 60 listhaut)))


(defun notelist (chan pgchange laccord datenoire dursymb listhaut ltextexpres grace vel layer split)
  (when laccord
    (if (>= (+ chan (car laccord)) 0)
    (noteli (nchan (+ chan (car laccord))) pgchange (nport (+ chan (car laccord))) datenoire dursymb (list (car listhaut)) ltextexpres grace vel layer split))
    (notelist chan pgchange (cdr laccord) datenoire dursymb (cdr listhaut) ltextexpres grace vel layer split)))

(defun cherchexpression (chan l)   ;; recherche si une expression (au sens FINALE) est associée au canal 24 10 01
  (if l
    (if (= chan (caar l)) (third (car l)) (cherchexpression chan (cdr l))) nil))



(defun noteli ( chan pgchange port datenoire dursymb listhaut ltextexpres grace vel layer split) 
  (declare (ignore split))
  (let* ((superchan (+ chan (* port 16)))
         (y (list datenoire superchan dursymb listhaut grace vel ltextexpres (if pgchange (cherchexpression superchan pgchange) nil) layer))      
         ;; création le 18 10 01 d'un septième champ dans les listes de notes
         ;(yy (list datenoire chan (changder dursymb) listhaut grace vel))     ;; en direction de FINALE. Ce 7eme champ code les TEXTES EXPRESSIONS
         )                                                                     ;; création d'un huitième champ qui code les expressions associées aux pgchange 24 10 01
    ;; de la forme nil si pas de pgchange num-expression-finale sinon
    ;; crtéation le 11 11 01 d'un 9eme champ spécifiant le num de layer FINALE (1 par default)
    
    
    (cond 
     ((and (= chan 0) (= port 0)) (setq *liste0* (cons y *liste0*)))     
     ((and (= chan 1) (= port 0)) (setq *liste1* (cons y *liste1*)))          
     ((and (= chan 2) (= port 0)) (setq *liste2* (cons y *liste2*)))
     ((and (= chan 3) (= port 0)) (setq *liste3* (cons y *liste3*)))
     ((and (= chan 4) (= port 0)) (setq *liste4* (cons y *liste4*)))
     ((and (= chan 5) (= port 0)) (setq *liste5* (cons y *liste5*)))
     ((and (= chan 6) (= port 0)) (setq *liste6* (cons y *liste6*)))
     ((and (= chan 7) (= port 0)) (setq *liste7* (cons y *liste7*)))
     ((and (= chan 8) (= port 0)) (setq *liste8* (cons y *liste8*)))
     ((and (= chan 9) (= port 0)) (setq *liste9* (cons y *liste9*)))
     ((and (= chan 10) (= port 0)) (setq *liste10* (cons y *liste10*)))
     ((and (= chan 11) (= port 0)) (setq *liste11* (cons y *liste11*)))
     ((and (= chan 12) (= port 0)) (setq *liste12* (cons y *liste12*)))  
     ((and (= chan 13) (= port 0)) (setq *liste13* (cons y *liste13*)))
     ((and (= chan 14) (= port 0)) (setq *liste14* (cons y *liste14*)))
     ((and (= chan 15) (= port 0)) (setq *liste15* (cons y *liste15*))) 
     
     ((and (= chan 0) (= port 1)) (setq *liste16* (cons y *liste16*)))     
     ((and (= chan 1) (= port 1)) (setq *liste17* (cons y *liste17*)))          
     ((and (= chan 2) (= port 1)) (setq *liste18* (cons y *liste18*)))
     ((and (= chan 3) (= port 1)) (setq *liste19* (cons y *liste19*)))
     ((and (= chan 4) (= port 1)) (setq *liste20* (cons y *liste20*)))
     ((and (= chan 5) (= port 1)) (setq *liste21* (cons y *liste21*))) 
     ((and (= chan 6) (= port 1)) (setq *liste22* (cons y *liste22*)))     
     ((and (= chan 7) (= port 1)) (setq *liste23* (cons y *liste23*)))          
     ((and (= chan 8) (= port 1)) (setq *liste24* (cons y *liste24*)))
     ((and (= chan 9) (= port 1)) (setq *liste25* (cons y *liste25*)))
     ((and (= chan 10) (= port 1)) (setq *liste26* (cons y *liste26*)))
     ((and (= chan 11) (= port 1)) (setq *liste27* (cons y *liste27*)))
     ((and (= chan 12) (= port 1)) (setq *liste28* (cons y *liste28*)))  
     ((and (= chan 13) (= port 1)) (setq *liste29* (cons y *liste29*)))
     ((and (= chan 14) (= port 1)) (setq *liste30* (cons y *liste30*)))
     ((and (= chan 15) (= port 1)) (setq *liste31* (cons y *liste31*))) 
     
     
     ((and (= chan 1) (= port 2)) (setq *liste32* (cons y *liste32*)))          
     ((and (= chan 2) (= port 2)) (setq *liste33* (cons y *liste33*)))
     ((and (= chan 3) (= port 2)) (setq *liste34* (cons y *liste34*)))
     ((and (= chan 4) (= port 2)) (setq *liste35* (cons y *liste35*)))
     ((and (= chan 5) (= port 2)) (setq *liste36* (cons y *liste36*))) 
     ((and (= chan 6) (= port 2)) (setq *liste37* (cons y *liste37*)))     
     ((and (= chan 7) (= port 2)) (setq *liste38* (cons y *liste38*)))          
     ((and (= chan 8) (= port 2)) (setq *liste39* (cons y *liste39*)))
     ((and (= chan 9) (= port 2)) (setq *liste40* (cons y *liste40*)))
     ((and (= chan 10) (= port 2)) (setq *liste41* (cons y *liste41*)))
     ((and (= chan 11) (= port 2)) (setq *liste42* (cons y *liste42*)))
     ((and (= chan 12) (= port 2)) (setq *liste43* (cons y *liste43*)))  
     ((and (= chan 13) (= port 2)) (setq *liste44* (cons y *liste44*)))
     ((and (= chan 14) (= port 2)) (setq *liste45* (cons y *liste45*)))
     ((and (= chan 15) (= port 2)) (setq *liste46* (cons y *liste46*)))
     ((and (= chan 0) (= port 2)) (setq *liste47* (cons y *liste47*)))
     
     (t (print "ERREUR DE CANAL") (print chan)(print port)  ))))




;;==============================================================================
;;                           SILENCELISTE
;;
;;           CONSTRUIT LA LISTE DE DONNÉES DES SILENCES POUR ENIGMA
;;               
;; datenoire: date du silence en fraction de noire: 2; 19/3; 3/4 ...
;; dursymb: durée (positive) symbolique du silence: (* (FDUR 2 3 D-C) 1/2): 4 ...
;;==============================================================================


(defun silenceliste (chan laccord datenoire dursymbsil layer) 
  (when laccord
    (if (>= (+ chan (car laccord)) 0)
      (silliste (nchan (+ chan (car laccord))) (nport (+ chan (car laccord))) datenoire dursymbsil layer))
    (silenceliste chan (cdr laccord) datenoire dursymbsil layer)))



(defun silliste (chan port datenoire dursymbsil layer)
  (let ((y (list datenoire chan dursymbsil '(20) '(255) nil nil nil layer))) 
    (cond 
     ((and (= chan 0) (= port 0)) (setq *liste0* (cons y *liste0*)))     
     ((and (= chan 1) (= port 0)) (setq *liste1* (cons y *liste1*)))          
     ((and (= chan 2) (= port 0)) (setq *liste2* (cons y *liste2*)))
     ((and (= chan 3) (= port 0)) (setq *liste3* (cons y *liste3*)))
     ((and (= chan 4) (= port 0)) (setq *liste4* (cons y *liste4*)))
     ((and (= chan 5) (= port 0)) (setq *liste5* (cons y *liste5*)))
     ((and (= chan 6) (= port 0)) (setq *liste6* (cons y *liste6*)))
     ((and (= chan 7) (= port 0)) (setq *liste7* (cons y *liste7*)))
     ((and (= chan 8) (= port 0)) (setq *liste8* (cons y *liste8*)))
     ((and (= chan 9) (= port 0)) (setq *liste9* (cons y *liste9*)))
     ((and (= chan 10) (= port 0)) (setq *liste10* (cons y *liste10*)))
     ((and (= chan 11) (= port 0)) (setq *liste11* (cons y *liste11*)))
     ((and (= chan 12) (= port 0)) (setq *liste12* (cons y *liste12*)))
     ((and (= chan 13) (= port 0)) (setq *liste13* (cons y *liste13*)))
     ((and (= chan 14) (= port 0)) (setq *liste14* (cons y *liste14*)))  
     ((and (= chan 15) (= port 0)) (setq *liste15* (cons y *liste15*)))  
     ((and (= chan 0) (= port 1)) (setq *liste16* (cons y *liste16*))) 
     
     ((and (= chan 1) (= port 1)) (setq *liste17* (cons y *liste17*)))  
     ((and (= chan 2) (= port 1)) (setq *liste18* (cons y *liste18*))) 
     ((and (= chan 3) (= port 1)) (setq *liste19* (cons y *liste19*))) 
     ((and (= chan 4) (= port 1)) (setq *liste20* (cons y *liste20*)))   
     ((and (= chan 5) (= port 1)) (setq *liste21* (cons y *liste21*)))  
     ((and (= chan 6) (= port 1)) (setq *liste22* (cons y *liste22*)))     
     ((and (= chan 7) (= port 1)) (setq *liste23* (cons y *liste23*)))          
     ((and (= chan 8) (= port 1)) (setq *liste24* (cons y *liste24*)))
     ((and (= chan 9) (= port 1)) (setq *liste25* (cons y *liste25*)))
     ((and (= chan 10) (= port 1)) (setq *liste26* (cons y *liste26*)))
     ((and (= chan 11) (= port 1)) (setq *liste27* (cons y *liste27*)))
     ((and (= chan 12) (= port 1)) (setq *liste28* (cons y *liste28*)))  
     ((and (= chan 13) (= port 1)) (setq *liste29* (cons y *liste29*)))
     ((and (= chan 14) (= port 1)) (setq *liste30* (cons y *liste30*)))
     ((and (= chan 15) (= port 1)) (setq *liste31* (cons y *liste31*)))
     
     ((and (= chan 1) (= port 2)) (setq *liste32* (cons y *liste32*)))          
     ((and (= chan 2) (= port 2)) (setq *liste33* (cons y *liste33*)))
     ((and (= chan 3) (= port 2)) (setq *liste34* (cons y *liste34*)))
     ((and (= chan 4) (= port 2)) (setq *liste35* (cons y *liste35*)))
     ((and (= chan 5) (= port 2)) (setq *liste36* (cons y *liste36*))) 
     ((and (= chan 6) (= port 2)) (setq *liste37* (cons y *liste37*)))     
     ((and (= chan 7) (= port 2)) (setq *liste38* (cons y *liste38*)))          
     ((and (= chan 8) (= port 2)) (setq *liste39* (cons y *liste39*)))
     ((and (= chan 9) (= port 2)) (setq *liste40* (cons y *liste40*)))
     ((and (= chan 10) (= port 2)) (setq *liste41* (cons y *liste41*)))
     ((and (= chan 11) (= port 2)) (setq *liste42* (cons y *liste42*)))
     ((and (= chan 12) (= port 2)) (setq *liste43* (cons y *liste43*)))  
     ((and (= chan 13) (= port 2)) (setq *liste44* (cons y *liste44*)))
     ((and (= chan 14) (= port 2)) (setq *liste45* (cons y *liste45*)))
     ((and (= chan 15) (= port 2)) (setq *liste46* (cons y *liste46*)))
     ((and (= chan 0) (= port 2)) (setq *liste47* (cons y *liste47*)))

     (t (print "ERREUR DE CANAL") (print chan)(print port)  ))))

;;-------------------------------------------------------------------------------
;;               BASE DE DONNÉE RYTHMIQUE
;;-------------------------------------------------------------------------------


(defvar *base-donnees*)

(setq *base-donnees*
      '(
        (6 (3 3) (2 2 2) (3/2 3/2 3/2 3/2)
         ((* (FDUR 6 5 D-N) 1) (* (FDUR 6 5 D-N) 1) (* (FDUR 6 5 D-N) 1) (* (FDUR 6 5 D-N) 1) (* (FDUR 6 5 D-N) 1))
         (1 1 1 1 1 1))
        (4 (2 2) ((* (FDUR 2 3 D-B) 2) (* (FDUR 2 3 D-B) 2) (* (FDUR 2 3 D-B) 2)) (1 1 1 1)
         ((* (FDUR 4 5 D-N) 1) (* (FDUR 4 5 D-N) 1) (* (FDUR 4 5 D-N) 1) (* (FDUR 4 5 D-N) 1) (* (FDUR 4 5 D-N) 1))
         ((* (FDUR 4 6 D-N) 1) (* (FDUR 4 6 D-N) 1) (* (FDUR 4 6 D-N) 1) (* (FDUR 4 6 D-N) 1) (* (FDUR 4 6 D-N) 1) (* (FDUR 4 6 D-N) 1)))
        (3 (3/2 3/2) (1 1 1) (3/4 3/4 3/4 3/4) ((* (FDUR 3 5 D-N) 1) (* (FDUR 3 5 D-N) 1) (* (FDUR 3 5 D-N) 1) (* (FDUR 3 5 D-N) 1) (* (FDUR 3 5 D-N) 1))
         (1/2 1/2 1/2 1/2 1/2 1/2))
        (2 (1 1) ((* (FDUR 2 3 D-N) 1) (* (FDUR 2 3 D-N) 1) (* (FDUR 2 3 D-N) 1)) (1/2 1/2 1/2 1/2)
         ((* (FDUR 4 5 D-C) 1/2) (* (FDUR 4 5 D-C) 1/2) (* (FDUR 4 5 D-C) 1/2) (* (FDUR 4 5 D-C) 1/2) (* (FDUR 4 5 D-C) 1/2))
         ((* (FDUR 4 6 D-C) 1/2) (* (FDUR 4 6 D-C) 1/2) (* (FDUR 4 6 D-C) 1/2) (* (FDUR 4 6 D-C) 1/2) (* (FDUR 4 6 D-C) 1/2) (* (FDUR 4 6 D-C) 1/2)))
        (3/2 (3/4 3/4) (1/2 1/2 1/2) (3/8 3/8 3/8 3/8) ((* (FDUR 3 5 D-C) 1/2) (* (FDUR 3 5 D-C) 1/2) (* (FDUR 3 5 D-C) 1/2) (* (FDUR 3 5 D-C) 1/2) (* (FDUR 3 5 D-C) 1/2))
         (1/4 1/4 1/4 1/4 1/4 1/4))
        (1 (1/2 1/2) ((* (FDUR 2 3 D-C) 1/2) (* (FDUR 2 3 D-C) 1/2) (* (FDUR 2 3 D-C) 1/2)) (1/4 1/4 1/4 1/4)
         ((* (FDUR 4 5 D-DC) 1/4) (* (FDUR 4 5 D-DC) 1/4) (* (FDUR 4 5 D-DC) 1/4) (* (FDUR 4 5 D-DC) 1/4) (* (FDUR 4 5 D-DC) 1/4))
         ((* (FDUR 4 6 D-DC) 1/4) (* (FDUR 4 6 D-DC) 1/4) (* (FDUR 4 6 D-DC) 1/4) (* (FDUR 4 6 D-DC) 1/4) (* (FDUR 4 6 D-DC) 1/4) (* (FDUR 4 6 D-DC) 1/4)))
        (3/4 (3/8 3/8) (1/4 1/4 1/4) (3/16 3/16 3/16 3/16) ((* (FDUR 3 5 D-DC) 1/4) (* (FDUR 3 5 D-DC) 1/4) (* (FDUR 3 5 D-DC) 1/4) (* (FDUR 3 5 D-DC) 1/4) (* (FDUR 3 5 D-DC) 1/4))
         (1/8 1/8 1/8 1/8 1/8 1/8))
        (1/2 (1/4 1/4) ((* (FDUR 2 3 D-DC) 1/4) (* (FDUR 2 3 D-DC) 1/4) (* (FDUR 2 3 D-DC) 1/4)) (1/8 1/8 1/8 1/8)
         ((* (FDUR 4 5 D-TC) 1/8) (* (FDUR 4 5 D-TC) 1/8) (* (FDUR 4 5 D-TC) 1/8) (* (FDUR 4 5 D-TC) 1/8) (* (FDUR 4 5 D-TC) 1/8))
         ((* (FDUR 4 6 D-TC) 1/8) (* (FDUR 4 6 D-TC) 1/8) (* (FDUR 4 6 D-TC) 1/8) (* (FDUR 4 6 D-TC) 1/8) (* (FDUR 4 6 D-TC) 1/8) (* (FDUR 4 6 D-TC) 1/8)))
        (1/4 (1/8 1/8) ((* (FDUR 2 3 D-TC) 1/8) (* (FDUR 2 3 D-TC) 1/8) (* (FDUR 2 3 D-TC) 1/8)) (1/16 1/16 1/16 1/16)
         ((* (FDUR 4 5 D-QC) 1/16) (* (FDUR 4 5 D-QC) 1/16) (* (FDUR 4 5 D-QC) 1/16) (* (FDUR 4 5 D-QC) 1/16) (* (FDUR 4 5 D-QC) 1/16))
         ((* (FDUR 4 6 D-QC) 1/16) (* (FDUR 4 6 D-QC) 1/16) (* (FDUR 4 6 D-QC) 1/16) (* (FDUR 4 6 D-QC) 1/16) (* (FDUR 4 6 D-QC) 1/16) (* (FDUR 4 6 D-QC) 1/16)))
        (1/8 (1/16 1/16) ((* (FDUR 2 3 D-QC) 1/16) (* (FDUR 2 3 D-QC) 1/16) (* (FDUR 2 3 D-QC) 1/16)))
        
        ((* (FDUR 4 5 D-N) 1) ((* (FDUR 4 5 D-N) 1/2) (* (FDUR 4 5 D-N) 1/2)) ((* (FDUR 4 5 D-N) (FDUR 2 3 D-C) 1/2) (* (FDUR 4 5 D-N) (FDUR 2 3 D-C) 1/2)
                                                                               (* (FDUR 4 5 D-N) (FDUR 2 3 D-C) 1/2)) ((* (FDUR 4 5 D-N) 1/4) (* (FDUR 4 5 D-N) 1/4) (* (FDUR 4 5 D-N) 1/4) (* (FDUR 4 5 D-N) 1/4))
         ((* (FDUR 4 5 D-N) (FDUR 4 5 D-DC) 1/4) (* (FDUR 4 5 D-N) (FDUR 4 5 D-DC) 1/4) (* (FDUR 4 5 D-N) (FDUR 4 5 D-DC) 1/4) (* (FDUR 4 5 D-N) (FDUR 4 5 D-DC) 1/4)
          (* (FDUR 4 5 D-N) (FDUR 4 5 D-DC) 1/4)) ((* (FDUR 4 5 D-N) (FDUR 4 6 D-DC) 1/4) (* (FDUR 4 5 D-N) (FDUR 4 6 D-DC) 1/4) (* (FDUR 4 5 D-N) (FDUR 4 6 D-DC) 1/4)
                                                   (* (FDUR 4 5 D-N) (FDUR 4 6 D-DC) 1/4) (* (FDUR 4 5 D-N) (FDUR 4 6 D-DC) 1/4) (* (FDUR 4 5 D-N) (FDUR 4 6 D-DC) 1/4)))
        
        ((* (FDUR 4 6 D-N) 1) ((* (FDUR 4 6 D-N) 1/2) (* (FDUR 4 6 D-N) 1/2)) ((* (FDUR 4 6 D-N) (FDUR 2 3 D-C) 1/2) (* (FDUR 4 6 D-N) (FDUR 2 3 D-C) 1/2)
                                                                               (* (FDUR 4 6 D-N) (FDUR 2 3 D-C) 1/2)) ((* (FDUR 4 6 D-N) 1/4) (* (FDUR 4 6 D-N) 1/4) (* (FDUR 4 6 D-N) 1/4) (* (FDUR 4 6 D-N) 1/4))
         ((* (FDUR 4 6 D-N) (FDUR 4 5 D-DC) 1/4) (* (FDUR 4 6 D-N) (FDUR 4 5 D-DC) 1/4) (* (FDUR 4 6 D-N) (FDUR 4 5 D-DC) 1/4) (* (FDUR 4 6 D-N) (FDUR 4 5 D-DC) 1/4)
          (* (FDUR 4 6 D-N) (FDUR 4 5 D-DC) 1/4)) ((* (FDUR 4 6 D-N) (FDUR 4 6 D-DC) 1/4) (* (FDUR 4 6 D-N) (FDUR 4 6 D-DC) 1/4) (* (FDUR 4 6 D-N) (FDUR 4 6 D-DC) 1/4)
                                                   (* (FDUR 4 6 D-N) (FDUR 4 6 D-DC) 1/4) (* (FDUR 4 6 D-N) (FDUR 4 6 D-DC) 1/4) (* (FDUR 4 6 D-N) (FDUR 4 6 D-DC) 1/4)))
        
        ((* (FDUR 2 3 D-B) 2) ((* (FDUR 2 3 D-B) 1) (* (FDUR 2 3 D-B) 1))
         ((* (FDUR 2 3 D-B) (FDUR 2 3 D-N) 1) (* (FDUR 2 3 D-B) (FDUR 2 3 D-N) 1) (* (FDUR 2 3 D-B) (FDUR 2 3 D-N) 1))
         ((* (FDUR 2 3 D-B) 1/2) (* (FDUR 2 3 D-B) 1/2) (* (FDUR 2 3 D-B) 1/2) (* (FDUR 2 3 D-B) 1/2))
         ((* (FDUR 2 3 D-B) (FDUR 4 5 D-C) 1/2) (* (FDUR 2 3 D-B) (FDUR 4 5 D-C) 1/2) (* (FDUR 2 3 D-B) (FDUR 4 5 D-C) 1/2) (* (FDUR 2 3 D-B) (FDUR 4 5 D-C) 1/2)
          (* (FDUR 2 3 D-B) (FDUR 4 5 D-C) 1/2))
         ((* (FDUR 2 3 D-B) (FDUR 4 6 D-C) 1/2) (* (FDUR 2 3 D-B) (FDUR 4 6 D-C) 1/2) (* (FDUR 2 3 D-B) (FDUR 4 6 D-C) 1/2) (* (FDUR 2 3 D-B) (FDUR 4 6 D-C) 1/2)
          (* (FDUR 2 3 D-B) (FDUR 4 6 D-C) 1/2) (* (FDUR 2 3 D-B) (FDUR 4 6 D-C) 1/2)))
        ((* (FDUR 2 3 D-N) 1) ((* (FDUR 2 3 D-N) 1/2) (* (FDUR 2 3 D-N) 1/2))
         ((* (FDUR 2 3 D-N) (FDUR 2 3 D-C) 1/2) (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) 1/2) (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) 1/2))
         ((* (FDUR 2 3 D-N) 1/4) (* (FDUR 2 3 D-N) 1/4) (* (FDUR 2 3 D-N) 1/4) (* (FDUR 2 3 D-N) 1/4))
         ((* (FDUR 2 3 D-N) (FDUR 4 5 D-DC) 1/4) (* (FDUR 2 3 D-N) (FDUR 4 5 D-DC) 1/4) (* (FDUR 2 3 D-N) (FDUR 4 5 D-DC) 1/4)
          (* (FDUR 2 3 D-N) (FDUR 4 5 D-DC) 1/4) (* (FDUR 2 3 D-N) (FDUR 4 5 D-DC) 1/4))
         ((* (FDUR 2 3 D-N) (FDUR 4 6 D-DC) 1/4) (* (FDUR 2 3 D-N) (FDUR 4 6 D-DC) 1/4) (* (FDUR 2 3 D-N) (FDUR 4 6 D-DC) 1/4)
          (* (FDUR 2 3 D-N) (FDUR 4 6 D-DC) 1/4) (* (FDUR 2 3 D-N) (FDUR 4 6 D-DC) 1/4) (* (FDUR 2 3 D-N) (FDUR 4 6 D-DC) 1/4)))
        ((* (FDUR 2 3 D-C) 1/2) ((* (FDUR 2 3 D-C) 1/4) (* (FDUR 2 3 D-C) 1/4))
         ((* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) 1/4) (* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) 1/4) (* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) 1/4))
         ((* (FDUR 2 3 D-C) 1/8) (* (FDUR 2 3 D-C) 1/8) (* (FDUR 2 3 D-C) 1/8) (* (FDUR 2 3 D-C) 1/8))
         ((* (FDUR 2 3 D-C) (FDUR 4 5 D-TC) 1/8) (* (FDUR 2 3 D-C) (FDUR 4 5 D-TC) 1/8) (* (FDUR 2 3 D-C) (FDUR 4 5 D-TC) 1/8) 
          (* (FDUR 2 3 D-C) (FDUR 4 5 D-TC) 1/8) (* (FDUR 2 3 D-C) (FDUR 4 5 D-TC) 1/8))
         ((* (FDUR 2 3 D-C) (FDUR 4 6 D-TC) 1/8) (* (FDUR 2 3 D-C) (FDUR 4 6 D-TC) 1/8) (* (FDUR 2 3 D-C) (FDUR 4 6 D-TC) 1/8) 
          (* (FDUR 2 3 D-C) (FDUR 4 6 D-TC) 1/8) (* (FDUR 2 3 D-C) (FDUR 4 6 D-TC) 1/8) (* (FDUR 2 3 D-C) (FDUR 4 6 D-TC) 1/8)))
        
        
        ((* (FDUR 4 5 D-C) 1/2) ((* (FDUR 4 5 D-C) 1/4) (* (FDUR 4 5 D-C) 1/4))
         ((* (FDUR 4 5 D-C) (FDUR 2 3 D-DC) 1/4) (* (FDUR 4 5 D-C) (FDUR 2 3 D-DC) 1/4) (* (FDUR 4 5 D-C) (FDUR 2 3 D-DC) 1/4))
         ((* (FDUR 4 5 D-C) 1/8) (* (FDUR 4 5 D-C) 1/8) (* (FDUR 4 5 D-C) 1/8) (* (FDUR 4 5 D-C) 1/8))
         ((* (FDUR 4 5 D-C) (FDUR 4 5 D-TC) 1/8) (* (FDUR 4 5 D-C) (FDUR 4 5 D-TC) 1/8) (* (FDUR 4 5 D-C) (FDUR 4 5 D-TC) 1/8) 
          (* (FDUR 4 5 D-C) (FDUR 4 5 D-TC) 1/8) (* (FDUR 4 5 D-C) (FDUR 4 5 D-TC) 1/8))
         ((* (FDUR 4 5 D-C) (FDUR 4 6 D-TC) 1/8) (* (FDUR 4 5 D-C) (FDUR 4 6 D-TC) 1/8) (* (FDUR 4 5 D-C) (FDUR 4 6 D-TC) 1/8) 
          (* (FDUR 4 5 D-C) (FDUR 4 6 D-TC) 1/8) (* (FDUR 4 5 D-C) (FDUR 4 6 D-TC) 1/8) (* (FDUR 4 5 D-C) (FDUR 4 6 D-TC) 1/8)))
        
        ((* (FDUR 4 6 D-C) 1/2) ((* (FDUR 4 6 D-C) 1/4) (* (FDUR 4 6 D-C) 1/4))
         ((* (FDUR 4 6 D-C) (FDUR 2 3 D-DC) 1/4) (* (FDUR 4 6 D-C) (FDUR 2 3 D-DC) 1/4) (* (FDUR 4 6 D-C) (FDUR 2 3 D-DC) 1/4))
         ((* (FDUR 4 6 D-C) 1/8) (* (FDUR 4 6 D-C) 1/8) (* (FDUR 4 6 D-C) 1/8) (* (FDUR 4 6 D-C) 1/8))
         ((* (FDUR 4 6 D-C) (FDUR 4 5 D-TC) 1/8) (* (FDUR 4 6 D-C) (FDUR 4 5 D-TC) 1/8) (* (FDUR 4 6 D-C) (FDUR 4 5 D-TC) 1/8) 
          (* (FDUR 4 6 D-C) (FDUR 4 5 D-TC) 1/8) (* (FDUR 4 6 D-C) (FDUR 4 5 D-TC) 1/8))
         ((* (FDUR 4 6 D-C) (FDUR 4 6 D-TC) 1/8) (* (FDUR 4 6 D-C) (FDUR 4 6 D-TC) 1/8) (* (FDUR 4 6 D-C) (FDUR 4 6 D-TC) 1/8) 
          (* (FDUR 4 6 D-C) (FDUR 4 6 D-TC) 1/8) (* (FDUR 4 6 D-C) (FDUR 4 6 D-TC) 1/8) (* (FDUR 4 6 D-C) (FDUR 4 6 D-TC) 1/8)))
        
        ((* (FDUR 2 3 D-DC) 1/4) ((* (FDUR 2 3 D-DC) 1/8) (* (FDUR 2 3 D-DC) 1/8))
         ((* (FDUR 2 3 D-DC) (FDUR 2 3 D-TC) 1/8) (* (FDUR 2 3 D-DC) (FDUR 2 3 D-TC) 1/8) (* (FDUR 2 3 D-DC) (FDUR 2 3 D-TC) 1/8))
         ((* (FDUR 2 3 D-DC) 1/16) (* (FDUR 2 3 D-DC) 1/16) (* (FDUR 2 3 D-DC) 1/16) (* (FDUR 2 3 D-DC) 1/16))
         ((* (FDUR 2 3 D-DC) (FDUR 4 5 D-QC) 1/16) (* (FDUR 2 3 D-DC) (FDUR 4 5 D-QC) 1/16) (* (FDUR 2 3 D-DC) (FDUR 4 5 D-QC) 1/16)
          (* (FDUR 2 3 D-DC) (FDUR 4 5 D-QC) 1/16) (* (FDUR 2 3 D-DC) (FDUR 4 5 D-QC) 1/16))
         ((* (FDUR 2 3 D-DC) (FDUR 4 6 D-QC) 1/16) (* (FDUR 2 3 D-DC) (FDUR 4 6 D-QC) 1/16) (* (FDUR 2 3 D-DC) (FDUR 4 6 D-QC) 1/16) 
          (* (FDUR 2 3 D-DC) (FDUR 4 6 D-QC) 1/16) (* (FDUR 2 3 D-DC) (FDUR 4 6 D-QC) 1/16) (* (FDUR 2 3 D-DC) (FDUR 4 6 D-QC) 1/16)))
        
        
        
        ((* (FDUR 4 5 D-DC) 3/4) ((* (FDUR 4 5 D-DC) 3/8) (* (FDUR 4 5 D-DC) 3/8))
         ((* (FDUR 4 5 D-DC) 1/4) (* (FDUR 4 5 D-DC) 1/4) (* (FDUR 4 5 D-DC) 1/4))
         ((* (FDUR 4 5 D-DC) 3/16) (* (FDUR 4 5 D-DC) 3/16) (* (FDUR 4 5 D-DC) 3/16) (* (FDUR 4 5 D-DC) 3/16)))
        
        
        ((* (FDUR 4 5 D-DC) 1/2) ((* (FDUR 4 5 D-DC) 1/4) (* (FDUR 4 5 D-DC) 1/4))
         ((* (FDUR 4 5 D-DC) (FDUR 2 3 D-DC) 1/4) (* (FDUR 4 5 D-DC) (FDUR 2 3 D-DC) 1/4) (* (FDUR 4 5 D-DC) (FDUR 2 3 D-DC) 1/4))
         ((* (FDUR 4 5 D-DC) 1/8) (* (FDUR 4 5 D-DC) 1/8) (* (FDUR 4 5 D-DC) 1/8) (* (FDUR 4 5 D-DC) 1/8))
         ((* (FDUR 4 5 D-DC) (FDUR 4 5 D-TC) 1/8) (* (FDUR 4 5 D-DC) (FDUR 4 5 D-TC) 1/8) (* (FDUR 4 5 D-DC) (FDUR 4 5 D-TC) 1/8)
          (* (FDUR 4 5 D-DC) (FDUR 4 5 D-TC) 1/8) (* (FDUR 4 5 D-DC) (FDUR 4 5 D-TC) 1/8))
         ((* (FDUR 4 5 D-DC) (FDUR 4 6 D-TC) 1/8) (* (FDUR 4 5 D-DC) (FDUR 4 6 D-TC) 1/8) (* (FDUR 4 5 D-DC) (FDUR 4 6 D-TC) 1/8) 
          (* (FDUR 4 5 D-DC) (FDUR 4 6 D-TC) 1/8) (* (FDUR 4 5 D-DC) (FDUR 4 6 D-TC) 1/8) (* (FDUR 4 5 D-DC) (FDUR 4 6 D-TC) 1/8)))
        
        
        ((* (FDUR 4 5 D-DC) 1/4) ((* (FDUR 4 5 D-DC) 1/8) (* (FDUR 4 5 D-DC) 1/8))
         ((* (FDUR 4 5 D-DC) (FDUR 2 3 D-TC) 1/8) (* (FDUR 4 5 D-DC) (FDUR 2 3 D-TC) 1/8) (* (FDUR 4 5 D-DC) (FDUR 2 3 D-TC) 1/8))
         ((* (FDUR 4 5 D-DC) 1/16) (* (FDUR 4 5 D-DC) 1/16) (* (FDUR 4 5 D-DC) 1/16) (* (FDUR 4 5 D-DC) 1/16))
         ((* (FDUR 4 5 D-DC) (FDUR 4 5 D-QC) 1/16) (* (FDUR 4 5 D-DC) (FDUR 4 5 D-QC) 1/16) (* (FDUR 4 5 D-DC) (FDUR 4 5 D-QC) 1/16)
          (* (FDUR 4 5 D-DC) (FDUR 4 5 D-QC) 1/16) (* (FDUR 4 5 D-DC) (FDUR 4 5 D-QC) 1/16))
         ((* (FDUR 4 5 D-DC) (FDUR 4 6 D-QC) 1/16) (* (FDUR 4 5 D-DC) (FDUR 4 6 D-QC) 1/16) (* (FDUR 4 5 D-DC) (FDUR 4 6 D-QC) 1/16) 
          (* (FDUR 4 5 D-DC) (FDUR 4 6 D-QC) 1/16) (* (FDUR 4 5 D-DC) (FDUR 4 6 D-QC) 1/16) (* (FDUR 4 5 D-DC) (FDUR 4 6 D-QC) 1/16)))
        
        ;;**********************
        ((* (FDUR 4 6 D-DC) 1/4) ((* (FDUR 4 6 D-DC) 1/8) (* (FDUR 4 6 D-DC) 1/8))
         ((* (FDUR 4 6 D-DC) (FDUR 2 3 D-TC) 1/8) (* (FDUR 4 6 D-DC) (FDUR 2 3 D-TC) 1/8) (* (FDUR 4 6 D-DC) (FDUR 2 3 D-TC) 1/8))
         ((* (FDUR 4 6 D-DC) 1/16) (* (FDUR 4 6 D-DC) 1/16) (* (FDUR 4 6 D-DC) 1/16) (* (FDUR 4 6 D-DC) 1/16))
         ((* (FDUR 4 6 D-DC) (FDUR 4 5 D-TC) 1/16) (* (FDUR 4 6 D-DC) (FDUR 4 5 D-TC) 1/16) (* (FDUR 4 6 D-DC) (FDUR 4 5 D-TC) 1/16) 
          (* (FDUR 4 6 D-DC) (FDUR 4 5 D-TC) 1/16) (* (FDUR 4 6 D-DC) (FDUR 4 5 D-TC) 1/16))
          ((* (FDUR 4 6 D-DC) (FDUR 4 6 D-TC) 1/16) (* (FDUR 4 6 D-DC) (FDUR 4 6 D-TC) 1/16) (* (FDUR 4 6 D-DC) (FDUR 4 6 D-TC) 1/16) 
           (* (FDUR 4 6 D-DC) (FDUR 4 6 D-TC) 1/16) (* (FDUR 4 6 D-DC) (FDUR 4 6 D-TC) 1/16) (* (FDUR 4 6 D-DC) (FDUR 4 6 D-TC) 1/16)))
         
         
         ((* (FDUR 2 3 D-TC) 1/8) ((* (FDUR 2 3 D-TC) 1/16) (* (FDUR 2 3 D-TC) 1/16))
          ((* (FDUR 2 3 D-TC) (FDUR 2 3 D-QC) 1/16) (* (FDUR 2 3 D-TC) (FDUR 2 3 D-QC) 1/16) (* (FDUR 2 3 D-TC) (FDUR 2 3 D-QC) 1/16)))
         ((* (FDUR 2 3 D-B) 1)((* (FDUR 2 3 D-B) (FDUR 2 3 D-C) 1/2) (* (FDUR 2 3 D-B) (FDUR 2 3 D-C) 1/2) (* (FDUR 2 3 D-B) (FDUR 2 3 D-C) 1/2))
          ((* (FDUR 2 3 D-B) 1/4) (* (FDUR 2 3 D-B) 1/4) (* (FDUR 2 3 D-B) 1/4) (* (FDUR 2 3 D-B) 1/4))
          ((* (FDUR 2 3 D-B) (FDUR 4 5 D-DC) 1/4) (* (FDUR 2 3 D-B) (FDUR 4 5 D-DC) 1/4) (* (FDUR 2 3 D-B) (FDUR 4 5 D-DC) 1/4) 
           (* (FDUR 2 3 D-B) (FDUR 4 5 D-DC) 1/4) (* (FDUR 2 3 D-B) (FDUR 4 5 D-DC) 1/4))
          ((* (FDUR 2 3 D-B) (FDUR 4 6 D-DC) 1/4) (* (FDUR 2 3 D-B) (FDUR 4 6 D-DC) 1/4) (* (FDUR 2 3 D-B) (FDUR 4 6 D-DC) 1/4) 
           (* (FDUR 2 3 D-B) (FDUR 4 6 D-DC) 1/4) (* (FDUR 2 3 D-B) (FDUR 4 6 D-DC) 1/4) (* (FDUR 2 3 D-B) (FDUR 4 6 D-DC) 1/4)))
         ((* (FDUR 2 3 D-N) 1/2) ((* (FDUR 2 3 D-N) 1/4) (* (FDUR 2 3 D-N) 1/4))
          ((* (FDUR 2 3 D-N) (FDUR 2 3 D-DC) 1/4) (* (FDUR 2 3 D-N) (FDUR 2 3 D-DC) 1/4) (* (FDUR 2 3 D-N) (FDUR 2 3 D-DC) 1/4))
          ((* (FDUR 2 3 D-N) 1/16) (* (FDUR 2 3 D-N) 1/16) (* (FDUR 2 3 D-N) 1/16) (* (FDUR 2 3 D-N) 1/16))
          ((* (FDUR 2 3 D-N) (FDUR 4 5 D-TC) 1/16) (* (FDUR 2 3 D-N) (FDUR 4 5 D-TC) 1/16) (* (FDUR 2 3 D-N) (FDUR 4 5 D-TC) 1/16) 
           (* (FDUR 2 3 D-N) (FDUR 4 5 D-TC) 1/16) (* (FDUR 2 3 D-N) (FDUR 4 5 D-TC) 1/16))
          ((* (FDUR 2 3 D-N) (FDUR 4 6 D-TC) 1/16) (* (FDUR 2 3 D-N) (FDUR 4 6 D-TC) 1/16) (* (FDUR 2 3 D-N) (FDUR 4 6 D-TC) 1/16) (* (FDUR 2 3 D-N) (FDUR 4 6 D-TC) 1/16)
           (* (FDUR 2 3 D-N) (FDUR 4 6 D-TC) 1/16) (* (FDUR 2 3 D-N) (FDUR 4 6 D-TC) 1/16)))
         ((* (FDUR 2 3 D-C) 1/4) ((* (FDUR 2 3 D-C) 1/8) (* (FDUR 2 3 D-C) 1/8)) ((* (FDUR 2 3 D-C) (FDUR 2 3 D-TC) 1/8)
                                                                                  (* (FDUR 2 3 D-C) (FDUR 2 3 D-TC) 1/8) (* (FDUR 2 3 D-C) (FDUR 2 3 D-TC) 1/8))
          ((* (FDUR 2 3 D-C) 1/16) (* (FDUR 2 3 D-C) 1/16) (* (FDUR 2 3 D-C) 1/16) (* (FDUR 2 3 D-C) 1/16))
          ((* (FDUR 2 3 D-C) (FDUR 4 5 D-QC) 1/16) (* (FDUR 2 3 D-C) (FDUR 4 5 D-QC) 1/16) (* (FDUR 2 3 D-C) (FDUR 4 5 D-QC) 1/16) 
           (* (FDUR 2 3 D-C) (FDUR 4 5 D-QC) 1/16) (* (FDUR 2 3 D-C) (FDUR 4 5 D-QC) 1/16))
          ((* (FDUR 2 3 D-C) (FDUR 4 6 D-QC) 1/16) (* (FDUR 2 3 D-C) (FDUR 4 6 D-QC) 1/16) (* (FDUR 2 3 D-C) (FDUR 4 6 D-QC) 1/16) 
           (* (FDUR 2 3 D-C) (FDUR 4 6 D-QC) 1/16) (* (FDUR 2 3 D-C) (FDUR 4 6 D-QC) 1/16) (* (FDUR 2 3 D-C) (FDUR 4 6 D-QC) 1/16)))
         ((* (FDUR 2 3 D-DC) 1/8) ((* (FDUR 2 3 D-DC) 1/16) (* (FDUR 2 3 D-DC) 1/16)) ((* (FDUR 2 3 D-DC) (FDUR 2 3 D-QC) 1/16) 
                                                                                       (* (FDUR 2 3 D-DC) (FDUR 2 3 D-QC) 1/16) (* (FDUR 2 3 D-DC) (FDUR 2 3 D-QC) 1/16)))
         ))
         
;; (assoc 2 *base-donnees*)       


;;-------------------------------------------------------------------------------
;;               Fonctions basiques
;;-------------------------------------------------------------------------------

(defun changder (l)
  (cond ((numberp l) (- 0 (abs l)))
        ((eq (first l) '+) `(,(car l) ,(changder (second l)) ,(changder (third l))))
        ((and (= 3 (length l)) (eq (first l) '*)) `(,(car l) ,(second l) , (- 0 (abs (car(last l))))))
        ((and (= 4 (length l)) (eq (first l) '*)) `(,(car l) ,(second l) , (third l) ,(- 0 (abs (car(last l))))))
        (t (error "changder"))))



(defun binaire? (x) 
  (if (numberp x) t nil))


(defun deliaison (l)
  (if (and (consp l) (eq (first l) '+))
    (append (deliaison (second l)) (deliaison (third l)))
    (list l)))



(defun transforythm (l v)
  (transfo (deliaison l) v))


(defun transfo (l v)
(if l
  (append (trans (car l) v) (transfo (cdr l) v))))

(defun trans (x v)
  (if (binaire? x) (tranb x v) (trant x v)))

(defun tranb ( x v)
  (if (> (eval x) 0)
    (let* ((nbr (floor (/ x v)))
           (reste (- x (* v nbr)))
           (l nil))
      (dotimes (i nbr)
        (setq l (cons v l)))
      (if (> reste 0) (setq l (cons reste l)))
      (reverse l))
    (list x)))

(defun trant (x v)
  (if (> (eval x) 0)
    (let* ((nbr (floor (/ (car (last x)) v)))
           (reste (- (car (last x)) (* v nbr)))
           (l nil))
      (dotimes (i nbr)
        (setq l (cons (append  (butlast x) (list v))l)))
      (if (> reste 0) (setq l (cons (append  (butlast x) (list reste)) l)))
      l)
    (list x)))




;;-------------------------------------------------------------------------------
;;               Traitement des trilles
;;-------------------------------------------------------------------------------


(defun notes-accord (chan pgchange stac dur haut ltransp vel laccord lecar lapog arpegecar)  
  ;(print (list "notes-accord laccord" laccord))
  (if (= arpegecar 0) (notes-accord-ord chan pgchange stac dur haut ltransp vel laccord lecar lapog arpegecar)
      (notes-accord-arp chan pgchange stac dur haut ltransp vel laccord lecar lapog arpegecar)))


(defun notes-accord-ord (chan pgchange stac dur haut ltransp vel laccord lecar lapog arpegecar) 
  ;(print (list "note-accord-ord laccord" laccord))
  (when laccord
    (if (>= (+ chan (car laccord)) 0)
      (notes (nchan (+ chan (car laccord))) dur (+ haut (car lapog) (car lecar)) ltransp vel pgchange (nport (+ chan (car laccord))) stac))
    (notes-accord-ord chan pgchange stac dur haut ltransp vel (cdr laccord) (cdr lecar) lapog arpegecar)))



(defun notes-accord-arpe (d chan pgchange stac dur haut ltransp vel laccord lecar lapog arpegecar i) 
  ;(print (list "notes-accord-arpe lapog date arpegecar" lapog (date?) arpegecar))
  (when laccord
    (if (>= (+ chan (car laccord)) 0)
      (progn (p-abs (+ d (* i arpegecar))) 
             (notes (nchan (+ chan (car laccord))) dur (+ haut (car lapog) (car lecar)) ltransp vel pgchange (nport (+ chan (car laccord))) stac)))
    (notes-accord-arpe (+ d arpegecar ) chan pgchange stac (- dur arpegecar) haut ltransp vel (cdr laccord) (cdr lecar) lapog arpegecar (1+ i))))


(defun notes-accord-arp (chan pgchange stac dur haut ltransp vel laccord lecar lapog arpegecar) 
  ;(print "notes-accord-arp")
  (let ((larp (* (* noi (eval arpegecar)) (1- (length laccord)))))
    (if (and (>= larp dur) (not (= dur 629))) (setq arpegecar (floor (/ dur (length laccord))))  ;;on perd le signe de arpegecar!!
        (setq arpegecar (* noi (eval arpegecar)))))
  (let ((d (date?)))
    (cond ((> arpegecar 0) (notes-accord-arpe d chan pgchange stac dur haut ltransp vel laccord lecar lapog arpegecar 0))
          ((< arpegecar 0) (notes-accord-arpe d chan pgchange stac dur haut ltransp vel laccord (reverse lecar) lapog (abs arpegecar) 0))
          (t (print "ERREUR DANS NOTES-ACCORD-ARP")))
    (p-abs d)))






(defun note-non-trille (chan pgchange dur haut ltransp ltextexpres vel stac laccord lecar lapog arpegecar layer ldurgliss lhautgliss) 
  ;(print (list "note-non-trille dur" dur)) 
  (if (> (eval dur) 0)
    (if (and lapog (> (floor (* noi (eval dur) stac)) (* 630 (length lapog))))
      (apogiatu1 chan pgchange dur haut ltransp ltextexpres vel stac laccord lecar lapog arpegecar layer) 
      
      (notes-accord chan pgchange stac (floor (* noi (special-evaluate dur))) haut ltransp vel laccord lecar '(0) arpegecar))) ; on ne multiplie pas par stac (fait dans notes)
  (let ((pgrace '(255)))   ; le 27 08 01
    ;;    (if (not(= 1 stac)) (setq pgrace '(254)))   ; le 27 08 01
    (if (< stac 1) (setq pgrace '(254)))   ; le 27 08 01 et remodif le 14 Mars 2005
    (if lhautgliss (setq pgrace (list 253 lhautgliss ldurgliss)))   ; le 28 08 01
    (noteliste chan pgchange laccord (/ (date?) noi) dur (transpoliste ltransp haut lecar) ltextexpres pgrace vel layer))
  (p-rel (floor (* noi (abs (special-evaluate dur))))))



(defun aproxrythm (x) ;; ********* force x à prendre les valeurs 1/16 1/8 ....1/2 1 2 4
  (cond ((< x 1/16) 1/16)
        ((> x 4) 4)
        (t(nth (+ 4 (round (log x 2))) '(1/16 1/8 1/4 1/2 1 2 4)))))


(defun note-trille (chan pgchange dur haut ltransp ltextexpres vel stac tril ecar laccord lecar lapog arpegecar layer ldurgliss lhautgliss) 
  (if lapog
    (note-tril-apog chan pgchange dur haut ltransp ltextexpres vel stac tril ecar laccord lecar lapog arpegecar layer ldurgliss lhautgliss) 
    (note-tril-sans-apog chan pgchange (transforythm dur (eval (aproxrythm tril))) ecar haut ltransp ltextexpres vel stac laccord lecar arpegecar layer ldurgliss lhautgliss)))


(defun note-tril-sans-apog (chan pgchange ldur ecar haut ltransp ltextexpres vel stac laccord lecar arpegecar layer ldurgliss lhautgliss) 
       ;(print (list "note-tril-sans-apog" ldur (floor (* (length ldur) stac))))
       (let ((dat (date?)))
         ;(note-tr-midi (* (length ldur) stac) stac chan ldur haut vel laccord lecar ecar 0 arpegecar)
         (note-tr-midi (length ldur) stac chan pgchange ldur haut ltransp vel laccord lecar ecar 0 arpegecar layer)
         (p-abs dat))
       (note-tr-finale  chan pgchange ldur haut ltransp ltextexpres vel laccord lecar ecar 0 stac layer ldurgliss lhautgliss))


(defun note-tril-apog (chan pgchange dur haut ltransp ltextexpres vel stac tril ecar laccord lecar lapog arpegecar layer ldurgliss lhautgliss)
  
  (if (> (eval dur) 0) 
    
    (let* ((dat (date?))
           (durapo (* (length lapog) 1/8))
           (durtr (- (eval dur) durapo))
           (ldutr (transforythm durtr (eval (aproxrythm tril))))
           (ldur (transforythm dur (eval (aproxrythm tril))))
           (f (* 630 (length lapog))))
      
      (if (and lapog (> (floor (* noi (eval dur) stac)) (* 630 (length lapog))))
        
        (progn  
          (apogiatu1 chan pgchange dur haut ltransp ltextexpres vel stac laccord lecar lapog arpegecar layer)
          (p-abs (+ f dat))
          ;(note-tr-midi (floor (* (length ldutr) stac)) stac chan ldutr haut ltransp vel laccord lecar ecar 0 arpegecar)  ;; attention ERREUR?
          (note-tr-midi (floor (* (length ldutr))) stac chan pgchange ldutr haut ltransp vel laccord lecar ecar 0 arpegecar layer)
          (p-abs dat)
          (note-tr-finale chan pgchange ldur haut ltransp ltextexpres vel laccord lecar ecar 0 stac layer ldurgliss lhautgliss))
        
        (progn 
               (note-tr-midi (floor (* (length ldutr))) stac chan pgchange ldutr haut ltransp vel laccord lecar ecar 0 arpegecar layer)
               (p-abs dat)
               (note-tr-finale  chan pgchange ldur haut ltransp ltextexpres vel laccord lecar ecar 0 stac layer ldurgliss lhautgliss))))
    (let ((pgrace '(255)))   ; le 27 08 01
;;      (if (not(= 1 stac)) (setq pgrace '(254)))   ; le 27 08 01
        (if (< stac 1) (setq pgrace '(254)))   ; le 27 08 01 et remodif le 14 Mars 2005
      (if lhautgliss (setq pgrace (list 253 lhautgliss ldurgliss)))   ; le 28 08 01
      (progn (noteliste chan pgchange laccord (/ (date?) noi) dur (transpoliste ltransp haut lecar) ltextexpres layer pgrace vel) 
             (p-rel (floor (* noi (abs (eval dur)))))))))





(defun note-tr-midi (n stac chan pgchange ldur haut ltransp vel laccord lecar ecar alt arpegecar layer) 
   ;(print (list "note-tr-midi" ldur (date?)))
  (when (> n 0)
    (if (> (floor (* noi (eval (car ldur)))) 30000) (format t "note-tr-midi: chan dur lapog ~S ~S ~S ~%" chan ldur ecar))
    (cond                                                                                                                   ; 30 08 01
     ((> (eval (car ldur)) 0) 
      (notes-accord chan pgchange stac (floor (* noi (eval (car ldur)))) (+ haut (* alt ecar)) ltransp vel laccord lecar '(0) arpegecar)
      (p-rel (floor (* noi (abs (eval (car ldur))))))
      (note-tr-midi (1- n) stac chan pgchange (cdr ldur) haut ltransp vel laccord lecar ecar (- 1 alt) arpegecar layer))
     ((< (eval (car ldur)) 0) 
      (silenceliste chan (remove-duplicates laccord) (/ (date?) noi) (eval (car ldur)) layer)
      (p-rel (floor (* noi (abs (eval (car ldur))))))
      (note-tr-midi (1- n) stac chan pgchange (cdr ldur) haut ltransp vel laccord lecar ecar alt arpegecar layer))
     (t (note-tr-midi (1- n) stac chan pgchange (cdr ldur) haut ltransp vel laccord lecar ecar alt arpegecar layer)))
    ))






(defun note-tr-finale ( chan pgchange ldur haut ltransp ltextexpres vel laccord lecar ecar alt stac layer ldurgliss lhautgliss)
  (when ldur 
    (let ((pgrace '(255)))   ; le 27 08 01
      ;;      (if (not(= 1 stac)) (setq pgrace '(254)))   ; le 27 08 01
      (if (< stac 1) (setq pgrace '(254)))   ; le 27 08 01 et remodif le 14 Mars 2005
      (if lhautgliss (setq pgrace (list 253 lhautgliss ldurgliss)))   ; le 28 08 01
      
      (cond
       ((> (eval (car ldur)) 0)
        (noteliste chan pgchange laccord (/ (date?) noi) (car ldur) (transpoliste ltransp (+ haut (* alt ecar)) lecar) ltextexpres pgrace vel layer)
        (p-rel (floor (* noi (abs (eval (car ldur))))))
        (note-tr-finale chan pgchange (cdr ldur) haut ltransp ltextexpres vel laccord lecar ecar (- 1 alt) stac layer ldurgliss lhautgliss))
       ((< (eval (car ldur)) 0)
        (noteliste chan pgchange laccord (/ (date?) noi) (car ldur) (transpoliste ltransp (+ haut (* alt ecar)) lecar) ltextexpres pgrace vel layer)
        (p-rel (floor (* noi (abs (eval (car ldur))))))
        (note-tr-finale chan pgchange (cdr ldur) haut ltransp ltextexpres vel laccord lecar ecar alt stac layer ldurgliss lhautgliss))
       (t (note-tr-finale chan pgchange (cdr ldur) haut ltransp ltextexpres vel laccord lecar ecar alt stac layer ldurgliss lhautgliss))))))
         
   




;;-------------------------------------------------------------------------------
;;               Traitement des apogiatures
;;-------------------------------------------------------------------------------

;; 630 = durée de la triple croche = durée des notes d'apogiature

;;--------------------------
;; apogiature sans trille
;;--------------------------


(defun apogiatu1 (chan pgchange dur haut ltransp ltextexpres vel stac laccord lecar lapog arpegecar layer) 
  ;(print (list "APOGIATURE lapog arpegecar" lapog arpegecar))
  (let ((dat (date?)))
    (midi-apo dat chan pgchange dur haut ltransp vel stac laccord lecar lapog (length lapog) arpegecar 0)
    (p-abs dat)
    (grace-enigma chan pgchange haut ltransp ltextexpres lapog vel laccord lecar layer (length lapog))
    (p-abs dat)))

         
       
;; création de midi-apopo le 28 05 2001 ***********************

(defun midi-apopo (dat chan pgchange dur haut ltransp vel stac laccord lecar lapog n arpegecar)
  
  ;(print (list "midi-apopo dat dur" dat dur))
  (cond (lapog  
         (notes-accord chan pgchange stac 629 haut ltransp vel laccord lecar lapog arpegecar)
         (p-rel 630)
         (midi-apopo dat chan pgchange dur haut ltransp vel stac laccord lecar (cdr lapog) n arpegecar))
        (t  (if (> (- (floor (* noi (eval dur) stac)) (* 630 n) 2) 30000) (format t "midi-apo grand: chan dur lapog ~S ~S ~S ~%" chan dur lapog))
            (if (< (- (floor (* noi (eval dur) stac)) (* 630 n) 2) 0) (format t "midi-apo négatif: chan dur lapog n ~S ~S ~S ~S ~%" chan dur lapog n))
            ;(notes-accord chan stac (- (floor (* noi (eval dur) stac)) (* 630 n) 2) haut ltransp vel laccord lecar '(0) arpegecar)
            (notes-accord chan pgchange stac (- (floor (* noi (eval dur))) (* 630 n) 2) haut ltransp vel laccord lecar '(0) arpegecar))))

;; transformation de midi-apo le 28 05 2001 *********************** rajout de eval 01 2002 (pour le cas des n-olets!!

(defun midi-apo (dat chan pgchange dur haut ltransp vel stac laccord lecar lapog n arpegecar i)
  (p-abs (+ dat (* noi i (eval arpegecar))))
  (cond ((and lapog (> (length laccord) 1))
         ;(print (list "midi-apo i dat" i (date?)))
         (midi-apopo (p-abs (+ dat (* noi i (eval arpegecar)))) (+ chan (car laccord)) pgchange dur (+ haut (car lecar)) ltransp vel stac '(0) '(0) lapog n arpegecar)
         (midi-apo dat chan pgchange (- (eval dur) arpegecar) haut ltransp vel stac (cdr laccord) (cdr lecar) lapog n arpegecar (1+ i)))
        (t (midi-apopo dat chan pgchange dur haut ltransp vel stac laccord lecar lapog n arpegecar))))



       
;; Création d'un cinquième champs Enigma:
;; (0): première et dernière grace note (grace note unique)     
;; (1): première grace note
;; (2): seconde grace note
;; etc .... 
;; (99): dernière grace note
;; (253 puis arguments): marque de glissando  ; ajout du 27 04 01
;; (254): marque de staccato
;; (255): note normale (non grace note)


(defun grace-enigma (chan pgchange haut ltransp ltextexpres lapog vel laccord lecar layer &optional (p 0))
  (when lapog
    (let ((grace (cond ((= 1 p) '(0))
                       ((not (cdr lapog)) '(99))
                       (t (list (1+ (- p (length lapog))))))))
      (noteliste chan pgchange laccord (/ (date?) noi) 1/2 (transpoliste ltransp (+ haut (car lapog)) lecar) ltextexpres grace vel layer))
      (grace-enigma chan pgchange haut ltransp ltextexpres (cdr lapog) vel laccord lecar layer p)))


    


    

;;-------------------------------------------------------------------------------
;;               Traitement de la note
;;-------------------------------------------------------------------------------


(defun transsup (binf h)
  (if (< binf h) (print "ERREUR DANS TRANSSUP")
  (let (( dif (- binf h)))
  (if (integerp (/ dif 12)) binf (+ (* 12 (1+ (floor (/ dif 12)))) h)))))

(defun transinf (bsup h)
  (if (< h bsup) (print "ERREUR DANS TRANSINF")
  (let (( dif (- h bsup)))
  (if (integerp (/ dif 12)) bsup (- h (* 12 (1+ (floor (/ dif 12)))))))))

(defun tran (transp h)
  (if transp
  (let ((min (car transp))
         (max (second transp)))
    (cond ((and (<= min h) (>= max h)) h)
          ((< h min) (if (<= (transsup min h) max) (transsup min h) (progn (print "HAUTEUR NON TRANSPOSABLE") h)))
          ((> h max) (if (>= (transinf max h) min) (transinf max h) (progn (print "HAUTEUR NON TRANSPOSABLE") h)))
          (t (print "ERREUR DANS TRAN"))))  h))


;; transp est de type (min max) ou val  ou (val) (ajout du 08 03 05

(defun transpoliste (transp h lecar)  ;; 19 10 01
  ;(print (list "transpoliste" transp h lecar))
  (if (listp transp)
    (if (= 1 (length transp))
      (progn
        ;(print (list "ici" transp h))
        (mapcar #'(lambda (x) (+ x h)) lecar))
      (mapcar #'(lambda (x) (tran transp (+ x h))) lecar))
    (mapcar #'(lambda (x) (+ transp (+ x h))) lecar)))








;;==============================================================================
;;                     NOTE-SILENCE A HAUTEUR NON ENTIERE
;;
;;               génére un fichier MIDI ainsi qu'un
;;              fichiers ENIGMA à destination de FINALE
;;      si les durées sont exprimées en nombre (fractionnaire) de noires
;;==============================================================================

(defun note-silence ( chan pgchange dur haut vel sil stac tril ecar laccord lecar lapog ltransp ltextexpres arpegecar layer ldurgliss lhautgliss) 
  ;(print (list "note-silence" chan dur sil))
       (if (> tril 0)
         (note-trille chan pgchange dur haut ltransp ltextexpres vel stac tril ecar laccord lecar lapog arpegecar layer ldurgliss lhautgliss)
         (note-non-trille chan pgchange dur haut ltransp ltextexpres vel stac laccord lecar lapog arpegecar layer ldurgliss lhautgliss))
       (when (not (= 0 sil)) 
         (silenceliste chan (remove-duplicates laccord) (/ (date?) noi) (- 0 (abs sil)) layer) 
         (p-rel (floor (* noi (abs (eval sil)))))))





;;-------------------------------------------------------------------------------
;;               Traitement des motifs
;;-------------------------------------------------------------------------------


;; TRANSFODUR:
;; Transformation d'un rythme:
;; Si pas de liaison: 1 retourne (1/4 1/4 1/4 1/4) en fonction de (length lmotifhaut)
;; Si liaison: (1 + 2) retourne (1/4 1/4 1/4 1/4 + 2) en fonction de (length lmotifhaut)

(defun transfodur (dur lmotifhaut)
  (if (and (listp dur) (equal '+ (first dur)))
    (transfo-liaison dur lmotifhaut)
    (transfo-non-liaison dur lmotifhaut)))

(defun transfo-liaison (dur lmotifhaut) 
  (let* ((ddur (assoc (second dur) *base-donnees* :test #'equal))
         (n (length lmotifhaut))
         (dddur (nth (1- n) ddur)))
    (cond ((< (eval dur) 0) (list dur))
          ((equal (second dur) dddur) (list dur))
          ((not ddur)
           (if (not (equal lmotifhaut '(0))) (format t "Liaison: Durée non convertible en motif:~S ~%" dur))
           (list dur))
          ((and (listp (second dur)) (= (length (second dur)) 4))
           (if (not (equal lmotifhaut '(0))) (format t "Liaison: Durée non convertible en motif:~S ~%" dur))
           (list dur))
          ((equal dddur nil) 
           (format t "Liste de motif trop longue ~%") (list dur))
          (t (append (butlast dddur) (list (list '+ (car(last dddur)) (car(cddr dur)))))))))

(defun transfo-non-liaison (dur lmotifhaut)
  (let* ((ddur (assoc dur *base-donnees* :test #'equal))
         (n (length lmotifhaut))
         (dddur (nth (1- n) ddur)))
    (cond ((< (eval dur) 0) (list dur))
          ((not ddur)
           (if (not (equal lmotifhaut '(0))) (format t "Non-liaison: Durée non convertible en motif:~S ~%" dur))
           (list dur))
          ((equal dur dddur) (list dur))
          ((and (listp dur) (= (length dur) 4))
           (if (not (equal lmotifhaut '(0))) (format t "Non-liaison: Durée non convertible en motif:~S ~%" dur))
           (list dur))
          ((equal dddur nil) 
           (format t "Liste de motif trop longue ~%") (list dur))
          (t dddur))))



(defun note-motif (chan pgchange dur haut vel sil stac tril ecar laccord lecar lmotifhaut lapog ltransp ltextexpres arpegecar layer ldurgliss lhautgliss)
  ;(print (list "NOTE-MOTIF" " sil " sil " dur " dur))
  (if (< (eval dur) 0)
    (progn (silenceliste chan (remove-duplicates laccord) (/ (date?) noi) dur layer)
           (p-rel (floor (* noi (abs (eval dur))))))
    (note-mot chan pgchange (transfodur dur lmotifhaut) haut vel stac tril ecar laccord lecar lmotifhaut lapog ltransp ltextexpres arpegecar layer ldurgliss lhautgliss))
  (when (not (= 0 sil)) ;; ici le key-paramètre sil n'est pas nul
    (silenceliste chan (remove-duplicates laccord) (/ (date?) noi) (- 0 (abs sil)) layer)   
    (p-rel (floor (* noi (abs (eval sil)))))))

;;------------------------------------------------------------- 02 03 05
;; Pour la mise en place de :sil hors n-olet dans note-motif
;; stocke la durée réelle cumulée des éléments d'un n-olet au cours de sa mise en place

;;(defvar *totrythm*) Placé dans init en haut de page
;;(setq *totrythm* 0) Initialisé dans init
;;------------------------------------------------------------- 


;;------------------------------------------------------------- 02 03 05
;; dénominateur pair?

(defun denpair (n)
  (if (= 1 (denominator n)) t
      (if (evenp (denominator n)) t nil)))

;; (denpair 3) --> T
;; (denpair 1/6) --> T
;; (denpair 1/3) --> NIL
;;-------------------------------------------------------------


;;-------------------------------------------------------------
;; stocke la durée réelle d'un n-olet

(defvar *durrythm*)
(setq *durrythm* 0)
;;-------------------------------------------------------------

;;------------------------------------------------------------- 02 03 05
;; retourne la durée réelle d'un n-olet à partir de l'information
;; donnée par une expression du type '(FDUR 2 3 D-N)

(defun dur-rythme (r) 
  ;(print (list "dur-rythme" r))
  (let ((rr (second r)))
    (* (second rr) (eval(car(last rr))))))

;; (dur-rythme '(* (FDUR 4 6 D-C) 1/2)) --> 2 (sextolet de croches)
;; (dur-rythme '(* (FDUR 2 3 D-N) 1/2)) --> 2 (triolet de noire)
;;-------------------------------------------------------------

;;--------------------------------------------------------------------------------------------
;;           NOTE-MOTIF
;; Modif du 02 03 05: On peut utiliser :sil avec des groupes rythmiques comportant des n-olets
;; Le silence n'est introduit qu'en dehors des n-olets
;;--------------------------------------------------------------------------------------------

(defun note-motif (chan pgchange dur haut vel sil stac tril ecar laccord lecar lmotifhaut lapog ltransp ltextexpres arpegecar layer ldurgliss lhautgliss)
  ;(print (list "note-motif" chan dur))

  (if (listp dur)
    (if (and (and (= 0 *totrythm*) (est-nolet? dur)))
    (setq *durrythm* (dur-rythme dur))))

  (setq *totrythm* (+ *totrythm* (special-evaluate dur)))
  (if (< (eval dur) 0)
    (progn (silenceliste chan (remove-duplicates laccord) (/ (date?) noi) dur layer)
           (p-rel (floor (* noi (abs (eval dur))))))
    (note-mot chan pgchange (transfodur dur lmotifhaut) haut vel stac tril ecar laccord lecar lmotifhaut lapog ltransp ltextexpres arpegecar layer ldurgliss lhautgliss))
  (when (and (not (= 0 sil)) (denpair *totrythm*) (>= *totrythm* *durrythm*)) ;; ici le key-paramètre sil n'est pas nul
    (setq *totrythm* 0)
    (setq *durrythm* 0)
    (silenceliste chan (remove-duplicates laccord) (/ (date?) noi) (- 0 (abs sil)) layer)   
    (p-rel (floor (* noi (abs (eval sil)))))))
           


(defun note-mot ( chan pgchange ldur haut vel stac tril ecar laccord lecar lmotifhaut lapog ltransp ltextexpres arpegecar layer ldurgliss lhautgliss)
  (note-silence chan pgchange (car ldur) (+ haut (car lmotifhaut)) vel 0 stac tril ecar laccord lecar lapog ltransp ltextexpres arpegecar layer ldurgliss lhautgliss)
  (note-mo chan pgchange (cdr ldur) haut vel stac tril ecar laccord lecar (cdr lmotifhaut) ltransp ltextexpres arpegecar layer ldurgliss lhautgliss))

(defun note-mo ( chan pgchange ldur haut vel stac tril ecar laccord lecar lmotifhaut ltransp ltextexpres arpegecar layer ldurgliss lhautgliss)
  ;(print (list "note-mo" ldur))
  (when (and ldur lmotifhaut) 
    (note-silence chan pgchange (car ldur) (+ haut (car lmotifhaut)) vel 0 stac tril ecar laccord lecar nil ltransp ltextexpres arpegecar layer ldurgliss lhautgliss)
    (note-mo  chan pgchange (cdr ldur) haut vel stac tril ecar laccord lecar (cdr lmotifhaut) ltransp ltextexpres arpegecar layer ldurgliss lhautgliss)))





;;-------------------------------------------------------------------------------
;;               Traitement des accords
;;-------------------------------------------------------------------------------

(defun nchan (chan)
  (mod chan 16))

(defun nport (chan)
  (floor (/ chan 16)))



;--------- traitement ENIGMA des canaux non actifs dans l'accord -------------

(defun fait-faux-accord ( pgchange dat dur sil lfauxaccord arpegecar layer ldurgliss lhautgliss) 
  ;(print (list "fait-faux-accord" sil lfauxaccord))
  (when lfauxaccord 
    (p-abs dat)
    (if (>= (car lfauxaccord) 0)
      (note-motif 0 pgchange (changder dur) 10 0 sil 1 0 1 lfauxaccord '(0) '(0) nil nil nil arpegecar layer ldurgliss lhautgliss)) 
    (fait-faux-accord pgchange dat dur sil (cdr lfauxaccord) arpegecar layer ldurgliss lhautgliss)))


;--------- retourne les n premiers éléments de la liste l -------------

(defun npremier (n l)
  (if (= n 1) (list (car l))
      (append (list (car l)) (npremier (1- n) (cdr l)))))

;--------- retourne la liste l2 de longueur (min long-l1 long-l2) ------------

(defun compare-liste (l1 l2)
  (let* ((nl1 (length l1))
           (nl2 (length l2))
           (n (min nl1 nl2)))
      (npremier n l2)))

(defun temp2 (d)
  (1+ (mod (/ d noi) 4)))

(defun mesure2 (d)
  (1+ (floor (/ d noi) 4)))

(defun fait-liste0 (l)
  (if l (cons 0 (fait-liste0 (cdr l)))
      nil))

        
(defun fait-accord ( dat chan pgchange dur haut vel sil stac tril ecar laccord lecar lmaxaccord lmotifhaut lapog ltransp ltextexpres larpege arpegecar layer ldurgliss lhautgliss)
  ;(print (list "FAIT-ACCORD" chan ltextexpres laccord lecar))
  (cond ((and (not (equal laccord '(0))) larpege) (print "ERREUR DANS FAIT-ACCORD: Il n'est pas possible d'arpeger des accords") (abort))
        (larpege (note-motif chan pgchange dur haut vel sil stac tril ecar (fait-liste0 larpege) larpege lmotifhaut lapog ltransp ltextexpres arpegecar layer ldurgliss lhautgliss))
        (t (setq lecar (compare-liste laccord lecar)) 
           (setq laccord (compare-liste lecar laccord))
           (note-motif chan pgchange dur haut vel sil stac tril ecar laccord lecar lmotifhaut lapog ltransp ltextexpres arpegecar layer ldurgliss lhautgliss)
           (fait-faux-accord pgchange dat dur sil (set-difference lmaxaccord (mapcar #'(lambda (x) (+ x chan)) laccord)) arpegecar layer ldurgliss lhautgliss))))

;;-------------------------------------------------------------------------------
;;                Traitement des glissandi
;;-------------------------------------------------------------------------------


(defun somlist (l)
  (let ((s 0))
    (dotimes (ii (length l) )
      (progn (if (numberp (car l)) (setq s (+ s (car l))))
             (setq l (cdr l))))
    s))



;;---------------------------------------
;; rampe de pitch durée, ambitus, offset
;;---------------------------------------

(defun ram-pitch (chan port dur amb off dat) ;(print "ram-pitch")
  (let ((v1(round(* *bend-demi-ton* (+ off amb))))
        (v2 (round (* *bend-demi-ton* (+ off(/ amb 2))))))
    (if (and (> dur *pas-temps*) (> (abs (- v1 v2)) 100))
      (progn
        (p-abs (+ dat (round(/ dur 2))))
        (midi-write-ev *out* (pitch-bend :bend v2 :chan chan :port port))
        (ram-pitch chan port (round (/ dur 2))  (/ amb 2) off dat)
        (ram-pitch chan port (round (/ dur 2)) (/ amb 2) (+ off (/ amb 2)) 
                   (+ dat (round(/ dur 2))))))))

(defun ramp-pitch (chan port dur amb off) ;(print "ramp-pitch")
  (if (not (= dur 0))               ;ajout du 24 08 01
    (progn
  (midi-write-ev *out* (pitch-bend :bend (round(* *bend-demi-ton* off )) :chan chan :port port))
  ;(print "la dans ramp")
  (let* ((curdat (date?)))
    (if (not(= amb 0))
      (ram-pitch chan port dur amb off curdat)) ;(print "ici dans ramp")
    (p-abs (+ curdat (- dur 2)))     ;ajout du 24 08 01
    (midi-write-ev *out* (pitch-bend :bend (round(* *bend-demi-ton* 
                                                    (+ off amb))) :chan chan :port port))))))

;;---------------------------------------
;; glisse : glissando de pitch
;; ldur: liste de durées; 
;; lhaut: liste d'écarts de hauteur en 1/2 tons par rapport à la hauteur de référence
;;---------------------------------------

(defun glisse (chan port ldur lhaut &optional (prem 0))
  ;(format t "GLISSE ldur~S ~%" ldur)
  (when (and ldur lhaut)
    (ramp-pitch chan port (car ldur) (- (car lhaut) prem) prem)  
    (glisse chan port (cdr ldur) (cdr lhaut) (car lhaut))))


(defun chngaccord (l &optional (lr nil))
  (if l
    (progn (if (not (member (car l) lr))
             (setq lr (cons (car l) lr)))
           (chngaccord (cdr l) lr))
    lr))


;; Si (lenght lh) < (length ldur) alors la hauteur reste à sa dernière valeur

(defun glissand1 (chan dur port ldur lh stac)
  (let* ((d (floor (* noi (eval dur) stac)))
         (nl (somlist ldur))
         (n (length ldur))
         (ll nil)) 
    (dotimes (i n)
      (setq ll (cons (floor (* d (/ (car ldur) nl))) ll))
      (setq ldur (cdr ldur)))
    (glisse chan port (reverse ll) lh)))

(defun glissand2 (chan dur port lh stac)
  (let* ((d (floor (* noi (eval dur) stac)))
         (n (length lh))
         (dd (floor (/ d n)))
         (ll nil))
    (dotimes (i n)
      (setq ll (cons dd ll)))
    (glisse chan port ll lh)))

(defun glissand (chan dur port ldurgliss lhautgliss stac)
  (if ldurgliss
    (glissand1 chan dur port ldurgliss lhautgliss stac)
    (glissand2 chan dur port lhautgliss stac)))


(defun glissando (dat chan dur lacc ldurgliss lhautgliss stac)  ;;pas de récursion sur lacc
  (p-abs dat) ;; ajouté le 14-12-2000
  (when (and lacc (> (eval dur) 0))
    ;;(p-abs dat) supprimé le 14-12-2000
    (if (>= (+ chan (car lacc)) 0)
      (glissand (nchan (+ chan (car lacc))) dur (nport (+ chan (car lacc))) ldurgliss lhautgliss stac))
    (glissando dat chan dur (cdr lacc) ldurgliss lhautgliss stac)))



;;-------------------------------------------------------------------------------
;;                Traitement général
;;-------------------------------------------------------------------------------




; introduction des programmeChange le 20 10 01

;;------------------------------------
;; verif-pgchange : contrôle la validité du paramètre pgchange de prorythme
;;------------------------------------


;;------------------------------------

(defun verif (l chan laccord)
  (if (not (and (verif1 l) (verif2 l) (verif3 l))) 
    (abort)
    (verif4 l chan laccord)) l)

(defun verif-pgchange (l chan laccord)
  (if l (verif l chan laccord) nil))

;;------------------------------------


(defun liste-de-trois? (l) ;;************************************ ici
  (if (= (length l) 1)
    (if (= (length (car l)) 3) t nil)
    (and (= (length (car l)) 3) (liste-de-trois? (cdr l)))))

(defun verif1 (l) ;; vérifie que la liste pgchange est bien composée de listes de trois éléments
  (if (liste-de-trois? l) t 
      (progn (format t "DANS FONCTION verif1: ERREUR: Element de longueur non égale à 3 dans PGCHANGE: ~S ~%" l) nil)))

;;------------------------------------

(defun un-dans-liste? (a l)
  (if l (or (= a (caar l)) (un-dans-liste? a (cdr l))) nil))

(defun canaux-differents? (l)  
  (cond ((= (length l) 1) t)
        (t (and (not (un-dans-liste? (caar l) (cdr l))) (canaux-differents? (cdr l))))))

(defun verif2 (l) ;; vérifie qu'un même canal n'adresse pas plusieurs pgchange ou expressions
  (if (canaux-differents? l) t 
      (progn (format t "DANS FONCTION verif2: ERREUR: Un même canal dans PGCHANGE adresse plusieurs suites différentes: ~S ~%" l) nil)))

;;------------------------------------

(defun ldn? (l)
  (if l (ldn0? l) nil))

(defun ldn0? (l)
  (if l (and (numberp (car l)) (ldn0? (cdr l))) t))

(defun liste-de-nbre? (l)
  (if l (liste-de-nbre0? l) nil))

(defun liste-de-nbre0? (l)
  (if l (and (ldn? (car l)) (liste-de-nbre0? (cdr l))) t))

(defun verif3 (l) ;; vérifie que pgchange est une liste de liste de nombre
  (if (liste-de-nbre? l) t
      (progn (format t "DANS FONCTION verif3: ERREUR: PGCHANGE n'est pas une liste de liste de nombre: ~S ~%" l) nil)))

;;------------------------------------

(defun in-list? (a l)
  (if l (or (= a (car l)) (in-list? a (cdr l))) nil))


(defun vm0 (a chan laccord) ;(print (list "vm0" a chan laccord))
  (if (not (in-list? a (mapcar #'(lambda (x) (+ x chan)) laccord))) 
    (format t "DANS FONCTION VM0: ALERTE: Pas de canal ~S pour appliquer PGCHANGE ~%" a)))

(defun vm0 (a chan laccord) ;(print (list "vm0" a chan laccord)) ; modifier cette fonction de manière à tenir compte du :key proba de gnotes
  (if (if laccord (not (in-list? a (mapcar #'(lambda (x) (+ x chan)) laccord))) (not (= a chan))) 
    (format t "DANS FONCTION VM0: ALERTE: Pas de canal ~S pour appliquer PGCHANGE ~%" a)))



(defun verif4 (l1 chan laccord) ;; vérifie les adressages de pgchange sont inclus dans chan et laccord (sinon alerte et le programme continue
  (if l1 (progn (vm0 (caar l1) chan laccord) 
                (verif4 (cdr l1) chan laccord))))

                              

;;-------------------------------------------------------------------------------
;;         MAKE-CONTROL
;;
;; Ajout dU 28 Février 2005
;; Génére les controleurs spécifiés par le paramètre :lcontrol de gnotes
;; Les controleurs sont écrits par note ou accord, indépendaments des trilles et motifs
;;-------------------------------------------------------------------------------

(defun make-control (chan dur laccord lcontrol) ;(print (list "MAKE-CONTROL" chan laccord dur (date?)))
  (let ((d (eval dur)))
    (if (> d 0)
      (let ((lcanaux (mapcar #'(lambda (x) (+ x chan)) (remove-duplicates laccord))))
        (make-contro lcanaux d lcontrol)))))

(defun make-contro (lcanaux d lcontrol)
  (if lcanaux 
    (progn
      (make-contr (car lcanaux) d lcontrol)
      (make-contro (cdr lcanaux) d lcontrol))))


(defun make-contr (spechan d lcontrol) 
  (let* ((stime (date?))
         (etime (floor (+ stime (* noi d))))
         (ctime stime)
         (reverse nil)) 
    (make-cont (nchan spechan) (nport spechan) lcontrol stime ctime etime reverse)))


(defun make-cont (chan port lcontrol stime ctime etime reverse &key (vprecedent -1000))
  (let ((num §(eval(first lcontrol)))
        (v §(eval(second lcontrol)))
        (sv §(eval(third lcontrol)))) ; seuil=ecart minimum entre deux valeurs successives du controleur
    (if (<= ctime etime)
      (progn
        (midi-move *out* :date ctime)
        (if (>= (abs (- v vprecedent)) sv)  
          (progn
            (midi-write-ev *out* (ctrl-change :ctrl num :value v :chan chan :port port))
            (make-cont chan port lcontrol stime (+ ctime 50) etime reverse :vprecedent v))
          (make-cont chan port lcontrol stime (+ ctime 50) etime reverse :vprecedent vprecedent)))
      (p-abs stime))))
#|

|#






;;-------------------------------------------------------------------------------
;;         ACCORD
;;
;; Modif du 04 09 01
;; l'ancien "accord" devient "accord0"
;; LE nouvel accord supprime les pb avec les liaisons entre note et silence:
;; (+ -1 2) --> (-1) et (2)
;; (+ 1 -2) --> (1) et (-2)
;;-------------------------------------------------------------------------------

(defun accord ( dat chan pgchange dur haut vel sil stac tril ecar laccord lecar lmaxaccord lmotifhaut ldurgliss lhautgliss lapog ltransp ltextexpres larpege arpegecar layer lcontrol)
  ;(print (list "accord" dat dur haut))
  (if lcontrol (make-control chan dur laccord lcontrol))
  ;(print (list "ACCORD" chan dur laccord lecar (date?)))
  (if pgchange (setq pgchange (verif-pgchange pgchange chan laccord)))  ;; vérifie la cohérence de pgchange et le modifie le cas échéant
  (if (and (listp dur) (equal '+ (first dur)) (or (sign (eval (second dur))) (sign (eval(third dur)))))
    (progn 
      (accord dat chan pgchange (second dur) haut vel sil stac tril ecar laccord lecar lmaxaccord lmotifhaut ldurgliss lhautgliss lapog ltransp ltextexpres larpege arpegecar layer lcontrol)
      (accord dat chan pgchange (third dur) haut vel sil stac tril ecar laccord lecar lmaxaccord lmotifhaut ldurgliss lhautgliss lapog ltransp ltextexpres larpege arpegecar layer lcontrol))
    (accord0 dat chan pgchange dur haut vel sil stac tril ecar laccord lecar lmaxaccord lmotifhaut ldurgliss lhautgliss lapog ltransp ltextexpres larpege arpegecar layer)))

(defun accord0 ( dat chan pgchange dur haut vel sil stac tril ecar laccord lecar lmaxaccord lmotifhaut ldurgliss lhautgliss lapog ltransp ltextexpres larpege arpegecar layer)
  ;(print (list "accord0" chan laccord lecar))
  (if lhautgliss (glissando dat chan dur (chngaccord laccord) ldurgliss lhautgliss stac)) 
  (fait-accord dat chan pgchange dur haut vel sil stac tril ecar laccord lecar lmaxaccord lmotifhaut lapog ltransp ltextexpres larpege arpegecar layer ldurgliss lhautgliss))










