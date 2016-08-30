
;; PAJ - Fichier crée le 9 Mars 2005

;;==============================================================================
;;
;;                            GESTION DES CONTROLEURS MIDI
;;
;;==============================================================================



;;------------------------------------------------------------------------------
;; eval-dur-rythme:  retourne la durée d'une structure rythmique
;; (eval-dur-rythme '(2/3n(-1 1 1) + 2/3n (1 2/3c(-1/2 1/2 1/2) -1/2 1/2))) -->4
;; (eval-dur-rythme '( 2/3n (2 2/3c (1/2 1/2 1/2)) + 1/2 + 1 1/2 1/2 + 1 + 1 + 1/2 + 4)) --> 11
;; (eval-dur-rythme '( 2/3n (2 2/3c (1/2 1/2 -1/2)) + 1/2 + 1 1/2 1/2 + 1 + 1 + 1/2 + 4))

;;------------------------------------------------------------------------------

(defun rendpos-der (x)
  (cond
   ((numberp x) (abs x))
   ((listp x) (rendpos-der-list x))
   (t x)))

(defun rendpos-der-list (x)
  (let ((der (car (last x))))
    (reverse (cons (abs der) (cdr (reverse x))))))

(defun rend-positif (l &optional (res nil))
  (if l
    (rend-positif (cdr l) (cons (rendpos-der (car l)) res))
    (reverse res)))

;; (rend-positif '(+ (* (FDUR 2 3 D-N) (FDUR 2 3 D-C) -1/2) -10 2))


;;------------------------------------------------------------------------------


(defun eval-dur-rythme (r)
  (eval-dur-rythm (prefixplus (rend-positif (code-rythme r)))))

(defun eval-dur-rythm (r)
  (if (eq r nil) 0
      (+ (abs (eval(first r))) (eval-dur-rythm (cdr r)))))

;;------------------------------------------------------------------------------
;; genere0: (deb fin interval rythme) - Tous les paramètres sont des générateurs.
;;
;; deb: date de début de la séquence
;; fin: date de fin de la séquence
;; rythme: motif rythmique
;; interval: interval temporel entre chaque instance de rythme
;;
;; Génére sur l'intervalle de temps deb/fin une trame temporelle sous forme d'une liste de liste.
;; Chaque liste interne est de la forme (datedeb datefin rythme) où datefin-datedeb est la durée
;; du motif rythmique rythme et où rythme est une instance de rythme
;; Chaque instance de rythme est séparée de la précedente d'une durée égale à interval (exprimé en noire)
;; Cette fonction est utilisée par la fonction GENERE ci-dessous
;;
;; Exemples:
;;
;; (genere0 °0 °17 °0 °(8)) --> ((0 40320 (8)) (40320 80640 (8)))
;; (genere0 °0 °17 °2 °(8)) --> ((0 40320 (8)))
;; (genere0 °0 °17 °2 °(4 + 2/3n (1 -1 1) + 2)) --> ((0 40320 (4 + 2/3N (1 -1 1) + 2)))
;; (genere0 °0 °24 °2 (alt °(1) °(4 + 2/3n (1 -1 1) + 2)))
;;------------------------------------------------------------------------------
(defun genere0 (deb fin interval rythme)
  (let* ((stime (* (funcall deb 0 0 1 nil) noi))
         (etime (* (funcall fin 0 1 1 nil) noi))
         (reverse nil))
    (p-abs stime) 
    (gener0 interval rythme stime stime etime reverse)))

(defun gener0 (interval rythme stime ctime etime reverse) 
  (let* ((rythm §rythme)
         (dur (* (eval-dur-rythme rythm) noi)))
    (if (<= (+ ctime dur) etime)
      (cons (list ctime (+ ctime dur) rythm) 
            (gener0 interval rythme stime (+ ctime dur (* noi §interval)) etime reverse)))))



;;==============================================================================
;; genere: (deb fin objet interval rythme) - Tous les paramètres sont des générateurs.
;;
;; deb: générateur de date de début (en noires) d'instanciation de l'objet "objet"
;; fin: générateur de date de fin (en noires) d'instanciation de l'objet "objet"
;; objet: générateur à instancier (par exemple: gcontroleur ou gcontroleurlineaire)
;; interval: générateur d'intervalle temporel entre deux séquences d'évaluation
;; rythme: générateur de rythme.
;;
;; Instancie des objets sur la trame rythmique créée par rythme 
;; 
;; Mode de fonctionnement:
;; 1- La durée dur-rythme de rythme est calculé à chaque évaluation
;; 2- La séquence temporelle de durées: (dur-rythme interval dur-rythme interval ...)
;;    est calculée entre les dates fournies par deb et fin
;; 3- L'objet objet est instancié sur les plages temporelles correspondant à dur-rythme.
;;
;; Exemples: (voir gcontroleur ci-dessous)
;;
;; (defun testgenere ()
;;  (midiclear)
;;  (genere °0 °16 (gcontroleur °0 °7 (floo (i °0 °127)) °100) °0 °(8)))
;;
;; (testgenere)
;;
;; (defun testgenere ()
;;  (midiclear)
;;  (genere °0 °16 (gcontroleur °0 °7 (floo (i °0 °127)) °100) °0 (seq °(1) °(4))))
;;
;; (testgenere)
;;
;; Examiner alors le contenu de la séquence par: (midiprint)
;;==============================================================================

(defun genere (deb fin objet interval rythme)
  (gener (genere0 deb fin interval rythme) objet))

(defun gener (l objet)
  (if l
    (let ((stime (caar l))
          (etime (second(car l))))  
      (funcall objet stime stime etime nil)
      (gener (cdr l) objet))))



;;==============================================================================
;; gcontroleur: (spechan num val &key (seuilt °100) (seuilv °1)) - Tous les paramètres sont des générateurs.
;;
;; spechan: générateur de canal Midi généralisé (de 0 à 31 ...)
;; num: générateur de numéro de controleur
;; val: générateur de valeurs
;; Paramètres optionnels:
;; seuilt: générateur d'écart temporel (en 1/5040 de noire) entre 2 écritures du controleur
;; seuilv: générateur d'écart de valeur entre 2 écritures du controleur
;;
;; Objet Controleur MIDI:
;; Ecrit les valeurs val du controleur n° num pour le canal généralisé spechan
;; Le pas minimum temporel est délivré par seuilt L'écart entre deux valeurs du controleur est donné par seuilv
;;
;; Exemple:
;; Génére des crescendo de volume (de 0 à 127) toutes les 8 noires.
;;
;; (defun testgenere ()
;;  (midiclear)
;;  (genere °0 °16 (gcontroleur °0 °7 (floo (i °0 °127))) °0 °(8))
;;  (prorythme °0 °0 °16 (gnotes) °0 °(1/2) (s°(60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75)) °110)
;;  (player))
;;
;; (testgenere)
;;
;; (defun testgenere ()
;;  (midiclear)
;;  (genere °0 °16 (gcontroleur °0 °7 (floo (i °0 °127)) :seuilv °5) °0 °(8))
;;  (prorythme °0 °0 °16 (gnotes) °0 °(1/2) (s°(60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75)) °110)
;;  (player))
;;
;; (testgenere)
;;
;; Remarque importante: Le programme impose que l'écart entre deux valeurs de controleur
;; successives, soit supérieure ou égal à seuilv Cela implique que l'écart seuilt n'est pas respecté dans le cas
;; où la valeur du controleur n'a pas suffisament changé pendant l'intervalle de temps seuilt
;;==============================================================================

(defun gcontroleur (spechan num val &key (seuilt °100) (seuilv °1))
  #'(lambda (stime ctime etime reverse)
      (setq ctime stime)
      (controleur spechan num val seuilt seuilv stime ctime etime reverse)))
      


(defun controleur (spechan num val seuilt seuilv stime ctime etime reverse &key (vprecedent °-1000))
  (let* ((v §val)
         (canal §spechan)
         (port (mod canal 15))
         (cha (- canal (* 15 port)))
         (s §seuilt)
         (vp §vprecedent))
         (if (<= ctime etime)
           (progn
             (midi-move *out* :date ctime)
             (if (>= (abs (- v vp)) §seuilv)  
               (progn
                 (midi-write-ev *out* (ctrl-change :ctrl §num :value v :chan cha :port port))
                 (controleur spechan num val seuilt seuilv stime (+ ctime s) etime reverse :vprecedent (g v)))
               (controleur spechan num val seuilt seuilv stime (+ ctime s) etime reverse :vprecedent (g vp)))))))


;;==============================================================================
;; gcontroleurlineaire: (spechan num lval ldur &key (seuilt °100) (seuilv °1)) - Tous les paramètres sont des générateurs.
;; 
;; spechan: générateur de canal Midi généralisé (de 0 à 31 ...)
;; num: générateur de numéro de controleur
;; lval: générateur de liste de valeurs de controleur
;; ldur: générateur de liste de durées de palier (les durées sont exprimées en proportion:
;; °(1 1) --> 2 paliers de durées égales; °(2 1) --> le premier palier est le double du second
;; Paramètres optionnels:
;; seuilt: générateur d'écart temporel (en 1/5040 de noire) entre 2 écritures du controleur
;; seuilv: générateur d'écart de valeur entre 2 écritures du controleur
;;
;; Objet Controleur MIDI:
;; Réalise des segments (de valeurs de controleur) reliant les valeurs de lval suivant les durées de ldur
;; Le pas minimum temporel est délivré par seuilt L'écart entre deux valeurs du controleur est donné par seuilv  
;;
;; Exemple:
;; Variation de volume tous les 500/5040 de noire.
;; Génére un profil toutes les 8 noires selon le schéma suivant:
;; de 30 à 127 en un palier de proportion 10/15, puis de 127 à 30 en un palier de proportion 5/15.
;; Si on avait écrit °(3 2) au lieu de °(15 10), le résultat serait le même!
;;
;;(defun testgenere ()
;;  (midiclear)
;;  (genere °0 °16 (gcontroleurlineaire °0 °7 °(30 127 30) °(10 5) :seuilt °500) °0 °(8))
;;  (prorythme °0 °0 °16 (gnotes) °0 °(1/2) (s°(60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75)) °110)
;;  (player))
;;
;; (testgenere)
;;==============================================================================                                          



(defun gcontroleurlineaire (spechan num lval ldur &key (seuilt °100) (seuilv °1))
  #'(lambda (stime ctime etime reverse)
      (setq ctime stime)
      (controlin spechan num lval ldur seuilt seuilv stime ctime etime reverse)))

(defun controlin (spechan num lval ldur seuilt seuilv stime ctime etime reverse &key (vprecedent °-1000))
  (let* ((lival §lval)
         (nval (length lival))
         (lidur §ldur)
         (ndur (length lidur))
         (dur (- etime stime))
         (somlidur (somlist lidur)))
    (cond
     ((> (1- nval) ndur)
      (format t "Erreur dans la fonction CONTROLIN: Liste de durées trop courte!~%")
      (format t "Elle ne contient que ~S valeurs " ndur)
      (format t "au lieu de ~S ~%" (1- nval))(break))
     ((< (1- nval) ndur) 
      (format t "Attention dans la fonction CONTROLIN: Liste de durées trop longue!~%")
      (format t "La liste est ramenée à une longueur de ~S ~%" (1- nval))
      (setq lidur (npremier (1- nval) lidur)))
     (t ))
    (setq somlidur (somlist lidur)) 
    (setq lidur (mapcar #'(lambda (x) (floor (* x (/ dur somlidur)))) lidur)) ; lidur est à présent la liste des durées réelles des paliers
    (controli spechan num lival lidur seuilt seuilv stime ctime etime reverse vprecedent)))

(defun controli (spechan num lival lidur seuilt seuilv stime ctime etime reverse vprecedent)
  (let* ((canal §spechan)
         (port (mod canal 15))
         (cha (- canal (* 15 port)))
         (n §num))
    (if lidur
      (progn
        (palier cha port n ctime (+ ctime (car lidur)) (car lival) (second lival) §seuilt §seuilv §vprecedent)
        (controli spechan num (cdr lival) (cdr lidur) seuilt seuilv stime (+ ctime (car lidur)) etime reverse vprecedent)))))


;; implémente une variation linéaire du controleur n° num 
;; entre les temps t1 et t2 et les valeurs v1 et v2

    (defun palier (canal port num t1 t2 v1 v2 seuilt seuilv vp)
      (if (< t1 t2) 
        (if (>= (abs (- v1 vp)) seuilv) 
          (progn
            (midi-move *out* :date t1)
            (midi-write-ev *out* (ctrl-change :ctrl num :value (floor v1) :chan canal :port port))
            (palier canal port num (+ t1 seuilt) t2 (proportion t1 t2 v1 v2 seuilt) v2 seuilt seuilv v1))
          (palier canal port num (+ t1 seuilt) t2 (proportion t1 t2 v1 v2 seuilt) v2 seuilt seuilv vp))))


(defun proportion (t1 t2 v1 v2 delta)
  (if (not (= t1 t2))
    (+ v1 (/ (* (- v2 v1) delta) (- t2 t1)))
    (format t "Erreur dans proportion: t1 = t2")))




;;==============================================================================
;; gcontrl (deb fin spechan num val &key (delta °100)) objet Controleur MIDI avec pour paramètres:
;;
;; deb: date de début (en noires) d'évaluation du controleur
;; fin: date de fin (en noires) d'évaluation du controleur
;; spechan: générateur de canal Midi généralisé (de 0 à 31 ...)
;; num: générateur de numéro de controleur
;; val: générateur de liste de valeurs de controleur
;; delta: générateur d'écart temporel (en 1/1000 seconde) entre 2 valeurs du controleur
;;
;; Instancie entre les dates deb et fin (toutes les tranches de temps delta),
;; le contoleur n° num avec les valeurs délivrées par val, pour le canal généralisé spechan.  
;;
;; Exemple:
;; Crescendo de volume de 0 à 127 (tous les 500/1000), entre les dates 0 et 8 noires.
;;
;;(defun testgcontrl ()
;;  (midiclear)
;;  (gcontrl °0 °8 °0 °7 (floo (i °0 °127))))
;;
;;(testgcontrl)
;;
;; Exemple:
;; Le volume de 0 est placé à la date zéro
;; Le volume de 127 est placé à la date 4 (noires)
;;
;;(defun testgcontrl ()
;;   (midiclear)
;;   (gcontrl °0 °8 °0 °7 (seq °50 °100) :delta °500))
;;==============================================================================


(defun gcontrl (deb fin spechan num val &key (delta °100))
  (let* ((stime (* noi (funcall deb 0 0 1 nil)))
         (etime (* noi (funcall fin 0 1 1 nil)))
         (ctime stime)
         (reverse nil))
    (gcont spechan stime ctime etime reverse num val delta -1000)))

(defun gcont (spechan stime ctime etime reverse num val delta vp)
  (if (< ctime etime)
    (let* ((canal §spechan)
           (port (mod canal 15))
           (cha (- canal (* 15 port)))
           (v §val))
      (if (not (= v vp))
        (progn
          (midi-move *out* :date ctime)
          (midi-write-ev *out* (ctrl-change :ctrl §num :value v :chan cha :port port))))
      (gcont spechan stime (+ ctime §delta) etime reverse num val delta v))))






;;==============================================================================
;; uncontroleur (date spechan num v) objet Controleur MIDI avec pour paramètres:
;;
;; ATTENTION: Les paramètres sont des nombres mais pas des générateurs
;;
;; date: date d'une valeur de controleur
;; spechan: générateur de canal Midi généralisé (de 0 à 31 ...)
;; num: numéro du controleur
;; v: valeurs du controleur
;;
;; Ecrit la valeur v du controleur n° num pour le canal généralisé spechan
;; à la date date dans la score *out*
;;
;; Exemple:
;; (midiclear)
;; (uncontroleur 1 0 7 100)
;; (midiprint)
;;
;; Remarque:
;; Cette fonction est utile pour remettre à 127 le niveau de volume (par exemple)
;; après des manipulations complexes de volume.
;;==============================================================================


(defun uncontroleur (date spechan num v)
  (let* ((canal spechan)
         (port (mod canal 15))
         (cha (- canal (* 15 port))))
    (midi-move *out* :date (* date noi))
    (midi-write-ev *out* (ctrl-change :ctrl num :value v  :chan cha :port port))))

(defun uncontroleur (date spechan num v)
  (let ((port (nport spechan))
         (cha (nchan spechan)))
    (midi-move *out* :date (* date noi))
    (midi-write-ev *out* (ctrl-change :ctrl num :value v  :chan cha :port port))))
;;------------------------------------------------------------------------------


