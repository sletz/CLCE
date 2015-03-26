
;; PAJ - Fichier cr�e le 9 Mars 2005

;;==============================================================================
;;
;;                            GESTION DES CONTROLEURS MIDI
;;
;;==============================================================================



;;------------------------------------------------------------------------------
;; eval-dur-rythme:  retourne la dur�e d'une structure rythmique
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
;; genere0: (deb fin interval rythme) - Tous les param�tres sont des g�n�rateurs.
;;
;; deb: date de d�but de la s�quence
;; fin: date de fin de la s�quence
;; rythme: motif rythmique
;; interval: interval temporel entre chaque instance de rythme
;;
;; G�n�re sur l'intervalle de temps deb/fin une trame temporelle sous forme d'une liste de liste.
;; Chaque liste interne est de la forme (datedeb datefin rythme) o� datefin-datedeb est la dur�e
;; du motif rythmique rythme et o� rythme est une instance de rythme
;; Chaque instance de rythme est s�par�e de la pr�cedente d'une dur�e �gale � interval (exprim� en noire)
;; Cette fonction est utilis�e par la fonction GENERE ci-dessous
;;
;; Exemples:
;;
;; (genere0 �0 �17 �0 �(8)) --> ((0 40320 (8)) (40320 80640 (8)))
;; (genere0 �0 �17 �2 �(8)) --> ((0 40320 (8)))
;; (genere0 �0 �17 �2 �(4 + 2/3n (1 -1 1) + 2)) --> ((0 40320 (4 + 2/3N (1 -1 1) + 2)))
;; (genere0 �0 �24 �2 (alt �(1) �(4 + 2/3n (1 -1 1) + 2)))
;;------------------------------------------------------------------------------
(defun genere0 (deb fin interval rythme)
  (let* ((stime (* (funcall deb 0 0 1 nil) noi))
         (etime (* (funcall fin 0 1 1 nil) noi))
         (reverse nil))
    (p-abs stime) 
    (gener0 interval rythme stime stime etime reverse)))

(defun gener0 (interval rythme stime ctime etime reverse) 
  (let* ((rythm �rythme)
         (dur (* (eval-dur-rythme rythm) noi)))
    (if (<= (+ ctime dur) etime)
      (cons (list ctime (+ ctime dur) rythm) 
            (gener0 interval rythme stime (+ ctime dur (* noi �interval)) etime reverse)))))



;;==============================================================================
;; genere: (deb fin objet interval rythme) - Tous les param�tres sont des g�n�rateurs.
;;
;; deb: g�n�rateur de date de d�but (en noires) d'instanciation de l'objet "objet"
;; fin: g�n�rateur de date de fin (en noires) d'instanciation de l'objet "objet"
;; objet: g�n�rateur � instancier (par exemple: gcontroleur ou gcontroleurlineaire)
;; interval: g�n�rateur d'intervalle temporel entre deux s�quences d'�valuation
;; rythme: g�n�rateur de rythme.
;;
;; Instancie des objets sur la trame rythmique cr��e par rythme 
;; 
;; Mode de fonctionnement:
;; 1- La dur�e dur-rythme de rythme est calcul� � chaque �valuation
;; 2- La s�quence temporelle de dur�es: (dur-rythme interval dur-rythme interval ...)
;;    est calcul�e entre les dates fournies par deb et fin
;; 3- L'objet objet est instanci� sur les plages temporelles correspondant � dur-rythme.
;;
;; Exemples: (voir gcontroleur ci-dessous)
;;
;; (defun testgenere ()
;;  (midiclear)
;;  (genere �0 �16 (gcontroleur �0 �7 (floo (i �0 �127)) �100) �0 �(8)))
;;
;; (testgenere)
;;
;; (defun testgenere ()
;;  (midiclear)
;;  (genere �0 �16 (gcontroleur �0 �7 (floo (i �0 �127)) �100) �0 (seq �(1) �(4))))
;;
;; (testgenere)
;;
;; Examiner alors le contenu de la s�quence par: (midiprint)
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
;; gcontroleur: (spechan num val &key (seuilt �100) (seuilv �1)) - Tous les param�tres sont des g�n�rateurs.
;;
;; spechan: g�n�rateur de canal Midi g�n�ralis� (de 0 � 31 ...)
;; num: g�n�rateur de num�ro de controleur
;; val: g�n�rateur de valeurs
;; Param�tres optionnels:
;; seuilt: g�n�rateur d'�cart temporel (en 1/5040 de noire) entre 2 �critures du controleur
;; seuilv: g�n�rateur d'�cart de valeur entre 2 �critures du controleur
;;
;; Objet Controleur MIDI:
;; Ecrit les valeurs val du controleur n� num pour le canal g�n�ralis� spechan
;; Le pas minimum temporel est d�livr� par seuilt L'�cart entre deux valeurs du controleur est donn� par seuilv
;;
;; Exemple:
;; G�n�re des crescendo de volume (de 0 � 127) toutes les 8 noires.
;;
;; (defun testgenere ()
;;  (midiclear)
;;  (genere �0 �16 (gcontroleur �0 �7 (floo (i �0 �127))) �0 �(8))
;;  (prorythme �0 �0 �16 (gnotes) �0 �(1/2) (s�(60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75)) �110)
;;  (player))
;;
;; (testgenere)
;;
;; (defun testgenere ()
;;  (midiclear)
;;  (genere �0 �16 (gcontroleur �0 �7 (floo (i �0 �127)) :seuilv �5) �0 �(8))
;;  (prorythme �0 �0 �16 (gnotes) �0 �(1/2) (s�(60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75)) �110)
;;  (player))
;;
;; (testgenere)
;;
;; Remarque importante: Le programme impose que l'�cart entre deux valeurs de controleur
;; successives, soit sup�rieure ou �gal � seuilv Cela implique que l'�cart seuilt n'est pas respect� dans le cas
;; o� la valeur du controleur n'a pas suffisament chang� pendant l'intervalle de temps seuilt
;;==============================================================================

(defun gcontroleur (spechan num val &key (seuilt �100) (seuilv �1))
  #'(lambda (stime ctime etime reverse)
      (setq ctime stime)
      (controleur spechan num val seuilt seuilv stime ctime etime reverse)))
      


(defun controleur (spechan num val seuilt seuilv stime ctime etime reverse &key (vprecedent �-1000))
  (let* ((v �val)
         (canal �spechan)
         (port (mod canal 15))
         (cha (- canal (* 15 port)))
         (s �seuilt)
         (vp �vprecedent))
         (if (<= ctime etime)
           (progn
             (midi-move *out* :date ctime)
             (if (>= (abs (- v vp)) �seuilv)  
               (progn
                 (midi-write-ev *out* (ctrl-change :ctrl �num :value v :chan cha :port port))
                 (controleur spechan num val seuilt seuilv stime (+ ctime s) etime reverse :vprecedent (g v)))
               (controleur spechan num val seuilt seuilv stime (+ ctime s) etime reverse :vprecedent (g vp)))))))


;;==============================================================================
;; gcontroleurlineaire: (spechan num lval ldur &key (seuilt �100) (seuilv �1)) - Tous les param�tres sont des g�n�rateurs.
;; 
;; spechan: g�n�rateur de canal Midi g�n�ralis� (de 0 � 31 ...)
;; num: g�n�rateur de num�ro de controleur
;; lval: g�n�rateur de liste de valeurs de controleur
;; ldur: g�n�rateur de liste de dur�es de palier (les dur�es sont exprim�es en proportion:
;; �(1 1) --> 2 paliers de dur�es �gales; �(2 1) --> le premier palier est le double du second
;; Param�tres optionnels:
;; seuilt: g�n�rateur d'�cart temporel (en 1/5040 de noire) entre 2 �critures du controleur
;; seuilv: g�n�rateur d'�cart de valeur entre 2 �critures du controleur
;;
;; Objet Controleur MIDI:
;; R�alise des segments (de valeurs de controleur) reliant les valeurs de lval suivant les dur�es de ldur
;; Le pas minimum temporel est d�livr� par seuilt L'�cart entre deux valeurs du controleur est donn� par seuilv  
;;
;; Exemple:
;; Variation de volume tous les 500/5040 de noire.
;; G�n�re un profil toutes les 8 noires selon le sch�ma suivant:
;; de 30 � 127 en un palier de proportion 10/15, puis de 127 � 30 en un palier de proportion 5/15.
;; Si on avait �crit �(3 2) au lieu de �(15 10), le r�sultat serait le m�me!
;;
;;(defun testgenere ()
;;  (midiclear)
;;  (genere �0 �16 (gcontroleurlineaire �0 �7 �(30 127 30) �(10 5) :seuilt �500) �0 �(8))
;;  (prorythme �0 �0 �16 (gnotes) �0 �(1/2) (s�(60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75)) �110)
;;  (player))
;;
;; (testgenere)
;;==============================================================================                                          



(defun gcontroleurlineaire (spechan num lval ldur &key (seuilt �100) (seuilv �1))
  #'(lambda (stime ctime etime reverse)
      (setq ctime stime)
      (controlin spechan num lval ldur seuilt seuilv stime ctime etime reverse)))

(defun controlin (spechan num lval ldur seuilt seuilv stime ctime etime reverse &key (vprecedent �-1000))
  (let* ((lival �lval)
         (nval (length lival))
         (lidur �ldur)
         (ndur (length lidur))
         (dur (- etime stime))
         (somlidur (somlist lidur)))
    (cond
     ((> (1- nval) ndur)
      (format t "Erreur dans la fonction CONTROLIN: Liste de dur�es trop courte!~%")
      (format t "Elle ne contient que ~S valeurs " ndur)
      (format t "au lieu de ~S ~%" (1- nval))(break))
     ((< (1- nval) ndur) 
      (format t "Attention dans la fonction CONTROLIN: Liste de dur�es trop longue!~%")
      (format t "La liste est ramen�e � une longueur de ~S ~%" (1- nval))
      (setq lidur (npremier (1- nval) lidur)))
     (t ))
    (setq somlidur (somlist lidur)) 
    (setq lidur (mapcar #'(lambda (x) (floor (* x (/ dur somlidur)))) lidur)) ; lidur est � pr�sent la liste des dur�es r�elles des paliers
    (controli spechan num lival lidur seuilt seuilv stime ctime etime reverse vprecedent)))

(defun controli (spechan num lival lidur seuilt seuilv stime ctime etime reverse vprecedent)
  (let* ((canal �spechan)
         (port (mod canal 15))
         (cha (- canal (* 15 port)))
         (n �num))
    (if lidur
      (progn
        (palier cha port n ctime (+ ctime (car lidur)) (car lival) (second lival) �seuilt �seuilv �vprecedent)
        (controli spechan num (cdr lival) (cdr lidur) seuilt seuilv stime (+ ctime (car lidur)) etime reverse vprecedent)))))


;; impl�mente une variation lin�aire du controleur n� num 
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
;; gcontrl (deb fin spechan num val &key (delta �100)) objet Controleur MIDI avec pour param�tres:
;;
;; deb: date de d�but (en noires) d'�valuation du controleur
;; fin: date de fin (en noires) d'�valuation du controleur
;; spechan: g�n�rateur de canal Midi g�n�ralis� (de 0 � 31 ...)
;; num: g�n�rateur de num�ro de controleur
;; val: g�n�rateur de liste de valeurs de controleur
;; delta: g�n�rateur d'�cart temporel (en 1/1000 seconde) entre 2 valeurs du controleur
;;
;; Instancie entre les dates deb et fin (toutes les tranches de temps delta),
;; le contoleur n� num avec les valeurs d�livr�es par val, pour le canal g�n�ralis� spechan.  
;;
;; Exemple:
;; Crescendo de volume de 0 � 127 (tous les 500/1000), entre les dates 0 et 8 noires.
;;
;;(defun testgcontrl ()
;;  (midiclear)
;;  (gcontrl �0 �8 �0 �7 (floo (i �0 �127))))
;;
;;(testgcontrl)
;;
;; Exemple:
;; Le volume de 0 est plac� � la date z�ro
;; Le volume de 127 est plac� � la date 4 (noires)
;;
;;(defun testgcontrl ()
;;   (midiclear)
;;   (gcontrl �0 �8 �0 �7 (seq �50 �100) :delta �500))
;;==============================================================================


(defun gcontrl (deb fin spechan num val &key (delta �100))
  (let* ((stime (* noi (funcall deb 0 0 1 nil)))
         (etime (* noi (funcall fin 0 1 1 nil)))
         (ctime stime)
         (reverse nil))
    (gcont spechan stime ctime etime reverse num val delta -1000)))

(defun gcont (spechan stime ctime etime reverse num val delta vp)
  (if (< ctime etime)
    (let* ((canal �spechan)
           (port (mod canal 15))
           (cha (- canal (* 15 port)))
           (v �val))
      (if (not (= v vp))
        (progn
          (midi-move *out* :date ctime)
          (midi-write-ev *out* (ctrl-change :ctrl �num :value v :chan cha :port port))))
      (gcont spechan stime (+ ctime �delta) etime reverse num val delta v))))






;;==============================================================================
;; uncontroleur (date spechan num v) objet Controleur MIDI avec pour param�tres:
;;
;; ATTENTION: Les param�tres sont des nombres mais pas des g�n�rateurs
;;
;; date: date d'une valeur de controleur
;; spechan: g�n�rateur de canal Midi g�n�ralis� (de 0 � 31 ...)
;; num: num�ro du controleur
;; v: valeurs du controleur
;;
;; Ecrit la valeur v du controleur n� num pour le canal g�n�ralis� spechan
;; � la date date dans la score *out*
;;
;; Exemple:
;; (midiclear)
;; (uncontroleur 1 0 7 100)
;; (midiprint)
;;
;; Remarque:
;; Cette fonction est utile pour remettre � 127 le niveau de volume (par exemple)
;; apr�s des manipulations complexes de volume.
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


