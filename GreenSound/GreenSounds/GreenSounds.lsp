#|
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////

                       GREEN-SOUNDS (version d�finitive du 25 mars 2015)

                 pour l'installation interactive � la galerie ATTRAPE COULEUR (mars 2015) 

 //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////





 GREEN-SOUNDS est constitu� des s�quences d'ANIMOTS (all�g�es) plus un syst�me d'interaction temps r�el
 KONTAKT: utiliser AttrapeCouleur-Convol6sorties1portBuild.nkm (tous les instruments sur le port 0)
 suppression de la possibilit� d'un second player le 25 janvier 2015

   
 R�alisation � partir de la restauration de l'installation ANIMOTS du Parc de Gerland - 11 Septembre 2013
 Utilise l'�chantilloneur KONTAKT (import version 3.1.0.4)
 Assistance informatique:
 St�phane LETZ pour la partie LISP
 Christophe LEBRETON pour la partie MAX
 Programmation expo Attrape Couleur: James Giroudon

                     







 //////////////////////////////////////////////////////////////////////////////////////////////////////////////////

                               FONCTIONNEMENT GENERAL 

 //////////////////////////////////////////////////////////////////////////////////////////////////////////////////






Une PARTIE est compos�e d'une SEQUENCE au sens PRORYTHME (jou�e par le PLAYER) et d'un type d'interaction
Toutes les parties ont la m�me dur�e = *duree-sequence*

Une PARTIE est lanc�e:

Phase 1:
Si une interaction est r�alis�e ( *debut-interaction* = t ) avant une dur�e de *duree-sequence-initiale*:
- alors le player est stopp� au bout de cette dur�e
- sinon le PLAYER n'est pas stopp�

Phase 2
- le player est stopp� d�s qu'un interaction se produit (au bout d'un temps = *delai-replay*)
- le player est relanc� si aucune interaction n'est produite au bout d'une dur�e de *attente*

Dans tous les cas l'interaction est active

Lorsque le temps �coul� depuis le d�but de la s�quence est �gal � *duree-sequence*, 
le player est stopp� et une nouvelle s�quence d�marre apr�s un temps �gal � *separation-partie* 



 Phase 1                         Phase 2
     *duree-sequence-initiale*               (- *duree-sequence* *duree-sequence-initiale*)          *separation-partie*
 I-------------------------------I***************************************************************I-------------------------i
 DEPART PLAYER                   STOP PLAYER (si il y a eu auparavant une interaction)     STOP PLAYER              



-------------------- = temps diff�r� + temps r�el
******************** = temps r�el et diff�r� altern�





 //////////////////////////////////////////////////////////////////////////////////////////////////////////////////

                               PROGRAMMES DE L'ECHANTILLONEUR KONTAKT 

 //////////////////////////////////////////////////////////////////////////////////////////////////////////////////





D�finition du Multi KONTAKT compos� de 12 programmes (A1 ... A6 et A7 ... A12) de chacun 6 layers (ou groupes) P1, P2, P3, P4, P5 et P6
							
Nom Layer (et type de sons et valeurs des hauteurs MIDI):
							
P1	Guiro	Percu	PercuCastagnet	Riviere			
	21-68	69-96	97-125	        126-127			
P2	Triangle tenu	Insectes faux	Riviere	-----------------------> les sons "triangle" ont �t� remplac� par "bol japonais" le 28 03 2015			
	21-83	        84-125	        126-127				
P3	Tenu nuit	Tenu jour	Insect/ oiseau	Riviere			
	21-38	        39-68	        69-78/79-125	126-127			
P4	Voix	Cecilia2					
	21-50	51-125					
P5	Voix	Tenu jour court	  Insect/ oiseau				
	21-50	51-68	          69-78/79-125				
P6	Celli   Violons						
	36-54   55-96	
P7      Fond    NewOiseaux
        12-14   24-83
					
6 programmes A (ch de 1 � 6) de chacun 7 layers:				
ProgA1:	
						
layer	Contrl n�9	Port  Canal		
P1	1	        A	1					
P2	2	        A	1	
P3	3	        A	1	
P4	4	        A	1	
P5	5	        A	1		
P6	6	        A	1
P7	7	        A	1
		
ProgA2 --> ProgA6 (idem A1)	
			
6 programmes B (ch de 7 � 12):			
ProgB1:	
			
layer	Contrl n�9	Port  Canal		
P1	1	        A	7						
P2	2	        A	7				
P3	3	        A	7				
P4	4	        A	7						
P5	5	        A	7
P6	6	        A	7
P7	7	        A	1						

ProgB2 --> ProgB6 (idem B1)



Adressage des layers (ou groupes) par la fonction:       PLACE-PROG (date M-xx *out*)
Suivant les valeurs M-xx ci-dessous pass�es � la fonction, pour respectivement les programmes A et B on active les layers:  
			
Nom	Layer   Layer	
				
M1	P1	P2	
M2	P1	P3	
M3	P1	P4	
M4	P2	P3	
M5	P2	P4	
M6	P3	P4	
M7	P1	P5	
M8	P2	P5	
M9	P6	P6
M10     P6      P3 
M11     P6      P7
				
La fonction PLACE_PROG activer les LAYERS par un envoit � KONTAKT du controleur n�9 avec des valeurs de 1 � 7 suivant le n� de LAYER � activer				
	


			
|#


;; ***************************************************************************     
;;
;; Enchainement temporel des parties 
;;
;; ***************************************************************************

(defvar *parties-process* nil)

;; Pour tester
(defun test1 (pitch dur)
  #'(lambda (stime ctime etime reverse)
      (declare (ignore �stime ctime etime reverse))
      (let ((curdate (midigettime)))
        (dotimes (i 10)
          (midi-send-at clce (note :pitch �pitch :port 0) (+ curdate (* i 1000)))))
        (sleep dur)))

(defun start-sequence (gparties)
  (print "start-sequence" *standard-output*)
  (setq *parties-process* (mp:process-run-function "parties-process" '(:priority 2) #'loop-sequence gparties)))

(defun loop-sequence (gparties)
  (do () (nil (print "stop"))
    (funcall gparties 0 0 0 0)))

(defun stop-sequence()
  (print "stop-sequence" *standard-output*)
  ;; On positionne une interaction qui ne fait rien
  (change-interaction 'interaction-null)
  ;; On arr�te le Player 
  (StopPlayer *session-mf-player*)
  ;; On arr�te la boucle d'enchainements des parties  
  (mp:process-kill *parties-process*))
 
;; ***************************************************************************     
;;
;; Interface utilisateur
;;
;; ***************************************************************************

(defvar *dialog* nil)

;; Construction de l'interface utilisateur

(defun start-dialog-callback(&rest args)
  (print "D�marrer" *standard-output*)
  ;;(start-sequence (test1 (rnd �60 �72) 20))
  (start-sequence (g-parties (g (floor (/ *duree-sequence* 1000)))))
)

(defun stop-dialog-callback(&rest args)
  (print "Arr�ter" *standard-output*)
  (stop-sequence)
)

;; D�finition du dialogue

(capi:define-interface controler-interface()
  ()
  (:panes

   (start-button
    capi:push-button
    :text "           D�marrer        "
    :callback-type :interface
    :callback 'start-dialog-callback)

   (stop-button
    capi:push-button
    :text "            Arr�ter           "
    :callback-type :interface
    :callback 'stop-dialog-callback)
   )

  (:layouts
    (start-layout
     capi:row-layout
     '(start-button))

    (stop-layout
     capi:row-layout
     '(stop-button))

     (main-layout
      capi:column-layout
       '(start-layout stop-layout))
 
     )

  (:default-initargs
   :title "GreenSounds"
   :X 400 :Y 50 :VISIBLE-WIDTH 400 :VISIBLE-HEIGHT 300  
   :WIDTH 400 :HEIGHT 300
   :layout 'main-layout))

 ;; Cr�ation du dialogue

(defun restart-function ()
  (install-background)
  (init-interaction)
  (unless *dialog* 
    (setq *dialog* (make-instance 'controler-interface))
    (capi:display *dialog*)))

#|
(defun restart-function ()
 (install-background)
 (dotimes (i 10)
   (print 'note)
   (midi-send-at clce (note) (+ (midigettime) (* i 1000))))
 (unless *dialog* 
   (setq *dialog* (make-instance 'controler-interface))
   (capi:display *dialog*)))
|#

;; //////////////////////////////////////////////////////////////////////////////////////////////////////////////////

;;                               CREATION DES VARIABLES GLOBALES 

;; //////////////////////////////////////////////////////////////////////////////////////////////////////////////////


;; //////////////               VARIABLES POUR LE TEMPS DIFFERE (PRORYTHME)



;(MidiTotalSpace)
(MidiGrowSpace 200000)


;; num�ro des groupes (au sens KONTAKT)  port A respectivement canal midi (0-5) et (6-11)

(defvar m1 '(1 2))
(defvar m2 '(1 3))
(defvar m3 '(1 4))
(defvar m4 '(2 3))
(defvar m5 '(2 4))
(defvar m6 '(3 4))
(defvar m7 '(1 5))
(defvar m8 '(2 5))
(defvar m9 '(6 6))
(defvar m10 '(6 3))
(defvar m11 '(6 7))


(defvar *n-gaccord* �(0 1 2 3 4 5))
(defvar *n-gecar* �(0 0 0 0 0 0)) 

; zones g�ographiques (Haut-parleurs)

(defvar *zone1* �(0 1 2))    
(defvar *zone2* �(3 4 5)) 
(defvar *zone3* �(1 2 3))  

(defvar *n-zone1* �(0 1 2))      ;; devant
(defvar *n-zone2* �(2 3 4))      ;; AR droit
(defvar *n-zone3* �(4 5 0))      ;; AR gauche
(defvar *totalzone* �(0 1 2 3 4 5)) ;; partout

; trajectoires sur les hp

(defvar *n-gaccord-demi* �(0 2 4))
(defvar *n-traj-total-direct* �(0 1 2 3 4 5)) 
(defvar *traj-total-direct2* �(5 3 1 2 0 4))
(defvar *traj-zig-zag* �(0 4 2 5 1 3 5 3 4 1 0))    

;; //////////////                                VARIABLES POUR LE TEMPS REEL  (INTERACTION)

(defvar *last-interaction-date* 0)        ;; date de la pr�c�dente interaction
(defvar *mode* t)                         ;; mode entre les jeu-a (t) et jeu-b (nil)
(defvar *ex-val21* -10)                   ;; pr�c�dente valeur des contr�leurs 21 31 32 et 33 (pour filtrer les sauts des capteurs)
(defvar *ex-val31* -10)
(defvar *ex-val32* -10)
(defvar *ex-val33* -10)
(defvar *attente* 5000)                  ;; dur�e au bout de laquelle on red�marre le player si il n'y a pas eu d'interaction
(defvar *delai-replay* 3000)              ;; si le player est stop� le relancer suite � une nouvelle interaction apr�s un temps �gal � *delai-replay*
(defvar *duree-sequence* 360000)          ;; dur�e des s�quences (identique pour toutes les s�quences - EXPRIM� EN ENTIER ET MILLISECONDES)
(defvar *duree-sequence-initiale* 60000)  ;; dur�e d'une s�quence avant arr�t (change pour chaque s�quence)
(defvar *debut-interaction* nil)          ;; t si il y a eu au moins une interaction nil sinon (permet de savoir si il faut arr�ter le player au bout de *duree-sequence-initiale*
(defvar *separation-partie* 10000)        ;; attente entre deux parties


;; Les variables pour l'interaction
(defvar *bleu-a1* �1) ; angles de rotation autour des axes respectivement 1 2 3 4 5 et 6 pour le capteur bleu (pour le jeu a)

(defvar *bleu-b1* �1) ; angles de rotation autour des axes respectivement 1 2 3 4 5 et 6 pour le capteur bleu (pour le jeu b)

(defvar *violet-a1* �1) ; angles de rotation autour des axes respectivement 1 2 3 4 5 et 6 pour le capteur violet (pour le jeu a)
(defvar *violet-a2* �1)
(defvar *violet-a3* �1)

(defvar *violet-b1* �1) ; angles de rotation autour des axes respectivement 1 2 3 4 5 et 6 pour le capteur violet (pour le jeu b)
(defvar *violet-b2* �1)
(defvar *violet-b3* �1)
(defvar *mode* t)       ; jeu a ou jeu b

;; //////////////////////////////////////////////////////////////////////////////////////////////////////////////////

;;                               FONCTIONS POUR LE TEMPS DIFFERE (PRORYTHME)

;; //////////////////////////////////////////////////////////////////////////////////////////////////////////////////


;; ----------------------------  adressage des layers (groupes) d�finis par les listes m-X

(defun place-prog (date prog  sequence)
  (midi-move sequence :date date)
  (dotimes (chan 6)
    (midi-send-im sequence (ctrl-change :ctrl 9 :value (car prog) :chan chan :port 0))
    (midi-send-im sequence (ctrl-change :ctrl 9 :value (cadr prog) :chan (+ 6 chan) :port 0))))



(defun xconcat (n rytm)
  #'(lambda (stime ctime etime reverse) 
      (let ((l) (vrytm �rytm))  
        (dotimes  (i �n) (push  (g vrytm) l)) 
        �(apply #'concat l))))


(defun sel-al (g1 &rest lg)
  #'(lambda (stime ctime etime reverse)
      (let (( val (1+ �g1)))
        �(elt lg (min (1- (length lg)) (random (max 0 val)))))))


;; ----------------------------  Affiche le nom de la s�quence en cours

(defun print-dialog (a)
  (format t "~%**********  fin de prorythme � la date ~S ~S ~%" (/ (date?) noi) a))


; ----------------------------  Pour les trajectoires

(defun traj (gtraj gnum)
  #'(lambda (stime ctime etime reverse)
      (l �gnum (s (s (g (l 1 gtraj)))))))

; ----------------------------  ----------------------------  Pour les groupes

(defun groupe (ggroup gnum gval)
  #'(lambda (stime ctime etime reverse)
      (l �gnum (rep (h (s (g (l 1 ggroup)))) gval))))

; ----------------------------  Retourne une liste de n

(defun nconcat-spe4 (n val)
  #'(lambda (stime ctime etime reverse) 
      (let (l) 
        (dotimes  (i �n) (push  (lst val) l)) 
        �(apply #'concat l))))

;(l 1 (nconcat-spe4 �8 (hs �(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)))) --> ((11 10 4 9 5 1 12 0))


;; ----------------------------  Correction bug interpolation

(defun i (g1 g2)
  #'(lambda (stime ctime etime reverse)
      (let ((v1 (if reverse �g2 �g1))
            (v2 (if reverse �g1 �g2)))
        (if (= etime stime) v1
             (/ (+ (* v1 (- etime ctime)) 
                  (* v2 (- ctime stime))) 
               (- etime stime))))))



;; ----------------------------  D�finition de trajectoires pour les 6 hp

(defun trajectoires ()
  (sel-al �12  
          �(0 1 2 3 4 5) 
          �(0 2 3 1 5 4) 
          �(4 5 2 1 0 3)
          �(4 3 5 2 0 1) 
          �(5 3 2 4 1 0) 
          �(5 2 4 1 0 3)
          �(0 2 4) 
          �(1 3 5) 
          �(5 3 1 0)
          �(5 3 2 0) 
          �(4 2 3 5 1) 
          �(3 1 2 0 4 5)
          �(4 2 3 0 1)))



(defun listgroupe ()
  (sel-al �3  �(6 7 8 9) �(8 9 10 11) �(7 8 9 10) �(6 8 10 11)))





;; //////////////////////////////////////////////////////////////////////////////////////////////////////////////////

;;                               FONCTIONS POUR LE TEMPS REEL (INTERACTION)

;; //////////////////////////////////////////////////////////////////////////////////////////////////////////////////


; ---------------------------- nouveau is-XX (les "classiques" ne fonctionne pas correctement
(defun is-ctrl-change2 (e)
 (= (evtype e) typectrlchange))

(defun is-key-on2 (e)
 (and (= (evtype e) typekeyon)
      (> (vel e) 0)))

; ----------------------------  filtre les petits changement de valeur des contr�leurs 21 31 32 et 33

(defun chge-val? (num val) ;(print (list num val "*******" *ex-val21* *ex-val31* *ex-val32* *ex-val33*))
  (cond
   ((= num 21) (if (<= (abs (- val *ex-val21*)) 2) nil (progn (setq *ex-val21* val) t)))
   ((= num 31) (if (<= (abs (- val *ex-val31*)) 2) nil (progn (setq *ex-val31* val) t)))
   ((= num 32) (if (<= (abs (- val *ex-val32*)) 2) nil (progn (setq *ex-val32* val) t)))
   ((= num 33) (if (<= (abs (- val *ex-val33*)) 2) nil (progn (setq *ex-val33* val) t)))
   (t nil)))




; ----------------------------  �tant compris entre 0 et 127 ram�ne x entre les valeurs v1 et v2 sous forme de g�n�rateur

(defun scale (x v1 v2)  ; g�n�rateur d'entier
  (if (= 0 x) (g v1)
  (g (floor (/ (+ (* v1 (- 127 x)) (* x v2)) 128)))))


(defun scale2 (x v1 v2)  ; entier
  (if (= x 0) v1  ;(g v1)
  (floor (/ (+ (* v1 (- 127 x)) (* x v2)) 128))))

; ----------------------------  �tant compris entre 0 et 127 ram�ne x entre les valeurs v2 et v1 sous forme de g�n�rateur

(defun scale-inv (x v1 v2)  ; g�n�rateur d'entier
  (scale x v2 v1))

; ---------------------------- on bloque le prog pendant une dur�e = delai. ((sleep 1) pour optimiser la boucle do)

(defun wait (delai)
  (let ((t0 (midi-get-time)))
    (do ((t1 (midi-get-time) (midi-get-time)))
        ((> (- t1 t0) delai))
      (sleep 1))))



;; ----------------------------  phase d'alternance entre jeu player et jeu interactif. Sa dur�e = d = (- *duree-sequence* *duree-sequence-initiale*)
;; ----------------------------  remplace l'ancien wait2 (pour les parties de 1 � 11)

(defun phase2 (d)
  (let ((datinit (midi-get-time)))
    (do ((t1 (midi-get-time) (midi-get-time)))
        ((> (- t1 datinit) d)
         (StopPlayer *session-mf-player*)) 
      
      (if (= 0 (s-state *player-state*)) 
          (if (> (- t1 *last-interaction-date*) *attente*)
              (ContPlayer *session-mf-player*))
        (if (< (- t1 *last-interaction-date*) *delai-replay*)
            (StopPlayer *session-mf-player*)))
      (sleep 1))))


;; ---------------------------- cr�ation des param�tres d'interaction pour les deux types de "jeux interactifs": jeu-a et jeu-b
;; ---------------------------- ces variables globales re�oivent les valeurs des controleurs de num�ros 21 pour le capteur bleu
;; ---------------------------- ces variables globales re�oivent les valeurs des controleurs de num�ros respectifs 31 � 33 pour le capteur violet



(defun init-interaction () 

  ;; Installe un processus sur l'entr�e MIDI (avec 'midi-set-rcv-alarm')
  ;; version generique (� �valuer un fois)
  (midi-set-rcv-alarm clce #'(lambda (self)
                               (do ((e (midi-get-ev self) (midi-get-ev self)))
                                   ((nullptrp e))
                                 (progn 
                                   (interaction e)
                                   (midi-free-ev e))))))

;;(init-interaction)


;; Pour activer le MIDI en entr�e (� faire une seule fois)
;(install-background)

;; Evalu�e plus loin
(defun interaction (e))


;; permet de passer en param�tre le nom de la nouvelle interaction
(defun change-interaction (symbol)
  (setf (symbol-function 'interaction) (symbol-function symbol)))

;; (setq interaction interaction-1)

;; Une interaction qui ne fait rien
(defun interaction-null (e))


;; //////////////////////////////////////////////////////////////////////////////////////////////////////////////////

;;                               FONCTIONS DE HAUT NIVEAU

;; //////////////////////////////////////////////////////////////////////////////////////////////////////////////////


(defun g-parties (d)       ;; attention la dur�e unique des s�quences d = *duree-sequence* est � pr�sent un entier exprim� en millisecondes
  (sel (hs �(0 1 2 3 4 5 6 7 8 9 10 11 12))
       (g-partie-1 �0 d)   
       (g-partie-2 �0 d)
       (g-partie-3 �0 d)
       (g-partie-4 �0 d)
       (g-partie-5 �0 d)
       (g-partie-6 �0 d)
       (g-partie-7 �0 d)
       (g-partie-8 �0 d)
       (g-partie-9 �0 d)
       (g-partie-10 �0 d)
       (g-partie-11 �0 d)
       (g-partie-12 �0 d)
       (g-partie-13 �0 d)))


;(defun g-parties (d)       ;; attention la dur�e unique des s�quences d = *duree-sequence* est � pr�sent un entier exprim� en millisecondes
;(print 'g-parties)
;  (alt
;   (g-partie-1 �0 d) 
;   (g-partie-8 �0 d)))  


; (l 1 (g-parties (g (floor (/ *duree-sequence* 1000)))))


;; //////////////////////////////////////////////////////////////////////////////////////////////////////////////////

;;                              PRORYTHME INTERACTIF - Version 2

;; //////////////////////////////////////////////////////////////////////////////////////////////////////////////////


(defun p-interactif (chan r h v n &key (fin �nil) (stac �1) (inter �0))
    (p-inter chan r h v 0 n nil (funcall fin 0 0 1 nil) stac inter))

(defun p-inter (chan r h v stime etime reverse fin stac inter)
  (let ((dat (now))
        (rr))
        (dotimes (ctime etime)
              (setq rr �r)
              (midi-send-at  clce (note :pitch �h  :vel �v :port 0 :chan �chan  :dur (if (and (= (+ 1 ctime) etime) fin) fin (floor (* �stac rr)))) dat)
              (setq dat (+ dat rr �inter)))))


;(p-interactif �6 �2000 �65 �100 4 :fin �1000 :inter �200 :stac �0.1)



;; //////////////////////////////////////////////////////////////////////////////////////////////////////////////////

;;                              VERS_MAX - envoi de donn�es vers MAX pour affichage

;; //////////////////////////////////////////////////////////////////////////////////////////////////////////////////

; envoi de l'info vers MAX sp�cifiant la s�quence en jeu et le type de jeu (jeu-a ou jeu-b) via le port MIDI 2 et canal 1
; les valeurs de hauteur vont de 1 � 24 (1 et 2 pour s�quence 1, 3 et 4 pour s�quence 2 ...)

(defun vers-MAX (h)
  (midi-send-im  clce (note :pitch h  :vel 10 :port 1 :chan 1  :dur �1000)))




;; //////////////////////////////////////////////////////////////////////////////////////////////////////////////////

;;                              12 SEQUENCE-X ET PARTIES-X

;; //////////////////////////////////////////////////////////////////////////////////////////////////////////////////





;; *******************************************************************     S�quence 1 (ex 3) 
;; Jour
;; Multi M2
;; avec fond g�n�ral tenu jour
;; avec trajectoires d'oiseaux
;; avec oiseaux al�atoire non r�p�t�s
;; avec phrase monocanal d'oiseaux
;; *******************************************************************

(defun sequence-1 (deb fin)
  
  (print-dialog "SEQUENCE 1")
  
  ;; ************* prog-change  
  (place-prog 0 m2 *out*)
  (place-tempo 0 60)
  
  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin))))
        (fin-debut (g (floor (/ *duree-sequence-initiale* 1000)))))

    
    ;; ************* fond g�n�ral jour proba 1
    (let ((son-de-fond (funcall (rnd �39 �68) 0 1 0 nil)))

    (prorythme �6 deb2 fin-debut                                             
               (gnotes :laccord *n-gaccord-demi*
                       :lecar *n-gecar* :proba (h �(1)))
               �0
               (lst (sub fin-debut deb2))
               (g son-de-fond) 
               �20)
    (prorythme �6 (add �1 fin-debut) fin2                                             
               (gnotes :laccord *n-gaccord-demi*
                       :lecar *n-gecar* :proba (h �(1)))
               �0
               (h �((4) (6) (8) (10) (12) (14) (20)))
               (g son-de-fond) 
               �20))



  
    ;; ************* Chant r�p�t� d'un oiseau sur un canal
    ;; zone1
    (share-gen n1 (h �( 1 2 2 2 2 3 3 3 4)))
    (prorythme (rep (add �6 (h *totalzone*)) (use-gen n1)) (add deb2 (h �(0 2 4 6 7 8 10))) fin2   ;; d�buts d�call�s al�atoirement
               (gnotes)
               (h �( 5 7 10 11 12 16 20 ))
               (xconcat (use-gen n1)  (sel-al �5  �(1/4) �(1/2) �(1) �(3/2) �(3) �(5)))
               (rep (rnd �69 �125) (use-gen n1)) 
               �90)
    (free-gen n1)
  
    ;; ************* Un son d'oiseau sur canal qq
    (prorythme (rnd �6 �11) (add deb2 (h �(0 2 4 6 7 8 10))) fin2          
               (gnotes)
               (h �(6 7 8 12 16 20))
               (sel-al �4 �(1/2) �(1) �(3/2) �(3) �(5))
               (rnd �69 �100) 
               �90)
  
    ;; ************* Phrase monocanal d'oiseaux 
    ;; zone1
    (share-gen n1 (h �(2 3 4 5 6)))
    (prorythme (rep (add �6 (h *totalzone*)) (use-gen n1)) (add deb2 (h �(0 1 2 3 4))) fin2
               (gnotes :proba (h �(1 1 1 1 1 0)))
               (h �(5 9 13 15 20 24))
               (concat �(1/2) (nconcat (sub (use-gen n1) �2)  �(1/8)) �(4)) 
               (rep (rnd �79 �100) (h �(1 1 1 2 3)))  
               �90)
    (free-gen n1)
   
    ;; ************* trajectoires al�atoires d'oiseaux al�atoires ;; 
    ;; ************* soit des trajectoires de 12, 15 ou 18 points
    (share-gen ntraj (h �(4 5 6)))
    (prorythme (add �6 (s (traj (trajectoires) (mult �3 (use-gen ntraj))))) (add deb2 (h �(0 2 4 6 7 8 10))) fin2  
               (gnotes)                                                                 
               (h �(8 16 32))
               (xconcat (use-gen ntraj)  �(2/3c(1/2 1/2 1/2)) )                              
               (rep (rnd �79 �125) (mult �3 (use-gen ntraj))) 
               �100)
    (free-gen ntraj)))

#|

(defun partie-1 (deb fin)
  (init)
  (sequence-1 deb fin)
  (player))

(partie-1 �0 �80)

|#




#|
////////////////////////////////////////////////////////////////////////////////////////////////////////////////// 1
                                 
Jeu interactif 1:
      jeu-a: oiseau
      jeu-b: guiro

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
|#



(defun init-interaction-1 ()

  ;--------------- jeu a
  (setq *bleu-a1* 2)          ;ctrl 21 --> nombre de r�p�tition de l'�chantillon 

  (setq *violet-a1* �70)      ;ctrl 31 --> hauteur de l'�chantillon r�p�t� 
  (setq *violet-a2* �500)     ;ctrl 32 --> dur�e et interval temporel de chaque �chantillon
  (setq *violet-a3* �8)       ;ctrl 33 --> canal de l'�chantillon r�p�t�

  ;----------------jeu b
  (setq *bleu-b1* 4)          ;ctrl 21 --> nombre de r�p�tition de l'�chantillon 

  (setq *violet-b1* �30)      ;ctrl 31 --> hauteur de l'�chantillon r�p�t� 
  (setq *violet-b2* �300)     ;ctrl 32 --> dur�e et interval temporel de chaque �chantillon
  (setq *violet-b3* �1)       ;ctrl 33 --> canal de l'�chantillon r�p�t� ou d�but de la trajectoire

  (setq *mode* t)
  (setq *debut-interaction* nil)
  )


(defun jeu-a1 (num val)   ;; oiseau

  (cond ((= num 21) (setq *bleu-a1* (scale2 val 1 6)))       ;nbre d'�v�nement
        ((= num 31) (setq *violet-a1* (scale val 69 125)))  ;type d'oiseau
        ((= num 32) (setq *violet-a2* (sel (scale-inv val 0 10) �100 �200 �300 �400 �500 �600 �700 �800 �900 �1000))) ;dur�e chaque oiseau
        ((= num 33) (setq *violet-a3* (scale val 6 12))))    ; canal de diffusion

  (p-interactif *violet-a3* *violet-a2* *violet-a1* �120 *bleu-a1* :fin �400))



(defun jeu-b1 (num val)   ;; guiro

  (cond ((= num 21) (setq *bleu-b1* (scale2 val 1 6)))     ; nbre d'�v�nements
        ((= num 31) (setq *violet-b1* (scale val 21 45)))  ; type d'�v�nement
        ((= num 32) (setq *violet-b2* (sel (scale-inv val 0 7) �50 �100 �200 �250 �300 �400 �500))) ; dur�e de chaque �v�nement
        ((= num 33) (setq *violet-b3* (scale val 0 6))))  ; canal de d�part de tralectoire circulaire dans un sens ou l'autre

  (p-interactif (gmod (add *violet-b3* (rdv �+ �1)) �6) *violet-b2* *violet-b1* �120 *bleu-b1* :fin �1200))



; si on filtre les contr�leurs dans MAX (cad suppression ici du chge-val?) il faut alors r�tablir le test sur les num de contr�leurs

(defun interaction-1 (e) 
  (when (is-ctrl-change2 e)
    (let ((num-ctrl (ctrl e))
          (val (valint e))
          (dat (date e)))
      
      (if (= num-ctrl 15) (progn (setq *mode* (not *mode*)) (vers-MAX (if *mode* 1 2))) 
        ;(when (and (> dat *last-interaction-date*) (or (= num-ctrl 21) (= num-ctrl 31) (= num-ctrl 32) (= num-ctrl 33)) (chge-val? num-ctrl val)) 
          (when (and (> dat *last-interaction-date*) (chge-val? num-ctrl val)) 

          (setq *debut-interaction* t) ;; pour savoir si une interaction a eu lieu

          (if *mode*
              (jeu-a1 num-ctrl val)  ;oiseau monocanal
            (jeu-b1 num-ctrl val))   ;guiro trajectoire
          (setq *last-interaction-date* (+  dat 1000)))))))






;----------------------------


(defun partie-1 (deb fin) 
  (vers-MAX 1)
  (init)
  (init-interaction-1)

  ;; change le principe d'interaction
  (change-interaction 'interaction-1)

  ;; calcul de la s�quence et activation du player
  (sequence-1 deb fin) 
  (player)
    
  ;; interaction pendant la dur�e *duree-sequence-initiale* et avec player (phase 1)    
  (wait *duree-sequence-initiale*)

  ;; stoper le player si il y a eu une interaction pendant la phase 1
  (if *debut-interaction*
      (progn (StopPlayer *session-mf-player*) 
        (sleep 1))) ; sleep 1 sec pour laiser le temps � (s-state *player-state*) de passer � 0
   

  ;; phase 2: player ou jeu interactif (exclusif)
  (phase2 (- *duree-sequence* *duree-sequence-initiale*))

  ;; stoper le player si ce n'est d�j� fait et attendre un peu avant de relancer la prochaine s�quence
  (StopPlayer *session-mf-player*)
  (wait *separation-partie*))



;--------------------



;(partie-1 �0 (g (floor (/ *duree-sequence* 1000))))


(defun g-partie-1 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore �stime ctime etime reverse))
      (progn
        (partie-1 deb fin))))








;; *******************************************************************     S�quence 2 (ex 4) 
;; Guiro
;;          Multi M2
;; avec fond g�n�ral nuit (proba 1)
;; Guiro r�p�t� monocanal
;; Insectes et oiseaux al�atoire r�p�t� de 1 � 3 fois sur m�me canal
;; Phrase monocanal de faux insectes
;; *******************************************************************



(defun sequence-2 (deb fin) 
  
  (print-dialog "SEQUENCE 2")
  
  ;; ************* prog-change 

  (place-prog 0 m2 *out*)
  (place-tempo 0 60)
  
  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin))))
        (fin-debut (g (floor (/ *duree-sequence-initiale* 1000)))))

    ;; ************* fond g�n�ral nuit proba 1
    (let ((fond (funcall (rnd �21 �38) 0 1 0 nil)))

    (prorythme �6 deb2 fin-debut
               (gnotes :laccord *n-gaccord-demi* 
                       :lecar *n-gecar* :proba �1) 
               �0
               (lst (sub fin-debut deb2))
               (g fond) 
               �20)

    (prorythme �6 (add �2 fin-debut) fin2
               (gnotes :laccord *n-gaccord-demi* 
                       :lecar *n-gecar* :proba �1) 
               �0
               (h �((4) (6) (8) (10) (12) (14) (20)))
               (g fond) 
               �20))


  
    ;; ************* Guiro r�p�t� sur un canal; 4 rythmes diff�rents
    (share-gen n1 (h �( 1 2 2 2 2 3 3 3 4 5)))
    (prorythme (rep (h *totalzone*) (use-gen n1)) (add deb2 (h �(0 1 2 3 4 5 6 8 10))) fin2
               (gnotes)
               (h �(0 4 8 8 12 16))
               (xconcat (use-gen n1)  (sel-al �3  �(1/4) �(1/2) �(3) �(5)))
               (rep (rnd �21 �68) (use-gen n1)) 
               �90)
    (free-gen n1)

  
    ;; ************* un peu d'oiseaux et d'insectes al�atoires r�p�t�s 1 ou 2 ou 3 fois sur un m�me canal
    ;; zone1
    (share-gen n1 (h �( 1 2 3)))
    (prorythme (rep (add �6 (h *totalzone*)) (use-gen n1)) (add deb2 (h �(0 1 2 3 4 5 6 8 10))) fin2
               (gnotes)                                                                  
               (h �(0 4 8 8 12 16))
               (xconcat (use-gen n1)  (sel-al �4 �(1/2) �(1) �(3/2) �(3) �(5)))
               (rep (rnd �69 �83) (use-gen n1))   
               (rep (h �(100 70 90 80)) (use-gen n1)))
    (free-gen n1)

    ;; ************* phrase monocanal Guiro m�me rythme + - long 
    ;; zone1
    (share-gen n1 (h �(6 7 8 9 10 11 12)))
    (prorythme (rep (h *totalzone*) (use-gen n1)) (add deb2 (h �(0 1 2 3 4 5 6 8 10))) fin2
               (gnotes :proba (h �(1 1 1 1 1 0)))
               (h �(0 5 9 11 13 15))
               (concat �(1/2) (nconcat (sub (use-gen n1) �2)  �(1/8)) �(4)) 
               (rep (rnd �21 �68) (h �(1 1 1 2 3 ))) 
               �90)
    (free-gen n1)))



#|

(defun partie-2 (deb fin)
  (init)
  (sequence-2 deb fin)
  (player))


(partie-2 �0 (rnd �60 �120))

////////////////////////////////////////////////////////////////////////////////////////////////////////////////// 2

Jeu interactif 2:
      jeu-a: oiseau
      jeu-b: guiro

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
|#



(defun init-interaction-2 ()

  ; jeu a
         
  (setq *bleu-a1* 1)     ;ctrl 21 --> nombre de r�p�tition de l'�chantillon 

  (setq *violet-a1* �70)     ;ctrl 31 --> hauteur de l'�chantillon r�p�t� 
  (setq *violet-a2* �400)   ;ctrl 32 --> dur�e et interval temporel de chaque �chantillon
  (setq *violet-a3* �7)       ;ctrl 33 --> canal de l'�chantillon 

  ; jeu b
           
  (setq *bleu-b1* 1)    ;ctrl 21 --> nombre de r�p�tition de l'�chantillon 

  (setq *violet-b1* �30)      ;ctrl 31 --> hauteur de l'�chantillon r�p�t� 
  (setq *violet-b2* �200)     ;ctrl 32 --> dur�e et interval temporel de chaque �chantillon
  (setq *violet-b3* �3)        ;ctrl 33 --> canal de l'�chantillon r�p�t� ou d�but de la trajectoire

  (setq *mode* t)
  (setq *debut-interaction* nil))




(defun jeu-a2 (num val)         ;; oiseau

  (cond ((= num 21) (setq *bleu-a1* (scale2 val 1 8)))            ; nbre d'oiseau
        ((= num 31) (setq *violet-a1* (scale val 69 125)))        ; type d'oiseau
        ((= num 32) (setq *violet-a2* (sel (scale-inv val 0 10) �100 �200 �300 �400 �500 �600 �700 �800 �900 �1000)))  ;dur�e de chaque �v�nement
        ((= num 33) (setq *violet-a3* (scale val 6 12))))         ; canal

  (p-interactif *violet-a3* *violet-a2* *violet-a1* �120 *bleu-a1* :fin �1200))

       
(defun jeu-b2 (num val)        ;; guiro

  (cond ((= num 21) (setq *bleu-b1* (scale2 val 1 8)))          ; nbre d'�v�nement
        ((= num 31) (setq *violet-b1* (scale val 21 45)))       ; type de guiro
        ((= num 32) (setq *violet-b2* (sel (scale-inv val 0 7) �50 �100 �200 �250 �300 �400 �500)))   ; dur�e de chaque �v�nement
        ((= num 33) (setq *violet-b3* (scale val 0 6))))        ; d�but d'une trajectoire circulaire dans un sens ou dans l'autre

  (p-interactif (gmod (add *violet-b3* (rdv (rep (h �(+ -)) �10) �1)) �6) *violet-b2* *violet-b1* �120 *bleu-b1* :fin �1200))


(defun interaction-2 (e) 
  (when (is-ctrl-change2 e)
    (let ((num-ctrl (ctrl e))
          (val (valint e))
          (dat (date e)))
      
      (if (= num-ctrl 15) (progn (setq *mode* (not *mode*)) (vers-MAX (if *mode* 3 4))) 
          (when (and (> dat *last-interaction-date*) (chge-val? num-ctrl val)) 

          (setq *debut-interaction* t) 

          (if *mode*
              (jeu-a2 num-ctrl val)  ;oiseau monocanal
            (jeu-b2 num-ctrl val))   ;guiro trajectoire
          (setq *last-interaction-date* (+  dat 1000)))))))




;----------------------------

(defun partie-2 (deb fin) 
  (vers-MAX 3)
    (init)
    (init-interaction-2)

    (change-interaction 'interaction-2)

    (sequence-2 deb fin) 
    (player)
    
    (wait *duree-sequence-initiale*)

    (if *debut-interaction*
        (progn (StopPlayer *session-mf-player*) 
          (sleep 1))) 
   
    (phase2 (- *duree-sequence* *duree-sequence-initiale*))

    (StopPlayer *session-mf-player*)
    (wait *separation-partie*))




;----------------------------

;(partie-2 �0 (g (floor (/ *duree-sequence* 1000))))

(defun g-partie-2 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore �stime ctime etime reverse))
      (progn
        (partie-2 deb fin))))




;; *******************************************************************     S�quence 3 (ex 6) 
;; Percu Castagnet
;;          Multi M2
;; avec fond g�n�ral nuit (proba 2/3)
;; avec insectes al�atoire
;; avec phrase monocanal de percu castagnet
;; avec phrase monocanal de guiro
;; *******************************************************************


(defun sequence-3 (deb fin) 
  
  (print-dialog "SEQUENCE 3")
  
  ;; ************* prog-change 
  (place-prog 0 m2 *out*)
  (place-tempo 0 60)
  
  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin))))
        (fin-debut (g (floor (/ *duree-sequence-initiale* 1000)))))

 
    ;; ************************************* fond g�n�ral de nuit (proba 3/4)
    (let ((fond (funcall (rnd �21 �38) 0 1 0 nil)))
               
    (prorythme �6 deb2 fin-debut
               (gnotes :laccord *n-gaccord*
                       :lecar *n-gecar*
                       :proba (h �(1 1 1 0))) 
               �0
               (lst (sub fin-debut deb2))
               (g fond) 
               �20)

    (prorythme �6 (add �2 fin-debut) fin2
               (gnotes :laccord *n-gaccord*
                       :lecar *n-gecar*
                       :proba (h �(1 1 1 0))) 
               �0
               (h �((4) (6) (8) (10) (12) (14) (20)))
               (g fond) 
               �20))


  
    ;; ************************************* percus castagnets r�p�t�es sur un canal; zone1
    (share-gen n1 (h �( 1 2 2 2 2 3 3 3 4 5)))
    (prorythme (rep (h *totalzone*) (use-gen n1)) (add deb2 (h �(0 1 2 3 4 5 6 8 10))) fin2
               (gnotes)
               (h �(0 4 8 10 12 20))
               (xconcat (use-gen n1)  (sel-al �3  �(1/4) �(1/2) �(3) �(5)))
               (rep (rnd �97 �110) (use-gen n1)) 
               �80)
    (free-gen n1)

  
    ;; ************************************* un peu d'insectes al�atoires
    (prorythme (rnd �6 �11) (add deb2 (h �(0 1 2 3 4 5 6 8 10))) fin2               
               (gnotes)
               (h �(0 4 4 6 8 8))
               (sel-al �6  �(1/4) �(1/2) �(1) �(3/2) �(3) �(5) �(-4))
               (rnd �69 �78) 
               (h �(100 70 80 90 80)))
  
    ;; ************************************* phrase monocanal ou multicanal (proba 1/2) de percu castagnet  
    ;; zone1
    (share-gen n1 (h �(8 9 10 11 12 13 14 15 16 17 18)))
    (prorythme (sel (rep (h �(0 1)) (use-gen n1)) (rep (h *totalzone*) (use-gen n1)) (h *totalzone*))  (add deb2 (h �(0 1 2 3 4 5 6 8 10))) fin2
               (gnotes :proba (h �(1 1 1 1 1 0)))
               (h �(9 13 20 20 30 30))
               (concat �(1/2) (nconcat (sub (use-gen n1) �2)  �(1/8)) �(4)) 
               (rep (rnd �97 �110) (h �(1 1 1 2 3)))
               �100)
    (free-gen n1)


    ;; ************************************* phrase multicanal de guiro
    ;; zone1
    (share-gen n1 (h �(12 13 14 15 16 17 18)))
    (prorythme (h *totalzone*) (add deb2 �10 (h �(0 1 2 3 4 5 6 8 10))) fin2
               (gnotes :proba (h �(1 1 1 1 1 0)))
               (h �(9 13 15 20 20 25 30))
               (concat �(1/2) (nconcat (sub (use-gen n1) �2)  �(1/8)) �(4)) 
               (rep (rnd �21 �68) (h �(1 1 1 2 3)))  
               �100)
    (free-gen n1)))


#|

(defun partie-3 (deb fin)
  (init)
  (sequence-3 deb fin)
  (player))



(partie-3 �0 (rnd �60 �120))
|#

#|
////////////////////////////////////////////////////////////////////////////////////////////////////////////////// 3
                                      interaction
Jeu interactif 3:
      jeu-a: percu/catagnets
      jeu-b: guiro 

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
|#

(defun init-interaction-3 ()

  ; jeu a
  (setq *bleu-a1* 4)                      ;ctrl 21 --> nombre de r�p�tition de l'�chantillon 
  (setq *violet-a1* �100)                 ;ctrl 31 ---> hauteur de l'�chantillon r�p�t�
  (setq *violet-a2* �100)                 ;ctrl 32 --> dur�e et interval temporel de chaque �chantillon
  (setq *violet-a3* �4)                   ;ctrl 33 --> nombre en canal mono ou en trajectoire al�atoire

  ; jeu b
  (setq *bleu-b1* 2)                      ;ctrl 21  --> nombre de r�p�tition de l'�chantillon 
  (setq *violet-b1* �30)                  ;ctrl 31 --> hauteur de l'�chantillon r�p�t� 
  (setq *violet-b2* �100)                 ;ctrl 32 --> dur�e et interval temporel de chaque �chantillon
  (setq *violet-b3* �3)                   ;ctrl 33 --> nombre de r�p�tition d'un m�mecanal 

  (setq *mode* t)
  (setq *debut-interaction* nil))






  
;; ************************************* phrases monocanals ou trajectoire de percu castagnet  
(defun jeu-a3 (num val)

  ( if (= num 21) (setq *bleu-a1* (scale2 val 1 11)))      ; nombre max 10 �v�nement
  ( if (= num 31) (setq *violet-a1* (scale val 1 8)))      ; nbre de r�p�tititon de la hauteur
  ( if (= num 32) (setq *violet-a2* (scale-inv val 100 300)))  ; dur�e et interval temporel de chaque �chantillon
  ( if (= num 33) (setq *violet-a3* (scale val 8 18)))     ; nbre �chantillons en canal mono ou en trajectoire al�atoire

  (p-interactif (sel (rep (h �(0 1)) *violet-a3*) (rep (h *totalzone*) *violet-a3*) (h *totalzone*))
                (pf �500 *violet-a2*) 
                (rep (rnd �97 �125) *violet-a1*) 
                (sel (h �(0 0 0 0 0 0 0 0 1 1 2)) �80 �110 �127) *bleu-a1* :fin �1200))


;; ************************************* phrases monocanals de guiro
(defun jeu-b3 (num val)

  ( if (= num 21) (setq *bleu-b1* (scale2 val 1 8)))      ; nombre max 7 �v�nement
  ( if (= num 31) (setq *violet-b1* (scale val 1 10)))    ; nbre de r�p�tititon de la hauteur
  ( if (= num 32) (setq *violet-b2* (scale-inv val 4 12)))    ; nbre de r�p�tition d'une m�me dur�e parmi 4
  ( if (= num 33) (setq *violet-b3* (scale val 2 8)))     ; nombre de r�p�tition d'un m�me canal mono

  (p-interactif (rep (h *totalzone*) *violet-b3*)
                (rep (sel (h �(0 0 0 1 2 3))  �125 �250 �425 �500) *violet-b2*) 
                (rep (rnd �21 �68) *violet-b1*) 
                (sel (h �(0 0 0 0 0 0 0 0 1 1 2)) �70 �110 �127) *bleu-b1* :fin �500))




(defun interaction-3 (e) 
  (when (is-ctrl-change2 e)
    (let ((num-ctrl (ctrl e))
          (val (valint e))
          (dat (date e)))
      
      (if (= num-ctrl 15) (progn (setq *mode* (not *mode*)) (vers-MAX (if *mode* 5 6))) 

          (when (and (> dat *last-interaction-date*) (chge-val? num-ctrl val)) 

          (setq *debut-interaction* t) 
                    

          (if *mode*
              (jeu-a3 num-ctrl val)  ;percu catagnette monocanal ou trajectoire
            (jeu-b3 num-ctrl val))   ;guiro trajectoire
          (setq *last-interaction-date* (+  dat 1000)))))))




;----------------------------

(defun partie-3 (deb fin) 
  (vers-MAX 5)
    (init)
    (init-interaction-3)

    (change-interaction 'interaction-3)

    (sequence-3 deb fin) 
    (player)
    
    (wait *duree-sequence-initiale*)

    (if *debut-interaction*
        (progn (StopPlayer *session-mf-player*) 
          (sleep 1))) 
   
    (phase2 (- *duree-sequence* *duree-sequence-initiale*))

    (StopPlayer *session-mf-player*)
    (wait *separation-partie*))


;----------------------------


;(partie-3 �0 (g (floor (/ *duree-sequence* 1000))))


(defun g-partie-3 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore �stime ctime etime reverse))
      (progn
        (partie-3 deb fin))))




;; *******************************************************************     S�quence 4 (ex 10) 
;; Voix principales
;;  M7
;; avec phrase de voix + percu et/ou oiseaux, �clat�e sur des groupes d'enceintes.
;; avec oiseaux rares
;; avec voix rares
;; *******************************************************************


(defun sequence-4 (deb fin) 
  
  (print-dialog "SEQUENCE 4")
  
  
  ;; ************* prog-change 
  (place-prog 0 m7 *out*)
  (place-tempo 0 60)
  
  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))
  
    ;; ************** phrase de voix + percu et/ou oiseaux, �clat�e sur des groupes d'enceintes. 

    (share-gen n1 (h �(12 13 14 15 16 17 18 20 28))) ;; nbre de sons par groupe d'enceinte
    (n-prorythme  deb2 fin2 (h �(3 4 5 6 7)) 
                    
                  ;; ***************** voix + percu
                    
                  (sel-al �4  

                          (n-gnotes (s (groupe (listgroupe) (add �1 (use-gen n1)) (h �(3 4 5 6))))
                                    (concat (nconcat (use-gen n1)  
                                                     (h �((1/8) (1/8) (1/8) (1/8) (1/8) (1/8) (1/8) (1/8) (-1/2)))) �(3/4))
                                    (mem (rnd �21 �50) (h �(4 3 2)) �5)
                                    �80
                                    :laccord �(0 -6) 
                                    :lecar (lst �0 (rep (h �(48 75)) (add �1 (use-gen n1) ))) :proba �0)  ;-------> probabilit� nulle (remplac� par interactif)
                            
                          ;; ***************** voix + percu ou oiseau
                          (n-gnotes (s (groupe (listgroupe) (add �1 (use-gen n1) ) (h �(3 4 5 6))))
                                    (concat (nconcat (use-gen n1)  
                                                     (h �((1/4) (1/4) (1/4) (1/4) (1/4) (1/4) (1/4) (1/4) (-1/2)))) �(3/4))
                                    (mem (rnd �21 �50) (h �(4 3 2)) �5)
                                    �80
                                    :laccord (lst �0 (rep (h �(0 -6)) (add �1 (use-gen n1) ))) 
                                    :lecar (lst �0 (rep (h �(48 75)) (add �1 (use-gen n1) ))) :proba �0) ;-------> probabilit� nulle (remplac� par interactif)
                            
                          ;; ***************** voix + oiseaux/insectes
                          (n-gnotes (s (groupe (listgroupe) (add �1 (use-gen n1) ) (h �(3 4 5 6))))
                                    (concat (nconcat (use-gen n1)  
                                                     (h �((1/2) (1/2) (1/2) (1/2) (1/2) (1/2) (1/2) (1/4) (-1/2)))) �(3/4))
                                    (mem (rnd �21 �50) (h �(4 3 2)) �5)
                                    �80
                                    :laccord �(0 0) :lecar (lst �0 (rep (h �(48 75)) (add �1 (use-gen n1)))) :proba (h �(0 1 1))) ; ---> ajout de proba
                            
                          ;; ***************** voix + percu et oiseau m�lang�s
                          (n-gnotes (s (groupe (listgroupe) (add �1 (use-gen n1) ) (h �(3 4 5 6))))
                                    (concat (nconcat (use-gen n1)  
                                                     (h �((1/4) (1/4) (1/4) (1/4) (1/4) (1/4) (1/4) (1/4) (-1/2)))) �(3/4))
                                    (mem (rnd �21 �50) (h �(4 3 2)) �5)
                                    �80
                                    :laccord (lst �0 (h �(0 -6))) :lecar (lst �0 (h �(48 75))) :proba (h �(0 1 1))) ; ---> ajout de proba
                            
                          ;; ***************** un seul son voix + percu synchro sur toutes les enceintes
                          (n-gnotes �0
                                    (sel-al �3  �(1/4) �(1/2) �(3) �(5))
                                    (rnd �21 �50)
                                    �50
                                    :laccord �(0 1 2 3 4 5) 
                                    :lecar �(48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48
                                                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))
    (free-gen n1)

    ;; ************* voix rares al�atoires
    (prorythme (rnd �6 �11) (add deb2 (h �(0 1 2 4 6 7 8 10))) fin2                       
               (gnotes)
               (h �(6 8 12 14 15 16))
               (sel-al �5  �(1/4) �(1/2) �(1) �(3/2) �(3) �(5))
               (rnd �21 �50) 
               (rep (h �(70 80 90 60))  (s �(1 2 3 4))))
  
    ;; ************* voix rares al�atoires r�p�t�es
    (prorythme (rep (rnd �6 �11) (s �(1 2 3 4))) (add deb2 (h �(0 1 2 4 6 7 8 10))) fin2                       
               (gnotes)
               (h �( 6 8 12 14 16))
               �(1/2)
               (rep (rnd �21 �50) (s �(1 2 3 4)) )
               (rep (h �(70 80 90 60))  (s �(1 2 3 4))))
  
    ;; ************* oiseaux rares al�atoires
    (prorythme (rnd �6 �11) (add deb2 (h �(0 1 2 4 6 7 8 10))) fin2                       
               (gnotes)
               (h �( 4 4 6 8 12))
               (sel-al �5  �(1/4) �(1/2) �(1) �(3/2) �(3) �(5))
               (rnd �69 �125) 
               (h �(100 110 90 80)))))

#|

(defun partie-4 (deb fin)
  (init)
  (sequence-4 deb fin)
  (player))


(partie-4 �0 (rnd �60 �120))

////////////////////////////////////////////////////////////////////////////////////////////////////////////////// 4
 
Jeu interactif 4:
      jeu-a: percus r�p�t�s monocanal sur un rythme choisi
      jeu-b: trajectoires circulaires voix + percussions

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
|#


(defun init-interaction-4 ()

  ; jeu a
  (setq *bleu-a1* 10)                 ;ctrl 21 --> nombre de r�p�tition de l'�chantillon 
  (setq *violet-a1* �67)              ;ctrl 31 --> hauteur de l'�chantillon r�p�t�
  (setq *violet-a2* �4)               ;ctrl 32 --> nbre de dur�e rythmique de m�me valeur parmi 3
  (setq *violet-a3* �4)               ;ctrl 33 --> nombre de r�p�tition de l'�chantillon sur un m�me canal choisi al�atoirement

  ; jeu b
  (setq *bleu-b1* 5)                  ;ctrl 21  --> nombre de r�p�tition de l'�chantillon 
  (setq *violet-b1* �30)              ;ctrl 31 --> hauteur de l'�chantillon r�p�t� 
  (setq *violet-b2* �4)               ;ctrl 32 --> nbre de dur�e rythmique de m�me valeur
  (setq *violet-b3* �0)               ;ctrl 33 --> canal de d�part d'un trajectoire circulaire

  (setq *mode* t)
  (setq *debut-interaction* nil))



  
;; ************************************* percus r�p�t�es monocanal



(defun jeu-a4 (num val)
  (cond ((= num 21) (setq *bleu-a1* (scale2 val 1 11)))          ;ctrl 21 --> nombre de r�p�tition de l'�chantillon 
        ((= num 31) (setq *violet-a1* (scale val 60 124)))       ;ctrl 31 --> hauteur de l'�chantillon r�p�t� 
        ((= num 32) (setq *violet-a2* (scale-inv val 2 8)))          ;ctrl 32 --> nbre de dur�e rythmique de m�me valeur parmi 3
        ((= num 33) (setq *violet-a3* (scale val 1 8))))         ;ctrl 33 --> nombre de r�p�tition de l'�chantillon sur un m�me canal

  (p-interactif (rep (rnd �0 �5)  *violet-a3*)
                (rep (sel (h �(0 0 0 1 1 2)) �100 �200 �300) *violet-a2*)  
                *violet-a1* 
                (sel (h �(0 0 0 0 0 0 0 0 1 1 2)) �80 �110 �127) *bleu-a1* :fin �750))



;; ************************************* trajectoires circulaires voix + percussions



(defun jeu-b4 (num val)

  (cond ((= num 21) (setq *bleu-b1* (scale2 val 1 10)))         ;ctrl 21 --> nombre de r�p�tition de l'�chantillon 
        ((= num 31) (setq *violet-b1* (scale val 21 51)))       ;ctrl 31 --> hauteur de l'�chantillon r�p�t� 
        ((= num 32) (setq *violet-b2* (scale-inv val 1 5)))         ;ctrl 32 --> nbre de dur�e rythmique de m�me valeur 
        ((= num 33) (setq *violet-b3* (scale val 0 5))))        ;ctrl 33 --> canal de d�part d'un trajectoire circulaire
  
  (share-gen chan (add �6 (gmod (add *violet-b3* (rdv �+ �1)) �6)))
  (share-gen dur (rep (sel (h �(0 1 1 2 2 3))  �500 �125 �375 �166) *violet-b2*))

  (p-interactif (use-gen chan)
                (use-gen dur)  
                *violet-b1* 
                �80 *bleu-b1* :fin �750)
  (p-interactif (use-gen chan)
                (use-gen dur)  
                (add *violet-b1* (rep (h �(48 75)) �4))
                �90 *bleu-b1* :fin �500)
  (free-gen chan)
  (free-gen dur))




(defun interaction-4 (e) 
  (when (is-ctrl-change2 e)
    (let ((num-ctrl (ctrl e))
          (val (valint e))
          (dat (date e)))
      
      (if (= num-ctrl 15) (progn (setq *mode* (not *mode*)) (vers-MAX (if *mode* 7 8))) 
          (when (and (> dat *last-interaction-date*) (chge-val? num-ctrl val)) 

          (setq *debut-interaction* t) 
                   

          (if *mode*
              (jeu-a4 num-ctrl val)  ;percu
            (jeu-b4 num-ctrl val))   ;voix
          (setq *last-interaction-date* (+  dat 1000)))))))




;----------------------------

(defun partie-4 (deb fin) 
  (vers-MAX 7)
    (init)
    (init-interaction-4)

    (change-interaction 'interaction-4)

    (sequence-4 deb fin) 
    (player)
    
    (wait *duree-sequence-initiale*)

    (if *debut-interaction*
        (progn (StopPlayer *session-mf-player*) 
          (sleep 1)))
   
    (phase2 (- *duree-sequence* *duree-sequence-initiale*))

    (StopPlayer *session-mf-player*)
    (wait *separation-partie*))
;----------------------------


;(partie-4 �0 (g (floor (/ *duree-sequence* 1000))))


(defun g-partie-4 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore �stime ctime etime reverse))
      (progn
        (partie-4 deb fin))))






;; *******************************************************************     S�quence 5 (ex 11) 
;; Cloches (rares)
;;                    Multi M4
;; phrase �clat�e (par groupe) de triangles
;; triangles aigus r�p�t�s
;; insectes rares al�atoires
;; *******************************************************************


(defun sequence-5 (deb fin) 
  
  (print-dialog "SEQUENCE 5")
  
  ;; ************* prog-change 
  (place-prog 0 m4 *out*)
  (place-tempo 0 60)


  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))
  
    ;; ************************************* phrase �clat�e (par groupe) de triangles 
    (share-gen n1 (h �(6 7 8 9 10 11))) ;; nbre de sons par groupe d'enceinte 
      (prorythme (sub (s (groupe (listgroupe) (use-gen n1) (h �(1 2 3)))) �6) deb2 fin2
                 (gnotes :stac �3)
                 (h �(8 16 32 24))
                 (concat �(1/2) (nconcat (sub (use-gen n1) �2)  (sel-al �6 �(1) �(2) �(2/3n (2 1)) �(3) �(1/4) �(1/4) �(1/4))) �(10))
                 (rep (rnd �21 �83) (h �(1 1 1 1 1 2 2 3 3 3 4)))
                 (h �( 60 70 90 110 10)))
      (free-gen n1)
  
        ;; ************************************* triangles graves r�p�t�s global r�p�t� sur m�me canal
    (share-gen n1 (h �(3 4 5 6 7)))
      (prorythme (rep (rnd �0 �5) (use-gen n1)) (add deb2 (h �(0 1 3 5 7 9 11))) fin2
                 (gnotes :stac �3)
                 (h �(16 32 24 28))
                 (concat (nconcat (sub (use-gen n1) �1)  
                                  (rep (h �((1) (3/2) (2/3b (2 2 2)) (2) (3))) (sub (use-gen n1) �1)))  �(10))
                 (rep (rnd �33 �60) (h �(7 14 21)))
                 �90)
      (free-gen n1)

      ;; ************************************* triangles graves multicanal r�p�t�s par zone
    (share-gen n1 (h �(3 4 5 6 7)))
      (prorythme (sel (rep (h �(0 0 1 1 2)) (use-gen n1)) (h *n-zone1*) (h *n-zone2*) (h *n-zone2*)) (add deb2 (h �(0 1 3 5 7 9 11))) fin2
                 (gnotes :stac �3)
                 (h �(16 12 24 28))
                 (concat (nconcat (sub (use-gen n1) �1)  
                                  (rep (h �((1) (3/2) (2/3b (2 2 2)) (2) (3))) (sub (use-gen n1) �1)))  �(10))
                 (rep (rnd �33 �60) (h �(7 14 21)))
                 �90)
      (free-gen n1)
 
    ;; ************************************* un peu de "tenu" jour / nuit br�ves al�atoires synchro 
    (prorythme �6 (add deb2 (h �(0 1 2 4 6 7 8 10))) fin2                                        
               (gnotes :laccord *n-gaccord* 
                       :lecar *n-gecar*)
               (h �( 4 8 20 20))
               (sel-al �3  �(8) �(3) �(4) �(6))
               (rnd �21 �68) 
               �30)
  
  
    ;; ************************************* insectes rares al�atoires
    ;; zone1
    (prorythme (add �6 (h *zone1*)) (add deb2 (h �(0 1 2 4 6 7 8 10))) fin2                       
               (gnotes)
               (h �(5 7 9 12 14))
               (sel-al �7  �(1/4) �(1/2) �(1/2) �(1/2) �(1) �(3/2) �(3) �(5))
               (rnd �69 �78) 
               �90)

       ;; zone2
    (prorythme (add �6 (h *zone2*)) (add deb2 (h �(0 1 2 4 6 7 8 10))) fin2                       
               (gnotes)
               (h �(5 7 9 12 14))
               (sel-al �7  �(1/4) �(1/2) �(1/2) �(1/2) �(1) �(3/2) �(3) �(5))
               (rnd �69 �78) 
               �90)))

#|

(defun partie-5 (deb fin)
  (init)
  (sequence-5 deb fin)
  (player))

(partie-5 �0 (rnd �60 �120))

////////////////////////////////////////////////////////////////////////////////////////////////////////////////// 5
 
Jeu interactif 5:
      jeu-a: triangles aigus r�p�t�s
      jeu-b: triangles m�dium r�p�t�s 

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
|#

(defun init-interaction-5 ()

  ; jeu a
  (setq *bleu-a1* 6)                     ;ctrl 21 --> nombre de r�p�tition de l'�chantillon 
  (setq *violet-a1* �70)                 ;ctrl 31 --> hauteur de l'�chantillon r�p�t�
  (setq *violet-a2* �4)                  ;ctrl 32 --> nbre de dur�e rythmique de m�me valeur
  (setq *violet-a3* (rnd �1 �8))         ;ctrl 33 --> ; nbre de r�p�tition d'un canal al�atoire

  ; jeu b
  (setq *bleu-b1* 6)                     ;ctrl 21  --> nombre de r�p�tition de l'�chantillon 
  (setq *violet-b1* �50)                 ;ctrl 31 --> hauteur de l'�chantillon r�p�t� 
  (setq *violet-b2* �2)                  ;ctrl 32 --> nbre de dur�e rythmique de m�me valeur
  (setq *violet-b3* �3)                  ;ctrl 33 --> nombre de r�p�tition sur un m�me canal

  (setq *mode* t)
  (setq *debut-interaction* nil))



;; ************************************* triangles aigus r�p�t�s


 
(defun jeu-a5 (num val)
  (cond ((= num 21) (setq *bleu-a1* (scale2 val 1 8)))        ;ctrl 21 --> nombre de r�p�tition de l'�chantillon 
        ((= num 31) (setq *violet-a1* (scale val 64 76)))     ;ctrl 31 --> d�part hauteur dune mont�e chromatique
        ((= num 32) (setq *violet-a2* (scale-inv val 1 5)))       ;ctrl 32 --> nbre de dur�e rythmique de m�me valeur
        ((= num 33) (setq *violet-a3* (scale val 1 9))))      ;ctrl 33 --> nbre de r�p�tition d'un canal al�atoire

  (p-interactif (rep (h *totalzone*) *violet-a3*)
                (rep (sel (h �(0 0 0 1 1 2)) �100 �200 �300) *violet-a2*)  
                (add (rdv �+ �1) *violet-a1*) 
                (rep (rnd �30 �110) (g *bleu-a1*)) *bleu-a1* :fin �2000 :stac �5))



;; ************************************* triangles m�dium r�p�t�s (scale val 21 51)
(defun jeu-b5 (num val)
 
    (cond ((= num 21) (setq *bleu-b1* (scale2 val 1 6)))        ;ctrl 21 --> nombre de r�p�tition de l'�chantillon 
        ((= num 31) (setq *violet-b1* (scale val 48 60)))       ;ctrl 31 --> d�part hauteur dune mont�e chromatique
        ((= num 32) (setq *violet-b2* (scale-inv val 1 5)))         ;ctrl 32 --> nbre de dur�e rythmique de m�me valeur 
        ((= num 33) (setq *violet-b3* (scale val 1 9))))        ;ctrl 33 --> nbre de r�p�tition d'un canal al�atoire

    (p-interactif (rep (h *totalzone*) *violet-b3*)
                (rep (sel (h �(0 0 0 1 1 2)) �100 �200 �300) *violet-b2*)  
                (add (rdv �- �1) *violet-b1*) 
                (rep (rnd �30 �100) (g *bleu-a1*)) *bleu-b1* :fin �2000 :stac �5))





(defun interaction-5 (e) 
  (when (is-ctrl-change2 e)
    (let ((num-ctrl (ctrl e))
          (val (valint e))
          (dat (date e)))
      
      (if (= num-ctrl 15) (progn (setq *mode* (not *mode*)) (vers-MAX (if *mode* 9 10))) 
          (when (and (> dat *last-interaction-date*) (chge-val? num-ctrl val)) 

          (setq *debut-interaction* t) 
                    

          (if *mode*
              (jeu-a5 num-ctrl val)  ;triangles aig�s
            (jeu-b5 num-ctrl val))   ;triangles �dium
          (setq *last-interaction-date* (+  dat 1000)))))))




;----------------------------

(defun partie-5 (deb fin) 
  (vers-MAX 9)
    (init)
    (init-interaction-5)

    (change-interaction 'interaction-5)

    (sequence-5 deb fin) 
    (player)
    
    (wait *duree-sequence-initiale*)

    (if *debut-interaction*
        (progn (StopPlayer *session-mf-player*) 
          (sleep 1))) 
   
    (phase2 (- *duree-sequence* *duree-sequence-initiale*))

    (StopPlayer *session-mf-player*)
    (wait *separation-partie*))

;----------------------------

;(partie-5 �0 (g (floor (/ *duree-sequence* 1000))))


(defun g-partie-5 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore �stime ctime etime reverse))
      (progn
        (partie-5 deb fin))))





;; *******************************************************************     S�quence 6 
;; Motif doux et boucl�
;; version pour 16 points       Multi M5
;; avec motifs c�cilia doux aigu et boucl�s 
;; avec coucou c�cilia
;; avec it�ratifs c�cilia 
;; avec rivi�re
;; *******************************************************************


(defun sequence-6 (deb fin) 
  
  (print-dialog "SEQUENCE 6")
  
  ;; ************* prog-change 
  (place-prog 0 m5 *out*)
  (place-tempo 0 60)


  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))
  
  
    ;; ************************************* motifs c�cilia doux aigu et boucl�s 
    ;; zone1
    (prorythme (add �6 (h *totalzone*)) (add deb2 (h �( 4 6 7 8 10))) fin2
               (gnotes :proba (h �(1 1 1 1 0 0)))
               (h �(2 3 4 6 8 10 12))
               (h �((8) (9) (10) (11) (14) (4)))
               (rnd �96 �111)
               (h �(80 100 110)))
  
  
    ;; ************************************* coucou c�cilia
    (prorythme (rnd �6 �11) (add deb2 (h �(2 4 6 7 8 10))) fin2
               (gnotes :proba (h �(1 1 1 1 0 0)))
               (h �(2 4 6 8 10))
               (h �((8) (9) (10) (2) (14) (4)))
               (rnd �55 �67)
               �90)
  
    ;; ************************************* it�ratifs c�cilia 
    (share-gen repet (rnd �1 �6))
    (prorythme (rnd �6 �11) deb2 fin2
               (gnotes :proba (h �(1 1 1 0 0 0)))
               (h �(6 8 10))
               (nconcat (use-gen repet) (h �((1/4) (1/4) (1/8))))
               (rnd �67 �96)
               (rep (h �(80 100)) (use-gen repet)))
    (free-gen repet)
  
    ;; ************************************* c�cilia coucou (n=111)  
    (share-gen n1 (h �(2 3)))
    (prorythme (rep (rnd �6 �11) (mult �2 (use-gen n1))) deb2 fin2
               (gnotes)
               (h �(8 15/2 32 24))
               (nconcat (use-gen n1) �(2 -1))
               �111
               (rep (h �(60 80 100)) (mult �2 (use-gen n1)) ))
    (free-gen n1)
  
    ;; ************************************* fond monocanal Rivi�re par moment (proba 3/4)
    (prorythme (rnd �0 �5) deb2 fin2
               (gnotes :proba (pf �1 (h�(1 1 1 0))) :laccord *n-gaccord-demi* :lecar *n-gecar*)        
               (h �(0 2 4 6 8 10 12))
               �(12)
               (rnd �126 �127) 
               �70)
  
    ;; ************************************* fond general court pseudo-nuit  proba 1/2 
    (prorythme �6 deb2 fin2                                             
               (gnotes :laccord *n-gaccord*
                       :lecar *n-gecar*
                       :proba (h �(1 0)))
               (h �( 0 8 16 4))
               (h �( (4) (6) (8) (10)))
               (rnd �52 �54) 
               �20)))


#|

(defun partie-6 (deb fin)
  (init)
  (sequence-6 deb fin)
  (player))

(partie-6 �0 (rnd �60 �120))

////////////////////////////////////////////////////////////////////////////////////////////////////////////////// 6
 
Jeu interactif 6:
      jeu-a:  c�cilia aigus r�p�t�s   

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
|#



(defun init-interaction-6 ()

  ; jeu a
  (setq *bleu-a1* 3)         ;ctrl 21 --> nombre de r�p�tition de l'�chantillon 
  (setq *violet-a1* �2)      ;ctrl 31 --> type de l'�chantillon r�p�t�
  (setq *violet-a2* �0)      ;ctrl 32 --> choix de la dur�e de l'intervalle rythmique
  (setq *violet-a3* �7)      ;ctrl 33 --> canal

  ; jeu b
  (setq *bleu-b1* 1)         ;ctrl 21  --> nombre de r�p�tition de l'�chantillon 
  (setq *violet-b1* �90)     ;ctrl 31 --> v�locit� de l'�chantillon r�p�t� 
  (setq *violet-b2* �0)      ;ctrl 32 --> ; choix de la dur�e rythmique
  (setq *violet-b3* �6)      ;ctrl 33 --> canal

  (setq *mode* t)
  (setq *debut-interaction* nil))


;; ************************************* c�cilia gouttes aigus r�p�t�s

(defun jeu-a6 (num val)

  (cond ((= num 21) (setq *bleu-a1* (scale2 val 1 8)))        ;ctrl 21 --> nombre de r�p�tition de l'�chantillon 
        ((= num 31) (setq *violet-a1* (scale val 0 7)))       ;ctrl 31 --> hauteur de l'�chantillon r�p�t�
        ((= num 32) (setq *violet-a2* (scale-inv val 100 500)))   ;ctrl 32 --> dur�e rythmique
        ((= num 33) (setq *violet-a3* (scale val 6 12))))     ;crtl 33 --> canal 

  (p-interactif *violet-a3* *violet-a2* (sel *violet-a1* �65 �69 �75 �77 �81 �83 �84)  �100 *bleu-a1*)) 



;; ************************************* coucou
(defun jeu-b6 (num val)
 
  (cond ((= num 21) (setq *bleu-b1* (scale2 val 1 5)))          ;ctrl 21 --> nombre de r�p�tition de l'�chantillon 
        ((= num 31) (setq *violet-b1* (scale val 40 127)))      ;ctrl 31 --> v�locit�
        ((= num 32) (setq *violet-b2* (scale-inv val 250 800)))     ;ctrl 32 --> intervalle entre notes
        ((= num 33) (setq *violet-b3* (scale val 6 12))))       ;ctrl 33 --> canal
  (p-interactif *violet-b3*
                *violet-b2*  
                �111 
                *violet-b1* *bleu-b1* ))




(defun interaction-6 (e) 
  (when (is-ctrl-change2 e)
    (let ((num-ctrl (ctrl e))
          (val (valint e))
          (dat (date e)))
      
      (if (= num-ctrl 15) (progn (setq *mode* (not *mode*)) (vers-MAX (if *mode* 11 12))) 

          (when (and (> dat *last-interaction-date*) (chge-val? num-ctrl val)) 

          (setq *debut-interaction* t) 
                    

          (if *mode*
              (jeu-a6 num-ctrl val)  ;gouttes aig�es
            (jeu-b6 num-ctrl val))   ;coucou
          (setq *last-interaction-date* (+  dat 1000)))))))


;----------------------------

(defun partie-6 (deb fin) 
  (vers-MAX 11)
    (init)
    (init-interaction-6)

    (change-interaction 'interaction-6)

    (sequence-6 deb fin) 
    (player)
    
    (wait *duree-sequence-initiale*)

    (if *debut-interaction*
        (progn (StopPlayer *session-mf-player*) 
          (sleep 1)))
   
    (phase2 (- *duree-sequence* *duree-sequence-initiale*))

    (StopPlayer *session-mf-player*)
    (wait *separation-partie*))

;----------------------------

;(partie-6 �0 (g (floor (/ *duree-sequence* 1000))))

(defun g-partie-6 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore �stime ctime etime reverse))
      (progn
        (partie-6 deb fin))))



;; *******************************************************************     S�quence 7 (ex 18) 
;; Petites gouttes � densit� croissante
;;                   Multi M5
;; avec triangle
;; avec faux insectes
;; avec fond nuit
;; *******************************************************************


(defun sequence-7 (deb fin) 
  
  (print-dialog "SEQUENCE 7")
  
  ;; ************* prog-change 
  (place-prog 0 m5 *out*)
  (place-tempo 0 60)


  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))
  
  ;; ************************************* goutte aig�e m�me rythme   
  (prorythme (rep (rnd �6 �11) (h �(2 3 4 5 6))) (add deb2 (h�(0 1/8 1/16 1/2 1 1/4 3/2)))  fin2 
             (gnotes)
             (sel (floo (i �4 �0)) �1 �2 �3 �4 �5)
             �(1/2)
             �77
             �97)
  (prorythme (rep (rnd �6 �11)(h �(2 3 4 5 6))) (add deb2 (h�(0 1/8 1/16 1/2 1 1/4 3/2)))  fin2 
             (gnotes)
             (sel (floo (i �4 �0)) �1 �2 �3 �4 �5)
             �(1/2)
             �81
             �70)
  (prorythme (rep (rnd �6 �11)(h �(2 3 4 5 6))) (add �10 deb2 (h�(0 1/8 1/16 1/2 1 1/4 3/2)))  fin2 
             (gnotes)
             (sel (floo (i �4 �0)) �1 �2 �3 �4 �5)
             �(1/2)
             �81
             �70)
  (prorythme (rep (rnd �6 �11) (h �(2 3 4 5 6))) (add deb2 (h�(0 1/8 1/16 1/2 1 1/4 3/2)))  fin2 
             (gnotes)
             (sel (floo (i �4 �0)) �1 �2 �3 �4 �5)
             �(1/2)
             �84
             �100)
  (prorythme (rep (rnd �6 �11) (h �(2 3 4 5 6))) (add deb2 (h�(0 1/8 1/16 1/2 1 1/4 3/2)))  fin2 
             (gnotes)
             (sel (floo (i �4 �0)) �1 �2 �3 �4 �5)
             �(1/2)
             �80
             �90)

   ;; ************************************* fond general court pseudo-nuit  proba 2/3   
  (prorythme �6 deb2 fin2                                             
             (gnotes :laccord *n-gaccord*
                     :lecar *n-gecar*
                     :proba (h �(1 1 0)))
             (h �(8 10 12 14))
             (h �( (4) (6) (8) (10)))
             (rnd �52 �54) 
             �20)))
#|


(defun partie-7 (deb fin)
  (init)
  (sequence-7 deb fin)
  (player))

(partie-7 �0 (rnd �60 �120))

////////////////////////////////////////////////////////////////////////////////////////////////////////////////// 7
 
Jeu interactif 7:
      jeu-a:   faux insectes 
      jeu-b:   triangles

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
|#



(defun init-interaction-7 ()

  ; jeu a
  (setq *bleu-a1* 1)            ;ctrl 21 --> nombre de r�p�tition de l'�chantillon 
  (setq *violet-a1* �84)        ;ctrl 31 --> type de l'�chantillon r�p�t�
  (setq *violet-a2* �250)       ;ctrl 32 --> dur�e de l'intervalle rythmique
  (setq *violet-a3* �2)         ;ctrl 33 --> canal

  ; jeu b
  (setq *bleu-b1* 1)            ;ctrl 21  --> nombre de r�p�tition de l'�chantillon 
  (setq *violet-b1* �80)        ;ctrl 31 --> hauteur de l'�chantillon r�p�t� 
  (setq *violet-b2* �250)       ;ctrl 32 --> ; choix de la dur�e rythmique
  (setq *violet-b3* �3)         ;ctrl 33 --> nombre de r�p�tition de l'�chantillon sur un m�me canal

  (setq *mode* t)
  (setq *debut-interaction* nil))


;; ************************************* faux insectes

(defun jeu-a7 (num val)

  (cond ((= num 21) (setq *bleu-a1* (scale2 val 1 5)))        ;ctrl 21 --> nombre de r�p�tition de l'�chantillon 
        ((= num 31) (setq *violet-a1* (scale val 84 126)))    ;ctrl 31 --> hauteur de l'�chantillon r�p�t�
        ((= num 32) (setq *violet-a2* (scale-inv val 250 500)))   ;ctrl 32 --> choix de la dur�e rythmique
        ((= num 33) (setq *violet-a3* (scale val 0 5))))      ;ctrl 33 --> canal 

  (p-interactif *violet-a3* *violet-a2* *violet-a1*  �110 *bleu-a1* :fin �1000))



;; ************************************* triangle-cloche
(defun jeu-b7 (num val)
 
    (cond ((= num 21) (setq *bleu-b1* (scale2 val 1 6)))        ;ctrl 21 --> nombre de r�p�tition de l'�chantillon 
        ((= num 31) (setq *violet-b1* (scale val 21 84)))       ;ctrl 31 --> hauteur
        ((= num 32) (setq *violet-b2* (scale-inv val 200 400)))     ;ctrl 32 --> dur�e notes
        ((= num 33) (setq *violet-b3* (scale val 0 5))))        ;ctrl 31 --> canal
(p-interactif *violet-b3* *violet-b2* *violet-b1* �100 *bleu-b1* :fin �1000 ))


(defun interaction-7   (e) 
  (when (is-ctrl-change2 e)
    (let ((num-ctrl (ctrl e))
          (val (valint e))
          (dat (date e)))
      
      (if (= num-ctrl 15) (progn (setq *mode* (not *mode*)) (vers-MAX (if *mode* 13 14))) 

          (when (and (> dat *last-interaction-date*) (chge-val? num-ctrl val)) 

          (setq *debut-interaction* t) 
                    

          (if *mode*
              (jeu-a7 num-ctrl val)  ;faux insectes
            (jeu-b7 num-ctrl val))   ;triangles
          (setq *last-interaction-date* (+  dat 1000)))))))


;----------------------------

(defun partie-7 (deb fin) 
  (vers-MAX 13)
    (init)
    (init-interaction-7)

    (change-interaction 'interaction-7)

    (sequence-7 deb fin) 
    (player)
    
    (wait *duree-sequence-initiale*)

    (if *debut-interaction*
        (progn (StopPlayer *session-mf-player*) 
           (sleep 1))) 
   
    (phase2 (- *duree-sequence* *duree-sequence-initiale*))

    (StopPlayer *session-mf-player*)
    (wait *separation-partie*))

;----------------------------

;(partie-7 �0 (g (floor (/ *duree-sequence* 1000))))


(defun g-partie-7 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore �stime ctime etime reverse))
      (progn
        (partie-7 deb fin))))


;; *******************************************************************     S�quence 8 (ex 19) 
;; Gouttes r�p�t�es et gong et voix r�p�t�es
;;        Multi M5
;; avec triangle r�gulier tout canaux
;; avec voix souffl�e rare
;; avec voix rares tout canaux 2 fois
;; *******************************************************************


(defun sequence-8 (deb fin)     
  
  (print-dialog "SEQUENCE 8")
  
  ;; ************* prog-change 
  (place-prog 0 m5 *out*)
  (place-tempo 0 60)


  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))

        (n-prorythme (add deb2 (h �(1 2 3 4 5 6 7 8 10 0 1/2 1/4 1/8 1/16))) fin2 (h�(1 2 3 4 0 1/4 1/8 1/16 ))
                 (n-gnotes (rnd �6 �11) (h �((7) (8) (9) (10)  (12) (14))) �77 �100))
  
    (n-prorythme (add deb2 (h �(1 2 3 4 5 6 7 8 10 0 1/2 1/4 1/8 1/16))) fin2 (h�(1 2 3 4 0 1/4 1/8 1/16 ))
                 (n-gnotes (rnd �6 �11) (h �((7) (8) (9) (10)  (12) (14))) �77 �100))
  
    (n-prorythme (add deb2 (h �(1 2 3 4 5 6 7 8 10 0 1/2 1/4 1/8 1/16))) fin2 (h�(1 2 3 4 0 1/4 1/8 1/16 ))
                 (n-gnotes (rnd �6 �11) (h �((7) (8) (9) (10)  (12) (14))) �77 �100))
  
    (prorythme (add �6 (h *totalzone*)) (add deb2 (h�(0 1/2 1 1/4 3/2))) fin2 
               (gnotes)
               (sel-al (floo (i �4 �0)) �0 �2 �4 �8 �16)
               (sel-al (floo (i �6 �0)) �(16) �(14) �(11) �(10) �(8) �(6) �(4) �(4))
               (rnd �61 �62)
               �90)
    (prorythme (add �6 (h *totalzone*)) (add deb2 (h�(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel-al (floo (i �4 �0)) �0 �2 �4 �8 �16)
               (sel-al (floo (i �6 �0)) �(16) �(14) �(11) �(10) �(8) �(6) �(4) �(4))
               (rnd �61 �62)
               �90)
    (prorythme (add �6 (h *totalzone*)) (add deb2 (h�(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel-al (floo (i �4 �0)) �0 �2 �4 �8 �16)
               (sel-al (floo (i �6 �0)) �(16) �(14) �(11) �(10) �(8) �(6) �(4) �(4))
               (rnd �61 �62)
               �90)
    (prorythme (add �6 (h *totalzone*)) (add deb2 (h�(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel-al (floo (i �4 �0)) �0 �2 �4 �8 �16)
               (sel-al (floo (i �6 �0)) �(16) �(14) �(11) �(10) �(8) �(6) �(4) �(4))
               (rnd �61 �62)
               �90)
    (prorythme (add �6 (h *totalzone*)) (add deb2 (h�(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel-al (floo (i �4 �0)) �0 �2 �4 �8 �16)
               (sel-al (floo (i �6 �0)) �(16) �(14) �(11) �(10) �(8) �(6) �(4) �(4))
               (rnd �61 �62)
               �90)
    (prorythme (add �6 (h *totalzone*)) (add deb2 (h�(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel-al (floo (i �4 �0)) �0 �2 �4 �8 �16)
               (sel-al (floo (i �6 �0)) �(16) �(14) �(11) �(10) �(8) �(6) �(4) �(4))
               (rnd �61 �62)
               �90)
    (prorythme (add �6 (h *totalzone*)) (add deb2 (h�(0 1/2 1 1/4 3/2))) fin2 
               (gnotes)
               (sel-al (floo (i �4 �0)) �0 �2 �4 �8 �16)
               (sel-al (floo (i �6 �0)) �(16) �(14) �(11) �(10) �(8) �(6) �(4) �(4))
               (rnd �61 �62)
               �90)

  
  
    ;; ***************************************** triangle r�gulier tout canaux (pitch= 30)  
  (prorythme �0 (add deb2 �24) fin2 
             (gnotes :laccord *n-gaccord* 
                     :lecar *n-gecar*)
             �8
             �(24)
             �36
             �45)

;; ***************************************** apparitions de rivi�res  
  (prorythme �0 (add �20 deb2) fin2                                             
             (gnotes :laccord *n-gaccord*
                     :lecar *n-gecar*)
             (h �(8 16 24))
             (h �( (8) (16) (24) (6)))
             (rnd �126 �127) 
             �60)

     ;; ***************************************** voix souffl�e rare (pitch= 27 ou 43)  
  (prorythme (rnd �6 �11) (add deb2 (h�(0 1/2 1 1/4 3/2))) fin2 
             (gnotes)
             (sel-al �3 �6 �10 �16 �20)
             �(1/4 1/4 1/4)
             �43
             �60)))
  

  


#|

(defun partie-8 (deb fin)
  (init)
  (sequence-8 deb fin)
  (player))

(partie-8 �0 (rnd �60 �120))


////////////////////////////////////////////////////////////////////////////////////////////////////////////////// 8
 
Jeu interactif 8:
      jeu-a:   voix 
      jeu-b:   guttes c�cilia

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
|#



(defun init-interaction-8 ()

  ; jeu a
  (setq *bleu-a1* 0)          ;ctrl 21 --> nombre de r�p�tition de l'�chantillon 
  (setq *violet-a1* �21)      ;ctrl 31 --> hauteur de l'�chantillon r�p�t�
  (setq *violet-a2* �250)     ;ctrl 32 --> choix de la dur�e de l'intervalle rythmique
  (setq *violet-a3* �8)       ;ctrl 33 --> canal

  ; jeu b
  (setq *bleu-b1* 1)          ;ctrl 21  --> nombre de r�p�tition de l'�chantillon 
  (setq *violet-b1* �60)      ;ctrl 31 --> hauteur de l'�chantillon r�p�t� 
  (setq *violet-b2* �250)     ;ctrl 32 --> ; choix de la dur�e rythmique

  (setq *mode* t)
  (setq *debut-interaction* nil))



;; ***************************************** voix 

(defun jeu-a8 (num val)

  (cond ((= num 21) (setq *bleu-a1* (scale2 val 1 3)))           ;;ctrl 21 --> nombre de r�p�tition de l'�chantillon 
        ((= num 31) (setq *violet-a1* (scale val 21 51)))        ;;ctrl 31 --> hauteur de l'�chantillon r�p�t�
        ((= num 32) (setq *violet-a2* (scale-inv val 250 500)))      ;;ctrl 32 --> choix de la dur�e rythmique
        ((= num 33) (setq *violet-a3* (scale val 6 12))))        ;;ctrl 33--> canal 

  (p-interactif *violet-a3* *violet-a2* *violet-a1*  �80 *bleu-a1*))



;; ************************************* gouttes c�cilia

(defun jeu-b8 (num val)
 
    (cond ((= num 21) (setq *bleu-b1* (scale2 val 1 14)))        ;ctrl 21 --> nombre de r�p�tition de l'�chantillon 
        ((= num 31) (setq *violet-b1* (scale val 50 84)))        ;ctrl 31 --> hauteur
        ((= num 32) (setq *violet-b2* (scale-inv val 100 400))))     ;ctrl 32 --> dur�e notes

(p-interactif (rnd �6 �11) *violet-b2* *violet-b1* �120 *bleu-b1*))



(defun interaction-8   (e) 
  (when (is-ctrl-change2 e)
    (let ((num-ctrl (ctrl e))
          (val (valint e))
          (dat (date e)))
      
      (if (= num-ctrl 15) (progn (setq *mode* (not *mode*)) (vers-MAX (if *mode* 15 16))) 

          (when (and (> dat *last-interaction-date*) (chge-val? num-ctrl val)) 

          (setq *debut-interaction* t) 
                    

          (if *mode*
              (jeu-a8 num-ctrl val)  ;voix
            (jeu-b8 num-ctrl val))   ;gouttes
          (setq *last-interaction-date* (+  dat 1000)))))))


;----------------------------

(defun partie-8 (deb fin) 
  (vers-MAX 15)
    (init)
    (init-interaction-8)

    (change-interaction 'interaction-8)

    (sequence-8 deb fin) 
    (player)
    
    (wait *duree-sequence-initiale*)

    (if *debut-interaction*
        (progn (StopPlayer *session-mf-player*) 
          (sleep 1)))
   
    (phase2 (- *duree-sequence* *duree-sequence-initiale*))

    (StopPlayer *session-mf-player*)
    (wait *separation-partie*))

;----------------------------


;(partie-8 �0 (g (floor (/ *duree-sequence* 1000))))


(defun g-partie-8 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore �stime ctime etime reverse))
      (progn
        (partie-8 deb fin))))


;; *******************************************************************     S�quence 9 (ex 20) 
;; Trajectoires          Multi M2
;;
;; percus rares r�p�t�es 
;; trajectoires completes sur les 16 enceintes enchain�es
;; *******************************************************************




(defun sequence-9 (deb fin) 
  
  (print-dialog "SEQUENCE 9")
  
  ;; ************* prog-change 
  (place-prog 0 m2 *out*)
  (place-tempo 0 60)


  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))
  
  
    ;; ************************************* un peu de "tenu" jour / nuit br�ves al�atoires synchro  
    (let ((fond (funcall (rnd �21 �68) 0 1 0 nil)))
    (prorythme �6 deb2 fin2                                        
               (gnotes :laccord *n-gaccord* 
                       :lecar *n-gecar*)
               (h �(4 8 10 15 20 ))
               (sel-al �5  �(10) �(5) �(6) �(7) �(8) �(9))
               (g fond) 
               �15))
  
    ;; ************************************* percus rares r�p�t�es 
    (share-gen n1 (h �( 1 2 2 2 2 3 3 3 3 4 4 4)))
    
    (n-prorythme  deb2 fin2  (h �(3 4 5 7 9 12 15))
                  (sel-al �3 
                          (n-gnotes (rep (h *n-zone1*) (use-gen n1))
                                    (xconcat (use-gen n1)  (sel-al �3   �(4) �(3) �(5) �(8)))
                                    (rep (h �(70 79 82)) (use-gen n1))
                                    �90)
                          (n-gnotes (rep (h *n-zone2*) (use-gen n1))
                                    (xconcat (use-gen n1)  (sel-al �3   �(4) �(3) �(5) �(8)))
                                    (rep (h �(70 79 82)) (use-gen n1))
                                    �90)
                          (n-gnotes (rep (h *n-zone3*) (use-gen n1))
                                    (xconcat (use-gen n1)  (sel-al �3   �(4) �(3) �(5) �(8)))
                                    (rep (h �(70 79 82)) (use-gen n1))
                                    �90)
                          
                          (n-gnotes (rnd �0 �5)
                                    �(12)
                                    (rnd �69 �125)
                                    �80)))
    (free-gen n1)
    

  
    ;; ************************************* trajectoires completes sur les 6 enceintes enchain�es 
    (share-gen t1 (sel (h �(0 1 2 4))
                       �(0 1 4 5 3 2)
                       �(0 1 2 3 4 5)
                       �(5 4 3 2 1 0)
                       �(0 2 3 1 4 5 0 4 3 1)
                       (nconcat-spe4 �16 (hs �(0 1 2 3 4 5)))))

    (share-gen ntraj (h �( 4 5 6 7 8 9 10 12 14)))

    (sel (rnd �0 �3)
         
         (prorythme (s (use-gen t1)) (add deb2 (h �(0 1 2 4 6 7 8 10))) fin2                           
                    (gnotes)                                                                  
                    (h�(7 9 11 17))
                    (sel (rnd �0 �3)                                                                
                         (xconcat (use-gen ntraj)  �(2/3c(1/4 1/4 1/4)))                   
                         (xconcat (use-gen ntraj)  �(1/8 1/8 1/8))
                         (xconcat (use-gen ntraj)  �(1/4 1/4 1/4))
                         (xconcat (use-gen ntraj)  �(1/2 1/2 1/2)))
                    (rep (h �(69 70 71 74 76 77 78 79 80 81 92 93 94 95 96 108))  (mult �3 (use-gen ntraj))) 
                    �80)

         (prorythme (add �6 (s (use-gen t1))) (add deb2 (h �(0 1 2 4 6 7 8 10))) fin2                           
                    (gnotes)                                                                  
                    (h�(7 9 11 17))
                    (sel (rnd �0 �3)                                                                
                         (xconcat (use-gen ntraj)  �(2/3c(1/4 1/4 1/4)))                   
                         (xconcat (use-gen ntraj)  �(1/8 1/8 1/8))
                         (xconcat (use-gen ntraj)  �(1/4 1/4 1/4))
                         (xconcat (use-gen ntraj)  �(1/2 1/2 1/2)))
                    (rep (rnd �69  �125) (mult �3 (use-gen ntraj))) 
                    �80))
    (free-gen ntraj)
    (free-gen t1)))

#|

(defun partie-9 (deb fin)
  (init)
  (sequence-9 deb fin)
  (player))

(partie-9 �0 (rnd �60 �120))

////////////////////////////////////////////////////////////////////////////////////////////////////////////////// 9
 
Jeu interactif 9:
      jeu-a:   percus 
      jeu-b:   percu diverses

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
|#



(defun init-interaction-9 ()

  ; jeu a
  (setq *bleu-a1* 1)         ;ctrl 21 --> nombre de r�p�tition de l'�chantillon 
  (setq *violet-a1* �0)      ;ctrl 31 --> hauteur de l'�chantillon r�p�t�
  (setq *violet-a2* �200)    ;ctrl 32 --> choix de la dur�e de l'intervalle rythmique

  ; jeu b
  (setq *bleu-b1* 1)        ;ctrl 21  --> nombre de r�p�tition de l'�chantillon 
  (setq *violet-b1* �10)    ;ctrl 31 --> type d'�chantillon r�p�t� 
  (setq *violet-b2* �2)     ;ctrl 32 --> ; choix de la dur�e rythmique
  (setq *violet-b3* �3)     ;ctrl 33 --> canal

  (setq *mode* t)
  (setq *debut-interaction* nil))


;; ***************************************** percus 

(defun jeu-a9 (num val)

  (cond ((= num 21) (setq *bleu-a1* (scale2 val 1 8)))        ;ctrl 21 --> nombre de r�p�tition de l'�chantillon 
        ((= num 31) (setq *violet-a1* (scale val 0 3)))       ;ctrl 31 --> choix de la hauteur de l'�chantillon r�p�t�
        ((= num 32) (setq *violet-a2* (scale-inv val 150 400))))  ;ctrl 32 --> dur�e rythmique

  (p-interactif (mem (rnd �0 �5) �2 �4) *violet-a2*  (sel *violet-a1* �70 �79 �82)  (h �(80 80 80 80 80 90 90 90 110 110 127)) *bleu-a1* :fin �2000))



;; ************************************* percus diverses

(defun jeu-b9 (num val)
 
  (cond 
   ((= num 21) (setq *bleu-b1* (scale2 val 1 14)))        ;ctrl 21 --> nombre de r�p�tition de l'�chantillon 
   ((= num 31) (setq *violet-b1* (scale val 0 16)))       ;ctrl 31 --> choix des hauteurs
   ((= num 32) (setq *violet-b2* (scale-inv val 0 4)))        ;ctrl 32 --> dur�e notes
   ((= num 33) (setq *violet-b3* (scale val 0 6))))       ;ctrl 33 --> canal

  (p-interactif *violet-b3* (sel *violet-b2* �166 �250 �500 �750) 
                (rep (sel *violet-b1* �69 �70 �71 �74 �76 �77 �78 �79 �80 �81 �92 �93 �94 �95 �96 �108) �3) (h �(80 80 80 90 90 90 100 110 120)) *bleu-b1* :fin �1000))



(defun interaction-9   (e) 
  (when (is-ctrl-change2 e)
    (let ((num-ctrl (ctrl e))
          (val (valint e))
          (dat (date e)))
      
      (if (= num-ctrl 15) (progn (setq *mode* (not *mode*)) (vers-MAX (if *mode* 17 18))) 
 
          (when (and (> dat *last-interaction-date*) (chge-val? num-ctrl val)) 

          (setq *debut-interaction* t) 
                    

          (if *mode*
              (jeu-a9 num-ctrl val)  ;percus
            (jeu-b9 num-ctrl val))   ;percus diverses
          (setq *last-interaction-date* (+  dat 1000)))))))


;----------------------------

(defun partie-9 (deb fin) 
  (vers-MAX 17)
    (init)
    (init-interaction-9)

    (change-interaction 'interaction-9)

    (sequence-9 deb fin) 
    (player)
    
    (wait *duree-sequence-initiale*)

    (if *debut-interaction*
        (progn (StopPlayer *session-mf-player*) 
          (sleep 1)))
   
    (phase2 (- *duree-sequence* *duree-sequence-initiale*))

    (StopPlayer *session-mf-player*)
    (wait *separation-partie*))

;----------------------------


;(partie-9 �0 (g (floor (/ *duree-sequence* 1000))))


(defun g-partie-9 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore �stime ctime etime reverse))
      (progn
        (partie-9 deb fin))))




;; *******************************************************************     S�quence 10 (ex 21) 

;;          Multi M7
;; trajectoires circulaires de voix al�atoires parfois doubl�es par des oiseaux et/ou percu
;; rythme constant �(1/8 1/8 1/8 1/8) ou �(1)

;; voix al�atoires r�p�t�es de plus en plus et circulairement
;; oiseaux/percu al�atoires r�p�t�es de plus en plus et circulairement et synchrone aux voix

;; *******************************************************************


(defun sequence-10 (deb fin) 
  
  (print-dialog "SEQUENCE 10")
  
  
  ;; ************* prog-change 
  (place-prog 0 m7 *out*)
  (place-tempo 0 60)
  
  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))
  

  
    ;; ************* voix al�atoires r�p�t�es de plus en plus et circulairement
    (share-gen inter (h �(0 0 0 1 2 2 2 3 3 4 5)))
    (share-gen velos (h �( 70 80 90 100 110)))
    (share-gen choix (h �(0 0 0 0 0 1 1 1 1 1 2 3 4)))
    (prorythme (s (concat (lst (rnd �0 �5)) *traj-total-direct2*)) (add deb2 �0) fin2                       
               (gnotes :laccord (sel (use-gen choix) �(0) �(0) �(0) *n-gaccord-demi* *n-gaccord-demi*) :lecar *n-gecar* )
               (use-gen inter)
               (sel (use-gen choix) �(1/8 1/8 1/8 1/8) �(1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8) �(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) �(4) �(4 4))
               (rep (rnd �21 �50) (s �(1 2 3 4 5 6 7 8 9)) )
               (add �0 (sel (use-gen choix) (rep (use-gen velos)  (s �(1 2 3 4 5 6 7))) (rep (use-gen velos)  (s �(1 2 3 4 5 6 7))) (rep (use-gen velos)  (s �(1 2 3 4 5 6 7))) �60 (s �(40 80)))))
    ;; ************* oiseaux/percu al�atoires r�p�t�es de plus en plus et circulairement et synchrone aux voix
    (prorythme (s *traj-total-direct2*) (add deb2 �0) fin2                       
               (gnotes :proba (rep (h �(0 1 1)) (rnd �2 �16)) :laccord (sel (use-gen choix) �(0) �(0) �(0) *n-gaccord-demi* *n-gaccord-demi*) :lecar *n-gecar*)
               (use-gen inter)
               (sel (use-gen choix) �(1/8 1/8 1/8 1/8) �(1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8) �(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) �(4) �(4 4))
               (rep (rnd �69 �125) (s �(1 2 3 4 5 6 7 8 9)) )
               (sel (use-gen choix) (rep (use-gen velos)  (s �(1 2 3 4 5 6 7))) (rep (use-gen velos)  (s �(1 2 3 4 5 6 7))) (rep (use-gen velos)  (s �(1 2 3 4 5 6 7))) �40 (s �(40 40))))
    (free-gen inter)
    (free-gen velos)
    (free-gen choix)))


#|

(defun partie-10 (deb fin)
  (init)
  (sequence-10 deb fin)
  (player))

(partie-10 �0 (rnd �60 �120))

////////////////////////////////////////////////////////////////////////////////////////////////////////////////// 10
 
Jeu interactif 10:
      jeu-a:   voix 
      jeu-b:   percus diverses

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
|#


(defun init-interaction-10 ()

  ; jeu a
  (setq *bleu-a1* 1)         ;ctrl 21 --> nombre de p-interactif
  (setq *violet-a1* �30)     ;ctrl 31 --> hauteurdu premier p-interactif
  ; jeu b
  (setq *bleu-b1* 1)         ;ctrl 21  --> nombre de p-interactif 
  (setq *violet-b1* �100)    ;ctrl 31 --> hauteurdu premier p-interactif

  (setq *mode* t)
  (setq *debut-interaction* nil))


;; ***************************************** voix 

(defun jeu-a10 (num val)

  (cond ((= num 21) (setq *bleu-a1* (scale2 val 0 3)))             ;ctrl 21 --> nombre de p-interactif
        ((= num 31) (setq *violet-a1* (scale val 21 51))))         ;ctrl 31 --> hauteurdu premier p-interactif   

  (share-gen velos (h �( 80 90 100 110 120)))
  (share-gen choix (h �(0 0 0 0 0 1 1 1 1 1 2 3 4)))

    (p-interactif (s �(6 7 8)) 
                  (sel (use-gen choix) �125 �125 �1000 �4000 �4000)  
                  *violet-a1* 
                  (add �0 (sel (use-gen choix) (rep (use-gen velos)  (s �(1 2 3 4 5 6 7))) (rep (use-gen velos)  (s �(1 2 3 4 5 6 7))) (rep (use-gen velos)  (s �(1 2 3 4 5 6 7))) �60 (s �(40 80)))) 
                  (funcall (sel (use-gen choix) �4 �16 �16 �1 �2) 0 0 1 nil))
    (if (> *bleu-a1* 0)
        (p-interactif (s �(9 10 11)) 
                      (sel (use-gen choix) �125 �125 �1000 �4000 �4000)  
                      (rep (rnd �21 �50) (s �(1 2 3 4 5 6 7 8 9)) )  
                      (add �0 (sel (use-gen choix) (rep (use-gen velos)  (s �(1 2 3 4 5 6 7))) (rep (use-gen velos)  (s �(1 2 3 4 5 6 7))) (rep (use-gen velos)  (s �(1 2 3 4 5 6 7))) �60 (s �(40 80)))) 
                      (funcall (sel (use-gen choix) �4 �16 �16 �1 �2) 0 0 1 nil)))
    (if (> *bleu-a1* 1)
        (p-interactif (rnd �0 �5) 
                      (sel (use-gen choix) �125 �125 �1000 �4000 �4000)  
                      (rep (rnd �21 �50) (s �(1 2 3 4 5 6 7 8 9)) )  
                      (add �0 (sel (use-gen choix) (rep (use-gen velos)  (s �(1 2 3 4 5 6 7))) (rep (use-gen velos)  (s �(1 2 3 4 5 6 7))) (rep (use-gen velos)  (s �(1 2 3 4 5 6 7))) �60 (s �(40 80)))) 
                      (funcall (sel (use-gen choix) �4 �16 �16 �1 �2) 0 0 1 nil)))
  (free-gen velos)
  (free-gen choix))



;; ************************************* percus diverses

(defun jeu-b10 (num val)
 
  (cond ((= num 21) (setq *bleu-b1* (scale2 val 0 3)))            ;ctrl 21 --> nombre de p-interactif
        ((= num 31) (setq *violet-b1* (scale val 69 126))))       ;ctrl 31 --> hauteurdu premier p-interactif 

  (share-gen velos (h �( 80 90 100 110 120)))
  (share-gen choix (h �(0 0 0 0 0 1 1 1 1 1 2 3 4)))

  (p-interactif (rep (s �(0 1 2 3 4 5)) (use-gen choix))
                (sel (use-gen choix) �125 �125 �500 �4000 �4000) 
                *violet-b1*
                (sel (use-gen choix) (rep (use-gen velos)  (s �(1 2 3 4 5 6 7))) (rep (use-gen velos)  (s �(1 2 3 4 5 6 7))) (rep (use-gen velos)  (s �(1 2 3 4 5 6 7))) �40 (s �(40 40)))
                (funcall (sel (use-gen choix) �4 �16 �16 �1 �2) 0 0 1 nil))
  (if (> *bleu-b1* 0)
      (p-interactif (s �(0 1 2 3 4 5))
                    (sel (use-gen choix) �125 �125 �500 �4000 �4000) 
                    (rep (rnd �69 �125) (s �(1 2 3 4 5 6 7 8 9)) ) 
                    (sel (use-gen choix) (rep (use-gen velos)  (s �(1 2 3 4 5 6 7))) (rep (use-gen velos)  (s �(1 2 3 4 5 6 7))) (rep (use-gen velos)  (s �(1 2 3 4 5 6 7))) �40 (s �(40 40)))
                    (funcall (sel (use-gen choix) �4 �16 �16 �1 �2) 0 0 1 nil)))
  (if (> *bleu-b1* 1)
      (p-interactif (s �(0 1 2 3 4 5))
                    (sel (use-gen choix) �125 �125 �500 �4000 �4000) 
                    (rep (rnd �69 �125) (s �(1 2 3 4 5 6 7 8 9)) ) 
                    (sel (use-gen choix) (rep (use-gen velos)  (s �(1 2 3 4 5 6 7))) (rep (use-gen velos)  (s �(1 2 3 4 5 6 7))) (rep (use-gen velos)  (s �(1 2 3 4 5 6 7))) �40 (s �(40 40)))
                    (funcall (sel (use-gen choix) �4 �16 �16 �1 �2) 0 0 1 nil)))
  (free-gen velos)
  (free-gen choix))



(defun interaction-10   (e) 
  (when (is-ctrl-change2 e)
    (let ((num-ctrl (ctrl e))
          (val (valint e))
          (dat (date e)))
      
      (if (= num-ctrl 15) (progn (setq *mode* (not *mode*)) (vers-MAX (if *mode* 19 20))) 

          (when (and (> dat *last-interaction-date*) (chge-val? num-ctrl val)) 

          (setq *debut-interaction* t) 
                    

          (if *mode*
              (jeu-a10 num-ctrl val)  ;percus
            (jeu-b10 num-ctrl val))   ;percus diverses
          (setq *last-interaction-date* (+  dat 1000)))))))


;----------------------------

(defun partie-10 (deb fin) 
  (vers-MAX 19)
    (init)
    (init-interaction-10)
    (change-interaction 'interaction-10)

    (sequence-10 deb fin) 
    (player)

    (wait *duree-sequence-initiale*)

    (if *debut-interaction*
        (progn (StopPlayer *session-mf-player*) 
          (sleep 1))) 
   
    (phase2 (- *duree-sequence* *duree-sequence-initiale*))

    (StopPlayer *session-mf-player*)
    (wait *separation-partie*))

;----------------------------


;(partie-10 �0 (g (floor (/ *duree-sequence* 1000))))

(defun g-partie-10 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore �stime ctime etime reverse))
      (progn
        (partie-10 deb fin))))



;; *******************************************************************     S�quence 11 (ex 34) 
;; Rivi�re seule
;;          Multi M2
;; avec un peu d'insectes et oiseaux al�atoire

;; *******************************************************************


(defun sequence-11 (deb fin) 
  
  (print-dialog "SEQUENCE 11")
  
  ;; ************* prog-change 
  (place-prog 0 m2 *out*)
  (place-tempo 0 60)
  
  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin))))
        (fin-debut (g (floor (/ *duree-sequence-initiale* 1000)))))
  
    ;; ************* fond g�n�ral Rivi�re
    (prorythme �0 deb2 fin-debut
               (gnotes :laccord *n-gaccord* :lecar *n-gecar*)        
               �0
               (lst (sub fin-debut deb2))
               �126 
               �50)

(prorythme �0 (add �2 fin-debut) fin2
               (gnotes :laccord *n-gaccord* :lecar *n-gecar*)        
               �0
               (h �((4) (6) (8) (10) (12) (14)))
               �126 
               �50)
 


  (prorythme (s (concat (lst (rnd �0 �5)) (sel (h �(0 1)) *traj-total-direct2* *traj-zig-zag*))) (add deb2 �0) fin2                       
               (gnotes :stac �5.0)
               (h �(2 4 6 8 10 12 14))
               (sel (h �(0 0 0 0 0 0 0 0 0 0 1 1 2)) �(1/8 1/8 1/8 1/8) �(1/4 1/4) �(1/4 1/4 1/4 1/4))
              (rep (hs �(23 28 30 32 38 39 40 41 42 43 44 45 46 47 48 49)) (rnd �60 �80))              
              (add �0 (s �(127 110 75 75 75 75 75 75 75 75 75 75 75 75 75 75 75 75))))
  
    ;; ************* un peu d'oiseaux al�atoires

    (share-gen nbre (rnd �1 �4))
    (prorythme �0 (add deb2 (h �(0 2 4 6 7 8 10))) fin2                 
               (gnotes :laccord (rep (nconcat-spe4 (rnd �3 �5) (rnd �6 �11)) (use-gen nbre)) :lecar *n-gecar*)
               (h �( 10 12))
               (sel-al �2 �(1/2) �(1) �(3/2))
               (rep (rnd �69 �125) (use-gen nbre)) 
               (h �(50 60 80 70)))
    (free-gen nbre)))

 
#|

(defun partie11 (deb fin)
  (init)
  (sequence-11 deb fin)
  (player))


(partie11 �0 (rnd �120 �180))

////////////////////////////////////////////////////////////////////////////////////////////////////////////////// 11
 
Jeu interactif 11:
      jeu-a:   oiseaux 
      jeu-b:   guiro

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
|#


(defun init-interaction-11 ()

  ; jeu a
  (setq *bleu-a1* 1)         ;ctrl 21 --> nombre d'�chantillons
  (setq *violet-a1* �72)     ;ctrl 31 --> hauteur
  (setq *violet-a2* �3)      ;ctrl 32 --> choix de la dur�e
  (setq *violet-a3* �7)      ;ctrl 33 --> canal

  (setq *mode* t)
  (setq *debut-interaction* nil))

;; ***************************************** oiseaux 

 


(defun jeu-a11 (num val)

  (cond ((= num 21) (setq *bleu-a1* (scale2 val 1 5)))             ;ctrl 21 --> nombre d'�chantillons
        ((= num 31) (setq *violet-a1* (scale val 69 125)))         ;ctrl 31 --> hauteur  
        ((= num 32) (setq *violet-a2* (scale-inv val 0 5)))            ;ctrl 32 --> choix de la dur�e
        ((= num 33) (setq *violet-a3* (scale val 6 12))))          ;ctrl 33 --> canal
  (p-interactif *violet-a3*
                (sel *violet-a2* �300 �750 �500 �1000 �5000)  
                *violet-a1*
                (rep *violet-a1* (h �(80 90 110)))
                *bleu-a1*))



;; ************************************* guiro - percus


(defun jeu-b11 (num val) ;al�atoire

  (share-gen choix (h �(0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)))
  (share-gen choix2 (h �(0 0 0 0 0 0 1 1 2)))
  (p-interactif (s (concat (lst (rnd �0 �5)) (sel (h �(0 1)) *n-traj-total-direct* *traj-zig-zag*)))
                (rep (sel (use-gen choix2) �125 �250 �250) (rnd �6 �16))  
                (sel (use-gen choix) (rep (hs �(23 28 30 32 38 39 40 41 42 43 44 45 46 47 48 49)) (rnd �60 �80)) (rnd �69 �96))  
                (sel (use-gen choix) (s �(120 85 85 85 85 85 85 85 85 85 85 85 85 85 85 )) �80)
                (funcall (sel (use-gen choix2) �24 �2 �4) 0 0 1 nil))
  (free-gen choix)
  (free-gen choix2))




(defun interaction-11   (e) 
  (when (is-ctrl-change2 e)
    (let ((num-ctrl (ctrl e))
          (val (valint e))
          (dat (date e)))
      
      (if (= num-ctrl 15) (progn (setq *mode* (not *mode*)) (vers-MAX (if *mode* 21 22))) 

          (when (and (> dat *last-interaction-date*) (chge-val? num-ctrl val)) 

          (setq *debut-interaction* t) 
                    

          (if *mode*
              (jeu-a11 num-ctrl val)  ;oiseaux
            (jeu-b11 num-ctrl val))   ;guiro- percus
          (setq *last-interaction-date* (+  dat 1000)))))))


;----------------------------

(defun partie-11 (deb fin) 
  (vers-MAX 21)
    (init)
    (init-interaction-11)

    (change-interaction 'interaction-11)

    (sequence-11 deb fin) 
    (player)
    
    (wait *duree-sequence-initiale*)

    (if *debut-interaction*
        (progn (StopPlayer *session-mf-player*) 
          (sleep 1))) 
   
    (phase2 (- *duree-sequence* *duree-sequence-initiale*))

    (StopPlayer *session-mf-player*)
    (wait *separation-partie*))

;----------------------------

;(partie-11 �0 (g (floor (/ *duree-sequence* 1000))))

(defun g-partie-11 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore �stime ctime etime reverse))
      (progn
        (partie-11 deb fin))))




;; *******************************************************************     S�quence 12 (ex 31) 
;;
;; violons  et calmes +oiseaux rares          Multi M10
;; 
;; *******************************************************************  ------------> pas d'arr�t du PLAYER 

(defun sequence-12 (deb fin) 
  
  (print-dialog "SEQUENCE 12")
  
  ;; ************* prog-change 
  (place-prog 0 m10 *out*)
  (place-tempo 0 60)


  (prorythme (rep (add �6 (h *totalzone*)) (h �(1 2 2 2 2 3 3 3 4 5))) deb fin   ;; d�buts d�call�s al�atoirement
             (gnotes)
             (h �( 1 2 2 2 4 6 8 10 12 14))
             (sel-al �5  �(1/4) �(1/2) �(1) �(3/2) �(3) �(5))
             (rnd �69 �125) 
             �70))

; ======================================

#|

(defun partie12 (deb fin)
  (init)
  (sequence-12 deb fin)
  (player))

(partie12 �1 (rnd �120 �180))

////////////////////////////////////////////////////////////////////////////////////////////////////////////////// 12
 
Jeu interactif 12: -----------------> pas d'arr�t du PLAYER 
      jeu-a:   violons aigus 
      jeu-b:   violons graves 

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
|#



(defun init-interaction-12 ()

  ; jeu
  (setq *bleu-b1* �1)        ;ctrl 21 --> canal
  (setq *violet-a1* �69)     ;ctrl 31 --> hauteur
; jeu b
  (setq *bleu-b1* �3)        ;ctrl 21 --> canal
  (setq *violet-b1* �56)     ;ctrl 31 --> hauteur

  (setq *mode* t))

;; ***************************************** violons aig�s 



(defun jeu-a12 (num val)

  (cond ((= num 21) (setq *bleu-b1* (scale val 0 6)))              ;ctrl 21 --> canal
        ((= num 31) (setq *violet-a1* (scale val 72 96))))         ;ctrl 31 --> hauteur  

  (p-interactif *bleu-b1*
                �10000 
                *violet-a1*  
                �60
                1))



;; ************************************* violons graves


(defun jeu-b12 (num val)

  (cond ((= num 21) (setq *bleu-b1* (scale val 0 6)))             ;ctrl 21 --> canal
        ((= num 31) (setq *violet-b1* (scale val 55 71))))        ;ctrl 31 --> hauteur  

  (p-interactif *bleu-b1*
                �10000  
                *violet-b1*  
                �60
                1))




(defun interaction-12   (e) 
  (when (is-ctrl-change2 e)
    (let ((num-ctrl (ctrl e))
          (val (valint e))
          (dat (date e)))
      
      (if (= num-ctrl 15) (progn (setq *mode* (not *mode*)) (vers-MAX (if *mode* 23 24))) 

          (when (and (> dat *last-interaction-date*) (chge-val? num-ctrl val)) 

          (if *mode*
              (jeu-a12 num-ctrl val)  ;violons aig�s 
            (jeu-b12 num-ctrl val))   ;violons graves 
          (setq *last-interaction-date* (+  dat 1000)))))))




;---------------------------- diff�re des autres parties (une seule phase diff�r� + interactif)




(defun partie-12 (deb fin) 
  (vers-MAX 23)
    (init)
    (init-interaction-12)

    (change-interaction 'interaction-12)

    (sequence-12 deb fin) 
    (player)
    
    (wait (+ *separation-partie* *duree-sequence*)))


;----------------------------

;(partie-12 �0 (g (floor (/ *duree-sequence* 1000))))

(defun g-partie-12 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore �stime ctime etime reverse))
      (progn
        (partie-12 deb fin))))



;; *******************************************************************     S�quence 13 
;;
;; violons + celli +oiseaux rares + fond          Multi M11
;; 
;; *******************************************************************  ------------> pas d'arr�t du PLAYER 

(defun sequence-13 (deb fin) 
  (print-dialog "SEQUENCE 13")

  (share-gen ffin fin)
  
  ;; ************* prog-change 
  (place-prog 0 m11 *out*)
  (place-tempo 0 60)

  ;; ************* fond 
  
    (prorythme �6 deb fin
               (gnotes :laccord *n-gaccord-demi* 
                       :lecar *n-gecar* ) 
               (h �(2 4 6 8 9 10))
               (h �((6) (8) (10) (12) (14) (20)))
               (h �(12 13 14)) 
               �70)
    


  ;; ************* oiseaux rares
  (prorythme (add �6 (h *totalzone*)) deb fin  
             (gnotes)
             (h �( 0 1 2 2 2 4 6 8 10 12))
             (sel-al �4 �(1/2) �(1) �(3/2) �(3) �(5))
             (rnd �24 �83) 
             (h �(60 70 80 90 100 110)))
  (free-gen ffin))

; ======================================

#|

(defun partie-13 (deb fin)
  (init)
  (sequence-13 deb fin)
  (player))

(partie-13 �1 (rnd �120 �180))

////////////////////////////////////////////////////////////////////////////////////////////////////////////////// 13
 
Jeu interactif 13: -----------------> pas d'arr�t du PLAYER 
      jeu-a:   violons  
      jeu-b:   celli  

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
|#



(defun init-interaction-13 ()

  ; jeu
  (setq *bleu-b1* �1)        ;ctrl 21 --> canal
  (setq *violet-a1* �69)     ;ctrl 31 --> hauteur
; jeu b
  (setq *bleu-b1* �3)        ;ctrl 21 --> canal
  (setq *violet-b1* �50)     ;ctrl 31 --> hauteur

  (setq *mode* t))


;; ***************************************** violons  



(defun jeu-a13 (num val)

  (cond ((= num 21) (setq *bleu-b1* (scale val 0 6)))              ;ctrl 21 --> canal
        ((= num 31) (setq *violet-a1* (scale val 55 96))))         ;ctrl 31 --> hauteur  

  (p-interactif *bleu-b1*
                �10000 
                *violet-a1*  
                �60
                1))



;; ************************************* celli 


(defun jeu-b13 (num val)

  (cond ((= num 21) (setq *bleu-b1* (scale val 0 6)))             ;ctrl 21 --> canal
        ((= num 31) (setq *violet-b1* (scale val 36 54))))        ;ctrl 31 --> hauteur  

  (p-interactif *bleu-b1*
                �10000  
                *violet-b1*  
                �80
                1))




(defun interaction-13   (e) 
  (when (is-ctrl-change2 e)
    (let ((num-ctrl (ctrl e))
          (val (valint e))
          (dat (date e)))
      
      (if (= num-ctrl 15) (progn (setq *mode* (not *mode*)) (vers-MAX (if *mode* 25 26))) 

          (when (and (> dat *last-interaction-date*) (chge-val? num-ctrl val)) 

          (if *mode*
              (jeu-a13 num-ctrl val)  ;violons  
            (jeu-b13 num-ctrl val))   ;celli  
          (setq *last-interaction-date* (+  dat 1000)))))))


;---------------------------- diff�re des autres parties (la phase 2 est particuli�re)

(defun partie-13 (deb fin) 
  (vers-MAX 25)
  (init)
  (init-interaction-13)

  (change-interaction 'interaction-13)

  (sequence-13 deb fin) 
  (player)
    
  (wait (+ *separation-partie* *duree-sequence*)))


;----------------------------

;  (partie-13 �0 (g (floor (/ *duree-sequence* 1000))))

(defun g-partie-13 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore �stime ctime etime reverse))
      (progn
        (partie-13 deb fin))))




