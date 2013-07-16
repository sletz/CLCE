;;==============================================================================
;;                     PROCESSUS ENIGMA PREMIERE MANIERE
;;                         PIERRE ALAIN JAFFRENNOU
;;                     Nouvelle version de d�cembre 1997
;;                     gnotes et evenement corrig�s le 4 02 2001
;;                     proryt modifi� le 09 02 2001
;;                     Ajout du param�tre optionnel ltextexpres dans gnotes et gaccent-tenu le octobre 2001
;;                     Ajout du param�tre optionnel pgchange dans prorythme le 25 10 01
;;                     Ajout du param�tre optionnel layer dans gnotes le 11 11 01   
;;                     Ajout du param�tre optionnel "controleur" le 27 F�vrier 2005 dans gnotes         
;;                     Ajout de modegliss le 4 mai 2007      
;;                     Ajout du key param�tre CONTROL-CHGE de GNOTES et N_GNOTES le 30 Septembre 2011    
;;                     Ajout du key parametre LLAYERSYNTHE de GNOTES et N_GNOTES le Avril 2012  
;;                     Ajout du key parametre LACCELERANDO de GNOTES et N_GNOTES le Mai 2012, modifi� novembre 2012 
;;                     Modification de n-evenement en septembre 2012
;;==============================================================================










;;==============================================================================
;;                           INITIALISATION DES CONSTANTES
;;
;;                dont configurer le pitch bend � 7 sur tous les canaux
;;==============================================================================



(defun init-constantes ()
  (setq  *pas-temps* 20 )
  (setq maxvol 127)
  (setq maj '(  2 2 1 2 2 2 1))
  (setq *bend* 7)
  (setq *bend-demi-ton* (/ 8192 *bend*)))



(init-constantes) 
  





;;==============================================================================
;;
;;                     OBJET DE LA COMPOSITION: GNOTES
;;
;;==============================================================================

;; TOUTES LES DUR�ES: SILENCE, PENTE ETC S'EXPRIMENT EN FRACTION DE NOIRE




;;------------------------------------------------------------------------------
;;                      GNOTES (prorythme)
;;------------------------------------------------------------------------------

;; gnotes: (&key (sil �0) 
;;                    (stac �1) 
;;                    (tril �0) (ecar �1) 
;;                    (proba �1) 
;;                    (laccord �(0)) (lecar �(0)) (lmaxaccord �nil)
;;                    (lmotifhaut �(0)) 
;;                    (ldurgliss �nil) (lhautgliss �nil) (modegliss �t)
;;                    (lapog �nil)
;;                    (ltransp �nil)
;;                    (ltextexpres �nil)
;;                    (larpege �nil) (arpegecar �0)
;;                    (layer �1)
;;                    (lcontrol �nil)
;;                    (lcontrol-chge �nil) 
;;                    (llayersynthe �nil)) 

;; Tous les param�tres sont des g�n�rateurs.

;; D�finie un g�n�rateur permettant l'�criture de notes MIDI 
;; suivant des param�tres de canal, port, groupe rythmique, hauteurs et v�locit�s
;; emprunt�s � PRORYTHME.
;; La variable haut peut �tre non enti�re (Micro-tonalit�): 
;; Exemple: la hauteur 60.5 signifie DO+1/4ton, 60.25 signifie DO+1/8ton



;; sil d�finit un silence (en fraction de noire) entre chaque note du groupe rythmique
;; qui ne sont pas incluses dans une structure n-olet. 
;; Ex :sil �1/2 --> silence de une croche entre chaque note

;; stac d�fini un coefficient de stacato jou� MIDI et not� ENIGMA
;; Si, par exemple, stac vaut 0.5, la dur�e de la note sera diminu�e de moiti�. 
;; Ex :stac �0.60 --> Dur�e des notes MIDI � 60%, marque de staccato FINALE
;; On peut aussi utiliser des valeurs > 1: 
;; Dans ce cas les notes MIDI (et non ENIGMA) seront recouvrantes, et le staccato ne sera pas not� 
;; Ex :stac �2 --> Doublement de la dur�e des notes MIDI, marque de staccato FINALE

;; tril d�fini un trille de valeur tril (not� ENIGMA)
;; (s'exprime en fraction de noire ou en valeur d�cimale: 1/4 ou 0.25 pour une double croche)
;; ecar est l'�cart du trille en demi-ton. 
;; Ex :tril �1/8 : ecar �1 --> Trille � la triple croche et au demi-ton

;; proba d�fini la probabilit� de jeu de la note:
;; proba=-1: suppression de la note et des �valuations des param�tres canal, hauteur, v�locit�
;; proba=0 suppression de la note. Evaluations des param�tres canal, hauteur, v�locit�
;; proba= autres valeurs, aucune suppression
;; Ex :proba �0 --> la note n'est pas �crite mais les param�tres sont �valu�s.

;; laccord d�fini un accord multicanaux midi (si chan + (car laccord) <0 la note n'est pas prise en compte)
;; lecar est pour chaque note de l'accord, l'�cart de hauteur � la hauteur de r�f�rence
;; Ex :laccord �(0 0 1) :lecar �(0 12 7) --> Si canal=5, accord � l'octave sur canal 5 et voix � la quinte sur canal 6

;; lhautgliss est la liste des �carts en hauteur en mode gliss�.
;; Un symbole est affich� sur la partition FINALE 
;; ldurgliss d�fini (en proportion) la dur�e des paliers de glisse
;; Si le param�tre ldurgliss ne figure pas, alors les paliers sont tous de dur�es �gales
;; Ex :lhautgliss �(0 4) :ldurgliss �(1 3) --> pas de gliss� pendant 1/4 de la dur�e, gliss� � la tierce majeure pendant les 3/4 restant


;; lapog d�fini les �carts de hauteur des apogiatures (en triple croche)
;; Les dur�es des apogiatures sont prises sur la dur�e de la note.
;; Ainsi, si cette dur�e est insuffisantes, les apogiatures ne sont pas appliqu�es
;; Les appogiatures sont �crites dans la partition FINALE
;; Ex :lapog �(-1 1) --> apogiatures -1/2 ton, +1/2 ton autour de la hauteur de  la note.

;; lmotifhaut divise la dur�e de la note en autant de parties que la longueur de  la liste "lmotifhaut"
;; Toutes les notes du motif sont de dur�es �gales
;; Chacune de ces nouvelles notes est transpos�e de la valeur correspondante de la liste "lmotifhaut" 
;; Ex :lmotifhaut �(1 2 3) --> la note est remplac�e par trois notes respectivement transpos�es de 1, 2 et 3 demi-tons

;; lmaxaccord liste des n� de canaux (de 0 � 31) utilis�s dans gnotes
;; Ce param�tre est � utiliser quand des changements de canaux interviennent au sein de goupes rythmiques de type n-olet
;; Ce param�tre produit un affichage correcte de la partition.

;; ltransp 
;; Premier cas: ltransp est une liste de deux valeurs de hauteur: les hauteurs de notes sont alors ramen�es (repli�es) par saut(s) d'octave(s)
;; � l'int�rieur de l'intervalle indiqu� si cela est possible. Sinon les hauteurs sont inchang�es (transpose MIDI et FINALE)
;; Ex :ltransp �( 60 72) --> les hauteurs de notes sont ramen�es dans l'intervalle Do3 - Do4 (bornes comprises)
;; Ce cas est utile pour �viter de g�n�rer des hauteurs hors la tessiture de l'instrument

;; Second cas: ltransp est une liste de 1 valeur: La valeur MIDI est transpos�e, l'�criture FINALE est inchang�e
;; Ex :ltransp �(-12) --> les hauteurs MIDI sont transpos�es de -1 octave
;; Ce cas est utile lorsque les �chantillons sont transpos�s sur le clavier

;; Troisi�me cas: ltransp est un nombre : La valeur MIDI est inchang�e, l'�criture FINALE est transpos�e
;; Ex :ltransp �2 --> les hauteurs MIDI sont inchang�es, la partition est transpos�e de 2 demi-tons
;; Ce cas est utile lorsque pour les instruments transpositeurs.

;; ltextexpres permet d'afficher un TEXTE EXPRESSION dans finale: 
;; Ex :ltextexpres �(((60 84)) (35)) --> affiche l'expression n� 35 pour la premi�re note se trouvant dans l'intervallle (60 84) 
;; Ce champ permet entre autre d'indiquer des modes de jeu, particuli�rement lorsque plusieurs jeux d'un m�me instrument
;; constituent un seul et m�me instrument MIDI (au sens du S6000). Ce champ peut se conjuguer avec ltransp. 
;; Une seule expression est �crite pour un accord � l'int�rieur d'un m�me canal MIDI.
;; Exemple: dans l'intervalle (36 59) violon arco et dans l'intervalle (60 84) violon piz pour le m�me canal MIDI, on �crira:
;; :ltextexpres �(((36 59) (60 84)) (48 47)) o� 48 est la r�f�rence de l'expression arco et 47 celle de piz. Si la hauteur est hors
;; ces intervalles, rien n'est �crit.

;; larpege et arpegecar permet de r�aliser des arp�ges MIDI ascendants ou descendants. 
;; Pas de repr�sentation FINALE actuellement
;; larpege = liste des �carts de hauteur relativement � la note la plus basse de l'arpege (ex: �(0 4 7))
;; arpegecar = d�calage temporel dechaque note de l'arpege exprim� en unit� de noire 
;; (ex: �1/4) si > 0 arpege ascendant, si < 0 arpege descendant
;; Ex :larpege �(0 4 7) :arpegecar �-1/8 --> arpege majeur descendant

;; layer indique un num�ro de layer de 1 � 4 pour l'�criture polyphonique dans FINALE (=1 par d�fault)   11 11 2001
;; Ex :layer �3 --> la partition est �crite sur le layer n� 3

;; lcontrol Permet la g�n�ration de controleurs MIDI. Pas d'effet sur la partition FINALE. Ajout du 28 F�vrier 2005
;; Le param�tre est une liste de type: (gnum�ro_de_controleur gvaleur_de_controleur gincr�ment_des_valeurs_de_controleur)
;; g�n�re les valeurs du controleur correspondant sur le canal de(s) la note(s) associ�e(s) pour chaque valeur de dur�e de note
;; en cas d'accord multi-canal, le controleur est appliqu� � chaque canal
;; en cas d'accord mono-canal, le controleur n'est appliqu� qu'une fois sur le canal correspondant
;; Pour l'�valuation des g�n�rateurs, le d�coupage par TRILLE ou MOTIF n'est pas pris en compte
;; ex: :lcontrol �(�7 (floo(i �0 �127)) �1) --> contoleur de volume variant pour chaque note de 0 � 127 par pas minimum de 1.

;; lcontrol-chge (numcontroleur, v1, v2 n1, n2) place le controleur de n� numcontroleur avec la valeur v1 puis n1 notes apr�s
;; place le controleur de n� numcontroleur avec la valeur v2, puis n2 notes apr�s replace la valeur v1 etc ...
;; les intervalles �ventuels entre les occurences rythmiques ne comptent pas comme des notes.
;; Un usage typique de cette fonctionnalit� est l'usage de la p�dale: �(64 127 0 4 2) : p�dale abaiss�e pendant 4 notes, relev�e pendant 2 notes


;; llayersynthe: liste de liste (hauteur, n�d'expression finale) permet de basculer d'un layer � l'autre � l'int�rieur d'un instrumet "machfive" 
;; et d'�crire le nom du layer correspondant dans la partition finale
;; typiquement un instrument "violon piz" de "machfive" peut comporter les 3 layers suivants:
;; "normal" command� par do2 (48)
;; "sec" command� par do#2 (49)
;; "bartok" command� par r�2 (50)
;; dans ces conditions :llayersynthe �((50 47)) la note de hauteur 50 fera basculer le violon en mode "bartok" 
;; et �crira "piz bartok" dans la partition finale (47 est le n� de l'expression "piz bartok" dans finale)
;; Ce param�tre s'applique m�me aux accords multicanaux suivant la valeur de "chan" et de "laccord". Exemples
;; chan=10 et :laccord �(1 1 8) :llayersynthe �((60 36) (72 45)) -->(60 36) s'applique au canal 11 et (72 45) au canal 18
;; chan=10 et :laccord �(1 1 8) :llayersynthe �((60 36)) -->(60 36) s'applique au canal 11 et rien n'est appliqu� au canal 18
;; chan=10 et :laccord �(1 1 8) :llayersynthe �(nil (72 45)) -->rien ne s'applique au canal 11 et (72 45) est appliqu� au canal 18
;; la liste de :llayersynthe est appliqu�e terme � terme � la liste :laccord expurg� des doublons (ici: (1 8)) 
;; si :laccord �(8 1 8 8 1) alors la liste de :llayersynthe est appliqu�e � (8 1)
;; le terme layer utilis� ici n'a rien � voir avec le concept layer de finale.

;; laccelerando: permet de g�n�rer un accelerando ou un ritardo avec �criture d'un symbole dans FINALE
;; exemple: �(1/8 1 -1 (floo (i �0 �12)) 120))
;; decrescendo de la triple croche (1/8) � la noire (1/8) pendant la dur�e de la note fournie par PRORYTHME
;; le troisi�me param�tre indique crescendo si positif, decrescendo si n�gatif, volume �gal si 0
;; le quatri�me param�tre donne les �carts en hauteur relativement � la note de base fournie par PRORYTHME
;; le cinqui�me param�tre indique la pr�sence d'un accent sur la premi�re note de l'accelerando (ici une v�locit� de 120)
;; pour ne pas avoir d'accent il faut �crire un accent de valeur 0.
;; accelerando, crescendo (ou decrescendo) et accent sont not�s par des symboles dans la partition FINALE
;; si il y a pr�sence de grace note � l'attaque, ces derni�res seront aussi accentu�es le cas �ch�ant
;; ATTENTION: tous les param�tres sont des nombres sauf le 4-�me (�carts de hauteur) qui est un g�n�rateur
;; ajout de Mai 2012 modifi� novembre 2012


#|
 Staccato al�atoire avec trille � la triple croche
 (prorythme �0 �0 �8
           (gnotes :stac (h �(1 1 0.5)) :tril �1/8 :ecar (h �(1 1 1 5)))
           �0
           �(1 1 1 + 1/2)
           (h �(65 61 67 48 69))  
           (rnd �100 �127))

 Micro-tonalit� et fusion de rythmes complexes
  (prorythme �0 �0 �20
           (gnotes)
           �0
           (fusion (sel-al �3 �(2) �trio2 �(2/3c (-1/2 1)) �(2/3c (-1/2 1/2 1/2)))
                   (sel-al �11 �(4) �sy2 �sy2 �ttrio1 
                           �ttrio2 �ddc2 �ddc3 �ddc1 �qquin4 �qquin5 �(1) �(1/2 -1/2)))
           (s �(65 65.25 65.50 65.75 66 48 ))  
           (rnd �100 �127))

 Probabilit� de jeu et trille variable (1/2 ton ou 5/2 ton) sur rythme de triolet de croche
  (prorythme �0 �0 �12
             (gnotes :proba (alt �1 �1 �1 �0)  :tril �1/16 :ecar (h �(1 1 1 5)))
             �0
             �ttrio2
             (s �(60 62 64 66 67 69 72))
             (rnd �100 �127))

 Probabilit� de jeu, trille variable en accord et apogiatures
 ( prorythme �0 �0 �12 (gnotes :lapog �(1 2 -1)   
                                :laccord  �( 0 2 2)
                                :lecar �(-12 0 7 12 19)
                                :tril (sel-al �1 �1/8 �0) 
                                :ecar �1 
                                :proba (h�(1 1 1 0)))
              �1
              (fusion �(1 1/2 1/2) (sel-al �2 �(1) �(3) �(0)))
              (rnd �36 �84)
              �100)
|#




;;================================================================= 9 septembre 2011 introduction du key parametre control-chge dans gnotes -------->



(defun gnotes (&key (sil �0) 
                    (stac �1) 
                    (tril �0) (ecar �1) 
                    (proba �1) 
                    (laccord �(0)) (lecar �(0)) (lmaxaccord �nil)
                    (lmotifhaut �(0)) 
                    (ldurgliss �nil) (lhautgliss �nil)
                    (lapog �nil)
                    (ltransp �nil)
                    (ltextexpres �nil)
                    (larpege �nil) (arpegecar �0)
                    (layer �1)
                    (lcontrol �nil)
                    (lcontrol-chge �nil)  ;; ajout le 11 septembre 2011. Permet l'�criture de controleurs � deux valeurs comme les p�dales  (exemple: eighth)
                    (llayersynthe �nil)  ;; ajout le 9 avril 2012. Permet le changement de layer d'un echantilloneur (machfive2) avec �criture de l'instrument dans finale  
                                            ;(exemple: :llayersynthe �((60 37) (64 42)) 
                    (laccelerando �nil) (ehaccelerando �nil))  ;; exemple :laccelerando �(1/8 1 -1 (floo (i �0 �12)) 120)) ajout de Mai 2012, modifi� novembre 2012
                
  
  (g (evenement sil stac tril ecar proba laccord lecar lmaxaccord lmotifhaut ldurgliss lhautgliss lapog ltransp ltextexpres larpege arpegecar layer lcontrol lcontrol-chge llayersynthe laccelerando ehaccelerando)))

#|
;;=======================================================

;; exemple typique, controleur 64 = p�dale de piano
;; la p�dale est enfonc�e pendant deux notes, relev�e pendant une note
;; l'intervalle �1 entre les occurences du rythme n'est^pas compt� comme une note

(defun exemple ()  
  (place-tempo 0 90)
  ( prorythme �0 �0 �16
              (gnotes)
              �1
              ;�(1)
             �(2/3n(1 1 1) + 1)
               (s �(60 61 62 63 64 65 66 67 68 69 70))
              �120
              �nil
              :controle-chge �(64 127 0 2 1)))

;;=======================================================

; 02 03 05 Partout dans evenement --> modif de vel en �vel (cf sequence) (sauf vel=0 pour proba=0) 
; 02 03 05 Partout dans evenement --> modif de chan en �chan (cf sequence) (sauf chan=0 pour proba=0)
(defun evenement (sil stac tril ecar proba laccord lecar lmaxaccord lmotifhaut ldurgliss lhautgliss lapog ltransp ltextexpres larpege arpegecar layer lcontrol) 
  
  #'(lambda (chan dur haut vel pgchange stime ctime etime reverse) ;; le 22 10 01 introduction des pgchg
      (if (> (SPECIAL-EVALUATE dur) 0)   ; 30 08 01
        (let  ((p �proba)) 
          (cond ((= p -1) (accord ctime �chan pgchange (changder dur) �haut �vel �sil �stac �tril   
                                  �ecar �laccord �lecar �lmaxaccord �lmotifhaut �ldurgliss �lhautgliss �lapog nil nil �larpege �arpegecar �layer �lcontrol))
                ((= p 0) (accord ctime �chan pgchange (changder dur) 10 0 �sil �stac �tril
                                 �ecar �laccord �lecar �lmaxaccord �lmotifhaut �ldurgliss �lhautgliss �lapog nil nil �larpege �arpegecar �layer �lcontrol))
                (t (accord ctime �chan pgchange dur �haut �vel �sil �stac �tril �ecar �laccord �lecar �lmaxaccord �lmotifhaut
                           �ldurgliss �lhautgliss �lapog �ltransp �ltextexpres �larpege �arpegecar �layer �lcontrol))))
        (accord ctime �chan pgchange dur �haut �vel �sil �stac �tril �ecar �laccord �lecar �lmaxaccord �lmotifhaut
                �ldurgliss �lhautgliss �lapog nil nil �larpege �arpegecar �layer �lcontrol))))
|#



#|
;; modifs le 30septembre 2011 pour introduire les controleurs dans gnotes


(defun chgtlast (l a1)
 (reverse(cons a1 (cdr(reverse l)))))

(defun chgt-elements (l a1 a2 a3)
  (chgtlast (append (list a1 a2) (cddr l)) a3))

;(chgt-elements '(1 2 3 4) 40 50 60) --> (40 50 3 60)
|#



(defun evenement (sil stac tril ecar proba laccord lecar lmaxaccord lmotifhaut ldurgliss lhautgliss lapog ltransp ltextexpres larpege arpegecar layer lcontrol lcontrol-chge llayersynthe laccelerando ecar-accelerando) 
  
  #'(lambda (chan dur haut vel pgchange stime ctime etime reverse) ;; le 22 10 01 introduction des pgchg

      (let* ((canal �chan)
             (lac �laccord)
             (ctrlchge �lcontrol-chge)
             (etat-ctrl-chge (aref *etat-ctrl-chge2* canal)))
 
 
        (if dur
            (progn
   
              (if ctrlchge
                  (if etat-ctrl-chge
                      (let ((vn (first etat-ctrl-chge))
                            (nbre (second etat-ctrl-chge))
                            (pedale (third etat-ctrl-chge))
                            (val 0))
                        (if (>= vn nbre)
                            (progn
                              (if pedale
                                  (progn
                                    (setq val (second ctrlchge))
                                    (setq nbre (fourth ctrlchge)))
                                (progn
                                  (setq val (third ctrlchge))
                                  (setq nbre (fifth ctrlchge))))
                              (setq pedale (not pedale))
                              (make-controle-chge canal lac val (first ctrlchge))
                              (setf (aref *etat-ctrl-chge2* canal) (list 1 nbre pedale)))

                          (setf (aref *etat-ctrl-chge2* canal) (list (+ 1 vn) nbre pedale))))


                    (progn
                      (make-controle-chge canal lac (second ctrlchge) (first ctrlchge))
                      (setf (aref *etat-ctrl-chge2* canal) (list 1 (fourth ctrlchge) nil))))


                (progn
                  (if (aref *etat-ctrl-chge2* canal)
                      (make-controle-chge canal lac (third ctrlchge) (first ctrlchge)))
                  (setf (aref *etat-ctrl-chge2* canal) nil)))



                  (let*  ((p �proba)
                         (llacelerando �laccelerando)
                         (mode (if llacelerando (fifth llacelerando)
                                 nil))
                         (ecar-h-acceler (if mode �ecar-accelerando     ; �valuation avec temps global (d�cembre 2012)
                                           ecar-accelerando)))          ; �valuation ult�rieure avec temps local (d�cembre 2012)



              (if (> (SPECIAL-EVALUATE dur) 0)   ; 30 08 01

   
                    (cond ((= p -1) (accord ctime canal pgchange (changder dur) �haut �vel �sil �stac �tril   
                                            �ecar �laccord �lecar �lmaxaccord �lmotifhaut �ldurgliss �lhautgliss �lapog nil nil �larpege �arpegecar �layer �lcontrol �llayersynthe llacelerando ecar-h-acceler))
                          ((= p 0) (accord ctime canal pgchange (changder dur) 10 0 �sil �stac �tril
                                           �ecar �laccord �lecar �lmaxaccord �lmotifhaut �ldurgliss �lhautgliss �lapog nil nil �larpege �arpegecar �layer �lcontrol �llayersynthe llacelerando ecar-h-acceler))
                          (t (accord ctime canal pgchange dur �haut �vel �sil �stac �tril �ecar �laccord �lecar �lmaxaccord �lmotifhaut
                                     �ldurgliss �lhautgliss �lapog �ltransp �ltextexpres �larpege �arpegecar �layer �lcontrol �llayersynthe llacelerando ecar-h-acceler)))


                (accord ctime canal pgchange dur �haut �vel �sil �stac �tril �ecar �laccord �lecar �lmaxaccord �lmotifhaut
                        �ldurgliss �lhautgliss �lapog nil nil �larpege �arpegecar �layer �lcontrol �llayersynthe llacelerando ecar-h-acceler))))))))







;;==============================================================================
;;
;;                            CONCEPT DE RYTHME
;;
;;==============================================================================
#|

 RYTHM-SYMB est une liste de rythmes �l�mentaires symboliques du style
 (1 + 2/3c (1/2 + 2/3dc (1/2 1/4) + -1/2) 1 -2 1)
 ou encore:
 (1 + 2/3c (1/2 + 2/3dc (1/2 1/4) + 1/2) -2 4)

 Le signe - indique un silence



 CODE-RYTHME et PREFIXPLUS sont deux fonctionS d�finies dans ENIGMA.LISP
 qui codent les RYTHMES SYMBOLIQUES:
 
 (code-rythme '(1 + 2/3c (1/2 + 2/3dc (1/2 1/4) + 1/2) -2 4))
 --> (1 + (* (FDUR 2 3 D-C) 1/2) + (* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) 1/2) 
     (* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) 1/4) + (* (FDUR 2 3 D-C) 1/2) -2 4)

prefixplus, code le rythme en notation pr�fix�e

 (prefixplus (code-rythme '(1 + 2/3c (1/2  2/3dc (-1/2 1/4) + 1/2) -2 4)))
 --> ((+ 1 (* (FDUR 2 3 D-C) 1/2)) (* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) -1/2) 
     (+ (* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) 1/4) (* (FDUR 2 3 D-C) 1/2)) -2 4)

|#



;; ------------------------------------------------------------------------------
;; DUR-MIDI est une fonction qui calcule la dur�e en noire 
;; d'un rythme symbolique exprim� en notation pr�fix�e
;; ------------------------------------------------------------------------------

(defun dur-midi (rythme &optional (tot 0))
  (if rythme
    (progn
      (setq tot (+ tot (abs (special-evaluate (first rythme)))))
      (dur-midi (cdr rythme) tot))
    (* tot 1)))
#|

 Exemple:

 (dur-midi '((+ 1 (* (FDUR 2 3 D-C) 1/2)) (* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) -1/2) 
     (+ (* (FDUR 2 3 D-C) (FDUR 2 3 D-DC) 1/4) (* (FDUR 2 3 D-C) 1/2)) -2 4))
 --> 8
|#
;; ------------------------------------------------------------------------------






;;==============================================================================
;;
;;                            PRORYTHME
;;
;;==============================================================================



;;==============================================================================
#| PRORYTHME (special-chan debpro finpro
                               objet
                               interval
                               distribution-rythme-symb
                               distribution-hauteurs
                               distribution-velocites 
                               &optional (pgchange �nil))

;; ensemble d'objets d�finis sur le canal d�livr�es par le g�n�rateur SPECIAL-CHAN
;; entre les dates t1 et t2 d�livr�es par les g�n�rateurs DEBPRO et FINPRO.
;; OBJET: G�n�rateur d'objets musicaux
;; INTERVAL: G�n�rateur temporel d�finissant un intervalle entre les occurences des objets
;; DISTRIBUTION-RYTHME-SYMB: g�n�rateur temporel de rythmes symboliques (voir plus haut)
;; DISTRIBUTION-HAUTEURS: g�n�rateur temporel de hauteurs
;; DISTRIBUTION-VELOCITES: g�n�rateur temporel de v�locit�s

;; :PGCHANGE param�tre optionnel de g�n�ration de program-change

;; special-chan int�gre chan et port:
;; si special-chan=18 alors port=1 et chan=2
;; special-canal de 0 � 15 --> canal de 0 � 15 et port 0
;; special-canal de 16 � 31 --> canal de 0 � 15 et port 1

;; NOTES SUR LE PARAM�TRE OPTIONNEL pgchange
;; Il permet de changer dynamiquement l'instrument MIDI 
;; et de noter une expression (par exemple le nom de l'instrument) dans la partition FINALE.
;; Cette expression est reper�e par son n� dans la liste des expressions du programme FINALE.

;; pgchange est une liste de liste de la forme:
;; ((num-canal1 num-instrument1 num-expression1) (num-canal2 num-instrument2 num-expression2) etc ...)
;; o� num-canal est not� de 0 � 31
;; o� num-instrument1 est le n� de pgchg de l'instrument
;; o� num-expression1 est le n� de l'expression que finale doit afficher

;; Si dans pgchange se trouve un canal ne figurant pas dans la note ou l'accord
;; un message d'alerte est envoy� mais les calculs ne sont pas arr�t�s.

;; pgchange s'applique toujours � la structure de base du prorythme ind�pendament des optionnelles de gnotes

 exemples:

(prorythme �0 �0 �10
           (gnotes)
           �1/4
           �(1/4)
           (rnd �60 �64)
           �100)

(prorythme  �0 �0 �4 
            (gnotes)
            �0
            (s �((1/4) (1) (1/2)))
            (i �60 �67)
            �100)


(prorythme �0 �0 �20 
           (gnotes)
           �1
           (alt �(4) �(3))
           �72
           �100)

(prorythme  (h �(0 1 2 9 14 13 12)) �0 �30
            (gnotes) �0
            (h �((0) (0) (0) (0) (0.05) (0.1) (0.1) (0.1) (1.33) (1.66) (2.00) (1.50)))
            (h �(60 75 62 63 66 69 71 80))
            (h �(60 60 60 60 120)))
|#
;;==============================================================================

(defvar *liste-rythme*) ; ------------ rajout 04 01 2001
(defvar *liste-canal*)

(defun ote-double (l)
  (if (null l) nil
      (cons (car l) (ote-double (delete (car l) l)))))



#|
;;=======================================================
;; special-chan int�gre chan et port:
;; si special-chan=18 alors port=1 et chan=2


(defun prorythme (special-chan debpro finpro
                               objet
                               interval
                               distribution-rythme-symb
                               distribution-hauteurs
                               distribution-velocites 
                               &optional (pgchange �nil))   ;; 22 10 01 mise en place des pgchange
  (let* ((stime (floor (* (funcall debpro 0 0 1 nil) noi)))
         (etime (floor (* (funcall finpro 0 1 1 nil) noi))))
    (p-abs stime) 
    ;(print (list stime etime))
    (proryt special-chan
            objet
            interval
            distribution-rythme-symb
            distribution-hauteurs
            distribution-velocites
            pgchange
            (date?) stime etime nil))
  (format t "~%**********  fin de prorythme � la date ~S ~%" (/ (date?) noi)))
|#


;;======================================================= 

;; special-chan int�gre chan et port:
;; si special-chan=18 alors port=1 et chan=2

(defun prorythme (special-chan debpro finpro
                               objet
                               interval
                               distribution-rythme-symb
                               distribution-hauteurs
                               distribution-velocites 
                               &optional (pgchange �nil))   
  (let* ((stime (floor (* (funcall debpro 0 0 1 nil) noi)))
         (etime (floor (* (funcall finpro 0 1 1 nil) noi))))




    (p-abs stime) 
    (proryt special-chan
            objet
            interval
            distribution-rythme-symb
            distribution-hauteurs
            distribution-velocites
            pgchange
            (date?) stime etime nil))
  (format t "~%**********  fin de prorythme � la date ~S ~%" (/ (date?) noi)))





;;=======================================================

(defun proryt (chan
               objet 
               interval 
               rythme-symb 
               hauteur 
               velocite
               pgchange
               ctime stime etime reverse) 
  (if (< ctime etime) 
      (let* ((rs �rythme-symb)
             (rythm-symb (prefixplus (code-rythme rs)))
             (dat (+ ctime (* noi (dur-midi rythm-symb))))
             (inter �interval))
    ;(format t "PROCESSUS rythme=~S ~%" rythm-symb)
        (when  ( <= dat etime)
          ;;(format t "PROCESSUS dat~S ~%" (floor (/ dat noi)))
          ;;(format t "PROCESSUS rythme-non-decod�=~S ~%" rs)
          (paj-sequence chan objet rythm-symb hauteur velocite pgchange ctime stime etime reverse)
          (p-rel (* noi (abs inter)))
          (proryt chan objet interval rythme-symb hauteur velocite pgchange (date?) stime etime reverse )))))


;;=======================================================




(defun paj-sequence (cha objet rythm-symb haut vel pgchange ctime stime etime reverse)
  
  (when rythm-symb
    ;(print (list "paj-sequence date rythm-symb: " (/ (date?) noi) rythm-symb))
    (let ((ryt-symb (list (first rythm-symb))))
      ;(format t "dans sequence  haut=~S date=~S rythme=~S~%" haut (/ (date?) noi) ryt-symb)
      (let (
            (pgchg �pgchange))
        (if ( > (SPECIAL-EVALUATE (first ryt-symb)) 0)     ; 30 08 01
            (funcall �objet  cha (first ryt-symb) haut vel pgchg stime ctime etime reverse)   ;change �vel en vel 02 03 05, supprime (let ((canal �cha))
          (funcall �objet  cha (first ryt-symb) �20 �0 pgchg stime ctime etime reverse)))
      (paj-sequence cha objet (cdr rythm-symb) haut vel pgchange (date?) stime etime reverse))))
















;;==============================================================================
;;
;;                           NOUVEAU PRORYTHME
;;    Les 4 param�tres chan dur haut vel, ne sont plus pass�s en externe
;;
;;==============================================================================
;;
;;                        N-PRORYTHME (DEB FIN INTERVAAL OBJET)
;;
;;                        Version D�cembre 1997 / PAJ
;;                        n-gnotes et n-evenement corrig�s le 04 02 2001
;;                        n-proryt modifi� le 09 02 2001
;;                        n-gaccent-tenu ajout� le 06 07 2001
;;                        Modification du 18 octobre 2001: cr�ationd'un 7�me champ dans les listes de notes en direction de FINALE
;;                                                       : Ce champ code les TEXTES EXPRESSIONS de FINALE
;;                        Ajout du param�tre optionnel ltextexpres dans n-gnotes et n-gaccent-tenu le octobre 2001
;;                        Ajout du param�tre optionnel pgchange dans n-gnotes le 27 10 01                     
;;

#| EX:
 ( n-prorythme �0 �20 (h �(0 1/4))
      (n-gnotes �0 �(4) (h �(72 67 66 73)) �90))

;;==============================================================================


|#

(defun n-prorythme ( debpro finpro interval objet)
  (let* ((stime (floor (* (funcall debpro 0 0 1 nil) noi)))
         (etime (floor (* (funcall finpro 0 1 1 nil) noi))))
    (p-abs stime)
    (new-proryt objet
                interval
                (date?) stime etime nil ))
  (format t "~%*************  fin de new-prorythme � ~S ~%" (/ (date?) noi)))
    
 



;; nouveau au 09 02 2001
(defun new-proryt (  objet 
                     interval 
                     ctime stime etime reverse ) 
  (let ((inter �interval)) 
    (when  ( < ctime etime)
      ;(format t "~%*************  new-proryt � ~S ~%" (/ (date?) noi))
      �objet  
      (if (<= (+ (date?) (* noi (abs inter))) etime) 
          (progn
            (p-rel (* noi (abs inter))) ;(print (list ctime (date?)))
            (new-proryt objet interval (date?) stime etime reverse ))))))








;;==============================================================================
;;                  N-GNOTES (CANAL RYTHME HAUTEUR VELOCITE)
;;==============================================================================

;; attention :ltransp utilis� avec :laccord  !!


(defun n-gnotes (chan rythme haut vel
                      &key (sil �0) 
                      (stac �1) 
                      (tril �0) (ecar �1) 
                      (proba �1) 
                      (laccord �(0)) (lecar �(0)) (lmaxaccord �nil) 
                      (lmotifhaut �(0)) 
                      (ldurgliss �nil) (lhautgliss �nil)
                      (lapog �nil)
                      (ltransp �nil)
                      (ltextexpres �nil)
                      (pgchange �nil)                   ;; ajout des pgchange le 27 10 01 sous la forme: 
                      ;; '((num-canal1 num-pgchg1 num-expression1) (num-canal2 num-pgchg2 num-expression2) ...)
                      (larpege �nil) (arpegecar �0)    ;; si arpegecar > 0 arpege montant sinon si < 0 arpege descendant) 
                      (layer �1)
                      (lcontrol �nil)
                      (lcontrol-chge �nil)   ;; ajout le 11 septembre 2011. Permet l'�criture de controleurs � deux valeurs comme les p�dales  (exemple: :lcontrol-chge �(64 127 0 3 2))
                      (llayersynthe �nil)  ;; ajout le 9 avril 2012. Permet le changement de layer d'un echantilloneur (machfive2) 
                                            ;;avec �criture de l'instrument dans finale  (exemple: :llayersynthe �((60 37) (64 42)) 
                       (laccelerando �nil) (ehaccelerando �nil))  ;; exemple :laccelerando �(1/8 1 -1 (floo (i �0 �12)) 120)) ajout de Mai 2012, modifi� novembre 2012

  #'(lambda (stime ctime etime reverse)
      (let* ((rs �rythme)
             (rythm-symb (prefixplus (code-rythme rs)))
             (dat (+ ctime (* noi (dur-midi rythm-symb))))) ;(format t "n-gnotes rythme=~S dur�e=~S date=~S ~%" rythm-symb (SPECIAL-EVALUATE rythm-symb) ctime)
        (if  ( <= dat etime)
            (n-evenement chan rythm-symb haut vel
                         sil stac tril ecar proba laccord lecar lmaxaccord lmotifhaut ldurgliss lhautgliss lapog ltransp ltextexpres pgchange larpege arpegecar layer lcontrol lcontrol-chge llayersynthe laccelerando ehaccelerando (date?) stime etime reverse)
          
          (p-abs etime)))))

#|

;; Voir la signification des key param�tres de n-gnotes dans gnotes ci-dessus
;; le code ci-dessous a �t� remplac� en septembre 2011



(defun n-evenement (chan rythme haut vel
                         sil stac tril ecar proba laccord lecar lmaxaccord lmotifhaut ldurgliss lhautgliss lapog ltransp ltextexpres pgchange larpege arpegecar layer lcontrol
                         ctime stime etime reverse)
  (print (list "n-evenement" rythme ctime))
  
  (when rythme
    
    (let ((canal �chan)
          (lma �lmaxaccord)
          (lac �laccord)
          (lec �lecar)) 
      
      (print (list  "n-evenement" �lcontrol))
      
      (if (> (SPECIAL-EVALUATE (first rythme)) 0)     ; 30 08 01
        (let  ((p �proba)) 
          (cond ((= p -1) (accord ctime canal nil (changder (first rythme)) �haut �vel �sil �stac �tril
                                  �ecar lac lec lma �lmotifhaut �ldurgliss �lhautgliss �lapog nil nil �larpege �arpegecar �layer �lcontrol))
                ((= p 0) (accord ctime canal nil (changder (first rythme)) 10 �vel �sil �stac �tril
                                 �ecar lac lec lma �lmotifhaut �ldurgliss �lhautgliss �lapog nil nil �larpege �arpegecar �layer �lcontrol))
                (t (accord ctime canal �pgchange (first rythme) �haut �vel �sil �stac �tril �ecar lac lec lma �lmotifhaut
                           �ldurgliss �lhautgliss �lapog �ltransp �ltextexpres �larpege �arpegecar �layer �lcontrol))))
        (accord ctime canal nil (first rythme) �haut �vel �sil �stac �tril �ecar lac lec lma �lmotifhaut
                �ldurgliss �lhautgliss �lapog nil nil �larpege �arpegecar �layer �lcontrol)))
    
    (n-evenement chan (cdr rythme) haut vel
                 sil stac tril ecar proba laccord lecar lmaxaccord lmotifhaut ldurgliss lhautgliss lapog ltransp ltextexpres pgchange larpege arpegecar layer lcontrol
                 (date?) stime etime reverse)))
|#


;; nouveau au 29 septembre 2011 pour introduite les controleurs



#|
(defun n-evenement (chan rythme haut vel
                         sil stac tril ecar proba laccord lecar lmaxaccord lmotifhaut ldurgliss lhautgliss lapog ltransp ltextexpres pgchange larpege arpegecar layer lcontrol lcontrol-chge llayersynthe laccelerando ctime stime etime reverse)

  (let* ((canal �chan)
         (lma �lmaxaccord)
         (lac �laccord)
         (lec �lecar)
         (ctrlchge �lcontrol-chge)
         (etat-ctrl-chge (aref *etat-ctrl-chge* canal))) (print (list "n-evenement lecar=" lec))
    

    (if rythme
        (progn
          (let ((lacceler �laccelerando))

            (if lacceler 
            (progn
              (setq haut �0)
              (setq vel �0)))
   
          (if ctrlchge
              (if etat-ctrl-chge
                  (let ((vn (first etat-ctrl-chge))
                        (nbre (second etat-ctrl-chge))
                        (pedale (third etat-ctrl-chge))
                        (val 0))
                    (if (>= vn nbre)
                        (progn
                          (if pedale
                              (progn
                                (setq val (second ctrlchge))
                                (setq nbre (fourth ctrlchge)))
                            (progn
                              (setq val (third ctrlchge))
                              (setq nbre (fifth ctrlchge))))
                          (setq pedale (not pedale))
                          (make-controle-chge canal lac val (first ctrlchge))
                          (setf (aref *etat-ctrl-chge* canal) (list 1 nbre pedale)))

                      (setf (aref *etat-ctrl-chge* canal) (list (+ 1 vn) nbre pedale))))


                (progn
                  (make-controle-chge canal lac (second ctrlchge) (first ctrlchge))
                  (setf (aref *etat-ctrl-chge* canal) (list 1 (fourth ctrlchge) nil))))


            (progn
              (if (aref *etat-ctrl-chge* canal)
                  (make-controle-chge canal lac (third ctrlchge) (first ctrlchge)))
              (setf (aref *etat-ctrl-chge* canal) nil)))


          (if (> (SPECIAL-EVALUATE (first rythme)) 0)     ; 30 08 01
              (let  ((p �proba))
            
                (cond ((= p -1) (accord ctime canal nil (changder (first rythme)) �haut �vel �sil �stac �tril
                                        �ecar lac lec lma �lmotifhaut �ldurgliss �lhautgliss �lapog nil nil �larpege �arpegecar �layer �lcontrol �llayersynthe lacceler))
                      ((= p 0) (accord ctime canal nil (changder (first rythme)) 10 �vel �sil �stac �tril
                                       �ecar lac lec lma �lmotifhaut �ldurgliss �lhautgliss �lapog nil nil �larpege �arpegecar �layer �lcontrol �llayersynthe lacceler))
                      (t  (accord ctime canal �pgchange (first rythme) �haut �vel �sil �stac �tril �ecar lac lec lma �lmotifhaut
                                  �ldurgliss �lhautgliss �lapog �ltransp �ltextexpres �larpege �arpegecar �layer �lcontrol �llayersynthe lacceler))))
            (accord ctime canal nil (first rythme) �haut �vel �sil �stac �tril �ecar lac lec lma �lmotifhaut
                    �ldurgliss �lhautgliss �lapog nil nil �larpege �arpegecar �layer �lcontrol �llayersynthe lacceler))
    
          (n-evenement chan (cdr rythme) haut vel
                       sil stac tril ecar proba laccord lecar lmaxaccord lmotifhaut ldurgliss lhautgliss lapog ltransp ltextexpres pgchange larpege arpegecar layer lcontrol lcontrol-chge llayersynthe laccelerando (date?) stime etime reverse))))))

|#


;; ********** modif septembre 2012 ***************************************
;; on �valuait (une fois de trop) les param�tres m�me si rythme �tait vide
;; ***********************************************************************


(defun n-evenement (chan rythme haut vel
                         sil stac tril ecar proba laccord lecar lmaxaccord lmotifhaut ldurgliss lhautgliss lapog ltransp ltextexpres pgchange larpege arpegecar layer lcontrol lcontrol-chge llayersynthe laccelerando ecar-accelerando ctime stime etime reverse)

  (if rythme
      (let* ((canal �chan)
             (lma �lmaxaccord)
             (lac �laccord)
             (lec �lecar)
             (ctrlchge �lcontrol-chge)
             (etat-ctrl-chge (aref *etat-ctrl-chge* canal))) 
   
        (if ctrlchge
            (if etat-ctrl-chge
                (let ((vn (first etat-ctrl-chge))
                      (nbre (second etat-ctrl-chge))
                      (pedale (third etat-ctrl-chge))
                      (val 0))
                  (if (>= vn nbre)
                      (progn
                        (if pedale
                            (progn
                              (setq val (second ctrlchge))
                              (setq nbre (fourth ctrlchge)))
                          (progn
                            (setq val (third ctrlchge))
                            (setq nbre (fifth ctrlchge))))
                        (setq pedale (not pedale))
                        (make-controle-chge canal lac val (first ctrlchge))
                        (setf (aref *etat-ctrl-chge* canal) (list 1 nbre pedale)))

                    (setf (aref *etat-ctrl-chge* canal) (list (+ 1 vn) nbre pedale))))


              (progn
                (make-controle-chge canal lac (second ctrlchge) (first ctrlchge))
                (setf (aref *etat-ctrl-chge* canal) (list 1 (fourth ctrlchge) nil))))

          (progn
            (if (aref *etat-ctrl-chge* canal)
                (make-controle-chge canal lac (third ctrlchge) (first ctrlchge)))
            (setf (aref *etat-ctrl-chge* canal) nil)))

        (let*  ((p �proba)
               (llacelerando �laccelerando)
               (mode (if llacelerando (fifth llacelerando)
                       nil))
               (ecar-h-acceler (if mode �ecar-accelerando     ; �valuation avec temps global (d�cembre 2012)
                                 ecar-accelerando)))          ; �valuation ult�rieure avec temps local (d�cembre 2012)
            

          (if (> (SPECIAL-EVALUATE (first rythme)) 0)     ; 30 08 01
      
              (cond ((= p -1) (accord ctime canal nil (changder (first rythme)) �haut �vel �sil �stac �tril
                                      �ecar lac lec lma �lmotifhaut �ldurgliss �lhautgliss �lapog nil nil �larpege �arpegecar �layer �lcontrol �llayersynthe llacelerando ecar-h-acceler))
                    ((= p 0) (accord ctime canal nil (changder (first rythme)) 10 �vel �sil �stac �tril
                                     �ecar lac lec lma �lmotifhaut �ldurgliss �lhautgliss �lapog nil nil �larpege �arpegecar �layer �lcontrol �llayersynthe llacelerando ecar-h-acceler))
                    (t  (accord ctime canal �pgchange (first rythme) �haut �vel �sil �stac �tril �ecar lac lec lma �lmotifhaut
                                �ldurgliss �lhautgliss �lapog �ltransp �ltextexpres �larpege �arpegecar �layer �lcontrol �llayersynthe llacelerando ecar-h-acceler)))


            (accord ctime canal nil (first rythme) �haut �vel �sil �stac �tril �ecar lac lec lma �lmotifhaut
                    �ldurgliss �lhautgliss �lapog nil nil �larpege �arpegecar �layer �lcontrol �llayersynthe llacelerando ecar-h-acceler)))

    
        (n-evenement chan (cdr rythme) haut vel
                     sil stac tril ecar proba laccord lecar lmaxaccord lmotifhaut ldurgliss lhautgliss lapog ltransp ltextexpres pgchange larpege arpegecar layer lcontrol lcontrol-chge llayersynthe laccelerando ecar-accelerando (date?) stime etime reverse))))


