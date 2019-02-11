
;; ***********************************************************************************     
;;
;; Restauration de l'installation ANIMOTS du Parc de Gerland - 11 Septembre 2013
;; Utilise l'échantilloneur KONTAKT (v4.0.2.2813)
;; Version2 avec découpage en zones géographiques
;;
;; Suppression des séquences 10-13-21-23-24 avec voix abondantes - 23 mars 2017
;; Correction de la fonction place-prog le 4-02-2019
;;
;; ************************************************************************************


#|

Définition du Multi KONTAKT composé de 32 programmes (A1 ... A16 et B1 ... B16) de chacun 6 layers (ou groupes) P1, P2, P3, P4, P5 et P6
							
Nom Layer (et type de sons et valeurs des hauteurs MIDI):
							
P1	Guiro	Percu	PercuCastagnet	Riviere			
	21-68	69-96	97-125	        126-127			
P2	Triangle tenu	Insectes faux	Riviere				
	21-83	        84-125	        126-127				
P3	Tenu nuit	Tenu jour	Insect/ oiseau	Riviere			
	21-38	        39-68	        69-78/79-125	126-127			
P4	Voix	Cecilia2					
	21-50	51-125					
P5	Voix	Tenu jour court	  Insect/ oiseau				
	21-50	51-68	          69-78/79-125				
P6	Violons						
	55-96	
					
16 programmes Canal A (ch de 1 à 16):				
ProgA1:	
						
layer	Contrl n°9	Canal		
P1	1	        A1						
P2	2	        A1	
P3	3	        A1	
P4	4	        A1	
P5	5	        A1		
P6	6	        A1
		
ProgA2 --> ProgA16 (idem avec Canal A2 --> A16)	
			
16 programmes CanalB (ch de 1 à 16):			
ProgB1:	
			
layer	Contrl n°9	Canal		
P1	1	        B1						
P2	2	        B1				
P3	3	        B1				
P4	4	        B1						
P5	5	        B1						

ProgB2 --> ProgB16 (idem avec Canal B2 --> B16)



Composition  et adressage des "sous multi" du nom des multi S5000:
			
Nom	Layer	Clef 16 canaux A	Layer	Clef 16 canaux B
				
M1	P1	1	                P2	2
M2	P1	1	                P3	3
M3	P1	1	                P4	4
M4	P2	2	                P3	3
M5	P2	2	                P4	4
M6	P3	3	                P4	4
M7	P1	1	                P5	5
M8	P2	2	                P5	5
M9	P6	6	                P6	6
				
Le controleur n°9 par l'envoi des valeurs de 1 à 6				
permet de passer d'un layer à un autre	
			
|#



;;=====================
;; Variables globales 
;;=====================

;; Pour les générateurs
;;======================

(defvar *gparties* nil)    ;; Générateur de partie global
(defvar *gtest* nil )      ;; Generateur de sequence de test

;; Pour l'afficheur
;;==================

(defvar *dialog* nil)      ;; Dialogue de commande

;; Pour le Player
;;==================

(defvar *task-play* nil)   ;; Tâche de jeu
(defvar *task-date* nil)   ;; Tâche d'affichage de la durée courante

;; Seession
;;==================

(defparameter *log-session-num* 0)
(defparameter *log-session-output* *standard-output*)

(defun open-log-session ()
  (multiple-value-bind (ss mn hh jj mo an) (get-decoded-time)
    (declare (ignore hh mn ss))
    (let ((fname (format nil "/Documents/PAJ/animots-session")))
      (setq *log-session-output* (open fname :direction :output :if-exists :new-version :if-does-not-exist :create)))))

(defun close-log-session ()
  (close *log-session-output*)
  (setq *log-session-output* *standard-output*))

(defun print-to-log-session ()
  (multiple-value-bind (ss mn hh jj mo an) (get-decoded-time)
    (format *log-session-output* "~%~%~%~%;==========================================================")
     (format *log-session-output* "~%; NUM : ~5D, DATE : ~2D/~2D/~2D, TIME : ~2D:~2D:~2D~%"
            *log-session-num* jj mo an hh mn ss))
  (finish-output *log-session-output*)
  (incf *log-session-num*))

;; ***************************************************************************     
;;
;; Interface utilisateur
;;
;; ***************************************************************************

;; Callback appelées par les bouttons

(defun start-dialog-callback (&rest args)
  (print 'start *standard-output*)
  (start-sequence *gparties*)
)

(defun stop-dialog-callback (&rest args)
  (print 'stop *standard-output*)
  (stop-sequence)
)

(defun test1-dialog-callback (&rest args)
  (print 'cercle1 *standard-output*)
  (init)
  (test1)
  (play-sequence)
)

(defun test2-dialog-callback (&rest args)
  (print 'cercle2 *standard-output*)
  (init)
  (test2)
  (play-sequence)
) 

(defun test3-dialog-callback (&rest args)
  (print 'tutti *standard-output*)
  (init)
  (test3)
  (play-sequence)
)

(defun test4-dialog-callback (&rest args)
  (print 'alterne *standard-output*)
  (init)
  (test4)
  (play-sequence)
)

(defun quit-dialog-callback (&rest args)
  (print 'quitter *standard-output*)
  (quit)
)

;; Construction de l'interface utilisateur

(capi:define-interface controler-interface()
  ()
  (:panes

   (start-button
    capi:push-button
    :text "Démarrer"
    :callback-type :interface
    :callback 'start-dialog-callback)

   (stop-button
    capi:push-button
    :text "Arrèter"
    :callback-type :interface
    :callback 'stop-dialog-callback)

   (test1-button
    capi:push-button
    :text "Cercle1"
    :callback-type :interface
    :callback 'test1-dialog-callback)
   (test2-button
    capi:push-button
    :text "Cercle2"
    :callback-type :interface
    :callback 'test2-dialog-callback)
   (test3-button
    capi:push-button
    :text "Tutti"
    :callback-type :interface
    :callback 'test3-dialog-callback)
   (test4-button
    capi:push-button
    :text "Alterne"
    :callback-type :interface
    :callback 'test4-dialog-callback)

   (seq-pane
    capi:text-input-pane
    :accessor seq-pane
    :min-height '(character 1)
    :max-width nil)

   (duration-pane
    capi:text-input-pane
    :accessor duration-pane
    :min-height '(character 1)
    :max-width nil)

   (all-duration-pane
    capi:text-input-pane
    :accessor all-duration-pane
    :min-height '(character 1)
    :max-width nil)

   (quit-pane
    capi:push-button
    :text "Quitter"
    :callback-type :interface
    :callback 'quit-dialog-callback)
   )

  (:layouts
    (start-layout
     capi:row-layout
     '(start-button))

    (stop-layout
     capi:row-layout
     '(stop-button))

    (test-layout
     capi:row-layout
     '(test1-button test2-button test3-button test4-button))

    (seq-layout
     capi:row-layout
     '(seq-pane))

    (duration-layout
     capi:row-layout
     '(duration-pane))

    (all-duration-layout
     capi:row-layout
     '(all-duration-pane)
     :title "Durée totale")

   (quit-layout
     capi:row-layout
     '(quit-pane))

    (main-layout
      capi:column-layout
       '(start-layout stop-layout test-layout seq-layout duration-layout all-duration-layout quit-layout))
 
     )

  (:default-initargs
   :title "Animots"
   :X 400 :Y 50 :VISIBLE-WIDTH 200 :VISIBLE-HEIGHT 500  
   :WIDTH 200 :HEIGHT 500
   :layout 'main-layout))

(defun update-sequence (self sequence)
  ;;(setf (capi:text-input-pane-text (seq-pane self)) (write-to-string sequence))
  (capi:apply-in-pane-process self #'(lambda (self sequence)
                                       (setf (capi:text-input-pane-text (seq-pane self)) (write-to-string sequence)))
                              self sequence)
  )
  

(defun update-dur (self dur)
  ;;(setf (capi:text-input-pane-text (duration-pane self)) (write-to-string (midi-string-date dur)))
   (capi:apply-in-pane-process self  #'(lambda (self dur)
                                         (setf (capi:text-input-pane-text (duration-pane self)) (write-to-string (midi-string-date dur))))
                                self dur)
  )


;;(defun update-msh-space (self)
;;  (setf (capi:text-input-pane-text (begin-pane self)) (write-to-string (midifreespace)))
;;  (capi:apply-in-pane-process self  #'(lambda (self)
;;                                        (setf (capi:text-input-pane-text (all-duration-pane self)) (write-to-string (midifreespace)))) self)
;;)

(defun print-all-duration (self dur)
 ;; (setf (capi:text-input-pane-text (begin-pane self)) (write-to-string (midifreespace)))
  (capi:apply-in-pane-process self  #'(lambda (self dur)
                                        (setf (capi:text-input-pane-text (all-duration-pane self)) (write-to-string (midi-string-date dur))))
                              self dur)
)

;; Ouverture de la session
;;=========================

(defun open-play-session()

  ;; Crée la session de log
  (open-log-session)

   ;; Teste la mémoire MidiShare
  (if (<= (midifreespace) 100000) (midigrowspace 500000))

  ;; Déconnecte CLCE de l'entrée MididShare
  (midi-connect 0 clce nil)

  ;; Création du générateur de parties
  (unless *gparties* 
    (setq *gparties* (g-parties)))
 
  ;; Création du dialogue
  (unless *dialog* 
     (setq *dialog* (make-instance 'controler-interface))
     (capi:display *dialog*))

)

;;===========
;; Resultat
;;===========

(defun exec(g) (funcall g 0 0 0 0))

(defun calcul-partie (gparties)
  (init)
  (exec gparties)
)

(defun play-sequence ()
  (player) ;; à revoir : utiliser un player sans fenêtre ?
  (print-to-log-session)
  (print-all-duration *dialog* (msh-seq-all-duration))
  (print-sequence)
)

(defun next-sequence (gparties dur)
  (setq *task-play* (mp:make-timer 'start-sequence gparties))
  (mp:schedule-timer-relative-milliseconds *task-play* dur))

(defun start-sequence (gparties)
  (ignore-errors
    (calcul-partie gparties))                      
  (play-sequence)
  ;;(next-sequence gparties (midi-get-real-duration *out*)) TODO
  (next-sequence gparties (msh-seq-all-duration)))

(defun stop-sequence()
  (stop-callback)  ;; à revoir : utiliser un player sans fenêtre ?
  (when *task-play* 
    (mp:unschedule-timer *task-play*)
    (setq *task-play* nil))
  (when *task-date* 
    (mp:unschedule-timer *task-date*)
    (setq *task-date* nil))
)

(defun print-sequence ()
  (update-dur *dialog* (msh-seq-duration))
  ;(update-msh-space *dialog*)
  (when *task-date*
    (mp:unschedule-timer *task-date*)
    (setq *task-date* nil))
  (setq *task-date* (mp:make-timer 'print-sequence))
  (mp:schedule-timer-relative-milliseconds *task-date* 1000))
 
;;===========
;; Outils
;;===========

;; Calcule la durée réelle (en tenant compte des durée des notes)
;;=================================================================

(defun midi-get-real-duration (sequence)
  (let ((dur (midi-get-duration sequence)))
    (midi-move sequence :date 0)
    (midi-transform sequence #'(lambda(e) (when (= (evtype e) typenote) (setq dur (max (+ (date e) (dur e)) dur)))))
    dur)) 

;; Insère des All-note-off sur les 2 ports 
;;=========================================

(defun place-all-note-off (date sequence)
  (midi-move sequence :date (1+ date))
  (dotimes (i 16)
    (midi-send-im sequence (ctrl-change  :ctrl 123 :val 0 :chan i :port 0))
    (midi-send-im sequence (ctrl-change  :ctrl 123 :val 0 :chan i :port 1)))
  (midi-move sequence :date (+ 500 date))
  (midi-send-im sequence (key-off :pitch 0 :vel 0)))

;; ***************************************************************************************************     
;;
;; Restauration de l'installation ANIMOTS du Parc de Gerland - 13 Février 2013 modifié 5 Novembre 2013
;;
;; ***************************************************************************************************    

;; numéro des groupes (au sens KONTAKT) respectivement pour le port A (CAR= 1 à 16) et le port B (CADR= 17 à 32)

(defvar m1 '(1 2))
(defvar m2 '(1 3))
(defvar m3 '(1 4))
(defvar m4 '(2 3))
(defvar m5 '(2 4))
(defvar m6 '(3 4))
(defvar m7 '(1 5))
(defvar m8 '(2 5))
(defvar m9 '(6 6))


;; adressage des layers (groupes) définis par les listes m(i) pour les ports A et B 

(defun place-prog (date prog)
  (p-abs date)
  (dotimes (chan 16)
    (p-write-abs (ctrl-change :ctrl 9 :value (car prog) :chan chan :port 0))
    (p-write-abs (ctrl-change :ctrl 9 :value (car prog) :chan chan :port 1))))

(defvar *gaccord* °(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
(defvar *gaccord-demi* °(0 2 4 6 8 10 12 14))
(defvar *gecar* °(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

(defvar *gparties* nil)    ;; Générateur de partie global
(defvar *gtest* nil )      ;; Generateur de sequence de test

; zones géographiques

(defvar *zone1* °(0 1 4 8 3 2))      ;; zone nord
(defvar *zone2* °(5 6 7 11 10 9))   ;; zone centrale
(defvar *zone3* °(12 13 15 14))      ;; zone sud

(defun xconcat (n rytm)
  #'(lambda (stime ctime etime reverse) 
      (let ((l) (vrytm §rytm))  
        (dotimes  (i §n) (push  (g vrytm) l)) 
        §(apply #'concat l))))

(defun sel-al (g1 &rest lg)
  #'(lambda (stime ctime etime reverse)
      (let (( val (1+ §g1)))
        §(elt lg (min (1- (length lg)) (random (max 0 val)))))))

;; Affiche le nom de la séquence en cours
;;========================================
(defun print-dialog (seq)
  (update-sequence *dialog* seq)
)

;; Pour les trajectoires
;;========================
(defun traj (gtraj gnum)
  #'(lambda (stime ctime etime reverse)
      (l §gnum (s (s (g (l 1 gtraj)))))))

;; Pour les groupes
;;==================
(defun groupe (ggroup gnum gval)
  #'(lambda (stime ctime etime reverse)
      (l §gnum (rep (h (s (g (l 1 ggroup)))) gval))))

;; Retourne une liste de n
;;==================
(defun nconcat-spe4 (n val)
  #'(lambda (stime ctime etime reverse) 
      (let (l) 
        (dotimes  (i §n) (push  (lst val) l)) 
        §(apply #'concat l))))

;(l 1 (nconcat-spe4 °8 (hs °(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)))) --> ((11 10 4 9 5 1 12 0))

;; Retourne une liste de n
;;==================
(defun list-nvaleur (n v)
  #'(lambda (stime ctime etime reverse) 
      (let* ((nn §n)
             (ll (l nn v)))
      ll)))

;(l 1 (list-nvaleur °8 (hs °(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)))) --> ((1 6 13 11 7 15 0 12))


; génére une list de n valeurs de v repliées entre min et max
;;==================
(defun glist (n v min max)
  #'(lambda (stime ctime etime reverse)
      (let* ((nn §n)
             (ll (l nn (greplie v min max))))
        ll)))

;;(l 2 (glist °3 (rnd °10 °20) °30 °40)) --> ((36 39 30) (40 35 30))


;; génére une liste de dates entre deux dates
;;==================
(defun glistedate (t1 t2 v)
  #'(lambda (stime ctime etime reverse)
      (let ((tt1 §t1)
            (tt2 §t2)
            
            (l))
        (do  ((i tt1 (+ i §v)))
              ((> i tt2))
              (push  (lst (g i)) l))
          (reverse §(apply #'concat l)))))

;; (l 1 (glistedate °0 °10 (alt °1 °3))) ---> ((0 1 4 5 8 9))

;; génére une liste de durées entre deux dates
;;==================
(defun glistedur (t1 t2 v)
  #'(lambda (stime ctime etime reverse)
      (let ((tt1 §t1)
            (tt2 §t2)
             (l))

        (do  ((i tt1 (+ i §v)))
              ((> i tt2))
              (push  (lst v) l))
          §(apply #'concat l))))

;; (l 1 (glistedate2 °0 °10 (alt °1 °3))) ---> ((1 3 1 3 1 3))

;; transforme une liste de date en une liste de durée
;;==================
(defun date-dur (l &optional res)
  (if (cdr l)
      (date-dur (cdr l) (cons (- (car (cdr l)) (car l)) res))
    (reverse res)))

;; (date-dur '(0 2 6 12 20 22 26)) --> (2 4 6 8 2 4)

;; transforme une liste de date par ajout de valeurs données par le générateur v
;;==================
(defun transf-dat (l v) 
    #'(lambda (stime ctime etime reverse) 
      (let (ll (n (length l))) 
        (dotimes  (i n) (push  (lst (add v (g (nth i l)))) ll)) 
        (reverse §(apply #'concat ll)))))

;; (l 1 (transf-dat '(0 2 6 12 20 22 26) (h °(-0.3 -0.2 -0.1 0 0.1 0.2 0.3))))
;; ------> ((0.2 1.9 5.7 12.1 20.1 22.2 26.2))

;;===============================
;; Correction bug interpolation
;;===============================

(defun i (g1 g2)
  #'(lambda (stime ctime etime reverse)
      (let ((v1 (if reverse §g2 §g1))
            (v2 (if reverse §g1 §g2)))
        (if (= etime stime) v1
             (/ (+ (* v1 (- etime ctime)) 
                  (* v2 (- ctime stime))) 
               (- etime stime))))))

;; *******************************************************************
;; Initialisations
;; *******************************************************************

;; Trajectoires au sol (pour le canal A du S5000)
;; Enceintes n°: (3 4 9 10 11 12 8 7 6 5 2 1), etc ...

(defun trajectoires ()
  (sel-al °12  
          °(2 3 8 9 10 11 7 6 5 4 1 0) 
          °(0 1 4 5 6 7 11 10 9 8 3 2) 
          °(13 12 7 6 5 4 8 9 10 11 14 15)
          °(15 14 11 10 9 8 4 5 6 7 12 13) 
          °(4 5 6 7 11 10 9 8) 
          °(8 9 10 11 9 6 5 4 1 0)
          °(0 2 1 3 4 8 5 9 10 11 14 15 13 12) 
          °(12 13 15 14 11 10 9 5 8 4 3 1 2 0) 
          °(7 6 9 8 4 1 0 2 3 5)
          °(5 3 2 0 1 4 8 9 6 7) 
          °(13 12 7 6 10 11 14 15) 
          °(15 14 11 10 6 7 12 13)
          °(0 1 4 5 9 8 3 2)))

(defvar *traj-total-direct* °(0 1 4 5 6 7 12 13 15 14 11 10 9 8 3 2))
(defvar *traj-total-indirect* °(2 3 8 9 10 11 14 15 13 12 7 6 5 4 1 0))

(defvar *traj-total-direct2* °(16 17 20 21 22 23 28 29 30 31 27 26 25 24 19 18))
(defvar *traj-total-indirect2* °(18 19 24 25 26 27 31 30 29 28 23 22 21 20 17 16))

(defvar *traj-zig-zag* °(0 2 3 1 4 8 9 5 6 10 11 7 12 14 15 14 11 7 6 10 9 5 4 8 3 1))                     ;; pour programme A
(defvar *traj-zig-zag2* °(16 18 19 17 20 24 25 21 22 26 27 23 28 30 31 30 27 23 22 26 25 21 20 24 19 17))  ;; pour programme B

;; Groupes d'enceintes rapprochées au sol (pour le canal B du S5000)
;; Enceintes n°: (1 2 3 4), (5 6 10 9), (5 6 7 8 12 11 10 9), 7 8 12 11), (13 14 16 15)

(defun listgroupe ()
  (sel-al °4  °(16 17 18 19) °(20 21 25 24) °(20 21 22 23 27 26 25 24) °(22 23 27 26) °(28 29 30 31)))

;;==========
;: Parties
;;==========

;; Générateur de parties
;;=======================

(defun g-parties()
  (alt (g-sequence-1 °0 °25)
       (sel (hs °(0 1 2 3 4 5 6 7 8 10 11 13 14 15 17 18 19 24 25 26 27 28 29 30 31 32 33 34 35
                    0 1 2 3 4 5 6 7 8 10 13 14 15 17 19 24 25 26 32 33 34 35))
            (g-sequence-1 °0 (rnd °60 °120))   ;; début,fin (en noires)
            (g-sequence-2 °0 (rnd °300 °600))
            (g-sequence-3 °0 (rnd °300 °600))
            (g-sequence-4 °0 (rnd °240 °540))
            (g-sequence-5 °0 (rnd °300 °600))
            (g-sequence-6 °0 (rnd °240 °480))
            (g-sequence-7 °0 (rnd °240 °480))
            (g-sequence-8 °0 (rnd °240 °480))
            (g-sequence-9 °0 (rnd °240 °480))
            (g-sequence-10 °0 (rnd °240 °480))
            (g-sequence-11 °0 (rnd °240 °540))
            (g-sequence-12 °0 (rnd °240 °480))
            (g-sequence-13 °0 (rnd °180 °480))
            (g-sequence-14 °0 (rnd °180 °480))
            (g-sequence-15 °0 (rnd °120 °300))
            (g-sequence-16 °0 (rnd °180 °360))
            (g-sequence-17 °0 (rnd °120 °300))
            (g-sequence-18 °0 (rnd °120 °240))
            (g-sequence-19 °0 (rnd °240 °420))
            (g-sequence-20 °0 (rnd °180 °360))
            (g-sequence-21 °0 (rnd °120 °300))
            (g-sequence-22 °0 (rnd °120 °300))
            (g-sequence-23 °0 (rnd °120 °300))
            (g-sequence-24 °0 (rnd °180 °300))
            (g-sequence-25 °0 (rnd °180 °360))
            (g-sequence-26 °0 (rnd °180 °360))
            (g-sequence-27 °0 (rnd °180 °360))
            (g-sequence-28 °0 (rnd °240 °360))
            (g-sequence-29 °0 (rnd °180 °480))
            (g-sequence-30 °0 (rnd °180 °360))
            (g-sequence-31 °0 (rnd °180 °360))
            (g-sequence-32 °0 (rnd °180 °240))
            (g-sequence-33 °0 (rnd °180 °240))
            (g-sequence-34 °0 (rnd °180 °300))
            (g-sequence-35 °0 (rnd °180 °300))
            (g-sequence-36 °0 (rnd °180 °300)))))


;; *******************************************************************     test
;;          Multi M2
;;
;; test
;;
;;
;; *******************************************************************

(defun test ()  
  
  (print-dialog "TEST")
  
  ;; ************* prog-change 
  (place-prog 0 m2)
  (place-tempo 0 60)
  ;; ************* silence                                                                    
  (prorythme (s °(0 1 2 3 4 5 6 7 )) °0 °200                
             (gnotes)
             °0
             °(1)
             °69 
             °80))

(defun partie ()
  (init)
  (test)
  (player))


;(partie)

;; *******************************************************************     test1: cercle 1
;;          Multi M2
;;
;; percu
;;
;;
;; *******************************************************************

(defun test1 ()  
  
  (print-dialog "CERCLE1")
  
  ;; ************* prog-change 
  (place-prog 0 m2)
  (place-tempo 0 60)
  ;; ************* silence                                                                    
  (prorythme (s *traj-total-direct*) °0 °1000                
             (gnotes)
             °0
             °(1)
             °69 
             °80))

(defun partie ()
  (init)
  (test1)
  (player))

;(partie)


;; *******************************************************************     test1bis
;;          Multi M2
;;
;; percu
;;
;;
;; *******************************************************************

(defun test1bis ()  
  
  (print-dialog "TEST1BIS")
  
  ;; ************* prog-change 
  (place-prog 0 m2)
  (place-tempo 0 60)
  ;; ************* silence                                                                    
  (prorythme °0 °0 °800                
             (gnotes :laccord  *gaccord* :lecar °(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
             °0
             °(1)
             °69 
             °80))

(defun partie ()
  (init)
  (test1bis)
  (player))

;(partie)

;; *******************************************************************     test2: cercle 2
;;          Multi M2
;;
;; tom
;;
;;
;; *******************************************************************

(defun test2 ()  
  
  (print-dialog "CERCLE2")
  
  ;; ************* prog-change 
  (place-prog 0 m2)
  (place-tempo 0 60)
  ;; ************* silence                                                                    
  (prorythme (s *traj-total-direct*) °0 °1000                
             (gnotes)
             °0
             °(1)
             °72 
             °80))

(defun partie ()
  (init)
  (test2)
  (player))

;(partie)

;; *******************************************************************     test3: tutti
;;          Multi M2
;;
;; guiro
;;
;;
;; *******************************************************************

(defun test3 ()  
  
  (print-dialog "TUTTI")
  
  ;; ************* prog-change 
  (place-prog 0 m2)
  (place-tempo 0 60)
  ;; ************* silence                                                                    
  (prorythme °0 °0 °1000                
             (gnotes :laccord *gaccord* :lecar *gecar* )
             °0
             °(1)
             °48 
             °80))


(defun partie ()
  (init)
  (test3)
  (player))



;(partie)

;; *******************************************************************     test4: alterne (permet de comparer les niveaux de sortie des enceintes)
;;          Multi M2
;;
;; Pour chaque couple d'enceintes adjacentes, alterne 5 fois un son de clave
;;
;;
;; *******************************************************************

(defun test4 ()  
  
  (print-dialog "ALTERNE")
  
  ;; ************* prog-change 
  (place-prog 0 m2)
  (place-tempo 0 60)
  ;; ************* silence                                                                    
  (prorythme (alt (rep (s *traj-total-direct*) °5) (rep (shift °1  (s *traj-total-direct*)) °5)) °0 °1000                
             (gnotes)
             °2
             °(1 1 1 1 1 1 1 1 1 1)
             °7 
             °127))


(defun partie ()
  (init)
  (test4)
  (player))

;(partie)

;; *******************************************************************     Séquence 1 (1' - 4')
;;
;; Silence
;;
;; *******************************************************************

(defun sequence-1 (deb fin)  
  
  (print-dialog "SEQUENCE 1")
  
  ;; ************* prog-change 
  (place-prog 0 m2)
  (place-tempo 0 60)
  
  ;; ************* silence                                                                    
  (prorythme °0 deb fin                                             
             (gnotes)
             °0
             (lst (sub fin deb))
             °0 
             °0))

(defun g-sequence-1 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore  stime ctime etime reverse))
      (sequence-1 deb fin)))

(defun partie (deb fin)
  (init)
  (sequence-1 deb fin)
  (player))

;(partie °0 (floo(rnd °60 °240)))

;; *******************************************************************     Séquence 2 (5' - 10')
;; Nuit         Multi M2
;;
;; version pour 16 points
;;
;;
;; *******************************************************************

(defun sequence-2 (deb fin)  
  
  (print-dialog "SEQUENCE 2")
  
  ;; ************* prog-change  
  (place-prog 0 m2)
  (place-tempo 0 60)
  
  ;; ************* fond général nuit synchro  proba 1/2

  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))
    (prorythme °16  deb2 fin2                                             
               (gnotes :laccord *gaccord-demi* :lecar *gecar*)
               °0
               (lst (sub fin2 deb2))
               (rnd °21 °38) 
               °30)
  
    ;; *************  oiseaux et insectes aléatoires répété 1 ou 2 fois sur un même canal (diffusé sur de 3 à 8 canaux)
    (share-gen n1 (h °( 1 2 1 2 3)))
    
    (prorythme °16 (add deb2 (h °(0 2 4 6 7 8 10))) fin2  
               (gnotes :laccord (lst (h *zone1*) (h *zone2*) (h *zone3*)) :lecar *gecar*)
               (h °(0 4 4 6 8))
               (xconcat (use-gen n1)  (sel-al °4 °(1/2) °(1) °(3/2) °(3) °(5)))
               (rep (rnd °69 °85) (use-gen n1))   
               (rep (h °(90 80 70)) (use-gen n1)))
    (free-gen n1)))

(defun g-sequence-2 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore  stime ctime etime reverse))
      (sequence-2 deb fin)))

(defun partie (deb fin)
  (init)
  (sequence-2 deb fin)
  (player))

;(partie °0 (floo(rnd °120 °600)))

;; *******************************************************************     Séquence 3 (5' - 10')
;; Jour
;; version pour 8 points         Multi M2
;; avec fond général (proba 1/2) tenu jour
;; avec trajectoires d'oiseaux
;; avec oiseaux aléatoire non répétés
;; avec phrase monocanal d'oiseaux
;; *******************************************************************

(defun sequence-3 (deb fin)
  
  (print-dialog "SEQUENCE 3")
  
  ;; ************* prog-change  
  (place-prog 0 m2)
  (place-tempo 0 60)
  
  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))

    ;; ************* fond général jour proba 1/2
    (prorythme °16 deb2 fin2                                             
               (gnotes :laccord *gaccord-demi*
                       :lecar *gecar* :proba (h °(0 1 1 1)))
               °0
               (lst (sub fin2 deb2))
               (rnd °39 °68) 
               °30)

  
    ;; ************* Chant répété d'un oiseau sur un canal
    ;; zone1
    (share-gen n1 (h °( 1 2 2 2 2 3 3 3 4 5)))
    (prorythme (rep (add °16 (h *zone1*)) (use-gen n1)) (add deb2 (h °(0 2 4 6 7 8 10))) fin2   ;; débuts décallés aléatoirement
               (gnotes)
               (h °( 5 7 10 11 ))
               (xconcat (use-gen n1)  (sel-al °5  °(1/4) °(1/2) °(1) °(3/2) °(3) °(5)))
               (rep (rnd °69 °125) (use-gen n1)) 
               °90)
    (free-gen n1)

    ;; zone2
    (share-gen n1 (h °( 1 2 2 2 2 3 3 3 4 5)))
    (prorythme (rep (add °16 (h *zone2*)) (use-gen n1)) (add deb2 (h °(0 2 4 6 7 8 10))) fin2   
               (gnotes)
               (h °( 5 7 10 11 ))
               (xconcat (use-gen n1)  (sel-al °5  °(1/4) °(1/2) °(1) °(3/2) °(3) °(5)))
               (rep (rnd °69 °125) (use-gen n1)) 
               °90)
    (free-gen n1)

    ;; zone3
    (share-gen n1 (h °( 1 2 2 2 2 3 3 3 4 5)))
    (prorythme (rep (add °16 (h *zone3*)) (use-gen n1)) (add deb2 (h °(0 2 4 6 7 8 10))) fin2   
               (gnotes)
               (h °( 5 7 10 11 ))
               (xconcat (use-gen n1)  (sel-al °5  °(1/4) °(1/2) °(1) °(3/2) °(3) °(5)))
               (rep (rnd °69 °125) (use-gen n1)) 
               °90)
    (free-gen n1)
  
    ;; ************* Un son d'oiseau sur canal qq
    (prorythme (rnd °16 °31) (add deb2 (h °(0 2 4 6 7 8 10))) fin2          
               (gnotes)
               (h °(3 4 6 7 8 12))
               (sel-al °4 °(1/2) °(1) °(3/2) °(3) °(5))
               (rnd °69 °125) 
               °90)
  
    ;; ************* Phrase monocanal d'oiseaux 
    ;; zone1
    (share-gen n1 (h °(12 13 14 15 16 17 18)))
    (prorythme (rep (add °16 (h *zone1*)) (use-gen n1)) deb2 fin2
               (gnotes :proba (h °(1 1 1 1 1 0)))
               (h °(0 5 9 13 15))
               (concat °(1/2) (nconcat (sub (use-gen n1) °2)  °(1/8)) °(4)) 
               (rep (rnd °79 °125) (h °(1 1 1 2 3)))  
               °90)
    (free-gen n1)

    ;;zone 2
    (share-gen n1 (h °(12 13 14 15 16 17 18)))
    (prorythme (rep (add °16 (h *zone2*)) (use-gen n1)) deb2 fin2
               (gnotes :proba (h °(1 1 1 1 1 0)))
               (h °(0 5 9 13 15))
               (concat °(1/2) (nconcat (sub (use-gen n1) °2)  °(1/8)) °(4)) 
               (rep (rnd °79 °125) (h °(1 1 1 2 3)))  
               °90)
    (free-gen n1)

    ;; zone3
    (share-gen n1 (h °(12 13 14 15 16 17 18)))
    (prorythme (rep (add °16 (h *zone3*)) (use-gen n1)) deb2 fin2
               (gnotes :proba (h °(1 1 1 1 1 0)))
               (h °(0 5 9 13 15))
               (concat °(1/2) (nconcat (sub (use-gen n1) °2)  °(1/8)) °(4)) 
               (rep (rnd °79 °125) (h °(1 1 1 2 3)))  
               °90)
    (free-gen n1)
  
    ;; ************* trajectoires aléatoires d'oiseaux aléatoires ;; 
    ;; ************* soit des trajectoires de 12, 15 ou 18 points
    (share-gen ntraj (h °(4 5 6)))
    (prorythme (add °16 (s (traj (trajectoires) (mult °3 (use-gen ntraj))))) (add deb2 (h °(0 2 4 6 7 8 10))) fin2  
               (gnotes)                                                                 
               (h °(8 16 32))
               (xconcat (use-gen ntraj)  °(2/3c(1/2 1/2 1/2)) )                              
               (rep (rnd °79 °125) (mult °3 (use-gen ntraj))) 
               °120)
    (free-gen ntraj)
  
    ))
 
(defun g-sequence-3 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore  stime ctime etime reverse))
      (sequence-3 deb fin)))

(defun partie (deb fin)
  (init)
  (sequence-3 deb fin)
  (player))

;(partie °0 (rnd °120 °600))

;; *******************************************************************     Séquence 4 (4' - 9')
;; Guiro
;; version pour 16 points         Multi M2
;; avec fond général nuit (proba 1/2)
;; avec trajectoires d'oiseaux
;; avec insectes et oiseaux aléatoire
;; avec phrase monocanal de faux insectes
;; *******************************************************************

(defun sequence-4 (deb fin) 
  
  (print-dialog "SEQUENCE 4")
  
  ;; ************* prog-change 

  (place-prog 0 m2)
  (place-tempo 0 60)
  
  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))

    ;; ************* fond général nuit proba 1/2
    (prorythme °16 deb2 fin2
               (gnotes :laccord *gaccord-demi* 
                       :lecar *gecar* :proba (h °(1 1 1))) 
               °0
               (lst (sub fin2 deb2))
               (rnd °21 °38) 
               °20)
  
    ;; ************* Guiro répété sur un canal; zone1
    (share-gen n1 (h °( 1 2 2 2 2 3 3 3 4 5)))
    (prorythme (rep (h *zone1*) (use-gen n1)) (add deb2 (h °(0 1 2 3 4 5 6 8 10))) fin2
               (gnotes)
               (h °(0 4 8 8 12 16))
               (xconcat (use-gen n1)  (sel-al °3  °(1/4) °(1/2) °(3) °(5)))
               (rep (rnd °21 °68) (use-gen n1)) 
               °90)
    (free-gen n1)
  
    ;; ************* Guiro répété sur un canal; zone2
    (share-gen n1 (h °( 1 2 2 2 2 3 3 3 4 5)))
    (prorythme (rep (h *zone2*) (use-gen n1)) (add deb2 (h °(0 1 2 3 4 5 6 8 10))) fin2
               (gnotes)
               (h °(0 4 8 8 12 16))
               (xconcat (use-gen n1)  (sel-al °3  °(1/4) °(1/2) °(3) °(5)))
               (rep (rnd °21 °68) (use-gen n1)) 
               °90)
    (free-gen n1)

    ;; ************* Guiro répété sur un canal; zone3
    (share-gen n1 (h °( 1 2 2 2 2 3 3 3 4 5)))
    (prorythme (rep (h *zone3*) (use-gen n1)) (add deb2 (h °(0 1 2 3 4 5 6 8 10))) fin2
               (gnotes)
               (h °(0 4 8 8 12 16))
               (xconcat (use-gen n1)  (sel-al °3  °(1/4) °(1/2) °(3) °(5)))
               (rep (rnd °21 °68) (use-gen n1)) 
               °90)
    (free-gen n1)
  
    ;; ************* un peu d'oiseaux et d'insectes aléatoires répétés 1 ou 2 ou 3 fois sur un même canal
    ;; zone1
    (share-gen n1 (h °( 1 2 3)))
    (prorythme (rep (add °16 (h *zone1*)) (use-gen n1)) (add deb2 (h °(0 1 2 3 4 5 6 8 10))) fin2
               (gnotes)                                                                  
               (h °(0 4 8 8 12 16))
               (xconcat (use-gen n1)  (sel-al °4 °(1/2) °(1) °(3/2) °(3) °(5)))
               (rep (rnd °69 °83) (use-gen n1))   
               (rep (h °(100 70 90 80)) (use-gen n1)))
    (free-gen n1)
  
    ;; zone2
    (share-gen n1 (h °( 1 2 3)))
    (prorythme (rep (add °16 (h *zone2*)) (use-gen n1)) (add deb2 (h °(0 1 2 3 4 5 6 8 10))) fin2
               (gnotes)                                                                  
               (h °(0 4 8 8 12 16))
               (xconcat (use-gen n1)  (sel-al °4 °(1/2) °(1) °(3/2) °(3) °(5)))
               (rep (rnd °69 °83) (use-gen n1))   
               (rep (h °(100 70 90 80)) (use-gen n1)))
    (free-gen n1)

    ;; zone3
    `(share-gen n1 (h °( 1 2 3)))
    (prorythme (rep (add °16 (h *zone3*)) (use-gen n1)) (add deb2 (h °(0 1 2 3 4 5 6 8 10))) fin2
               (gnotes)                                                                  
               (h °(0 4 8 8 12 16))
               (xconcat (use-gen n1)  (sel-al °4 °(1/2) °(1) °(3/2) °(3) °(5)))
               (rep (rnd °69 °83) (use-gen n1))   
               (rep (h °(100 70 90 80)) (use-gen n1)))
    (free-gen n1)
  
    ;; ************* phrase monocanal de faux insectes 
    ;; zone1
    (share-gen n1 (h °(6 7 8 9 10 11 12)))
    (prorythme (rep (h *zone1*) (use-gen n1)) (add deb2 (h °(0 1 2 3 4 5 6 8 10))) fin2
               (gnotes :proba (h °(1 1 1 1 1 0)))
               (h °(0 5 9 11 13 15))
               (concat °(1/2) (nconcat (sub (use-gen n1) °2)  °(1/8)) °(4)) 
               (rep (rnd °21 °68) (h °(1 1 1 2 3 ))) 
               °90)
    (free-gen n1)

    ;; zone2
    (share-gen n1 (h °(6 7 8 9 10 11 12)))
    (prorythme (rep (h *zone2*) (use-gen n1)) (add deb2 (h °(0 1 2 3 4 5 6 8 10))) fin2
               (gnotes :proba (h °(1 1 1 1 1 0)))
               (h °(0 5 9 11 13 15))
               (concat °(1/2) (nconcat (sub (use-gen n1) °2)  °(1/8)) °(4)) 
               (rep (rnd °21 °68) (h °(1 1 1 2 3 ))) 
               °90)
    (free-gen n1)

    ;; zone3
    (share-gen n1 (h °(6 7 8 9 10 11 12)))
    (prorythme (rep (h *zone3*) (use-gen n1)) (add deb2 (h °(0 1 2 3 4 5 6 8 10))) fin2
               (gnotes :proba (h °(1 1 1 1 1 0)))
               (h °(0 5 9 11 13 15))
               (concat °(1/2) (nconcat (sub (use-gen n1) °2)  °(1/8)) °(4)) 
               (rep (rnd °21 °68) (h °(1 1 1 2 3 ))) 
               °90)
    (free-gen n1)))

(defun g-sequence-4 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore  stime ctime etime reverse))
      (sequence-4 deb fin)))

(defun partie (deb fin)
  (init)
  (sequence-4 deb fin)
  (player))

;(partie °0 (rnd °120 °600))

;; *******************************************************************     Séquence 5 (5' - 10')
;; Rivière
;; version pour 16 points         Multi M2
;; avec fond général rivière 
;; avec trajectoires d'oiseaux
;; avec insectes et oiseaux aléatoire
;; avec phrase monocanal d'oiseaux
;; *******************************************************************

(defun sequence-5 (deb fin) 
  
  (print-dialog "SEQUENCE 5")
  
  ;; ************* prog-change 
  (place-prog 0 m2)
  (place-tempo 0 60)
  
  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))
  
    ;; ************* fond général Rivière
    (prorythme °0 deb2 fin2
               (gnotes :laccord *gaccord* :lecar *gecar*)        
               °0
               (lst (sub fin2 deb2))
               (rnd °126 °127) 
               °60)
  
    ;; ************* un peu d'oiseaux aléatoires
    (prorythme (rnd °16 °31) (add deb2 (h °(0 2 4 6 7 8 10))) fin2                 
               (gnotes)
               (h °(0 4 4 6 8))
               (sel-al °4 °(1/2) °(1) °(3/2) °(3) °(5))
               (rnd °69 °125) 
               °100)
  
    ;; ************* trajectoires aléatoires d'oiseaux aléatoires pour les 16 points
    (share-gen ntraj (h °(4 5 6)))
    (prorythme (add °16 (s (traj (trajectoires) (mult °3 (use-gen ntraj))))) (add deb2 (h °(0 2 4 6 7 8 10))) fin2 
               (gnotes)                                                                 
               (h °(8 16 20))
               (xconcat (use-gen ntraj)  °(2/3c(1/2 1/2 1/2)) )                               
               (rep (rnd °79 °125) (mult °3 (use-gen ntraj))) 
               °100)
    (free-gen ntraj)
  
    ;; ************* phrase monocanal d'oiseaux
  
    ;; zone1
    (share-gen n1 (h °(12 13 14 15 16 17 18)))
    (prorythme (rep (add °16 (h *zone1*)) (use-gen n1)) °0 °96
               (gnotes :proba (h °(1 1 1 1 1 0)))
               (h °(5 9 20 13))
               (concat °(1/2) (nconcat (sub (use-gen n1) °2)  °(1/8)) °(4)) 
               (rep (rnd °79 °125)  (h °(1 1 1 2 3 ))) 
               °90)
    (free-gen n1)

    ;; zone2
    (share-gen n1 (h °(12 13 14 15 16 17 18)))
    (prorythme (rep (add °16 (h *zone1*)) (use-gen n1)) °0 °96
               (gnotes :proba (h °(1 1 1 1 1 0)))
               (h °(5 9 20 13))
               (concat °(1/2) (nconcat (sub (use-gen n1) °2)  °(1/8)) °(4)) 
               (rep (rnd °79 °125)  (h °(1 1 1 2 3 ))) 
               °90)
    (free-gen n1)

    ;; zone3
    (share-gen n1 (h °(12 13 14 15 16 17 18)))
    (prorythme (rep (add °16 (h *zone1*)) (use-gen n1)) °0 °96
               (gnotes :proba (h °(1 1 1 1 1 0)))
               (h °(5 9 20 13))
               (concat °(1/2) (nconcat (sub (use-gen n1) °2)  °(1/8)) °(4)) 
               (rep (rnd °79 °125)  (h °(1 1 1 2 3 ))) 
               °90)
    (free-gen n1)))


(defun g-sequence-5 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore  stime ctime etime reverse))
      (sequence-5 deb fin)))

(defun partie (deb fin)
  (init)
  (sequence-5 deb fin)
  (player))

;(partie °0 (rnd °120 °600))

;; *******************************************************************     Séquence 6 (4' - 8')
;; Percu Castagnet
;; version pour 16 points         Multi M2
;; avec fond général nuit (proba 1/2)
;; avec insectes aléatoire
;; avec phrase monocanal de percu castagnet
;; avec phrase monocanal de guiro
;; *******************************************************************

(defun sequence-6 (deb fin) 
  
  (print-dialog "SEQUENCE 6")
  
  ;; ************* prog-change 
  (place-prog 0 m2)
  (place-tempo 0 60)




  
  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))

 
    ;; ************************************* fond général de nuit (proba 1/2)
    (prorythme °16 °0 fin2
               (gnotes :laccord *gaccord*
                       :lecar *gecar*
                       :proba (h °(0 1 1))) 
               (h °(4 8 12 16))
               °(112)
               (rnd °21 °38) 
               °30)
  
    ;; ************************************* percus castagnets répétées sur un canal; zone1
    (share-gen n1 (h °( 1 2 2 2 2 3 3 3 4 5)))
    (prorythme (rep (h *zone1*) (use-gen n1)) (add deb2 (h °(0 1 2 3 4 5 6 8 10))) fin2
               (gnotes)
               (h °(0 4 8 10 12 20))
               (xconcat (use-gen n1)  (sel-al °3  °(1/4) °(1/2) °(3) °(5)))
               (rep (rnd °97 °125) (use-gen n1)) 
               °80)
    (free-gen n1)
  
    ;; ************************************* percus castagnets répétées sur un canal; zone2
    (share-gen n1 (h °( 1 2 2 2 2 3 3 3 4 5)))
    (prorythme (rep (h *zone2*) (use-gen n1)) (add deb2 (h °(0 1 2 3 4 5 6 8 10))) fin2
               (gnotes)
               (h °(0 4 8 10 12 20))
               (xconcat (use-gen n1)  (sel-al °3  °(1/4) °(1/2) °(3) °(5)))
               (rep (rnd °97 °125) (use-gen n1)) 
               °80)
    (free-gen n1)
  
    ;; ************************************* percus castagnets répétées sur un canal; zone3
    (share-gen n1 (h °( 1 2 2 2 2 3 3 3 4 5)))
    (prorythme (rep (h *zone3*) (use-gen n1)) 
               (add deb2 (h °(0 1 2 3 4 5 6 8 10))) fin2
               (gnotes)
               (h °(0 4 8 10 12 20))
               (xconcat (use-gen n1)  (sel-al °3  °(1/4) °(1/2) °(3) °(5)))
               (rep (rnd °97 °125) (use-gen n1)) 
               °80)
    (free-gen n1)
  
    ;; ************************************* un peu d'insectes aléatoires
    (prorythme (rnd °16 °31) (add deb2 (h °(0 1 2 3 4 5 6 8 10))) fin2               
               (gnotes)
               (h °(0 4 4 6 8 8))
               (sel-al °6  °(1/4) °(1/2) °(1) °(3/2) °(3) °(5) °(-4))
               (rnd °69 °78) 
               (h °(100 70 80 90 80)))
  
    ;; ************************************* phrase monocanal ou multicanal (proba 1/2) de percu castagnet  
    ;; zone1
    (share-gen n1 (h °(8 9 10 11 12 13 14 15 16 17 18)))
    (prorythme (sel (rep (h °(0 1)) (use-gen n1)) (rep (h *zone1*) (use-gen n1)) (h *zone1*))  (add deb2 (h °(0 1 2 3 4 5 6 8 10))) fin2
               (gnotes :proba (h °(1 1 1 1 1 0)))
               (h °(9 13 20 20 30 30))
               (concat °(1/2) (nconcat (sub (use-gen n1) °2)  °(1/8)) °(4)) 
               (rep (rnd °97 °125) (h °(1 1 1 2 3)))
               °100)
    (free-gen n1)

    ;; zone2
    (share-gen n1 (h °(8 9 10 11 12 13 14 15 16 17 18)))
    (prorythme (sel (rep (h °(0 1)) (use-gen n1)) (rep (h *zone2*) (use-gen n1)) (h *zone1*))  (add deb2 (h °(0 1 2 3 4 5 6 8 10))) fin2
               (gnotes :proba (h °(1 1 1 1 1 0)))
               (h °(9 13 20 20 30 30))
               (concat °(1/2) (nconcat (sub (use-gen n1) °2)  °(1/8)) °(4)) 
               (rep (rnd °97 °125) (h °(1 1 1 2 3)))
               °100)
    (free-gen n1)

    ;; zone3
    (share-gen n1 (h °(8 9 10 11 12 13 14 15 16 17 18)))
    (prorythme (sel (rep (h °(0 1)) (use-gen n1)) (rep (h *zone3*) (use-gen n1)) (h *zone1*))  (add deb2 (h °(0 1 2 3 4 5 6 8 10))) fin2
               (gnotes :proba (h °(1 1 1 1 1 0)))
               (h °(9 13 20 20 30 30))
               (concat °(1/2) (nconcat (sub (use-gen n1) °2)  °(1/8)) °(4)) 
               (rep (rnd °97 °125) (h °(1 1 1 2 3)))
               °100)
    (free-gen n1)
  
    ;; ************************************* phrase multicanal de guiro
    ;; zone1
    (share-gen n1 (h °(12 13 14 15 16 17 18)))
    (prorythme (h *zone1*) (add deb2 °10 (h °(0 1 2 3 4 5 6 8 10))) fin2
               (gnotes :proba (h °(1 1 1 1 1 0)))
               (h °(9 13 15 20 20 25 30))
               (concat °(1/2) (nconcat (sub (use-gen n1) °2)  °(1/8)) °(4)) 
               (rep (rnd °21 °68) (h °(1 1 1 2 3)))  
               °100)
    (free-gen n1)

    ;; zone2
    (share-gen n1 (h °(12 13 14 15 16 17 18)))
    (prorythme (h *zone2*) (add deb2 °10 (h °(0 1 2 3 4 5 6 8 10))) fin2
               (gnotes :proba (h °(1 1 1 1 1 0)))
               (h °(9 13 15 20 20 25 30))
               (concat °(1/2) (nconcat (sub (use-gen n1) °2)  °(1/8)) °(4)) 
               (rep (rnd °21 °68) (h °(1 1 1 2 3)))  
               °100)
    (free-gen n1)

    ;; zone3
    (share-gen n1 (h °(12 13 14 15 16 17 18)))
    (prorythme (h *zone3*) (add deb2 °10 (h °(0 1 2 3 4 5 6 8 10))) fin2
               (gnotes :proba (h °(1 1 1 1 1 0)))
               (h °(9 13 15 20 20 25 30))
               (concat °(1/2) (nconcat (sub (use-gen n1) °2)  °(1/8)) °(4)) 
               (rep (rnd °21 °68) (h °(1 1 1 2 3)))  
               °100)
    (free-gen n1)))

(defun g-sequence-6 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore  stime ctime etime reverse))
      (sequence-6 deb fin)))

(defun partie (deb fin)
  (init)
  (sequence-6 deb fin)
  (player))

;(partie °0 (rnd °120 °600))

;; *******************************************************************     Séquence 7 (4' - 8')
;; Percu
;; version pour 16 points         Multi M2
;; avec insectes aléatoire
;; avec phrase monocanal de percu castagnet
;; avec un peu de "tenu" brèves aléatoires
;; avec un peu de guiro aléatoire 
;; *******************************************************************

(defun sequence-7 (deb fin) 
  
  (print-dialog "SEQUENCE 7")
  
  ;; ************* prog-change 
  (place-prog 0 m2)
  (place-tempo 0 60)
  
  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))
  
  
    ;; ************************************* insectes oiseaux aléatoires un peu répétés
    ;; zone1
    (share-gen n1 (h °( 1 2 2 2 2 3 3 3 4 5)))
    (prorythme (rep (add °16 (h *zone1*)) (use-gen n1)) (add deb2 (h °(0 1 2 4 6 7 8 10))) fin2
               (gnotes)
               (h °(4 8 12 20))
               (xconcat (use-gen n1)  (sel-al °3  °(1/4) °(1/2) °(3) °(5)))
               (rep (rnd °69 °125) (use-gen n1)) 
               (rep (h °(100 70 90 80)) (use-gen n1)))
    (free-gen n1)

    ;; zone2
    (share-gen n1 (h °( 1 2 2 2 2 3 3 3 4 5)))
    (prorythme (rep (add °16 (h *zone2*)) (use-gen n1)) (add deb2 (h °(0 1 2 4 6 7 8 10))) fin2
               (gnotes)
               (h °(4 8 12 20))
               (xconcat (use-gen n1)  (sel-al °3  °(1/4) °(1/2) °(3) °(5)))
               (rep (rnd °69 °125) (use-gen n1)) 
               (rep (h °(100 70 90 80)) (use-gen n1)))
    (free-gen n1)

    ;; zone3
    (share-gen n1 (h °( 1 2 2 2 2 3 3 3 4 5)))
    (prorythme (rep (add °16 (h *zone3*)) (use-gen n1)) (add deb2 (h °(0 1 2 4 6 7 8 10))) fin2
               (gnotes)
               (h °(4 8 12 20))
               (xconcat (use-gen n1)  (sel-al °3  °(1/4) °(1/2) °(3) °(5)))
               (rep (rnd °69 °125) (use-gen n1)) 
               (rep (h °(100 70 90 80)) (use-gen n1)))
    (free-gen n1)
  
    ;; ************************************* phrase monocanal de percu  
    ;; zone1
    (share-gen n1 (h °(4 6 8 12 13 14 15 16 17 18))) 
    (prorythme (rep (h *zone1*) (use-gen n1)) 
               deb2 fin2
               (gnotes :proba (h °(1 1 1 1 1 0)))
               (h °(5 9 13))
               (sel-al °1
                       (concat °(1/2) (nconcat (sub (use-gen n1) °2)  °(1/8)) °(4))
                       (concat °(1/2) °(1/2) °(2) (nconcat (sub (use-gen n1) °2)  (sel-al °1  °(1/8) °(2/3dc(1/4 1/4 1/4)))) °(4)))
               (rep (rnd °69 °96) (h °(1 1 1 2 3)))
               °90)
    (free-gen n1)
  
    ;; zone2
    (share-gen n1 (h °(4 6 8 12 13 14 15 16 17 18))) 
    (prorythme (rep (h *zone2*) (use-gen n1)) 
               deb2 fin2
               (gnotes :proba (h °(1 1 1 1 1 0)))
               (h °(5 9 13))
               (sel-al °1
                       (concat °(1/2) (nconcat (sub (use-gen n1) °2)  °(1/8)) °(4))
                       (concat °(1/2) °(1/2) °(2) (nconcat (sub (use-gen n1) °2)  (sel-al °1  °(1/8) °(2/3dc(1/4 1/4 1/4)))) °(4)))
               (rep (rnd °69 °96) (h °(1 1 1 2 3)))
               °90)
    (free-gen n1)

    ;; zone3
    (share-gen n1 (h °(4 6 8 12 13 14 15 16 17 18))) 
    (prorythme (rep (h *zone3*) (use-gen n1)) 
               deb2 fin2
               (gnotes :proba (h °(1 1 1 1 1 0)))
               (h °(5 9 13))
               (sel-al °1
                       (concat °(1/2) (nconcat (sub (use-gen n1) °2)  °(1/8)) °(4))
                       (concat °(1/2) °(1/2) °(2) (nconcat (sub (use-gen n1) °2)  (sel-al °1  °(1/8) °(2/3dc(1/4 1/4 1/4)))) °(4)))
               (rep (rnd °69 °96) (h °(1 1 1 2 3)))
               °90)
    (free-gen n1)
  
    ;; ************************************* un peu de "tenu" jour / nuit brèves aléatoires
    (prorythme °16 (add deb2 (h °(0 1 2 4 6 7 8 10))) fin2             
               (gnotes :laccord *gaccord-demi*
                       :lecar *gecar*)
               (h °(4 6 8 8 12 24))
               (sel-al °5  °(3) °(1) °(2) °(8) °(4) °(7))
               (rnd °21 °68) 
               (h °(60 70 50 80)))
  
    ;; ************************************* un peu de guiro aléatoire
    (prorythme (rnd °0 °15) (add deb2 (h °(0 1 2 4 6 7 8 10))) fin2                        
               (gnotes)
               (h °(0 4 4 6 8 8))
               (sel-al °5  °(3) °(1/2) °(1) °(3/2) °(3) °(5))
               (rnd °21 °68) 
               °90)))


(defun g-sequence-7 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore  stime ctime etime reverse))
      (sequence-7 deb fin)))


(defun partie (deb fin)
  (init)
  (sequence-7 deb fin)
  (player))

;(partie °0 (rnd °120 °600))

;; *******************************************************************     Séquence 8 (4' - 8')
;; Percu Zen
;; version pour 16 points         Multi M2
;; avec fond général jour ou nuit par brefs moments
;; avec percus rares répétées
;; avec phrase monocanal de percu
;; avec insectes aléatoires rares
;; *******************************************************************

(defun sequence-8 (deb fin) 
  
  (print-dialog "SEQUENCE 8")
  
  ;; ************* prog-change 
  (place-prog 0 m2)
  (place-tempo 0 60)
  
  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))
  
  
    ;; ************************************* un peu de "tenu" jour / nuit brèves aléatoires
    (prorythme (rnd °16 °31) (add deb2 (h °(0 1 2 4 6 7 8 10))) fin2              
               (gnotes)
               (h °(6 12 16 16 20))
               (sel-al °5  °(2) °(4) °(5) °(6) °(7) °(8))
               (rnd °21 °68) 
               (h °(60 70 50)))
  
    ;; ************************************* percus rares répétées
    ;; zone1
    (share-gen n1 (h °( 1 2 2 2 2 3 3 3 3 4 4 4 5 5)))
    (n-prorythme  deb2 fin2 (h °(0 1 2 3 4 5))
                    (sel  (h°(0 0 0 0 0 1)) 
                          (n-gnotes (rep (h *zone1*) (use-gen n1))
                                    (xconcat (use-gen n1)  
                                             (sel-al °4   °(4) °(2) °(3) °(5) °(8)))
                                    (rep (h °(70 71 74 76 77 78 80 84 85)) (use-gen n1))
                                    °100)
                          (n-gnotes (h *zone1*)
                                    °(16)
                                    °75
                                    °70)))
    (free-gen n1)
  
    ;; zone2
    (share-gen n1 (h °( 1 2 2 2 2 3 3 3 3 4 4 4 5 5)))
    (n-prorythme  deb2 fin2 (h °(0 1 2 3 4 5))
                    (sel  (h°(0 0 0 0 0 1)) 
                          (n-gnotes (rep (h *zone2*) (use-gen n1))
                                    (xconcat (use-gen n1)  
                                             (sel-al °4   °(4) °(2) °(3) °(5) °(8)))
                                    (rep (h °(70 71 74 76 77 78 80 84 85)) (use-gen n1))
                                    °100)
                          (n-gnotes (h *zone2*)
                                    °(16)
                                    °75
                                    °70)))
    (free-gen n1)

        ;; zone3
    (share-gen n1 (h °( 1 2 2 2 2 3 3 3 3 4 4 4 5 5)))
    (n-prorythme  deb2 fin2 (h °(0 1 2 3 4 5))
                    (sel  (h°(0 0 0 0 0 1)) 
                          (n-gnotes (rep (h *zone3*) (use-gen n1))
                                    (xconcat (use-gen n1)  
                                             (sel-al °4   °(4) °(2) °(3) °(5) °(8)))
                                    (rep (h °(70 71 74 76 77 78 80 84 85)) (use-gen n1))
                                    °100)
                          (n-gnotes (h *zone3*)
                                    °(16)
                                    °75
                                    °70)))
    (free-gen n1)
  
    ;; ************************************* phrase monocanal de percu 
    (share-gen n1 (h °(4 6 8 12 13 14 15 16 17 18)))
    (prorythme (rep (rnd °0 °15) (use-gen n1)) (add deb2 (h °(0 1 2 4 6 7 8 10))) fin2
               (gnotes :proba (h °(1 1 1 1 1 0)))
               (h °(16 8 24))
               (concat °(1/2) (nconcat (sub (use-gen n1) °2)  °(1/8)) °(4))
               (rep (rnd °69 °96) (h °(1 1 1 2 3)))
               °90)
    (free-gen n1)
  
    ;; ************************************* insectes aléatoires rares
    (prorythme (rnd °16 °31) (add deb2 (h °(0 1 2 3 4 5 6 8 10))) fin2               
               (gnotes)
               (h °(0 4 4 6 8))
               (sel-al °5  °(1/4) °(1/2) °(1) °(3/2) °(3) °(5))
               (rnd °69 °78) 
               (h °(90 70 80)))))

(defun g-sequence-8 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore  stime ctime etime reverse))
      (sequence-8 deb fin)))

(defun partie (deb fin)
  (init)
  (sequence-8 deb fin)
  (player))

;(partie °0 (rnd °120 °600))

;; *******************************************************************     Séquence 9 (4' - 8')
;; Percu Trajectoires
;; version pour 16 points         Multi M2
;; avec fond général jour / nuit synchro par moments brefs
;; avec percus rares répétées (2 fois)
;; avec trajectoires de percussions
;; *******************************************************************

(defun sequence-9 (deb fin) 
  
  (print-dialog "SEQUENCE 9")
  
  ;; ************* prog-change 
  (place-prog 0 m2)
  (place-tempo 0 60)
  
  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))
  
    ;; ************************************* un peu de "tenu" jour / nuit brèves aléatoires synchro 
    (prorythme °16 deb2 fin2                                        
               (gnotes :laccord *gaccord-demi*
                       :lecar *gecar*)
               (h °(20 20 32 ))
               (sel-al °5 °(14) °(7) °(8) °(9) °(10) °(12))
               (rnd °21 °68) 
               °40)
  
    ;; ************************************* percus rares répétées
    ;; zone1
    (share-gen n1 (h °( 1 2 2 2 2 3 3 3 3 4 4 4 5 5 5 5 5)))
    (n-prorythme  (add deb2 (h °(0 1 2 4 6 7 8 10))) fin2  (h °(3 6 9))
                  (sel (h°(0 0 0 1))
                       (n-gnotes (rep (h *zone1*) (use-gen n1))
                                 (xconcat (use-gen n1)  (sel-al °3   °(4) °(3) °(5) °(6)))
                                 (rep (h °(70 71 74 76 77 78 80 84 85)) (use-gen n1))
                                 °90)
                       (n-gnotes (h *zone1*)
                                 °(12)
                                 (rnd °69 °125)
                                 °70)))
    (free-gen n1)
  
    ;; zone2
    (share-gen n1 (h °( 1 2 2 2 2 3 3 3 3 4 4 4 5 5 5 5 5)))
    (n-prorythme  (add deb2 (h °(0 1 2 4 6 7 8 10))) fin2  (h °(3 6 9))
                  (sel (h°(0 0 0 1))
                       (n-gnotes (rep (h *zone2*) (use-gen n1))
                                 (xconcat (use-gen n1)  (sel-al °3   °(4) °(3) °(5) °(6)))
                                 (rep (h °(70 71 74 76 77 78 80 84 85)) (use-gen n1))
                                 °90)
                       (n-gnotes (h *zone2*)
                                 °(12)
                                 (rnd °69 °125)
                                 °70)))
    (free-gen n1)

        ;; zone3
    (share-gen n1 (h °( 1 2 2 2 2 3 3 3 3 4 4 4 5 5 5 5 5)))
    (n-prorythme  (add deb2 (h °(0 1 2 4 6 7 8 10))) fin2  (h °(3 6 9))
                  (sel (h°(0 0 0 1))
                       (n-gnotes (rep (h *zone3*) (use-gen n1))
                                 (xconcat (use-gen n1)  (sel-al °3   °(4) °(3) °(5) °(6)))
                                 (rep (h °(70 71 74 76 77 78 80 84 85)) (use-gen n1))
                                 °90)
                       (n-gnotes (h *zone3*)
                                 °(12)
                                 (rnd °69 °125)
                                 °70)))
    (free-gen n1)
  

  
    ;; ************* trajectoires aléatoires d'oiseaux aléatoires ;; 
    ;; ************* soit des trajectoires de 12, 15 ou 18 points
  
    (share-gen ntraj (h °( 4 5 6 7 8 9 10 10 16)))
    (prorythme (add °16 (s (traj (trajectoires) (mult °3 (use-gen ntraj))))) (add deb2 (h °(0 2 4 6 7 8 10))) fin2 
               (gnotes)                                                                  
               (h °(1 8 16 16))
               (sel-al °1                                                                
                       (xconcat (use-gen ntraj)  °(2/3c(1/4 1/4 1/4)))                   
                       (xconcat (use-gen ntraj)  °(1/8 1/8 1/8)))                               ;; soit des trajectoires de 12, 15 ou 18 points
               (rep (h °(69 70 71 74 76 77 78 79 80 81 92 93 94 95 96 108))  (mult °3 (use-gen ntraj))) 
               °90)
    (free-gen ntraj)

    (share-gen ntraj (h °( 4 5 6 7 8 9 10 10 16)))
    (prorythme (add °16 (s (traj (trajectoires) (mult °3 (use-gen ntraj))))) (add deb2 (h °(0 2 4 6 7 8 10))) fin2 
               (gnotes)                                                                  
               (h °(1 8 16 16))
               (sel-al °1                                                                
                       (xconcat (use-gen ntraj)  °(2/3c(1/4 1/4 1/4)))                   
                       (xconcat (use-gen ntraj)  °(1/8 1/8 1/8)))                               ;; soit des trajectoires de 12, 15 ou 18 points
               (rep (h °(69 70 71 74 76 77 78 79 80 81 92 93 94 95 96 108))  (mult °3 (use-gen ntraj))) 
               °90)
    (free-gen ntraj)))

(defun g-sequence-9 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore  stime ctime etime reverse))
      (sequence-9 deb fin)))

(defun partie (deb fin)
  (init)
  (sequence-9 deb fin)
  (player))

;(partie °0 (rnd °120 °600))

;; *******************************************************************     Séquence 10 (4' - 8')
;; Voix principales
;; version pour 16 points groupés         Multi M7
;; avec phrase de voix + percu et/ou oiseaux, éclatée sur des groupes d'enceintes.
;; avec oiseaux rares
;; avec voix rares
;; *******************************************************************

(defun sequence-10 (deb fin) 
  
  (print-dialog "SEQUENCE 10")
  
  
  ;; ************* prog-change 
  (place-prog 0 m7)
  (place-tempo 0 60)
  
  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))
  
    ;; ************** phrase de voix + percu et/ou oiseaux, éclatée sur des groupes d'enceintes. 

    (share-gen n1 (h °(12 13 14 15 16 17 18 20 28))) ;; nbre de sons par groupe d'enceinte
    (n-prorythme  deb2 fin2 (h °(0 1 2 3 4 5)) 
                    
                  ;; ***************** voix + percu
                    
                  (sel-al °4  
                          (n-gnotes (s (groupe (listgroupe) (add °1 (use-gen n1)) (h °(3 4 5 6))))
                                    (concat (nconcat (use-gen n1)  
                                                     (h °((1/8) (1/8) (1/8) (1/8) (1/8) (1/8) (1/8) (1/8) (-1/2)))) °(3/4))
                                    (mem (rnd °21 °50) (h °(4 3 2)) °5)
                                    °80
                                    :laccord °(0 -16) :lecar (lst °0 (rep (h °(48 75)) (add °1 (use-gen n1) ))))
                            
                          ;; ***************** voix + percu ou oiseau
                          (n-gnotes (s (groupe (listgroupe) (add °1 (use-gen n1) ) (h °(3 4 5 6))))
                                    (concat (nconcat (use-gen n1)  
                                                     (h °((1/4) (1/4) (1/4) (1/4) (1/4) (1/4) (1/4) (1/4) (-1/2)))) °(3/4))
                                    (mem (rnd °21 °50) (h °(4 3 2)) °5)
                                    °80
                                    :laccord (lst °0 (rep (h °(0 -16)) (add °1 (use-gen n1) ))) :lecar (lst °0 (rep (h °(48 75)) (add °1 (use-gen n1) ))))
                            
                          ;; ***************** voix + oiseaux/insectes
                          (n-gnotes (s (groupe (listgroupe) (add °1 (use-gen n1) ) (h °(3 4 5 6))))
                                    (concat (nconcat (use-gen n1)  
                                                     (h °((1/2) (1/2) (1/2) (1/2) (1/2) (1/2) (1/2) (1/4) (-1/2)))) °(3/4))
                                    (mem (rnd °21 °50) (h °(4 3 2)) °5)
                                    °80
                                    :laccord °(0 0) :lecar (lst °0 (rep (h °(48 75)) (add °1 (use-gen n1) ))))
                            
                          ;; ***************** voix + percu et oiseau mélangés
                          (n-gnotes (s (groupe (listgroupe) (add °1 (use-gen n1) ) (h °(3 4 5 6))))
                                    (concat (nconcat (use-gen n1)  
                                                     (h °((1/4) (1/4) (1/4) (1/4) (1/4) (1/4) (1/4) (1/4) (-1/2)))) °(3/4))
                                    (mem (rnd °21 °50) (h °(4 3 2)) °5)
                                    °80
                                    :laccord (lst °0 (h °(0 -16))) :lecar (lst °0 (h °(48 75))))
                            
                          ;; ***************** un seul son voix + percu synchro sur toutes les enceintes
                          (n-gnotes °0
                                    (sel-al °3  °(1/4) °(1/2) °(3) °(5))
                                    (rnd °21 °50)
                                    °50
                                    :laccord °(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
                                                 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31) 
                                    :lecar °(48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48
                                                0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))
    (free-gen n1)

  
    ;; ************* voix rares aléatoires
    (prorythme (rnd °16 °31) (add deb2 (h °(0 1 2 4 6 7 8 10))) fin2                       
               (gnotes)
               (h °( 4 4 6 8 12))
               (sel-al °5  °(1/4) °(1/2) °(1) °(3/2) °(3) °(5))
               (rnd °21 °50) 
               (rep (h °(70 80 90 60))  (s °(1 2 3 4))))
  
    ;; ************* voix rares aléatoires répétées
    (prorythme (rep (rnd °16 °31) (s °(1 2 3 4))) (add deb2 (h °(0 1 2 4 6 7 8 10))) fin2                       
               (gnotes)
               (h °( 4 4 6 8 12))
               °(1/2)
               (rep (rnd °21 °50) (s °(1 2 3 4)) )
               (rep (h °(70 80 90 60))  (s °(1 2 3 4))))
  
    ;; ************* oiseaux rares aléatoires
    (prorythme (rnd °16 °31) (add deb2 (h °(0 1 2 4 6 7 8 10))) fin2                       
               (gnotes)
               (h °( 4 4 6 8 12))
               (sel-al °5  °(1/4) °(1/2) °(1) °(3/2) °(3) °(5))
               (rnd °69 °125) 
               (h °(100 110 90 80)))))

(defun g-sequence-10 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore  stime ctime etime reverse))
      (sequence-10 deb fin)))

(defun partie (deb fin)
  (init)
  (sequence-10 deb fin)
  (player))

;(partie °0 (rnd °120 °600))

;; *******************************************************************     Séquence 11 (4' - 9')
;; Cloches (rares)
;; version pour 16 points       Multi M4
;; phrase éclatée (par groupe) de triangles
;; triangles aigus répétés
;; insectes rares aléatoires
;; *******************************************************************

(defun sequence-11 (deb fin) 
  
  (print-dialog "SEQUENCE 11")
  
  ;; ************* prog-change 
  (place-prog 0 m4)
  (place-tempo 0 60)


  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))
  
    ;; ************************************* phrase éclatée (par groupe) de triangles 
    (share-gen n1 (h °(6 7 8 9 10 11))) ;; nbre de sons par groupe d'enceinte 
      (prorythme (sub (s (groupe (listgroupe) (use-gen n1) (h °(1 2 3)))) °16) (add deb2 (h °(0 1 2 4 6 7 8 10))) fin2
                 (gnotes :stac °3)
                 (h °(4 4 8 16 32 24))
                 (concat °(1/2) (nconcat (sub (use-gen n1) °2)  (sel-al °6 °(1) °(2) °(2/3n (2 1)) °(3) °(1/4) °(1/4) °(1/4))) °(10))
                 (rep (rnd °21 °83) (h °(1 1 1 1 1 2 2 3 3 3 4)))
                 (h °( 60 70 90 110 10)))
      (free-gen n1)
  
        ;; ************************************* triangles graves répétés global répété sur même canal
    (share-gen n1 (h °(3 4 5 6 7)))
      (prorythme (rep (rnd °0 °15) (use-gen n1)) (add deb2 (h °(0 1 3 5 7 9 11))) fin2
                 (gnotes :stac °3)
                 (h °(16 32 24 28))
                 (concat (nconcat (sub (use-gen n1) °1)  
                                  (rep (h °((1) (3/2) (2/3b (2 2 2)) (2) (3))) (sub (use-gen n1) °1)))  °(10))
                 (rep (rnd °33 °60) (h °(7 14 21)))
                 °80)
      (free-gen n1)

      ;; ************************************* triangles graves multicanal répétés par zone
    (share-gen n1 (h °(3 4 5 6 7)))
      (prorythme (sel (rep (h °(0 0 1 1 2)) (use-gen n1)) (h *zone1*) (h *zone2*) (h *zone2*)) (add deb2 (h °(0 1 3 5 7 9 11))) fin2
                 (gnotes :stac °3)
                 (h °(16 12 24 28))
                 (concat (nconcat (sub (use-gen n1) °1)  
                                  (rep (h °((1) (3/2) (2/3b (2 2 2)) (2) (3))) (sub (use-gen n1) °1)))  °(10))
                 (rep (rnd °33 °60) (h °(7 14 21)))
                 °80)
      (free-gen n1)
  
    ;; ************************************* triangles aigus répétés 
    ;; zone1
    (share-gen n1 (h °(3 4 5 6 7))) 
      (prorythme (rep (h *zone1*) (use-gen n1)) (add deb2 (h °(0 1 3 5 7 9 11))) fin2
                 (gnotes :stac °3)
                 (h °(16 32 24))
                 (concat (nconcat (sub (use-gen n1) °1)  
                                  (rep (h °((1/8) (2/3dc (1/4 1/4 1/4)) (1/4))) (sub (use-gen n1) °1)))  °(10))
                 (rep (rnd °74 °83) (h °(7 14 21)))
                 °100)
      (free-gen n1)

      ;; zone2
    (share-gen n1 (h °(3 4 5 6 7))) 
      (prorythme (rep (h *zone2*) (use-gen n1)) (add deb2 (h °(0 1 3 5 7 9 11))) fin2
                 (gnotes :stac °3)
                 (h °(16 32 24))
                 (concat (nconcat (sub (use-gen n1) °1)  
                                  (rep (h °((1/8) (2/3dc (1/4 1/4 1/4)) (1/4))) (sub (use-gen n1) °1)))  °(10))
                 (rep (rnd °74 °83) (h °(7 14 21)))
                 °100)
      (free-gen n1)

      ;; zone3
    (share-gen n1 (h °(3 4 5 6 7))) 
      (prorythme (rep (h *zone3*) (use-gen n1)) (add deb2 (h °(0 1 3 5 7 9 11))) fin2
                 (gnotes :stac °3)
                 (h °(16 32 24))
                 (concat (nconcat (sub (use-gen n1) °1)  
                                  (rep (h °((1/8) (2/3dc (1/4 1/4 1/4)) (1/4))) (sub (use-gen n1) °1)))  °(10))
                 (rep (rnd °74 °83) (h °(7 14 21)))
                 °100)
      (free-gen n1)
  
    ;; ************************************* un peu de "tenu" jour / nuit brèves aléatoires synchro 
    (prorythme °16 (add deb2 (h °(0 1 2 4 6 7 8 10))) fin2                                        
               (gnotes :laccord *gaccord* 
                       :lecar *gecar*)
               (h °( 4 8 20 20 32))
               (sel-al °3  °(8) °(3) °(4) °(6))
               (rnd °21 °68) 
               °40)
  
  
    ;; ************************************* insectes rares aléatoires
    ;; zone1
    (prorythme (add °16 (h *zone1*)) (add deb2 (h °(0 1 2 4 6 7 8 10))) fin2                       
               (gnotes)
               (h °(5 7 9 12 14))
               (sel-al °7  °(1/4) °(1/2) °(1/2) °(1/2) °(1) °(3/2) °(3) °(5))
               (rnd °69 °78) 
               °90)

       ;; zone2
    (prorythme (add °16 (h *zone2*)) (add deb2 (h °(0 1 2 4 6 7 8 10))) fin2                       
               (gnotes)
               (h °(5 7 9 12 14))
               (sel-al °7  °(1/4) °(1/2) °(1/2) °(1/2) °(1) °(3/2) °(3) °(5))
               (rnd °69 °78) 
               °90)

       ;; zone3
    (prorythme (add °16 (h *zone3*)) (add deb2 (h °(0 1 2 4 6 7 8 10))) fin2                       
               (gnotes)
               (h °(5 7 9 12 14))
               (sel-al °7  °(1/4) °(1/2) °(1/2) °(1/2) °(1) °(3/2) °(3) °(5))
               (rnd °69 °78) 
               °90)))

(defun g-sequence-11 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore  stime ctime etime reverse))
      (sequence-11 deb fin)))

(defun partie (deb fin)
  (init)
  (sequence-11 deb fin)
  (player))

;(partie °0 (rnd °120 °300))

;; *******************************************************************     Séquence 12 (4' - 8')
;; Cloches et voix
;; version pour 16 points       Multi M8
;; avec phrase éclatée de triangles
;; avec triangles graves ou aigus répétés
;; avec voix rares aléatoires
;; *******************************************************************

(defun sequence-12 (deb fin) 
  
  
  (print-dialog "SEQUENCE 12")
  
  ;; ************* prog-change 
  (place-prog 0 m8)
  (place-tempo 0 60)


  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))
  
  
  
    ;; ************************************* phrase éclatée de triangles 
    (prorythme (rnd °0 °15) (add deb2 (h °(0 1 2 4 6 7 8 10))) fin2
               (gnotes :proba (h °(1 1 1 1 1 0)) :stac °3)
               (h °(4 4 8 16 24))
               (concat °(1/2) (nconcat (h °(3 4 5 6 7))  (sel-al °4 °(1) °(2) °(2/3n (1 -1 1)) °(3) °(3))) °(10))
               (rep (rnd °21 °83) (h °(1 1 1 1 1 2 2 3 3 3 4)))
               °100)
  
    ;; ************************************* triangles graves répétés  
    (share-gen n1 (h °(3 4 5 6 7)))
      (prorythme (rep (rnd °0 °15) (use-gen n1)) deb2 fin2
                 (gnotes :stac °8)
                 (h °(16 8 24))
                 (concat (nconcat (sub (use-gen n1) °1)  
                                  (rep (h °((1) (3/2) (2/3b (2 2 2)) (2) (3))) (sub (use-gen n1) °1)))  °(10))
                 (rep (rnd °21 °60) (h °(7 14 21)))
                 °90)
      (free-gen n1)
  
    ;; ************************************* triangles aigus répétés  
    (share-gen n1 (h °(3 4 5 6 7)))
      (prorythme (rep (rnd °0 °15) (use-gen n1)) deb2 fin2
                 (gnotes :stac °3)
                 (h °(16 32 24))
                 (concat (nconcat (sub (use-gen n1) °1)  
                                  (rep (h °((1/8) (2/3dc (1/4 1/4 1/4)) (1/4))) (sub (use-gen n1) °1)))  °(10))
                 (rep (rnd °74 °83) (h °(7 14 21)))
                 °90)
      (free-gen n1)
  
    ;; ************************************* un peu de "tenu" jour brèves aléatoires synchro 
    (prorythme °16 (add deb2 (h °(0 1 2 4 6 7 8 10))) fin2                                        
               (gnotes :laccord *gaccord*
                       :lecar *gecar*)
               (h °( 4 8 20 20 32))
               (sel-al °3  °(8) °(10) °(4) °(6))
               (rnd °51 °68) 
               °40)
  
  
    ;; ************************************* voix rares aléatoires
    (prorythme (rep (add °16 (h *zone1*)) (h °(2 3))) (add deb2 (h °(0 1 2 4 6 7 8 10))) fin2                       
               (gnotes)
               (h °(0 1 5 9 13))
               (nconcat (h °(2 3)) (rep (sel-al °2  °(1/8) °(1/8) °(1/16)) (h °(2 3))))  
               (mem (rnd °21 °50) °3 °4) 
               °90)
    (prorythme (rep (add °16 (h *zone2*)) (h °(2 3))) (add deb2 (h °(0 1 2 4 6 7 8 10))) fin2                       
               (gnotes)
               (h °(0 1 4 7 12))
               (nconcat (h °(2 3)) (rep (sel-al °2  °(1/8) °(1/8) °(1/16)) (h °(2 3))))  
               (mem (rnd °21 °50) °3 °4) 
               °90)
    (prorythme (rep (add °16 (h *zone3*)) (h °(4 3))) (add deb2 (h °(1/2 1 2 4 6 7 8 10))) fin2                       
               (gnotes)
               (h °(0 1/2 4 7 12))
               (nconcat (h °(4 3)) (rep (sel-al °2  °(1/8) °(1/8) °(1/16)) (h °(4 3))))  
               (mem (rnd °21 °50) °3 °4) 
               °90)))

(defun g-sequence-12 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore  stime ctime etime reverse))
      (sequence-12 deb fin)))

(defun partie (deb fin)
  (init)
  (sequence-12 deb fin)
  (player))

;(partie °0 (rnd °120 °300))

;; *******************************************************************     Séquence 13 (3' - 9')
;; Cécilia et voix
;; version pour 16 points       Multi M5
;; avec cécilia long répétés
;; avec cécilia aigus répétés
;; avec voix répétées
;; avec triangles aigus répétés 
;; *******************************************************************

(defun sequence-13 (deb fin) 
  
  (print-dialog "SEQUENCE 13")
  
  ;; ************* prog-change 
  (place-prog 0 m5)
  (place-tempo 0 60)


  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))


 
    ;; ************************************* cécilia long répétés  
    
    (prorythme °16 deb2 fin2
               (gnotes :laccord (concat (lst (rnd °0 °15)) (lst (rnd °0 °15)) (lst (rnd °0 °15)) (lst (rnd °0 °15))) :lecar °(0 0 0 0))
               (h °(16 18 24))
               (sel (h °(0 1 2 2 3 3 3 4 4 4 4)) °(6) °(8) °(8) °(10) °(12))
               (rnd °61 °67) ;(rnd °52 °60)
               °127)
 #|
    ;; ************************************* phrase éclatée de cécilia 
    ;; zone1
    (prorythme (add °16 (h *zone1*)) (add deb2 (h °(0 1 2 4 6 7 8 10))) fin2
               (gnotes :proba (h °(1 1 1 1 1 0)))
               (h °( 8 12 16))
               (concat °(1/2) (nconcat (h °(3 4 5 6 7))  (sel-al °4 °(1) °(2) °(2/3n (1 -1 1)) °(3) °(3))) °(10))
               (rep (rnd °51 °125) (h °(1 1 1 1 1 2 2 3 3 3 4)))
               °100)

    ;; zone2
    (prorythme (add °16 (h *zone2*)) (add deb2 (h °(2 4 6 7 8 10))) fin2
               (gnotes :proba (h °(1 1 1 1 1 0)))
               (h °( 8 12 16))
               (concat °(1/2) (nconcat (h °(3 4 5 6 7))  (sel-al °4 °(1) °(2) °(2/3n (1 -1 1)) °(3) °(3))) °(10))
               (rep (rnd °51 °125) (h °(1 1 1 1 1 2 2 3 3 3 4)))
               °100)

    ;; zone3
    (prorythme (add °16 (h *zone3*)) (add deb2 (h °(10 5 2 4 6 7 8 10))) fin2
               (gnotes :proba (h °(1 1 1 1 1 0)))
               (h °( 8 12 16))
               (concat °(1/2) (nconcat (h °(3 4 5 6 7))  (sel-al °4 °(1) °(2) °(2/3n (1 -1 1)) °(3) °(3))) °(10))
               (rep (rnd °51 °125) (h °(1 1 1 1 1 2 2 3 3 3 4)))
               °100)

|#
  
    ;; ************************************* cécilia aigus répétés
    ;; zone1
    (share-gen n1 (h °(3 4 5 6 7)))
    (prorythme (rep (add °16 (h *zone1*)) (use-gen n1)) (add deb2 (h °(1 2 3 4 5 6 7))) fin2
               (gnotes)
               (h °(16 8 20))
               (concat (nconcat (sub (use-gen n1) °1)  
                                (rep (h °((1/8) (2/3dc (1/4 1/4 1/4)) (1/4))) (sub (use-gen n1) °1)))  °(10))
               (rep (rnd °74 °83) (h °(7 14 21)))
               °100)
    (free-gen n1)


    ;; zone2
    (share-gen n1 (h °(3 4 5 6 7)))
    (prorythme (rep (add °16 (h *zone2*)) (use-gen n1)) (add deb2 (h °(1 2 3 4 5 6 7))) fin2
               (gnotes)
               (h °(16 8 20))
               (concat (nconcat (sub (use-gen n1) °1)  
                                (rep (h °((1/8) (2/3dc (1/4 1/4 1/4)) (1/4))) (sub (use-gen n1) °1)))  °(10))
               (rep (rnd °74 °83) (h °(7 14 21)))
               °100)
    (free-gen n1)

    ;; zone3
    (share-gen n1 (h °(3 4 5 6 7)))
    (prorythme (rep (add °16 (h *zone3*)) (use-gen n1)) (add deb2 (h °(1 2 3 4 5 6 7))) fin2
               (gnotes)
               (h °(16 8 20))
               (concat (nconcat (sub (use-gen n1) °1)  
                                (rep (h °((1/8) (2/3dc (1/4 1/4 1/4)) (1/4))) (sub (use-gen n1) °1)))  °(10))
               (rep (rnd °74 °83) (h °(7 14 21)))
               °100)
    (free-gen n1)
  
  

      

    ;; *************************************  voix répétées  
    ;; zone1
    (share-gen n1 (h °(3 4 5 6 7)))
    (prorythme (sel (rep (h °(0 1)) (use-gen n1)) (rep (add °16 (h *zone1*)) (use-gen n1)) (add °16 (h *zone1*))) deb2 fin2
               (gnotes)
               (h °(16 32 24 32))
               (concat (nconcat (sub (use-gen n1) °1)  
                                (rep (h °((1) (3/2) (2/3b (2 2 2)) (2) (3))) (sub (use-gen n1) °1)))  °(10))
               (rep (rnd °21 °50) (h °(7 14 21)))
               °60)
    (free-gen n1)

    ;; zone2
    (share-gen n1 (h °(3 4 5 6 7)))
    (prorythme (sel (rep (h °(0 1)) (use-gen n1)) (rep (add °16 (h *zone2*)) (use-gen n1)) (add °16 (h *zone2*))) (add deb2 (h°(2 3 4 5))) fin2
               (gnotes)
               (h °(16 32 24 32))
               (concat (nconcat (sub (use-gen n1) °1)  
                                (rep (h °((1) (3/2) (2/3b (2 2 2)) (2) (3))) (sub (use-gen n1) °1)))  °(10))
               (rep (rnd °21 °50) (h °(7 14 21)))
               °60)
    (free-gen n1)

    ;; zone3
    (share-gen n1 (h °(3 4 5 6 7)))
    (prorythme (sel (rep (h °(0 1)) (use-gen n1)) (rep (add °16 (h *zone3*)) (use-gen n1)) (add °16 (h *zone3*))) (add deb2 (h°(5 6 7 8 9 10))) fin2
               (gnotes)
               (h °(16 32 24 32))
               (concat (nconcat (sub (use-gen n1) °1)  
                                (rep (h °((1) (3/2) (2/3b (2 2 2)) (2) (3))) (sub (use-gen n1) °1)))  °(10))
               (rep (rnd °21 °50) (h °(7 14 21)))
               °60)
    (free-gen n1)
  
    
    ;; ************************************* fond général pseudo-nuit synchro  proba 1/2 
    (prorythme °16 deb2 fin2                                             
               (gnotes :laccord *gaccord*
                       :lecar *gecar*
                       :proba (h °(1 0)))
               (h °( 0 8 16 24 30 32))
               (h °( (8) (16) (24) (32)))
               (rnd °52 °54) 
               °60)
  
    ;; ************************************* triangles aigus répétés  
    (share-gen n1 (h °(3 4 5 6 7)))
    (prorythme (rep (rnd °0 °15) (use-gen n1)) deb2 fin2
               (gnotes :stac °8)
               (h °(16 8 24))
               (concat (nconcat (sub (use-gen n1) °1)  
                                (rep (h °((1/8) (2/3dc (1/4 1/4 1/4)) (1/4))) (sub (use-gen n1) °1)))  °(10))
               (rep (rnd °76 °83) (h °(7 14 21)))
               °110)
    (free-gen n1)
  
    ;; ************************************* gouttes cécilia aigus répétés  
    (share-gen n1 (h °(3 4 5 6 7)))
    (prorythme (rep (rnd °16 °31) (use-gen n1)) deb2 fin2
               (gnotes)
               (h °(8 4 16 32 24))
               (concat (nconcat (sub (use-gen n1) °1)  
                                (rep (h °((1/8) (2/3dc (1/4 1/4 1/4)) (1/4))) (sub (use-gen n1) °1)))  °(10))
               (rep (rnd °79 °85) (h °(7 14 21)))
               °90)
    (free-gen n1)
  
    ;; ************************************* fond général Rivière (proba 1/3)
    (prorythme °0 deb2 fin2
               (gnotes :laccord *gaccord*
                       :lecar *gecar*
                       :proba (h°(1)))        
               °24
               °(48)
               (rnd °126 °127) 
               °80)))

(defun g-sequence-13 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore  stime ctime etime reverse))
      (sequence-13 deb fin)))

(defun partie (deb fin)
  (init)
  (sequence-13 deb fin)
  (player))

;(partie °0 (rnd °120 °300))

;; *******************************************************************     Séquence 14 (3' - 8')
;; Motif doux et bouclé
;; version pour 16 points       Multi M5
;; avec motifs cécilia doux aigu et bouclés 
;; avec coucou cécilia
;; avec itératifs cécilia 
;; avec rivière
;; *******************************************************************

(defun sequence-14 (deb fin) 
  
  (print-dialog "SEQUENCE 14")
  
  ;; ************* prog-change 
  (place-prog 0 m5)
  (place-tempo 0 60)


  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))
  
  
    ;; ************************************* motifs cécilia doux aigu et bouclés 
    ;; zone1
    (prorythme (add °16 (h *zone1*)) deb2 fin2
               (gnotes :proba (h °(1 1 1 1 1 0)))
               (h °(0 1 2 3 4 6 8 10 12))
               (h °((8) (9) (10) (11) (14) (4)))
               (rnd °96 °111)
               (h °(80 100 110)))
  
        ;; zone2
    (prorythme (add °16 (h *zone2*)) deb2 fin2
               (gnotes :proba (h °(1 1 1 1 1 0)))
               (h °(0 1 2 3 4 6 8 10 12))
               (h °((8) (9) (10) (11) (14) (4)))
               (rnd °96 °111)
               (h °(80 100 110)))

        ;; zone3
    (prorythme (add °16 (h *zone3*)) deb2 fin2
               (gnotes :proba (h °(1 1 1 1 1 0)))
               (h °(0 1 2 3 4 6 8 10 12))
               (h °((8) (9) (10) (11) (14) (4)))
               (rnd °96 °111)
               (h °(80 100 110)))
  
    ;; ************************************* coucou cécilia
    (prorythme (rnd °16 °31) (add deb2 (h °(0 1 2 4 6 7 8 10))) fin2
               (gnotes :proba (h °(1 1 1 1 1 0)))
               (h °(0 2 4 6 8 10))
               (h °((8) (9) (10) (11) (14) (4)))
               (rnd °55 °67)
               °90)
  
    ;; ************************************* itératifs cécilia 
    (share-gen repet (rnd °1 °6))
    (prorythme (rnd °16 °31) (add deb2 (h °(0 1 2 4 6 7 8 10))) fin2
               (gnotes :proba (h °(1 1 1 1 1 0)))
               (h °(0 2 4 6 8 10))
               (nconcat (use-gen repet) (h °((1/4) (1/4) (1/8))))
               (rnd °67 °96)
               (rep (h °(80 100)) (use-gen repet)))
    (free-gen repet)
  
    ;; ************************************* cécilia coucou (n=111)  
    (share-gen n1 (h °(2 3)))
    (prorythme (rep (rnd °16 °31) (mult °2 (use-gen n1))) deb2 fin2
               (gnotes)
               (h °(8 15/2 32 24))
               (nconcat (use-gen n1) °(2 -1))
               °111
               (rep (h °(80 100 117)) (mult °2 (use-gen n1)) ))
    (free-gen n1)
  
    ;; ************************************* fond monocanal Rivière par moment (proba 1/3)
    (prorythme (rnd °0 °15) deb2 fin2
               (gnotes :proba (h°(1 0)) :laccord *gaccord-demi* :lecar *gecar*)        
               °24
               °(12)
               (rnd °126 °127) 
               °70)
  
    ;; ************************************* fond general court pseudo-nuit  proba 1/2 
    (prorythme °16 deb2 fin2                                             
               (gnotes :laccord *gaccord*
                       :lecar *gecar*
                       :proba (h °(1 0)))
               (h °( 0 8 16 20 4))
               (h °( (4) (6) (8) (10)))
               (rnd °52 °54) 
               °60)))

(defun g-sequence-14 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore  stime ctime etime reverse))
      (sequence-14 deb fin)))

(defun partie (deb fin)
  (init)
  (sequence-14 deb fin)
  (player))

;(partie °0 (rnd °120 °300))

;; *******************************************************************     Séquence 15 (3' - 8')
;; Gouttes 0       Multi M5
;;
;; version pour 16 points
;; goutte aigüe
;; note longue (ou moins longue) et aigüe rare
;; *******************************************************************

(defun sequence-15 (deb fin) 
  
  (print-dialog "SEQUENCE 15")
  
  ;; ************* prog-change 
  (place-prog 0 m5)
  (place-tempo 0 60)


  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))
  
  
  
    ;; ************************************* goutte aigüe 
    (prorythme °16 (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
               °(1/2)
               °77
               °80)
    (prorythme °17 (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel (floo (i °5 °0)) °1 °2 °3 °4 °5 °6)
               °(1/2)
               °77
               °80)
    (prorythme °18 (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel (floo (i °5 °0)) °2 °3 °4 °5 °6 °7)
               °(1/2)
               °77
               °80)
    (prorythme °19 (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel (floo (i °5 °0)) °3 °4 °5 °6 °7 °8)
               °(1/2)
               °77
               °80)
    (prorythme °20 (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel (floo (i °5 °0)) °4 °5 °6 °7 °8 °9)
               °(1/2)
               °77
               °80)
    (prorythme °21 (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel (floo (i °5 °0)) °5 °6 °7 °8 °9 °10)
               °(1/2)
               °77
               °80)
    (prorythme °22 (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel (floo (i °5 °0)) °5 °6 °7 °8 °9 °10)
               °(1/2)
               °77
               °80)
    (prorythme °23 (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel (floo (i °5 °0)) °5 °6 °7 °8 °9 °10)
               °(1/2)
               °77
               °80)
    (prorythme °24 (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel (floo (i °5 °0)) °5 °6 °7 °8 °9 °10)
               °(1/2)
               °77
               °80)
    (prorythme °25 (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel (floo (i °5 °0)) °5 °6 °7 °8 °9 °10)
               °(1/2)
               °77
               °80)
    (prorythme °26 (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel (floo (i °5 °0)) °5 °6 °7 °8 °9 °10)
               °(1/2)
               °77
               °80)
    (prorythme °27 (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel (floo (i °5 °0)) °5 °6 °7 °8 °9 °10)
               °(1/2)
               °77
               °80)
    (prorythme °28 (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel (floo (i °5 °0)) °5 °6 °7 °8 °9 °10)
               °(1/2)
               °77
               °80)
    (prorythme °29 (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel (floo (i °5 °0)) °5 °6 °7 °8 °9 °10)
               °(1/2)
               °77
               °80)
    (prorythme °30 (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel (floo (i °5 °0)) °5 °6 °7 °8 °9 °10)
               °(1/2)
               °77
               °80)
    (prorythme °31 (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel (floo (i °5 °0)) °5 °6 °7 °8 °9 °10)
               °(1/2)
               °77
               °80)
  
    ;; ************************************* fond general court pseudo-nuit  proba 1/2 
    (prorythme °16 deb2 fin2                                             
               (gnotes :laccord *gaccord*
                       :lecar *gecar*
                       :proba (h °(1 1 0)))
               (h °(8 16 24))
               (h °((8) (12)))
               °52 
               °70)
  
    ;; ************************************* note longue et aigüe rare 
    (prorythme °16 deb2 fin2                                             
               (gnotes :laccord (concat (lst(rnd °0 °15)) (lst(rnd °0 °15)) (lst (rnd °0 °15))) :lecar °(0 0 0))
               (h °( 0 1 15/2 3))
               (h °((16) (8)))
               (rnd °79 °95) 
               °80)
  
    ;; ************************************* note moins longue et aigüe rare 
    (prorythme (rnd °16 °31) deb2 fin2                                             
               (gnotes)
               (h °( 0 1 15/2 3))
               (h °((2) (5) (8)))
               (rnd °79 °95) 
               °80)
  
    ;; ************************************* note moins longue et aigüe rare, plus tard 
    (prorythme (rnd °16 °31) (floo (div fin2 °2)) fin2                                             
               (gnotes)
               (h °( 0 1 15/2 3))
               °(1/4 1)
               (rnd °79 °95) 
               °90)))

(defun g-sequence-15 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore  stime ctime etime reverse))
      (sequence-15 deb fin)))

(defun partie (deb fin)
  (init)
  (sequence-15 deb fin)
  (player))

;(partie °0 (rnd °120 °240))

;; *******************************************************************     Séquence 16 (3' - 6')
;; Gouttes 1        Multi M5
;; 
;; version pour 16 points       
;; goutte aigüe même rythme
;; note moins longue (ou moins longue) et aigüe rare, rep 2 fois
;; percu, répétée 2 fois
;; *******************************************************************

(defun sequence-16 (deb fin) 
  
  (print-dialog "SEQUENCE 16")
  
  ;; ************* prog-change 
    (place-prog 0 m5)
  (place-tempo 0 60)


  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))
  
  ;; ************************************* goutte aigüe même rythme 
  (prorythme (rep (rnd °16 °31) (h °(2 4 6))) (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
             (gnotes)
             (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
             °(1/2)
             °77
             °90)
  (prorythme (rep (rnd °16 °31) (h °(2 4 6))) (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
             (gnotes)
             (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
             °(1/2)
             °77
             °90)
  (prorythme (rep (rnd °16 °31) (h °(2 4 6))) (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
             (gnotes)
             (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
             °(1/2)
             °77
             °90)
  (prorythme (rep (rnd °16 °31) (h °(2 4 6))) (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
             (gnotes)
             (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
             °(1/2)
             °77
             °90)
  (prorythme (rep (rnd °16 °31) (h °(2 4 6))) (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
             (gnotes)
             (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
             °(1/2)
             °77
             °90)
  (prorythme (rep (rnd °16 °31) (h °(2 4 6))) (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
             (gnotes)
             (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
             °(1/2)
             °77
             °90)
  
  ;; ************************************* fond general court pseudo-nuit  proba 1/2   
  (prorythme °16 deb2 (sub fin2 °60)                                           
             (gnotes :laccord *gaccord*
                     :lecar *gecar*
                     :proba (h °(1 1 0)))
             (h °( 15 8 4 10))
             (h °( (10) (6) (8) (7)))
             (dif (rnd °52 °54)) 
             °70)
  
  ;; ************************************* note moins longue et aigüe rare, rep 2 fois  
  (prorythme (rep (rnd °16 °31) °2) (floo (div fin2 °2)) fin2                                             
             (gnotes)
             (h °( 0 1 15/2 3))
             °(1/4 1)
             (rnd °79 °95) 
             °90)
  
  ;; ************************************* note moins longue et aigüe rare, rep 2 fois
  (prorythme (rep (rnd °16 °31) °2) (floo (div fin2 °2)) fin2                                             
             (gnotes)
             (h °( 0 1 15/2 3))
             °(1/4 1)
             (rnd °79 °95) 
             °90)
  
  ;; *************************************  rep 2 fois  
  (prorythme (rep (rnd °0 °15) °2) deb2 fin2                                             
             (gnotes)
             (h °( 0 1 15/2 3))
             °(1/4 2)
             (rnd °21 °68) 
             °80)
  
  ;; ************************************* percu, répétée 2 fois  
  (prorythme (rep (rnd °0 °15) °2) (add °6 deb2) fin2                                             
             (gnotes)
             (h °( 0 1 15/2 3))
             °(1/8 1)
             (rnd °69 °96) 
             °90)))

(defun g-sequence-16 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore  stime ctime etime reverse))
      (sequence-16 deb fin)))

(defun partie (deb fin)
  (init)
  (sequence-16 deb fin)
  (player))

;(partie °0 (rnd °120 °240))

;; *******************************************************************     Séquence 17 (2' - 5') --------> cette séquence n'est plus jouée (5-11-2013)
;; OUI et Non avec trainées aigüe       Multi M5
;;
;; version pour 16 points
;; voix et notes répétée
;;  
;; *******************************************************************

;; (l 4 (nconcat-spe4 °3 (hs °(0 1 2 3 4)))) ---> ((4 2 1) (0 3 2) (0 3 4) (1 3 4))

(defun sequence-17 (deb fin) 
  
  
  (print-dialog "SEQUENCE 17")
  
  ;; ************* prog-change 
  (place-prog 0 m5)
  (place-tempo 0 60)


  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))
  
    ;; ************************************* voix répétée  
    (prorythme °16 (add deb2 (h°(0 1/2 1 1/4 3/2)))  (floo (mult fin2 °0.80)) 
               (gnotes :laccord (rep (nconcat-spe4 °3 (hs °(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))) (h °(2 4 6))) :lecar °(0 0 0))
               (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
               °(1/2)
               (rep (h °(37 50)) °20)
               °60)


    (prorythme °16 (floo (mult fin2 °0.80))  fin2 
               (gnotes :laccord (rep (nconcat-spe4 °3 (hs °(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))) (h °(2 4 6))) :lecar °(0 0 0))
               °5
               °(1/2)
               (rep (h °(37 50)) °20)
               °60)
  
    ;; ************************************* note répétée 
    ;; zone1
    (prorythme (rep (add °16 (h *zone1*)) (h °(2 4 6))) (add deb2 (h°(0 1/2 1 1/4 3/2)))  (floo (mult fin2 °0.80)) 
               (gnotes)
               (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
               °(1/2)
               °80
               °100)
    (prorythme (rep (add °16 (h *zone1*)) (h °(2 4 6))) (floo (mult fin2 °0.80))  fin2 
               (gnotes)
               °5
               °(1/2)
               °80
               °100)
  
    ;; ************************************* note répétée
    ;; zone1
    (prorythme (rep (add °16 (h *zone1*)) (h °(2 4 6))) (add deb2 (h°(0 1/2 1 1/4 3/2)))  (floo (mult fin2 °0.80)) 
               (gnotes)
               (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
               °(1/2)
               °77
               °100)
    (prorythme (rep (add °16 (h *zone1*)) (h °(2 4 6))) (floo (mult fin2 °0.80))  fin2 
               (gnotes)
               °5
               °(1/2)
               °77
               °100)
  
    ;; ************************************* note répétée 
    ;; zone2
    (prorythme (rep (add °16 (h *zone2*)) (h °(2 4 6))) (add deb2 (h°(0 1/2 1 1/4 3/2)))  (floo (mult fin2 °0.80)) 
               (gnotes)
               (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
               °(1/2)
               °70
               °110)
    (prorythme (rep (add °16 (h *zone2*)) (h °(2 4 6))) (floo (mult fin2 °0.80))  fin2 
               (gnotes)
               °5
               °(1/2)
               °70
               °110)
  
    ;; ************************************* note répétée 
    ;; zone2
    (prorythme (rep (add °16 (h *zone2*)) (h °(2 4 6))) (add deb2 (h°(0 1/2 1 1/4 3/2)))  (floo (mult fin2 °0.80)) 
               (gnotes)
               (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
               °(1/2)
               °77
               °100)
    (prorythme (rep (add °16 (h *zone2*)) (h °(2 4 6))) (floo (mult fin2 °0.80))  fin2 
               (gnotes)
               °5
               °(1/2)
               °77
               °100)
  
    ;; ************************************* note répétée 
    ;; zone3
    (prorythme (rep (add °16 (h *zone3*)) (h °(2 4 6))) (add deb2 (h°(0 1/2 1 1/4 3/2)))  (floo (mult fin2 °0.80)) 
               (gnotes)
               (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
               °(1/2)
               °90
               °100)
    (prorythme (rep (add °16 (h *zone3*)) (h °(2 4 6))) (floo (mult fin2 °0.80))  fin2 
               (gnotes)
               °5
               °(1/2)
               °90
               °100)

        ;; ************************************* note répétée 
    ;; zone3
    (prorythme (rep (add °16 (h *zone3*)) (h °(2 4 6))) (add deb2 (h°(0 1/2 1 1/4 3/2)))  (floo (mult fin2 °0.80)) 
               (gnotes)
               (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
               °(1/2)
               °77
               °100)
    (prorythme (rep (add °16 (h *zone3*)) (h °(2 4 6))) (floo (mult fin2 °0.80))  fin2 
               (gnotes)
               °5
               °(1/2)
               °77
               °100)
  
  
    ;; ************************************* note moins longue et aigüe rare, rep 2 fois  
    (prorythme (rep (rnd °16 °31) °2) (floo (div fin2 °3)) fin2                                             
               (gnotes)
               (h °( 0 1 15/2 3))
               °(1/4 1)
               (rnd °79 °95) 
               °80)
  
    (prorythme (rep (rnd °16 °31) °2) (floo (mult fin2 °0.80))  fin2                                             
               (gnotes)
               (h °( 0 1 15/2 3))
               °(1/4 1)
               (rnd °79 °95) 
               °80)
  
    ;; ************************************* fond general court pseudo-nuit  proba 1/2   
    (prorythme °16 deb2 (sub fin2 °60)                                            
               (gnotes :laccord *gaccord* 
                       :lecar *gecar*
                       :proba (h °(1 1 0)))
               (h °(16 24 30 50))
               (h °( (4) (6) (8) (3)))
               °52 
               °60)))

(defun g-sequence-17 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore  stime ctime etime reverse))
      (sequence-17 deb fin)))

(defun partie (deb fin)
  (init)
  (sequence-17 deb fin)
  (player))

;(partie °0 (rnd °120 °300))

;; *******************************************************************     Séquence 18 (3' - 7')
;; Petites gouttes à densité croissante
;; version pour 16 points       Multi M5
;; avec triangle
;; avec faux insectes
;; avec fond nuit
;; *******************************************************************



(defun sequence-18 (deb fin) 
  
  (print-dialog "SEQUENCE 18")
  
  ;; ************* prog-change 
  (place-prog 0 m5)
  (place-tempo 0 60)


  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))
  
  ;; ************************************* goutte aigüe même rythme   
  (prorythme (rep (rnd °16 °31) (h °(2 3 4 5 6))) (add deb2 (h°(0 1/8 1/16 1/2 1 1/4 3/2)))  fin2 
             (gnotes)
             (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
             °(1/2)
             °77
             °97)
  (prorythme (rep (rnd °16 °31)(h °(2 3 4 5 6))) (add deb2 (h°(0 1/8 1/16 1/2 1 1/4 3/2)))  fin2 
             (gnotes)
             (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
             °(1/2)
             °81
             °70)
  (prorythme (rep (rnd °16 °31)(h °(2 3 4 5 6))) (add °10 deb2 (h°(0 1/8 1/16 1/2 1 1/4 3/2)))  fin2 
             (gnotes)
             (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
             °(1/2)
             °81
             °70)
  (prorythme (rep (rnd °16 °31) (h °(2 3 4 5 6))) (add deb2 (h°(0 1/8 1/16 1/2 1 1/4 3/2)))  fin2 
             (gnotes)
             (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
             °(1/2)
             °84
             °100)
  (prorythme (rep (rnd °16 °31) (h °(2 3 4 5 6))) (add deb2 (h°(0 1/8 1/16 1/2 1 1/4 3/2)))  fin2 
             (gnotes)
             (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
             °(1/2)
             °80
             °90)
  (prorythme (rep (rnd °16 °31) (h °(2 3 4 5 6))) (add deb2 (h°(0 1/8 1/16 1/2 1 1/4 3/2)))  fin2 
             (gnotes)
             (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
             °(1/2)
             °77
             °70)
  (prorythme (rep (rnd °16 °31) (h °(2 3 4 5 6))) (add deb2 (h°(0 1/8 1/16 1/2 1 1/4 3/2)))  fin2 
             (gnotes)
             (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
             °(1/2)
             °77
             °90)
  (prorythme (rep (rnd °16 °31) (h °(2 3 4 5 6))) (add deb2 (h°(0 1/8 1/16 1/2 1 1/4 3/2)))  fin2 
             (gnotes)
             (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
             °(1/2)
             °77
             °97)
  (prorythme (rep (rnd °16 °31) (h °(2 3 4 5 6))) (add deb2 (h°(0 1/8 1/16 1/2 1 1/4 3/2)))  fin2 
             (gnotes)
             (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
             °(1/2)
             °77
             °97)
  
  ;; ************************************* faux insectes, rep 2 fois 
  ;; zone1
  (prorythme (rep (h *zone1*) °2) deb2 fin2                                             
             (gnotes)
            (seq (h °( 0 1 15/2 3)) (h °(0 0 1 2)))
             °(1/4 1)
             (rnd °84 °125) 
             °80)
  
  ;; zone2
  (prorythme (rep (h *zone2*) °2) (add °6 deb2) fin2                                             
             (gnotes)
             (h °( 0 1 15/2 3))
             °(1/4 1)
             (rnd °84 °125) 
             °80)

   ;; zone3
  (prorythme (rep (h *zone3*) °2) (add °6 deb2) fin2                                             
             (gnotes)
             (h °( 0 1 15/2 3))
             °(1/4 1)
             (rnd °84 °125) 
             °80)
  
  ;; ************************************* triangle-cloche, rep 2 fois  
  (prorythme (rep (rnd °0 °15) °2) deb2 fin2                                             
             (gnotes)
             (h °( 0 1 15/2 3))
             °(1/4 1)
             (rnd °21 °83) 
             °50)
  
  ;; ************************************* fond general court pseudo-nuit  proba 1/2   
  (prorythme °16 deb2 fin2                                             
             (gnotes :laccord *gaccord*
                     :lecar *gecar*
                     :proba (h °(1 1 0)))
             (h °( 16 20 24 30))
             (h °( (4) (6) (8) (3)))
             (rnd °52 °54) 
             °90)))

(defun g-sequence-18 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore  stime ctime etime reverse))
      (sequence-18 deb fin)))


(defun partie (deb fin)
  (init)
  (sequence-18 deb fin)
  (player))

;(partie °0 (rnd °120 °240))

;; *******************************************************************     Séquence 19 (4' - 7')
;; Gouttes répétées et gong et voix répétées
;; version pour 16 points       Multi M5
;; avec triangle régulier tout canaux
;; avec voix soufflée rare
;; avec voix rares tout canaux 2 fois
;; *******************************************************************

(defun sequence-19 (deb fin)     
  
  (print-dialog "SEQUENCE 19")
  
  ;; ************* prog-change 
  (place-prog 0 m5)
  (place-tempo 0 60)


  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))

        (n-prorythme (add deb2 (h °(1 2 3 4 5 6 7 8 10 0 1/2 1/4 1/8 1/16))) fin2 (h°(1 2 3 4 0 1/4 1/8 1/16 ))
                 (n-gnotes (rnd °16 °31) (h °((7) (8) (9) (10)  (12) (14))) °77 °100))
  
    (n-prorythme (add deb2 (h °(1 2 3 4 5 6 7 8 10 0 1/2 1/4 1/8 1/16))) fin2 (h°(1 2 3 4 0 1/4 1/8 1/16 ))
                 (n-gnotes (rnd °16 °31) (h °((7) (8) (9) (10)  (12) (14))) °77 °100))
  
    (n-prorythme (add deb2 (h °(1 2 3 4 5 6 7 8 10 0 1/2 1/4 1/8 1/16))) fin2 (h°(1 2 3 4 0 1/4 1/8 1/16 ))
                 (n-gnotes (rnd °16 °31) (h °((7) (8) (9) (10)  (12) (14))) °77 °100))
  
    (prorythme (add °16 (h *zone1*)) (add deb2 (h°(0 1/2 1 1/4 3/2))) fin2 
               (gnotes)
               (sel-al (floo (i °4 °0)) °0 °2 °4 °8 °16)
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °127)
    (prorythme (add °16 (h *zone1*)) (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel-al (floo (i °4 °0)) °0 °2 °4 °8 °16)
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °127)
    (prorythme (add °16 (h *zone1*)) (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel-al (floo (i °4 °0)) °0 °2 °4 °8 °16)
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °127)
    (prorythme (add °16 (h *zone2*)) (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel-al (floo (i °4 °0)) °0 °2 °4 °8 °16)
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °127)
    (prorythme (add °16 (h *zone2*)) (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel-al (floo (i °4 °0)) °0 °2 °4 °8 °16)
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °127)
    (prorythme (add °16 (h *zone2*)) (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel-al (floo (i °4 °0)) °0 °2 °4 °8 °16)
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °127)
    (prorythme (add °16 (h *zone2*)) (add deb2 (h°(0 1/2 1 1/4 3/2))) fin2 
               (gnotes)
               (sel-al (floo (i °4 °0)) °0 °2 °4 °8 °16)
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °127)
    (prorythme (add °16 (h *zone3*)) (add (h°(0 1/2 1 1/4 3/2)) (floo (div fin2 °2))) fin2 
               (gnotes)
               (sel-al (floo (i °4 °0)) °0 °2 °4 °8 °16)
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °127)
    (prorythme (add °16 (h *zone3*)) (add (h°(0 1/2 1 1/4 3/2)) (floo (div fin2 °2))) fin2 
               (gnotes)
               (sel-al (floo (i °4 °0)) °0 °2 °4 °8 °16)
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °127)
    (prorythme (add °16 (h *zone3*)) (add (h°(0 1/2 1 1/4 3/2)) (floo (div fin2 °2))) fin2 
               (gnotes)
               (sel-al (floo (i °4 °0)) °0 °2 °4 °8 °16)
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °120)
  
  
    ;; ***************************************** triangle régulier tout canaux (pitch= 30)  
  (prorythme °0 (add deb2 °24) fin2 
             (gnotes :laccord *gaccord* 
                     :lecar *gecar*)
             °8
             °(36)
             °36
             °25)

;; ***************************************** apparitions de rivières  
  (prorythme °0 (add °20 deb2) fin2                                             
             (gnotes :laccord *gaccord*
                     :lecar *gecar*)
             (h °(8 16 24))
             (h °( (8) (16) (24) (6)))
             (rnd °126 °127) 
             °60)

     ;; ***************************************** voix soufflée rare (pitch= 27 ou 43)  
  (prorythme (rnd °16 °31) (add deb2 (h°(0 1/2 1 1/4 3/2))) fin2 
             (gnotes)
             (sel-al °3 °6 °10 °16 °20)
             °(1/4 1/4 1/4)
             °43
             °60)
  

  
  ;; ***************************************** voix rares tout canaux 2 fois (pitch= 30) 
  (prorythme (rnd °16 °31) (add deb2 °7) fin2 
             (gnotes)
             (h °(0 8 16 8 4))
             (sel (h °(0 0 0 0 1)) °(1/8) °(1/4 1/4 1/4))
             (rep (rnd °21 °50) °2)
             °70)))


(defun g-sequence-19 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore  stime ctime etime reverse))
      (sequence-19 deb fin)))

(defun partie (deb fin)
  (init)
  (sequence-19 deb fin)
  (player))

;(partie °0 (rnd °180 °300))

;; *******************************************************************     Séquence 20 (3' - 7')
;; Trajectoires          Multi M2
;;
;; version pour 16 points
;; percus rares répétées 
;; trajectoires completes sur les 16 enceintes enchainées
;; *******************************************************************

(defun sequence-20 (deb fin) 
  
  (print-dialog "SEQUENCE 20")
  
  ;; ************* prog-change 
  (place-prog 0 m2)
  (place-tempo 0 60)


  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))
  
  
    ;; ************************************* un peu de "tenu" jour / nuit brèves aléatoires synchro   
    (prorythme °16 (add deb2 (h °(0 1 2 4 6 7 8 10))) fin2                                        
               (gnotes :laccord *gaccord* 
                       :lecar *gecar*)
               (h °(20 20 32 44))
               (sel-al °5  °(10) °(5) °(6) °(7) °(8) °(9))
               (rnd °21 °68) 
               °40)
  
    ;; ************************************* percus rares répétées 
    (share-gen n1 (h °( 1 2 2 2 2 3 3 3 3 4 4 4)))
    
    (n-prorythme  deb2 fin2  (h °(3 4 5 7 9 12 15))
                  (sel-al °3 
                          (n-gnotes (rep (h *zone1*) (use-gen n1))
                                    (xconcat (use-gen n1)  (sel-al °3   °(4) °(3) °(5) °(8)))
                                    (rep (h °(70 79 82)) (use-gen n1))
                                    °90)
                          (n-gnotes (rep (h *zone2*) (use-gen n1))
                                    (xconcat (use-gen n1)  (sel-al °3   °(4) °(3) °(5) °(8)))
                                    (rep (h °(70 79 82)) (use-gen n1))
                                    °90)
                          (n-gnotes (rep (h *zone3*) (use-gen n1))
                                    (xconcat (use-gen n1)  (sel-al °3   °(4) °(3) °(5) °(8)))
                                    (rep (h °(70 79 82)) (use-gen n1))
                                    °90)
                          
                          (n-gnotes (rnd °0 °15)
                                    °(12)
                                    (rnd °69 °125)
                                    °80)))
    (free-gen n1)
    
  
    ;; ************************************* percus rares répétées  
    (share-gen n1 (h °( 1 2 2 2 2 3 3 3 3 4 4 4)))
    
    (n-prorythme  deb2 fin2  (h °(3 4 5 7 9 12 15))
                  (sel-al °3 
                          (n-gnotes (rep (h *zone1*) (use-gen n1))
                                    (xconcat (use-gen n1)  (sel-al °3   °(4) °(3) °(5) °(8)))
                                    (rep (h °(70 79 82)) (use-gen n1))
                                    °90)
                          (n-gnotes (rep (h *zone2*) (use-gen n1))
                                    (xconcat (use-gen n1)  (sel-al °3   °(4) °(3) °(5) °(8)))
                                    (rep (h °(70 79 82)) (use-gen n1))
                                    °90)
                          (n-gnotes (rep (h *zone3*) (use-gen n1))
                                    (xconcat (use-gen n1)  (sel-al °3   °(4) °(3) °(5) °(8)))
                                    (rep (h °(70 79 82)) (use-gen n1))
                                    °90)
                          
                          (n-gnotes (rnd °0 °15)
                                    °(12)
                                    (rnd °69 °125)
                                    °80)))
    (free-Gen n1)
    



  
    ;; ************************************* trajectoires completes sur les 16 enceintes enchainées 
    (share-gen t1 (sel (h °(0 1 2 4))
                       °(0 1 4 5 6 7 12 13 15 14 11 10 9 8 3 2)
                       °(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
                       °(15 14 13 12 10 9 8 7 6 5 4 3 2 1 0)
                       °(0 2 3 1 4 8 9 5 6 10 11 7 12 14 15 14 11 7 6 10 9 5 4 8 3 1)
                       (nconcat-spe4 °16 (hs °(0 1 4 5 6 7 12 13 15 14 11 10 9 8 3 2)))))


    (sel (rnd °0 °3)
         (share-gen ntraj (h °( 4 5 6 7 8 9 10 12 14)))

         (prorythme (s (use-gen t1)) (add deb2 (h °(0 1 2 4 6 7 8 10))) fin2                           
                    (gnotes)                                                                  
                    (h°(7 9 11 17))
                    (sel (rnd °0 °3)                                                                
                         (xconcat (use-gen ntraj)  °(2/3c(1/4 1/4 1/4)))                   
                         (xconcat (use-gen ntraj)  °(1/8 1/8 1/8))
                         (xconcat (use-gen ntraj)  °(1/4 1/4 1/4))
                         (xconcat (use-gen ntraj)  °(1/2 1/2 1/2)))
                    (rep (h °(69 70 71 74 76 77 78 79 80 81 92 93 94 95 96 108))  (mult °3 (use-gen ntraj))) 
                    °80)
         (free-gen ntraj)
         
         
         
         (share-gen ntraj (h °( 4 5 6 7 8 9 10 12 14)))

         (prorythme (add °16 (s (use-gen t1))) (add deb2 (h °(0 1 2 4 6 7 8 10))) fin2                           
                    (gnotes)                                                                  
                    (h°(7 9 11 17))
                    (sel (rnd °0 °3)                                                                
                         (xconcat (use-gen ntraj)  °(2/3c(1/4 1/4 1/4)))                   
                         (xconcat (use-gen ntraj)  °(1/8 1/8 1/8))
                         (xconcat (use-gen ntraj)  °(1/4 1/4 1/4))
                         (xconcat (use-gen ntraj)  °(1/2 1/2 1/2)))
                    (rep (rnd °69  °125) (mult °3 (use-gen ntraj))) 
                    °80)
         (free-gen ntraj))
    (free-gen t1)))

(defun g-sequence-20 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore  stime ctime etime reverse))
      (sequence-20 deb fin)))

(defun partie (deb fin)
  (init)
  (sequence-20 deb fin)
  (player))

;(partie °0 (rnd °180 °360))

;; *******************************************************************     Séquence 21 (2' - 5')

;; version pour 16 points groupés         Multi M7
;; trajectoires circulaires de vois aléatoires parfois doublées par des oiseaux et/ou percu
;; rythme constant °(1/8 1/8 1/8 1/8) ou °(1)

;; voix aléatoires répétées de plus en plus et circulairement
;; oiseaux/percu aléatoires répétées de plus en plus et circulairement et synchrone aux voix

;; *******************************************************************

(defun sequence-21 (deb fin) 
  
  (print-dialog "SEQUENCE 21")
  
  
  ;; ************* prog-change 
  (place-prog 0 m7)
  (place-tempo 0 60)
  
  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))
  

  
    ;; ************* voix aléatoires répétées de plus en plus et circulairement
    (share-gen inter (h °(0 0 0 1 2 2 2 3 3 4 5 6 10)))
    (share-gen velos (add °-30 (h °(70 80 90 100 110))))
    (share-gen choix (h °(0 0 0 0 0 1 1 1 1 1 2 3 4)))
    (prorythme (s (concat (lst (rnd °0 °15)) *traj-total-direct2*)) (add deb2 °0) fin2                       
               (gnotes :laccord (sel (use-gen choix) °(0) °(0) °(0) *gaccord-demi* *gaccord-demi*) :lecar *gecar* )
               (use-gen inter)
               (sel (use-gen choix) °(1/8 1/8 1/8 1/8) °(1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8) °(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) °(4) °(4 4))
               (rep (rnd °21 °50) (s °(1 2 3 4 5 6 7 8 9)) )
               (add °-10 (sel (use-gen choix) (rep (use-gen velos)  (s °(1 2 3 4 5 6 7))) (rep (use-gen velos)  (s °(1 2 3 4 5 6 7))) (rep (use-gen velos)  (s °(1 2 3 4 5 6 7))) °40 (s °(40 40)))))
    ;; ************* oiseaux/percu aléatoires répétées de plus en plus et circulairement et synchrone aux voix
    (prorythme (s *traj-total-direct2*) (add deb2 °0) fin2                       
               (gnotes :proba (rep (h °(0 1 1)) (rnd °2 °16)) :laccord (sel (use-gen choix) °(0) °(0) °(0) *gaccord-demi* *gaccord-demi*) :lecar *gecar*)
               (use-gen inter)
               (sel (use-gen choix) °(1/8 1/8 1/8 1/8) °(1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8 1/8) °(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) °(4) °(4 4))
               (rep (rnd °69 °125) (s °(1 2 3 4 5 6 7 8 9)) )
               (sel (use-gen choix) (rep (use-gen velos)  (s °(1 2 3 4 5 6 7))) (rep (use-gen velos)  (s °(1 2 3 4 5 6 7))) (rep (use-gen velos)  (s °(1 2 3 4 5 6 7))) °40 (s °(40 40))))
    (free-gen inter)
    (free-gen velos)
    (free-gen choix)))

(defun g-sequence-21 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore  stime ctime etime reverse))
      (sequence-21 deb fin)))

(defun partie (deb fin)
  (init)
  (sequence-21 deb fin)
  (player))

;(partie °0 (rnd °180 °360))

;; *******************************************************************     Séquence 22 (2' - 5') --------> cette séquence n'est plus jouée (5-11-2013)

;; version pour 16 points groupés         Multi M7
;; trajectoires circulaires de voix aléatoires répétées parfois doublées par des percus
;; rythme constant °(1/8 1/8 1/8 1/8)
;; staccato variables

;; *******************************************************************

(defun sequence-22 (deb fin) 
  
  (print-dialog "SEQUENCE 22")
  
  
  ;; ************* prog-change 
  (place-prog 0 m7)
  (place-tempo 0 60)
  
  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))
  

  
     ;; ************* voix aléatoires répétées et circulairement
    (share-gen inter (h °(0 1 1 2 2 3 3 4 5 6 10)))
    (share-gen velos (add °-30 (h °(70 80 90 100 110))))
    (share-gen repet (h °(1 2 3 4 5 6 7 8 9 10)))
    (prorythme (s (concat (lst (rnd °0 °15)) *traj-total-direct2*)) (add deb2 °0) fin2                       
               (gnotes :stac (rep (h °(0.2 5.0)) (rnd °8 °24)))
               (use-gen inter)
               °(1/8 1/8 1/8 1/8 1/8)
              (rep (rnd °21 °50) (s °(1 2 3 4 5 6 7 8)) )
               (rep (use-gen velos)  (use-gen repet)))
    ;; ************* percu aléatoires répétées et circulairement et synchrone aux voix
    (prorythme (s *traj-total-direct*) (add deb2 °0) fin2                       
               (gnotes)
               (use-gen inter)
               °(1/8 1/8 1/8 1/8 1/8)
              (rep (rnd °69 °125) (s °(1 2 3 4 5 6 7 8)) )
               (rep (use-gen velos)  (use-gen repet))))
  (free-gen inter)
  (free-gen velos)
  (free-gen repet))

(defun g-sequence-22 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore  stime ctime etime reverse))
      (sequence-22 deb fin)))

(defun partie (deb fin)
  (init)
  (sequence-22 deb fin)
  (player))

;(partie °0 (rnd °120 °300))

;; *******************************************************************     Séquence 23 (2' - 5')

;; version pour 16 points groupés         Multi M7
;; trajectoires circulaires de voix aléatoires longuement répétées 
;; alternées avec petits objets, long staccato
;; avec accidents de percu 

;; *******************************************************************

(defun sequence-23 (deb fin)
  
  (print-dialog "SEQUENCE 23")
  
  
  ;; ************* prog-change 
  (place-prog 0 m7)
  (place-tempo 0 60)
  
  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))
  

  
    ;; ************* voix aléatoires longuement répétées
    
    (prorythme (s (concat (lst (rnd °0 °15)) (sel (h °(0 1)) *traj-total-direct2* *traj-zig-zag*))) (add deb2 °0) fin2                       
               (gnotes :stac °5.0)
               (h °(0 0 0 0 0 0 0 0 1 2 0 0 0 0 0 0 0 0 0 0 0 0 1 2 4 8))
               (sel (h °(0 0 0 0 0 0 0 0 0 0 1 1 2)) °(1/8 1/8 1/8 1/8) °(1/4 1/4) °(1/4 1/4 1/4 1/4))
              (rep (hs °(23 28 30 32 38 39 40 41 42 43 44 45 46 47 48 49)) (rnd °60 °80))              
              (add °-20 (s °(110 75 75 75 75 75 75 75 75 75 75 75 75 75 75 75 75))))
    ))

(defun g-sequence-23 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore  stime ctime etime reverse))
      (sequence-23 deb fin)))

(defun partie (deb fin)
  (init)
  (sequence-23 deb fin)
  (player))

;(partie °0 (rnd °120 °300))

;; *******************************************************************     Séquence 24 (3' - 5')

;; version pour 16 points groupés         Multi M7
;; trajectoires circulaires de voix aléatoires répétées 
;; trajectoires circulaires de percus répétées désynchronisées

;; *******************************************************************

(defun sequence-24 (deb fin) 
  
  (print-dialog "SEQUENCE 24")
  
  
  ;; ************* prog-change 
  (place-prog 0 m7)
  (place-tempo 0 60)
  
  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))
  

  
    ;; ************* voix  aléatoires répétées
    
    
    
     (prorythme (s (sel (h °(0 1)) *traj-total-direct2* *traj-zig-zag2*)) (add deb2 °0) fin2                       
               (gnotes :stac °5.0)
               (h °(12 16 20))  ;(h °(6 8 10 16 20))
               (nconcat (rnd °3 °10)  (sel (h °(0 0 0 0 0 0 1 1 2)) °(1/8 1/8 1/8 1/8) °(1/4 1/4) °(1/4 1/4 1/4 1/4)))
              (rep (rnd °21 °50) (h °(1 2 3 4 5 6 7 8 9 10 11 12)) )
               (rep (h °(50 40 60))  (h °(1 2 3 4 5 6 7 8 9 10))))

    ;; ************* percus  aléatoires répétées asynchrones

    (prorythme (s *traj-total-indirect*) (add deb2 °0) fin2                       
               (gnotes)
               (h °(0 0 0 0 0 0 0 0 0 0 1 2 4 4 6))
               (sel (h °(0 0 0 0 1 1 2)) °(1/8 1/8 1/8 1/8) °(1/4 1/4) °(1/4 1/4 1/4 1/4))
              (rep (rnd °69 °125) (h °(1 2 3 4 5 6 7 8 9 10 11 12)) )
               (rep (h °(80 90 100))  (h °(1 2 3 4 5 6 7 8 9 10))))))

(defun g-sequence-24 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore  stime ctime etime reverse))
      (sequence-24 deb fin)))

(defun partie (deb fin)
  (init)
  (sequence-24 deb fin)
  (player))

;(partie °0 (rnd °180 °300))

;; *******************************************************************     Séquence 25 (3' - 6')
;; Gouttes 1        Multi M3
;; 
;; version pour 16 points       
;; goutte aigüe même rythme
;; note moins longue et aigüe rare, rep 2 fois
;; guiro, rep 2 fois
;; percu, rep 2 fois
;; *******************************************************************

(defun sequence-25 (deb fin) 
  
  (print-dialog "SEQUENCE 25")
  
  ;; ************* prog-change 
  (place-prog 0 m3)
  (place-tempo 0 60)


  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))
  
    ;; ************************************* goutte aigüe même rythme 
    (prorythme (rep (rnd °16 °31) (h °(2 4 6))) (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
               °(1/2)
               °78
               (seq °70 (floo (i °70 °30))))
    (prorythme (rep (rnd °16 °31) (h °(2 4 6))) (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
               °(1/2)
               °78
               (seq °70 (floo (i °70 °30))))
    (prorythme (rep (rnd °16 °31) (h °(2 4 6))) (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
               °(1/2)
               °78
               (seq °70 (floo (i °70 °30))))
    (prorythme (rep (rnd °16 °31) (h °(2 4 6))) (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
               °(1/2)
               °78
               (seq °70 (floo (i °70 °30))))
    (prorythme (rep (rnd °16 °31) (h °(2 4 6))) (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
               °(1/2)
               °77
               (seq °80 °80 (floo (i °80 °30))))
    (prorythme (rep (rnd °16 °31) (h °(2 4 6))) (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
               °(1/2)
               °77
               (seq °80 °80 (floo (i °80 °30))))
    (prorythme (rep (rnd °16 °31) (h °(2 4 6))) (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
               °(1/2)
               °77
               (seq °80 °80 (floo (i °80 °30))))
    (prorythme (rep (rnd °16 °31) (h °(2 4 6))) (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
               °(1/2)
               °77
               (seq °80 °80 (floo (i °80 °30))))
  
    ;; ************************************* fond general court pseudo-nuit  proba 1/2   
    (prorythme °16 deb2 fin2                                             
               (gnotes :laccord *gaccord*
                       :lecar *gecar*
                       :proba (h °(1 1 0)))
               (h °( 15 8 4 10 20 30))
               (h °( (10) (6) (8) (7) (12)))
               (dif (rnd °52 °54)) 
               °50)
  
    ;; ************************************* note moins longue et aigüe rare, rep 2 fois  
    (prorythme (rep (rnd °16 °31) °2) (floo (div fin2 °2)) (sub fin2 °20)                                             
               (gnotes)
               (h °( 0 1 15/2 3))
               °(1/4 1)
               (rnd °79 °95) 
               °70)
  
    ;; ************************************* note moins longue et aigüe rare, rep 2 fois
    (prorythme (rep (rnd °16 °31) °2) (floo (div fin2 °2)) fin2                                             
               (gnotes)
               (h °( 0 1 15/2 3))
               °(1/4 1)
               (rnd °79 °95) 
               °70)
  
    ;; ************************************* guiro, rep 2 fois  
    (prorythme (rep (rnd °0 °15) °2) deb2 (sub fin2 °10)                                             
               (gnotes)
               (h °( 0 1 15/2 3))
               °(1/4 2)
               (rnd °21 °68) 
               °70)
  
    ;; ************************************* percu, rep 2 fois  
    (prorythme (rep (rnd °0 °15) °2) (add °6 deb2) (sub fin2 °15)                                           
               (gnotes)
               (h °( 0 1 15/2 3))
               °(1/8 1)
               (rnd °69 °96) 
               °80)))

(defun g-sequence-25 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore  stime ctime etime reverse))
      (sequence-25 deb fin)))

(defun partie (deb fin)
  (init)
  (sequence-25 deb fin)
  (player))

;(partie °0 (rnd °180 °360))

;; *******************************************************************     Séquence 26 (3' - 6')
;; Guirro généralisé
;; version pour 16 points       Multi M4
;; avec faux insectes, rep 2 fois
;; *******************************************************************

(defun sequence-26 (deb fin) 
  
  (print-dialog "SEQUENCE 26")
  
  ;; ************* prog-change 
  (place-prog 0 m4)
  (place-tempo 0 60)


  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))
  
  ;; ************************************* 
  (prorythme (rep (rnd °16 °31) (h °(2 3 4 5 6))) (add deb2 (h°(0 1/8 1/16 1/2 1 1/4 3/2)))  fin2 
             (gnotes)
             (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
             °(1/2)
             °77
             °60)
  (prorythme (rep (rnd °16 °31)(h °(2 3 4 5 6))) (add deb2 (h°(0 1/8 1/16 1/2 1 1/4 3/2)))  fin2 
             (gnotes)
             (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
             °(1/2)
             °77
             °30)
  (prorythme (rep (rnd °16 °31) (h °(2 3 4 5 6))) (add deb2 (h°(0 1/8 1/16 1/2 1 1/4 3/2)))  fin2 
             (gnotes)
             (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
             °(1/2)
             °77
             °40)
  (prorythme (rep (rnd °16 °31) (h °(2 3 4 5 6))) (add deb2 (h°(0 1/8 1/16 1/2 1 1/4 3/2)))  fin2 
             (gnotes)
             (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
             °(1/2)
             °77
             °60)
  (prorythme (rep (rnd °16 °31) (h °(2 3 4 5 6))) (add deb2 (h°(0 1/8 1/16 1/2 1 1/4 3/2)))  fin2 
             (gnotes)
             (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
             °(1/2)
             °77
             °70)
  (prorythme (rep (rnd °16 °31) (h °(2 3 4 5 6))) (add deb2 (h°(0 1/8 1/16 1/2 1 1/4 3/2)))  fin2 
             (gnotes)
             (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
             °(1/2)
             °77
             °60)
  (prorythme (rep (rnd °16 °31) (h °(2 3 4 5 6))) (add deb2 (h°(0 1/8 1/16 1/2 1 1/4 3/2)))  fin2 
             (gnotes)
             (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
             °(1/2)
             °77
             °60)
  (prorythme (rep (rnd °16 °31) (h °(2 3 4 5 6))) (add deb2 (h°(0 1/8 1/16 1/2 1 1/4 3/2)))  fin2 
             (gnotes)
             (sel (floo (i °5 °0)) °0 °1 °2 °3 °4 °5)
             °(1/2)
             °77
             °70)
  
  ;; ************************************* faux insectes, rep 2 fois 
  (prorythme (rep (rnd °0 °15) °2) deb2 fin2                                             
             (gnotes)
            (seq (h °( 0 1 15/2 3)) (h °(0 0 1 2)))
             °(1/4 1)
             (rnd °84 °125) 
             (h °(60 70 80)))
  
  (prorythme (rep (rnd °0 °15) °2) (add °6 deb2) fin2                                             
             (gnotes)
             (h °( 0 1 15/2 3))
             °(1/4 1)
             (rnd °84 °125) 
             (h °(60 70 80)))
  
  ;; ************************************* triangle, rep 2 fois  
  (prorythme (rep (rnd °0 °15) °2) deb2 fin2                                             
             (gnotes)
             (h °( 0 1 15/2 3 5))
             °(1/4 1)
             (rnd °33 °83) 
             °60)
  
  ;; ************************************* fond general court pseudo-nuit  proba 2/3   
  (prorythme °16 deb2 fin2                                             
             (gnotes :laccord *gaccord*
                     :lecar *gecar*
                     :proba (h °(1 1 0)))
             (h °(  12 16 24))
             (h °( (4) (6) (8) (10) (12)))
             (rnd °51 °54) 
             °45)))

(defun g-sequence-26 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore  stime ctime etime reverse))
      (sequence-26 deb fin)))

(defun partie (deb fin)
  (init)
  (sequence-26 deb fin)
  (player))

;(partie °0 (rnd °120 °300))

;; *******************************************************************     Séquence 27 (3' - 6')
;; Guirro, oiseaux, triangle en deux coups, faux insectes
;; version pour 16 points       Multi M8
;; *******************************************************************

(defun sequence-27 (deb fin) 
  
  (print-dialog "SEQUENCE 27")
  
  ;; ************* prog-change 
  (place-prog 0 m8)
  (place-tempo 0 60)


  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))
  
  ;; *************************************   
  (prorythme (rep (add °16 (h *zone1*)) (h °(2 3 4 5 6))) (add deb2 (h°(0 1/8 1/16 1/2 1 1/4 3/2)))  fin2 
             (gnotes)
             (sel (floo (i °5 °0)) °1 °2 °3 °4 °5 °6)
             °(1/2)
             °77
             °70)
  (prorythme (rep (add °16 (h *zone1*)) (h °(2 3 4 5 6))) (add deb2 (h°(0 1/8 1/16 1/2 1 1/4 3/2)))  fin2 
             (gnotes)
             (sel (floo (i °5 °0)) °1 °2 °3 °4 °5 °6)
             °(1/2)
             °81
             °80)
  (prorythme (rep (add °16 (h *zone2*))(h °(2 3 4 5 6))) (add °10 deb2 (h°(0 1/8 1/16 1/2 1 1/4 3/2)))  fin2 
             (gnotes)
             (sel (floo (i °5 °0)) °1 °2 °3 °4 °5 °6)
             °(1/2)
             °81
             °80)
  (prorythme (rep (add °16 (h *zone2*)) (h °(2 3 4 5 6))) (add deb2 (h°(0 1/8 1/16 1/2 1 1/4 3/2)))  fin2 
             (gnotes)
             (sel (floo (i °5 °0)) °1 °2 °3 °4 °5 °6)
             (sel (h °(0 0 1)) °(1/2) °(3/2))
             °84
             °100)
  (prorythme (rep (add °16 (h *zone2*)) (h °(2 3 4 5 6))) (add deb2 (h°(0 1/8 1/16 1/2 1 1/4 3/2)))  fin2 
             (gnotes)
             (sel (floo (i °5 °0)) °1 °2 °3 °4 °5 °6)
             (sel (h °(0 0 1)) °(1/2) °(3/2))
             °80
             °80)
  (prorythme (rep (add °16 (h *zone3*)) (h °(2 3 4 5 6))) (add deb2 (h°(0 1/8 1/16 1/2 1 1/4 3/2)))  fin2 
             (gnotes)
             (sel (floo (i °5 °0)) °1 °2 °3 °4 °5 °6)
             °(1/2)
             °77
             °60)
  (prorythme (rep (add °16 (h *zone3*)) (h °(2 3 4 5 6))) (add deb2 (h°(0 1/8 1/16 1/2 1 1/4 3/2)))  fin2 
             (gnotes)
             (sel (floo (i °5 °0)) °1 °2 °3 °4 °5 °6)
             °(1/2)
             °77
             °60)
  
  


  
  ;; ************************************* faux insectes, rep 2 fois 
  (prorythme (rep (rnd °0 °15) °2) deb2 fin2                                             
             (gnotes)
            (seq (h °( 0 1 15/2 3)) (h °(0 0 1 2)))
             °(1/4 1)
             (rnd °84 °125) 
             (h °(60 70 80)))
  
  (prorythme (rep (rnd °0 °15) °2) (add °6 deb2) fin2                                             
             (gnotes)
             (h °( 0 1 15/2 3))
             °(1/4 1)
             (rnd °84 °125) 
             (h °(60 70 80)))
  
  ;; ************************************* triangle, rep 2 fois  
  (prorythme (rep (rnd °0 °15) °2) deb2 fin2                                             
             (gnotes)
             (h °( 0 1 15/2 3 4 5))
             °(1/4 1)
             (rnd °45 °83) 
             °50)
  

  ;; ************************************* fond general court pseudo-nuit  proba 1/2   
  (prorythme °16 deb2 fin2                                             
             (gnotes :laccord *gaccord*
                     :lecar *gecar*
                     :proba (h °(1 1 0)))
             (h °(  12 16 24))
             (h °( (4) (6) (8) (12) (10) (14)))
             (rnd °52 °54) 
             °45)))

(defun g-sequence-27 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore  stime ctime etime reverse))
      (sequence-27 deb fin)))

(defun partie (deb fin)
  (init)
  (sequence-27 deb fin)
  (player))

;(partie °0 (rnd °120 °300))

;; *******************************************************************     Séquence 28 (4' - 6')
;; Gouttes (cécilia) répétées et gong et voix répétées
;; version pour 16 points       Multi M5
;; voix soufflée rare
;; triangle régulier tout canaux
;; *******************************************************************

(defun sequence-28 (deb fin)     
  
  (print-dialog "SEQUENCE 28")
  
  ;; ************* prog-change 
  (place-prog 0 m5)
  (place-tempo 0 60)


  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))
  

  
    (n-prorythme (add deb2 (h °(1 2 3 4 5 6 7 8 10 0 1/2 1/4 1/8 1/16))) fin2 (h°(1 2 3 4 0 1/4 1/8 1/16 ))
                 (n-gnotes (rnd °16 °31) (h °((7) (8) (9) (10)  (12) (14))) °77 °100))
  
    (n-prorythme (add deb2 (h °(1 2 3 4 5 6 7 8 10 0 1/2 1/4 1/8 1/16))) fin2 (h°(1 2 3 4 0 1/4 1/8 1/16 ))
                 (n-gnotes (rnd °16 °31) (h °((7) (8) (9) (10)  (12) (14))) °77 °100))
  
    (n-prorythme (add deb2 (h °(1 2 3 4 5 6 7 8 10 0 1/2 1/4 1/8 1/16))) fin2 (h°(1 2 3 4 0 1/4 1/8 1/16 ))
                 (n-gnotes (rnd °16 °31) (h °((7) (8) (9) (10)  (12) (14))) °77 °100))
  
    (prorythme (rnd °16 °31) (add deb2 (h°(0 1/2 1 1/4 3/2))) fin2 
               (gnotes)
               (sel-al (floo (i °4 °0)) °0 °2 °4 °8 °16)
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °100)
    (prorythme (rnd °16 °31) (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel-al (floo (i °4 °0)) °0 °2 °4 °8 °16)
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °100)
    (prorythme (rnd °16 °31) (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel-al (floo (i °4 °0)) °0 °2 °4 °8 °16)
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °100)
    (prorythme (rnd °16 °31) (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel-al (floo (i °4 °0)) °0 °2 °4 °8 °16)
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °100)
    (prorythme (rnd °16 °31) (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel-al (floo (i °4 °0)) °0 °2 °4 °8 °16)
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °100)
    (prorythme (rnd °16 °31) (add deb2 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel-al (floo (i °4 °0)) °0 °2 °4 °8 °16)
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °100)
    (prorythme (rnd °16 °31) (add deb2 (h°(0 1/2 1 1/4 3/2))) fin2 
               (gnotes)
               (sel-al (floo (i °4 °0)) °0 °2 °4 °8 °16)
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °100)
    (prorythme (rnd °16 °31) (add (h°(0 1/2 1 1/4 3/2)) (floo (div fin2 °2))) fin2 
               (gnotes)
               (sel-al (floo (i °4 °0)) °0 °2 °4 °8 °16)
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °100)
    (prorythme (rnd °16 °31) (add (h°(0 1/2 1 1/4 3/2)) (floo (div fin2 °2))) fin2 
               (gnotes)
               (sel-al (floo (i °4 °0)) °0 °2 °4 °8 °16)
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °100)
    (prorythme (rnd °16 °31) (add (h°(0 1/2 1 1/4 3/2)) (floo (div fin2 °2))) fin2 
               (gnotes)
               (sel-al (floo (i °4 °0)) °0 °2 °4 °8 °16)
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °100)
  

; ****************************


  
    (n-prorythme (add deb2 °60 (h °(1 2 3 4 5 6 7 8 10 0 1/2 1/4 1/8 1/16))) fin2 (h°(1 2 3 4 0 1/4 1/8 1/16 ))
                 (n-gnotes °16 (h °((7) (8) (9) (10)  (12) (14))) °77 °100))
  
    (n-prorythme (add deb2 °60 (h °(1 2 3 4 5 6 7 8 10 0 1/2 1/4 1/8 1/16))) fin2 (h°(1 2 3 4 0 1/4 1/8 1/16 ))
                 (n-gnotes °17 (h °((7) (8) (9) (10)  (12) (14))) °77 °100))
  
    (n-prorythme (add deb2 °60 °18) fin2 (h°(1 2 3 4 0 1/4 1/8 1/16 ))
                 (n-gnotes °18 (h °((7) (8) (9) (10)  (12) (14))) °77 °100))
  
    (prorythme °19 (add deb2 °60 (h°(0 1/2 1 1/4 3/2))) fin2 
               (gnotes)
               (sel-al (floo (i °4 °0)) °0 °2 °4)
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °100)
    (prorythme °20 (add deb2 °60 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel-al (floo (i °4 °0)) °0 °2 °4)
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °100)
    (prorythme °21 (add deb2 °60 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel-al (floo (i °4 °0)) °0 °2 °4)
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °100)
    (prorythme °22 (add deb2 °60 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel-al (floo (i °4 °0)) °0 °2 °4)
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °100)
    (prorythme °23  (add deb2 °60 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel-al (floo (i °4 °0)) °0 °2 °4)
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °100)
    (prorythme °24 (add deb2 °60 (h°(0 1/2 1 1/4 3/2)))  fin2 
               (gnotes)
               (sel-al (floo (i °4 °0)) °0 °2 °4)
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °100)
    (prorythme °25 (add deb2 °60 (h°(0 1/2 1 1/4 3/2))) fin2 
               (gnotes)
               (sel-al (floo (i °4 °0)) °0 °2 °4)
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °100)
    (prorythme °26 (add °60 (h°(0 1/2 1 1/4 3/2)) (floo (div fin2 °2))) fin2 
               (gnotes)
               (sel-al (floo (i °4 °0)) °0 °2 °4)
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °100)
    (prorythme °27 (add °60 (h°(0 1/2 1 1/4 3/2)) (floo (div fin2 °2))) fin2 
               (gnotes)
               (sel-al (floo (i °4 °0)) °0 °2 °4 °8 °16)
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °100)
    (prorythme °28 (add °60 (h°(0 1/2 1 1/4 3/2)) (floo (div fin2 °2))) fin2 
               (gnotes)
               (sel-al (floo (i °4 °0)) °0 °2 °4)
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °100)
    (prorythme °29 (add °60 (h°(0 1/2 1 1/4 3/2)) (floo (div fin2 °2))) fin2 
               (gnotes)
               (sel-al (floo (i °4 °0)) °0 °2 °4)
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °100)
    (prorythme °30 (add °60 (h°(0 1/2 1 1/4 3/2)) (floo (div fin2 °2))) fin2 
               (gnotes)
               (sel-al (floo (i °4 °0)) °0 °2 °4)
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °100)
    (prorythme °31 (add °60 (h°(0 1/2 1 1/4 3/2)) (floo (div fin2 °2))) fin2 
               (gnotes)
               (sel-al (floo (i °4 °0)) °0 °2 °4)
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °100)


; ****************************


  
    (n-prorythme (add deb2 °120) fin2 (floo (h°(1/8 1/16 1/32 )))
                 (n-gnotes °16 (h °((7) (8) (9) (10)  (12) (14))) °77 °100))
  
    (n-prorythme (add deb2 °120) fin2 (floo (h°(1/8 1/16 1/32 )))
                 (n-gnotes °17 (h °((7) (8) (9) (10)  (12) (14))) °77 °100))
  
    (n-prorythme (add deb2 °100) fin2 (floo (h°(1/8 1/16 1/32 )))
                 (n-gnotes °18 (h °((7) (8) (9) (10)  (12) (14))) °77 °100))
  
    (prorythme °19 (add deb2 °100 (floo (h°(1/8 1/16 1/32 )))) fin2 
               (gnotes)
               (floo (h°(1/8 1/16 1/32 )))
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °100)
    (prorythme °20 (add deb2 °120 (h°(1/8 1/16 1/32 )))  fin2 
               (gnotes)
               (floo (h°(1/8 1/16 1/32 )))
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °100)
    (prorythme °21 (add deb2 °120 (h°(1/8 1/16 1/32 )))  fin2 
               (gnotes)
               (floo (h°(1/8 1/16 1/32 )))
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °100)
    (prorythme °22 (add deb2 °120 (h°(1/8 1/16 1/32 )))  fin2 
               (gnotes)
               (floo (h°(1/8 1/16 1/32 )))
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °100)
    (prorythme °23  (add deb2 °120 (h°(1/8 1/16 1/32 )))  fin2 
               (gnotes)
               (floo (h°(1/8 1/16 1/32 )))
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °100)
    (prorythme °24 (add deb2 °120 (h°(1/8 1/16 1/32 )))  fin2 
               (gnotes)
               (floo (h°(1/8 1/16 1/32 )))
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °100)
    (prorythme °25 (add deb2 °120 (h°(1/8 1/16 1/32 ))) fin2 
               (gnotes)
               (h °(0 1/3 1/2 1 2))
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °100)
    (prorythme °26 (add deb2 °120 (h°(1/8 1/16 1/32 )) (floo (div fin2 °2))) fin2 
               (gnotes)
               (floo (h°(1/8 1/16 1/32 )))
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °100)
    (prorythme °27 (add deb2 °120 (h°(1/8 1/16 1/32 )) (floo (div fin2 °2))) fin2 
               (gnotes)
               (floo (h°(1/8 1/16 1/32 )))
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °100)
    (prorythme °28 (add deb2 °120 (h°(1/8 1/16 1/32 )) (floo (div fin2 °2))) fin2 
               (gnotes)
               (floo (h°(1/8 1/16 1/32 )))
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °100)
    (prorythme °29 (add deb2 °120 (h°(1/8 1/16 1/32 )) (floo (div fin2 °2))) fin2 
               (gnotes)
               (floo (h°(1/8 1/16 1/32 )))
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °100)
    (prorythme °30 (add deb2 °120 (h°(1/8 1/16 1/32 )) (floo (div fin2 °2))) fin2 
               (gnotes)
               (floo (h°(1/8 1/16 1/32 )))
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °100)
    (prorythme °31 (add °120 deb2 (h°(1/8 1/16 1/32 )) (floo (div fin2 °2))) fin2 
               (gnotes)
               (floo (h°(1/8 1/16 1/32 )))
               (sel-al (floo (i °6 °0)) °(16) °(14) °(11) °(10) °(8) °(6) °(4) °(4))
               (rnd °61 °62)
               °100)


    ;;*************************************** rivière
    (prorythme °0 (add deb2 °120) fin2 
               (gnotes :laccord *gaccord* :lecar *gecar*)
               °0
               (lst (sub fin2 °120 deb2))
               °126
               °127)

    


  ;; ***************************************** voix soufflée rare (pitch= 27 ou 43)  
  (prorythme (rep (rnd °16 °31) °3) (add deb2 (h°(0 1/2 1 1/4 3/2))) fin2 
             (gnotes :proba (h °(0 1)))
             (sel-al °5 °0 °1 °2 °4 °8 °16)
             (sel (h °(0 1)) °(1/4 1/4 1/4) °(1/4 -1/4 -1/4))
             (h °(43 27))
             °40)
  
  
  ;; ***************************************** triangle régulier tout canaux (pitch= 30)  
  (prorythme °0 (add deb2 °24) fin2 
             (gnotes :laccord *gaccord* 
                     :lecar *gecar*)
             °8
             °(36)
             °36
             °40)
  
  ;; ***************************************** apparitions de rivières  
  (prorythme °0 (add °20 deb2) fin2                                             
             (gnotes :laccord *gaccord*
                     :lecar *gecar*)
             (h °(8 16 24))
             (h °( (8) (16) (24) (6)))
             (rnd °126 °127) 
             °50)
  
  ;; ***************************************** voix rares tout canaux 2 fois (pitch= 30) 
  (prorythme (rnd °16 °31) (add deb2 °7) fin2 
             (gnotes)
             (h °(0 8 16 8 4))
             °(1/8)
             (rep (rnd °21 °50) °2)
             °40)))

(defun g-sequence-28 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore  stime ctime etime reverse))
      (sequence-28 deb fin)))

(defun partie (deb fin)
  (init)
  (sequence-28 deb fin)
  (player))

;(partie °0 (rnd °240 °360))

;; *******************************************************************     Séquence 29 (3' - 8')
;; Triangles harmoniques lent et calme, quasi synchrones     Multi M5
;; avec quelques voix rares et répétées
;; version pour 16 points
;; 
;; 
;; *******************************************************************

(defun sequence-29 (deb fin) 
  
  (print-dialog "SEQUENCE 29")
  
  ;; ************* prog-change 
  (place-prog 0 m5)
  (place-tempo 0 60)


  (let* ((deb2 (g (car (l 1 deb))))
         (fin2 (g (car (l 1 fin))))
         (ldate1 (car (l 1 (glistedate deb2 fin2 (h °(12 16 8 10))))))
         (ldate2 (car (l 1 (transf-dat ldate1 (h °(-0.7 -0.5 -0.3 -0.25 -0.2 -0.15 -0.1 -0.05 0 0.05 0.1 0.15 0.2 0.25 0.3 0.35 0.5 0.7))))))
         (ldate3 (car (l 1 (transf-dat ldate1 (h °(-0.7 -0.5 -0.3 -0.25 -0.2 -0.15 -0.1 -0.05 0 0.05 0.1 0.15 0.2 0.25 0.3 0.35 0.5 0.7))))))
         (ldate4 (car (l 1 (transf-dat ldate1 (h °(-0.7 -0.5 -0.3 -0.25 -0.2 -0.15 -0.1 -0.05 0 0.05 0.1 0.15 0.2 0.25 0.3 0.35 0.5 0.7))))))
         (ldate5 (car (l 1 (transf-dat ldate1 (h °(-0.7 -0.5 -0.3 -0.25 -0.2 -0.15 -0.1 -0.05 0 0.05 0.1 0.15 0.2 0.25 0.3 0.35 0.5 0.7))))))
         (ldate6 (car (l 1 (transf-dat ldate1 (h °(-0.7 -0.5 -0.3 -0.25 -0.2 -0.15 -0.1 -0.05 0 0.05 0.1 0.15 0.2 0.25 0.3 0.35 0.5 0.7))))))
         (ldur1 (date-dur ldate1))
         (ldur2 (date-dur ldate2))
         (ldur3 (date-dur ldate3))
         (ldur4 (date-dur ldate4))
         (ldur5 (date-dur ldate5))
         (ldur6 (date-dur ldate6)))

    (prorythme (rnd °0 °15) deb2 fin2 
               (gnotes)
               °0
               (lst (s °ldur1))
               (add (seq °0 °7 °2 °4) (h °(59 63 66 69 76)))
               (h °(90 80 70 60)))

    (prorythme (rnd °0 °15) deb2 fin2 
               (gnotes)
               °0
               (lst (s °ldur2))
               (add (seq °0 °7 °2 °4) (h °(59 63 66 69 76)))
               (h °(90 80 70 60)))

    (prorythme (rnd °0 °15) deb2 fin2 
               (gnotes)
               °0
               (lst (s °ldur3))
               (add (seq °0 °7 °2 °4) (h °(59 63 66 69 76)))
               (h °(90 80 70 60)))

    (prorythme (rnd °0 °15) deb2 fin2 
               (gnotes)
               °0
               (lst (s °ldur4))
               (add (seq °0 °7 °2 °4) (h °(59 63 66 69 76)))
               (h °(90 80 70 60)))
    
    (prorythme (rnd °0 °15) deb2 fin2 
               (gnotes)
               °0
               (lst (s °ldur5))
               (add (seq °0 °7 °2 °4) (h °(59 63 66 69 76)))
               (h °(90 80 70 60)))

    (prorythme (rnd °0 °15) deb2 fin2 
               (gnotes)
               °0
               (lst (s °ldur6))
               (add (seq °0 °7 °2 °4) (h °(59 63 66 69 76)))
               (h °(90 80 70 60)))

    ;; ************************************* demi accord
    (prorythme °0 (add °16 deb2) fin2 
               (gnotes :laccord *gaccord-demi* :lecar *gecar* )
               °0
               °(24)
               (add (seq °0 °7 °2 °4) °47)
               °25)


    ;; ************************************* voix rares répétées  
    (share-gen voix (rep (h °(42 48 49)) (rnd °3 °8)))
    (prorythme (rnd °16 °23) deb2 fin2 
               (gnotes :proba (h °(0 0 0 1)))
               °0
               (lst (s °ldur5))
               (use-gen voix)
               °30)
    (prorythme (rnd °24 °31) deb2 fin2 
               (gnotes :proba (h °(0 0 0 1)))
               °0
               (lst (s °ldur6))
               (use-gen voix)
               °30)
    (free-gen voix)))

(defun g-sequence-29 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore  stime ctime etime reverse))
      (sequence-29 deb fin)))

(defun partie (deb fin)
  (init)
  (sequence-29 deb fin)
  (player))

;(partie °1 °180)

;; *******************************************************************     Séquence 30 (3' - 6')
;; Violons aléatoires alternés côte à côte          Multi M9
;;
;; version pour 16 points
;; 
;; 
;; *******************************************************************

(defun sequence-30 (deb fin) 
  
  (print-dialog "SEQUENCE 30")
  
  ;; ************* prog-change 
  (place-prog 0 m9)
  (place-tempo 0 60)

  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))

    (prorythme (alt °0 °1) deb2 fin2 
               (gnotes)
               (h °(6 8 10 12 14))
               (h °((10) (12) (14) (11) (16)))
               (rep (rnd °55 °79) (rnd °1 °4))
               °50)
    (prorythme (alt °2 °3) deb2 fin2 
               (gnotes)
               (h °(6 8 10 12 14))
               (h °((10) (12) (14) (11) (16)))
               (rep (rnd °55 °79) (rnd °1 °4))
               °50)
    (prorythme (alt °4 °5) deb2 fin2 
               (gnotes)
               (h °(6 8 10 12 14))
               (h °((10) (12) (14) (11) (16)))
               (rep (rnd °55 °79) (rnd °1 °4))
               °50)
    (prorythme (alt °6 °7) deb2 fin2 
               (gnotes)
               (h °(6 8 10 12 14))
               (h °((10) (12) (14) (11) (16)))
               (rep (rnd °55 °79) (rnd °1 °4))
               °50)
    (prorythme (alt °8 °9) deb2 fin2 
               (gnotes)
               (h °(6 8 10 12 14))
               (h °((10) (12) (14) (11) (16)))
               (rep (rnd °55 °79) (rnd °1 °4))
               °50)
    (prorythme (alt °10 °11) deb2 fin2 
               (gnotes)
               (h °(6 8 10 12 14))
               (h °((10) (12) (14) (11) (16)))
               (rep (rnd °55 °79) (rnd °1 °4))
               °50)
    (prorythme (alt °12 °13) deb2 fin2 
               (gnotes)
               (h °(6 8 10 12 14))
               (h °((10) (12) (14) (11) (16)))
               (rep (rnd °55 °79) (rnd °1 °4))
               °50)
    (prorythme (alt °14 °15) deb2 fin2 
               (gnotes)
               (h °(6 8 10 12 14))
               (h °((10) (12) (14) (11) (16)))
               (rep (rnd °55 °79) (rnd °1 °4))
               °50)

    ))

(defun g-sequence-30 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore  stime ctime etime reverse))
      (sequence-30 deb fin)))

(defun partie (deb fin)
  (init)
  (sequence-30 deb fin)
  (player))

;(partie °0 °240)

;; *******************************************************************     Séquence 31 (3' - 5')
;; violons harmoniques lents, quasi synchrones et calmes          Multi M9
;;
;; version pour 16 points
;; 
;; 
;; *******************************************************************

(defun sequence-31 (deb fin) 
  
  (print-dialog "SEQUENCE 31")
  
  ;; ************* prog-change 
  (place-prog 0 m9)
  (place-tempo 0 60)


  (let* ((deb2 (g (car (l 1 deb))))
         (fin2 (g (car (l 1 fin))))
         (ldate1 (car (l 1 (glistedate deb2 fin2 (h °(12 16 8 10 4))))))
         (ldate2 (car (l 1 (transf-dat ldate1 (h °(-4 -3.5 -3 -2.5 -2 -1.5 -1 -0.5 0 0.5 1 1.5 2 2.5 3 3.5 4 4.5))))))
         (ldate3 (car (l 1 (transf-dat ldate1 (h °(-4 -3.5 -3 -2.5 -2 -1.5 -1 -0.5 0 0.5 1 1.5 2 2.5 3 3.5 4 4.5))))))
         (ldate4 (car (l 1 (transf-dat ldate1 (h °(-4 -3.5 -3 -2.5 -2 -1.5 -1 -0.5 0 0.5 1 1.5 2 2.5 3 3.5 4 4.5))))))
         (ldate5 (car (l 1 (transf-dat ldate1 (h °(-4 -3.5 -3 -2.5 -2 -1.5 -1 -0.5 0 0.5 1 1.5 2 2.5 3 3.5 4 4.5))))))
         (ldate6 (car (l 1 (transf-dat ldate1 (h °(-4 -3.5 -3 -2.5 -2 -1.5 -1 -0.5 0 0.5 1 1.5 2 2.5 3 3.5 4 4.5))))))
         (ldate7 (car (l 1 (transf-dat ldate1 (h °(-4 -3.5 -3 -2.5 -2 -1.5 -1 -0.5 0 0.5 1 1.5 2 2.5 3 3.5 4 4.5))))))
         (ldate8 (car (l 1 (transf-dat ldate1 (h °(-4 -3.5 -3 -2.5 -2 -1.5 -1 -0.5 0 0.5 1 1.5 2 2.5 3 3.5 4 4.5))))))
         (ldur1 (date-dur ldate1))
         (ldur2 (date-dur ldate2))
         (ldur3 (date-dur ldate3))
         (ldur4 (date-dur ldate4))
         (ldur5 (date-dur ldate5))
         (ldur6 (date-dur ldate6))
         (ldur7 (date-dur ldate7))
         (ldur8 (date-dur ldate8)))

    (prorythme (rnd °0 °7) deb2 fin2 
               (gnotes)
               °5
               (lst (s °ldur1))
               (add (seq °0 °7 °2 °9 °0) (h °(55 59 62 65 67 72)))
               °50)

    (prorythme (rnd °0 °7) deb2 fin2 
               (gnotes)
               °5
               (lst (s °ldur2))
               (add (seq °0 °7 °2 °9 °0) (h °(55 59 62 65 67 72)))
               °50)

    (prorythme (rnd °0 °7) deb2 fin2 
               (gnotes)
               °5
               (lst (s °ldur3))
               (add (seq °0 °7 °2 °9 °0) (h °(55 59 62 65 67 72)))
               °50)

    (prorythme (rnd °0 °7) deb2 fin2 
               (gnotes)
               °5
               (lst (s °ldur4))
               (add (seq °0 °7 °2 °9 °0) (h °(55 59 62 65 67 72)))
               °50)
    
    (prorythme (rnd °8 °15) deb2 fin2 
               (gnotes)
               °5
               (lst (s °ldur5))
               (add (seq °0 °7 °2 °9 °0) (h °(55 59 62 65 67 72)))
               °50)

    (prorythme (rnd °8 °15) deb2 fin2 
               (gnotes)
               °5
               (lst (s °ldur6))
               (add (seq °0 °7 °2 °9 °0) (h °(55 59 62 65 67 72)))
               °50)

    (prorythme (rnd °8 °15) deb2 fin2 
               (gnotes)
               °5
               (lst (s °ldur7))
               (add (seq °0 °7 °2 °9 °0) (h °(55 59 62 65 67 72)))
               °50)

    (prorythme (rnd °8 °15) deb2 fin2 
               (gnotes)
               °5
               (lst (s °ldur8))
               (add (seq °0 °7 °2 °9 °0) (h °(55 59 62 65 67 72)))
               °50)))

; ======================================

(defun g-sequence-31 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore  stime ctime etime reverse))
      (sequence-31 deb fin)))

(defun partie (deb fin)
  (init)
  (sequence-31 deb fin)
  (player))

;(partie °1 °240)

;; *******************************************************************     Séquence 32 (3' - 5')
;; violons harmoniques lents, synchronisés et calmes 2        Multi M9
;;
;; version pour 16 points
;; 
;; 
;; *******************************************************************

(defun sequence-32 (deb fin) 
  
  (print-dialog "SEQUENCE 32")
  
  ;; ************* prog-change 
  (place-prog 0 m9)
  (place-tempo 0 60)

  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))

    (prorythme °0 deb2 fin2 
               ;(gnotes :laccord (list-nvaleur °8 (hs °(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))) :lecar (list-nvaleur °8 (hs °(0 0 0 3.86 3.86 3.86 7.02 7.02 7.02 9.69 9.69 12 14.04 15.86 17.51 19.02 20.41))))
               (gnotes :laccord (list-nvaleur °8 (hs °(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))) :lecar (list-nvaleur °8 (hs °(0 0 0 4 4 4 7 7 7 10 10 12 14 16 18 19 20))))
               (h °(0 2 4 6 8 8 10))
               (h °((6) (10) (12) (14) (11) (16)))
               (rep (rnd °55 (floo (i °55 °68))) (rnd °1 °4))
               °50)
    ))

(defun g-sequence-32 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore  stime ctime etime reverse))
      (sequence-32 deb fin)))

(defun partie (deb fin)
  (init)
  (sequence-32 deb fin)
  (player))

;(partie °1 °240)

;; *******************************************************************     Séquence 33 (3' - 5')
;; Fond jour seul
;; version pour 16 points         Multi M2
;; avec insectes et oiseaux aléatoire

;; *******************************************************************

(defun sequence-33 (deb fin) 
  
  (print-dialog "SEQUENCE 33")
  
  ;; ************* prog-change 
  (place-prog 0 m2)
  (place-tempo 0 60)
  
  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))
  
    ;; ************* fond général jour
    (prorythme °16 deb2 fin2
               (gnotes :laccord *gaccord-demi* :lecar *gecar*)        
               °0
               (lst (sub fin2 deb2))
               (sel (h °(0 1)) (rnd °39 °60) (rnd °62 °68))  ; ne pas prendre 61
               °30)
  
    ;; ************* un peu d'oiseaux aléatoires

    (share-gen nbre (rnd °1 °3))
    (prorythme °0 (add deb2 (h °(0 2 4 6 7 8 10))) fin2                 
               (gnotes :laccord (rep (nconcat-spe4 (rnd °3 °8) (dif (rnd °16 °31))) (use-gen nbre)) :lecar *gecar*)
               (h °(4 6 8 8 10 12))
               (sel-al °4 °(1/2) °(1) °(3/2) °(3) °(5))
               (rep (rnd °69 °125) (use-gen nbre))
               (h °(60 50 40)))
    (free-gen nbre)
    ))
  
(defun g-sequence-33 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore  stime ctime etime reverse))
      (sequence-33 deb fin)))

(defun partie (deb fin)
  (init)
  (sequence-33 deb fin)
  (player))

;(partie °0 (rnd °120 °180))

;; *******************************************************************     Séquence 34 (3' - 5')
;; Rivière seule
;; version pour 16 points         Multi M2
;; avec un peu d'insectes et oiseaux aléatoire

;; *******************************************************************

(defun sequence-34 (deb fin) 
  
  (print-dialog "SEQUENCE 34")
  
  ;; ************* prog-change 
  (place-prog 0 m2)
  (place-tempo 0 60)
  
  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))
  
    ;; ************* fond général Rivière
    (prorythme °0 deb2 fin2
               (gnotes :laccord *gaccord* :lecar *gecar*)        
               °0
               (lst (sub fin2 deb2))
               °126 
               °70)
  
    ;; ************* un peu d'oiseaux aléatoires

    (share-gen nbre (rnd °1 °4))
    (prorythme °0 (add deb2 (h °(0 2 4 6 7 8 10))) fin2                 
               (gnotes :laccord (rep (nconcat-spe4 (rnd °3 °8) (rnd °16 °31)) (use-gen nbre)) :lecar *gecar*)
               (h °(4 6 8 8 10 12))
               (sel-al °4 °(1/2) °(1) °(3/2) °(3) °(5))
               (rep (rnd °69 °125) (use-gen nbre)) 
               (h °(60 50 70)))
    (free-gen nbre)))
  
(defun g-sequence-34 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore  stime ctime etime reverse))
      (sequence-34 deb fin)))

(defun partie (deb fin)
  (init)
  (sequence-34 deb fin)
  (player))

;(partie °0 (rnd °120 °180))

;; *******************************************************************     Séquence 35 (3' - 5')

;; version pour 16 points groupés         Multi M7
;; trajectoires circulaires de guiro longuement répétées, long staccato
;; avec accidents de percu 

;; *******************************************************************

(defun sequence-35 (deb fin)
  
  (print-dialog "SEQUENCE 35")
  
  ;; ************* prog-change 
  (place-prog 0 m7)
  (place-tempo 0 60)
  
  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))
  
    ;; ************* voix aléatoires longuement répétées
    
    (share-gen choix (h °(0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)))
    (prorythme (s (concat (lst (rnd °0 °15)) (sel (h °(0 1)) *traj-total-direct* *traj-zig-zag*))) (add deb2 °0) fin2                       
               (gnotes :stac °5.0)
               (h °(0 2 4 6 8 10))
               (nconcat (rnd °3 °16)  (sel (h °(0 0 0 0 0 0 1 1 2)) °(1/8 1/8 1/8 1/8) °(1/4 1/4) °(1/4 1/4 1/4 1/4)))
              (sel (use-gen choix) (rep (hs °(23 28 30 32 38 39 40 41 42 43 44 45 46 47 48 49)) (rnd °60 °80)) (rnd °69 °96))              
              (sel (use-gen choix) (add °-15 (s °(110 75 75 75 75 75 75 75 75 75 75 75 75 75 75 75 75))) °80))
    (free-gen choix)))

(defun g-sequence-35 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore  stime ctime etime reverse))
      (sequence-35 deb fin)))

(defun partie (deb fin)
  (init)
  (sequence-35 deb fin)
  (player))

;(partie °0 (rnd °120 °240))

;; *******************************************************************     Séquence 36 (3' - 6')
;; 2 phrases monorales de percu-castagnet
;; version pour 16 points         Multi M2
;; avec insectes et oiseaux aléatoire
;; avec phrase monocanal de castagnet
;; *******************************************************************

(defun sequence-36 (deb fin) 
  
  (print-dialog "SEQUENCE 36")
  
  ;; ************* prog-change 
  (place-prog 0 m2)
  (place-tempo 0 60)
  
  (let ((deb2 (g (car (l 1 deb))))
        (fin2 (g (car (l 1 fin)))))
  
    ;; ************* un peu d'oiseaux aléatoires
    (prorythme (rnd °16 °31) (add deb2 (h °(0 2 4 6 7 8 10))) fin2                 
               (gnotes)
               (h °(0 4 4 6 8 8 12))
               (sel-al °4 °(1/2) °(1) °(3/2) °(3) °(5))
               (rnd °69 °125) 
               (h °(100 70 90 80)))

    ;; ************* phrase monocanal de percu-catagnet
  
    (share-gen n1 (h °(12 13 14 15 16 17 18)))
    (prorythme (rep (h *zone1*) (use-gen n1)) °0 fin2
               (gnotes :proba (h °(1 1 1 1 1 0)))
               (h °(0 5 9 13))
               (concat °(1/2) (nconcat (sub (use-gen n1) °2)  °(1/8)) °(8)) 
               (rep (rnd °97 °125)  (h °(1 1 1 2 3 4 ))) 
               (h °(60 70 80 90 100)))
    (free-gen n1)

    (share-gen n1 (h °(12 13 14 15 16 17 18)))
    (prorythme (rep (h *zone2*) (use-gen n1)) (add deb2 (h °(0 2 4 6 7 8 10))) fin2
               (gnotes :proba (h °(1 1 1 1 1 0)))
               (h °(0 5 9 13))
               (concat °(1/2) (nconcat (sub (use-gen n1) °2)  °(1/8)) °(8)) 
               (rep (rnd °97 °125)  (h °(1 1 1 2 3 4 ))) 
               (h °(60 70 80 90 100)))
    (free-gen n1)

    (share-gen n1 (h °(12 13 14 15 16 17 18)))
    (prorythme (rep (h *zone3*) (use-gen n1)) (add deb2 (h °(0 2 4 6 7 8 10))) fin2
               (gnotes :proba (h °(1 1 1 1 1 0)))
               (h °(0 5 9 13))
               (concat °(1/2) (nconcat (sub (use-gen n1) °2)  °(1/8)) °(8)) 
               (rep (rnd °97 °125)  (h °(1 1 1 2 3 4 ))) 
               (h °(60 70 80 90 100)))
    (free-gen n1)

    ;; ************* phrase monocanal de catagnet
     (share-gen n1 (h °(12 13 14 15 16 17 18)))
    (prorythme (rep (rnd °0 °15) (h °(1 1 1 1 2 2 3 4))) (add deb2 (h °(0 2 4 6 7 8 10))) fin2
               (gnotes :proba (h °(1 1 1 1 1 0)))
               (h °(0 5 9 13))
               (concat °(1/2) (nconcat (sub (use-gen n1) °2)  °(1/8)) °(4)) 
               (rep (rnd °116 °121)  (h °(1 1 1 2 3 4 ))) 
               (h °(60 70 80 90 100)))
    (free-gen n1)))

(defun g-sequence-36 (deb fin) 
  #'(lambda (stime ctime etime reverse)
      (declare (ignore  stime ctime etime reverse))
      (sequence-36 deb fin)))

(defun partie (deb fin)
  (init)
  (sequence-36 deb fin)
  (player))

;(partie °0 (rnd °120 °600))

;; Fonction appelée automatiquement au redémarrage de l'application 
(defun restart-function ()
  (open-play-session)
  (start-dialog-callback)
)






























  
























  












