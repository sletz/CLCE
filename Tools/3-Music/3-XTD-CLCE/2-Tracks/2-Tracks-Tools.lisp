;;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;;
;;
;;                                 	 Tracks-Tools.lisp
;;
;;                                     © 1994, GRAME.
;;
;;
;;
;;
;;
;;  Outils plus evolues pour les tracks musicales
;;
;; HISTORIQUE :
;;  06-12-94, Reprise du fichier tools de "la couleur du vent" 
;;  05-12-00, Mise a jour pour la librairie partagee MIDIFilesPPC, 
;;            modification de read-track et write-track
;;  23-04-01  Gestion du port Midi dans la sauvergarde/restauration des tracks (avec des ev PortPrefix)      
;;  13-06-01  Remplacement de %null-ptr-p par nullptrp pour rendre le code independant platforme
;;  15-06-01  Suppression de l'interface avec le gestionnaire de fichier du Macintosh, les fonctions read-track et
;;            write-track sont dans un fichier par platforme, nouvelles fonctions read-track-aux et writetrack-aux
;;            multi-platforme (utilisant un filename)
;;  20-06-01  La fonction trackp est mise dans tracks-interface.lisp 
;;  21-06-01  Les fonctions trkaddev et trkaddat sont mises dans tracks-tools.lisp 
;;  25-06-01  Macptr remplace par t pour faciliter le portage Linux
;;  07-07-04  Renommage, evtype à la place de type.
;;
;;
;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/



;;===========================================================================================
;;
;; GARBAGE COLLECTING POUR LES TRACKS
;; Une sorte de GC est implemente pour les tracks de façon a permettre un programmation plus
;; fonctionnelle. La macro SET-TRACK doit-être utilisee a la place de setq chaque fois que 
;; l'on veut garder de maniere permanente une track dans une variable.
;;
;;===========================================================================================

(defvar *temp-tracks* nil)
(defvar *perm-tracks* nil)
  

(defun trkaddev (trk ev)
  (trkadd trk (trkfind trk (+ 1 (date ev))) ev))

(defun trkaddat (trk dt ev)
  (date ev dt)
  (trkadd trk (trkfind trk (+ 1 dt)) ev))
(defun add-temp-track (trk)
  (declare (special *temp-tracks*))
  (push trk *temp-tracks*)
  trk)

(defun new-temp-track ()
  (add-temp-track (trknew)))

(defun rem-temp-track (trk)
  (declare (special *temp-tracks*))
  (setq *temp-tracks* (delete  trk *temp-tracks*))
  trk)

(defun gc-temp-tracks ()
  (declare (special *temp-tracks*))
  (mapc #'(lambda (tr) (trkfree tr)) *temp-tracks*)
  (setq *temp-tracks* nil))

(defun add-perm-track (trk)
  (declare (special *perm-tracks*))
  (if (trackp trk)
    (let ((x (assoc trk *perm-tracks*)))
      (if x
        (incf (cdr x))
        (push (cons trk 1) *perm-tracks*))
      trk)))

(defun rem-perm-track (trk)
  (declare (special *perm-tracks*))
  (if (trackp trk)
    (let ((x (assoc trk *perm-tracks*)))
      (if x
        (if (> (cdr x) 1)
          (decf (cdr x))
          (progn
            (trkfree (car x))
            (setq *perm-tracks* (delete x *perm-tracks*))
            0))
        (cerror  "ignore rem-perm-track" "I can't find ~S is the permanent track list" trk)))))



;--------------------------------------------------------------------------------------------
; DEF-TRACK
; equivalent a defparameter mais permet une sorte de garbage-collecting pour les tracks intermediares
; utilisee dans le calcul de l'expression. <<exemple : (set-track toto (track)) >>


(defmacro def-track (sym exp)
  (let ((trk (gensym)))
    `(if (boundp ',sym)
       (setq ,sym (let ((*temp-tracks* nil))
                            (let ((,trk ,exp))
                              (rem-temp-track ,trk)
                              (gc-temp-tracks)
                              (add-perm-track ,trk)
                              (rem-perm-track ,sym)
                              ,trk)))
       (defparameter ,sym (let ((*temp-tracks* nil))
                            (let ((,trk ,exp))
                              (rem-temp-track ,trk)
                              (gc-temp-tracks)
                              (add-perm-track ,trk)
                              ,trk)))) 
    ))

;--------------------------------------------------------------------------------------------
; SET-TRACK
; equivalent a setq mais permet une sorte de garbage-collecting pour les tracks intermediares
; utilisee dans le calcul de l'expression. <<exemple : (set-track toto (track)) >>

(defmacro set-track (sym exp)
  (let ((trk (gensym)))
    `(setq ,sym (let ((*temp-tracks* nil))
                  (let ((,trk ,exp))
                    (rem-temp-track ,trk)
                    (gc-temp-tracks)
                    (add-perm-track ,trk)
                    (if (boundp ',sym)
                      (rem-perm-track ,sym))
                    ,trk)))))


;--------------------------------------------------------------------------------------------
; WITH-TRACK-GC
; supprime les tracks intermediares necessaires au calcul d'une expression
;   exemple : (calc-track <exp> ) 

(defmacro with-track-gc (exp)
  (let ((trk (gensym)))
    `(let (,trk)
       (let (*temp-tracks*)
         (setq ,trk ,exp)
         (rem-temp-track ,trk)
         (gc-temp-tracks))
       (add-temp-track ,trk))))


;;===========================================================================================
;;
;; MACROS DIVERSES
;;
;;===========================================================================================

;--------------------------------------------------------------------------------------------
; DOTRACK
; iterateur sur les evenements d'une track. 
; ex1 : (dotrack (i e mytrack) .... ) -- parcours mytrack du debut a la fin
; ex2 : (dotrack (i e mytrack resultat) .... ) -- comme ex1 mais retourne resultat
; ex3 : (dotrack (i e mytrack :start-pos 3 ) .... ) -- de la position 3 a la fin
; ex4 : (dotrack (i e mytrack :end-pos 6) .... ) -- du debut jusqu'a la pos 6 EXCLUE
; ex5 : (dotrack (i e mytrack :start-date 100) .... ) -- de la date 100 a la fin
; ex6 : (dotrack (i e mytrack :end-date 200) .... ) -- du debut a la date 200 EXCLUE
; Toutes ces options peuvent être panachees. En cas de conflit entre deux options c'est 
; la derniere qui sera appliquee ex: ... :start-date 1000... :start-pos 2...
; c'est :start-pos 2 qui sera utilisee. Par ailleur, une option suivie de NIL sera ignoree.
; ceci pour facilite l'implementation de fonctions qui utilisent dotrack et qui ne veulent
; pas avoir a redecoder les options.
; Si l'on veut renvoyer un resultat il doit imperativement être apres toutes les options.
; ex7 : (dotrack (i e mytrack :start-date 100 :end-date 200 resultat) .... )

(defmacro dotrack ((symindex symevent trackform . options) &body forms)
  (let ((symtrack (gensym))
        (symendpos (gensym)))
    (let ((start-form 0)
          (end-form `(trklen ,symtrack)))
      (loop
        (case (first options)
          (:start-date
           (if (second options) (setq start-form `(trkfind ,symtrack ,(second options))))
           (setq options (cddr options)))
          (:start-pos
           (if (second options) (setq start-form (second options)))
           (setq options (cddr options)))
          (:end-date
           (if (second options) (setq end-form `(trkfind ,symtrack ,(second options))))
           (setq options (cddr options)))
          (:end-pos
           (if (second options) (setq end-form (second options)))
           (setq options (cddr options)))
          (otherwise
           (return
            `(let* ((,symevent nil)
                    (,symtrack ,trackform)
                    (,symendpos ,end-form)) 
               (do ((,symindex ,start-form (+ 1 ,symindex)))
                   ((>= ,symindex ,symendpos) ,@options)
                 (setq ,symevent (trkget ,symtrack ,symindex))
                 ,@forms)))))))))


;;===========================================================================================
;;
;; FONCTIONS DE BASE
;;
;;===========================================================================================

;--------------------------------------------------------------------------------------------
; ADD-TRACK
; Ajoute des evenements a une track existante. Les arguments optionnels peuvent être des 
; evenements, des options ou des listes d'arguments. Les options possibles sont :
;   :DATE   <num>  insere les evenements qui suivent a cette date.
;   :POS    <num>  insere les evenements qui suivent a cette position
;   :OFFSET <num>  insere les evenements qui suivent a leur date plus l'offset
;   :END    <num>  positionne la date de fin de la sequence
;   :NORMAL        insere les evenements qui suivent a leur date normale
; 
; << exemple : (add-track toto :offset 1000 (note :date 0 :dur 250) (note :date 500 :dur 250) :end 5000) >>

(defun add-track (self &rest largs)
  (real-add-track self :normal 0 largs)
  self)

(defun real-add-track (self option val largs)
  (let ((resize nil))
    (dolist (x largs)
      (typecase x
        (keyword (if (eq x :end) (setq resize t) (setq option x)))
        (number (if resize 
                  (progn (trkresize self x) 
                         (setq resize nil))
                  (setq val x)))
        (cons (real-add-track self option val x))
        (t (case option
                  (:normal (trkadd self (trkfind self (+ 1 (date x))) x))
                  (:date (date x val) (trkadd self (trkfind self (+ 1 val)) x))
                  (:pos (trkadd self val x))
                  (:offset (let ((dt (+ val (date x)))) 
                             (date x dt) 
                             (trkadd self (trkfind self (+ 1 dt)) x)))
                  (otherwise (trkadd self (trkfind self (+ 1 (date x))) x))))))))

;--------------------------------------------------------------------------------------------
; TRACK
; Cree une nouvelle track. Des evenements optionnels permettent d'initialiser
; son contenu. On peut egalement mettre un nombre parmis ces arguments pour definir la duree
; totale de la track. << exemple : (track (note :dur 250) 1000) >>

(defun track (&rest lev)
  (apply #'add-track (new-temp-track) lev))


;--------------------------------------------------------------------------------------------
; TRACK-SIZE
; Donne la duree totale de la track ou, si le parametre optionnel size est fournis, change la
; duree totale de la track. << exemple : (track-size toto) -> 1000; (track-size toto 2000) >>

(defun track-size (self &optional size)
  (if size 
    (trkresize self size)
    (trksize self)))


;;===========================================================================================
;;
;; FONCTIONS DE DECOUPAGE
;; Ces fonctions ont pour but de decouper une tracks en une liste de tracks suivant 
;; differents criteres. Elles sont en quelque sorte l'inverse des fonctions de montage.
;;
;;===========================================================================================

;--------------------------------------------------------------------------------------------
; TRACK-CHORDS
; Decoupe une tracks en "accords" successifs. Chaque accord donne une nouvelle track.
; Le resultat est la liste des tracks ainsi obtenues.
;
; (track-chords <trk>) -> list of tracks

(defparameter *max-chord-delta* 20)

(defun track-chords (src &optional n)
  (declare (ignore n))
  (let* ((ltrak nil) 
         (d1 0) (d2 (+ d1 *max-chord-delta*))
         (ix 0) (maxix (trklen src))
         (dst (new-temp-track)) e1 e2 dt)
    (loop 
      (when (>= ix maxix)			; A-t'on encore des evenements a lire
        ; non, on est arrive a la fin de la piste
        (trkresize dst (- (trksize src) d1))	; il faut dimensionne la tranche
        (push dst ltrak)			; prevoir la demande d'une nouvelle track
        (return (nreverse ltrak))		; sortir de la boucle
        )
      (setq e1 (trkget src ix))			; on lit l'evenement courant e1
      (when (> (date e1) d2)			; Est-il dans la tranche ?
        ; non, on est arrive a la fin de la tranche
        (trkresize dst (- (date e1) d1))	; il faut dimensionne la tranche
        (setq d1 (date e1))			; et preparer les nouvelles
        (setq d2 (+ d1 *max-chord-delta*))	; references temporelles
        (push dst ltrak)
        (setq dst (new-temp-track))
        )
      ; on est toujours dans la tranche
      (setq e2 (midi-copy-ev e1))		; on fait une copie dans e2
      (when (not (nullptrp e2))		; la copie-est elle bonne?
        ; oui, on a pu copier l'evenement
        (setq dt (- (date e1) d1))		; offset la date de l'evenement
        (date e2 dt)
        (trkadd dst (trkfind dst (+ 1 dt)) e2)	; ajout a la tranche
        (incf ix))				; passage a l'evenement suivant
      )))

;--------------------------------------------------------------------------------------------
; TRACK-CHANS
; Decoupe une tracks en 16 tracks, suivant les 16 canaux Midi. Retourne la liste des 16 
; tracks ainsi obtenues.
;
; (track-chans <trk>) -> list of tracks

(defun track-chans (src &optional n)
  (declare (ignore n))
  (let ((a (make-array 16))
        (d (trksize src)))
    (dotimes (i 16) 
      (let ((dst (new-temp-track)))
        (trkresize dst d)
        (setf (aref a i) dst)))
    (dotrack (i e src)
      (let ((dst (aref a (chan e)))
            (e2 (midi-copy-ev e)))
        (chan e2 0)
        (add-track dst e2)))
    (coerce a 'list)))

;--------------------------------------------------------------------------------------------
; TRACK-PORTS
; Decoupe une tracks en 16 tracks, suivant les 16 premiers ports Midi. Retourne la liste des 16 
; tracks ainsi obtenues.
;
; (track-ports <trk>) -> list of tracks

(defun track-ports (n src)
  (declare (ignore n))
  (let ((a (make-array 16))
        (d (trksize src)))
    (dotimes (i 16) 
      (let ((dst (new-temp-track)))
        (trkresize dst d)
        (setf (aref a i) dst)))
    (dotrack (i e src)
      (let ((dst (aref a (mod (port e) 16)))
            (e2 (midi-copy-ev e)))
        (port e2 0)
        (add-track dst e2)))
    (coerce a 'list)))

;--------------------------------------------------------------------------------------------
; TRACK-KEYS
; Decoupe une track en une liste de 12 tracks, une par degre dans la gamme. Les octaves sont
; conservees. La relation avec les hauteurs midi est : hauteur = octave*12 + degre.
;
; (track-keys <trk>) -> list of tracks

(defun track-keys (src &optional n)
  (declare (ignore n))
  (let ((a (make-array 12))
        (d (trksize src)))
    (dotimes (i 12) 
      (let ((dst (new-temp-track)))
        (trkresize dst d)
        (setf (aref a i) dst)))
    (dotrack (i e src)
      (when (is-key-p e)
        (let* ((deg  (mod (pitch e) 12))
               (dst (aref a deg))
               (e2 (midi-copy-ev e)))
          (pitch e2 (- (pitch e2) deg))
          (add-track dst e2))))
    (coerce a 'list)))

;--------------------------------------------------------------------------------------------
; TRACK-OCTS
; Decoupe une track en une liste de 11 tracks, une par octave dans la gamme. Les degres sont
; conservees. La relation avec les hauteurs midi est : hauteur = octave*12 + degre.
;
; (track-octs <n> <trk>) -> list of tracks

(defun track-octs (src &optional n)
  (declare (ignore n))
  (let ((a (make-array 11))
        (d (trksize src)))
    (dotimes (i 11) 
      (let ((dst (new-temp-track)))
        (trkresize dst d)
        (setf (aref a i) dst)))
    (dotrack (i e src)
      (when (is-key-p e)
        (let* ((oct  (floor (pitch e) 12))
               (dst (aref a oct))
               (e2 (midi-copy-ev e)))
          (pitch e2 (- (pitch e2) (* oct 12)))
          (add-track dst e2))))
    (coerce a 'list)))

;--------------------------------------------------------------------------------------------
; TRACK-VELS
; Decoupe une track en une liste de 16 tracks, une par "tranche" de 8 unites de velocite. Ne 
; fonctionne que pour les notes.
;
; (track-vels <trk>) -> list of tracks

(defun track-vels (src &optional n)
  (declare (ignore n))
  (let ((a (make-array 16))
        (d (trksize src)))
    (dotimes (i 16) 
      (let ((dst (new-temp-track)))
        (trkresize dst d)
        (setf (aref a i) dst)))
    (dotrack (i e src)
      (when (is-note-p e)
        (let* ((v  (floor (vel e) 8))
               (dst (aref a v))
               (e2 (midi-copy-ev e)))
          (vel e2 (- (vel e2) (* v 8)))
          (add-track dst e2))))
    (coerce a 'list)))


;;===========================================================================================
;;
;; FONCTIONS DE MONTAGE
;; Ces fonctions ont pour but de construire de nouvelles tracks a partir de tracks
;; existantes suivant differentes dimensions : le temps, les hauteurs, les canaux etc.
;;
;;===========================================================================================

;;;=======================================================================================
;;; Linearisation d'une liste de track
;;;=======================================================================================

(defun walk (op lt r)
  (cond  ((null lt) 
          r)
         ((or (trackp (car lt)) (numberp (car lt)))
          (walk op (cdr lt) (cons (car lt) r)))
         ((eq (caar lt) op) 
          (walk op (cdr lt) (walk op (cdar lt) r)))
         ((not (eq (caar lt) op))
          (walk op (cons (apply (caar lt) (cdar lt)) (cdr lt)) r))
         (t 
          (error "wrong argument ~S" lt))))

(defun linearize-track-list (op lt)
  (reverse (walk op lt nil)))

;--------------------------------------------------------------------------------------------
; PAR-TRACKS
; Donne une nouvelle track dont le contenu est le montage en "parallele" des tracks passee en
; argument. Le montage est effectue en faisant coincider le debut des differentes tracks. La 
; liste des arguments peut comporter des nombres afin de faciliter les "calages".
;
; (par-tracks <trk> ou <num>, <trk> ou <num>, ...)

(defun par-tracks (&rest lt)
  (setq lt (linearize-track-list #'par-tracks lt))
  (let ((self (new-temp-track)))
    (track-size self (apply #'max (mapcar #'(lambda (x) (if (trackp x) (track-size x) x)) lt)))
    (dolist (x lt)
      (when (trackp x)
        (dotrack (i e x)
          (add-track self (midi-copy-ev e)))))
    self))

;--------------------------------------------------------------------------------------------
; EPAR-TRACKS
; Donne une nouvelle track dont le contenu est le montage en "parallele" des tracks passee en
; argument. Le montage est effectue en faisant coincider la fin des differentes tracks. La 
; liste des arguments peut comporter des nombres afin de faciliter les "calages".
;
; (epar-tracks <trk> ou <num>, <trk> ou <num>, ...)

(defun epar-tracks (&rest lt)
  (setq lt (linearize-track-list #'epar-tracks lt))
  (let ((self (new-temp-track))
        (size (apply #'max (mapcar #'(lambda (x) (if (trackp x) (track-size x) x)) lt))))
    (track-size self size)
    (dolist (x lt)
      (when (trackp x)
        (let ((offset (- size (track-size x))))
          (dotrack (i e x)
            (add-track self :offset offset (midi-copy-ev e))))))
    self))

;--------------------------------------------------------------------------------------------
; CHAN-TRACKS
; Donne une nouvelle track dont le contenu est le montage en "parallele" des tracks passee en
; argument. Le montage est effectue en faisant coincider le debut des differentes tracks et 
; en assignant a chaque track un canal midi croissant a partir de 0. La liste des arguments
; peut comporter des nombres afin de faciliter les "calages".
;
; (chan-tracks <trk> ou <num>, <trk> ou <num>, ...)

(defun chan-tracks (&rest lt)
  (setq lt (linearize-track-list #'chan-tracks lt))
  (let ((self (new-temp-track))
        (e2)
        (ch 0))
    (track-size self (apply #'max (mapcar #'(lambda (x) (if (trackp x) (track-size x) 0)) lt)))
    (dolist (x lt)
      (cond ((trackp x)
             (dotrack (i e x)
               (when (setq e2 (midi-copy-ev e))
                 (chan e2 ch)
                 (add-track self e2)))
             (incf ch 1))
            ((numberp x)
             (incf ch x))
            (t
             (error "~s is not a track or a number" x)) ))
    self))

;--------------------------------------------------------------------------------------------
; PORT-TRACKS
; Donne une nouvelle track dont le contenu est le montage en "parallele" des tracks passee en
; argument. Le montage est effectue en faisant coincider le debut des differentes tracks et 
; en assignant a chaque track un port midi croissant a partir de 0. La liste des arguments 
; peut comporter des nombres afin de faciliter les "calages".
;
; (port-tracks <trk> ou <num>, <trk> ou <num>, ...)

(defun port-tracks (&rest lt)
  (setq lt (linearize-track-list #'port-tracks lt))
  (let ((self (new-temp-track))
        (e2)
        (p 0))
    (track-size self (apply #'max (mapcar #'(lambda (x) (if (trackp x) (track-size x) 0)) lt)))
    (dolist (x lt)
      (cond ((trackp x)
             (dotrack (i e x)
               (when (setq e2 (midi-copy-ev e))
                 (port e2 p)
                 (add-track self e2)))
             (incf p 1))
            ((numberp x)
             (incf p x))
            (t
             (error "~s is not a track or a number" x)) ))
    self))

;--------------------------------------------------------------------------------------------
; KEY-TRACKS
; Donne une nouvelle track dont le contenu est le montage en "parallele" des tracks passee en
; argument. Le montage est effectue en faisant coincider le debut des differentes tracks et 
; en assignant a chaque track un degre de 0 a 11. L'octave d'origine est conservee. La relation
; avec les hauteurs midi est : hauteur = octave*12 + degre. La liste des arguments peut comporter
; des nombres afin de faciliter les "calages".
;
; (key-tracks <trk> ou <num>, <trk> ou <num>, ...)

(defun key-tracks (&rest lt)
  (setq lt (linearize-track-list #'key-tracks lt))
  (let ((self (new-temp-track))
        (e2)
        (k 0))
    (track-size self (apply #'max (mapcar #'(lambda (x) (if (trackp x) (track-size x) 0)) lt)))
    (dolist (x lt)
      (cond ((trackp x)
             (dotrack (i e x)
               (when (setq e2 (midi-copy-ev e))
                 (when (is-key-p e2)
                   (pitch e2 (+ (pitch e2) (- k (mod (pitch e2) 12)))))
                 (add-track self e2)))
             (incf k 1))
            ((numberp x)
             (incf k x))
            (t
             (error "~s is not a track or a number" x))))
    self))

;--------------------------------------------------------------------------------------------
; OCT-TRACKS
; Donne une nouvelle track dont le contenu est le montage en "parallele" des tracks passee en
; argument. Le montage est effectue en faisant coincider le debut des differentes tracks et 
; en assignant a chaque track une octave a partir de 0. Le degre d'origine est conservee. La relation
; avec les hauteurs midi est : hauteur = octave*12 + degre. La liste des arguments peut comporter
; des nombres afin de faciliter les "calages".
;
; (oct-tracks <trk> ou <num>, <trk> ou <num>, ...)

(defun oct-tracks (&rest lt)
  (setq lt (linearize-track-list #'oct-tracks lt))
  (let ((self (new-temp-track))
        (e2)
        (oct 0))
    (track-size self (apply #'max (mapcar #'(lambda (x) (if (trackp x) (track-size x) 0)) lt)))
    (dolist (x lt)
      (cond ((trackp x)
             (dotrack (i e x)
               (when (setq e2 (midi-copy-ev e))
                 (when (is-key-p e2)
                   (pitch e2 (+ (* oct 12) (mod (pitch e2) 12))))
                 (add-track self e2)))
             (incf oct 1))
            ((numberp x)
             (incf oct x))
            (t
             (error "~s is not a track or a number" x))))
    self))

;--------------------------------------------------------------------------------------------
; VEL-TRACKS
; Donne une nouvelle track dont le contenu est le montage en "parallele" des tracks passee en
; argument. Le montage est effectue en faisant coincider le debut des differentes tracks et 
; en assignant a chaque track une "tranche" de velocite a partir de 0. La liste des arguments 
; peut comporter des nombres afin de faciliter les "calages".
;
; (vel-tracks <trk> ou <num>, <trk> ou <num>, ...)

(defun vel-tracks (&rest lt)
  (setq lt (linearize-track-list #'vel-tracks lt))
  (let ((self (new-temp-track))
        (e2)
        (v 0))
    (track-size self (apply #'max (mapcar #'(lambda (x) (if (trackp x) (track-size x) 0)) lt)))
    (dolist (x lt)
      (cond ((trackp x)
             (dotrack (i e x)
               (when (setq e2 (midi-copy-ev e))
                 (when (is-note-p e2)
                   (vel e2 (+ (* v 8) (vel e2))))
                 (add-track self e2)))
             (incf v 1))
            ((numberp x)
             (incf v x))
            (t
             (error "~s is not a track or a number" x))))
    self))

;--------------------------------------------------------------------------------------------
; SEQ-TRACKS
; Donne une nouvelle track dont le contenu est le montage en "serie" des tracks passee en
; argument. Le montage est effectue en faisant coincider la fin d'une track avec le debut de 
; la suivante. La liste des arguments peut comporter des nombres afin de faciliter les "calages".
;
; (seq-tracks <trk> ou <num>, <trk> ou <num>, ...)

(defun monte-track-copy (dst src pos)
  (when (< pos 0)
    (track-size dst (- (track-size dst) pos))
    (dotrack (i e dst) 
      (date e (- (date e) pos)))
    (setq pos 0))
  (let* ((d1 (track-size dst))
         (d2 (track-size src))
         (dr (max (+ pos d2) d1)))
    (track-size dst dr)
    (dotrack (i e src) 
      (add-track dst :offset pos (midi-copy-ev e))))
  dst)

(defun mix-track-copy (dst src delta)
  (monte-track-copy dst src (+ (track-size dst) delta)))

(defun seq-tracks (&rest lt)
  (setq lt (linearize-track-list #'seq-tracks lt))
  (let ((self (new-temp-track))
        (offset 0))
    (dolist (x lt)
      (cond ((trackp x) 
             (mix-track-copy self x offset) 
             (setq offset 0))
            ((numberp x)
             (incf offset x))
            (t
             (error "~s is not a track or a number" x)) ))
    self))

;--------------------------------------------------------------------------------------------
; LOOP-TRACK
; Donne une nouvelle track dont le contenu est la repetition du contenu de la track passee
; en argument autant de fois que necessaire pour atteindre la duree souhaitee.

(defun loop-track (dur src)
  (let ((dst (new-temp-track))
        (offset 0)
        (delta (track-size src)))
    (track-size dst dur)
    (do () ((<= dur delta))
      (dotrack (i e src) (add-track dst :offset offset (midi-copy-ev e)))
      (incf offset delta)
      (decf dur delta))
    (dotrack (i e src :end-date dur) (add-track dst :offset offset (midi-copy-ev e)))
    dst))

(defun copy-track (src)
  (add-temp-track (trkcopy src)))

(defun track-first-evs (n src)
  (if (< n (trklen src))
    (let ((dst (new-temp-track)))
      (track-size dst (date (trkget src n)))
      (dotrack (i e src :end-pos n) (add-track dst (midi-copy-ev e)))
      dst)
    (copy-track src)))

(defun track-last-evs (n src)
  (setq n (- (trklen src) n))
  (if (> n 0)
    (let ((dst (new-temp-track))
          (offset (- 0 (date (trkget src n)))))
      (track-size dst (+ (track-size src) offset))
      (dotrack (i e src :start-pos n) (add-track dst :offset offset (midi-copy-ev e)))
      dst)
    (copy-track src)))

(defun track-butfirst-evs (n src)      
  (track-last-evs (- (trklen src) n) src))

(defun track-butlast-evs (n src)
  (track-first-evs (- (trklen src) n) src))

;--------------------------------------------------------------------------------------------
; REV-START
; Renverse dans le temps une track

(defun rev-track (src)
  (let ((dst (new-temp-track))
        (dur (track-size src)))
    (track-size dst dur)
    (dotrack (i e src) (add-track dst :date (- dur (date e)) (midi-copy-ev e)))
    dst))

;--------------------------------------------------------------------------------------------
; TRACK-START
; Donne une copie du debut d'une track. Par defaut il s'agit des 2 premieres secondes.
; <:dur d> permet de specifiee la duree et <:len n> la longueur en nombre d'evenements

(defun track-start (src &key (dur 2000) len)
  (if len
    (cond ((<= len 0)
           (new-temp-track))
          ((>= len (trklen src))
           (copy-track src))
          (t
           (let ((dst (new-temp-track)))
             (track-size dst (date (trkget src len)))
             (dotrack (i e src :end-pos len) (add-track dst (midi-copy-ev e)))
             dst)))
    (cond ((<= dur 0)
           (new-temp-track))
          ((>= dur (trksize src))
           (copy-track src))
          (t
           (let ((dst (new-temp-track)))
             (track-size dst dur)
             (dotrack (i e src :end-date dur) (add-track dst (midi-copy-ev e)))
             dst))) ))


;--------------------------------------------------------------------------------------------
; TRACK-END
; Donne une copie de la fin d'une track. Par defaut il s'agit des 2 dernieres secondes.
; <:dur d> permet de specifiee la duree et <:len n> la longueur en nombre d'evenements

(defun track-end (src &key (dur 2000) len)
  (if len
    (cond ((<= len 0)
           (new-temp-track))
          ((>= len (trklen src))
           (copy-track src))
          (t
           (let* ((p (- (trklen src) len))
                  (dst (new-temp-track))
                  (offset (- 0 (date (trkget src p)))))
             (track-size dst (+ (track-size src) offset))
             (dotrack (i e src :start-pos p) (add-track dst :offset offset (midi-copy-ev e)))
             dst)))
    (cond ((<= dur 0)
           (new-temp-track))
          ((>= dur (trksize src))
           (copy-track src))
          (t
           (let* ((dst (new-temp-track))
                  (d (- (trksize src) dur))
                  (offset (- 0 d)))
             (track-size dst dur)
             (dotrack (i e src :start-date d) (add-track dst :offset offset (midi-copy-ev e)))
             dst))) ))


;--------------------------------------------------------------------------------------------
; TRACK-JOINT
; Fin de T1 suivi du debut de T2. Par defaut il s'agit des 2 dernieres secondes.

(defun track-joint (t1 t2 &key (dur 2000))
  (seq-tracks (track-end t1 :dur dur) (track-start  t2 :dur dur)))


;--------------------------------------------------------------------------------------------
; TRACK-ERGN
; Donne une copie d'une partie de track entre deux positions p1 a p2 exclue

(defun track-ergn (src p1 p2 &optional transform)
  (let* ((dst (new-temp-track))
         (offset (- 0 (date (trkget src p1)))))
    (track-size dst (+ (track-size src) offset))
    (if transform
      (dotrack (i e src :start-pos p1 :end-pos p2) 
        (let ((e2 (midi-copy-ev e)))
          (funcall transform e2)
          (add-track dst :offset offset e2)))
      (dotrack (i e src :start-pos p1 :end-pos p2) 
        (add-track dst :offset offset (midi-copy-ev e))))
    (track-size dst (+ offset (if (trkget src p2) (date (trkget src p2)) (track-size src))))
    dst))

;--------------------------------------------------------------------------------------------
; TRACK-MIDDLE
; Copie d'une portion de track entre deux dates : de d1 inclue a d2 exclue

(defun track-middle (src d1 d2)
  (let* ((dst (new-temp-track))
         (offset (- d1)))
    (track-size dst (- d2 d1))
    (dotrack (i e src :start-date d1 :end-date d2) 
        (add-track dst :offset offset (midi-copy-ev e)))
    dst))


;--------------------------------------------------------------------------------------------
; TRACK2SEQ
; Convertie une track en sequence MidiShare (c'est une copie!)

(defun track2seq (src)
  (let ((s (midi-new-seq)))
    (dotrack (i e src) (midi-add-seq s (midi-copy-ev e)))
    s))


;--------------------------------------------------------------------------------------------
; KEY2NOTE-TRACK
; copie d'une track ou les couples keyon-keyoff sont remplaces par des notes

(defun key2note-track (src)
  (let ((dst (new-temp-track))
        (pending nil)
        (dur (track-size src)))
    (dotrack (i e src)
      (cond ((and (= (evtype e) typeKeyOn) (> (vel e) 0))
             (let ((c (midi-copy-ev e)))
               (setq pending (add-pending-key c pending))
               (add-track dst c)))
            ((and (= (evtype e) typeKeyOn) (= (vel e) 0))
             (setq pending (rem-pending-key e pending)))
            ((= (evtype e) typeKeyOff)
             (setq pending (rem-pending-key e pending)))
            (t
             (add-track dst (midi-copy-ev e)))))
    (dolist (e pending)
      (key2note e dur))
    dst))

; ajoute un keyon a la liste des keyon ouverts et pas encore referme.
(defun add-pending-key (e l)
  (cons e l))

; referme un keyon et le supprime de la liste
(defun rem-pending-key (e2 pending)
  (let ((k (pitch e2))
        (c (chan e2))
        (p (port e2))
        (prev nil)
        (next pending)
        (e1))
  (loop  
    (when (null next) (return pending))
    (setq e1 (pop next))
    (when (and (= (pitch e1) k)
             (= (chan e1) c)
             (= (port e1) p))
      (key2note e1 (date e2))
      (return (revappend prev next)))
    (push e1 prev))))


; referme d'office tous les keyons encore ouverts
(defun force-pending-key (lk dur)
  (dolist (e lk)
    (key2note e dur)))

; transforme un keyon en note
(defun key2note (e d)
  (evtype e typeNote)
  (dur e (- d (date e))))

;;===========================================================================================
;;
;; FONCTIONS DE VISUALISATION
;;
;;===========================================================================================

;--------------------------------------------------------------------------------------------
; PRINT-TRACK
; Affiche la liste des evenements contenus dans une track.

(defun print-track (trk &key (start-date 0) (end-date 268435454))
  (when (trackp trk)
    (format t "~%Track: Len = ~3D, Size = ~A (~D) ~%~%" (trklen trk) (midi-string-date (trksize trk)) (trksize trk))
    (dotrack (i e trk :start-date start-date :end-date end-date)
      (format t "~4D - " i) (write-line (car (midi-string-ev e))))))



;;===========================================================================================
;;
;; FONCTIONS ENTREE-SORTIE
;;
;;===========================================================================================

;--------------------------------------------------------------------------------------------
; WRITE-TRACK-AUX
; Sauvegarde une track au format MidiFile

(defun write-track-aux (self name)
  (let ((mf (MidiFileCreate1 name clce-MidiFileFormat0 TicksPerQuarterNote 500))
        (port 0))
    (MidiFileNewTrack mf)
    (MidiFileWriteEv mf (time-sign :tsnum 4 :tsdenom 2 :tsclick 24 :tsquarter 8))
    (MidiFileWriteEv mf (tempo-change :tempo 500000))
    (dotrack (i e self)
      (unless (= (port e) port) 
        (setq port (port e))
        (MidiFileWriteEv mf (port-prefix :prefix port :date (date e))))
      (MidiFileWriteEv mf e))
    (MidiFileWriteEv mf (end-track :date (track-size self)))
    (MidiFileCloseTrack mf)
    (MidiFileClose mf)))

;--------------------------------------------------------------------------------------------
; READ-TRACK-AUX
; Charge une track au format MidiFile
; modifications pour convertir les keyon keyoff en notes

(defun read-track-aux (name convert)
  (let ((self (new-temp-track))
        (mf (MidiFileOpen1 name clce-MidiFileReadMode))
        (port 0))
    (if convert
      (loop
        (if (eq (MidiFileOpenTrack mf) 0) (return self))
        (do ((e (MidiFileReadEv mf) (MidiFileReadEv mf))
             (lk nil))
            ((nullptrp e) (MidiFileCloseTrack mf) (force-pending-key lk (track-size self)))
          (port e port)
          (cond ((= typeEndTrack (evtype e))
                 (track-size self (date e))(midi-free-ev e))
                
                ((= typePortPrefix (evtype e))
                 (setq port (prefix e)))
                
                ((and (= typeKeyOn (evtype e)) (> (vel e) 0))
                 (setq lk (add-pending-key e lk))
                 (add-track self e))
                
                ((or (and (= typeKeyOn (evtype e)) (= (vel e) 0))
                     (= typeKeyOff (evtype e))) 
                 (setq lk (rem-pending-key e lk))
                 (midi-free-ev e))
                
                (t
                 (add-track self e)))))
      (loop
        (if (eq (MidiFileOpenTrack mf) 0) (return self))
        (do ((e (MidiFileReadEv mf) (MidiFileReadEv mf)))
            ((nullptrp e) (MidiFileCloseTrack mf))
          (port e port)
          (cond ((= typePortPrefix (evtype e))
                 (setq port (prefix e)))
                
                ((= typeEndTrack (evtype e))
                 (progn (track-size self (date e)) (midi-free-ev e)))
                
                (t
                 (add-track self e))))))
    self))



;;===========================================================================================
;;
;; INSTALLATION DU MODULE
;;
;;===========================================================================================

(defun install-track-tools ()
  (setq *temp-tracks* nil)
  (setq *perm-tracks* nil))

(defun remove-track-tools ()
  (mapc #'(lambda (tr) (trkfree tr)) *temp-tracks*)
  (mapc #'(lambda (x) (trkfree (car x))) *perm-tracks*)
  (setq *temp-tracks* nil)
  (setq *perm-tracks* nil))

(progn
  (midi-add-ouverture-action #'install-track-tools)
  (midi-add-fermeture-action #'remove-track-tools))
