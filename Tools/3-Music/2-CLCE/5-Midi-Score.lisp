;;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;;
;;
;;                                  Midi-Score.lisp
;;
;;                                     © 1991, GRAME.
;;
;;
;;
;;
;;
;;  Fichier definissant les objets score, des sequences a la MidiLogo.
;;
;;  CONVENTION D'ECRITURE :
;;  De même que dans MidiShare toutes les fonctions ont un nom de la forme "midixxyy",
;;  toutes les fonctions definis ici auront un nom de la forme "midi-xx-yy".
;;
;; HISTORIQUE :
;;  01-09-91, Premiere version. -Yo-
;;  05-09-91, ajout de  midi-search-rule  -SL-
;;  06-09-91, modification de basic-midi-search-rule pour le temps reel
;;  09-09-91, methode midi-add-ev -ß-
;;  10-09-91, rapatriement de quelques restes de Parts.lisp -ß-
;;              des champs en plus pour midi-score
;;              fct refresh
;;  12-09-91, appel de la methode refresh-midi-window et wrefresh
;;  16-09-91, methode midi-set-file pour sauver un pathname
;;  19-09-91 methode de synthese des regles :midi-code-rule,midi-make-rule
;;  24-09-91 methode midi-do-score : ajout d'evenements differents
;;  27-09-91 methode midi-note-to-keyon :transforme les notes en couples keyon-keyoff
;;           (pour la synthese de regles en temps reel)
;;           mot clef real-time dans midi-code-rule et midi-make-rule
;;           mot clef :date au lieu de :at dans midi-add-ev
;;  7-10-91 correction midi-code-rule et midi-search-rule (suite remarques R-Pascal)
;;  13-10-91 correction bug midi-search-lastev
;;  07-11-91  status devient un slot de actor, midi-get-status definie dans midi-actor.lisp
;; CLCE IV
;;  08-11-91 SSelCopy et SSelCut prennent 1 param en plus : len
;;           Scopy, SCut remplacent SSelCopy et SSelCut
;;           nouveau Midi-clear avec date pos dur len sel dst
;;           midi-fw-pos et bk-pos sont merges avec midi-set-pos
;;           midi-bk-read et midi-fw-read sont merges dans midi-get-ev
;;           midi-count-evs remplace midi-get-size (clce3.lisp)
;;           midi-send remplace midi-send-at et midi-send-im
;;           midi-clear remplace midi-flush-evs
;; fcts C    SSetRelPos  remplace SForwardPos  et SBackwardPos
;;           SSetRelDate remplace SForwardDate et SBackwardDate
;; 17/12/91  SGoBeforePos prend un parametre sel en plus
;; 30/12/91  Nouveau Midi-move
;;           SSetRelPos & SSetRelDate supprimes  (remplace par midi-move)
;; 10/01/92  Midi-add-ev est remplace par Midi-write-ev, retrait
;;
;; MIDIFILES
;; 15-01-92  ajout d'un champ format dans les scores et de quoi lire et ecrire les midifiles
;; 20/02/92  Midi-add-ev retrait de la declaration du export (oubliee)
;; 24-02-92  Midi-search-rule rend une taille de segment reconnu et non plus une duree -SL-
;; 27-02-92  fonctions sur evenemnts : (date :cur) remplace par date
;; 05-03-92  move-to remplace par clce-move-to (conflit avec QuickDraw)
;;           cat-scores appelle cat (midi-actor)
;; 07/04/92  nouvelle interface C des MidiFiles : ajout du record MidiFileInfos dans
;;           la classe midi-score -DF-
;; 09/04/92  interface C pour l'ecriture et la lecture des fichiers au format MidiShare. -DF-
;; 14-05-92  adaptation Make-key pour les evenements Key-off en plus des Key-on a velocite 0 -SL-
;; 17/05/92  PRINT a la place de EDIT dans les infos-box
;; 30/06/92
;;  ----------------------------------------------------------------------
;;  05-12-94, Adaptation au nouveau mode de chargement, suppression du
;;	      package MidiShare, simplification.
;;  15-01-99  Correction bug chargement librairie MidiFile
;;  13-12-00  Ajout des fonctions seqscore2 et score2seq
;;  14-06-00  Suppression des parties liees au mac, creation d'un fichier Midi-Score-Interface.lisp
;;  25-06-01  Macptr remplace par t pour faciliter le portage Linux
;;  16-07-01  without remplace par une macro multi-platforme : disable-interrupts
;;  07-07-04  Renommage pour eviter des conflits avec Player-Interface.lisp. Correction bug dans midi-move lorsque la score est vide
;;  21-10-09  midi-transform reecrite (ne fait plus appel au code C)
;;
;;
;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

;;                              Classe des score MidiShare
;;========================================================================================

;;...................................................................: variables globales
(defvar *midi-default-master* -1)
(defvar *midi-score-count* 0)
(defvar *midi-score-list* nil)

;;...................................................................: midi-score
(defclass midi-score (midi-actor)
  (
   (master      :initform clce :initarg :master)
   (file-name   :initform :ask :initarg :file)
   (file-format :initform :midishare :initarg :format)  ;; formats possibles ":midishare" ou ":midifile"
   (file-infos  :initform (make-midifile-infos))
   (score       :initform (SNew))
   (play-dst-list :initform nil)
   (play-task   :initform nil)
   (play-offset :initform 0)
   (play-limit  :initform 0)
   (rec-src-list :initform nil)
   (rec-task    :initform nil)
   (rec-offset  :initform 0)
   (rec-limit   :initform 0)
   (rec-sync    :initform :full)
   (rec-key-off :initform :yes)
   (savflag     :documentation "modification flag of the score : t = not saved"
                :initform t)
   ))

;;                                 Creation et destruction
;;========================================================================================


;;...................................................................: initialize-instance
(defmethod initialize-instance :after ((self midi-score) &rest initargs)
  (declare (ignore initargs))
  (clce-mf-format (my file-infos) 1)
  (clce-mf-timedef (my file-infos) 0)
  (clce-mf-clicks (my file-infos) 500)
  (midi-check-ptr (my score))
  (incf *midi-score-count*)
  (pushnew self *midi-score-list*))

;;...................................................................: update-instance-for-different-class
(defmethod update-instance-for-different-class :before ((previous midi-score) (self dead) &rest initargs)
  (declare (ignore initargs))
  (decf *midi-score-count*)
  (setq *midi-score-list* (delete self *midi-score-list* :count 1))
  (sfree (slot-value previous 'score))
  (free-midifile-infos (slot-value previous 'file-infos)))


;;                                 Acces aux informations
;;========================================================================================

;;...................................................................: midi-set-name
(defmethod midi-set-name ((self midi-score) (name string))
  (my name name))

;;...................................................................: midi-get-file
(defmethod midi-get-file ((self midi-score))
  (my file-name))

;;...................................................................: midi-set-file
(defmethod midi-set-file ((self midi-score) (name string))
  (my file-name name))

;;...................................................................: midi-set-file
(defmethod midi-set-file ((self midi-score) (name pathname))
  (my file-name name))

;;...................................................................: midi-get-format
(defmethod midi-get-format ((self midi-score))
  (my file-format))

;;...................................................................: midi-set-format
(defmethod midi-set-format ((self midi-score) (format t))
  (my file-format format))

;;...................................................................: midi-get-midifile-format
(defmethod midi-get-midifile-format ((self midi-score))
  (clce-mf-format (my file-infos)))

;;...................................................................: midi-set-midifile-format
(defmethod midi-set-midifile-format ((self midi-score) format)
  (clce-mf-format (my file-infos) format))

;;...................................................................: midi-get-time-def
(defmethod midi-get-time-def ((self midi-score))
  (clce-mf-timedef (my file-infos)))

;;...................................................................: midi-set-time-def
(defmethod midi-set-time-def ((self midi-score) def)
  (clce-mf-timedef (my file-infos)  def))

;;...................................................................: midi-get-cliks
(defmethod midi-get-clicks ((self midi-score))
  (clce-mf-clicks (my file-infos) ))

;;...................................................................: midi-set-cliks
(defmethod midi-set-clicks ((self midi-score) n)
  (clce-mf-clicks (my file-infos)  n))


;;                       Utilitaires et Predicats
;;========================================================================================

;;...................................................................: midi-end-p
(defmethod midi-end-p ((self midi-score))
  (nullptrp (snextev (my score))))


;;...................................................................: midi-begin-p
(defmethod midi-begin-p ((self midi-score))
  (nullptrp (sprevev (my score))))


;;...................................................................: midi-empty-p
(defmethod midi-empty-p ((self midi-score))
  (and (nullptrp (sprevev (my score)))
       (nullptrp (snextev (my score)))))


;;...................................................................: midi-avail-ev
(defmethod midi-avail-ev ((self midi-score))
  (snextev (my score)))

;;...................................................................: midi-count-evs
(defmethod midi-count-evs ((self midi-score) &key (sel nil) (date 0) pos
                (dur most-positive-fixnum) (len most-positive-fixnum))
  "Calcul du nb d'evts du type donne a partir de la date courante"
  ; s'il y a une date ou pos, on y va. Sinon on reste a la date courante
  ; les primitives en C ne vont qu'en marche avant
  (let ((s (my score)) olddate oldpos (size 0) (*selection* sel))
    (declare (special *selection*))

    ; la fonction ne doit pas provoquer de deplacement
    (setq olddate (scurdate s))
    (setq oldpos  (snextpos s))

    ; on fixe la date et la position de travail
    (multiple-value-setq (date pos) (midi-move self :date date :pos  pos))

    ; gestion de la marche arriere (dur<0 ou len<0)
    (when (< dur 0)
      (let ((tempdate (if date date olddate)))
        (setq date (max (+ tempdate dur) 0))
        (setq dur (- tempdate date))))
    (when (< len 0)
      (let ((temppos (if pos pos oldpos)))
        (setq pos  (max (+ temppos len) 0))
        (setq len (- temppos pos))))

    (midi-move self :date date :pos  pos)         ; en cas de marche arriere

    (if sel
      (setq size (ssize s c-select dur len))
      (setq size (ssize s (nullptr) dur len)))

    (midi-move self :date olddate :pos  oldpos)   ; on remet la date courante

    size
))

;;...................................................................: midi-get-duration
(defmethod midi-get-duration ((self midi-score) &key (sel nil) (date 0) pos (full nil)
                              (dur most-positive-fixnum) (len most-positive-fixnum))
  ; les primitives en C ne vont qu'en marche avant
  (let ((s (my score)) olddate oldpos (*selection* sel) (d 0))
    (declare (special *selection*))

    ; la fonction ne doit pas provoquer de deplacement
    (setq olddate (scurdate s))
    (setq oldpos  (snextpos s))

    ; on fixe la date et la position de travail
    (multiple-value-setq (date pos) (midi-move self :date date :pos  pos))

    ; gestion de la marche arriere (dur<0 ou len<0)
    (when (< dur 0)
      (setq olddate (scurdate s))
      (setq date (- (if date date (scurdate s)) dur))
      (setq dur (- dur)))
    (when (< len 0)
      (setq oldpos (snextpos s))
      (setq pos (- (if pos pos (snextpos s)) len))
      (setq len (- len)))

    (midi-move self :date date :pos  pos)         ; en cas de marche arriere

    (setq d (sduration s c-select dur len (if full 1 0)))

    (midi-move self :date olddate :pos  oldpos)   ; on remet la date courante

d))


;; Positionnement et Deplacements absolus (date pos) ou relatifs (dur len)
;;========================================================================================
;;...................................................................: midi-get-pos
(defmethod midi-get-pos ((self midi-score))
  (snextpos (my score)))

;;...................................................................: midi-get-date
(defmethod midi-get-date ((self midi-score))
  (scurdate (my score)))

;; midi-move remplace midi-set-date et midi-set-pos
;;...................................................................: midi-move
(defmethod midi-move ((self midi-score) &key (sel nil) date dur pos len (refresh t))
  (declare (ignore refresh))
  (let ((sc (my score)) (*selection* sel))
      (declare (special *selection*))

      (if (equal date :cur)  (setq date (scurdate sc)))   ; :cur

      ; tous les cas de 3 ou 4 mot-cles sont ramenes a 2 mots-cles
      ; (date dur pos len) (date dur *) (pos len *)

      (when (and date dur)
        (setq date (min (+ date dur) most-positive-fixnum))
        (setq dur nil))
      (when (and pos len)
        (setq pos (min (+ pos len) most-positive-fixnum))
        (setq len nil)
        (if dur (setq dur nil)))
      (when (and dur (null date) (or len pos))
        (setq date (min (+ (scurdate sc) dur) most-positive-fixnum))
        (setq dur nil))

      ; (write-line (format nil "dat~5D  dur~5D  pos~5D  len~5D" date dur pos len))

      (if sel

        (cond               ; if sel
         ((and date pos)
          (sgobegindate sc date)
          (sgobeforepos sc (nullptr) 1)
          (sgobeforepos sc c-select pos)
          (if (and (not (nullptrp (sprevev sc))) (not (nullptrp (snextev sc)))
                   (> date (date (sprevev sc))) (<= date (date (snextev sc))))
            (sgobegindate sc date)))      ; alors on peut repositionner la date
         ((and date len)
          (sgobegindate sc date)
          (smoveatdate sc c-select len))
         ; (and dur len)
         ; (dur pos) = pos
         ; (date dur) = date
         ; (pos len) = pos
         (date           (sgobegindate sc date)
                         (smoveatdate sc c-select 1))
         (pos            (sgobeforepos sc (nullptr) 1)
                         (sgobeforepos sc c-select pos))
         (dur            (sgobegindate sc (+ (scurdate sc) dur))
                         (smoveatdate sc c-select 1))
         ; si on arrive ici, alors : date = pos = dur = NIL
         ((null len)     (sgobeforepos sc c-select 1))
         ((> len 0)      (sgobeforepos sc c-select (+ len 1)))
         ((<= len 0)     (sgobeforepos sc c-select len)))

        (cond               ; if not sel
         ((and date pos)
          (sgobegindate sc date)
          (sgobeforepos sc (nullptr) pos)
          (if (and (not (nullptrp (sprevev sc))) (not (nullptrp (snextev sc)))
                   (> date (date (sprevev sc))) (<= date (date (snextev sc))))
            (sgobegindate sc date)))      ; alors on peut repositionner la date
         ((and date len)
          (sgobegindate sc date)
          (if (> len 0) (smoveatdate sc (nullptr) len)))
         ;   (when (and (= date (date (snextev sc))) (> len 0))
         ;     (do ((e (sforwardread sc) (sforwardread sc)))
         ;          ((or (<= len 1) (> (date (link (snextev sc))) date) (nullptrp e)))
         ;       (decf len))))
         ; (and dur len)
         ; (dur pos) = pos
         ; (date dur) = date
         ; (pos len) = pos
         (date           (sgobegindate sc date))
         (pos            (sgobeforepos sc (nullptr) pos))
         (dur            (sgobegindate sc (+ (scurdate sc) dur)))
         (len            (sgobeforepos sc (nullptr) (+ (snextpos sc) len)))))

      (values (scurdate sc) (snextpos sc))))

;;                                      Impression
;;========================================================================================

;;...................................................................: print-object
(defmethod print-object ((self midi-score) (out stream))
  (format out
       "<SCORE~2A~A ~15A  pos:~3A  date: ~5A size: ~3A dur: ~5Ds status: ~D>"
        (my order)
        (if (my savflag) "†" " ")
        (midi-get-name self)
        (midi-get-pos self)
        (midi-get-date self)
        (midi-count-evs self :date 0)
        (float (/ (round (/ (midi-get-duration self :date 0 :full t) 100)) 10))
        (midi-get-status self)))
;;...................................................................: midi-print
(defmethod midi-print ((self midi-score) &key (sel nil) (date 0) pos
                (dur most-positive-fixnum) (len most-positive-fixnum))
  "Imprime les evts du type donne a partir de la date specifiee (courante)"
  (print self)
  (when (eq :idle (my status))
    (write-line "")
    (write-line "----------------------------------------------------------------")

    ; les primitives en C ne vont qu'en marche avant
    (let ((sc (my score)) olddate oldpos (*selection* sel) (offend 0))
      (declare (special *selection*))

      ; la fonction ne doit pas provoquer de deplacement
      (setq olddate (scurdate sc))
      (setq oldpos  (snextpos sc))

      ; on fixe la date et la position de travail
      (multiple-value-setq (date pos) (midi-move self :date date :pos  pos))

      ; gestion de la marche arriere (dur<0 ou len<0) => positionne pos & date
      (when (< dur 0)
        (let ((tempdate (if date date olddate)))
          (setq date (max (+ tempdate dur) 0))
          (setq dur (+ (- tempdate date) 1))
          (multiple-value-setq (date pos) (midi-move self :date date))))
      (when (and sel (< len 0))
        (setq offend (- (+ (ssize sc c-select 1 most-positive-fixnum) 1)))
        (multiple-value-setq (date pos) (midi-move self :len len :sel sel))
        (setq len (- (+ offend len -1))))
      (when (< len 0)
        (setq offend (ssize sc c-select 1 most-positive-fixnum))
        (let ((temppos (+ (if pos pos oldpos) offend)))
          (setq pos  (max (+ temppos len -2) 0))
          (setq len (- temppos pos))))

      (if sel (setq pos nil))

    ; (write-line (format nil "dat~5D  dur~5D  pos~5D  len~5D" date dur pos len))

      (let ((p (- (midi-get-pos self) offend)))
        (midi-transform self
                        #'(lambda (e)
                              (write-line (format nil
                                                  "~5D - ~A"
                                                  p
                                                  (midi-string-ev e)))
                            (incf p))
                        :date date :pos pos :dur dur :len len :sel sel))

    (midi-move self :date olddate :pos  oldpos)   ; on remet la date courante
    (write-line "----------------------------------------------------------------")))
t)

;;                                      Macros utiles
;;========================================================================================

(defmacro with-temp-score (vl &body actions)
  (let ((l1 (mapcar #'(lambda (v) `(,v (new 'midi-score))) vl))
        (l2 (mapcar #'(lambda (v) `(free ,v)) vl)))
    `(let ,l1 ,@actions ,@l2)))

;;                                Ecriture des fichiers midi
;;========================================================================================

;;...................................................................: variables globales
(defvar *midi-scrap*)
(defvar *midi-trash*)

;;...................................................................: midi-interface-error
(defun midi-interface-error (c)
  (cond ((= c 1)
          (prin1 "Error : opening file"))
         ((= c 2)
          (prin1 "Error : reading file"))
         ((= c 3)
          (prin1 "Error : writing file"))
         ((or (= c 4) (= c 5) (= c 6))
          (prin1 "Error : setting file's info"))
         ((= c 7)
          (prin1 "Error : unknown MidiFile format"))
         ( t (prin1 (format nil "Unknown error : code ~D" c))))
  nil)
;;...................................................................: midishare-error
(defun midishare-error (c)
   (cond ((= c -1)
          (prin1 "Error : no more MidiShare events"))
         ((= c -2)
          (prin1 "Error : unknown event"))
         ((= c -3)
          (prin1 "Error : C function has got a nil SeqPtr"))
         ( t (midi-interface-error c)))
   nil)
;;...................................................................: midifile-error
(defun midifile-error (c)
  (cond ((= c -1)
         (prin1 "Error : bad format file"))
        ((= c -2)
         (prin1 "Error : no more MidiShare events"))
        ((= c -6)
         (prin1 "Error : unknown event"))
        ((= c -7)
         (prin1 "Error : trying to write a reset event"))
        ((= c -8)
         (prin1 "Error : track closed"))
        ( t (midi-interface-error c)))
  nil)
;;...................................................................: midishare-save
(defun midishare-save (self name)
   (let ((ret (midishare-c-save name (my score))))
    (if (not (= ret 0)) (midishare-error ret)
        t)))

;;...................................................................: midifile-save
(defun midifile-save (self name)
    (print name)
  (let ((ret (midifile-c-save name (my score) (my file-infos))))
    (if (not (= ret 0)) (midifile-error ret)
        t)))

;;                            lecture des fichiers midi
;;========================================================================================

;;...................................................................: midifile-load
(defun midifile-load (self name)
  (midi-clear self)
  (let ((ret (midifile-c-load name (my score) (my file-infos))))
    (midi-set-format self :midifile)
    (if (= ret 0) t
        (midifile-error ret))))

;;...................................................................: midishare-load
(defun midishare-load (self name)
  (midi-clear self)
  (let ((ret (midishare-c-load name (my score))))
    (midi-set-format self :midishare)
    (if (= ret 0) t
        (midishare-error ret))))

;;  Lecture, extraction ou suppression d'evenement
;;========================================================================================

;;...................................................................: midi-get-ev
(defmethod midi-read-ev ((self midi-score) &key (sel nil) date pos len dur)
  (midi-move self :date date :pos pos :dur dur :len len :sel sel)
  (snextev (my score)))

; double definition : midi-get-ev = midi-rem-ev
;;...................................................................: midi-rem-ev
(defmethod midi-rem-ev ((self midi-score) &key (sel nil) date pos len dur)
  (midi-move self :date date :pos pos :dur dur :len len :sel sel :refresh nil)
  (sremoveev (my score)))

;;...................................................................: midi-get-ev
(defmethod midi-get-ev ((self midi-score) &key (sel nil) date pos len dur)
  (midi-move self :date date :pos pos :dur dur :len len :sel sel :refresh nil)
  (sremoveev (my score)))

;;    Remplissage des scores, ecriture, envoi d'evenement
;;========================================================================================

; pour ecrire un evenement ou n evenements
;...................................................................: midi-write-ev
(defmethod midi-write-ev ((self midi-score) (event t) &key (sel nil) date pos
                          (dur most-positive-fixnum) (len 1) (step 125))
  (setq date (midi-move self :date date :pos pos :sel sel :refresh nil))
  (let ((s (my score)) )
    (dotimes (i len)
      (when (<= (* i step) dur)
        (sgoenddate s (+ (* i step) date))
        (sforwardwrite s (midi-copy-ev event))))
    (midi-free-ev event)
    ))
;;...................................................................: midi-do-score
(defmethod midi-do-score ((self midi-score) &rest arg)
  (let ((date 0) (s1 (my score)))
    (mapc #'(lambda (s)
              (if (numberp s)
                (setq date s)
                (progn
                  (sgoenddate s1 date)
                  (sforwardwrite s1  s))))
          arg))
  self)

;;                   Gestion des transmissions et receptions d'evenements
;;========================================================================================
;; idem midi-write-ev

;;...................................................................: midi-send-im
(defmethod midi-send-im ((self midi-score) (e t))
  (sforwardwrite (my score) e))

;;...................................................................: midi-send
(defmethod midi-send ((self midi-score) (e t))
  (let ((s (my score)))
    (sgoenddate s (date e))
    (sforwardwrite s e)))

;;...................................................................: midi-send-at
(defmethod midi-send-at ((self midi-score) (e t) (date integer))
  (let ((s (my score)))
    (sgoenddate s date)
    (sforwardwrite s e)))

;;                                          Montage
;;========================================================================================

(defmethod midi-clear ((self midi-score) &key (sel nil) (date 0) pos (dur most-positive-fixnum)
                       (len most-positive-fixnum) (dst *midi-trash*))
  (midi-move self :date date :pos pos :sel sel :refresh nil)
  (if sel
    (let ((*selection* sel))
      (declare (special *selection*))
      (scut (my score) (slot-value dst 'score) c-select dur len))
    ; if not sel
    (scut (my score) (slot-value dst 'score) (nullptr) dur len))
  )

;;...................................................................: midi-cut
(defmethod midi-cut ((self midi-score) &key (sel nil) (date 0) pos (dur most-positive-fixnum)
                     (len most-positive-fixnum) (dst *midi-scrap*))
  (midi-move self :date date :pos pos :sel sel :refresh nil)
  (if sel
    (let ((*selection* sel))
      (declare (special *selection*))
      (scut (my score) (slot-value dst 'score) c-select dur len))
    ; if not sel
    (scut (my score) (slot-value dst 'score) (nullptr) dur len))
  )

;;...................................................................: midi-copy
(defmethod midi-copy ((self midi-score) &key (sel nil) (date 0) pos (dur most-positive-fixnum)
                      (len most-positive-fixnum) (dst *midi-scrap*))
  (midi-move self :date date :pos pos :sel sel :refresh nil)
  (if sel
    (let ((*selection* sel))
      (declare (special *selection*))
      (scopy (my score) (slot-value dst 'score) c-select dur len))
    ; if not sel
    (scopy (my score) (slot-value dst 'score) (nullptr) dur len))
  )

;;...................................................................: midi-splice
(defmethod midi-splice ((self midi-score) &key (sel nil) (date 0) pos dur
                        (len most-positive-fixnum) (dst *midi-scrap*))
  (multiple-value-setq (date pos) (midi-move self :date date :pos pos :sel sel :refresh nil))
  (when sel                                 ; il faut rendre contigu
    (setq dur (- (midi-move self :len len :dur dur :sel sel) date))
    (midi-move self :date date :pos pos)    ; pas de sel, car on connait la pos exacte
    (setq len nil))
  (ssplice (my score) (slot-value dst 'score)
             (if dur dur most-positive-fixnum) (if len len most-positive-fixnum))
  )

;;...................................................................: midi-mix
(defmethod midi-mix ((self midi-score) &key (sel nil) (date 0) pos (dur most-positive-fixnum)
                     (len most-positive-fixnum) (src *midi-scrap*) change)
  (declare (special *transformation*))
  (setq *transformation* (if (functionp change) change nil))
  (midi-move self :date date :pos pos :sel sel :refresh nil)
  (if *transformation*
    (if sel
      (let ((*selection* sel))
        (declare (special *selection*))
        (smix (my score) (slot-value src 'score) c-select c-transform dur len))
      ; if not sel
      (smix (my score) (slot-value src 'score) (nullptr) c-transform dur len))
    (if sel
      (let ((*selection* sel))
        (declare (special *selection*))
        (smix (my score) (slot-value src 'score) c-select (nullptr) dur len))
      ; if not sel
      (smix (my score) (slot-value src 'score) (nullptr) (nullptr) dur len)))
  )

;;...................................................................: midi-insert
(defmethod midi-insert ((self midi-score) &key (sel nil) (date 0) pos dur
                        (len most-positive-fixnum) (src *midi-scrap*) change)
  (declare (special *transformation*))
  (setq *transformation* (if (functionp change) change nil))
  (midi-move self :date date :pos pos :sel sel :refresh nil)
  (if (null dur) (setq dur (midi-get-duration src :full t)))
  (if *transformation*
    (if sel
      (let ((*selection* sel))
        (declare (special *selection*))
        (sinsert (my score) (slot-value src 'score) c-select c-transform dur len))
      ; if not sel
      (sinsert (my score) (slot-value src 'score) (nullptr) c-transform dur len))
    (if sel
      (let ((*selection* sel))
        (declare (special *selection*))
        (sinsert (my score) (slot-value src 'score) c-select (nullptr) dur len))
      ; if not sel
      (sinsert (my score) (slot-value src 'score) (nullptr) (nullptr) dur len)))
  )

;;...................................................................: midi-insert-rest
(defmethod midi-insert-rest ((self midi-score) &key (sel nil) (dur 0) date pos)
  (midi-move self :date date :pos pos :sel sel :refresh nil)
  (cond ((plusp dur)
           (sinsert (my score) (slot-value *midi-scrap* 'score) (nullptr) (nullptr) dur most-positive-fixnum))
        ((minusp dur)
           (ssplice (my score) (slot-value *midi-scrap* 'score) (- dur) most-positive-fixnum)))
  )

;;                                  Transformations globales
;;========================================================================================

;;...................................................................: midi-sort
(defmethod midi-sort ((self midi-score))
  (ssort (my score)))


;;...................................................................: midi-sort
(defmethod midi-match-key-off ((self midi-score))
  (smatchkeyoff (my score)))

;;                                  Transformations musicales
;;========================================================================================

;;...................................................................: midi-transform
;;(defmethod midi-transform ((self midi-score) (*transformation* function) &key (sel nil) pos
;;                           (date 0) (dur most-positive-fixnum) (len most-positive-fixnum))
;;  (declare (special *transformation*))
;;  (midi-move self :date date :pos pos :sel sel :refresh nil)
;;  (if sel
;;    (let ((*selection* sel))
;;      (declare (special *selection*))
;;      (stransform (my score) c-transform c-select dur len))
;;    ; if not sel
;;    (stransform (my score) c-transform (nullptr) dur len))
;;  )

;; 21/10/09 : SL rewritten
;; 17/03/10 : revision DLO pour integrer :len
(defmethod midi-transform ((self midi-score) (fun function) &key (sel nil) pos
                           (date 0) (dur most-positive-fixnum) (len most-positive-fixnum))
  (midi-move self :date date :pos pos :sel sel :refresh nil)
  (let ((s (my score))
        (end (+ date dur)))
    (if sel
        (do ((e (snextev s) (snextev s)))
            ((or (nullptrp e) (> (date e) end) (zerop len)))
          (when (funcall sel e)
            (funcall fun e)
            (decf len))
          (sgobeforepos s (nullptr) (+ (snextpos s) 1)))
      (do ((e (snextev s) (snextev s)))
          ((or (nullptrp e) (> (date e) end) (zerop len)))
        (funcall fun e)
        (decf len)
        (sgobeforepos s (nullptr) (+ (snextpos s) 1))))))

;;                                      play et record
;;========================================================================================

;;...................................................................: variables globales
(defvar *advance*)
(defvar *slice*)

;;...................................................................: play-slice
(defun play-slice (date master self len *selection*)
  (my play-task nil)
  (when (eq :playing (my status))
    (let ((s (my score))
          (dmax (+ *slice* date *advance*)))
      (do ((e (snextev s) (snextev s))
           (d))
          ((nullptrp e) (midi-stop self))           ; condition d'arret / resultat

        (setq d (+ (my play-offset) (date e)))
        (when (or (>= d (my play-limit)) (<= len 0))
            (midi-stop self))
        (when (>= d dmax)
          (my play-task (midi-task #'play-slice (- d *advance*) master self len *selection*))
          (return))
        (when (or (null *selection*) (funcall *selection* e))
          (decf len)
          (dolist (dst (my destinations))
            (midi-send-at dst (midi-copy-ev e) d)))
      ;      (midi-send dst (midi-copy-ev e))))
        (sgobeforepos s (nullptr) (+ (snextpos s) 1))))))

;;...................................................................: midi-play
(defmethod midi-play ((self midi-score) &key dst dur (date 0) pos (len most-positive-fixnum) sel)
  (disable-interrupts
   (midi-move self :date date :pos pos :sel sel :refresh nil)
   (let ((d (midi-get-time)))
     (when (eq :idle (my status))
       (cond ((null dst) (my play-dst-list (list (my master))))
             ((atom dst) (my play-dst-list (list dst)))
             (t (my play-dst-list dst)))
       (my status :playing)
       (my play-offset (- d (scurdate (my score))))
       (my play-limit (if dur (+ d dur) most-positive-fixnum))
       (mapc #'(lambda (d) (midi-connect self d t)) (my play-dst-list))
       (play-slice (- d *advance*) (my master) self len sel))))
  nil)

;;...................................................................: midi-record
(defmethod midi-record ((self midi-score) &key src dur (key-off :no) (sync :full))
  (disable-interrupts
   (let ((d (midi-get-time)))
     (when (eq :idle (my status))
       (cond ((null src) (my rec-src-list (list (my master))))
             ((atom src) (my rec-src-list (list src)))
             (t (my rec-src-list src)))
       (my status :recording)
       (my rec-offset d)
       (my rec-sync sync)
       (my rec-key-off key-off)
       (sclearall (my score))
       (if dur
         (progn (my rec-limit dur)
                (my rec-task (midi-task #'(lambda (d m self)
                                            (declare (ignore d m))
                                            (my rec-task nil)
                                            (midi-stop self))
                                        (+ d dur) (my master) self)) )
         (progn (my rec-limit most-positive-fixnum)
                (my rec-task nil)))
       (mapc #'(lambda (s) (midi-connect s self t)) (my rec-src-list)))))
  self)

;;...................................................................: midi-stop
(defmethod midi-stop ((self midi-score))
  (disable-interrupts
   (cond ((eq :playing (my status))
          (mapc #'(lambda (d) (midi-connect self d nil)) (my play-dst-list))
          (my status :idle)
          (if (my play-task)
            (midi-forget-task (my play-task))
            (my play-task nil)) )
         ((eq :recording (my status))
          (mapc #'(lambda (s) (midi-connect s self nil)) (my rec-src-list))
          (my status :idle)
          (when (my rec-task)
            (midi-forget-task (my rec-task))
            (my rec-task nil))
          (when (eq :no (my rec-key-off)) (smatchkeyoff (my score)))
          (sgobegindate (my score) 0)
          (unless (nullptrp (snextev (my score)))
            (cond ((eq :full (my rec-sync))
                   (sgobegindate (my score) 0)
                   (let* ((dd (date (snextev (my score))))
                          (*transformation* #'(lambda (e) (date e (- (date e) dd)))))
                     (declare (special *transformation*))
                     (stransform (my score) c-transform (nullptr) most-positive-fixnum most-positive-fixnum)))
                  ((eq :partial (my rec-sync))
                   (let* ((dd (my rec-offset))
                          (*transformation* #'(lambda (e) (date e (- (date e) dd)))))
                     (declare (special *transformation*))
                     (stransform (my score) c-transform (nullptr) most-positive-fixnum most-positive-fixnum)))
            )))))
  self)

;;...................................................................: score2seq
(defmethod score2seq ((self midi-score))
  (let ((sco (my score))
        (seq (midi-new-seq)))
    (sgobegindate sco 0)
    (do ((ev (SForwardRead sco) (SForwardRead sco)))
        ((nullptrp ev) seq)
      (midi-add-seq seq (midi-copy-ev ev)))))

;;...................................................................: seqs2core
(defmethod seq2score ((self midi-score) seq)
  (let ((sco (my score)))
    (sgobegindate sco 0)
    (do ((ev (firstev seq) (link ev)))
        ((nullptrp ev))
      (SGoBeginDate sco (date ev))
      (SForwardWrite sco (midi-copy-ev ev)))
    (midi-free-seq seq)))

;;                                Installation et desinstallation
;;========================================================================================

;;...................................................................: midi-init-score
(defun midi-init-score ()
  (setq *advance* 500)
  (setq *slice* 250)
  (setq *midi-score-list* nil)
  (setq *midi-score-count* 0)
  (setq *midi-scrap* (new 'midi-score :name "*midi-scrap*"))
  (setq *midi-trash* (new 'midi-score :name "*midi-trash*")))

;;...................................................................: midi-free-all-score
(defun midi-free-all-score ()
  (mapc #'free (copy-list *midi-score-list*)))

;;...................................................................: lancement
(eval-when (:load-toplevel :execute)
  (midi-add-ouverture-action #'midi-init-score)
  (midi-add-fermeture-action #'midi-free-all-score)
  (midi-init-score))
