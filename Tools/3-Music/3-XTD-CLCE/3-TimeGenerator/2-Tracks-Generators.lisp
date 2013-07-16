;;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;;
;;
;;                                 	 Tracks-Generators.lisp
;;
;;                                     © 2001, GRAME.
;;
;;
;;
;;
;;
;; Interface avec les generateurs temporels
;;
;; HISTORIQUE :
;;
;;  25-06-01 Macptr remplace par t pour faciliter le portage Linux
;;  07-07-04 Utilisation du package MidiShare, evtype à la place de type.
;;  20-09-10 Add gtext-ev and gtext
;;
;;
;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/


;; ------------------------------- GNRPARAM : generateur de non registred parameters

(defun gnrparam (&key (param °60) (val °80) (chan °0) (port °0))
  #'(lambda (stime ctime etime reverse)
      (non-reg-param :param (floor §param) 
                     :val (floor §val) 
                     :chan §chan 
                     :port §port)))


;; ------------------------------- GNOTE : generateur de notes

(defun gnote (&key (pitch °60) (vel °80) (dur °125) (chan °0) (port °0))
  #'(lambda (stime ctime etime reverse)
      (note :pitch (floor §pitch) 
            :vel (floor §vel) 
            :dur (floor §dur) 
            :chan §chan 
            :port §port)))

(defun gchord (&key (pitch °60) (vel °80) (dur °125) (chan °0) (port °0) (chord °(0)))
  #'(lambda (stime ctime etime reverse)
      (let ((pitch (floor §pitch))
            (vel (floor §vel))
            (dur (floor §dur))
            (chan §chan)
            (port §port)
            (chord §chord))
        (mapcar #'(lambda (x) (note :pitch (floor (+ pitch x)) :vel vel :dur dur :chan chan :port port))
                  chord))))

(defun gvol (&key (chan °0) (port °0) (vols °(0)))
  #'(lambda (stime ctime etime reverse)
      (let ((vols §vols)
            (chan (- §chan 1))
            (port §port))
        (mapcar #'(lambda (v) (ctrl-change  :ctrl 7 :value (floor v) :chan (incf chan) :port port))
                  vols))))

(defun gtext-ev (&key (text °"") (chan °0) (port °0))
  #'(lambda (stime ctime etime reverse)
      (let ((text§text)
            (chan §chan)
            (port §port))
        (text-event :text text :chan chan :port port))))

;--------------------------------------------------------------------------------------------
; GEN-TRACK
; Collecte en une track les evenements issu de generateurs
    
(defun gen-track (&key (gdate °0) (gdur °1000) (gstep °125) (gevent (gnote)) (gliaison °nil))
  (labels ((recws2 (self ev date dur liaison)
             (cond ((typep ev 'cons) 
                    (recws2 self (car ev) date dur liaison) 
                    (recws2 self (cdr ev) date dur liaison))
                   (t 
                    (when (and liaison (= typeNote (evtype ev))) 
                      (dur ev (floor (* dur liaison))))
                    (add-track self :date date ev)))))
    (let*  ((st 0) (ct 0) (et 1) (reverse nil) (delta) (self (new-temp-track)))
      (do* ((stime (funcall gdate st ct et reverse) st)
            (etime (+ stime (funcall gdur st ct et reverse)) et)
            (ctime stime ct))
           ((>= ctime etime) (track-size self etime) self)
        (setq delta §gstep)
        (recws2 self §gevent (floor ctime) (- (floor delta) 1) §gliaison)
        (setq st §gdate)
        (setq et (+ st §gdur))
        (setq ct (+ ctime delta))) )))   

;--------------------------------------------------------------------------------------------
; GN : Genere Note
; genere une track contenant un simple accord de notes

(defun gn (&key (pitch °60) (vel °80) (dur °125) (chan °0) (port °0) (liaison °0.95))
  (labels ( (enrobage (obj) (mapcar #'floor (if (listp obj) obj (list obj)))) )
    #'(lambda (stime ctime etime reverse)
        (let ((lk (enrobage §pitch))
              (lv (enrobage §vel))
              (ld (enrobage §dur))
              (lc (enrobage §chan))
              (lp (enrobage §port))
              (l §liaison)
              (dt 0)
              (dst (new-temp-track)))
          (dolist (dn ld)
            (let ((du (floor (* dn l))))
              (dolist (p lp)
                (dolist (c lc)
                  (dolist (k lk)
                    (dolist (v lv)
                      (add-track dst :date dt (note :pitch k :vel v :dur du :chan c :port p)))))))
            (incf dt dn))
          (track-size dst dt)
          dst))))

;--------------------------------------------------------------------------------------------
; GCTRL : Genere un controleur
; genere une track contenant un controleur

(defun gctrl (&key (ctrl °7) (val °64) (dur °125) (chan °0) (port °0))
  #'(lambda (stime ctime etime reverse)
      (let ((dst (new-temp-track)))
        (add-track dst :date 0 (ctrl-change  :ctrl §ctrl :value (floor §val) :chan §chan :port §port))
        (track-size dst §dur)
        dst)))

;--------------------------------------------------------------------------------------------
; GTEXT : Genere un controleur
; genere une track contenant un evenement texte

(defun gtext (&key (text °"") (dur °125) (chan °0) (port °0))
  #'(lambda (stime ctime etime reverse)
      (let ((dst (new-temp-track))
            (chan §chan)
            (port §port)
            (text §text))
        (if (listp text)
            (dolist (el text)
                 (add-track dst :date 0 (text-event :text (if (numberp el)  (write-to-string el) el) :chan chan :port port)))
            (add-track dst :date 0 (text-event  :text text :chan chan :port port)))
        (track-size dst §dur)
        dst)))

;--------------------------------------------------------------------------------------------
; CT : Collect Track
; Collecte sequentiellement en une track les n tracks issues d'un generateur
; C'est l'equivalent de (l <n> <g>)

(defun ct (n gtrk)
  (with-track-gc
    (apply #'seq-tracks (l n gtrk))))

;(defun ct (n gtrk)
;  (apply #'seq-tracks (l n gtrk)))

(defun ct2 (n1 n2 gtrk)
  (with-track-gc
    (let ((lt (l n2 gtrk)))
      (dotimes (i n1) (pop lt))
      (apply #'seq-tracks lt))))

(defun cdt (d gtrk)
  (let (lt trk)
    (with-track-gc
      (do  ((stime 0) 
            (ctime 0 (+ ctime (track-size trk)))
            (etime d)
            (reverse nil))
           ((>= ctime etime) (apply #'seq-tracks (nreverse lt)))
        (setq trk §gtrk)
        (push trk lt)))))

;--------------------------------------------------------------------------------------------
; ST : Scan Track
; Scan une track en la decoupant en tranches.  sous la form de tracks
; C'est l'equivalent de (s <g>)

(defun st (gtrack)
  (let (lt)
    #'(lambda (stime ctime etime reverse)
        (when (null lt)
          (setq lt (track-chords §gtrack)))
        (pop lt))))

;--------------------------------------------------------------------------------------------
; GEN-MODIF
; Realise une copie modifiee d'une track. Les key parametres sont des generateurs
; de valeurs. 

(defvar *cpitch* 60)
(defvar *cvel* 80)
(defvar *cdur* 250)
(defvar *cchan* 0)
(defvar *cport* 0)

;; les generateurs de "valeurs courantes"

(defun cpitch ()					;; pitch de l'evenement courant
  #'(lambda (stime ctime etime reverse)   
      (declare (ignore stime ctime etime reverse))
      *cpitch*))

(defun cvel ()						;; velocite de l'evenement courant
  #'(lambda (stime ctime etime reverse)   
      (declare (ignore stime ctime etime reverse))
      *cvel*))

(defun cdur ()						;; duree de l'evenement courant
  #'(lambda (stime ctime etime reverse)   
      (declare (ignore stime ctime etime reverse))
      *cdur*))

(defun cchan ()						;; canal de l'evenement courant
  #'(lambda (stime ctime etime reverse)   
      (declare (ignore stime ctime etime reverse))
      *cchan*))

(defun cport ()						;; port de l'evenement courant
  #'(lambda (stime ctime etime reverse)   
      (declare (ignore stime ctime etime reverse))
      *cport*))

;; la fonction de copie avec modification
(defun gen-modif (src &key gdate gchan gport gpitch gvel gdur)
  (let ((dst (new-temp-track))
        (cp)
        (stime 0)
        (etime (track-size src))
        (ctime 0)
        (reverse nil))
    (dotrack (i e src)
      (setq ctime (date e))
      (setq *cchan* (chan e))
      (setq *cport* (port e))
      (setq cp (midi-copy-ev e))
      (when gdate (date cp (floor §gdate)))
      (when gchan (chan cp (floor §gchan)))
      (when gport (port cp (floor §gport)))
      (when (<= (evtype e) typeKeyPress)
        (setq *cpitch* (pitch e))
        (setq *cvel* (vel e))
        (setq *cdur* (dur e))
        (when gpitch (pitch cp (floor §gpitch)))
        (when gvel (vel cp (floor §gvel)))
        (when gdur (dur cp (floor §gdur))))
      (add-track dst cp))
    (setq ctime etime)
    (track-size dst (if gdate (floor §gdate) etime))
    dst))
      
;; exemples :
;;
;; inversion des hauteurs et des velocites :
;;   	(gen-modif toto :gpitch (cvel) :gvel (cpitch))
;;
;; ralentissement :
;;	(gen-modif toto :gdate (mult (ctime) (i °1 °2)) :gvel (cpitch))
