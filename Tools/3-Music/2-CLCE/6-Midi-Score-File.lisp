;;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;;
;;
;;                                 	 Midi-Score-File.lisp
;;
;;                                     � 2001, GRAME.
;;
;;
;;
;;
;;
;;
;;
;; HISTORIQUE :
;;  20-06-01 Interface avec le gestionnaire de fichier du Macintosh
;;  27-06-01 Ajout du code relatif a la version linux (non termine)
;;  09-02-05 Utilisation de set-mac-file-type pour positionner le type dans midi-save
;;  12-10-11 Correction de midi-load (si fichier déjà associé, il n'était pas rechargé)
;;  21-10-11 Correction de midi-load.
;;
;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/


;;--------------------------------------------------------------------------
;; Interface for LispWorks
;;--------------------------------------------------------------------------


(progn

;;...................................................................: midi-save
(defmethod midi-save ((self midi-score) &optional name)
  (midi-move self :date 0)
  (unless name (setq name (my file-name)))
  (cond ((eq name :ask)
         (setq name (namestring (CAPI:prompt-for-file "Nom du fichier" :filters '() :operation :save))))
        ((eq name :midifile)
         (setq name (namestring (CAPI:prompt-for-file "Nom du fichier" :filters '() :operation :save)))
         (my file-format :midifile))
        ((eq name :midishare)
         (setq name (namestring (CAPI:prompt-for-file "Nom du fichier" :filters '() :operation :save)))
         (my file-format :midishare)))
  (let ((ret
         (if (eq (my file-format) :midishare)                       ;; midifiles
           (midishare-save self (namestring name))
           (midifile-save self (namestring name)))))
    (if ret (my savflag nil))           ; c'est sauve
    (midi-set-file self name)
    ret))

;;...................................................................: midi-load
(defmethod midi-load ((self midi-score) &optional name)
  (unless name (setq name (my file-name)))
  (let (res filter)
    (cond ((eq name :ask)
           (multiple-value-setq (name res filter) (CAPI:prompt-for-file "Nom du fichier" :filters '("MidiFile" "*.mid" "MidiShare" "*.ms") :operation :open)))
          ((eq name :midishare)
           (multiple-value-setq (name res filter) (CAPI:prompt-for-file "Nom du fichier" :filters '("MidiShare" "*.ms") :operation :open)))
          ((eq name :midifile)
           (multiple-value-setq (name res filter) (CAPI:prompt-for-file "Nom du fichier" :filters '("MidiFile" "*.mid") :operation :open)))
          (t (cond ((eq (my file-format) :midishare) (setq filter "MidiShare"))
                   ((eq (my file-format) :midifile) (setq filter "MidiFile")))))
    (if (probe-file name)
        (progn
          (cond ((equal filter "MidiShare")
                 (midishare-load self (namestring name)))
                ((equal filter "MidiFile")
                 (midifile-load self (namestring name))))
          (midi-set-file self name)
          (my savflag nil))                     ; c'est sauve
    :no-such-file)))

)  ;; End of LispWorks interface



