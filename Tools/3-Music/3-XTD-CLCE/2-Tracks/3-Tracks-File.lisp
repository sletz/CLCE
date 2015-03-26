;;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;;
;;
;;                                 	 Tracks-File.lisp
;;
;;                                     © 2001, GRAME.
;;
;;
;;
;;
;;
;; 
;;
;; HISTORIQUE :
;;  15-06-01 Interface avec le gestionnaire de fichier du Macintosh
;;  27-06-01 Ajout du code relatif a la version linux (non terminé)
;;
;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

;;--------------------------------------------------------------------------
;; Interface for LispWorks
;;--------------------------------------------------------------------------

(progn
;--------------------------------------------------------------------------------------------
; WRITE-TRACK
; Sauvegarde une track au format MidiFile

(defun write-track (self &optional name)
  (unless name (setq name (CAPI:prompt-for-file "Nom du fichier" :filters '() :operation :save)))
  (setq name (namestring name))
  (write-track-aux self name))
 
;--------------------------------------------------------------------------------------------
; READ-TRACK
; Charge une track au format MidiFile
; modifications pour convertir les keyon keyoff en notes

(defun read-track (&optional name (convert t))
  (unless name (setq name (CAPI:prompt-for-file "Nom du fichier" :filters '("MidiFile" "*.mid"))))
  (setq name (namestring name))
   (if (probe-file name)
     (read-track-aux name convert)))

)     ;; End of LispWorks interface
