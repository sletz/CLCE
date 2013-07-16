;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Midifile-Interface.lisp
;;
;;  Copyright (c) 1999,2000 GRAME.  All rights reserved.
;;
;;  
;;   Interface to the MidiFile library
;;
;;   15/01/99 : Version 1.0 
;;   06/12/00 : ajout des points d'entree bas niveaux : MidiFileOpen, MidiFileCreate....
;;   21/06/01 : Ajout de fonctions de creation/destruction make-midifile-infos et free-midifile-infos
;;   07/07/04 : Renommage pour eviter des conflits avec Player-Interface.lisp
;;   06/06/05 : Correction de path2unixpath
;;   29/07/08 : LispWorks version
;;   
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package :midishare)

(defvar *clce-framework* nil)
(defvar *libclce* "/System/Library/Frameworks/CLCE.framework/CLCE")
  
(defun clce-framework ()
    (or *clce-framework*
        (setq *clce-framework*
              (if (probe-file cl-user::*libclce*)
                  (progn (cffi:load-foreign-library cl-user::*libclce*)
           t)))))


;;-------------------------------------------------------------------------- 
;; MIDIfile  
;;-------------------------------------------------------------------------- 

(defparameter clce-midifile0  0)
(defparameter clce-midifile1  1)
(defparameter clce-midifile2  2)

(defparameter clce-TicksPerQuarterNote	0)
(defparameter clce-Smpte24		24)
(defparameter clce-Smpte25		25)
(defparameter clce-Smpte29		29)
(defparameter clce-Smpte30		30)

 ;;-------------------------------------------------------------------------- 
 ;; Errors  :  for MidiFile
 ;;-------------------------------------------------------------------------- 

(defparameter clce-noErr	        0)	;; no error 						 
(defparameter clce-ErrOpen	        1)	;; file open error 	 
(defparameter clce-ErrRead		2)      ;; file read error	 
(defparameter clce-ErrWrite		3)      ;; file write error	 
(defparameter clce-ErrVol	        4)	;; Volume error 	 
(defparameter clce-ErrGetInfo 		5)	;; GetFInfo error	 
(defparameter clce-ErrSetInfo		6)	;; SetFInfo error	 
(defparameter clce-ErrMidiFileFormat	7)	;; bad MidiFile format	 


;; constantes diverses

(defparameter clce-MidiFileReadMode 	0)
(defparameter clce-MidiFileAppendMode 	1)

(defparameter clce-MidiFileFormat0 	0)
(defparameter clce-MidiFileFormat1 	1)
(defparameter clce-MidiFileFormat2 	2)

(defparameter clce-TicksPerQuarterNote	0)
(defparameter clce-Smpte24		24)
(defparameter clce-Smpte25		25)
(defparameter clce-Smpte29		29)
(defparameter clce-Smpte30		30)


(progn


(cffi:define-foreign-type file-name () ':pointer)
(cffi:define-foreign-type score-ptr () ':pointer)
(cffi:define-foreign-type midifile-info-ptr () ':pointer)
(cffi:define-foreign-type midifile-ptr () ':pointer)

;; Record for MidiFile
;;================================

(cffi:defcstruct MidiFileInfos
  (format  :long)     
  (timedef :long)   
  (clicks  :long)      
  (tracks  :long))   

(defun make-midifile-infos () (cffi:foreign-alloc 'MidiFileInfos))

(defun free-midifile-infos (info) (cffi:foreign-free  info))

(defun clce-mf-format (e &optional (d nil d?))
   (if d? (setf (cffi:foreign-slot-value e 'MidiFileInfos 'format) d)
     (cffi:foreign-slot-value e 'MidiFileInfos 'format)))

(defun clce-mf-timedef (e &optional (d nil d?))
  (if d? (setf (cffi:foreign-slot-value e 'MidiFileInfos 'timedef) d)
      (cffi:foreign-slot-value e 'MidiFileInfos 'timedef)))

(defun clce-mf-clicks (e &optional (d nil d?))
  (if d? (setf (cffi:foreign-slot-value e 'MidiFileInfos 'clicks) d)
      (cffi:foreign-slot-value e 'MidiFileInfos 'clicks)))

(defun clce-mf-tracks (e)
  (cffi:foreign-slot-value e 'MidiFileInfos 'tracks))

)


(progn

(defun path2unixpath (path)
  path)

;................................................................................: MidiFilesVersion
(cffi:defcfun ("CLCE_MidiFilesVersion" midi-get-version) :short)

(defun MidiFilesVersion()
  (midi-get-version))
 
;................................................................................: MidiFileSave
(cffi:defcfun ("CLCE_MidiFileSave" clce-midi-file-save) :long (name file-name) (score score-ptr) (infos  midifile-info-ptr))

(defun clce-MidiFileSave (name score infos)
   (cffi:with-foreign-string (s name)
     (clce-midi-file-save s score infos)))
            
;................................................................................: MidiFileLoad
(cffi:defcfun ("CLCE_MidiFileLoad" clce-midi-file-load) :long (name file-name) (score score-ptr) (infos  midifile-info-ptr))

(defun clce-MidiFileLoad (name score infos)
   (cffi:with-foreign-string (s name)
     (clce-midi-file-load s score infos)))

;................................................................................: MidiFileCountTracks
(cffi:defcfun ("CLCE_MidiFileCountTracks" midi-file-count-tracks) :long (name file-name))

(defun MidiFileCountTracks (name)
   (cffi:with-foreign-string (s name)
     (midi-file-count-tracks s)))

;................................................................................: MidiShareSave
(cffi:defcfun ("CLCE_MidiShareSave" midi-share-save) :long (name file-name) (score score-ptr))

(defun clce-MidiShareSave (name score)
   (cffi:with-foreign-string (s name)
      (midi-share-save s score)))

;................................................................................: MidiShareLoad
(cffi:defcfun ("CLCE_MidiShareLoad" midi-share-load) :long (name file-name) (score score-ptr))

(defun clce-MidiShareLoad (name score)
   (cffi:with-foreign-string (s name)
     (midi-share-load s score)))

;;--------------------------------- ouverture/fermeture d'un fichier ----------------------------
(cffi:defcfun ("CLCE_MidiFileOpen" midifile-open) midifile-ptr (name file-name) (mode :long))

(defun MidiFileOpen (name mode)
   (cffi:with-foreign-string (s name)
     (midifile-open s mode)))

(defun MidiFileOpen1 (name val) (MidiFileOpen name val))

(cffi:defcfun ("CLCE_MidiFileCreate" midifile-create) midifile-ptr (name file-name) (a1 :long) (a2 :long) (a3 :long))

(defun MidiFileCreate (name a1 a2 a3)
   (cffi:with-foreign-string (s name)
     (midifile-create s a1 a2 a3)))

(defun MidiFileCreate1 (name a1 a2 a3) (MidiFileCreate name a1 a2 a3))

(cffi:defcfun ("CLCE_MidiFileClose" midifile-close) :long (file midifile-ptr))

(defun MidiFileClose (file)
  (midifile-close file))

;;------------------------------------ gestion des pistes du fichier ----------------------------
(cffi:defcfun ("CLCE_MidiFileOpenTrack" midifile-open-track) :long (file midifile-ptr))

(defun MidiFileOpenTrack (file)
 (midifile-open-track file))

(cffi:defcfun ("CLCE_MidiFileNewTrack" midifile-new-track) :long (file midifile-ptr))

(defun MidiFileNewTrack (file)
 (midifile-new-track file))

(cffi:defcfun ("CLCE_MidiFileCloseTrack" midifile-close-track) :long (file midifile-ptr))

(defun MidiFileCloseTrack (file)
 (midifile-close-track file))

(cffi:defcfun ("CLCE_MidiFileChooseTrack" midifile-choose-track) :long (file midifile-ptr) (num :long))

(defun MidiFileChooseTrack (file num)
 (midifile-choose-track file num))

;;----------------------------------------- les fonctions de lecture ----------------------------
(cffi:defcfun ("CLCE_MidiFileReadEv" midifile-read-ev) midi-ev-ptr (file midifile-ptr))

(defun MidiFileReadEv (file)
 (midifile-read-ev file))

(cffi:defcfun ("CLCE_MidiFileReadTrack" midifile-read-track) midi-seq-ptr (file midifile-ptr))

(defun MidiFileReadTrack (file)
 (midifile-read-track file))

;----------------------------------------- les fonctions d'ecriture ----------------------------
(cffi:defcfun ("CLCE_MidiFileWriteEv" midifile-write-ev) :long (file midifile-ptr) (ev midi-ev-ptr))

(defun MidiFileWriteEv (file ev)
  (midifile-write-ev file ev))

(cffi:defcfun ("CLCE_MidiFileWriteTrack" midifile-write-track) :long (file midifile-ptr) (seq midi-seq-ptr))

(defun MidiFileWriteTrack (file seq)
  (midifile-write-track file seq))

(defun midifile-c-load (name score info)
  (clce-MidiFileLoad (path2unixpath name) score info))

(defun midifile-c-save (name score info)
  (clce-MidiFileSave (path2unixpath name) score info))

(defun midifile-count-tracks (name)
  (MidiFileCountTracks name))

(defun midishare-c-save (name score)
   (clce-MidiShareSave (path2unixpath name) score)
  )

(defun midishare-c-load (name score)
  (clce-MidiShareLoad (path2unixpath name) score))
)

