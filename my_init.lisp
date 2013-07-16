
;; Definition des variables ------------------------------------------------------------------------------------
;;........ Pendant le chargement, ces variables contiennent des informations sur le fichier en cours
;;........ de chargement. Ces informations peuvent etre utilisees pour le chargement de fichiers 
;;........ compl»mentaires. A noter que l'extension .LISP ou .FASL pour mac et .X86F pour Linux  ne
;;........ correspond pas forcement au fichier reellement charge car c'est LOAD qui decide de cela.

(defvar *auto-path*      )	; pathname complet du fichier en cours de chargement
(defvar *auto-dir*       )	; directory du fichier en cours de chargement
(defvar *auto-file*      )	; nom du fichier en cours de chargement
(defvar *auto-type*      )	; extension du fichier (pas toujours significatif)
(defvar *auto-type-name* )      ; extension du fichier (pas toujours significatif)

(defvar *clce-directory* )      ; repertoire oo se trouve le present fichier Init.lisp
(defvar *server-host*    )      ; nom de l'hote pour l'affichage de l'interface graphique


(defun make-relative-path-string (filename)
  (let ((pathname-string (namestring (make-pathname :directory (append (butlast (pathname-directory (LISP-IMAGE-NAME)) 3) (list filename))))))
    (subseq pathname-string 0 (1- (length pathname-string)))))

(defun load-all-files (&optional (dir "ccl:tools;**;*.*") (*load-verbose* t))
  (labels ((shielded-name (name) (eq (char name 0) #\( )))
    (let (curr prev )  
      (dolist (*auto-path* (reverse (directory (make-relative-path-string "CLCE/Tools/**/*.*"))))
        (let* ((*auto-dir* (pathname-directory *auto-path*))
              (*auto-file* (pathname-name *auto-path*))
              (*auto-type* (pathname-type *auto-path*))
              (*auto-type-name* (if (stringp *auto-type*) (nstring-downcase *auto-type*) "")))
          (unless (or (not (or (equal *auto-type-name* "lisp") (equal *auto-type-name* "xfasl")))
                      (some #'shielded-name (rest *auto-dir*)) 
                      (shielded-name *auto-file*))
            (setq curr (make-pathname :directory *auto-dir* :name *auto-file*))
            (unless (equal curr prev)
              (load curr)
              (print curr)
              (setq prev curr) 
	     );; /unless
	   );; /unless
	 );; /let*
       );; /dolist
     );; /let
   );; /labels
 );; fin de load-all-files

(defun compile-all-files (&optional (dir "ccl:tools;**;*.*") (*load-verbose* t))
  (labels ((shielded-name (name) (eq (char name 0) #\( )))
    (let (curr prev )  
      (dolist (*auto-path* (reverse (directory (make-relative-path-string "CLCE/Tools/**/*.*"))))
        (let* ((*auto-dir* (pathname-directory *auto-path*))
              (*auto-file* (pathname-name *auto-path*))
              (*auto-type* (pathname-type *auto-path*))
              (*auto-type-name* (if (stringp *auto-type*) (nstring-downcase *auto-type*) "")))
          (unless (or (not (or (equal *auto-type-name* "lisp") (equal *auto-type-name* "xfasl")))
                      (some #'shielded-name (rest *auto-dir*)) 
                      (shielded-name *auto-file*))
            (setq curr (make-pathname :directory *auto-dir* :name *auto-file*))
            (unless (equal curr prev)
              (compile-file curr)
              (print curr)
              (setq prev curr) 
	     );; /unless
	   );; /unless
	 );; /let*
       );; /dolist
     );; /let
   );; /labels
)

(load (make-relative-path-string "load-cffi.xfasl"))
(load-all-files)

;; LAS
;;(load "/Documents/LibAudioStream-git/lisp/LibAudioStream-interface-CFFI.lisp")

;;(in-package :au)
;;(libaudiostream-framework)


;;(install-background)

