;;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;;
;;
;;                                  tuples.lisp
;;
;;                                     © 2001, GRAME.
;;
;;
;;
;;
;;
;;  Fichier définissant de petites extensions à CLOS.
;;
;;
;; HISTORIQUE :
;;  01-09-91, Première version. -Yo-
;;
;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

#|
			(2) TUPLES

Historique :
------------
-- 2/5/99 : correction bug dans xxx-p qui ne testait pas le fait que l'objet etait une liste
tuple-modifier
-- (4) ajout d'une fonction (replace-tuple <dest> <src>) qui transforme <dest> en lui donnant le type et tous les champs de <src>
-- (3) ajout generation d'un "tuple-modifier" de la forme "(set-xxx <objet> <val field 1> ... <val field n>)"
	les fonctions d'acces aux champs permettent maintenant de modifiers les champs : 
	lecture "(xxx-fff <obj>)" -> val et ecriture "(xxx-fff <obj> <val>)" 
-- (2) modif pour les tuples vides et ajout de NIL pour ignorer une partie d'un tuple dans tuplecase
-- (1) commencé le 29-05-94


Mode d'emploi :
---------------

--- exemple de déclaration d'un tuple

(deftuple toto f1 f2 f3)	crée les fonctions : toto, toto-p, toto-f1, toto-f2, toto-f3 et set-toto 

--- exemple d'utilisation de tuplecase

(tuplecase exp
  ((score exp1 exp2) (+ (duration exp1) (duration exp2) ))
  ((chord exp1 exp2) (max (duration exp1) (duration exp2) ))
  (otherwise 0))

|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; implémentation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; declaration d'un tuple

(defun tuple-remove-default (tfield)
  (if (consp tfield)
    (car tfield)
    tfield))

(defun tuple-builder (name tfieldlist)
  (if tfieldlist
    `(defun ,name (&optional ,@tfieldlist) 
       (list ',name ,@(mapcar #'tuple-remove-default tfieldlist)))
    `(defun ,name () (list ',name))))

(defun tuple-predicat (name)
  (let ((fun-name (intern (concatenate 'string (string name) "-P") (symbol-package name))))
    `(defun ,fun-name (tuple) (and (consp tuple) (eq ',name (car tuple))))))


(defun tuple-accessors (name tfieldlist)
  (let ((res ())
        (tnum 1))
    (dolist (tfield tfieldlist (nreverse res))
      (let ((fun-name (intern (concatenate 'string (string name) "-" (string (tuple-remove-default tfield))) 
                              (symbol-package name))))
        (setq res (cons `(defun ,fun-name (tuple &optional (val nil asval)) 
                           (if asval 
                             (setf (nth ,tnum tuple) val)
                             (nth ,tnum tuple))) res))
        (incf tnum)))))


(defun tuple-modifier (name tfieldlist)
  (let ((fun-name (intern (concatenate 'string "SET-" (string name)) 
                              (symbol-package name))))
    `(defun ,fun-name (this ,@tfieldlist) 
       (setf (cdr this) (list ,@tfieldlist)))))

;;;DEFTUPLE
(defmacro deftuple (name &rest tfieldlist)
  `(progn
     ,(tuple-predicat name)
     ,@(tuple-accessors name tfieldlist)
     ,(tuple-builder name tfieldlist)
     ,(tuple-modifier name tfieldlist)
     ))



;; tuplecase

(defun tuplecase-build-let (expvar tfieldlist)
  (let ((res ())
        (tnum 1))
    (dolist (tfield tfieldlist (nreverse res))
      (if tfield
        (setq res (cons `(,tfield (nth ,tnum ,expvar)) res)))
      (incf tnum))))

(defun tuplecase-build-rule (expvar typevar rule elsepart)
  (if (or (eq (car rule) 'otherwise)
          (eq (car rule) t))
    `(progn ,@(cdr rule))
    (if (symbolp (car rule))
      `(let ((,(car rule) ,expvar)) ,@(cdr rule))
      (destructuring-bind ((name . tfields) . actions) rule
        `(if (eq ',name ,typevar)
           (let ,(tuplecase-build-let expvar tfields)
             ,@actions)
           ,elsepart)))))

(defun tuplecase-build (expvar typevar rules)
  (if (null rules)
    ()
    (tuplecase-build-rule expvar typevar (car rules) (tuplecase-build expvar typevar (cdr rules)))))

;;;TUPLECASE
(defmacro tuplecase (tuple-exp &body rules)
  (let ((expvar (gensym "tuple-exp-"))
        (typevar (gensym "tuple-type-")))
    `(let ((,expvar ,tuple-exp))
       (let ((,typevar (car ,expvar)))
         ,(tuplecase-build expvar typevar rules)))))

;;;REPLACE-TUPLE
;;;remplace le type et les champs de <dst> par ceux de <src>
(defun replace-tuple (dst src)
  (setf (car dst) (car src))
  (setf (cdr dst) (copy-list (cdr src)))
  dst)


;;;COPY-TUPLE
;;;copie d'un tuple
(defun copy-tuple (tpl)
  (copy-list tpl))