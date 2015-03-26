;;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;;
;;
;;                                  case-of.lisp
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	Case-Of : permet de faire des "case" sur des patterns
	proche de ce que l'on a dans un langage fonctionnel

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
4 categories de patterns sont possibles : variable, constante, simple, complexe

pattern variable :	*, <v>
pattern constante : 	10, "toto", 'allo, '(une jolie fille), t, nil
pattern simple : 	(number <v>), (atom <v>), (string <v>), (symbol <v>)
pattern complexe : 	(cons <p1> <p2>), (<p1> <p2> ... <pn>)

un pattern variable match n'importe quoi :
	* est un pattern variable 'sans nom' que l'on utilise quand on ne s'interesse pas a la valeur matchée.
	<v> est un nom de variable dans lequel on va recuperer la valeur matchée

un pattern constant ne match que l'expression qu'il definit

un pattern simple match un type d'objet elementaire : number atom string symbol.
Ainsi (number x) match 10 et lie x avec 10

un pattern complexe est de deux types :
	(cons <p1> <p2>) match une liste dont <p1> match le car et <p2> le cdr
	(<p1> <p2> ... <pn>) match une liste de n element dont <p1> match le premier, etc.

exemple d'utilisation
---------------------
(defun durée (exp)
  (case-of exp
    (('NOTE d * *) d)
    (('REST d) d)
    (('SEQ e1 e2) (+ (durée e1) (durée e2)))))

|#

;; CASE-OF : la macro proprement dite
;;------------------------------------
;; (case-of <exp>
;;	(<pattern1> <action1>)
;;	(<pattern2> <action2>)
;;	...
;;	(<patternX> <actionX>) )

(defmacro case-of (expression &body rule-list)
  (built-case-of (gensym "pattern-block-") expression (append rule-list '((* (error "unrecognized case")))) ))

;; BIND-OF : raccourci
;;------------------------------------
;; (bind-of <pattern> <exp>
;;	<action>)

(defmacro bind-of (pattern expression &body action)
  `(case-of ,expression
     (,pattern		,@action)))

#| exemple
(bind-of ('ac du ln) e 
  (first ln))
|#

;; BUILT-CASE-OF : construit l'expression comme un block comportant une suite de if
;;---------------------------------------------------------------------------------
;; 
(defun built-case-of (blockname expression rule-list)
  (if (atom expression)
    `(block ,blockname
       ,@(mapcar #'(lambda (rule) 
                     (built-pattern-rule expression (car rule) (built-return-action blockname (cdr rule)) ) )
                 rule-list))
    (let ((expname (gensym "pattern-exp-")))
      `(block ,blockname
         (let ((,expname ,expression))
           ,@(mapcar #'(lambda (rule) 
                         (built-pattern-rule expname (car rule) (built-return-action blockname (cdr rule)) ) )
                     rule-list))))))


;; built-return-action : construit l'action de retour en cas de succes
;;---------------------------------------------------------------------------------
;; 
(defun built-return-action (blockname action-list)
  (cond ((null action-list) `(return-from ,blockname nil))
        ((null (cdr action-list)) `(return-from ,blockname ,(car action-list)))
        (t `(return-from ,blockname (progn ,@action-list)))))



;; built-pattern-rule : construit le test en fonction du pattern
;;---------------------------------------------------------------------------------
;; 
(defun built-pattern-rule (expression pattern success-action)
  (cond ((eq pattern nil) 	`(if (null ,expression) ,success-action))
        ((numberp pattern) 	`(if (eq ,expression ,pattern) ,success-action))
        ((stringp pattern) 	`(if (equal ,expression ,pattern) ,success-action))
        ((eq pattern t) 	`(if (eq ,expression t) ,success-action))
        ((eq pattern '*) 	success-action)
        ((symbolp pattern) 	`(let ((,pattern ,expression)) ,success-action))
        ((consp pattern)
         (let ((k (car pattern)))
           (cond ((eq k 'cons)
                  (if (= 3 (length pattern))
                    (if (atom expression)
                      (built-cons-pattern-rule expression (second pattern) (third pattern) success-action)
                      (let ((expname (gensym "exp-")))
                        `(let ((,expname ,expression))
                           ,(built-cons-pattern-rule expname (second pattern) (third pattern) success-action))))
                    (error "invalide CONS pattern : ~S" pattern)))
                 ((member k '(atom number string symbol))
                  `(if (typep ,expression ',(car (member k '(atom number string symbol)))) 
                     ,(if (eq '* (second pattern))
                        success-action
                        `(let ((,(second pattern) ,expression)) 
                           ,success-action)) ))
                 ((eq k 'quote)
                  `(if (equal ,expression ,pattern) ,success-action))
                 (t
                  (if (atom expression)
                    `(if (and (consp ,expression) (= (length ,expression) ,(length pattern)))
                       ,(built-list-pattern expression pattern success-action))
                    (let ((expname (gensym "exp-")))
                      `(let ((,expname ,expression))
                         (if (and (consp ,expname) (= (length ,expname) ,(length pattern)))
                           ,(built-list-pattern expname pattern success-action)))))))))
        (t
         (error "invalide pattern : ~S" pattern))))


;; built-cons-pattern-rule : construit le test pour un pattern du type (CONS <p1> <p2>)
;;-------------------------------------------------------------------------------------
;; 
(defun built-cons-pattern-rule (expname p1 p2 success-action)
  `(if (consp ,expname)
     ,(built-pattern-rule `(car ,expname) p1 (built-pattern-rule `(cdr ,expname) p2 success-action))))


;; built-list-pattern : construit le test pour un pattern du type (<p1> <p2> ... <pX>)
;;-------------------------------------------------------------------------------------
;; 
(defun built-list-pattern (expname patlist success-action)
  (if (null patlist) success-action
      (built-pattern-rule `(car ,expname) (car patlist) (built-list-pattern `(cdr ,expname) (cdr patlist) success-action))))



;; xpand-pattern : pour tester l'expansion des patterns
;;-------------------------------------------------------------------------------------
;; 
(defmacro xpand-pattern (pattern)
  `(pprint (built-pattern-rule 'expr ',pattern 'success)))

#|
;; Quelques fonctions de test
;;

;; test des differentes expansions
(xpand-pattern 'test)
(xpand-pattern 10)
(xpand-pattern nil)
(xpand-pattern t)
(xpand-pattern '(1 2 3))
(xpand-pattern "test")
(xpand-pattern (number v))
(xpand-pattern (atom v))
(xpand-pattern (string v))

(xpand-pattern (cons p1 p2))
(xpand-pattern (p1 p2 p3))

(xpand-pattern ((number v) (cons p1 p2) "test"))
(xpand-pattern ((number v) (p1 p2) "test"))

;; comparatif de vitesse entre une ecriture normale et avec case-of
;;-----------------------------------------------------------------
(defun count1 (l)
  (if (null l) 0
      (+ (car l) (count1 (cdr l)))))

(defun count2 (l)
  (case-of l
    (nil	0)
    ((cons n ls) (+ n (count2 ls)))))


(progn
  (time (dotimes (i 100000) (count1 '(1 2 3 4 5))))
  (time (dotimes (i 100000) (count2 '(1 2 3 4 5)))))

(defun al1 (l1 l2)
  (case-of `(pair ,l1 ,l2)
    (('pair nil nil) nil)
    (('pair a nil) a)
    (('pair nil b) b)
    (('pair a b) (cons (car a) (al1 (cdr a) b)))))

(defun al2 (l1 l2)
  (case-of `(,l1 ,l2)
    ((nil nil) nil)
    ((a nil) a)
    ((nil b) b)
    ((a b) (cons (car a) (al2 (cdr a) b)))))

(defun test (exp)
  (case-of exp
    (0 'zero)
    ('toto '(un toto))
    ((string s) `(une chaine ,s))
    ((atom x) `(atom ,x))
    ((number x) `(le nombre ,x))
    (('INT (number x)) `(Int ,x))
    ((a '= b)	`(equalite entre ,a et ,b))
    ((a b c)	`(triplet ,a ,b ,c))
    (((string s) (number x)) `(special ,s ,x))
    ((cons x l) `(un cons ,x ,l))
    (((string s) (number x)) `(special ,s ,x)) ))
|#

