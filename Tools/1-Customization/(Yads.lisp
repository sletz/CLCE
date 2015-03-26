;;;-*- Mode: Lisp; Package: (Make) -*-
;;;; File: Yads.lisp
;;;;
;;;; Author: Peter Bengtson
;;;;
;;;; Created: 96-08-07
;;;;
;;;; Purpose: Yet Another DefSystem -- similar to Mark Kantrowitz'
;;;;          DEFSYSTEM utility. This one actually works (!): it supports
;;;;          Macintosh file names, logical hosts and hierarchical
;;;;          source folders. It is also much smaller. and it is designed
;;;;          to be easily extended with functionality for things like
;;;;          file search and replace, deletion of binaries, etc. It also
;;;;          supports multi-user file check in/out and file locking.
;;;;
;;;; To do: Implement the dependency logic in :COMPILE.
;;;;        Add some documentation.
;;;;
;;;; Modification History:
;;;; date      modification
;;;; ============================================================
;;;; 96-08-07  Created
;;;; 96-08-08  Generalized MK to take several arguments and to use
;;;;             the recursive descent generators instead of the
;;;;             list produced by LOAD-ORDER.
;;;; 96-12-12  Yads now always loads the system definition when doing
;;;;             a MK, in case it has been changed by the user. Also,
;;;;             the *central-registry* should be a list of directories.
;;;; 97-01-17  Added MK-SEARCH, MK-REPLACE and MK-OPEN for convenience
;;;; 97-02-20  Added multi-user support. Files can now be checked out, 
;;;;             the shared originals being locked until the new file(s)
;;;;             are checked into the project/system again. 

#+(and apple mcl powerpc)
(progn

(defpackage :make)
(in-package :make)

(import '(defsystem 
           mk mk-ed mk-search mk-replace mk-open mk-print
           *central-registry*) :ccl)

(export '(defsystem 
           mk mk-ed mk-search mk-replace mk-open mk-print
           *central-registry*) :ccl)


(defparameter *central-registry*  ; Folders containing system definitions
  '(#P"ccl:Systems;" 
    #P"DOCYANN:LispServer:Systems:"))

(defparameter *checked-out-dir*   ; Directory for the checked-out files in a multi-developer system
  #P"ccl:Yads-Checked-Out-Files;")
                                 

(defvar *defined-systems* nil)                           ; Contains the defined system objects
(defvar *loaded-files* (make-hash-table :test #'equal))  ; Pathnames -> load times

(defvar *seen-modules*)          ; Used during order-dependent module traversals

(defvar *debug* nil)             ; T if print debugging information
(defvar *dry* nil)               ; T if simulate loads/compiles (a "dry run")

(defvar *indent* 0)              ; Current number of spaces to indent


;;; -----------------------------------------------------------------------------
;;; CLOS classes to represent the systems, modules, files, etc.
;;; -----------------------------------------------------------------------------

(defclass node-object ()
  ((name              :initform nil   :accessor name              :initarg :name)
   (parent            :initform nil   :accessor parent            :initarg :parent)
   (compile-p         :initform t     :accessor compile-p         :initarg :compile-p)
   (source-directory  :initform nil   :accessor source-directory  :initarg :source-directory)
   (binary-directory  :initform nil   :accessor binary-directory  :initarg :binary-directory)
   (source-extension  :initform nil   :accessor source-extension  :initarg :source-extension)
   (binary-extension  :initform nil   :accessor binary-extension  :initarg :binary-extension)
   (source-pathname   :initform nil   :accessor source-pathname)
   (binary-pathname   :initform nil   :accessor binary-pathname)))


(defclass leaf-object (node-object) ())

(defclass file-object (leaf-object) ())


(defclass composite-object (node-object)
  ((module-package     :initform nil   :accessor module-package     :initarg :package)
   (depends-on         :initform nil   :accessor depends-on         :initarg :depends-on)   
   (required           :initform nil   :accessor required           :initarg :required)
   (initially-do       :initform nil   :accessor initially-do       :initarg :initially-do)
   (components         :initform nil   :accessor components         :initarg :components)
   (finally-do         :initform nil   :accessor finally-do         :initarg :finally-do)))


(defclass module-object (composite-object) ())

(defclass subsystem-object (composite-object) ())

(defclass system-object (composite-object) 
  ((compile-external-p :initform nil   :accessor compile-external-p :initarg :compile-external-p)))


;;; -----------------------------------------------------------------------------
;;; Two print methods (mostly for debugging purposes)
;;; -----------------------------------------------------------------------------

(defmethod print-object ((self composite-object) stream)
  (print-unreadable-object (self stream :type t :identity nil)
    (format stream "~S" (name self))))


(defmethod print-object ((self file-object) stream)
  (print-unreadable-object (self stream :type t :identity nil)
    (format stream "~S" (name self))))


;;; -----------------------------------------------------------------------------
;;; Indented messages and debug reports
;;; -----------------------------------------------------------------------------

(defun deeper ()
  (if *debug* (+ *indent* 3) *indent*))
    

(defun message-n (n string args)
  (format t "~V&;~V@T~?" n *indent* string args))

(defun message (string &rest args)
  (declare (dynamic-extent args))
  (message-n 1 string args))

(defun message2 (string &rest args)
  (declare (dynamic-extent args))
  (message-n 2 string args))


(defun debug-report (node)
  (message2 "~S" node)
  (message "Source pathname ~S" (source-pathname node))
  (message "Binary pathname ~S" (binary-pathname node)))


;;; -----------------------------------------------------------------------------
;;; The basic macro
;;; -----------------------------------------------------------------------------

(defmacro defsystem (name &rest args)
  `(make-system ',name ',args))


(defun make-system (name args)
  (let ((sysobj (make-component ':system name nil args)))
    (setq *defined-systems* (remove name *defined-systems* :key #'name :test #'string-equal))
    (determine-inheritance sysobj)
    (push sysobj *defined-systems*)
    name))


;;; -----------------------------------------------------------------------------
;;; MAKE-COMPONENT constructs a component given its type, name, parent and
;;; arg list. It descends recursively through its subcomponents.
;;; -----------------------------------------------------------------------------

(defun make-component (type name parent args)
  (apply #'make-instance (translate-to-class-name type) 
         :name name :parent parent args))


(defgeneric translate-to-class-name (type)
  (:method ((type (eql ':system)))    'system-object)
  (:method ((type (eql ':subsystem))) 'subsystem-object)
  (:method ((type (eql ':module)))    'module-object)
  (:method ((type (eql ':file)))      'file-object)
  (:method ((type t)) (error "Unsupported module type: ~S" type)))


(defmethod initialize-instance :after ((self node-object) 
                                       &rest rest 
                                       &key (compile-p t compile-p-present-p))
  (declare (ignore rest compile-p))
  (setf (name self) (filename-to-string (name self)))
  (when (and (not compile-p-present-p) (parent self))
    (setf (compile-p self) (compile-p (parent self)))))
                             

(defmethod initialize-instance :after ((self composite-object) &rest args)
  (declare (ignore args))
  (setf (depends-on self) (mklist (depends-on self)))
  (setf (required self) (mklist (required self)))
  (setf (components self) (descend-components (mklist (components self)) self)))
      

(defun mklist (thing)
  (if (listp thing)
    thing
    (list thing)))


(defun descend-components (components parent)
  (mapcar #'(lambda (component)
              (cond ((or (stringp component)
                         (pathnamep component)) (make-component :file component parent nil))
                    ((null component) component)
                    ((atom component) (error "Illegal component: ~S" component))
                    (t (destructuring-bind (type name &rest args)
                                           component
                         (make-component type name parent args)))))
          components))


;;; ---------------------------------------------------------------------------
;;; Descend through a given system or module and determine the inheritance of 
;;; properties and pathnames of all modules and files
;;; ---------------------------------------------------------------------------

(defmethod determine-inheritance ((name t))
  (determine-inheritance (or (find-defined-system (string name))
                             (error "The system ~S is not loaded" name)))
  name)


(defmethod determine-inheritance ((self system-object))
  ;; Nothing is inherited - pathnames are absolute by default
  (when *debug* (message2 "***Assembling pathnames for system ~S" self))
  (unless (source-extension self) (setf (source-extension self) *.lisp-pathname*))
  (unless (binary-extension self) (setf (binary-extension self) *.fasl-pathname*))
  (setf (source-pathname self) (source-directory self))
  (setf (binary-pathname self) (binary-directory self))
  (mapc #'determine-inheritance (components self))
  (when *debug* (debug-report self))
  (when *debug* (message2 "***End of assembly of pathnames for system ~S~&" self))
  self)


(defmethod determine-inheritance ((self subsystem-object))
  ;; Only the extensions are inherited - the pathnames are absolute
  (let ((*indent* (deeper)))
    (inherit-extensions self)
    (setf (source-pathname self) (source-directory self))
    (setf (binary-pathname self) (binary-directory self))
    (mapc #'determine-inheritance (components self))
    (when *debug* (debug-report self))
    self))


(defmethod determine-inheritance ((self module-object))
  ;; Inherit both extensions and directories - pathnames are relative
  (let ((*indent* (deeper)))
    (inherit-extensions self)
    (inherit-directories self)
    (mapc #'determine-inheritance (components self))
    (when *debug* (debug-report self))
    self))
  

(defmethod determine-inheritance ((self file-object))
  ;; Inherit extensions and assemble the complete relative pathnames
  (let ((*indent* (deeper)))
    (inherit-extensions self)
    (inherit-pathnames self)
    (when *debug* (debug-report self))
    self))



(defun inherit-extensions (node)
  (let ((parent (parent node)))
    (unless (source-extension node) (setf (source-extension node) (source-extension parent)))
    (unless (binary-extension node) (setf (binary-extension node) (binary-extension parent)))))


(defun inherit-directories (node)
  (setf (source-pathname node)
        (merge-pathnames (pathname (or (source-directory node)
                                       (make-pathname :directory 
                                                      `(:relative ,(filename-to-string (name node))))))
                         (source-pathname (parent node))))
  (setf (binary-pathname node)
        (merge-pathnames (pathname (or (binary-directory node)
                                       (make-pathname :directory 
                                                      `(:relative ,(filename-to-string (name node))))))
                         (binary-pathname (parent node)))))


(defun inherit-pathnames (node)
  (setf (source-pathname node)
        (merge-pathnames (pathname (or (source-directory node) (source-pathname (parent node))))
                         (merge-pathnames (make-pathname :name (filename-to-string (name node)))
                                          (source-extension node))))
  (setf (binary-pathname node)
        (merge-pathnames (pathname (or (binary-directory node) (binary-pathname (parent node))))
                         (merge-pathnames (make-pathname :name (filename-to-string (name node)))
                                          (binary-extension node)))))


(defun filename-to-string (name)
  ;; Don't capitalize strings, only the print names of symbols
  (if (stringp name)
    name
    (string-capitalize name)))


;;; ----------------------------------------------------------------------------------
;;; Functions to search for modules by name and to load external system definitions
;;; from the central registry.
;;; ----------------------------------------------------------------------------------

(defun find-named-module (name modules)
  (find (string name) modules :key #'name :test #'string-equal))


(defun find-internal-module (name module)
  ;; Search each sibling: go to the parent, if any, and check each component
  (when (parent module)
    (when *debug* (message "Searching for ~S among its siblings" name))
    (find-named-module name (components (parent module)))))


(defun find-defined-system (name)
  (when *debug* (message "Looking for the system ~S among the defined systems" name))
  (find-named-module name *defined-systems*))


(defun find-registry-system (name)
  ;; Searches each registry folder for the system definition file 
  (when *debug* (message "Finding external defsystem specification for ~S" name))
  (let ((npn (make-pathname :name (filename-to-string name) :type "system")))
    (some #'(lambda (registry)
              (let ((complete (merge-pathnames npn registry)))
                (when *debug* (message "Probing registry for system ~S" complete))
                (probe-file complete)))
          *central-registry*)))
                                         

(defun load-named-defsys (name)
  (let ((syspn (find-registry-system name)))
    (unless syspn 
      (error "The system definition file ~S cannot be found" syspn))
    (load syspn)
    (find-defined-system name)))


;;; ----------------------------------------------------------------------------------
;;; MK-ED brings up a Fred window containing the definition of a system
;;; ----------------------------------------------------------------------------------

(defun mk-ed (name)
  (ed (find-registry-system name)))


;;; -------------------------------------------------------------------------------------
;;; MK-RECURSE recurses through a system or module and applies FN to any FILE-OBJECT
;;; it encounters. Dependency circularities are handled properly, as are package changes.
;;; 
;;; Arguments:
;;; 
;;;   FN           The function that will be called with a file-object, the root or NIL,
;;;                and the contents of the list of fixed arguments. The arglist of FN
;;;                should thereby be equivalent to (fileobj root {arg}*)
;;; 
;;;   EVAL-AUX     True if evaluate REQUIRED, INITIALLY-DO and FINALLY-DO forms. This is
;;;                normally true when doing a load or a compilation, false for anything
;;;                else.
;;; 
;;;   ROOT         The root system object, which allows subnodes to access rootsys info.
;;;               
;;;   CURRENTSYS   The system object we are currently processing. When this is no longer
;;;                EQ to ROOT, we are processing an external system.
;;; 
;;;   ARGS         a list of arguments that is passed unchanged to each invocation of FN.
;;; -------------------------------------------------------------------------------------

(defmethod mk-recurse ((file-object file-object) fn eval-aux root currentsys args)
  (declare (ignore eval-aux))
  (let ((*indent* (deeper)))
    (apply fn file-object root currentsys args)))


(defmethod mk-recurse ((module composite-object) fn eval-aux root currentsys args)
  (unless (find module *seen-modules*)          ;Don't load modules twice
    (let ((*indent* (deeper)))
      (when *debug* 
        (message2 "Beginning processing of ~:[external ~]module ~S" (eq root currentsys) module))
      (pushnew module *seen-modules*)             ;Record that we've started processing this module
      ;; First traverse the modules this module depends on
      (mapc #'(lambda (name)
                (unless (find-named-module name *seen-modules*)  ;Do nothing if it is being processed
                  (let* ((depmod (or (find-internal-module name module)
                                     (find-defined-system name)
                                     (load-named-defsys name)))
                         (curr-or-new (if (typep depmod 'system-object) depmod currentsys)))
                    (determine-inheritance depmod)
                    (mk-recurse depmod fn eval-aux root curr-or-new args))))
            (depends-on module))
      ;; Do the requires
      (when eval-aux
        (let ((req (required module)))
          (when req
            (mapc #'(lambda (req) 
                      (message "Requiring ~S" req)
                      (if (listp req) 
                        (apply #'require req)
                        (funcall #'require req)))
                  (required module)))))
      (let ((*package* (if (module-package module)               ; Bind *package* to requested value
                         (find-package (module-package module))  ; or to itself if NIL
                         *package*)))
        (when *debug* (message "Package is now bound to ~S" *package*))
        ;; Do the initial stuff 
        (when eval-aux
          (let ((init (initially-do module)))
            (when init
              (message "Evaluating ~S" init)
              (eval init))))
        ;; Traverse the components
        (mapc #'(lambda (comp) (mk-recurse comp fn eval-aux root currentsys args)) 
              (components module))
        ;; Finally do the final stuff (after which *package* reverts to its previous value)
        (when eval-aux
          (let ((final (finally-do module)))
            (when final
              (message "Evaluating ~S" final)
              (eval final))))))))


;;; ---------------------------------------------------------------------------------
;;; FILE-COMPONENT-LIST uses MK-RECURSE to build an ordered list of the pathnames of 
;;; the source files that make up the system. If INCLUDE-EXTERNALS-P is true, the 
;;; list will include sources in DEPEND-ON systems.
;;; ---------------------------------------------------------------------------------

(defun file-component-list (rootsys include-externals-p &key (key #'source-pathname))
  (let ((*seen-modules* nil) ; This allows this function to be used outside an MK call
        (files nil))
    (mk-recurse rootsys #'(lambda (fileobj rootsys currentsys)
                            (when (or include-externals-p
                                      (eq rootsys currentsys))
                              (push (funcall key fileobj) files)))
                nil rootsys rootsys nil)
    (nreverse files)))


;;; ------------------------------------------------------------------------------
;;; MK uses MK-RECURSE to make the system. NAME is the name of the system.
;;; ------------------------------------------------------------------------------

(defun mk (name &key (action :load) force dry debug arg1 arg2)
  (let ((*seen-modules* nil)
        (*indent* 0)
        (*dry* dry)
        (*debug* debug)
        (rootsys (load-named-defsys name)))        ; Refresh the system definition
    (determine-inheritance name)
    (mk-action action  :rootsys rootsys  :force force  :arg1 arg1  :arg2 arg2)
    name))


(defmethod mk-action ((act t) &key &allow-other-keys)
  (error "Unrecognized MK action: ~S" act))


(defmethod mk-action ((act (eql :load)) &key rootsys force &allow-other-keys)
  (mk-recurse rootsys #'mk-load-action (not *dry*) rootsys rootsys (list force)))


(defmethod mk-action ((act (eql :compile)) &key rootsys force &allow-other-keys)
  (mk-recurse rootsys #'mk-compile-action (not *dry*) rootsys rootsys (list force)))


(defmethod mk-action ((act (eql :delete-binaries)) &key rootsys &allow-other-keys)
  (mk-recurse rootsys #'mk-delete-action nil rootsys rootsys nil))


(defmethod mk-action ((act (eql :search)) &key rootsys arg1 &allow-other-keys)
  (process-run-function "Search System"
    #'(lambda (pathname arg1)
        (let ((*standard-output* (ccl::ed-standard-output)))
          (ccl::do-dialog-file-search pathname arg1)))
    (file-component-list rootsys nil) arg1))


(defmethod mk-action ((act (eql :replace)) &key rootsys arg1 arg2 &allow-other-keys)
  (mk-recurse rootsys #'mk-replace-action nil rootsys rootsys (list arg1 arg2)))


(defmethod mk-action ((act (eql :open)) &key rootsys arg1 &allow-other-keys)
  (mk-recurse rootsys #'mk-open-action nil rootsys rootsys (list arg1)))


(defmethod mk-action ((act (eql :print)) &key rootsys &allow-other-keys)
  (mk-recurse rootsys #'mk-print-action nil rootsys rootsys nil))


;;; ------------------------------------------------------------------------------
;;; Support functions for the MK-ACTION methods
;;; ------------------------------------------------------------------------------

(defun mk-load-action (fileobj rootsys currentsys force)
  (multiple-value-bind (sourcepn binarypn)
                       (determine-pathname-candidates fileobj)
    (ecase (how-load-file sourcepn binarypn fileobj rootsys currentsys force)
      (:LOAD-BINARY             (mk-load binarypn))
      (:COMPILE-AND-LOAD-BINARY (mk-compile sourcepn binarypn) 
                                (mk-load binarypn))
      (:LOAD-SOURCE             (mk-load sourcepn))
      (:DO-NOTHING nil))))


(defun how-load-file (sourcepn binarypn fileobj rootsys currentsys force)
  (let ((source (probe-file sourcepn))
        (binary (probe-file binarypn)))
    (unless (or source binary) 
      (error "Neither the source file ~S nor its binary ~S can be found" sourcepn binarypn))
    (cond ((not source) 
           (warn "Cannot find source file ~S, only its binary ~S" sourcepn binarypn)
           (if (or force 
                   (> (file-write-date binarypn) (last-loaded binarypn)))
             :LOAD-BINARY
             :DO-NOTHING))
          ((or (not binary) 
               (> (file-write-date sourcepn) (file-write-date binarypn)))           
           (if (and (compile-p fileobj)
                    (or (eq rootsys currentsys)
                        (compile-external-p currentsys)))
             :COMPILE-AND-LOAD-BINARY
             :LOAD-SOURCE))
          ((or force 
               (> (file-write-date binarypn) (last-loaded binarypn)))
           :LOAD-BINARY)
          (t :DO-NOTHING))))


(defun last-loaded (pathname)
  (gethash pathname *loaded-files* 0))

(defun set-last-loaded (pathname time)
  (setf (gethash pathname *loaded-files*) time))


(defun checked-out-pathname (pn)
  "Returns the pathname to the checked-out version of the file pn."
  (merge-pathnames (make-pathname :directory `(:relative ,@(cdr (pathname-directory pn)))
                                  :name (pathname-name pn)
                                  :type (pathname-type pn)
                                  :version (pathname-version pn))
                   *checked-out-dir*))


(defun determine-pathname-candidates (fileobj)
  "Returns the pathnames of the source and the binary files as two values. 
      If the source is checked out, the checked-out binary pn is used."
  (let* ((sourcepn (source-pathname fileobj))
         (binarypn (binary-pathname fileobj))
         (chosource (probe-file (checked-out-pathname sourcepn))))
    (if chosource 
      (values chosource (checked-out-pathname binarypn))  ; Checked-out source -> ch-out binary
      (values sourcepn binarypn))))                       ; No ch-o source -> no ch-out binary


(defun mk-load (pathname)
  (cond (*dry* 
         (message "Load of ~S" pathname))
        (t     
         (load pathname :verbose t)
         (set-last-loaded pathname (get-universal-time)))))


(defun mk-compile (source binary)
  (if *dry*
    (message "Compile of ~S, saving as ~S" source binary)
    (compile-file source :output-file binary :verbose t)))


(defun mk-delete-action (fileobj rootsys currentsys)
  (when (eq currentsys rootsys)  ; Only delete internal binaries
    (let ((bin (nth-value 1 (determine-pathname-candidates fileobj))))  ; the binary
      (when bin
        (message "Deleting binary ~S" bin)
        (unless *dry*
          (delete-file bin))))))


(defun mk-compile-action (fileobj rootsys currentsys force)
  ;; This function is not complete yet. For each encountered file which needs
  ;; or is forced to be compiled, the objects that depend on the module it is
  ;; a part of (etc) must also be recompiled. Might be fairly tricky. 
  ;; For now, we skip the dependencies.
  (multiple-value-bind (sourcepn binarypn)
                       (determine-pathname-candidates fileobj)
    (let ((source (probe-file sourcepn))
          (binary (probe-file binarypn)))
      (unless (or source binary) 
        (error "Neither the source file ~S nor its binary ~S can be found" sourcepn binarypn))
      (cond ((not source)
             (warn "Cannot find source file ~S, only its binary ~S" sourcepn binarypn)
             (mk-load binarypn))
            ((or force 
                 (not binary)
                 (> (file-write-date sourcepn) (file-write-date binarypn)))
             ;; At this point we should do the dependencies
             (when (and (compile-p fileobj)                 ; Compile only if compileable
                        (or (eq rootsys currentsys)           ; and if we're inside the root system
                            (compile-external-p currentsys))) ; or if explicitly allowed by the system
               (mk-compile sourcepn binarypn))
             (mk-load binarypn))                        ; Always load
            (t nil)))))


(defun mk-replace-action (fileobj rootsys currentsys search-string replace-string)
  (declare (ignore fileobj rootsys currentsys search-string replace-string))
  (error ":REPLACE is not an implemented :ACTION yet"))


(defun mk-open-action (fileobj rootsys currentsys filename)
  (when (and (eq rootsys currentsys)
             (string-equal filename (name fileobj)))
    (ed (determine-pathname-candidates fileobj))))
    

(defun mk-print-action (fileobj rootsys currentsys)
  (when (eq rootsys currentsys)
    (ed (determine-pathname-candidates fileobj))
    (window-hardcopy (front-window) nil)
    (window-close (front-window))))
    

;;; ------------------------------------------------------------------------------
;;; Convenience functions
;;; ------------------------------------------------------------------------------

(defun mk-search (system string)     (mk system :action :search :arg1 string))
(defun mk-replace (system str1 str2) (mk system :action :replace :arg1 str1 :arg2 str2))
(defun mk-open (system filename)     (mk system :action :open :arg1 filename))
(defun mk-print (system)             (mk system :action :print))


;;; ------------------------------------------------------------------------------
;;; Checking files out of and back into a project
;;; ------------------------------------------------------------------------------

(defun find-file-object (pn)
  "Searches all defined systems for the file object pointing to the given full source pathname."
  (dolist (system *defined-systems*)
    (let ((fileobj (find pn (file-component-list system nil :key #'identity)
                         :key #'source-pathname :test #'equal)))
      (when fileobj (return-from find-file-object fileobj)))))


(defun find-original-file-object (pn)
  "Searches all defined systems for the original file object from which the checked-out source file pn was created."
  (let ((pntrsl (translate-logical-pathname pn)))
    (dolist (system *defined-systems*)
      (let ((fileobj (find pntrsl (file-component-list system nil :key #'identity)
                           :key (lambda (fobj)
                                  (translate-logical-pathname 
                                   (checked-out-pathname (source-pathname fobj))))
                           :test #'equal)))
        (when fileobj (return-from find-original-file-object fileobj))))))


(defun find-root-system (node)
  "For any node in a system, returns the root node, the system itself."
  (let ((parent (parent node)))
    (if parent
      (find-root-system parent)
      node)))


(defun find-open-window (filename)
  (find filename (windows) :key #'window-filename :test #'equal))


(defun msg-dialog (string &rest args)
  (message-dialog (apply #'format nil string args) 
                  :size #@(400 100)
                  :title "Error"
                  :position '(:top 100)))


(defun readable-namestring (pn)
  (format nil "~A {~A}" (file-namestring pn) (directory-namestring pn)))


(defun mk-check-out (filename)
  "Checks out a file and its binary from any project it might be a part of. Locks the originals."
  (let ((fileobj (find-file-object filename)))
    (unless fileobj
      (if (find-original-file-object filename)
        (msg-dialog "The file ~A is already a checked-out copy." (readable-namestring filename))
        (msg-dialog "The file ~A is not part of any currently defined and loaded system." 
                    (readable-namestring filename)))
      (return-from mk-check-out))
    (let* ((w nil)
           (root (find-root-system fileobj))
           (sourcepn (source-pathname fileobj))
           (binarypn (binary-pathname fileobj))
           (source-co (checked-out-pathname sourcepn))
           (binary-co (checked-out-pathname binarypn)))
      (when (probe-file source-co)
        (msg-dialog "You have already checked out the source file ~A from system ~S." 
                    (readable-namestring sourcepn) (name root))
        (return-from mk-check-out))
      (when (file-locked-p sourcepn)
        (msg-dialog "Somebody else has already checked out the source file ~A from system ~S." 
                    (readable-namestring sourcepn) (name root))
        (return-from mk-check-out))
      (when (probe-file binary-co)
        (msg-dialog "The binary file ~A has already been checked out from system ~S." 
                    (readable-namestring binarypn) (name root))
        (return-from mk-check-out))
      (unless (y-or-n-dialog 
               (format nil "Check out source file ~A and any associated binary file from system ~S?"
                       (readable-namestring filename) (name root))
               :cancel-text nil
               :position '(:top 100))
        (return-from mk-check-out))
      (setq w (find-open-window filename))
      (when w (window-close w))
      (copy-file sourcepn source-co)
      (lock-file sourcepn)
      (when (probe-file binarypn)
        (copy-file binarypn binary-co)
        (lock-file binarypn))
      (when w (ed source-co)))))


(defun mk-check-in (filename)
  "Checks in a file and its binary into any project it might be a part of. Removes the locked originals."
  (let ((fileobj (find-original-file-object filename)))
    (unless fileobj
      (msg-dialog "The file ~A has not been checked out from any currently defined and loaded system." 
                  (readable-namestring filename))
      (return-from mk-check-in))
    (let* ((w nil)
           (root (find-root-system fileobj))
           (sourcepn (source-pathname fileobj))
           (binarypn (binary-pathname fileobj))
           (source-co (checked-out-pathname sourcepn))
           (binary-co (checked-out-pathname binarypn)))
      (unless (probe-file sourcepn)
        (msg-dialog "Warning: The shared source version of ~A cannot be found in system ~S." 
                    (readable-namestring source-co) (name root)))
      (unless (file-locked-p sourcepn)
        (msg-dialog "Warning: the shared source version of ~A in system ~S should be locked but isn't." 
                    (readable-namestring source-co) (name root)))
      (unless (y-or-n-dialog 
               (format nil "Check in source file ~A and any associated binary file into system ~S?"
                       (readable-namestring filename) (name root))
               :cancel-text nil
               :position '(:top 100))
        (return-from mk-check-in))
      (setq w (find-open-window filename))
      (when w (window-close w))
      (unlock-file sourcepn)
      (delete-file sourcepn)
      (copy-file source-co sourcepn)
      (delete-file source-co)
      (when (probe-file binary-co)
        (when (probe-file binarypn)
          (unlock-file binarypn)
          (delete-file binarypn))
        (copy-file binary-co binarypn)
        (delete-file binary-co))
      (when w (ed sourcepn)))))


(def-fred-command (:function #\5) fred-check-out)
(def-fred-command (:function #\6) fred-check-in)


(defun fred-check-out (item)
  (mk-check-out (window-filename item)))

(defun fred-check-in (item)
  (mk-check-in (window-filename item)))



); end of Yads.lisp
