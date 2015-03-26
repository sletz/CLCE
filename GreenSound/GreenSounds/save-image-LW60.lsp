(load "/Users/GerlandAttraprCouleur/LispAttrapeCouleur/LispWorks\ 6.0/CLCE/my_init.lisp")
(load "/Users/GerlandAttraprCouleur/LispAttrapeCouleur/LispWorks\ 6.0/GreenSounds/GreenSounds.lsp")
#+:cocoa
(compile-file-if-needed
 (example-file 
  "configuration/macos-application-bundle")
 :load t)
(save-image 
 #+:cocoa
 (write-macos-application-bundle
  "/Users/GerlandAttraprCouleur/LispAttrapeCouleur/LispWorks\ 6.0/GreenSounds.app")
 #-:cocoa
 "GreenSounds" :restart-function #'restart-function :multiprocessing t)