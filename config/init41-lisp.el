(setq inferior-lisp-program "sbcl.exe") ; it probably easier to use UNIX style  forward slashes
 
;then add your SLIME folder to Emacs load path
;(add-to-list 'load-path "PUT_YOUR_SLIME_INSTALLATION_FOLDER_HERE")
(require 'slime)
(slime-setup)