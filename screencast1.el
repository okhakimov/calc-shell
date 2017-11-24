(setq s-list (my-scr-add-commands (split-string "
;;Type an expression and press <f5> to evaluate.
;l
;;Basics:
(1.2+5)*10/2                  
4^2+2^3+sqrt(16)+8^(1/3)      
ln(e) + log10(100)            
sin(90) + cos(60) + tan(45)   
deg(pi/4)                     
rad(180)                      
arcsin(0.5)
;;
;;Assignments:
a=arccos(0.5)
b=a-30
;;
;;Operate on result of previous operation (variable ii):
18+2 
ii*10
;;
;;End of the screencast. Bye.
" "\n")))


(defun my-scr-add-commands (slist) 
  (let ((value ()))
    (dolist (elt slist value)
      (if (not (eq "" elt))
	(progn
          (let ((x (substring elt 0 2)) (rest (substring elt 2)))
	    (cond ((equal x ";;") (progn (setq value (append value (cons (list 'i rest) (list '(newline)))))))
	      ((equal x ";l") (progn (setq value (append value (cons (list 'insert (make-string 46 ?-)) (list '(newline)))))))
	      (t (progn (setq value (append value (cons (list 'i elt) (list '(calc-shell))))))))))))))    
 



(setq screencast-use-message-buffer nil)
(setq screencast-speed 3.0)


(screencast s-list "calc-shell basic" 1)


