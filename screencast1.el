;;;; -*- lisp -*-
;;;; --------- defun format commands
(defun my-scr-add-commands (slist) 
  (let ((value ()))
    (dolist (elt slist value)
      (if (not (eq "" elt))
	(progn
          (let ((x (substring elt 0 2)) (rest (substring elt 2)))
	    (cond ((equal x ";;") (progn (setq value (append value (cons (list 'i rest) (list '(newline)))))))
	      ((equal x ";l") (progn (setq value (append value (cons (list 'insert (make-string 46 ?-)) (list '(newline)))))))
	      (t (progn (setq value (append value (cons (list 'i elt) (list '(calc-shell))))))))))))))    
 

;;;; --------- basic screencast
(setq s-list (my-scr-add-commands (split-string "
;;Type an expression and press <f5> to evaluate.
;;
;;Arithmetics.
10/2*(1.2+5)                  
4^2+2^3+sqrt(16)+8^(1/3)
;;
;;Logarithms.
ln(e) + log10(100)
;;
;;Trigonometric functions.            
sin(90) + cos(60) + tan(45)   
deg(pi/4)                     
rad(180)                      
arcsin(0.5)
;;
;;Operate on the result of the previous operation (variable ii).
18+2 
ii*10
ii+ii
;;
;;Assignments.
a=10
b=20
c=a+b
;;
;;Complex numbers. (a,b)= a + i*b. polar notation: (r; arg).
;;
;;create a complex number: 3 + i*4:
c1=(3,4) 
abs(c1)  
arg(c1)  
re(c1)   
im(c1)   
c1*(2,1) 
polar((3,4)) 
cpolar=(4;60)
rect(cpolar) 
;;
;;Misc commands:
;;% ?     - help 
;;% fix n - set to n number of significant digits in floating point notation.
;;% deg   - angles will be measured in degrees
;;% rad   - angles will be measured in radians
% fix 3
10/7
% deg
arcsin(0.5)
% rad
arcsin(0.5)
;;
;;The end of the screencast. Bye.
" "\n")))
;;;; --------- advanced screencast
(setq s-list-advanced (my-scr-add-commands (split-string "
;;Type an expression and press <f5> to evaluate.
;;
;;Vectors, statistics.
;;
;;create a vector:
v=[1,3,4,100] 
;;
;;sum the first and the second element of the vector:
v_1+v_3
;;
;;mean, median, and standard deviation:       
vmean(v)      
vmedian(v)    
vsdev(v)
;;
;;build a vector of consecutive integers from 1 to 4:
v1=index(4)
;;
;;build a vector of 3 copies of 100:
v2=cvec(100,3) 
;;
;;generate vector of 3 random numbers in the range: 0 - 99
map(random,cvec(100,3)) 
;;
;;Calculus.
;;
;;define a function f:
f=3x^2 + 2x +1      
;;
;;the derivative of f with respect to x:    
deriv(f,x)              
;;
;;the derivative of f with respect to x evaluated at the point x=2.
deriv(f,x,2)            
deriv(f(2x), x)         
integ(3x^2+1,x)         
vsum(v)                     
;;
;;Date, time.
d=now()                     
d+2*7                       
time(d)                     
d+10@ 30'                   
;;
;;Unit conversion.
;; 
;;convert 1 feet to m
uc(ft/m)
;;
;;convert 2 in to cm                    
uc(2in/cm)                   
;;
;;The end of the screencast. Bye.
" "\n")))


;;;; --------- settings
 (progn 
   (setq screencast-use-message-buffer nil)
   (setq screencast-speed 3)
 )
;;;; --------- reset floating point number format to default
(progn
  (ignore-errors (set-buffer "*Calculator*"))
  (setq calc-float-format '(float 0)))

;;;; --------- run screencast
(screencast s-list "basic-screencast.txt" 1)
(screencast s-list-advanced "advanced-screencast.txt" 1)


