;;; my-calc.el --- A  wrapper for calc embedded mode.

;; Copyright (C) 2017 Oleg Khakimov

;; Author: Oleg Khakimov <okhakimov@gmail.com>
;; Version: 1.0
;; Package-Requires
;; Keywords: calc
;; URL: https://github.com/.../my-calc.el

;;; Commentary:

;; my-calc.el is a lightweight wrapper script to help you interact
;; with calc.  This
;; package is designed to be unobtrusive and to defer to calc

;;; Code:


(setq my-calc-active nil)
(setq my-calc-examples-p nil)
(setq my-calc-file (locate-library "my-calc"))
(setq my-calc-dir (file-name-directory my-calc-file))

(defun my-get-curr-line ()
      (buffer-substring-no-properties
       (line-beginning-position)
       (line-end-position)
       ))

(defun my-delete-line-if-empty ()
  (if (string-match-p "^$" (my-get-curr-line))
      (kill-line)))

(defun my-calc-pre ()
   "Prepare current line for evaluation by calc-update-formula"
   (interactive)
   (setq my-calc-org_expr (my-get-curr-line))
   (move-end-of-line 1)
   (newline)
   (previous-line 1)
   (setq expr  (replace-regexp-in-string "\s+$" "" (car (split-string my-calc-org_expr "=>"))))
   ;add ii:= at the beginning of the line if there is no other assignment
   (if (not (string-match-p "^ *[_A-Za-z0-9]+ *:?=" expr))
       (setq expr (replace-regexp-in-string "^" "ii:= " (my-get-curr-line))))
   ;replace = by := 
   (setq expr (replace-regexp-in-string "\\(^ *[_A-Za-z0-9]+ *\\)=" "\\1:= " expr))
   (move-beginning-of-line 1)
   (kill-line)
   (insert (format "$%s =>  $\n" expr))
   (previous-line 1)
   (forward-char 2))
  
(defun my-calc-post ()
  "Remove calc delimiters"
  ;remove "$" around expression
  (interactive)
  (if (not my-calc-examples-p)
     (progn
       (re-search-backward "\\$" nil t)
       (replace-match "")
       (re-search-forward "\\$" nil t)
       (replace-match ""))
     (progn 
       (move-beginning-of-line 1)
       (insert (format "%-34s   "  my-calc-org_expr))))
  (next-line 1)
  (my-delete-line-if-empty)
  (my-delete-line-if-empty))

(defun my-calc-commands ()
  (interactive)
  (setq expr (split-string (replace-regexp-in-string "^% *" ""  (my-get-curr-line))))
  (let ((x (car expr)))
       (cond ((equal x "?") (progn (describe-function 'my-calc))
                                (newline))
             ((equal x "fix") (progn (move-beginning-of-line nil)
                                (kill-line)
                                (insert (format "%% [calc-mode: float-format: (fix %s)]" (cadr expr)))
                                (newline)))
             ((equal x "example") (progn (move-beginning-of-line nil)
                                (setq my-calc-examples-p (cadr expr))                                
                                (move-end-of-line 1)
                                (newline)))
             ((equal x "deg") (progn (message "switching to degrees mode")
                                (calc-degrees-mode 1)
                                (move-end-of-line 1)
                                (insert " : angles measured in degrees.")
                                (newline)))
             ((equal x  "rad") (progn (message "switching to radians mode")
                                (calc-radians-mode)
                                (insert " : angles measured in radians.")
                                (newline)))
             (t (progn (describe-function 'my-calc))
                                (newline))))
)

(defun my-calc-eval ()
  (interactive)
   (if (not my-calc-active)
     (progn 
       (calc-embedded-activate)
       (setq my-calc-active t)))
   (setq expr (my-get-curr-line))
   (if (or (string-match-p "^%" expr) (string-match-p "^ *$" expr))
       (my-calc-commands)
       (my-calc-pre)
       (calc-embedded-update-formula nil)
       (my-calc-post)))

(defun my-calc ()
  "Calculator (simplified embedded calc)

Type an expresssion and press \\[my-calc].

Examples:

input:                               result:
------                               -------  

(3+5)*10/2                           ii := (3 + 5) 10 / 2 => 40  
2.3/(20.1+2.9)                       ii := 2.3 / (20.1 + 2.9) => 0.1  
4^2                                  ii := 4^2 => 16  
2^3                                  ii := 2^3 => 8  
8^(1/3)                              ii := 8^(1/3) => 2.  
sqrt(16)                             ii := sqrt(16) => 4  
pi                                   ii := pi => 3.14159265359  
e                                    ii := e => 2.71828182846  
ln(e)                                ii := ln(e) => 1.  
log10(100)                           ii := log10(100) => 2  
log(4^3,4)                           ii := log(4^3, 4) => 3  
sin(90)                              ii := sin(90) => 1  
cos(60)                              ii := cos(60) => 0.5  
tan(45)                              ii := tan(45) => 1.  
deg(pi/4)                            ii := deg(pi / 4) => 45.  
rad(180)                             ii := rad(180) => 3.14159265359  
arcsin(0.5)                          ii := arcsin(0.5) => 30.  

;;; assignments:
a=5                                  a := 5 => 5
b=10                                 b := 10 => 10
c=a*b                                c := a b => 50

;; operate on result of previous operation (variable ii):
18+2                                 ii := 20 
ii*10                                ii := ii 10 => 200

;; solving equations:
f=4x+1=0                             f := 4 x + 1 = 0 => -4 x = 1  
solve(f,x)                           ii := solve(f, x) => x = -0.25  
solve(x^2 - 3 x + 1 = 0, x)          ii := solve(x^2 - 3 x + 1 = 0, x) => x = 2.61803398875  

;; system of equations:
f1=4x+y=0                            f1 := 4 x + y = 0 => 4 x + y = 0  
f2=x-y=10                            f2 := x - y = 10 => x - y = 10  
solve([f1,f2],[x,y])                 ii := solve([f1, f2], [x, y]) => [x = 2., y = -8.]  

;; complex numbers. (a,b)= a + i*b. polar notation: (r; arg)
c1=(3,4)                             c1 := (3, 4) => (3, 4)  
abs(c1)                              ii := abs(c1) => 5  
arg(c1)                              ii := arg(c1) => 53.1301023542  
re(c1)                               ii := re(c1) => 3  
im(c1)                               ii := im(c1) => 4  
c1*(2,1)                             ii := c1*(2, 1) => (2, 11)  
polar((3,4))                         ii := polar((3, 4)) => (5; 53.1301023542)  
cpolar=(4;60)                        ii := cpolar := (4; 60) => (4; 60)  
rect(cpolar)                         ii := rect(cpolar) => (2., 3.46410161514)  

;; vectors, statistics
v=[1,3,4,100]                        v := [1, 3, 4, 100] => [1, 3, 4, 100]  
v_1+v_3                              ii := v_1 + v_3 => 5  
vmean(v)                             ii := vmean(v) => 27  
vmedian(v)                           ii := vmedian(v) => 3.5  
vsdev(v)                             ii := vsdev(v) => 48.6826457786  


;; calculus
f=3x^2 + 2x +1                       f := 3 x^2 + 2 x + 1 => 3 x^2 + 2 x + 1  
deriv(f,x)                           ii := deriv(f, x) => 6 x + 2  
deriv(f,x,2)                         ii := deriv(f, x, 2) => 14  
deriv(f(2x), x)                      ii := deriv(f(2 x), x) => 2 f'(2 x)  
integ(3x^2+1,x)                      ii := integ(3 x^2 + 1, x) => x^3 + x  
vsum(v)                              ii := vsum(v) => 108  

;; date, time
d=now()                              d := now() => <8:35:51pm Mon Jun 26, 2017>  
d+2*7                                ii := d + 2 7 => <8:35:51pm Mon Jul 10, 2017>  
time(d)                              ii := time(d) => 20@ 35' 51\"  
d+10@ 30'                            ii := d + 10@ 30' 0\" => <7:05:51am Tue Jun 27, 2017>  

;; misc commands:
% ?     - this help
% fix 3 - number of digits in floating point numbers will be 3 (can be set to any number)
% deg   - angles will be measured in degrees
% rad   - angles will be measured in radians

Info node `(Calc)'
Info node `(Calc) Function Index'

"
  (interactive)
  (my-calc-eval))

(provide 'my-calc)

;;; my-calc.el ends here
