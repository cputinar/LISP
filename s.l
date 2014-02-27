;—————1——————
;part 1a

(defun double-recursive (y)
  (cond
    ((null y) nil); empty list return nil
  	((atom y) nil); y is not a list return nil
		(t (append (list (car y) (car y)) (double-recursive (cdr y))))
	);
);


;(print(double-recursive'(1 2 (4 5) 3)))

;part 1b

(defun double-iterative (y)
  (do
		(
			(y y (cdr y)) 
			(x nil (append x (list (car y) (car y))))
		);variable declarations, intialization, and update rules

		( (atom y) x)
	);end do
  )
  
 ; (print(double-iterative'(1 2 (4 5) 3)))

;part 1c

(defun double-mapcar (y)
  (cond
     ((atom y) nil)
    (t (apply #'append (mapcar (lambda (arg) (list arg arg)) y)))
)
)

;(print(double-mapcar '(1 2 (4 5) 3)))

;—————2————

(defun double-recursive (y)
  (cond
    ((null y) nil); empty list return nil
    ((atom y) nil); y is not a list return nil
	((atom (car y)) (append (list (car y) (car y)) (double-recursive (cdr y))))	
  (t(append(list(double-recursive (car y))) (double-recursive (cdr y))))
	);
);

;(print(double-recursive’((4 5) 1 2 (4 5) 3)))


(defun rdouble(y)
  (cond
    ((null y) nil); empty list return nil
    ((atom y) nil); y is not a list return nil
    ((numberp (car y)) (append (list (car y) (car y)) (rdouble (cdr y))))
    ((atom (car y)) (rdouble(cdr y)))
    (t (cons (rdouble(car y)) (rdouble(cdr y))))
  );
);



;—————3————



(defun my-mapcar(fun l1 l2) ;mapcar function
  (cond
    ((or (null l1) (null l2)) nil)
    (t 
      (append (list (funcall fun (car l1) ( car l2))) (my-mapcar fun (cdr l1) (cdr l2)))
    ) ;done with t
  ) ;done with cond
) ;done with my-mapcar
 

;—————4————

(defun rewrite(x)
  (cond
    ((null x) nil) ;empty list return nil
    ((numberp x)  x)
    ((atom x) x)
    ((symbolp (car x)) ;if it's a symbol 
      (cond
        ((equal (car x) 'if)  
          (cond

            ((or (numberp (cadr x)) (null (cadr x))) (cons 'cond (append(list (append(list (cadr x) (rewrite(caddr x)))))
              (cond
                ((null (cdddr x))nil)
                (t (list(cons 't (list(rewrite(cadddr x))))))
              ))))
            ((listp (cdr x)) (cons 'cond (append(list(append (list(rewrite (cadr x))) (list(rewrite (caddr x)))))
              (cond
                ((null (cdddr x))nil)
                (t (list(cons 't (list(rewrite(cadddr x))))))
              )))
            )

          )
        )
        (t 
          (cond
            ((equal 'cons (car x)) 
              (append (list(car x))  (list(rewrite (cadr x))) (list(rewrite (cddr x))))
            )
            ((and (equal 'list (car x)) (listp (cadr x))) 
              (append (list(car x))  (list(rewrite (cadr x))) (list(rewrite (cddr x)))) 
            )
            (t (append(list (car x)) (rewrite (cdr x))))
          )

        )

      )
    )
    ((numberp (car x)) 
      (cond
        ((or (numberp (cadr x)) (null (cadr x))) (append (list(car x)) (rewrite (cdr x))))
        ((listp (cadr x)) (append (list(car x))  (list(rewrite (cadr x))) (rewrite (cddr x)))
        )
      )

    )
    ((listp (car x)) (append(rewrite(car x)) (rewrite(cdr x)))) 
  )
)


;—————5————

(defun check(x)
  (list(equal 
    (eval x) 
    (eval (rewrite x))) 
  (eval x) 
  (eval (rewrite x))
  )

)
