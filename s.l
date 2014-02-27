
;----------- Program 5 -------------

;--------******New Part*****----------
;-----part 5 -------
(defun check(x) ;function check(x)

  (list(equal (eval x) 
    (eval (rewrite x))) 
  (eval x) (eval 
  (rewrite x))) ;calls function rewrite x

)


;--------******New Part*****----------
;---part 4----
(defun rewrite(x)
  (cond
    ((null x) nil) ;x is null
    ((numberp x)  x) 
    ((atom x) x)
    ((symbolp (car x))  
      (cond
        ((equal 
          (car x) 'if) ;use if
          (cond
            ((or (numberp 
              (cadr x)) (null (
                cadr x))) (cons 'cond 
                    (append(list (append(list 
                        (cadr x) (rewrite(caddr x)))))
              (cond
                ((null 
                  (cdddr x))nil)
                (t (list
                  (cons 't (list
                    (rewrite(cadddr x)))))) ;calls rewrite
              ))))
            ((listp (cdr x)) 
              (cons 'cond (append
                (list (list
                  (rewrite (cadr x)) ;calls rewrite
                  (rewrite (caddr x)))) ;calls rewrite
              (cond
                ((null 
                  (cdddr x))nil)
                (t (list
                  (cons 't (list
                    (rewrite(cadddr x)))))) ;calls rewrite
              )))
            )

          )
        )
        (t 
          (cond
            ((equal 'cons 
              (car x)) 
              (append (list(car x))  
                (list(rewrite (cadr x))) 
                (list(rewrite (cddr x))))
            )
            ((and 
              (equal 'list (car x)) 
              (listp (cadr x))) 
              (append (list(car x))  
                (list(rewrite (cadr x))) (list(rewrite (cddr x)))) 
            )
            (t (append(list (car x)) 
              (rewrite (cdr x)))) ;calls rewrite
          )

        )

      )
    )
    ((numberp (car x))  ;numberp
      (cond
        ((or (numberp (cadr x)) ;or function
        (null (cadr x))) (append (list(car x)) (rewrite (cdr x))))

        ((listp (cadr x)) (append (list(car x))  
          (list(rewrite (cadr x))) (rewrite (cddr x)))
        )
      )

    )
    ((listp (car x)) 
    (append(rewrite(car x)) (
    rewrite(cdr x)))) ;calls rewrite
  )
)


;--------******New Part*****----------
;-----part 3------

(defun my-mapcar(fun l1 l2)
  (cond
    ((or (null l1) 
      (null l2)) nil)
    (t 
      (append (list 
        (funcall fun (car l1) 
          ( car l2))) 
              (my-mapcar fun (cdr l1) (cdr l2))) ;recursively calls my-mapcar
    ) 
  )

)


;--------******New Part*****----------
;---------part 2--------


(defun rdouble(y) ;function rdouble
  (cond
    ((not(listp y)) 
      nil)
    ((null y)
      nil)
    ((null (car y)) 
      (append(list (car y)) 
        (rdouble(cdr y))))
    ((numberp (car y)) 
      (append (list (car y) (car y)) 
        (rdouble (cdr y))))
    ((atom (car y)) (rdouble(cdr y)))
          (t (cons (rdouble(car y)) 
            (rdouble(cdr y)))) ;recursively calls rdouble
  )
) ;end rdouble function




;--------******New Part*****----------
;--------part 1----------


(
defun double-recursive(y)

  (cond

    ((not(listp y)) nil) ;y isn't a list

    ((null y)nil) ;y is null

    ((null (car y)) ;first element is null
      (append(list 
        (car y ) (car y)) 
      (double-recursive(cdr y))))
    ((numberp (car y)) 
      (append (list (car y) (car y)) 
        (double-recursive (cdr y)))) ;call double recursive
    ((atom (car y)) 
      (double-recursive (cdr y))) ;call double recursive
    (t (append (list (car y) (car y)) 
      (double-recursive(cdr y)))) ;call double recursive
  )
)

(defun double-iterative(y) 
  (cond
    ((not(listp y))
               nil) ;list of y 
    ((null y) 
              nil) ;y is null
    (t (let (temp))
      (do ;use do function
        ((l nil 
          (append (list (car y) 
            (car y)))))
        ((null y) 
          temp)
        (cond 
          ((null 
          (car y)) 
            (cond
              ((null l) (setq temp 
                (append (list (car y) (car y))))
                (setq y (cdr y)))
              (t (setq temp 
                (append temp l)) (setq y (cdr y)))
            )
          )
          ((numberp (car y))
            (cond
              ((null l) (setq temp 
                (append (list (car y) (car y))))
                (setq y (cdr y)))
              (t (setq temp 
                (append temp l)) (setq y (cdr y)))
            )
          )

          ((atom (car y)) (setq y (cdr y)) ) 

          (t ;
            (cond
              ((null l) (setq temp 
                (append (list (car y) (car y))))
                (setq y (cdr y)))
              (t (setq temp 
                (append temp l)) (setq y (cdr y)))
            )
          )
        )
      )
    )
  )
)

(defun double-mapcar(y)
  (cond
    ((not(listp y)) nil) ;y is not a list
    ((null y) nil) ;y is null

    ((numberp (car y))
      (apply 'append 
        (apply 'mapcar (cons 'list (list y y)))) ;call mapcar

    )
    ((atom (car y)) nil)
    (t 
      (apply 'append  
        (apply 'mapcar (cons 'list (list y y)))) ;use mapcar
    )

    
  )

)

