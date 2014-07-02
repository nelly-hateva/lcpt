;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname church) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;Church Numerals

(define (c n)
  (lambda (f)
    (lambda (x)
      (if (= n 0)
        x
        (f (((c (- n 1)) f) x))
       )
    )
  )
)

(define (cton c)
  ((c (lambda (x) (+ x 1))) 0)
)

(define succ
  (lambda (n)
    (lambda (f)
      (lambda (x)
        (f ((n f) x))
      )
    )
  )
)
(define plus
  (lambda (n)
    (lambda (m)
      (lambda (f)
        (lambda (x)
          ((m f) ((n f) x))
        )
      )
    )
  )
)
(define mult
  (lambda (n)
    (lambda (m)
      (lambda (f)
        (lambda (x)
          ((n (m f)) x)
        )
      )
    )
  )
)
(define pow
  (lambda (n)
    (lambda (m)
      (lambda (f)
        (lambda (x)
          (((m n) f) x)
        )
      )
    )
  )
)

(define ifthenelse
  (lambda (x) x)
)
(define lc_true
  (lambda (x)
    (lambda (y)
      x
    )
  )
)
(define lc_false
  (lambda (x)
    (lambda (y)
      y
    )
  )
)

(define iszero
  (lambda (n)
    ((n (lambda (x) lc_false)) lc_true)
   )
)

(define (printbool b)
  ((b "True") "False")
)

(define lc_and
  (lambda (x)
    (lambda (y)
      ((x y) lc_false)
    )
  )
)
(define lc_or
  (lambda (x)
    (lambda (y)
      ((x lc_true) y)
    )
  )
)
(define lc_not
  (lambda (x)
    ((x lc_false) lc_true)
  )
)

(define lc_cons
  (lambda (x)
    (lambda (y)
      (lambda (b)
        ((b x) y)
      )
    )
  )
)
(define lc_car
  (lambda (z)
    (z lc_true)
  )
)
(define lc_cdr
  (lambda (z)
    (z lc_false)
  )
)

(define (printpair z)
  (cons (cton (lc_car z)) (cons (cton (lc_cdr z)) '()))
)

(define pred
  (lambda (n)
    (lc_cdr ((n (lambda (z) ((lc_cons (succ (lc_car z))) (lc_car z)))) ((lc_cons (c 0)) (c 0))))
  )
)

(define minus
  (lambda (n)
    (lambda (m)
      ((m pred) n)
    )
  )
)

(define eq
  (lambda (m)
    (lambda (n)
      ((lc_and (iszero ((minus m) n))) (iszero ((minus n) m)))
    )
  )
)

(define lt
  (lambda (m)
    (lambda (n)
      ((lc_and (iszero ((minus m) n))) (lc_not ((eq m) n)))
    )
  )
)
