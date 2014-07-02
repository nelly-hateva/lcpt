-- Church numerals

c 0 = \f x -> x
c n = \f x -> f (c (n-1) f x)	

cton c = c (+1) 0

s = \n f x -> f (n f x)
plus = \n m f x -> m f (n f x)
mult = \n m f x -> n (m f) x
pow = \n m f x -> (m n) f x

ifthenelse = \x -> x
true = \x y -> x
false = \x y -> y

iszero = \n -> n (\x -> false) true

printbool b = b "True" "False"

and_ = \x y -> x y false
or_ = \x y -> x true y
not_ = \x -> x false true

cons = \x y b -> b x y
car = \z -> z true
cdr = \z -> z false

printpair z = ((cton (car z)), (cton (cdr z)))

lc_pred = \n -> cdr (n (\z -> cons (s (car z)) (car z)) (cons (c 0) (c 0)))
