(add-pvar-name "A" "B" (make-arity))

(set-goal (pf "(A -> B) ord (B -> A)"))
(use "StabLog")
(assume 1)
(use 1)
(intro 0)
(assume 2)
(use "EfqLog")
(use 1)
(intro 1)
(assume 3)
(use 2)
(cdp)