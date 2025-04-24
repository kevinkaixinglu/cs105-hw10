;; Starter code for SmallTalk assignement.
;; Author: Richard Townsend 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;             Exercise 1 classes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(class Natural
   [subclass-of Magnitude]

   (class-method base () 2) ; private

   (class-method fromSmall: (anInteger) 
      ((anInteger = 0) ifTrue:ifFalse: 
         {(NatZero new)}
         {(self first:rest:(anInteger mod: (self base)) (self fromSmall: (anInteger div: (self base))))}
         ))
   
   (class-method first:rest: (digit rest)
    (((digit = 0) & (rest isZero)) ifTrue:ifFalse:
      {(NatZero new)}
      {((NatNonzero new) first:rest: digit rest)}))
      
   


   ; private methods suggested from textbook (page 672)
   (method modBase () (self subclassResponsibility)) 
   (method divBase () (self subclassResponsibility)) 
   (method timesBase () (self subclassResponsibility)) 
   (method compare:withLt:withEq:withGt: (aNatural ltBlock eqBlock gtBlock) 
      (self subclassResponsibility)) 
   (method plus:carry: (aNatural c) (self subclassResponsibility)) 
   (method minus:borrow: (aNatural c) (self subclassResponsibility)) 
   
   (method timesDigit:plus: (d r) (self subclassResponsibility)) ; private

   (method = (aNatural) (self compare:withLt:withEq:withGt: aNatural 
                           {false} 
                           {true}  
                           {false}))
   (method < (aNatural) (self compare:withLt:withEq:withGt: aNatural 
                           {true}   
                           {false}  
                           {false}))

   (method + (aNatural) (self plus:carry: aNatural 0))
   (method * (aNatural) (self subclassResponsibility))
   (method subtract:withDifference:ifNegative: (aNatural diffBlock exnBlock)
      ((self < aNatural) ifTrue:ifFalse:
       {(exnBlock value)} 
       {(diffBlock value:(self minus:borrow: aNatural 0))}))

   (method sdivmod:with: (n aBlock) (self subclassResponsibility))

   (method decimal () [locals digitList newQ] 
      (set digitList (List new))
      (set newQ self)
      ({(newQ isZero)} whileFalse:
         { 
            (newQ sdivmod:with: 10
               [block (Q r)
               (digitList addFirst: r)
               (set newQ Q)])
         })
      ((digitList isEmpty) ifTrue:
      {(digitList addFirst: 0)}) digitList)

   (method isZero  () (self NatZero))


   ; methods that are already implemented for you
   (method - (aNatural)
      (self subtract:withDifference:ifNegative:
            aNatural
            [block (x) x]
            {(self error: 'Natural-subtraction-went-negative)}))
   (method sdiv: (n) (self sdivmod:with: n [block (q r) q]))
   (method smod: (n) (self sdivmod:with: n [block (q r) r]))
   (method print () ((self decimal) do: [block (x) (x print)]))

   ;private methods for testing
   (method validated ()
    ((self invariant) ifFalse:
      {(self printrep)
       (self error: 'invariant-violation)})
    self)
   (method compare-symbol: (aNat)
    (self compare:withLt:withEq:withGt: aNat {'LT} {'EQ} {'GT}))
 )


; Represents a 0 natural number
(class NatZero
  [subclass-of Natural]

  (method invariant () true) 

  (method timesDigit:plus: (d r) (Natural fromSmall: r)) ; private

  ;; for debugging
  (method printrep () (0 print))
  (method isZero () true)
  (method divBase () (NatZero new)) 
  (method modBase () 0) 
  (method timesBase () (NatZero new)) 

  (method * (aNatural) (NatZero new))

  (method sdivmod:with: (d b)
      (b value:value: (NatZero new) 0))

  (method compare:withLt:withEq:withGt:
          (aNatural ltBlock eqBlock gtBlock)
    ((aNatural isZero) ifTrue:ifFalse:
      {(eqBlock value)}              
      {(ltBlock value)}))

  (method plus:carry: (aNatural c)
    ((aNatural isZero) ifTrue:ifFalse:
      {(Natural fromSmall: c)}         
      {(aNatural plus:carry: self c)}))

  (method minus:borrow: (aNatural c)
    (((aNatural isZero) & (c = 0)) ifTrue:ifFalse:
      {(NatZero new)}  
      {(self error: 'Natural-subtraction-went-negative)}))
  
)

; Represents a natural number greater than 0
(class NatNonzero
  [subclass-of Natural]
  [ivars m d] ; a non-zero natural number is of the form d + m * b, where d
              ; is an integer representing a digit of base b, and m is a natural
              ; number
  (method divBase () m)
  (method modBase () d)
  (method timesBase () (Natural first:rest: 0 self))
  
   (method first:rest: (digit rest)
      (set d digit)
      (set m rest)
      self)

  (method invariant () (((d < (Natural base)) & (d >= 0)) &  ;; private
                       (((m isZero) & (d = 0)) not)))

  ; addition with a carry bit
  (method plus:carry: (aNatural c) [locals sum least cout]
     (set sum ((d + (aNatural modBase)) + c))
     (set least (sum mod: (Natural base)))
     (set cout  (sum div: (Natural base)))
     (NatNonzero first:rest: least (m plus:carry: (aNatural divBase) cout)))
      
  ; subtraction with a borrow bit
  (method minus:borrow: (aNatural b) [locals diff least bout]
     (set diff (d - ((aNatural modBase) + b)))
     ((diff < 0) ifTrue:ifFalse:
         {(set diff (diff + (Natural base)))
          (set bout 1)}
         {(set bout 0)})
     (NatNonzero first:rest: diff (m minus:borrow: (aNatural divBase) bout)))

  ; multiplication
  (method * (aNatural) [locals d1 d2 m1 m2]
     ; simple method; fastest; based on this law:
     ;   (d + b * m) * n == (d * n) + b * (m * n)
     ((aNatural timesDigit:plus: d 0) + ((m * aNatural) timesBase)))

  (method timesDigit:plus: (dig r) ; private, answers self * d + r
      [locals pp]
      (set pp ((d * dig) + r))
      (NatNonzero first:rest: (pp mod: (Natural base))
                  (m timesDigit:plus: dig (pp div: (Natural base)))))
  
  ;; debugging method
  (method printrep () (m printrep) (', print) (d print))

  (method isZero () false)

  (method sdivmod:with: (divisor cont)
    [locals restQ restR]
    (m sdivmod:with: divisor
      [block (restQ restR)
        (cont 
          value:value:
            ((restQ timesBase)
             + (Natural fromSmall: (((restR * (Natural base)) + d) div: divisor)))
            (((restR * (Natural base)) + d) mod: divisor)) ]))


  (method compare:withLt:withEq:withGt: (aNatural ltBlock eqBlock gtBlock)
    [locals yRest yDigit]
    (set yRest  (aNatural divBase))
    (set yDigit (aNatural modBase))

    (m compare:withLt:withEq:withGt: yRest ltBlock
      { 
        ((d < yDigit) ifTrue:ifFalse:
          { (ltBlock value) }                 
          { ((d = yDigit) ifTrue:ifFalse:
              {(eqBlock value)}             
              {(gtBlock value)})})         
      }
      gtBlock))

)

; For testing naturals
(class DebugNat
  [subclass-of Object]
  [ivars nat] ; a natural number
  (class-method of: (aNat) ((self new) init: aNat))
  (method init: (n) (set nat n) self) ; private
  (method print () (nat printrep))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Put your unit tests for Exercise 1 here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (check-assert ((Natural fromSmall: 0) isZero)) 

      (check-assert (((Natural fromSmall: 1) isZero) not))

      (check-print (DebugNat of: (Natural fromSmall: 5)) 0,1,0,1)

      (check-print (DebugNat of: (Natural fromSmall: 0))
                  0) 

      (check-print (DebugNat of: 
                     (Natural fromSmall: ((Natural base) * (Natural base))))
                  0,1,0,0) 

      (check-print (DebugNat of: (Natural fromSmall: 1))
                  0,1)

      (check-print (DebugNat of: (Natural fromSmall: 1000))
                  0,1,1,1,1,1,0,1,0,0,0) 

      (check-print (DebugNat of: (Natural fromSmall: 1234))
                  0,1,0,0,1,1,0,1,0,0,1,0) 

      (check-print (DebugNat of: (Natural fromSmall: 4096))
                  0,1,0,0,0,0,0,0,0,0,0,0,0,0) 

      (check-assert (3 isKindOf: SmallInteger))

      (check-assert (0 isKindOf: SmallInteger))

      (check-assert (((NatZero new) divBase) isZero))

      (check-assert (((NatZero new) modBase) isZero))

      (check-assert (((NatZero new) timesBase) isZero))

      (check-assert (((Natural fromSmall: 1) divBase) isZero))

      (check-assert (((Natural fromSmall: 1) modBase) = 1))

      (check-assert (((Natural fromSmall: 1) modBase) isKindOf: SmallInteger))

      (check-assert ((((Natural fromSmall: 1) timesBase) modBase) = 0))

      (check-assert ((((Natural fromSmall: 1) timesBase) isZero) = false))

      (check-assert (((Natural fromSmall: 1) timesBase) isKindOf: NatNonzero))

      (check-assert (((NatZero new) * (NatZero new)) isZero))

      (check-assert (((NatZero new) * (Natural fromSmall: 7)) isZero))

      (check-assert (((NatZero new) * (Natural fromSmall: 7)) 
                        isKindOf: NatZero))
         
      (check-assert ((NatZero new) sdivmod:with: 13 
                        [block (q r) ((q isZero) & (r = 0))]))

      (check-assert (((NatZero new) compare-symbol: (NatZero new)) = 'EQ))

      (check-assert (((NatZero new) compare-symbol: 
                        (Natural fromSmall: 5)) = 'LT))

      (check-assert (((NatZero new) minus:borrow: (NatZero new) 0) isZero))

      (check-error ((NatZero new) minus:borrow: (NatZero new) 1))

      (check-error ((NatZero new) minus:borrow: (Natural fromSmall: 2) 0))

      (check-assert (((NatZero new) plus:carry: (NatZero new) 0) isZero))

      (check-assert ((((NatZero new) plus:carry: (NatZero new) 1) modBase) = 1))

      (check-assert ((((Natural fromSmall: 5) plus:carry: 
                        (Natural fromSmall: 0) 1) modBase) = 0))
      (check-assert ((((Natural fromSmall: 5) plus:carry: 
                        (Natural fromSmall: 0) 1) modBase) = 0))

      (check-assert (((((Natural fromSmall: 5) plus:carry:
                           (Natural fromSmall: 0) 1) divBase) modBase) = 1))
      (check-assert (((((Natural fromSmall: 5) plus:carry: 
                           (Natural fromSmall: 0) 1) divBase) modBase) = 1))

      (check-assert ((((Natural fromSmall: 3) + (Natural fromSmall: 4)) 
                        modBase) = 1))      

      (check-assert (((((Natural fromSmall: 3) + (Natural fromSmall: 4)) divBase) modBase) = 1))          

      (check-assert ((((Natural fromSmall: 3) + (Natural fromSmall: 4)) isZero) not))

      (check-assert (((Natural fromSmall: 3) + (Natural fromSmall: 4)) isKindOf: NatNonzero))

      (check-assert ((((Natural fromSmall: 3) + (Natural fromSmall: 4)) modBase)
                      = (((Natural fromSmall: 4) + (Natural fromSmall: 3)) 
                           modBase))) 

      (check-assert (((((Natural fromSmall: 3) + (Natural fromSmall: 4)) 
                        divBase) modBase) = ((((Natural fromSmall: 4) + 
                                                 (Natural fromSmall: 3)) 
                                                 divBase) modBase)))

      (check-print (DebugNat of: ((Natural fromSmall: 5) sdiv: 2)) 0,1,0) 

      (check-assert (((Natural fromSmall: 5) smod: 2) = 1))

      (check-print (DebugNat of: ((Natural fromSmall: 37) sdiv: 2)) 
         0,1,0,0,1,0) 

      (check-assert (((Natural fromSmall: 37) smod: 2) = 1))

      (check-print (DebugNat of: ((Natural fromSmall: 7) sdiv: 1)) 0,1,1,1) 

      (check-assert (((Natural fromSmall: 7) smod: 1) = 0))

      ;; Decimal Tests

      (check-print (Natural fromSmall: 0) 0)

      (check-print (Natural fromSmall: 1) 1)

      (check-print (Natural fromSmall: 5) 5)

      (check-print (Natural fromSmall: ((Natural base) * (Natural base))) 4)

      (check-print (Natural fromSmall: 1000) 1000)

      (check-print (Natural fromSmall: 1234) 1234)

      (check-print (Natural fromSmall: 4096) 4096)

      (check-print ((Natural fromSmall: 5) plus:carry: (Natural fromSmall: 0) 1)
         6)

      (check-print ((Natural fromSmall: 5) plus:carry: (Natural fromSmall: 1) 0)
         6)

      (check-print ((Natural fromSmall: 3) + (Natural fromSmall: 4)) 7)

      (check-print ((Natural fromSmall: 4) + (Natural fromSmall: 3)) 7)

      ;; Compare tests

      (check-assert ((NatZero new) = (NatZero new)))

      (check-assert ((NatZero new) < (Natural fromSmall: 7)))

      (check-assert ((Natural fromSmall: 3) < (Natural fromSmall: 6)))

      (check-assert ((Natural fromSmall: 9) > (Natural fromSmall: 2)))

      (check-assert ((Natural fromSmall: 14) = (Natural fromSmall: 14)))

      (check-assert ((Natural fromSmall: 10) < (Natural fromSmall: 11)))

      (check-assert ((Natural fromSmall: 11) > (Natural fromSmall: 10)))

      ;; Subtraction tests

      (check-print ((Natural fromSmall: 10) - (Natural fromSmall: 5)) 5)

      (check-print ((Natural fromSmall: 2) - (Natural fromSmall: 2)) 0)

      (check-error ((Natural fromSmall: 2) - (Natural fromSmall: 234)))

      ;; Multiplication and large tests

      (Natural addSelector:withMethod: 'squared
      (compiled-method () (self * self)))
      (Natural addSelector:withMethod: 'coerce:
      (compiled-method (i) (Natural fromSmall: i)))
      (Natural addSelector:withMethod: 'raisedToInteger:
      (Number compiledMethodAt: 'raisedToInteger:))

      (check-print ((Natural fromSmall: 10) raisedToInteger: 10) 10000000000)

      (check-print ((Natural fromSmall:  9) raisedToInteger:  9)   387420489)

      



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;             Exercise 2 classes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(class LargeInteger
  [subclass-of Integer]
  [ivars magnitude]

  (class-method withMagnitude: (aNatural)
      ((self new) magnitude: aNatural))
  (method magnitude: (aNatural) ; private, for initialization
      (set magnitude aNatural)
      self)

  (method magnitude () magnitude)

  (class-method fromSmall: (anInteger)
     ((anInteger isNegative) ifTrue:ifFalse: 
        {(((self fromSmall: 1) + (self fromSmall: ((anInteger + 1) negated)))
          negated)}
        {((LargePositiveInteger new) magnitude: 
                 (Natural fromSmall: anInteger))}))
  (method isZero () (magnitude isZero))
  (method = (anInteger) ((self - anInteger)     isZero))
  (method < (anInteger) ((self - anInteger) isNegative))

  (method div: (n) (self sdiv: n))
  (method mod: (n) (self smod: n))

  (method sdiv: (n) (self subclassResponsibility))
  (method smod: (n) (self - ((LargeInteger fromSmall: n) * (self sdiv: n))))
)



; Represents a positive integer
(class LargePositiveInteger
  [subclass-of LargeInteger]

  (method print () ((magnitude decimal) do: [block (x) (x print)]))

  (method isNegative () false)
  (method isNonnegative () true)
  (method isStrictlyPositive () ((magnitude isZero) not))

  (method negated ()
  ((magnitude isZero) ifTrue:ifFalse:
    {self}   
    {(LargeNegativeInteger withMagnitude: magnitude)}))

  ;; short division (already implemented for you)
  (method sdiv: (anInteger)
    ((anInteger isStrictlyPositive) ifTrue:ifFalse: 
       {(LargePositiveInteger withMagnitude:  (magnitude sdiv: anInteger))}
       {((((self - (LargeInteger fromSmall: anInteger)) -
                                                  (LargeInteger fromSmall: 1))
             sdiv: (anInteger negated))
            negated)}))

  (method * (otherInteger)
    (otherInteger multiplyByLargePositiveInteger: self))

  (method multiplyByLargePositiveInteger: (positiveOperand)
    (LargePositiveInteger 
      withMagnitude: 
      ((self magnitude) * (positiveOperand magnitude))))

  (method multiplyByLargeNegativeInteger: (negativeOperand)
    (LargeNegativeInteger 
      withMagnitude: 
      ((self magnitude) * (negativeOperand magnitude))))

)

;; Represents a negative integer
(class LargeNegativeInteger
  [subclass-of LargeInteger]

  (method print ()
    ('- print)
    ((magnitude decimal) do: [block (x) (x print)]))

  (method isNegative () true)
  (method isNonnegative () false)
  (method isStrictlyPositive () false)
  (method negated () (LargePositiveInteger withMagnitude: magnitude))


  ;; short division (already implemented for you)
  (method sdiv: (anInteger)
    ((self negated) sdiv: (anInteger negated)))

  (method * (otherInteger)
    (otherInteger multiplyByLargeNegativeInteger: self))

  (method multiplyByLargePositiveInteger: (positiveOperand)
    (LargeNegativeInteger 
      withMagnitude: 
      ((self magnitude) * (positiveOperand magnitude))))

  (method multiplyByLargeNegativeInteger: (negativeOperand)
    (LargePositiveInteger 
      withMagnitude: 
      ((self magnitude) * (negativeOperand magnitude))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Put your unit tests for Exercise 2 here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Test negation and printing

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Put your unit tests for Exercise 2 here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        (check-assert (((LargeNegativeInteger withMagnitude: (Natural fromSmall: 1)) isNegative) = true))
        (check-assert (((LargePositiveInteger withMagnitude: (Natural fromSmall: 2)) isNegative) = false))
        (check-assert (((LargePositiveInteger withMagnitude: (Natural fromSmall: 3)) isNonnegative) = true))
        (check-assert (((LargeNegativeInteger withMagnitude: (Natural fromSmall: 4)) isNonnegative) = false))
        (check-assert (((LargePositiveInteger withMagnitude: (Natural fromSmall: 5)) isStrictlyPositive) = true))
        (check-assert (((LargeNegativeInteger withMagnitude: (Natural fromSmall: 0)) isStrictlyPositive) = false))

        (check-assert ((((LargePositiveInteger withMagnitude: (Natural fromSmall: 5)) negated) isNegative) = true))
        (check-assert ((((LargeNegativeInteger withMagnitude: (Natural fromSmall: 5)) negated) isNegative) = false))
        (check-assert ((((LargePositiveInteger withMagnitude: (Natural fromSmall: 5)) negated) isNonnegative) = false))
        (check-assert ((((LargeNegativeInteger withMagnitude: (Natural fromSmall: 5)) negated) isNonnegative) = true))
        (check-print (LargePositiveInteger withMagnitude: (Natural fromSmall: 5)) 5)
        (check-print (LargeNegativeInteger withMagnitude: (Natural fromSmall: 5)) -5)
        (check-print ((LargePositiveInteger withMagnitude: (Natural fromSmall: 5)) negated) -5)
        (check-print ((LargeNegativeInteger withMagnitude: (Natural fromSmall: 5)) negated) 5)
        (check-print ((LargeInteger fromSmall: 0) negated) 0)

        ;;DIDNT KNOW HOW TO DEFINE FOUR AND NEGFOUR HERE help figure it out 

        (check-print (four * four)     16) 
        (check-print (four * negFour) -16) 
        (check-print (negFour * four) -16) 
        (check-print (negFour * negFour) 16)

        echo '(use bignum.smt) ({(four * four)} messageTrace)' | usmalltalk -qq | grep multiplyByLarge
        echo '(use bignum.smt) ({(four * negFour)} messageTrace)' | usmalltalk -qq | grep multiplyByLarge
        echo '(use bignum.smt) ({(negFour * four)} messageTrace)' | usmalltalk -qq | grep multiplyByLarge
        echo '(use bignum.smt) ({(negFour * negFour)} messageTrace)' | usmalltalk -qq | grep multiplyByLarge
