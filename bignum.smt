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
         {(self first:rest:(anInteger mod: (self base)) (self fromSmall: 
                (anInteger div: (self base))))}
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

  (method timesDigit:plus: (d r) (Natural fromSmall: r)) 

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
             + (Natural fromSmall: 
                (((restR * (Natural base)) + d) div: divisor)))
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
  (method magnitude: (aNatural) 
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
  

  (method + (otherInteger)
    (otherInteger addToLargePositiveInteger: self))

  (method addToLargePositiveInteger: (positiveOperand)
    (LargePositiveInteger 
      withMagnitude: 
      ((self magnitude) + (positiveOperand magnitude))))

  (method addToLargeNegativeInteger: (negativeOperand)
  (((self magnitude) < (negativeOperand magnitude)) ifTrue:ifFalse:
    {(LargeNegativeInteger 
       withMagnitude: 
       ((negativeOperand magnitude) - (self magnitude)))}
    {(LargePositiveInteger 
       withMagnitude: 
       ((self magnitude) - (negativeOperand magnitude)))})
  )
)


;; Represents a negative integer
(class LargeNegativeInteger
  [subclass-of LargeInteger]

  (method print ()
    ('- print)
    ((magnitude decimal) do: [block (x) (x print)]))

  (method isNegative () ((magnitude isZero) not))
  (method isNonnegative () (magnitude isZero))
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

  (method + (otherInteger)
    (otherInteger addToLargeNegativeInteger: self))

  (method addToLargePositiveInteger: (positiveOperand)
    (((self magnitude) < (positiveOperand magnitude)) ifTrue:ifFalse:
      {(LargePositiveInteger 
         withMagnitude: 
         ((positiveOperand magnitude) - (self magnitude)))}
      {(LargeNegativeInteger 
         withMagnitude: 
         ((self magnitude) - (positiveOperand magnitude)))}))
  
  (method addToLargeNegativeInteger: (negativeOperand)
    (LargeNegativeInteger 
      withMagnitude: 
      ((self magnitude) + (negativeOperand magnitude))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Put your unit tests for Exercise 2 here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
