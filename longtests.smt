(check-print ((Natural fromSmall: 99) raisedToInteger: 99)   
  ;; result broken into multiple lines for readability; they must be rejoined
  369729637649726772657187905628805440595668764281741102430259972423552570455277523421410650010128232727940978889548326540119429996769494359451621570193644014418071060667659301384999779999159200499899)
;exercise 1
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

      
;exercise 2
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
      (val four (LargePositiveInteger withMagnitude: (Natural fromSmall: 4)))
      (val negFour (LargeNegativeInteger withMagnitude: (Natural fromSmall: 4)))

      (check-print (four * four)     16) 
      (check-print (four * negFour) -16) 
      (check-print (negFour * four) -16) 
      (check-print (negFour * negFour) 16)

      (check-print ((LargePositiveInteger withMagnitude: (Natural fromSmall: 5)) 
              + (LargePositiveInteger withMagnitude: (Natural fromSmall: 3))) 8)

      (check-print ((LargePositiveInteger withMagnitude: (Natural fromSmall: 5)) 
                    + (LargeNegativeInteger withMagnitude: (Natural fromSmall: 3))) 2)

      (check-print ((LargePositiveInteger withMagnitude: (Natural fromSmall: 3)) 
                    + (LargeNegativeInteger withMagnitude: (Natural fromSmall: 5))) -2)

      (check-print ((LargeNegativeInteger withMagnitude: (Natural fromSmall: 3)) 
                    + (LargePositiveInteger withMagnitude: (Natural fromSmall: 5))) 2)

      (check-print ((LargeNegativeInteger withMagnitude: (Natural fromSmall: 5)) 
                    + (LargePositiveInteger withMagnitude: (Natural fromSmall: 3))) -2)

      (check-print ((LargeNegativeInteger withMagnitude: (Natural fromSmall: 5)) 
                    + (LargeNegativeInteger withMagnitude: (Natural fromSmall: 3))) -8)
