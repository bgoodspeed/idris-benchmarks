# idris-benchmarks
Benchmark programs for Idris

Based on the computer language benchmarks game:
http://benchmarksgame.alioth.debian.org/


Notes on porting Haskell code to Idris


Imports:

* importing particular functions via import Module(f1, f2) is not available, you can use access/export modifiers in your modules though. (private, public)

Declarations

* generally declarations have to be explicitly typed
* to use multiple functions that refer to each other you need to place them in a mutual block
* one declaration per line, ie x,y : Int is not valid
* data type names and their constructors cannot have the same name the way they can in Haskell, ie data Foo = Foo s | ... is not valid

Conversion and Type inference

* some type inference is done for helper functions in where clauses, but it less able to determine the appropriate types than haskell's
* types are not automatically converted (int, integer, nat etc) must call cast explicitly after registering the appropriate type

Record Types

* in haskell you can update a record, r, using r { thingToChange = newValue },  in idris use record { thingToChange = newValue } r


Proofs

* some standard haskell functions are possibly unsafe (like init, etc).  to use them you need to provide proofs that they are safe, or use an unsafe, potentially non-total version

Annotations

* no default datatype declaration for implicit typing (ie. default Integer ...)

Dependent Types

* because dep types can be predicated on values, things declared at the top level (like p : Bool p = True then using p as a placeholder like List p will tell the system you mean List True not list of stg)
* type Foo = Bar in haskell is Foo : Type Foo = Bar because types are values


Argument handling

* getArgs returns an array, which always has at least the program name in it
* there is no read call
* need to explicitly cast string arguments to numeric types (cast x), see :doc Cast to see what is available

Pattern Matching

* in haskell arbitrary patterns can be extended using: | n > 2 = ..., to do similar things in idris you need an explicit call to "with (newExprToMatch foo)"
* matching strings is not well supported, often with (unpack str) ... | [] = foo type constructs can emulate this, sometimes StrM if you want to match on a cons-cell like list
* matching against comparisons (ie the Ord class in haskell) is done against LT, GT, EQ etc
* can't match on 0::xs then x::xs, it says the pattern has been exhausted

Syntax

* :: and : are swapped.  :: is for concat, : is for types
* where clauses in haskell are often used as suffix-based let blocks.  ie f(x) where x = y in haskell would be let x = y in f(x)
* where clauses that define methods often (always?) require an explicit type signature
* placeholder/unused types in declarations can't be omitted entirely, ie for tuples (,) is not valid, but (_,_) would be
* no syntax ~ to force pattern matching, might not be necessary due to eager/strictness
* a ^ b is pow a b, and requires Nat type for b (ie strictly positive, whole number)
* implicit variable/type hints are given in {}'s, ala {a : Type} -> ...
* prefix is a reserved word (as is infixr etc)
* lambda expressions \ x -> f(x)  is \x => f(x)  (note spacing and =>)
* Idris does not like [a] -> ... in type signatures, due to overloading between list and vector, be explicit with List a -> ...


Totality

* in idris we can demand that functions be total and this will be checked.  %default total or just total.  for codata it checks it is productive.  makes sure pattern matching is exhaustive
Lazy/Eager

* force and seq are not really needed
* !Type strictness annotations are not needed
* further confusion with !term as "idiom brackets" which is syntactic sugar for unpacking monad values

Streams (infinite lists) vs Lists

* some monad comprehensions are available, but not all, for example [x| x<-[0..], x `mod` 2 == 0] is not ok because streams are not alternatives, we need to guarantee productivness for all codata types
* streams are lists without an empty constructor and no bounds
* list access that might be out of bounds ie (!!) in haskell, is not often available without a proof the list is long enough

* :t \naturals : Stream Nat => [ 2 * n | n <- naturals ]
* \naturals => naturals >>= (\n3 => return (fromInteger 2 * n3)) : Stream Nat -> Stream Nat
* "Also, the Monad Stream instance is like the Vect one, it zips, since appending makes no sense with infinite Streams." Melvar on #idris on freenode
* 


*  [ 2*n | n <- naturals, True ] becomes
* do n <- naturals; guard True; pure (2*n)
* where guard : Alternative f => Bool -> f ()
* stream is not an alternative instance, so it doesn't work
* stream filtering can't be total "since the stream may hold finitely many elements for which the predicate holds"
* :t\ns : List Nat => [ 2 * n | n <- ns, ((mod n 2) == 0) ] 
*\ns =>
*        ns >>=
*            \n =>
*                     guard (mod n (fromInteger 2) == fromInteger 0) >>=
*                           \{bindx0} => return (fromInteger 2 * n) : List Nat -> List Nat
* guarded list/monad comprehensions require instances of Alternative (which requires a notion of empty/bottom, streams lack this)

* takeWhile is not likely to exist or be ported, takeWhile is either a finite or infinite list, (which in haskell is fine), here we must know ahead of time which it will be.
* iterate will not exist on list, though iternateN can be created (see utils)

Errors/Exceptions

* there is an error/abort function but it is in the Debug.Error package and not the prelude
* exceptions are modelled as Effects


Library stuff

* printf is not in the standard library, though Brian McKenna built an idris version
* Bits and Low level data types are handled quite differently
* see https://github.com/bgoodspeed/idris-misc for a handful of operations useful in porting from haskell
* concurrency - not sure what level of support exists
* no Array class
* no fractional and thus no rational class, this means no quotRem (use div and mod)
* no random library
* Bounded library behaves differently
* deriving does not exist
* signum is not on the Num class (though it does exist in another typeclass)
* newtype does not exist, not sure what it does
* overriding/customizing Show is done differently (showsPrec ?)

General notes

* use the haskell interactive repl ghci or hugs to give you type hints where inference is used and the type signature is not clear
* make sure things are declared before they are called (unless in mutual blocks)

