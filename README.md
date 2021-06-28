# Maejiri: LF type checker written in Haskell.

### Quick Start
```
$ git clone http://github.com/kwanghoon/maejiri.git
$ cd maejiri
$ ghc --make Main.hs
$ ./Main FOL.lf
MaeJi> :t forall_e
PIA F:PIA $d:indi.prop.PIA x:indi.PIA $d:(true (forall Lam x':indi.(F x))).(true (F x))
MaeJi> :t impl (forall (\x:indi.equal x x)) (exists (\x:indi.equal x x))
prop
MaeJi> :q
$ ./Main -check FOL.lf HOL.lf STLC.lf
MaeJi LF Explorer (Ver. 0.1)
FOL.lf:
	Parsing...
	Checking...
HOL.lf:
	Parsing...
	Checking...
STLC.lf:
	Parsing...
	Checking...
$
```

### Examples
 - FOL.lf: the first-order logic
 - HOL.lf: the higher-order logic
 - HOARE.lf: Hoare logic
 - Presburger.lf: The theory of Presburger Arithmetic (Proof of commutativity and associativity of addition)
 - STLC.lf: The simply-typed lambda calculus
 - Vector.lf: Vectors

### Todo
 - More LF examples
 - Error reporting in typechecking
 - Basic libraries such as integer, boolean, bit, word, list, etc.
 - Supporting modules by qualified names
 - Supporting decision procedures
