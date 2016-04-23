# Music DSL Prototype

This is a Scala embedded DSL for composing and transforming music, which aims
to be expressive and intuitive for non-programmers.

##Examples

Here is a use case demonstrating how to create modulations from one key to
another using function composition:

```scala
scala> import PitchClass._, RomanNum._
import PitchClass._
import RomanNum._

scala> C`_`4
res0: rc.dsl.Primitives.Pitch = C₄

scala> val cMajor = Ionian(res0)
cMajor: rc.dsl.Ionian = Ionian(C)

scala> val fSharpMajor = Ionian(F`#`4)
fSharpMajor: rc.dsl.Ionian = Ionian(F♯)

scala> fSharpMajor.to compose cMajor.from
res1: rc.dsl.Primitives.Pitch => rc.dsl.Primitives.Pitch = <function1>

scala> res1(D`#`4)
res2: rc.dsl.Primitives.Pitch = G𝄪₄
```

Another goal of this DSL is to be able to start from small ideas, like a
rhythm, and be able to build onto it. Here is an example of zipping together a
list of beats with a list of pitches to form notes:

```scala
scala> val pitches = List(C`_`4, E`_`4, G`_`4, B`_`4)
pitches: List[rc.dsl.Primitives.Pitch] = List(C₄, E₄, G₄, B₄)

scala> val beats = List(Beat(4), Beat(4), Beat(8), Beat(8))
beats: List[rc.dsl.Primitives.Beat] = List(Quarter, Quarter, Eighth, Eighth)

scala> val notes = (pitches zip beats).collect { case (p:Pitch,b:Beat) => Note(p,b) }
notes: List[rc.dsl.Primitives.Note] = List(Note(C₄,Quarter), Note(E₄,Quarter), Note(G₄,Eighth), Note(B₄,Eighth))

scala> val measure = Measure(TimeSignature(3,4), notes:_*)
measure: rc.dsl.Structures.Measure = Measure(TimeSignature(3,4),List(Note(C₄,Quarter), Note(E₄,Quarter), Note(G₄,Eighth), Note(B₄,Eighth)))
```
