# Music DSL Prototype

This is a prototype Scala-embedded domain specific language for composing and
transforming music, which aims to be expressive and intuitive for
non-programmers. The DSL also contains generative modules that can be pulled in
by the composer when necessary, such as Xenakis sieves and Markov chains.
Examples of these can be found in the [wiki][docs].

## Examples

Here is a use case demonstrating how to create modulations from one key to
another using function composition:

```scala
scala> import PitchClass._, RomanNum._
import PitchClass._
import RomanNum._

scala> C`_`4
res0: rc.dsl.Primitives.Pitch = C₄

scala> (F`#`4) != (G`b`4)
res1: Boolean = true

scala> (F`#`4).isEnharmonic(G`b`4) // can also use (F`#`4) ~= (G`b`4)
res2: Boolean = true

scala> val cMajor = Ionian(res0)
cMajor: rc.dsl.Ionian = Ionian(C)

scala> val fSharpMajor = Ionian(F`#`4)
fSharpMajor: rc.dsl.Ionian = Ionian(F♯)

scala> val modulate = fSharpMajor.to compose cMajor.from
modulate: rc.dsl.Primitives.Pitch => rc.dsl.Primitives.Pitch = <function1>

scala> modulate(D`#`4)
res3: rc.dsl.Primitives.Pitch = G𝄪₄
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

Here is another example of functional composition, which threads a starting
pitch through a sequence of musical intervals:

```scala
scala> import IntervalQuality._
import IntervalQuality._

scala> val steps = List(Interval(2,Major), Interval(5,Perfect), Interval(-3, Minor), Interval(2,Minor))
steps: List[rc.dsl.Interval] = List(M2, P5, -m3, m2)

scala> val melody = thread(C`_`4)(steps)
melody: List[rc.dsl.Primitives.Pitch] = List(C₄, D₄, A₄, F♯₄, G₄)
```

Examples for additional DSL modules can be found in the [wiki][docs].

[docs]: https://github.com/gmoe/dsl-prototype/wiki
