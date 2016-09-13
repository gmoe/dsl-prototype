package rc.dsl

import org.scalatest._

import Primitives._

class IntervalSpec extends FlatSpec with Matchers {
  import PitchClass._, PitchDecorator._, IntervalQuality._

  "Intervals" should "invert to their complement" in {
    Interval(5,Perfect).invert should be (Interval(4,Perfect))
    Interval(4,Perfect).invert should be (Interval(5,Perfect))

    Interval(3,Major).invert should be (Interval(6,Minor))
    Interval(2,Minor).invert should be (Interval(7,Major))
    
    Interval(3,Diminished).invert should be (Interval(6,Augmented))
    Interval(6,Augmented).invert should be (Interval(3,Diminished))
  }

  "Two Pitches" should "create correct Intervals" in {
    Interval(Pitch(C,None,4), Pitch(C,None,5)) should be (Interval(8, Perfect))
    Interval(Pitch(C,None,4), Pitch(C,None,4)) should be (Interval(1, Perfect))
    Interval(Pitch(C,None,4), Pitch(D,None,4)) should be (Interval(2, Major))
    Interval(Pitch(C,None,4), Pitch(E,Flat,4)) should be (Interval(3, Minor))

    Interval(Pitch(B,Flat,4), Pitch(G,None,5)) should be (Interval(6, Major))
    Interval(Pitch(B,Flat,4), Pitch(F,None,5)) should be (Interval(5, Perfect))
  }

  "Unison" should "create correct notes" in {
    val P1 = Interval(1, Perfect)
    P1.fromPitch(Pitch(C,None,4)) should be (Pitch(C,None,4))
    P1.fromPitch(Pitch(D,None,4)) should be (Pitch(D,None,4))
    P1.fromPitch(Pitch(E,Flat,4)) should be (Pitch(E,Flat,4))
    P1.fromPitch(Pitch(F,Sharp,4)) should be (Pitch(F,Sharp,4))
    P1.fromPitch(Pitch(G,DoubleSharp,4)) should be (Pitch(G,DoubleSharp,4))
    P1.fromPitch(Pitch(A,DoubleFlat,4)) should be (Pitch(A,DoubleFlat,4))
  }

  "Major second" should "result in correct notes" in {
    val M2 = Interval(2, Major)
    M2.fromPitch(Pitch(C,None,4)) should be (Pitch(D,None,4))
    M2.fromPitch(Pitch(A,None,4)) should be (Pitch(B,None,4))
    M2.fromPitch(Pitch(G,Sharp,4)) should be (Pitch(A,Sharp,4))
    M2.fromPitch(Pitch(D,Flat,4)) should be (Pitch(E,Flat,4))
    M2.fromPitch(Pitch(E,None,4)) should be (Pitch(F,Sharp,4))
    M2.fromPitch(Pitch(B,Flat,4)) should be (Pitch(C,None,5))
    M2.fromPitch(Pitch(B,None,4)) should be (Pitch(C,Sharp,5))

    M2.fromPitch(Pitch(E,Sharp,4)) should be (Pitch(F,DoubleSharp,4))
    M2.fromPitch(Pitch(B,Sharp,4)) should be (Pitch(C,DoubleSharp,5))
  }

  "Major third" should "result in correct notes" in {
    val M3 = Interval(3, Major)
    M3.fromPitch(Pitch(C,None,4)) should be (Pitch(E,None,4))
    M3.fromPitch(Pitch(D,None,4)) should be (Pitch(F,Sharp,4))
    M3.fromPitch(Pitch(C,Sharp,4)) should be (Pitch(E,Sharp,4))
    M3.fromPitch(Pitch(B,Flat,4)) should be (Pitch(D,None,5))
    M3.fromPitch(Pitch(E,None,4)) should be (Pitch(G,Sharp,4))

    M3.fromPitch(Pitch(E,Sharp,4)) should be (Pitch(G,DoubleSharp,4))
  }

  "Perfect fifth" should "result in correct notes" in {
    val P5 = Interval(5, Perfect)
    P5.fromPitch(Pitch(C,None,4)) should be (Pitch(G,None,4))
    P5.fromPitch(Pitch(D,Sharp,4)) should be (Pitch(A,Sharp,4))
    P5.fromPitch(Pitch(E,Flat,4)) should be (Pitch(B,Flat,4))
    P5.fromPitch(Pitch(F,None,4)) should be (Pitch(C,None,5))

    P5.fromPitch(Pitch(B,Flat,4)) should be (Pitch(F,None,5))
    P5.fromPitch(Pitch(B,None,4)) should be (Pitch(F,Sharp,5))
  }

  "Diminished fifth" should "result in correct notes" in {
    val d5 = Interval(5, Diminished)
    d5.fromPitch(C`_`4) should be (G`b`4)
    d5.fromPitch(B`b`4) should be (F`b`5)
    d5.fromPitch(F`b`4) should be (C`bb`5)
    d5.fromPitch(D`b`4) should be (A`bb`4)
    d5.fromPitch(E`x`4) should be (B`#`4)
  }

  "Perfect 12th" should "result in correct notes" in {
    val P12 = Interval(12, Perfect)
    P12.fromPitch(C`_`4) should be (G`_`5)
    P12.fromPitch(E`b`4) should be (B`b`5)
    P12.fromPitch(D`#`4) should be (A`#`5)

    P12.fromPitch(F`_`4) should be (C`_`6)
    P12.fromPitch(B`b`4) should be (F`_`6)
    P12.fromPitch(B`_`4) should be (F`#`6)
  }

  "Negative major second" should "result in correct notes" in {
    val negM2 = Interval(-2, Major)
    negM2.fromPitch(D`_`4) should be (C`_`4)
    negM2.fromPitch(E`b`4) should be (D`b`4)
    negM2.fromPitch(G`#`4) should be (F`#`4)

    negM2.fromPitch(C`_`4) should be (B`b`3)
    negM2.fromPitch(C`#`4) should be (B`_`3)
    negM2.fromPitch(C`b`4) should be (B`bb`3)
  }

}
