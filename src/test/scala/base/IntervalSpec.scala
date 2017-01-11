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

    Interval(13,Major).invert should be (Interval(3,Minor))
    Interval(9,Minor).invert should be (Interval(7,Major))
    Interval(12,Perfect).invert should be (Interval(4,Perfect))
  }

  it should "invert to their complement (negative)" in {
    Interval(-5,Perfect).invert should be (Interval(-4,Perfect))
    Interval(-4,Perfect).invert should be (Interval(-5,Perfect))

    Interval(-3,Major).invert should be (Interval(-6,Minor))
    Interval(-2,Minor).invert should be (Interval(-7,Major))

    Interval(-3,Diminished).invert should be (Interval(-6,Augmented))
    Interval(-6,Augmented).invert should be (Interval(-3,Diminished))

    Interval(-13,Major).invert should be (Interval(-3,Minor))
    Interval(-9,Minor).invert should be (Interval(-7,Major))
    Interval(-12,Perfect).invert should be (Interval(-4,Perfect))
  }

  it should "create correct Intervals from two ascending Pitches" in {
    Interval(Pitch(C,None,4), Pitch(C,None,5)) should be (Interval(8, Perfect))
    Interval(Pitch(C,None,4), Pitch(C,None,4)) should be (Interval(1, Perfect))
    Interval(Pitch(C,None,4), Pitch(D,None,4)) should be (Interval(2, Major))
    Interval(Pitch(C,None,4), Pitch(E,Flat,4)) should be (Interval(3, Minor))

    Interval(Pitch(B,Flat,4), Pitch(G,None,5)) should be (Interval(6, Major))
    Interval(Pitch(B,Flat,4), Pitch(F,None,5)) should be (Interval(5, Perfect))
    Interval(Pitch(C,Sharp,4), Pitch(E,None,4)) should be (Interval(3,Minor))
    Interval(Pitch(F,Sharp,4), Pitch(A,Sharp,4)) should be (Interval(3,Major))

    Interval(Pitch(D,None,4), Pitch(D,None,6)) should be (Interval(15,Perfect))
    Interval(Pitch(C,None,4), Pitch(G,None,5)) should be (Interval(12,Perfect))
    Interval(Pitch(C,None,4), Pitch(D,Flat,5)) should be (Interval(9,Minor))
    Interval(Pitch(E,None,4), Pitch(D,None,6)) should be (Interval(14,Minor))
  }

  it should "create correct Intervals from two descending Pitches" in {
    Interval(Pitch(D,None,4), Pitch(D,None,3)) should be (Interval(-8, Perfect))
    Interval(Pitch(C,None,4), Pitch(B,Flat,3)) should be (Interval(-2, Major))
    Interval(Pitch(F,None,4), Pitch(C,None,4)) should be (Interval(-4, Perfect))
  }

  it should "create general Intervals from semitone gaps" in {
    Interval(0) should be (Interval(1, Perfect))
    Interval(12) should be (Interval(8, Perfect))
    Interval(7) should be (Interval(5, Perfect))
    Interval(4) should be (Interval(3, Major))
    Interval(1) should be (Interval(2, Minor))

    Interval(24) should be (Interval(15, Perfect))
    Interval(19) should be (Interval(12, Perfect))
    Interval(14) should be (Interval(9, Major))
    Interval(13) should be (Interval(9, Minor))

    Interval(-2) should be (Interval(-2, Major))
    Interval(-12) should be (Interval(-8, Perfect))
  }

  it should "match enharmonic Intervals" in {
    assert(Interval(3, Major) ~= Interval(4, Diminished))
    assert(Interval(6, Minor) ~= Interval(5, Augmented))
    assert(Interval(4, Augmented) ~= Interval(5, Diminished))
    assert(Interval(12, Perfect) ~= Interval(13, Diminished))
    assert(Interval(-8, Perfect) ~= Interval(-9, Diminished))
  }

  it should "use modes to determine natural notes" in {
    val P5 = Interval(5, Perfect)
    P5(B`b`4)(Ionian(B`_`4)) should be (F`n`5)
    P5(C`_`4)(Ionian(C`#`4)) should be (G`n`4)
  }

  "Unison" should "create correct notes" in {
    val P1 = Interval(1, Perfect)
    P1(Pitch(C,None,4)) should be (Pitch(C,None,4))
    P1(Pitch(D,None,4)) should be (Pitch(D,None,4))
    P1(Pitch(E,Flat,4)) should be (Pitch(E,Flat,4))
    P1(Pitch(F,Sharp,4)) should be (Pitch(F,Sharp,4))
    P1(Pitch(G,DoubleSharp,4)) should be (Pitch(G,DoubleSharp,4))
    P1(Pitch(A,DoubleFlat,4)) should be (Pitch(A,DoubleFlat,4))
  }

  "Major second" should "result in correct notes" in {
    val M2 = Interval(2, Major)
    M2(Pitch(C,None,4)) should be (Pitch(D,None,4))
    M2(Pitch(A,None,4)) should be (Pitch(B,None,4))
    M2(Pitch(G,Sharp,4)) should be (Pitch(A,Sharp,4))
    M2(Pitch(D,Flat,4)) should be (Pitch(E,Flat,4))
    M2(Pitch(E,None,4)) should be (Pitch(F,Sharp,4))
    M2(Pitch(B,Flat,4)) should be (Pitch(C,None,5))
    M2(Pitch(B,None,4)) should be (Pitch(C,Sharp,5))

    M2(Pitch(E,Sharp,4)) should be (Pitch(F,DoubleSharp,4))
    M2(Pitch(B,Sharp,4)) should be (Pitch(C,DoubleSharp,5))
  }

  "Major third" should "result in correct notes" in {
    val M3 = Interval(3, Major)
    M3(Pitch(C,None,4)) should be (Pitch(E,None,4))
    M3(Pitch(D,None,4)) should be (Pitch(F,Sharp,4))
    M3(Pitch(C,Sharp,4)) should be (Pitch(E,Sharp,4))
    M3(Pitch(B,Flat,4)) should be (Pitch(D,None,5))
    M3(Pitch(E,None,4)) should be (Pitch(G,Sharp,4))

    M3(Pitch(E,Sharp,4)) should be (Pitch(G,DoubleSharp,4))
  }

  "Perfect fifth" should "result in correct notes" in {
    val P5 = Interval(5, Perfect)
    P5(Pitch(C,None,4)) should be (Pitch(G,None,4))
    P5(Pitch(D,Sharp,4)) should be (Pitch(A,Sharp,4))
    P5(Pitch(E,Flat,4)) should be (Pitch(B,Flat,4))
    P5(Pitch(F,None,4)) should be (Pitch(C,None,5))

    P5(Pitch(B,Flat,4)) should be (Pitch(F,None,5))
    P5(Pitch(B,None,4)) should be (Pitch(F,Sharp,5))
  }

  "Diminished fifth" should "result in correct notes" in {
    val d5 = Interval(5, Diminished)
    d5(C`_`4) should be (G`b`4)
    d5(B`b`4) should be (F`b`5)
    d5(F`b`4) should be (C`bb`5)
    d5(D`b`4) should be (A`bb`4)
    d5(E`x`4) should be (B`#`4)
  }

  "Perfect 12th" should "result in correct notes" in {
    val P12 = Interval(12, Perfect)
    P12(C`_`4) should be (G`_`5)
    P12(E`b`4) should be (B`b`5)
    P12(D`#`4) should be (A`#`5)

    P12(F`_`4) should be (C`_`6)
    P12(B`b`4) should be (F`_`6)
    P12(B`_`4) should be (F`#`6)
  }

  "Negative major second" should "result in correct notes" in {
    val negM2 = Interval(-2, Major)
    negM2(D`_`4) should be (C`_`4)
    negM2(E`b`4) should be (D`b`4)
    negM2(G`#`4) should be (F`#`4)

    negM2(C`_`4) should be (B`b`3)
    negM2(C`#`4) should be (B`_`3)
    negM2(C`b`4) should be (B`bb`3)
  }

}
