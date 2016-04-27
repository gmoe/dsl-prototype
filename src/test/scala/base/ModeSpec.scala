package rc.dsl

import org.scalatest._

import Primitives._

class ModeSpec extends FlatSpec with Matchers {
  import PitchClass._, PitchDecorator._, IntervalQuality._, RomanNum._

  "Ionian mode" should "result in correct degrees using cached values" in {
    val majorC = Ionian(Pitch(C, None, 4))
    majorC.I should be (Pitch(C, None, 4))
    majorC.II should be (Pitch(D, None, 4))
    majorC.III should be (Pitch(E, None, 4))
    majorC.IV should be (Pitch(F, None, 4))
    majorC.V should be (Pitch(G, None, 4))
    majorC.VI should be (Pitch(A, None, 4))
    majorC.VII should be (Pitch(B, None, 4))
  }

  it should "result in correct degrees using to()" in {
    val majorC = Ionian(Pitch(C, None, 4))
    
    majorC.to(RomanPitch(I, None, 4)) should be (Pitch(C, None, 4))
    majorC.to(RomanPitch(II, None, 4)) should be (Pitch(D, None, 4))
    majorC.to(RomanPitch(III, None, 4)) should be (Pitch(E, None, 4))
    majorC.to(RomanPitch(IV, None, 4)) should be (Pitch(F, None, 4))
    majorC.to(RomanPitch(V, None, 4)) should be (Pitch(G, None, 4))
    majorC.to(RomanPitch(VI, None, 4)) should be (Pitch(A, None, 4))
    majorC.to(RomanPitch(VII, None, 4)) should be (Pitch(B, None, 4))

    majorC.to(RomanPitch(I, Sharp, 4)) should be (Pitch(C, Sharp, 4))
    majorC.to(RomanPitch(I, Flat, 4)) should be (Pitch(C, Flat, 4))
    majorC.to(RomanPitch(I, DoubleSharp, 4)) should be (Pitch(C, DoubleSharp, 4))
    majorC.to(RomanPitch(I, DoubleFlat, 4)) should be (Pitch(C, DoubleFlat, 4))
  }

  "Dorian mode" should "result in correct degrees using cached values" in {
    val dorianC = Dorian(Pitch(C, None,4))
    dorianC.I should be (Pitch(C, None, 4))
    dorianC.II should be (Pitch(D, None, 4))
    dorianC.III should be (Pitch(E, Flat, 4))
    dorianC.IV should be (Pitch(F, None, 4))
    dorianC.V should be (Pitch(G, None, 4))
    dorianC.VI should be (Pitch(A, None, 4))
    dorianC.VII should be (Pitch(B, Flat, 4))
  }

  it should "result in correct degrees using to()" in {
    val dorianC = Dorian(Pitch(C, None, 4))

    dorianC.to(RomanPitch(I, None, 4)) should be (Pitch(C, None, 4))
    dorianC.to(RomanPitch(II, None, 4)) should be (Pitch(D, None, 4))
    dorianC.to(RomanPitch(III, None, 4)) should be (Pitch(E, Flat, 4))
    dorianC.to(RomanPitch(IV, None, 4)) should be (Pitch(F, None, 4))
    dorianC.to(RomanPitch(V, None, 4)) should be (Pitch(G, None, 4))
    dorianC.to(RomanPitch(VI, None, 4)) should be (Pitch(A, None, 4))
    dorianC.to(RomanPitch(VII, None, 4)) should be (Pitch(B, Flat, 4))

    dorianC.to(RomanPitch(III, Sharp, 4)) should be (Pitch(E, Natural, 4))
    dorianC.to(RomanPitch(III, Flat, 4)) should be (Pitch(E, DoubleFlat, 4))
    dorianC.to(RomanPitch(III, DoubleSharp, 4)) should be (Pitch(E, Sharp, 4))
  }

  "Modulation" should "result in correct note spellings" in {
    val majorC = Ionian(Pitch(C, None, 4))
    val majorBb = Ionian(Pitch(B, Flat, 4))

    val cToBb = majorBb.to compose majorC.from
    cToBb(C`_`1) should be (B`b`1)
    cToBb(D`_`2) should be (C`_`2)
    cToBb(E`_`3) should be (D`_`3)
    cToBb(F`_`4) should be (E`b`4)
    cToBb(G`_`5) should be (F`_`5)
    cToBb(A`_`6) should be (G`_`6)
    cToBb(B`_`7) should be (A`_`7)

    val bFlatToC = majorC.to compose majorBb.from
    bFlatToC(B`b`1) should be (C`_`1)
    bFlatToC(B`_`2) should be (C`#`2)
    bFlatToC(B`#`3) should be (C`x`3)
  }

  "Identically spelled modes" should "be enharmonically equivalent" in {
    (Ionian(Pitch(C, None, 4)).isEnharmonic(Dorian(Pitch(D, None, 4)))) should be (true)
    (Ionian(Pitch(B, Flat, 4)).isEnharmonic(Dorian(Pitch(C, None, 4)))) should be (true)
  }

}
