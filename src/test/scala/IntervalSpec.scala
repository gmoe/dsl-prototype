package rc.dsl

import org.scalatest._

import Structures._

class IntervalSpec extends FlatSpec with Matchers {
  import PitchClass._, PitchDecorator._, IntervalQuality._

  "Major second" should "result in correct notes" in {
    val M2 = Interval(2, Major)
    M2.fromPitch(Pitch(C,None,4)) should be (Pitch(D,None,4))
    M2.fromPitch(Pitch(A,None,4)) should be (Pitch(B,None,4))
    M2.fromPitch(Pitch(G,Sharp,4)) should be (Pitch(A,Sharp,4))
    M2.fromPitch(Pitch(D,Flat,4)) should be (Pitch(E,Flat,4))
    M2.fromPitch(Pitch(E,None,4)) should be (Pitch(F,Sharp,4))
    M2.fromPitch(Pitch(B,Flat,4)) should be (Pitch(C,None,5))
    M2.fromPitch(Pitch(B,None,4)) should be (Pitch(C,Sharp,5))

    //println(Pitch(B,Sharp,4).midiNumber + " - " + Pitch(C,DoubleSharp,5).midiNumber)

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

    P5.fromPitch(Pitch(B,Flat,4)) should be (Pitch(F,None,5))
    P5.fromPitch(Pitch(B,None,4)) should be (Pitch(F,Sharp,5))
  }

  "Intervals" should "invert to their complement" in {
    Interval(5,Perfect).invert should be (Interval(4,Perfect))
    Interval(4,Perfect).invert should be (Interval(5,Perfect))

    Interval(3,Major).invert should be (Interval(6,Minor))
    Interval(2,Minor).invert should be (Interval(7,Major))
    
    Interval(3,Diminished).invert should be (Interval(6,Augmented))
    Interval(6,Augmented).invert should be (Interval(3,Diminished))
  }
}
