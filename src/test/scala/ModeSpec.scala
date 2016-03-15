package rc.dsl

import org.scalatest._

import Structures._

class ModeSpec extends FlatSpec with Matchers {
  import PitchClass._, PitchDecorator._, IntervalQuality._

  "Ionian mode" should "result in correct degrees" in {
    val io = Ionian(Pitch(C, None, 4))
    io.I should be (Pitch(C, None, 4))
    io.II should be (Pitch(D, None, 4))
    io.III should be (Pitch(E, None, 4))
    io.IV should be (Pitch(F, None, 4))
    io.V should be (Pitch(G, None, 4))
    io.VI should be (Pitch(A, None, 4))
    io.VII should be (Pitch(B, None, 4))
  }

  "Dorian mode" should "result in correct degrees" in {
    val io = Dorian(Pitch(C, None,4))
    io.I should be (Pitch(C, None, 4))
    io.II should be (Pitch(D, None, 4))
    io.III should be (Pitch(E, Flat, 4))
    io.IV should be (Pitch(F, None, 4))
    io.V should be (Pitch(G, None, 4))
    io.VI should be (Pitch(A, None, 4))
    io.VII should be (Pitch(B, Flat, 4))
  }

}
