package rc.dsl

import org.scalatest._

import Structures._

class PitchSpec extends FlatSpec with Matchers {
  import PitchClass._, PitchDecorator._, IntervalQuality._
  
  "Enharmonic pitches" should "not be objectively equivalent" in {
    Pitch(C, Sharp, 4) should not be Pitch(D, Flat, 4)
    Pitch(E, Sharp, 4) should not be Pitch(F, None, 4)
    Pitch(B, Flat, 4) should not be Pitch(A, Sharp, 4)
  }

  it should "be enharmonically equivalent" in {
    assert(Pitch(C, Sharp, 4).isEnharmonic(Pitch(D, Flat, 4)))
    assert(Pitch(E, Sharp, 4).isEnharmonic(Pitch(F, None, 4)))
    assert(Pitch(B, Flat, 4).isEnharmonic(Pitch(A, Sharp, 4)))
  }

  "Pitches" should "be ordered" in {
    assert(Pitch(C, None, 4) < Pitch(C, None, 5))
    assert(Pitch(D, None, 4) < Pitch(A, None, 4))
    assert(Pitch(E, None, 4) < Pitch(E, Sharp, 4))
    assert(Pitch(G, None, 4) > Pitch(G, Flat, 4))
    assert(Pitch(D, None, 4) < Pitch(D, DoubleSharp, 4))
    assert(Pitch(B, None, 4) > Pitch(B, DoubleFlat, 4))
  }

}
