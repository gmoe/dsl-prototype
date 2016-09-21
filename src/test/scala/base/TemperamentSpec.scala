package rc.dsl

import org.scalatest._

import Primitives._

class TemperamentSpec extends FlatSpec with Matchers {
  import PitchClass._, PitchDecorator._
  
  "Equal Temperament" should "generate correct frequencies" in {
    val et = EqualTemperament(440)

    et.frequency(A`_`4) should be (440)
    et.frequency(A`_`5) should be (880)
    et.frequency(A`_`2) should be (110)
    et.frequency(C`_`4) should be (261.6256)
  }

}
