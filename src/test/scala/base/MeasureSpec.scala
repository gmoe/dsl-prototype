package rc.dsl

import org.scalatest._

import rc.dsl._
import rc.dsl.Primitives._
import rc.dsl.Structures._

class MeasureSpec extends FlatSpec with Matchers {
  import PitchClass._, PitchDecorator._, IntervalQuality._

  "isFullMeasure()" should "check if the music would constitute a full Measure" in {
    val fullNotes44 = List(Note(C`_`4, Beat(4)), Note(C`_`4, Beat(4)),
      Note(C`_`4, Beat(4)), Note(C`_`4, Beat(8)), Note(C`_`4, Beat(8)))
    val fullNotes34 = List(Note(C`_`4, Beat(4)), Note(C`_`4, Beat(4)),
      Note(C`_`4, Beat(4)))

    isFullMeasure(TimeSignature(4,4), fullNotes44:_*) should be (true)
    isFullMeasure(TimeSignature(3,4), fullNotes34:_*) should be (true)

    isFullMeasure(TimeSignature(4,4), fullNotes34:_*) should be (false)
    isFullMeasure(TimeSignature(3,4), fullNotes44:_*) should be (false)
  }

}
