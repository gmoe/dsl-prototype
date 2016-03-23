package rc.dsl

import Primitives._

case class Interval(val ic: Int, val quality: IntervalQuality) {
  import PitchClass._, PitchDecorator._, IntervalQuality._
  require(ic > 0, 
    "Interval must have a valid class (0 < ic <= 8)")
  require(if(quality==Perfect) ic==1||ic==4||ic==5||ic==8 else true,
    "Perfect intervals can only be unisons, fourths, fifths, and octaves")

  override def toString = quality + ic.toString

  //TODO: Can the number of semitones be determined procedurally?
  //TODO: Add support for Intervals > 8
  def semitones: Int = this match {
    case Interval(1, Perfect) => 0
    case Interval(2, Diminished) => 0
    case Interval(2, Minor) => 1
    case Interval(1, Augmented) => 1
    case Interval(2, Major) => 2
    case Interval(3, Diminished) => 2
    case Interval(3, Minor) => 3
    case Interval(2, Augmented) => 3
    case Interval(3, Major) => 4
    case Interval(4, Diminished) => 4
    case Interval(4, Perfect) => 5
    case Interval(3, Augmented) => 5
    case Interval(5, Diminished) => 6
    case Interval(4, Augmented) => 6
    case Interval(5, Perfect) => 7
    case Interval(6, Diminished) => 7
    case Interval(6, Minor) => 8
    case Interval(5, Augmented) => 8
    case Interval(6, Major) => 9
    case Interval(7, Diminished) => 9
    case Interval(7, Minor) => 10
    case Interval(6, Augmented) => 10
    case Interval(7, Major) => 11
    case Interval(8, Diminished) => 11
    case Interval(8, Perfect) => 12
    case Interval(7, Augmented) => 12
  }

  def fromPitch(a: Pitch): Pitch = {
    val oct = if(PitchClass.streamFrom(a.pitchClass).take(ic
      ).containsSlice(List(PitchClass.B, PitchClass.C))) 1 else 0
    val pc = PitchClass.streamFrom(a.pitchClass).apply(ic - 1)

    //TODO: Natural in some cases (implicit Mode)
    val pcTones = Pitch(pc,PitchDecorator.None,oct+a.octave).midiNumber - a.midiNumber 
    val dec = PitchDecorator(this.semitones - pcTones)

    Pitch(pc, dec, a.octave+oct)
  }

  def invert: Interval = Interval(9-ic, quality.invert)
  def unary_- = invert
}

object Interval {

  import IntervalQuality._

  //TODO: Determine this procedurally, if possible
  private def findQuality(i: Int): IntervalQuality = i match {
    case 0 => Perfect
    case 1 => Minor
    case 2 => Major
    case 3 => Minor
    case 4 => Major
    case 5 => Perfect
    case 6 => Diminished
    case 7 => Perfect
    case 8 => Minor
    case 9 => Major
    case 10 => Minor
    case 11 => Major
    case 12 => Perfect
  }

  def apply(a: Pitch, b: Pitch): Interval = {
    val hi = List(a,b).max
    val lo = List(a,b).min

    val ic = {
      if (a == b) 1 
      else if(a.pitchClass == b.pitchClass && hi.octave > lo.octave) 8
      else PitchClass.streamFrom(lo.pitchClass).indexOf(hi.pitchClass)+1
    }

    val quality = findQuality(hi.midiNumber - lo.midiNumber)

    Interval(ic, quality) 
  }
}

sealed abstract class IntervalQuality {
  import IntervalQuality._

  def invert: IntervalQuality = this match {
    case Major => Minor
    case Minor => Major
    case Perfect => Perfect
    case Diminished => Augmented
    case Augmented => Diminished
  }

  def unary_- = invert

}

object IntervalQuality {
  case object Major extends IntervalQuality {
    override def toString = "M"
  }
  case object Minor extends IntervalQuality {
    override def toString = "m"
  }
  case object Perfect extends IntervalQuality {
    override def toString = "P"
  }
  case object Diminished extends IntervalQuality {
    override def toString = "d"
  }
  case object Augmented extends IntervalQuality {
    override def toString = "A"
  }
}
