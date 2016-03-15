package rc.dsl

import Structures._

case class Interval(val ic: Int, val quality: IntervalQuality) {
  import PitchClass._, RomanNum._, PitchDecorator._, IntervalQuality._
  require(ic > 0, 
    "Interval must have a valid class (0 < ic <= 8)")
  require(if(quality==Perfect) ic==1||ic==4||ic==5||ic==8 else true,
    "Perfect intervals can only be unisons, fourths, fifths, and octaves")

  override def toString = quality + ic.toString

  //TODO: Can the number of semitones be determined procedurally?
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
    val oct = if(PitchClass.values.drop(PitchClass.values.indexOf(a.pitchClass)).take(ic
      ).containsSlice(List(PitchClass.B, PitchClass.C))) 1 else 0
    val pc = PitchClass.values.drop(PitchClass.values.indexOf(a.pitchClass)).apply(ic - 1)

    //TODO: Natural in some cases (implicit Mode)
    val pcTones = Pitch(pc,PitchDecorator.None,oct+a.octave).midiNumber - a.midiNumber 
    val dec = PitchDecorator(this.semitones - pcTones)

    Pitch(pc, dec, a.octave+oct)
  }
}

object Interval {
  def apply(a: Pitch, b: Pitch): Interval = {
    val ic = if(a.pitchClass == b.pitchClass) 1 else
      PitchClass.values.drop(PitchClass.values.indexOf(a.pitchClass)).indexOf(b.pitchClass)+1
    Interval(ic, ???) 
  }
}

sealed abstract class IntervalQuality

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
