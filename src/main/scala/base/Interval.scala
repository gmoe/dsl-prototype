package rc.dsl

import Primitives._

case class Interval(val ic: Int, val quality: IntervalQuality) {
  import PitchClass._, PitchDecorator._, IntervalQuality._, Math.abs
  require(ic <= 15 && ic >= -15,
    "Interval must have a valid class (-15 <= ic <= 15)")
  require(if(quality==Perfect)
    ic==1||abs(ic)==4||abs(ic)==5||abs(ic)==8||abs(ic)==11||abs(ic)==12||abs(ic)==15 else true,
    "Perfect intervals can only be unisons, 4ths, 5ths, 8ves, 11ths, 12ths, or 15ves.")

  override def toString = s"${if(ic < 0) "-" else ""}$quality${Math.abs(ic)}"

  def semitones: Int = if(ic > 0) this match {
    case Interval(1, Perfect) => 0; case Interval(2, Diminished) => 0
    case Interval(2, Minor) => 1; case Interval(1, Augmented) => 1
    case Interval(2, Major) => 2; case Interval(3, Diminished) => 2
    case Interval(3, Minor) => 3; case Interval(2, Augmented) => 3
    case Interval(3, Major) => 4; case Interval(4, Diminished) => 4
    case Interval(4, Perfect) => 5; case Interval(3, Augmented) => 5
    case Interval(5, Diminished) => 6; case Interval(4, Augmented) => 6
    case Interval(5, Perfect) => 7; case Interval(6, Diminished) => 7
    case Interval(6, Minor) => 8; case Interval(5, Augmented) => 8
    case Interval(6, Major) => 9; case Interval(7, Diminished) => 9
    case Interval(7, Minor) => 10; case Interval(6, Augmented) => 10
    case Interval(7, Major) => 11; case Interval(8, Diminished) => 11
    case Interval(8, Perfect) => 12; case Interval(7, Augmented) => 12
    case Interval(9, Diminished) => 12;
    case Interval(9, Minor) => 13; case Interval(8, Augmented) => 13
    case Interval(9, Major) => 14; case Interval(10, Diminished) => 14
    case Interval(10, Minor) => 15; case Interval(9, Augmented) => 15
    case Interval(10, Major) => 16; case Interval(11, Diminished) => 16
    case Interval(11, Perfect) => 17; case Interval(10, Augmented) => 17
    case Interval(12, Diminished) => 18; case Interval(11, Augmented) => 18
    case Interval(12, Perfect) => 19; case Interval(13, Diminished) => 19
    case Interval(13, Minor) => 20; case Interval(12, Augmented) => 20
    case Interval(13, Major) => 21; case Interval(14, Diminished) => 21
    case Interval(14, Minor) => 22; case Interval(13, Augmented) => 22
    case Interval(14, Major) => 23; case Interval(15, Diminished) => 23
    case Interval(15, Perfect) => 24; case Interval(14, Augmented) => 24
  } else (-this.negate.semitones)

  def fromPitch(a: Pitch): Pitch = {
    if(ic > 0) {
      val pc = PitchClass.streamForward(a.pitchClass).apply(ic - 1)

      val oct = PitchClass.streamForward(a.pitchClass).take(ic).toList.sliding(2,1).count(
        x => x == List(B,C))

      //TODO: Natural in some cases (implicit Mode)
      val pcTones = Pitch(pc,PitchDecorator.None,oct+a.octave).midiNumber - a.midiNumber
      val dec = PitchDecorator(this.semitones - pcTones)

      return Pitch(pc, dec, a.octave+oct)
    } else {
      val pc = PitchClass.streamBackward(a.pitchClass).apply((-ic) - 1)

      val oct = PitchClass.streamBackward(a.pitchClass).take(-ic).toList.sliding(2,1).count(
        x => x == List(C,B))

      //TODO: Natural in some cases (implicit Mode)
      val pcTones = a.midiNumber - Pitch(pc,PitchDecorator.None,a.octave-oct).midiNumber
      val dec = PitchDecorator((this.semitones) + pcTones)

      return Pitch(pc, dec, a.octave-oct)
    }
  }

  def invert: Interval = Interval(9-ic, quality.invert)
  def negate: Interval = Interval(-ic, quality)
  def unary_- = negate
}

object Interval {

  import IntervalQuality._

  private def findQuality(i: Int): IntervalQuality = Math.abs(i) match {
    case 0 | 12 | 24 => Perfect
    case 1 | 13  => Minor
    case 2 | 14  => Major
    case 3 | 15  => Minor
    case 4 | 16  => Major
    case 5 | 17  => Perfect
    case 6 | 18  => Diminished
    case 7 | 19  => Perfect
    case 8 | 20  => Minor
    case 9 | 21  => Major
    case 10 | 22 => Minor
    case 11 | 23 => Major
  }

  def apply(a: Pitch, b: Pitch): Interval = {
    val hi = List(a,b).max
    val lo = List(a,b).min

    val ic = {
      if (a == b) 1 
      else if(a.pitchClass == b.pitchClass && hi.octave > lo.octave) 8
      else PitchClass.streamForward(lo.pitchClass).indexOf(hi.pitchClass)+1
    }

    val quality = findQuality(hi.midiNumber - lo.midiNumber)

    Interval(ic, quality) 
  }

  def apply(i: Int): Interval = {
    require(-24 <= i && i <= 24, "Interval steps in semitones must be between -24 and 24.") 
    val ic = Math.abs(i) match {
      case 0 => 1
      case 1 | 2 => 2
      case 3 | 4 => 3
      case 5 => 4
      case 6 | 7 => 5
      case 8 | 9 => 6
      case 10 | 11 => 7
      case 12 => 8
      case 13 | 14 => 9
      case 15 | 16 => 10
      case 17 => 11
      case 18 | 19 => 12
      case 20 | 21 => 13
      case 22 | 23 => 14
      case 24 => 15
    }
    Interval(ic * (i / Math.abs(i)), findQuality(i))
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
