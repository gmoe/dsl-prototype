package rc.dsl

import scalaz._, Isomorphism._
import Primitives._

sealed trait Mode extends Music with IsoSet[RomanPitch, Pitch] with IsEnharmonic[Mode] {
  import IntervalQuality._

  val root: Pitch
  val I: Pitch = root
  val II: Pitch = (degreeIntervals take 1).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val III: Pitch = (degreeIntervals take 2).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val IV: Pitch = (degreeIntervals take 3).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val V: Pitch = (degreeIntervals take 4).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val VI: Pitch = (degreeIntervals take 5).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val VII: Pitch = (degreeIntervals take 6).foldLeft(root) { (c, a) => a.fromPitch(c) }

  def degrees: Set[Pitch] = Set(I, II, III, IV, V, VI, VII)
  def degreeIntervals: List[Interval]

  def isEnharmonic(that: Mode) = {
    def flattenOctaves(p:Pitch) = Pitch(p.pitchClass, p.decorator, 4)
    this.degrees.map(flattenOctaves) == that.degrees.map(flattenOctaves)
  }

  private def spellTo(d: PitchDecorator, p: Pitch): PitchDecorator = {
    val pd = PitchDecorator(d.midiNumber+p.decorator.midiNumber) 
    if (p.decorator != PitchDecorator.None) pd match {
      case PitchDecorator.None => PitchDecorator.Natural
      case p: PitchDecorator => p
    } else pd
  }

  def to: RomanPitch => Pitch = {
    case RomanPitch(RomanNum.I,d,o) => Pitch(this.I.pitchClass, spellTo(d,this.I), o)
    case RomanPitch(RomanNum.II,d,o) => Pitch(this.II.pitchClass, spellTo(d,this.II), o)
    case RomanPitch(RomanNum.III,d,o) => Pitch(this.III.pitchClass, spellTo(d,this.III), o)
    case RomanPitch(RomanNum.IV,d,o) => Pitch(this.IV.pitchClass, spellTo(d,this.IV), o)
    case RomanPitch(RomanNum.V,d,o) => Pitch(this.V.pitchClass, spellTo(d,this.V), o)
    case RomanPitch(RomanNum.VI,d,o) => Pitch(this.VI.pitchClass, spellTo(d,this.VI), o)
    case RomanPitch(RomanNum.VII,d,o) => Pitch(this.VII.pitchClass, spellTo(d,this.VII), o)
  }

  private def spellFrom(d: PitchDecorator, p: Pitch): PitchDecorator = {
    PitchDecorator(d.midiNumber - p.decorator.midiNumber)
  }

  def from: Pitch => RomanPitch = {
    case Pitch(this.I.pitchClass,d,o) => RomanPitch(RomanNum.I, spellFrom(d,this.I), o)
    case Pitch(this.II.pitchClass,d,o) => RomanPitch(RomanNum.II, spellFrom(d,this.II), o)
    case Pitch(this.III.pitchClass,d,o) => RomanPitch(RomanNum.III, spellFrom(d,this.III), o)
    case Pitch(this.IV.pitchClass,d,o) => RomanPitch(RomanNum.IV, spellFrom(d,this.VI), o)
    case Pitch(this.V.pitchClass,d,o) => RomanPitch(RomanNum.V, spellFrom(d,this.V), o)
    case Pitch(this.VI.pitchClass,d,o) => RomanPitch(RomanNum.VI, spellFrom(d,this.VI), o)
    case Pitch(this.VII.pitchClass,d,o) => RomanPitch(RomanNum.VII, spellFrom(d,this.VII), o)
  }

  override def toString: String = {
    s"${this.getClass.getSimpleName}(${this.I.pitchClass}${this.I.decorator})"
  }
}

case class Ionian(val root: Pitch) extends Mode {
  import IntervalQuality._
  override def degreeIntervals: List[Interval] = List(Interval(2, Major), Interval(2, Major),
    Interval(2, Minor), Interval(2, Major), Interval(2, Major), Interval(2, Major),
    Interval(2, Minor))
}

case class Dorian(val root: Pitch) extends Mode {
  import IntervalQuality._
  override def degreeIntervals: List[Interval] = List(Interval(2, Major), Interval(2, Minor),
    Interval(2, Major), Interval(2, Major), Interval(2, Major), Interval(2, Minor),
    Interval(2, Major))
}

case class Phrygian(val root: Pitch) extends Mode {
  import IntervalQuality._
  override def degreeIntervals: List[Interval] = List(Interval(2, Minor), Interval(2, Major),
    Interval(2, Major), Interval(2, Major), Interval(2, Minor), Interval(2, Major),
    Interval(2, Major))
}

case class Lydian(val root: Pitch) extends Mode {
  import IntervalQuality._
  override def degreeIntervals: List[Interval] = List(Interval(2, Major), Interval(2, Major),
    Interval(2, Major), Interval(2, Minor), Interval(2, Major), Interval(2, Major),
    Interval(2, Minor))
}

case class Mixolydian(val root: Pitch) extends Mode {
  import IntervalQuality._
  override def degreeIntervals: List[Interval] = List(Interval(2, Major), Interval(2, Major),
    Interval(2, Minor), Interval(2, Major), Interval(2, Major), Interval(2, Minor),
    Interval(2, Minor))
}

case class Aeolian(val root: Pitch) extends Mode {
  import IntervalQuality._
  override def degreeIntervals: List[Interval] = List(Interval(2, Major), Interval(2, Minor),
    Interval(2, Major), Interval(2, Major), Interval(2, Minor), Interval(2, Major),
    Interval(2, Major))
}

case class Locrian(val root: Pitch) extends Mode {
  import IntervalQuality._
  override def degreeIntervals: List[Interval] = List(Interval(2, Minor), Interval(2, Major),
    Interval(2, Major), Interval(2, Minor), Interval(2, Major), Interval(2, Major),
    Interval(2, Major))
}
