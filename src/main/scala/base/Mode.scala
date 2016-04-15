package rc.dsl

import scalaz._, Isomorphism._
import Primitives._

sealed trait Mode extends IsoSet[RomanNum, Pitch] with IsEnharmonic[Mode] {
  import IntervalQuality._

  def degreeIntervals: List[Interval]
  
  //TODO: Maybe not use Pitch?
  val root: Pitch
  val I: Pitch = root
  val II: Pitch = (degreeIntervals take 1).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val III: Pitch = (degreeIntervals take 2).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val IV: Pitch = (degreeIntervals take 3).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val V: Pitch = (degreeIntervals take 4).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val VI: Pitch = (degreeIntervals take 5).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val VII: Pitch = (degreeIntervals take 6).foldLeft(root) { (c, a) => a.fromPitch(c) }

  def degrees = Set(I, II, III, IV, V, VI, VII)

  def isEnharmonic(that: Mode) = {
    def flattenOctaves(p:Pitch) = Pitch(p.pitchClass, p.decorator, 4)

    this.degrees.map(flattenOctaves) == that.degrees.map(flattenOctaves)
  }

  def to: RomanNum => Pitch = {
    case RomanNum.I => this.I
    case RomanNum.II => this.II
    case RomanNum.III => this.III
    case RomanNum.IV => this.IV
    case RomanNum.V => this.V
    case RomanNum.VI => this.VI
    case RomanNum.VII => this.VII
  }

  def from: Pitch => RomanNum = {
    //TODO: This needs to be more robust
    case this.I => RomanNum.I
    case this.II => RomanNum.II
    case this.III => RomanNum.III
    case this.IV => RomanNum.IV
    case this.V => RomanNum.V
    case this.VI => RomanNum.VI
    case this.VII => RomanNum.VII
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
