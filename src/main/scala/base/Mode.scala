package rc.dsl

import Primitives._

sealed trait Mode extends IsEnharmonic[Mode] {
  import IntervalQuality._

  def degreeIntervals: List[Interval]
  
  //TODO: Maybe not use Pitch?
  def root: Pitch
  def I: Pitch = root
  def II: Pitch = (degreeIntervals take 1).foldLeft(root) { (c, a) => a.fromPitch(c) }
  def III: Pitch = (degreeIntervals take 2).foldLeft(root) { (c, a) => a.fromPitch(c) }
  def IV: Pitch = (degreeIntervals take 3).foldLeft(root) { (c, a) => a.fromPitch(c) }
  def V: Pitch = (degreeIntervals take 4).foldLeft(root) { (c, a) => a.fromPitch(c) }
  def VI: Pitch = (degreeIntervals take 5).foldLeft(root) { (c, a) => a.fromPitch(c) }
  def VII: Pitch = (degreeIntervals take 6).foldLeft(root) { (c, a) => a.fromPitch(c) }

  def degrees = Set(I, II, III, IV, V, VI, VII)

  def isEnharmonic(that: Mode) = {
    def flattenOctaves(p:Pitch):Pitch = Pitch(p.pitchClass, p.decorator, 4)

    this.degrees.map(flattenOctaves) == that.degrees.map(flattenOctaves)
  }

  def apply(r: RomanNum): Pitch = r match {
    case RomanNum.I => this.I
    case RomanNum.II => this.II
    case RomanNum.III => this.III
    case RomanNum.IV => this.IV
    case RomanNum.V => this.V
    case RomanNum.VI => this.VI
    case RomanNum.VII => this.VII
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
