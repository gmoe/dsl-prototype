package rc.dsl

import Structures._

sealed trait Mode  {

  //TODO: Maybe not use Pitch?
  val root: Pitch
  val I: Pitch
  val II: Pitch
  val III: Pitch
  val IV: Pitch
  val V: Pitch
  val VI: Pitch
  val VII: Pitch
}

case class Ionian(val root: Pitch) extends Mode {
  import IntervalQuality._
  private val degreeIntervals: List[Interval] = List(Interval(2, Major), Interval(2, Major),
    Interval(2, Minor), Interval(2, Major), Interval(2, Major), Interval(2, Major),
    Interval(2, Minor))
  val I: Pitch = root
  val II: Pitch = (degreeIntervals take 1).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val III: Pitch = (degreeIntervals take 2).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val IV: Pitch = (degreeIntervals take 3).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val V: Pitch = (degreeIntervals take 4).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val VI: Pitch = (degreeIntervals take 5).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val VII: Pitch = (degreeIntervals take 6).foldLeft(root) { (c, a) => a.fromPitch(c) }
}

case class Dorian(val root: Pitch) extends Mode {
  import IntervalQuality._
  private val degreeIntervals: List[Interval] = List(Interval(2, Major), Interval(2, Minor),
    Interval(2, Major), Interval(2, Major), Interval(2, Major), Interval(2, Minor),
    Interval(2, Major))
  val I: Pitch = root
  val II: Pitch = (degreeIntervals take 1).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val III: Pitch = (degreeIntervals take 2).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val IV: Pitch = (degreeIntervals take 3).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val V: Pitch = (degreeIntervals take 4).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val VI: Pitch = (degreeIntervals take 5).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val VII: Pitch = (degreeIntervals take 6).foldLeft(root) { (c, a) => a.fromPitch(c) }
}

case class Phrygian(val root: Pitch) extends Mode {
  import IntervalQuality._
  private val degreeIntervals: List[Interval] = List(Interval(2, Minor), Interval(2, Major),
    Interval(2, Major), Interval(2, Major), Interval(2, Minor), Interval(2, Major),
    Interval(2, Major))
  val I: Pitch = root
  val II: Pitch = (degreeIntervals take 1).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val III: Pitch = (degreeIntervals take 2).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val IV: Pitch = (degreeIntervals take 3).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val V: Pitch = (degreeIntervals take 4).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val VI: Pitch = (degreeIntervals take 5).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val VII: Pitch = (degreeIntervals take 6).foldLeft(root) { (c, a) => a.fromPitch(c) }
}

case class Lydian(val root: Pitch) extends Mode {
  import IntervalQuality._
  private val degreeIntervals: List[Interval] = List(Interval(2, Major), Interval(2, Major),
    Interval(2, Major), Interval(2, Minor), Interval(2, Major), Interval(2, Major),
    Interval(2, Minor))
  val I: Pitch = root
  val II: Pitch = (degreeIntervals take 1).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val III: Pitch = (degreeIntervals take 2).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val IV: Pitch = (degreeIntervals take 3).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val V: Pitch = (degreeIntervals take 4).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val VI: Pitch = (degreeIntervals take 5).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val VII: Pitch = (degreeIntervals take 6).foldLeft(root) { (c, a) => a.fromPitch(c) }
}

case class Mixolydian(val root: Pitch) extends Mode {
  import IntervalQuality._
  private val degreeIntervals: List[Interval] = List(Interval(2, Major), Interval(2, Major),
    Interval(2, Minor), Interval(2, Major), Interval(2, Major), Interval(2, Minor),
    Interval(2, Minor))
  val I: Pitch = root
  val II: Pitch = (degreeIntervals take 1).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val III: Pitch = (degreeIntervals take 2).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val IV: Pitch = (degreeIntervals take 3).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val V: Pitch = (degreeIntervals take 4).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val VI: Pitch = (degreeIntervals take 5).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val VII: Pitch = (degreeIntervals take 6).foldLeft(root) { (c, a) => a.fromPitch(c) }
}

case class Aeolian(val root: Pitch) extends Mode {
  import IntervalQuality._
  private val degreeIntervals: List[Interval] = List(Interval(2, Major), Interval(2, Minor),
    Interval(2, Major), Interval(2, Major), Interval(2, Minor), Interval(2, Major),
    Interval(2, Major))
  val I: Pitch = root
  val II: Pitch = (degreeIntervals take 1).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val III: Pitch = (degreeIntervals take 2).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val IV: Pitch = (degreeIntervals take 3).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val V: Pitch = (degreeIntervals take 4).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val VI: Pitch = (degreeIntervals take 5).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val VII: Pitch = (degreeIntervals take 6).foldLeft(root) { (c, a) => a.fromPitch(c) }
}

case class Locrian(val root: Pitch) extends Mode {
  import IntervalQuality._
  private val degreeIntervals: List[Interval] = List(Interval(2, Minor), Interval(2, Major),
    Interval(2, Major), Interval(2, Minor), Interval(2, Major), Interval(2, Major),
    Interval(2, Major))
  val I: Pitch = root
  val II: Pitch = (degreeIntervals take 1).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val III: Pitch = (degreeIntervals take 2).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val IV: Pitch = (degreeIntervals take 3).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val V: Pitch = (degreeIntervals take 4).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val VI: Pitch = (degreeIntervals take 5).foldLeft(root) { (c, a) => a.fromPitch(c) }
  val VII: Pitch = (degreeIntervals take 6).foldLeft(root) { (c, a) => a.fromPitch(c) }
}
