package rc.dsl

import rc.dsl.Primitives._

object Structures {
  // TODO: Considering implementing...
  // Part/Voice
  // Chord
  
  sealed trait MeasureMarker extends Music
  final case object `|` extends MeasureMarker
  final case object `|:` extends MeasureMarker
  final case object `:|` extends MeasureMarker
  final case object `:|:` extends MeasureMarker

  case class Measure(timeSig: TimeSignature, music: Note*) {
    require(isFullMeasure(timeSig, music:_*), "Measure has to be complete.")
  }

  def isFullMeasure(ts: TimeSignature, music: Note*): Boolean = music.foldLeft(0.0) { 
    case (sum, note) => sum + note.duration.num.toDouble / note.duration.denom.toDouble
  } == (ts.num.toDouble / ts.denom.toDouble)

  implicit class SeqEnriched[A](val value: Seq[A]) extends AnyVal {
    def rotate(i: Int) = value.drop(i) ++ value.take(i)
    def retrograde = value.reverse
  }

  implicit class ChordBuilder(val p: Pitch) extends AnyVal {
    import IntervalQuality._

    def P5(): List[Pitch] = p :: Interval(5, Perfect).fromPitch(p) :: Nil

    def majTriad: List[Pitch] = p :: Interval(3, Major).fromPitch(p) ::
      Interval(5, Perfect).fromPitch(p) :: Nil

    def minTriad: List[Pitch] = p :: Interval(3, Minor).fromPitch(p) ::
      Interval(5, Perfect).fromPitch(p) :: Nil

  }

}
