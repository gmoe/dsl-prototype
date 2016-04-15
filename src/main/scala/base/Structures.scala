package rc.dsl

import rc.dsl.Primitives._

object Structures {
  // TODO: Considering implementing...
  // Measure
  // Part/Voice
  // Chord
  
  /* Sequencing/Chaining Notes ((C4 -+- E4 -+- G4) == Measure(C4 E4 G4))
   * should be injected by monoids)
   */
  def |(timeSig: TimeSignature, music: Primitive*): Measure = Measure(timeSig, music:_*)

  case class Measure(timeSig: TimeSignature, music: Primitive*) {

  }

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
