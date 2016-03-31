package rc.dsl

import rc.dsl.Primitives._

object Structures {
  // TODO: Considering implementing...
  // Measure
  // Part/Voice
  // Chord

  implicit class SeqEnriched[A](val value: Seq[A]) extends AnyVal {
    def rotate(i: Int): Seq[A] = value.drop(i) ++ value.take(i)
    def retrograde: Seq[A] = value ++ value.reverse
    def test[A <: Music](b: Int) = ???
  }

}
