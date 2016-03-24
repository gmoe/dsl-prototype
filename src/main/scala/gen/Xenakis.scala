package rc.dsl.gen

package object sieve {
  import scala.language.implicitConversions

  case class Residual(m: Int, i: Int) {
    def sieve(range: Range): IndexedSeq[Int] = range.filter { x => Math.abs(x % m) == i }
  }

  case class IntWrapper(value: Int) {
    def `@`(i: Int): Residual = Residual(value, i)
  }

  implicit def wrapIntResidual(value: Int) = new IntWrapper(value)
}
