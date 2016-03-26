package rc.dsl.gen

package object sieve {
  import scala.language.implicitConversions

  /* TODO:
   * Infinite range <- ... -2, -1, 0, 1, 2 ... ->
   * Support for Pitch
   * Support for Beat
   */

  sealed trait Sieve {
    def apply(r: Range): IndexedSeq[Int] = sieve(r,this)
    def unary_-(): Sieve = Complement(this)
  }
  case class Residual(m: Int, i: Int) extends Sieve
  case class Union(a: Sieve, b: Sieve) extends Sieve
  case class Intersection(a: Sieve, b: Sieve) extends Sieve
  case class Difference(a: Sieve, b: Sieve) extends Sieve
  case class Complement(a: Sieve) extends Sieve

  def sieve(range: Range, sieve: Sieve): IndexedSeq[Int] = sieve match {
    case r: Residual => {
      if(r.m == 0 && r.i == 0) IndexedSeq()
      else range.filter { x => Math.abs(x % r.m) == r.i }
    }
    case u: Union => (u.a(range) union u.b(range)).sorted.distinct
    case i: Intersection => i.a(range) intersect i.b(range)
    case d: Difference => d.a(range) diff d.b(range)
    case c: Complement => Residual(1,0).apply(range) diff c.a(range)
  }

  case class IntWrapper(value: Int) {
    def `@`(i: Int): Residual = Residual(value, i)
  }

  implicit def wrapIntResidual(value: Int) = new IntWrapper(value)

  implicit class SieveTransforms(val a: Sieve) extends AnyVal {
    def |(b: Sieve): Sieve = Union(a,b)
    def &(b: Sieve): Sieve = Intersection(a,b)
    def ^(b: Sieve): Sieve = Difference(a,b)
  }
}
