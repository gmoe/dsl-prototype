package rc.dsl.gen

import scala.util.Random

package object markov {

  case class MarkovChain[+A](source: A, dest: A, chance: Double) {
    override def toString: String = s"${source} -[${chance}]-> ${dest}"
  }

  case class MarkovUniverse[A](init: A, chains: MarkovChain[A]*) {
    def cycle(state: A): A = {
      val r = (new Random).nextDouble
      val stateChains = chains.filter(_.source == state)
      val cdf = 0.0 +: (0.0 +: stateChains.map(_.chance)).sliding(2,1).map(_.sum).toList 
      val cdfTupled = cdf.sliding(2,1).map({ case List(a,b) => (a,b) })
      return stateChains(cdfTupled.indexWhere({case (a,b) => a <= r && r <= b})).dest
    }
    lazy val stream: Stream[A] = init #:: cycle(init) #:: stream.tail.map(cycle)
  }

  implicit class MarkovChainSyntax[+A](val source: A) extends AnyVal {
    def markov[B >: A](dest: B)(chance: Double): MarkovChain[B] = MarkovChain[B](source, dest, chance)
  }
}
