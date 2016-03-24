package rc.dsl

import scala.util.parsing.combinator.RegexParsers

import Primitives._
import Structures._

package object parser {

  object DSLParser extends RegexParsers {

    def pitchClass: Parser[PitchClass] = """([a-g,A-G])""".r ^^ { 
      PitchClass.apply(_) 
    }

    def romanNum: Parser[RomanNum] = """(?i)([iii|ii|iv|vii|vi|v])""".r ^^ {
      RomanNum.apply(_)
    }

    def pitchDecorator: Parser[PitchDecorator] = """([n|#|x|X|\-|_]?)""".r ^^ {
      PitchDecorator.apply(_) 
    }

    def pitchOctave: Parser[Int] = """([,|']*)""".r ^^ { 
      case s => s.count(_ == ''') - s.count(_ == ',')
    }

    //def pitch: Parser[Pitch] = (pitchClass|romanNum) ~ opt(pitchDecorator) ~ opt(pitchOctave) ^^ {
    //  case c ~ d ~ o => Pitch(c, d.getOrElse(PitchDecorator.None), o.getOrElse(0))
    //}
    def pitch: Parser[Pitch] = (pitchClass) ~ opt(pitchDecorator) ~ opt(pitchOctave) ^^ {
      case c ~ d ~ o => Pitch(c, d.getOrElse(PitchDecorator.None), o.getOrElse(0))
    }
    
    def note: Parser[Note] = pitchClass ~ opt(pitchDecorator) ~ 
    opt(pitchOctave) ~ """([\d])""".r ^^ {
      case c ~ d ~ o ~ b => Note(Pitch(c, d.getOrElse(PitchDecorator.None), 
        o.getOrElse(0)), Beat(1, b.toInt))
    }

    def timeSig: Parser[TimeSignature] = "[" ~> """([\d])""".r ~ """([\d])""".r <~ "]" ^^ {
      case n ~ d => TimeSignature(n.toInt, d.toInt)
    }

    def measure: Parser[Measure] = opt("|") ~> opt(timeSig) ~ rep1(note | pitch) <~ "|" ^^ {
      case None ~ p => Measure(TimeSignature(), p:_*)
      case Some(t) ~ p => Measure(t, p:_*)
    }

    def music: Parser[Music] = measure | note | pitch

    def apply(input: String): Music = parseAll(music, input) match {
        case Success(result, _) => result
        case failure : NoSuccess => scala.sys.error(failure.msg)
    }

  }

  implicit class DSLHelper(val sc: StringContext) extends AnyVal {
    def m(args: Any*) = DSLParser(sc.parts(0))
  }

}
