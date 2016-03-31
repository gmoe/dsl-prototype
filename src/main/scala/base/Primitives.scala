package rc.dsl

object Primitives {

  sealed trait Music

  sealed case class Beat(num: Int = 1, denom: Int = 4) {
    require((denom & (denom-1)) == 0, "Denominator must be a power of two (1,2,4,8,...)")
  }
  sealed case class TimeSignature(num: Int = 4, denom: Int = 4) {
    require((denom & (denom-1)) == 0, "Denominator must be a power of two (1,2,4,8,...)")
  }

  sealed trait Primitive extends Music
  sealed case class Rest() extends Primitive

  sealed trait PitchPrimitive extends Primitive
  sealed abstract class RomanNum extends PitchPrimitive
  sealed abstract class PitchClass extends PitchPrimitive with HasMidiNumber
  sealed abstract class PitchDecorator extends HasMidiNumber

  trait HasMidiNumber {
    def midiNumber: Int
  }

  trait IsEnharmonic[A] {
    def isEnharmonic(that: A): Boolean
    def ~=(that: A): Boolean = isEnharmonic(that)
  }

  trait CanStream[A] {
    def stream: Stream[A]
    def streamFrom(start: A): Stream[A]
  }

  case class Pitch(val pitchClass: PitchClass, val decorator: PitchDecorator, val octave: Int)
    extends Primitive with Ordered[Pitch] with IsEnharmonic[Pitch] with HasMidiNumber {

    override def toString = s"$pitchClass$decorator${toSubScript(octave)}"
    def midiNumber = octave * 12 + pitchClass.midiNumber + decorator.midiNumber
    def compare(that: Pitch) = this.midiNumber - that.midiNumber
    def isEnharmonic(that: Pitch) = this.midiNumber == that.midiNumber
    private def toSubScript(i: Int) = i.toString.map { c => c match {
        case '0' => '\u2080'
        case '1' => '\u2081'
        case '2' => '\u2082'
        case '3' => '\u2083'
        case '4' => '\u2084'
        case '5' => '\u2085'
        case '6' => '\u2086'
        case '7' => '\u2087'
        case '8' => '\u2088'
        case '9' => '\u2089'
        case _ => ???
      }
    }.mkString
  }

  object Pitch {
    def apply(pc: PitchClass, oct: Int): Pitch = Pitch(pc, PitchDecorator.None, oct)
  }

  //TODO: Possibly support RomanNums
  case class Note(val pitch: Pitch, val duration: Beat) extends Music

  /* Sequencing/Chaining Notes ((C4 -+- E4 -+- G4) == Measure(C4 E4 G4))
   * should be injected by monoids)
   */
  case class Measure(timeSig: TimeSignature, music: Music*) extends Music with Traversable[Music] {
    override def foreach[U](f: Music => U) = music foreach f
    override def toString: String = s"Measure($timeSig, $music)"
  }

  object RomanNum extends CanStream[RomanNum] {
    case object I extends RomanNum
    case object II extends RomanNum
    case object III extends RomanNum
    case object IV extends RomanNum
    case object V extends RomanNum
    case object VI extends RomanNum
    case object VII extends RomanNum

    /** TODO: RomanNum has no support for quality (M, m, sharp, etc.) */
    def apply(s: String): RomanNum = s.toUpperCase match {
      case "I" => I
      case "II" => II
      case "III" => III
      case "IV" => IV
      case "V" => V
      case "VI" => VI
      case "VII" => VII
    }

    def stream = Stream.continually(List(I,II,III,IV,V,VI,VII)).flatten
    def streamFrom(start: RomanNum) = stream.drop(stream.indexOf(start))
  }

  object PitchClass extends CanStream[PitchClass] {
    case object C extends PitchClass {
      override def midiNumber: Int = 0
    }
    case object D extends PitchClass {
      override def midiNumber: Int = 2
    }
    case object E extends PitchClass {
      override def midiNumber: Int = 4
    }
    case object F extends PitchClass {
      override def midiNumber: Int = 5
    }
    case object G extends PitchClass {
      override def midiNumber: Int = 7
    }
    case object A extends PitchClass {
      override def midiNumber: Int = 9
    }
    case object B extends PitchClass {
      override def midiNumber: Int = 11
    }

    def apply(s: String): PitchClass = s.toUpperCase match {
      case "C" => C
      case "D" => D
      case "E" => E
      case "F" => F
      case "G" => G
      case "A" => A
      case "B" => B
    }

    def stream = Stream.continually(List(C,D,E,F,G,A,B)).flatten
    def streamFrom(start: PitchClass) = stream.drop(stream.indexOf(start))
  }

  object PitchDecorator {
    case object None extends PitchDecorator {
      override def toString = ""
      def midiNumber: Int = 0
    }
    case object Natural extends PitchDecorator {
      override def toString = "♮"
      def midiNumber: Int = 0
    }
    case object Sharp extends PitchDecorator {
      override def toString = "♯"
      def midiNumber: Int = 1
    }
    case object Flat extends PitchDecorator {
      override def toString = "♭"
      def midiNumber: Int = -1
    }
    case object DoubleSharp extends PitchDecorator {
      override def toString = "𝄪"
      def midiNumber: Int = 2
    }
    case object DoubleFlat extends PitchDecorator {
      override def toString = "𝄫"
      def midiNumber: Int = -2
    }

    def apply(s: String): PitchDecorator = s match {
      case ""  => None
      case "#" => Sharp
      case "n" => Natural
      case "x" => DoubleSharp
      case "-" => Flat
      case "_" => DoubleFlat
    }

    def apply(i: Int): PitchDecorator = i match {
      case -2 => DoubleFlat
      case -1 => Flat
      case 0 => None 
      case 1 => Sharp
      case 2 => DoubleSharp
    }

  }

  implicit class PitchBuilder(val pc: PitchClass) extends AnyVal {
    def `bb`(oct: Integer): Pitch = Pitch(this.pc, PitchDecorator.DoubleFlat, oct)
    def `b`(oct: Integer): Pitch = Pitch(this.pc, PitchDecorator.Flat, oct)
    def `_`(oct: Integer): Pitch = Pitch(this.pc, PitchDecorator.None, oct)
    def `n`(oct: Integer): Pitch = Pitch(this.pc, PitchDecorator.Natural, oct)
    def `#`(oct: Integer): Pitch = Pitch(this.pc, PitchDecorator.Sharp, oct)
    def `##`(oct: Integer): Pitch = Pitch(this.pc, PitchDecorator.DoubleSharp, oct)

    def flat2(oct: Integer): Pitch = Pitch(this.pc, PitchDecorator.DoubleFlat, oct)
    def flat(oct: Integer): Pitch = Pitch(this.pc, PitchDecorator.Flat, oct)
    def natural(oct: Integer): Pitch = Pitch(this.pc, PitchDecorator.Natural, oct)
    def sharp(oct: Integer): Pitch = Pitch(this.pc, PitchDecorator.Sharp, oct)
    def sharp2(oct: Integer): Pitch = Pitch(this.pc, PitchDecorator.DoubleSharp, oct)
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
