package rc.dsl

object Primitives {

  sealed trait Music

  sealed case class Beat(num: Int = 1, denom: Int = 4)
  sealed case class TimeSignature(num: Int = 4, denom: Int = 4)

  sealed trait Primitive extends Music
  sealed case class Rest() extends Primitive

  sealed trait PitchPrimitive extends Primitive
  sealed abstract class RomanNum extends PitchPrimitive
  sealed abstract class PitchClass extends PitchPrimitive {
    def midiNumber: Int
  }
  sealed abstract class PitchDecorator {
    def midiNumber: Int
  }

  trait IsEnharmonic[A] {
    def isEnharmonic(that: A): Boolean
    def ~=(that: A): Boolean = isEnharmonic(that)
  }

  case class Pitch(val pitchClass: PitchClass, val decorator: PitchDecorator, val octave: Int)
    extends Primitive 
    with Ordered[Pitch] 
    with IsEnharmonic[Pitch] {

    override def toString = pitchClass.toString + decorator.toString + "(" + octave + ")"

    val midiNumber = octave * 12 + pitchClass.midiNumber + decorator.midiNumber

    def compare(that: Pitch) = this.midiNumber - that.midiNumber

    def isEnharmonic(that: Pitch) = this.midiNumber == that.midiNumber

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

  object RomanNum {
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

    val values = Stream.continually(List(I,II,III,IV,V,VI,VII)).flatten
  }

  object PitchClass {
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

    val values = Stream.continually(List(C,D,E,F,G,A,B)).flatten
  }

  object PitchDecorator {
    case object None extends PitchDecorator {
      override def toString = ""
      override def midiNumber: Int = 0
    }
    case object Natural extends PitchDecorator {
      override def toString = "♮ "
      override def midiNumber: Int = 0
    }
    case object Sharp extends PitchDecorator {
      override def toString = "♯ "
      override def midiNumber: Int = 1
    }
    case object Flat extends PitchDecorator {
      override def toString = "♭ "
      override def midiNumber: Int = -1
    }
    case object DoubleSharp extends PitchDecorator {
      override def toString = "𝄪"
      override def midiNumber: Int = 2
    }
    case object DoubleFlat extends PitchDecorator {
      override def toString = "𝄫"
      override def midiNumber: Int = -2
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

  //TODO: Finalize the syntax for these methods
  implicit class PitchBuilder(val pc: PitchClass) extends AnyVal {
    def sharp(oct: Integer): Pitch = Pitch(this.pc, PitchDecorator.Sharp, oct)
    def ♯(oct: Integer): Pitch = Pitch(this.pc, PitchDecorator.Sharp, oct)
    def `#`(oct: Integer): Pitch = Pitch(this.pc, PitchDecorator.Sharp, oct)
    def flat(oct: Integer): Pitch = Pitch(this.pc, PitchDecorator.Flat, oct)
    def sharp2(oct: Integer): Pitch = Pitch(this.pc, PitchDecorator.DoubleSharp, oct)
    def flat2(oct: Integer): Pitch = Pitch(this.pc, PitchDecorator.DoubleFlat, oct)
    def natural(oct: Integer): Pitch = Pitch(this.pc, PitchDecorator.Natural, oct)

    def _1(): Pitch = Pitch(this.pc, PitchDecorator.None, 1)
    def _2(): Pitch = Pitch(this.pc, PitchDecorator.None, 2)
    def _3(): Pitch = Pitch(this.pc, PitchDecorator.None, 3)
    def _4(): Pitch = Pitch(this.pc, PitchDecorator.None, 4)
    def _5(): Pitch = Pitch(this.pc, PitchDecorator.None, 5)
    def _6(): Pitch = Pitch(this.pc, PitchDecorator.None, 6)
    def _7(): Pitch = Pitch(this.pc, PitchDecorator.None, 7)
    def _8(): Pitch = Pitch(this.pc, PitchDecorator.None, 8)
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
