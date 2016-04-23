package rc.dsl

object Primitives {

  trait Music

  sealed abstract class Beat(fractValue: (Int,Int)) {
    require((fractValue._2 & (fractValue._2-1)) == 0, "Denominator must be a power of two.")
    val num: Int = fractValue._1
    val denom: Int = fractValue._2
  }

  final case class DottedBeat(beat: Beat) extends Beat((beat.num+2, beat.denom*2)) 

  object Beat {
    final case object Whole extends Beat((1,1))
    final case object Half extends Beat((1,2))
    final case object Quarter extends Beat((1,4))
    final case object Eighth extends Beat((1,8))
    final case object Sixteenth extends Beat((1,16))
    final case object ThirtySecond extends Beat((1,32))
    final case object SixtyFourth extends Beat((1,64))
    final case object HundredTwentyEighth extends Beat((1,128))
    final case object TwoHundredFiftySixth extends Beat((1,256))

    def apply(i: Int): Beat = i match {
      case 1 => Whole
      case 2 => Half
      case 4 => Quarter
      case 8 => Eighth
      case 16 => Sixteenth
      case 32 => ThirtySecond
      case 64 => SixtyFourth
      case 128 => HundredTwentyEighth
      case 256 => TwoHundredFiftySixth
    }
  }

  sealed case class TimeSignature(num: Int, denom: Int) {
    require((denom & (denom-1)) == 0, "Denominator must be a power of two (1,2,4,8,...)")
  }

  sealed trait Primitive extends Music
  sealed case class Rest() extends Primitive

  sealed abstract class PitchPrimitive(val decorator: PitchDecorator, val octave: Int) 
  extends Primitive {
    require(octave >= 0 && octave <= 8, "Octave range limited to 0 - 8")

    protected def toSubScript(i: Int) = i.toString.map { c => c match {
        case '0' => '\u2080'; case '1' => '\u2081'; case '2' => '\u2082';
        case '3' => '\u2083'; case '4' => '\u2084'; case '5' => '\u2085';
        case '6' => '\u2086'; case '7' => '\u2087'; case '8' => '\u2088';
        case '9' => '\u2089'
      }
    }.mkString
  }
  sealed abstract class RomanNum
  sealed abstract class PitchClass extends HasMidiNumber
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

  final case class Pitch(val pitchClass: PitchClass, override val decorator: PitchDecorator, 
    override val octave: Int) extends PitchPrimitive(decorator, octave) with Ordered[Pitch] 
    with IsEnharmonic[Pitch] with HasMidiNumber {

    override def toString = s"$pitchClass$decorator${toSubScript(octave)}"
    def midiNumber = octave * 12 + pitchClass.midiNumber + decorator.midiNumber
    def compare(that: Pitch) = this.midiNumber - that.midiNumber
    def isEnharmonic(that: Pitch) = this.midiNumber == that.midiNumber
  }

  final case class RomanPitch(val romanNum: RomanNum, override val decorator: PitchDecorator,
    override val octave: Int) extends PitchPrimitive(decorator, octave) {

    override def toString = s"$romanNum$decorator${toSubScript(octave)}"
  }

  case class Note(pitch: Pitch, duration: Beat) extends Music

  case class Chord(duration: Beat, pitches: Pitch*) extends Music {
    def invert: Chord = {
      val inv = Interval(8, IntervalQuality.Perfect).fromPitch(pitches.sorted.head)
      Chord(duration, (pitches.sorted.drop(1) :+ inv):_*)
    }
  }

  object RomanNum extends CanStream[RomanNum] {
    final case object I extends RomanNum
    final case object II extends RomanNum
    final case object III extends RomanNum
    final case object IV extends RomanNum
    final case object V extends RomanNum
    final case object VI extends RomanNum
    final case object VII extends RomanNum

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
    final case object C extends PitchClass {
      override def midiNumber: Int = 0
    }
    final case object D extends PitchClass {
      override def midiNumber: Int = 2
    }
    final case object E extends PitchClass {
      override def midiNumber: Int = 4
    }
    final case object F extends PitchClass {
      override def midiNumber: Int = 5
    }
    final case object G extends PitchClass {
      override def midiNumber: Int = 7
    }
    final case object A extends PitchClass {
      override def midiNumber: Int = 9
    }
    final case object B extends PitchClass {
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
    final case object None extends PitchDecorator {
      override def toString = ""
      def midiNumber: Int = 0
    }
    final case object Natural extends PitchDecorator {
      override def toString = "♮"
      def midiNumber: Int = 0
    }
    final case object Sharp extends PitchDecorator {
      override def toString = "♯"
      def midiNumber: Int = 1
    }
    final case object Flat extends PitchDecorator {
      override def toString = "♭"
      def midiNumber: Int = -1
    }
    final case object DoubleSharp extends PitchDecorator {
      override def toString = "𝄪"
      def midiNumber: Int = 2
    }
    final case object DoubleFlat extends PitchDecorator {
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
    def `x`(oct: Integer): Pitch = Pitch(this.pc, PitchDecorator.DoubleSharp, oct)

    def flat2(oct: Integer): Pitch = Pitch(this.pc, PitchDecorator.DoubleFlat, oct)
    def flat(oct: Integer): Pitch = Pitch(this.pc, PitchDecorator.Flat, oct)
    def natural(oct: Integer): Pitch = Pitch(this.pc, PitchDecorator.Natural, oct)
    def sharp(oct: Integer): Pitch = Pitch(this.pc, PitchDecorator.Sharp, oct)
    def sharp2(oct: Integer): Pitch = Pitch(this.pc, PitchDecorator.DoubleSharp, oct)
  }

  //TODO: Possible use for Scala macros?
  implicit class RomanPitchBuilder(val rn: RomanNum) extends AnyVal {
    def `bb`(oct: Integer): RomanPitch = RomanPitch(this.rn, PitchDecorator.DoubleFlat, oct)
    def `b`(oct: Integer): RomanPitch = RomanPitch(this.rn, PitchDecorator.Flat, oct)
    def `_`(oct: Integer): RomanPitch = RomanPitch(this.rn, PitchDecorator.None, oct)
    def `n`(oct: Integer): RomanPitch = RomanPitch(this.rn, PitchDecorator.Natural, oct)
    def `#`(oct: Integer): RomanPitch = RomanPitch(this.rn, PitchDecorator.Sharp, oct)
    def `##`(oct: Integer): RomanPitch = RomanPitch(this.rn, PitchDecorator.DoubleSharp, oct)
    def `x`(oct: Integer): RomanPitch = RomanPitch(this.rn, PitchDecorator.DoubleSharp, oct)

    def flat2(oct: Integer): RomanPitch = RomanPitch(this.rn, PitchDecorator.DoubleFlat, oct)
    def flat(oct: Integer): RomanPitch = RomanPitch(this.rn, PitchDecorator.Flat, oct)
    def natural(oct: Integer): RomanPitch = RomanPitch(this.rn, PitchDecorator.Natural, oct)
    def sharp(oct: Integer): RomanPitch = RomanPitch(this.rn, PitchDecorator.Sharp, oct)
    def sharp2(oct: Integer): RomanPitch = RomanPitch(this.rn, PitchDecorator.DoubleSharp, oct)
  }
}
