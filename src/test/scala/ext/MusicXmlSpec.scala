package rc.dsl.ext

import org.scalatest._
import scala.xml._

import rc.dsl.Primitives._

class MusicXmlSpec extends FlatSpec with Matchers {

  //Divisions per quarter note
  val divisions: Int = 8
  def trim = Utility.trimProper(_)

  "Pitches" should "generate correct XML elements" in {

    trim(MusicXml.makeNode(PitchClass.C`_`4, divisions)) strict_== trim(
      <note>
        <pitch>
          <step>C</step>
          <alter>0</alter>
          <octave>4</octave>
        </pitch>
        <duration>8</duration>
        <type>quarter</type>
        <stem>none</stem>
      </note>
      ) should be (true)

    //This is a double-check, as XML equivalence is very strange
    trim(MusicXml.makeNode(PitchClass.C`#`4, divisions)) strict_== trim(
      <note>
        <pitch>
          <step>C</step>
          <alter>0</alter>
          <octave>4</octave>
        </pitch>
        <duration>8</duration>
        <type>quarter</type>
        <stem>none</stem>
      </note>
    ) should be (false)

    trim(MusicXml.makeNode(PitchClass.G`#`5, divisions)) xml_== trim(<note>
      <pitch>
        <step>G</step>
        <alter>1</alter>
        <octave>5</octave>
      </pitch>
      <duration>4</duration>
      <type>quarter</type>
      <stem>none</stem>
    </note>) should be (true)
  }

}
