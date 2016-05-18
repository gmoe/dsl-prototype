package rc.dsl.ext

import org.specs2._
import org.specs2.matcher._
import scala.xml._

import rc.dsl.Primitives._

class MusicXmlSpec extends Specification with org.specs2.matcher.XmlMatchers { def is = s2"""

  MusicXML Export Specification
    pitch is correct                 $pitch1
    incorrect pitch is incorrect     $pitch2
                                    """
  val divisions: Int = 8
  def trim = Utility.trimProper(_)

  def pitch1 = trim(MusicXml.makeNode(PitchClass.C`_`4, divisions)) must beEqualToIgnoringSpace (
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
    )

  def pitch2 = trim(MusicXml.makeNode(PitchClass.C`_`5, divisions)) must not be beEqualToIgnoringSpace (
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
    )
}
