package rc.dsl.ext

import org.scalatest._

import rc.dsl.Primitives._

class MusicXmlSpec extends FlatSpec with Matchers {

  //Divisions per quarter note
  val div = 8

  "Pitches" should "generate correct XML elements" in {
    //TODO: This passes in all cases??
    (MusicXml.makeNode(Pitch(PitchClass.C, PitchDecorator.None, 4), div)) should be
        (<note>
          <pitch>
            <step>C</step>
            <alter>0</alter>
            <octave>4</octave>
          </pitch>
          <duration>4</duration>
          <type>quarter</type>
          <stem>none</stem>
        </note>)

    (MusicXml.makeNode(Pitch(PitchClass.G, PitchDecorator.Sharp, 5), div)) should be
        (<note>
          <pitch>
            <step>G</step>
            <alter>1</alter>
            <octave>5</octave>
          </pitch>
          <duration>4</duration>
          <type>quarter</type>
          <stem>none</stem>
        </note>)
  }

}
