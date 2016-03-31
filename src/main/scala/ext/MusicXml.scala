package rc.dsl.ext

import rc.dsl.Primitives._
import rc.dsl.Structures._

import scala.xml._
import scala.xml.dtd.{DocType, PublicID}

object MusicXml {

  trait MusicXmlGen[A <: Music] {
    def parse(obj: A, divisions: Int): Elem
  }
  
  object MusicXmlGen {
    implicit val pitch = new MusicXmlGen[Pitch] {
      def parse(p: Pitch, divisions: Int): Elem = {
        <note>
          <pitch>
            <step>{p.pitchClass}</step>
            <alter>{p.decorator.midiNumber}</alter>
            <octave>{p.octave}</octave>
          </pitch>
          <duration>{divisions}</duration>
          <type>quarter</type>
          <stem>none</stem>
        </note>
      }
    }

    implicit val note = new MusicXmlGen[Note] {
      def parse(n: Note, divisions: Int): Elem = {
        <note>
          <pitch>
            <step>{n.pitch.pitchClass}</step>
            <alter>{n.pitch.decorator.midiNumber}</alter>
            <octave>{n.pitch.octave}</octave>
          </pitch>
          <duration>{ (divisions*4) / n.duration.denom  }</duration>
          <type>{ n.duration.denom match {
              case 1 => "whole"
              case 2 => "half"
              case 4 => "quarter"
              case 8 => "eighth"
              case 16 => "16th"
              case 32 => "32nd"
            } }</type>
        </note>
      }
    }
  }

  def makeNode[A <: Music](m: A, div: Int)(implicit p: MusicXmlGen[A]): Elem = p.parse(m,div)

  private def countDivisions[A <: Music](music: A*): Int = music.foldLeft(1) {
    (m, x) => x match {
      case n: Note => (n.duration.denom/4) max m
      case p: Pitch => 1 max m
    }
  }

  def writeXml[A <: Music](fileName: String, m: A*)(implicit p: MusicXmlGen[A]): Unit = {
    val divisions = countDivisions(m:_*)

    val xmlOutput = <score-partwise version="3.0">
			<part-list>
				<score-part id="P1">
					<part-name>Music</part-name>
				</score-part>
			</part-list>
			<part id="P1">
				<measure number="1">
					<attributes>
						<divisions>{divisions}</divisions>
						<key>
							<fifths>0</fifths>
						</key>
						<clef>
							<sign>G</sign>
							<line>2</line>
						</clef>
					</attributes>
          { m.map { p.parse(_, divisions) }
          }
				</measure>
			</part>
		</score-partwise>


    val doctype = DocType("score-partwise",
      PublicID("-//Recordare//DTD MusicXML 3.0 Partwise//EN",
      "http://www.musicxml.org/dtds/partwise.dtd"), Nil)

    XML.save(fileName, xmlOutput, "UTF-8", true, doctype)
  }

}
