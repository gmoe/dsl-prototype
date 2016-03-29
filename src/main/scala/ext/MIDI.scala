package rc.dsl.ext

import rc.dsl.Primitives._
import javax.sound.midi.{ShortMessage => jMess}
import javax.sound.midi.{MidiSystem => mSys}
import javax.sound.{midi => jsm}

package object midi {

  import scala.language.implicitConversions

  sealed trait Message {
    def toJava: jsm.MidiMessage    
  }

  case class NoteOn(channel: Int, pitch: Pitch, velocity: Int) extends Message {
    require(channel >= 1 && channel <= 16, "Channel value must be between 1 and 16")
    require(velocity >= 0 && velocity <= 127, "Velocity value must be between 0 and 127")
    def toJava = new jMess(jMess.NOTE_ON, channel-1, pitch.midiNumber, velocity)
  }

  case class NoteOff(channel: Int, pitch: Pitch, velocity: Int) extends Message {
    require(channel >= 1 && channel <= 16, "Channel value must be between 1 and 16")
    require(velocity >= 0 && velocity <= 127, "Velocity value must be between 0 and 127")
    def toJava = new jMess(jMess.NOTE_OFF, channel-1, pitch.midiNumber, velocity)
  }

  implicit def jMessToRc(value: Message): jsm.MidiMessage = value.toJava

  object MidiSystem {

    def printDevices:Unit = mSys.getMidiDeviceInfo().zipWithIndex foreach { 
      case (info:jsm.MidiDevice.Info,i:Int) => {
        val d = mSys.getMidiDevice(info)
        val f: (Int => String) = x => if(x < 0) "inf" else x.toString
        println(s"#$i: ${info.getName} (${info.getVendor}) - "+
          s"Inputs[${f(d.getMaxReceivers)}], "+
          s"Outputs[${f(d.getMaxTransmitters)}]")
      } 
    }

    def getDeviceInput(info: jsm.MidiDevice.Info) = mSys.getMidiDevice(info).getReceiver()
    def getDeviceInput(i: Int) = mSys.getMidiDevice(mSys.getMidiDeviceInfo()(i)).getReceiver()

    def getDeviceOutput(info: jsm.MidiDevice.Info) = mSys.getMidiDevice(info).getTransmitter()
    def getDeviceOutput(i: Int) = mSys.getMidiDevice(mSys.getMidiDeviceInfo()(i)).getTransmitter()

  }

}
