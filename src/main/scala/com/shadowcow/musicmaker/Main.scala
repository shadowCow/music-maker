package com.shadowcow.musicmaker

import javax.sound.midi.MidiSystem
import javax.sound.sampled.{AudioSystem, Line, Mixer}

object Main extends App {
//  MyDeviceInfo.describeAudioSystem()
//  MyDeviceInfo.describeMidiSystem()

//  MidiPlayground.basicSynthExample()
//  MidiPlayground.sequencerExample()

  //  MidiPlayground.testInstruments()
//  MidiPlayground.testPercussion()
//  MidiPlayground.testEffects()
//  MidiPlayground.testTelephone()
//  MidiPlayground.testChoir()
  MidiPlayground.testWavey()
}

object MyDeviceInfo {
  def describeMidiSystem(): Unit = {
    val midiDeviceInfos = MidiSystem.getMidiDeviceInfo
    midiDeviceInfos.foreach(deviceInfo => {
      println(s"name: ${deviceInfo.getName} description: ${deviceInfo.getDescription} vendor: ${deviceInfo.getVendor}")
    })

    val synth = MidiSystem.getSynthesizer
    printHeader("Available Instruments:")
    synth.getAvailableInstruments.foreach(println)

    printHeader("Loaded Instruments:")
    synth.getLoadedInstruments.foreach(println)

    //    printHeader("Channels:")
    //    synth.getChannels.foreach(println)
    //
    //    printHeader("Voice Status:")
    //    synth.getVoiceStatus.foreach(vs => println(s"${vs.bank}"))
  }

  def describeAudioSystem(): Unit = {
    val mixerInfos = AudioSystem.getMixerInfo()
    mixerInfos.foreach(mi => {
      println(s"name: ${mi.getName()} description: ${mi.getDescription} vendor: ${mi.getVendor}")
    })

    val defaultDevice = AudioSystem.getMixer(mixerInfos(0))
    describeMixer(defaultDevice)

    val portSpeakers = AudioSystem.getMixer(mixerInfos(4))
    describeMixer(portSpeakers)

    val speakers = AudioSystem.getMixer(mixerInfos(2))
    describeMixer(speakers)
  }

  def describeMixer(mixer: Mixer): Unit = {
    printHeader(s"Mixer: ${mixer.getMixerInfo.getName}")
    printHeader("Source Lines:")
    mixer.getSourceLineInfo().foreach(lineInfo => {
      val line = mixer.getLine(lineInfo)
      describeLine(line)
    })

    printHeader("Target Lines:")
    mixer.getTargetLineInfo().foreach(lineInfo => {
      val line = mixer.getLine(lineInfo)
      describeLine(line)
    })

    printHeader("Controls:")
    mixer.getControls.foreach(println)
  }

  def describeLine(line: Line): Unit = {
    println(s"Line: ${line.getLineInfo}")
    println(s"Controls: ${line.getControls.mkString(", ")}")
  }


  def printHeader(text: String): Unit = {
    println()
    println("------------------")
    println(text)
  }
}
