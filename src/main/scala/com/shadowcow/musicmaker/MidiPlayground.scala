package com.shadowcow.musicmaker

import javax.sound.midi.{MidiEvent, MidiSystem, Sequence, ShortMessage, Synthesizer, Track}

object MidiPlayground {
  def basicSynthExample(): Unit = {
    val synth = MidiSystem.getSynthesizer
    if (!synth.isOpen) { synth.open() }

    println(s"latency: ${synth.getLatency}")
    println(s"maxPolyphony: ${synth.getMaxPolyphony}")


    val initialPosition = synth.getMicrosecondPosition
    println(s"initialPosition ${initialPosition}")

    val velocityTimestampPairs = Seq(
      (60, 1),
      (61, 2),
      (62, 3),
      (61, 4),
      (62, 5),
      (60, 6),
      (61, 7),
      (60, 8),
      (62, 9),
      (61, 10),
    )

//    velocityTimestampPairs.foreach {
//      case (velocity, offset) => send(synth, velocity, offset)
//    }

    Thread.sleep(12000)

    synth.close()
  }

  def send(synth: Synthesizer, note: Int, secondOffset: Long): Unit = {
    val velocity = 100
    val onMsg = new ShortMessage()
    onMsg.setMessage(ShortMessage.NOTE_ON, 0, note, velocity)

    val timestamp = toMicroseconds(secondOffset)

    val receiver = synth.getReceiver
    receiver.send(onMsg, timestamp)

    val offMsg = new ShortMessage()
    offMsg.setMessage(ShortMessage.NOTE_OFF, 0, note, velocity)

    val offTimestamp = timestamp + 500000
    receiver.send(offMsg, offTimestamp)
  }

  def toMicroseconds(seconds: Long): Long = {
    seconds * 1000000
  }

  def sequencerExample(): Unit = {
    val synth = MidiSystem.getSynthesizer
    if (!synth.isOpen) { synth.open() }

    val sequencer = MidiSystem.getSequencer
    if (!sequencer.isOpen) { sequencer.open() }

    // connect sequencer to synthesizer
    sequencer.getTransmitter.setReceiver(synth.getReceiver)

    sequencer.setTempoInBPM(120)

    val sequence = new Sequence(Sequence.PPQ, 10)
    println(s"divisionType ${sequence.getDivisionType}")
    println(s"resolution ${sequence.getResolution}")

    val track1 = sequence.createTrack()
    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, 0, 60, 93), 10))
    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, 0, 60, 93), 20))

    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, 0, 61, 93), 20))
    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, 0, 61, 93), 30))

    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, 0, 62, 93), 30))
    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, 0, 62, 93), 40))

    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, 0, 61, 93), 40))
    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, 0, 61, 93), 50))

    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, 0, 60, 93), 50))
    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, 0, 60, 93), 60))

    sequencer.setSequence(sequence)

    sequencer.start()

    while (sequencer.isRunning) {
      Thread.sleep(5)
    }
    Thread.sleep(1000)

    sequencer.close()
    synth.close()
  }

//  def addToTrack(track: Track, channel: Int, note: Int)
}
