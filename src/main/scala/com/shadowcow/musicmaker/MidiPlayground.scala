package com.shadowcow.musicmaker

import java.util

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

    val sequence = createSampleSequence()

    sequencer.setSequence(sequence)

    sequencer.start()

    while (sequencer.isRunning) {
      Thread.sleep(5)
    }
    Thread.sleep(1000)

    sequencer.close()
    synth.close()
  }

  def testAllPianoNotes(instrument: Int, display: Boolean = true): Unit = {
    if (display) {
      testNotesWithDisplay(instrument, (0 until 88).toArray)
    } else {
      testNotes(instrument, (0 until 88).toArray)
    }
  }

  def testNotesWithDisplay(instrument: Int, notes: Array[Int]): Unit = {
    require(instrument > -1 && instrument < 129, s"Instrument must be 0 <= i <= 128, got $instrument")
    val startTick = 10
    val tickInterval = 5

    notes.foreach { note =>
      var lastTick = startTick

      val sequence = new Sequence(Sequence.PPQ, 10)

      val track1 = sequence.createTrack()
      // set instrument
      track1.add(new MidiEvent(new ShortMessage(ShortMessage.PROGRAM_CHANGE, 0, instrument, 0), 0))

      track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, 0, note + 21, 93), lastTick))
      lastTick += tickInterval
      track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, 0, note + 21, 93), lastTick))

      println(s"playing note: $note")
      playSequence(sequence)
    }
  }

  def testNotes(instrument: Int, notes: Array[Int]): Unit = {
    require(instrument > -1 && instrument < 129, s"Instrument must be 0 <= i <= 128, got $instrument")
    val startTick = 10
    val tickInterval = 5

    var lastTick = startTick

    val sequence = new Sequence(Sequence.PPQ, 10)

    val track1 = sequence.createTrack()
    // set instrument
    track1.add(new MidiEvent(new ShortMessage(ShortMessage.PROGRAM_CHANGE, 0, instrument, 0), 0))

    notes.foreach { note =>
      track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, 0, note + 21, 93), lastTick))
      lastTick += tickInterval
      track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, 0, note + 21, 93), lastTick))
      lastTick += tickInterval
    }

    playSequence(sequence)
  }

  def testInstruments(): Unit = {
    Instruments.ALL_INSTRUMENTS.map(i => createSampleSequence(i))
      .foreach(playSequence)
  }

  def testPercussion(): Unit = {
    (35 to 81).map(k => createPercussionSequence(0, k))
      .foreach(playSequence)
  }

  def testEffects(): Unit = {
    (121 to 127).map(i => createEffectsSequence(i))
      .foreach(playSequence)
  }

  def testTelephone(): Unit = {
    (35 to 81).map(k => createTelephoneSequence(k))
      .foreach(playSequence)
  }

  def testChoir(): Unit = {
    playSequence(createChoirSequence())
  }

  def testWavey(): Unit = {
    playSequence(createWaveySequence())
  }

  def createWaveySequence(): Sequence = {
    val sequence = new Sequence(Sequence.PPQ, 10)
    val channel = 0
    val instrument = 127
    val key = 99

    val track1 = sequence.createTrack()
    // set instrument
    track1.add(new MidiEvent(new ShortMessage(ShortMessage.PROGRAM_CHANGE, channel, instrument, 0), 0))

    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, channel, key, 93), 10))
    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, channel, key, 93), 30))

    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, channel, key+1, 93), 30))
    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, channel, key+1, 93), 50))

    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, channel, key+2, 93), 50))
    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, channel, key+2, 93), 70))

    sequence
  }

  def createChoirSequence(): Sequence = {
    val sequence = new Sequence(Sequence.PPQ, 10)
    val channel = 0
    val instrument = 91
    val key = 60

    val track1 = sequence.createTrack()
    // set instrument
    track1.add(new MidiEvent(new ShortMessage(ShortMessage.PROGRAM_CHANGE, channel, instrument, 0), 0))

    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, channel, key, 93), 10))
    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, channel, key, 93), 30))

    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, channel, key+1, 93), 30))
    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, channel, key+1, 93), 50))

    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, channel, key+2, 93), 50))
    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, channel, key+2, 93), 70))

    sequence
  }

  def createTelephoneSequence(key: Int = 0): Sequence = {
    val sequence = new Sequence(Sequence.PPQ, 10)
    val channel = 9
    val instrument = 124

    val track1 = sequence.createTrack()
    // set instrument
    track1.add(new MidiEvent(new ShortMessage(ShortMessage.PROGRAM_CHANGE, channel, instrument, 0), 0))

    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, channel, key, 93), 10))
    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, channel, key, 93), 20))

    sequence
  }

  def createEffectsSequence(instrument: Int = 0): Sequence = {
    val sequence = new Sequence(Sequence.PPQ, 10)
    val channel = 0
    val key = 60

    val track1 = sequence.createTrack()
    // set instrument
    track1.add(new MidiEvent(new ShortMessage(ShortMessage.PROGRAM_CHANGE, channel, instrument, 0), 0))

    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, channel, key, 93), 10))
    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, channel, key, 93), 100))

    sequence
  }

  def createPercussionSequence(instrument: Int = 0, key: Int = 60): Sequence = {
    val sequence = new Sequence(Sequence.PPQ, 10)
    val channel = 9

    val track1 = sequence.createTrack()
    // set instrument
    track1.add(new MidiEvent(new ShortMessage(ShortMessage.PROGRAM_CHANGE, channel, instrument, 0), 0))

    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, channel, key, 93), 10))
    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, channel, key, 93), 20))

    sequence
  }

  def playSequence(sequence: Sequence): Unit = {
    val synth = MidiSystem.getSynthesizer
    if (!synth.isOpen) { synth.open() }

    val sequencer = MidiSystem.getSequencer
    if (!sequencer.isOpen) { sequencer.open() }

    // connect sequencer to synthesizer
    sequencer.getTransmitter.setReceiver(synth.getReceiver)

    sequencer.setTempoInBPM(120)

    sequencer.setSequence(sequence)

    println(s"playing sequence ${util.Arrays.toString(sequence.getTracks()(0).get(0).getMessage().getMessage())}, ${util.Arrays.toString(sequence.getTracks()(0).get(1).getMessage.getMessage)}")

    sequencer.start()

    while (sequencer.isRunning) {
      Thread.sleep(5)
    }
    Thread.sleep(1000)

    sequencer.close()
    synth.close()
  }

  def createSampleSequence(instrument: Int = 0, startTick: Int = 0): Sequence = {
    val sequence = new Sequence(Sequence.PPQ, 10)
//    println(s"divisionType ${sequence.getDivisionType}")
//    println(s"resolution ${sequence.getResolution}")

    val track1 = sequence.createTrack()
    // set instrument
    track1.add(new MidiEvent(new ShortMessage(ShortMessage.PROGRAM_CHANGE, 0, instrument, 0), 0))

    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, 0, 60, 93), startTick + 10))
    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, 0, 60, 93), startTick + 20))

    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, 0, 61, 93), startTick + 20))
    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, 0, 61, 93), startTick + 30))

    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, 0, 62, 93), startTick + 30))
    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, 0, 62, 93), startTick + 40))

    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, 0, 61, 93), startTick + 40))
    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, 0, 61, 93), startTick + 50))

    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, 0, 60, 93), startTick + 50))
    track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, 0, 60, 93), startTick + 60))

    sequence
  }

//  def addToTrack(track: Track, channel: Int, note: Int)
}
