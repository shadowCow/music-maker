package com.shadowcow.musicmaker.piano.adapters.driven

import com.shadowcow.musicmaker.{Instruments, MidiPlayground}
import com.shadowcow.musicmaker.piano.domain.ports.Audio
import javax.sound.midi.{MidiEvent, Sequence, ShortMessage}

class MidiAudio extends Audio {
  override def playPianoNotes(notes: Seq[Int]): Unit = {
    println(s"playing $notes")
    MidiPlayground.playSequence(notesToMidiSequence(notes))
  }

  private def notesToMidiSequence(notes: Seq[Int]): Sequence = {
    val instrument = Instruments.ELECTRIC_PIANO_1
    val startTick = 10
    val tickInterval = 5
    val sequence = new Sequence(Sequence.PPQ, 10)

    val track1 = sequence.createTrack()
    // set instrument
    track1.add(new MidiEvent(new ShortMessage(ShortMessage.PROGRAM_CHANGE, 0, instrument, 0), 0))

    var lastTick = startTick
    notes.foreach { note =>
      track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, 0, toMidi(note), 93), lastTick))
      lastTick += tickInterval
      track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, 0, toMidi(note), 93), lastTick))
      lastTick += tickInterval
    }

    sequence
  }

  private def toMidi(note: Int): Int = note + 21
}
