package com.shadowcow.musicmaker.piano.domain.ports

trait Audio {
  def playPianoNotes(notes: Seq[Int]): Unit
}
