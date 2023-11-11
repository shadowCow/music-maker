package com.shadowcow.musicmaker.piano.domain.service

import com.shadowcow.musicmaker.piano.domain.model.{Keyboard, Player}
import com.shadowcow.musicmaker.piano.domain.ports.Audio

class LearningLoop(val keyboard: Keyboard,
                   val pianoPlayer: Player,
                   val audio: Audio) {
  def replay(): Unit = {
    playNotes(pianoPlayer.playedNotes(), keyboard)
  }

  def like(): Unit = {
    pianoPlayer.like()
    pianoPlayer.playNextNote()
    playNotes(pianoPlayer.playedNotes(), keyboard)
  }

  def dislike(): Unit = {
    pianoPlayer.dislike()
    pianoPlayer.resetComposition()
    playNotes(pianoPlayer.playedNotes(), keyboard)
  }

  def neutral(): Unit = {
    pianoPlayer.playNextNote()
    playNotes(pianoPlayer.playedNotes(), keyboard)
  }

  private def playNotes(notes: Seq[Int], keyboard: Keyboard): Unit = {
    audio.playPianoNotes(notes)
  }
}

