package com.shadowcow.musicmaker.piano.domain.model

import com.shadowcow.musicmaker.piano.domain.ports.NotePicker

import scala.collection.mutable

class Player(val notePicker: NotePicker) {
  private val played = mutable.ListBuffer[Int]()
  resetComposition()

  def playNextNote(): Unit = {
    played += notePicker.pickNextNote(played.toSeq)
  }

  def playedNotes(): Seq[Int] = played.toSeq

  def resetComposition(): Unit = {
    played.clear()
    notePicker.seedNotes().foreach(n => played += n)
  }

  def like(): Unit = {
    notePicker.likeLastNote(played.toSeq)
  }

  def dislike(): Unit = {
    notePicker.dislikeLastNote(played.toSeq)
  }
}
