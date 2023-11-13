package com.shadowcow.musicmaker.piano.domain.model

import com.shadowcow.musicmaker.piano.domain.ports.NotePicker

import scala.collection.mutable

class Player(val firstNote: Int,
             val notePicker: NotePicker) {
  private val played = mutable.ListBuffer[Int]()
  resetComposition()

  def playNextNote(): Unit = {
    played += notePicker.pickNextNote(played.toSeq)
  }

  def playedNotes(): Seq[Int] = played.toSeq

  def resetComposition(): Unit = {
    played.clear()
    played += firstNote
    played += notePicker.pickNextNote(played.toSeq)
  }

  def like(): Unit = {
    notePicker.like(played.toSeq)
  }

  def dislike(): Unit = {
    notePicker.dislike(played.toSeq)
  }
}