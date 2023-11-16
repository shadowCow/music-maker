package com.shadowcow.musicmaker.piano.domain.ports

trait NotePicker {
  def seedNotes(): Seq[Int]
  def pickNextNote(played: Seq[Int]): Int
  def likeLastNote(played: Seq[Int]): Unit
  def dislikeLastNote(played: Seq[Int]): Unit
}
