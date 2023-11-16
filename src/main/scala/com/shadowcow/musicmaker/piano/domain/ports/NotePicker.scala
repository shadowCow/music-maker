package com.shadowcow.musicmaker.piano.domain.ports

trait NotePicker {
  def seedNotes(): Seq[Int]
  def pickNextNote(played: Seq[Int]): Int
  def like(played: Seq[Int]): Unit
  def dislike(played: Seq[Int]): Unit
}
