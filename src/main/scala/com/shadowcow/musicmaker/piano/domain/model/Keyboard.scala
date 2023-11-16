package com.shadowcow.musicmaker.piano.domain.model

import scala.util.Random

/**
 *
 *
 *
 * @param firstNote
 * @param lastNote
 */
class Keyboard(val firstNote: Int, val lastNote: Int) {
  require(firstNote > -1 && firstNote < 88, s"firstNote must be in range [0, 87], got $firstNote")
  require(lastNote > -1 && lastNote < 88, s"lastNote must be in range [0, 87], got $lastNote")
  require(firstNote <= lastNote, s"firstNote must be < lastNote, got $firstNote, $lastNote")

  def numKeys(): Int = 1 + lastNote - firstNote

  def toIndex(note: Int): Int = note - firstNote
  def fromIndex(index: Int): Int = {
    val note = index + firstNote
    require(note >= firstNote && note <= lastNote, s"note must be in range [$firstNote, $lastNote], got $note")
    note
  }

  def randomNote(): Int = {
    val randomKeyIndex = Random.nextInt(numKeys())

    fromIndex(randomKeyIndex)
  }
}

object Keyboard {
  val Full = new Keyboard(0, 87)

  /**
   * It seems like we could work with just the range 19 to 59
   * to produce useful music.
   *
   * We could perhaps also shift which notes we are working with
   * based on the ambiance we want. If we want dark and foreboding,
   * we could shift the range lower.
   *
   * If we want... whatever is at the high range, we could
   * shift that way.
   */
  val Small = new Keyboard(19, 59)
}
