package com.shadowcow.musicmaker.piano.domain.model

import scala.util.Random

trait Keyboard {
  def numKeys(): Int
  def noteToIndex(note: Int): Int
  def indexToNote(index: Int): Int
  def randomNote(): Int = {
    val randomKeyIndex = Random.nextInt(numKeys())

    indexToNote(randomKeyIndex)
  }
}

/**
 * A keyboard that uses both white and black keys.
 *
 * @param firstNote First note, 0 to 87 inclusive
 * @param lastNote Last note, 0 to 87 inclusive
 */
class WhiteAndBlackKeyboard(val firstNote: Int, val lastNote: Int) extends Keyboard {
  require(firstNote > -1 && firstNote < 88, s"firstNote must be in range [0, 87], got $firstNote")
  require(lastNote > -1 && lastNote < 88, s"lastNote must be in range [0, 87], got $lastNote")
  require(firstNote <= lastNote, s"firstNote must be < lastNote, got $firstNote, $lastNote")

  def numKeys(): Int = 1 + lastNote - firstNote

  def noteToIndex(note: Int): Int = note - firstNote
  def indexToNote(index: Int): Int = {
    val note = index + firstNote
    require(note >= firstNote && note <= lastNote, s"note must be in range [$firstNote, $lastNote], got $note")
    note
  }
}

/**
 * A keyboard that uses only white keys.
 *
 * @param firstNote First note, 0 to 87 inclusive, must be a white key.
 * @param lastNote Last note, 0 to 87 inclusive, must be a white key.
 */
class WhiteOnlyKeyboard(val firstNote: Int, val lastNote: Int) extends Keyboard {
  require(firstNote > -1 && firstNote < 88, s"firstNote must be in range [0, 87], got $firstNote")
  require(lastNote > -1 && lastNote < 88, s"lastNote must be in range [0, 87], got $lastNote")
  require(firstNote <= lastNote, s"firstNote must be < lastNote, got $firstNote, $lastNote")
  require(
    !Keyboard.blackKeys.contains(firstNote) && !Keyboard.blackKeys.contains(lastNote),
    s"notes must be white keys, got $firstNote, $lastNote"
  )

  val keys = Keyboard.whiteKeys.filter(k => k >= firstNote && k <= lastNote)

  def numKeys(): Int = keys.size

  override def noteToIndex(note: Int): Int = keys.indexOf(note)

  override def indexToNote(index: Int): Int = keys(index)
}

object Keyboard {
  val c3 = 27
  val c4 = 39
  val c5 = 51

  val blackKeys: Seq[Int] = Seq(

  )
  val whiteKeys: Seq[Int] = (0 until 88).filter(k => !blackKeys.contains(k))

  val Full = new WhiteAndBlackKeyboard(0, 87)

  val Small = new WhiteAndBlackKeyboard(19, 59)

  val WhiteC3C5 = new WhiteOnlyKeyboard(c3, c5)
}
