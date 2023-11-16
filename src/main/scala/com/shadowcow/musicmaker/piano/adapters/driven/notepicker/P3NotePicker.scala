package com.shadowcow.musicmaker.piano.adapters.driven.notepicker

import java.io.PrintWriter
import java.nio.file.Path

import com.shadowcow.musicmaker.piano.domain.model.{Feedback, Keyboard, Tpm}
import com.shadowcow.musicmaker.piano.domain.ports.NotePicker

import scala.io.Source
import scala.util.Random

class P3NotePicker(val keyboard: Keyboard,
                   val persistence: P3CsvPersistence) extends NotePicker {
  val (p1, p2, p3) = persistence.read()
  val factor = 0.5
  val p1Weight = 0.34
  val p2Weight = 0.33

  override def seedNotes(): Seq[Int] = {
    val randomColumn = Random.nextInt(keyboard.numKeys())
    val firstNote = keyboard.fromIndex(randomColumn)
    val secondNote = pickNextNote(Seq(firstNote))
    val thirdNote = pickNextNote(Seq(firstNote, secondNote))
    val fourthNote = pickNextNote(Seq(firstNote, secondNote, thirdNote))

    Seq(firstNote, secondNote, thirdNote, fourthNote)
  }

  override def pickNextNote(played: Seq[Int]): Int = {
    val p1Row = p1(keyboard.toIndex(played.last))
    if (played.size < 2) {
      keyboard.fromIndex(Tpm.getRandom(p1Row))
    } else if (played.size < 3) {
      val penultimate = keyboard.toIndex(played(played.size - 2))

      val p2RowIndex = penultimate * keyboard.numKeys() + keyboard.toIndex(played.last)
      val p2Row = p2(p2RowIndex)
      val combined = p1Row.zip(p2Row).map {
        case (a,b) => p1Weight * a + (1 - p1Weight) * b
      }

      keyboard.fromIndex(Tpm.getRandom(combined))
    } else {
      val penultimate = keyboard.toIndex(played(played.size - 2))
      val third = keyboard.toIndex(played(played.size - 3))

      val p2RowIndex = penultimate * keyboard.numKeys() + keyboard.toIndex(played.last)
      val p2Row = p2(p2RowIndex)

      val p3RowIndex = third * keyboard.numKeys() * keyboard.numKeys() + p2RowIndex
      val p3Row = p3(p3RowIndex)

      val combined = p1Row.zipWithIndex.map {
        case (p, j) => p1Weight * p + p2Weight * p2Row(j) + (1 - (p1Weight + p2Weight)) * p3Row(j)
      }

      keyboard.fromIndex(Tpm.getRandom(combined))
    }
  }

  override def likeLastNote(played: Seq[Int]): Unit = {
    val last = keyboard.toIndex(played.last)
    val second = keyboard.toIndex(played(played.size - 2))
    val third = keyboard.toIndex(played(played.size - 3))

    val f = (v: Double) => (1 - v) * factor
    val p1RowIndex = second
    val p2RowIndex = second * keyboard.numKeys() + last
    val p3RowIndex = third * keyboard.numKeys() * keyboard.numKeys() + p2RowIndex

    Feedback.likeOne(p1, p1RowIndex, last, f)
    Feedback.likeOne(p2, p2RowIndex, last, f)
    Feedback.likeOne(p3, p3RowIndex, last, f)

    persistence.write(p1, p2, p3)
  }

  override def dislikeLastNote(played: Seq[Int]): Unit = {
    val last = keyboard.toIndex(played.last)
    val second = keyboard.toIndex(played(played.size - 2))
    val third = keyboard.toIndex(played(played.size - 3))

    val f = (v: Double) => (1 - v) * factor
    val p1RowIndex = second
    val p2RowIndex = second * keyboard.numKeys() + last
    val p3RowIndex = third * keyboard.numKeys() * keyboard.numKeys() + p2RowIndex

    Feedback.dislikeOne(p1, p1RowIndex, last, f)
    Feedback.dislikeOne(p2, p2RowIndex, last, f)
    Feedback.dislikeOne(p3, p3RowIndex, last, f)

    persistence.write(p1, p2, p3)
  }
}


class P3CsvPersistence(val keyboard: Keyboard,
                       val filepath: Path) {
  if (!filepath.toFile.exists()) {
    try {
      filepath.toFile.createNewFile()
      write(Tpm.distanceP1(keyboard), Tpm.uniformP2(keyboard), Tpm.uniformP3(keyboard))
    } catch {
      case t: Throwable =>
        println("Failed to create transition probabilities file")
        t.printStackTrace()
        sys.exit(0)
    }
  }

  def write(P1: Array[Array[Double]], P2: Array[Array[Double]], P3: Array[Array[Double]]): Unit = {
    val writer = new PrintWriter(filepath.toFile)
    try {
      for (row <- P1) {
        writer.println(row.mkString(","))
      }
      for (row <- P2) {
        writer.println(row.mkString(","))
      }
      for (row <- P3) {
        writer.println(row.mkString(","))
      }
    } finally {
      writer.close()
    }
  }

  def read(): (Array[Array[Double]], Array[Array[Double]], Array[Array[Double]]) = {
    if (filepath.toFile.exists()) {
      println(s"Reading transition probabilities from $filepath")
      val source = Source.fromFile(filepath.toFile)
      val data = source.getLines().map(line => line.split(",").map(_.toDouble))
      val P1 = data.take(keyboard.numKeys()).toArray
      val P2and3 = data.drop(keyboard.numKeys()).toArray

      val p2and3split = keyboard.numKeys() * keyboard.numKeys()
      val P2 = P2and3.take(p2and3split)
      val P3 = P2and3.drop(p2and3split)

      source.close()
      (P1, P2, P3)
    } else {
      (Tpm.distanceP1(keyboard), Tpm.uniformP2(keyboard), Tpm.uniformP3(keyboard))
    }
  }
}