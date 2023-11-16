package com.shadowcow.musicmaker.piano.adapters.driven.notepicker

import java.io.PrintWriter
import java.nio.file.Path

import com.shadowcow.musicmaker.piano.domain.model.{Keyboard, Tpm}
import com.shadowcow.musicmaker.piano.domain.ports.NotePicker

import scala.io.Source
import scala.util.Random

class P2NotePicker(val keyboard: Keyboard,
                   val persistence: P2CsvPersistence) extends NotePicker {
  val (p1, p2) = persistence.read()
  val factor = 0.5
  val p1Weight = 0.5

  override def seedNotes(): Seq[Int] = {
    val randomColumn = Random.nextInt(keyboard.numKeys())
    val firstNote = keyboard.fromIndex(randomColumn)
    val secondNote = pickNextNote(Seq(firstNote))
    val thirdNote = pickNextNote(Seq(firstNote, secondNote))

    Seq(firstNote, secondNote, thirdNote)
  }

  override def pickNextNote(played: Seq[Int]): Int = {
    val p1Row = p1(keyboard.toIndex(played.last))
    if (played.size < 2) {
      keyboard.fromIndex(Tpm.getRandom(p1Row))
    } else {
      val penultimate = keyboard.toIndex(played(played.size - 2))

      val p2RowIndex = penultimate * keyboard.numKeys() + keyboard.toIndex(played.last)
      val p2Row = p2(p2RowIndex)
      val combined = p1Row.zip(p2Row).map {
        case (a,b) => p1Weight * a + (1 - p1Weight) * b
      }

      keyboard.fromIndex(Tpm.getRandom(combined))
    }
  }

  override def like(played: Seq[Int]): Unit = {
    val last = keyboard.toIndex(played.last)
    val penultimate = keyboard.toIndex(played(played.size - 2))

    val f = (1 - p1(penultimate)(last)) * factor
    p1(penultimate)(last) *= (1 + f)

    val sum = p1(penultimate).sum
    p1(penultimate) = p1(penultimate).map(p => p / sum)

    val p2Index = penultimate * keyboard.numKeys() + played.last
    val f2 = (1 - p2(p2Index)(last)) * factor
    p2(p2Index)(last) *= (1 + f2)

    val p2Sum = p2(p2Index).sum
    p2(p2Index) = p2(p2Index).map(p => p / p2Sum)

    persistence.write(p1, p2)
  }

  override def dislike(played: Seq[Int]): Unit = {
    val last = keyboard.toIndex(played.last)
    val penultimate = keyboard.toIndex(played(played.size - 2))

    val f = (1 - p1(penultimate)(last)) * factor
    p1(penultimate)(last) *= (1 - f)

    val sum = p1(penultimate).sum
    p1(penultimate) = p1(penultimate).map(p => p / sum)

    val p2Index = penultimate * keyboard.numKeys() + played.last
    val f2 = (1 - p2(p2Index)(last)) * factor
    p2(p2Index)(last) *= (1 - f2)

    val p2Sum = p2(p2Index).sum
    p2(p2Index) = p2(p2Index).map(p => p / p2Sum)

    persistence.write(p1, p2)
  }
}

class P2CsvPersistence(val keyboard: Keyboard,
                       val filepath: Path) {
  if (!filepath.toFile.exists()) {
    try {
      filepath.toFile.createNewFile()
      write(Tpm.distanceP1(keyboard), Tpm.uniformP2(keyboard))
    } catch {
      case t: Throwable =>
        println("Failed to create transition probabilities file")
        t.printStackTrace()
        sys.exit(0)
    }
  }

  def write(P1: Array[Array[Double]], P2: Array[Array[Double]]): Unit = {
    val writer = new PrintWriter(filepath.toFile)
    try {
      for (row <- P1) {
        writer.println(row.mkString(","))
      }
      for (row <- P2) {
        writer.println(row.mkString(","))
      }
    } finally {
      writer.close()
    }
  }

  def read(): (Array[Array[Double]], Array[Array[Double]]) = {
    if (filepath.toFile.exists()) {
      println(s"Reading transition probabilities from $filepath")
      val source = Source.fromFile(filepath.toFile)
      val data = source.getLines().map(line => line.split(",").map(_.toDouble))
      val P1 = data.take(keyboard.numKeys()).toArray
      val P2 = data.drop(keyboard.numKeys()).toArray
      source.close()
      (P1, P2)
    } else {
      (Tpm.distanceP1(keyboard), Tpm.uniformP2(keyboard))
    }
  }
}