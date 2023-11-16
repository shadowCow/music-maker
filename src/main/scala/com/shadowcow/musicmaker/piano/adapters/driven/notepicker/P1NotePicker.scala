package com.shadowcow.musicmaker.piano.adapters.driven.notepicker

import java.io.PrintWriter
import java.nio.file.Path

import com.shadowcow.musicmaker.piano.domain.model.{Keyboard, Tpm}
import com.shadowcow.musicmaker.piano.domain.ports.NotePicker

import scala.io.Source

class P1NotePicker(val keyboard: Keyboard,
                   val feedback: Markov1Feedback,
                   val persistence: P1CsvPersistence) extends NotePicker {
  val P: Array[Array[Double]] = persistence.read()

  override def pickNextNote(played: Seq[Int]): Int = {
    val row = P(keyboard.toIndex(played.last))
    Tpm.getRandom(row)
  }

  override def like(played: Seq[Int]): Unit = {
    feedback.like(P, penultimate(played), last(played))
    persistence.write(P)
  }

  override def dislike(played: Seq[Int]): Unit = {
    feedback.dislike(P, penultimate(played), last(played))
    persistence.write(P)
  }

  private def penultimate(played: Seq[Int]): Int = keyboard.toIndex(played(played.length - 2))
  private def last(played: Seq[Int]): Int = keyboard.toIndex(played.last)
}

trait Markov1Feedback {
  def like(P: Array[Array[Double]], penultimateIndex: Int, lastIndex: Int): Unit
  def dislike(P: Array[Array[Double]], penultimateIndex: Int, lastIndex: Int): Unit
}

object Markov1Feedback {
  def likeComposition(feedback: Markov1Feedback, P: Array[Array[Double]], composition: Array[Int]): Unit = {
    require(composition.length > 1, s"composition.length must be >= 2")

    composition.sliding(2).foreach { pair =>
      feedback.like(P, pair(0), pair(1))
    }
  }

  def dislikeComposition(feedback: Markov1Feedback, P: Array[Array[Double]], composition: Array[Int]): Unit = {
    require(composition.length > 1, s"composition.length must be >= 2")

    composition.sliding(2).foreach { pair =>
      feedback.dislike(P, pair(0), pair(1))
    }
  }
}

class MarkovExponentialFeedback(factor: Double) extends Markov1Feedback {
  override def like(P: Array[Array[Double]], penultimateIndex: Int, lastIndex: Int): Unit = {
    P(penultimateIndex)(lastIndex) *= (1 + factor)
    val sum = P(penultimateIndex).sum

    P(penultimateIndex) = P(penultimateIndex).map(p => p / sum)
  }

  override def dislike(P: Array[Array[Double]], penultimateIndex: Int, lastIndex: Int): Unit = {
    P(penultimateIndex)(lastIndex) *= (1 - factor)
    val sum = P(penultimateIndex).sum

    P(penultimateIndex) = P(penultimateIndex).map(p => p / sum)
  }
}

class MarkovExponentialWithDecayFeedback(factor: Double) extends Markov1Feedback {
  override def like(P: Array[Array[Double]], penultimateIndex: Int, lastIndex: Int): Unit = {
    val f = (1 - P(penultimateIndex)(lastIndex)) * factor
    P(penultimateIndex)(lastIndex) *= (1 + f)

    val sum = P(penultimateIndex).sum
    P(penultimateIndex) = P(penultimateIndex).map(p => p / sum)
  }

  override def dislike(P: Array[Array[Double]], penultimateIndex: Int, lastIndex: Int): Unit = {
    val f = (1 - P(penultimateIndex)(lastIndex)) * factor
    P(penultimateIndex)(lastIndex) *= (1 - f)

    val sum = P(penultimateIndex).sum
    P(penultimateIndex) = P(penultimateIndex).map(p => p / sum)
  }
}

class MarkovExponentialWithDecayNeighborsFeedback(factor: Double) extends Markov1Feedback {
  override def like(P: Array[Array[Double]], penultimateIndex: Int, lastIndex: Int): Unit = {
    val f = (1 - P(penultimateIndex)(lastIndex)) * factor
    P(penultimateIndex)(lastIndex) *= (1 + f)

    if (lastIndex > 0) {
      val f2 = (1 - P(penultimateIndex)(lastIndex - 1)) * factor
      P(penultimateIndex)(lastIndex - 1) *= (1 + f2)
    }

    if (lastIndex < P(penultimateIndex).length - 1) {
      val f3 = (1 - P(penultimateIndex)(lastIndex + 1)) * factor
      P(penultimateIndex)(lastIndex + 1) *= (1 + f3)
    }

    val sum = P(penultimateIndex).sum
    P(penultimateIndex) = P(penultimateIndex).map(p => p / sum)
  }

  override def dislike(P: Array[Array[Double]], penultimateIndex: Int, lastIndex: Int): Unit = {
    val f = P(penultimateIndex)(lastIndex) * factor
    P(penultimateIndex)(lastIndex) *= (1 - f)

    if (lastIndex > 0) {
      val f2 = P(penultimateIndex)(lastIndex - 1) * factor
      P(penultimateIndex)(lastIndex - 1) *= (1 - f2)
    }

    if (lastIndex < P(penultimateIndex).length - 1) {
      val f3 = P(penultimateIndex)(lastIndex + 1) * factor
      P(penultimateIndex)(lastIndex + 1) *= (1 - f3)
    }

    val sum = P(penultimateIndex).sum
    P(penultimateIndex) = P(penultimateIndex).map(p => p / sum)
  }
}


class P1CsvPersistence(val keyboard: Keyboard,
                       val filepath: Path) {

  if (!filepath.toFile.exists()) {
    try {
      filepath.toFile.createNewFile()
      write(Tpm.distanceP1(keyboard))
    } catch {
      case t: Throwable =>
        println("Failed to create transition probabilities file")
        t.printStackTrace()
        sys.exit(0)
    }
  }

  def write(P: Array[Array[Double]]): Unit = {
    val writer = new PrintWriter(filepath.toFile)
    try {
      for (row <- P) {
        writer.println(row.mkString(","))
      }
    } finally {
      writer.close()
    }
  }

  def read(): Array[Array[Double]] = {
    if (filepath.toFile.exists()) {
      println(s"Reading transition probabilities from $filepath")
      val source = Source.fromFile(filepath.toFile)
      val data = source.getLines().map(line => line.split(",").map(_.toDouble)).toArray
      source.close()
      data
    } else {
      Tpm.distanceP1(keyboard)
    }
  }
}
