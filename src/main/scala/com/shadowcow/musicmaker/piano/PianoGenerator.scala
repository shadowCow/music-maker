package com.shadowcow.musicmaker.piano

import java.io.PrintWriter
import java.nio.file.{Path, Paths}

import com.shadowcow.musicmaker.piano.adapters.driving.Cli
import com.shadowcow.musicmaker.piano.domain.model.{Keyboard, Player, Tpm}
import com.shadowcow.musicmaker.piano.domain.service.LearningLoop

import scala.io.Source
import scala.util.Random

object PianoGenerator {
  val keyboard = Keyboard.Full
  val filepath = Paths.get("/Users/dwadeson/music_workspace/markov_piano/P.csv").toAbsolutePath

  def main(args: Array[String]): Unit = {
    println("Welcome to Piano Generator")
    if (!filepath.toFile.exists()) {
      try {
        filepath.toFile.createNewFile()
        persistTransitionProbabilities(Tpm.distanceP1(keyboard), filepath)
      } catch {
        case t: Throwable =>
          println("Failed to create transition probabilities file")
          t.printStackTrace()
          sys.exit(0)
      }
    }

    val P = loadTransitionProbabilities(filepath, keyboard)

    val middleC = 39

    val firstNote = middleC
    val feedback = new MarkovExponentialWithDecayNeighborsFeedback(0.5)
    val notePicker = new P1NotePicker(keyboard, feedback, P)
    val pianoPlayer = new Player(firstNote, notePicker)

    val learningLoop = new LearningLoop(keyboard, pianoPlayer)

    val cli = new Cli(learningLoop)

    cli.start()
  }
}



trait NotePicker {
  def pickNextNote(played: Seq[Int]): Int
  def like(played: Seq[Int]): Unit
  def dislike(played: Seq[Int]): Unit
}

class P1CsvPersistence(val keyboard: Keyboard,
                       val filepath: Path) {
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


class P1NotePicker(val keyboard: Keyboard,
                   val feedback: Markov1Feedback,
                   val P: Array[Array[Double]]) extends NotePicker {
  override def pickNextNote(played: Seq[Int]): Int = {
    val randomValue = Random.nextDouble()  // Generate a random number between 0 and 1
    var cumulativeSum = 0.0

    for (j <- 0 until keyboard.numKeys) {
      cumulativeSum += P(played.last)(j)
      if (cumulativeSum > randomValue) {
        return j
      }
    }

    keyboard.numKeys() - 1
  }

  override def like(played: Seq[Int]): Unit = {
    feedback.like(P, penultimate(played), played.last)
  }

  override def dislike(played: Seq[Int]): Unit = {
    feedback.dislike(P, penultimate(played), played.last)
  }

  private def penultimate(played: Seq[Int]): Int = played(played.length - 2)
}

trait Markov1Feedback {
  def like(P: Array[Array[Double]], penultimateNote: Int, lastNote: Int): Unit
  def dislike(P: Array[Array[Double]], penultimateNote: Int, lastNote: Int): Unit
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
  override def like(P: Array[Array[Double]], penultimateNote: Int, lastNote: Int): Unit = {
    P(penultimateNote)(lastNote) *= (1 + factor)
    val sum = P(penultimateNote).sum

    P(penultimateNote) = P(penultimateNote).map(p => p / sum)
  }

  override def dislike(P: Array[Array[Double]], penultimateNote: Int, lastNote: Int): Unit = {
    P(penultimateNote)(lastNote) *= (1 - factor)
    val sum = P(penultimateNote).sum

    P(penultimateNote) = P(penultimateNote).map(p => p / sum)
  }
}

class MarkovExponentialWithDecayFeedback(factor: Double) extends Markov1Feedback {
  override def like(P: Array[Array[Double]], penultimateNote: Int, lastNote: Int): Unit = {
    val f = (1 - P(penultimateNote)(lastNote)) * factor
    P(penultimateNote)(lastNote) *= (1 + f)

    val sum = P(penultimateNote).sum
    P(penultimateNote) = P(penultimateNote).map(p => p / sum)
  }

  override def dislike(P: Array[Array[Double]], penultimateNote: Int, lastNote: Int): Unit = {
    val f = (1 - P(penultimateNote)(lastNote)) * factor
    P(penultimateNote)(lastNote) *= (1 - f)

    val sum = P(penultimateNote).sum
    P(penultimateNote) = P(penultimateNote).map(p => p / sum)
  }
}

class MarkovExponentialWithDecayNeighborsFeedback(factor: Double) extends Markov1Feedback {
  override def like(P: Array[Array[Double]], penultimateNote: Int, lastNote: Int): Unit = {
    val f = (1 - P(penultimateNote)(lastNote)) * factor
    P(penultimateNote)(lastNote) *= (1 + f)

    if (lastNote > 0) {
      val f2 = (1 - P(penultimateNote)(lastNote - 1)) * factor
      P(penultimateNote)(lastNote - 1) *= (1 + f2)
    }

    if (lastNote < P(penultimateNote).length - 1) {
      val f3 = (1 - P(penultimateNote)(lastNote + 1)) * factor
      P(penultimateNote)(lastNote + 1) *= (1 + f3)
    }

    val sum = P(penultimateNote).sum
    P(penultimateNote) = P(penultimateNote).map(p => p / sum)
  }

  override def dislike(P: Array[Array[Double]], penultimateNote: Int, lastNote: Int): Unit = {
    val f = P(penultimateNote)(lastNote) * factor
    P(penultimateNote)(lastNote) *= (1 - f)

    if (lastNote > 0) {
      val f2 = P(penultimateNote)(lastNote - 1) * factor
      P(penultimateNote)(lastNote - 1) *= (1 - f2)
    }

    if (lastNote < P(penultimateNote).length - 1) {
      val f3 = P(penultimateNote)(lastNote + 1) * factor
      P(penultimateNote)(lastNote + 1) *= (1 - f3)
    }

    val sum = P(penultimateNote).sum
    P(penultimateNote) = P(penultimateNote).map(p => p / sum)
  }
}
