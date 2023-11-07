package com.shadowcow.musicmaker

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream, PrintWriter}
import java.nio.file.{Path, Paths}

import javax.sound.midi
import javax.sound.midi.{MidiEvent, Sequence, ShortMessage}

import scala.collection.mutable
import scala.io.Source
import scala.util.Random

object PianoGenerator {
  val numKeys = 88
  val filepath = Paths.get("/Users/dwadeson/music_workspace/markov_piano/P.csv").toAbsolutePath

  def main(args: Array[String]): Unit = {
    println("Welcome to Piano Generator")
    if (!filepath.toFile.exists()) {
      try {
        filepath.toFile.createNewFile()
        persistTransitionProbabilities(distanceP(), filepath)
      } catch {
        case t: Throwable =>
          println("Failed to create transition probabilities file")
          t.printStackTrace()
          sys.exit(0)
      }
    }

    val P = loadTransitionProbabilities(filepath)

    val middleC = 39

    val firstNote = middleC
    val feedback = new MarkovExponentialWithDecayNeighborsFeedback(0.5)
    val pianoPlayer = new Markov1Player(firstNote, feedback, P)

    playNotes(pianoPlayer.playedNotes())

    while (true) {
      print("Enter a command (replay (R), like (L), dislike (D), neutral (N)) or 'exit' to quit: ")
      val input = scala.io.StdIn.readLine()

      input match {
        case "R" => // Process replay option
          playNotes(pianoPlayer.playedNotes())
        case "L" => // Process like option
          pianoPlayer.like()
          persistTransitionProbabilities(pianoPlayer.P, filepath)
          pianoPlayer.playNextNote()
          playNotes(pianoPlayer.playedNotes())
        case "D" => // Process dislike option
          pianoPlayer.dislike()
          persistTransitionProbabilities(pianoPlayer.P, filepath)
          pianoPlayer.resetComposition()
          playNotes(pianoPlayer.playedNotes())
        case "N" => // Process neutral option
          pianoPlayer.playNextNote()
          playNotes(pianoPlayer.playedNotes())
        case "exit" =>
          println("Exiting the Piano Generator.")
          sys.exit(0)
        case _ =>
          println("Invalid input. Please enter a valid command.")
      }
    }
  }

  def playNotes(notes: Seq[Int]): Unit = {
    println(s"Notes: $notes")

    MidiPlayground.playSequence(notesToMidiSequence(notes))
  }

  def notesToMidiSequence(notes: Seq[Int]): Sequence = {
    val instrument = Instruments.ELECTRIC_PIANO_1
    val startTick = 10
    val tickInterval = 5
    val sequence = new Sequence(Sequence.PPQ, 10)

    val track1 = sequence.createTrack()
    // set instrument
    track1.add(new MidiEvent(new ShortMessage(ShortMessage.PROGRAM_CHANGE, 0, instrument, 0), 0))

    var lastTick = startTick
    notes.foreach { note =>
      track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, 0, note + 21, 93), lastTick))
      lastTick += tickInterval
      track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, 0, note + 21, 93), lastTick))
      lastTick += tickInterval
    }

    sequence
  }

  def persistTransitionProbabilities(P: Array[Array[Double]], filepath: Path): Unit = {
    val writer = new PrintWriter(filepath.toFile)
    try {
      for (row <- P) {
        writer.println(row.mkString(","))
      }
    } finally {
      writer.close()
    }
  }

  def loadTransitionProbabilities(filepath: Path): Array[Array[Double]] = {
    if (filepath.toFile.exists()) {
      println(s"Reading transition probabilities from $filepath")
      val source = Source.fromFile(filepath.toFile)
      val data = source.getLines().map(line => line.split(",").map(_.toDouble)).toArray
      source.close()
      data
    } else {
      distanceP()
    }
  }

  def uniformP(): Array[Array[Double]] = {
    val P = emptyP()

    val Pij: Double = 1.0 / 88.0

    for (i <- 0 until numKeys; j <- 0 until numKeys) {
      P(i)(j) = Pij
    }

    P
  }

  def distanceP(): Array[Array[Double]] = {
    val P = emptyP()

    // seed the array with weights based on distance from key
    for (i <- 0 until numKeys; j <- 0 until numKeys) {
      val distance: Double = 1.0 * math.abs(i - j)

      val weight = 88.0 - (distance / 2)
      P(i)(j) = weight
    }

    // normalize each row
    for (i <- 0 until numKeys) {
      val sum = P(i).sum
      P(i) = P(i).map(v => v / sum)
    }

    P
  }

  def emptyP(): Array[Array[Double]] =
    Array.ofDim[Double](88, 88)

}

trait Player {
  def playNextNote(): Unit
  def playedNotes(): Seq[Int]
  def resetComposition(): Unit
  def like(): Unit
  def dislike(): Unit
}

class Markov1Player(val firstNote: Int,
                    val feedback: Markov1Feedback,
                    val P: Array[Array[Double]]) extends Player {
  private val played = mutable.ListBuffer[Int]()
  resetComposition()

  override def playNextNote(): Unit = {
    played += pickNextNote(played.last)
  }

  private def pickNextNote(lastNote: Int): Int = {
    val randomValue = Random.nextDouble()  // Generate a random number between 0 and 1
    var cumulativeSum = 0.0

    for (j <- 0 until PianoGenerator.numKeys) {
      cumulativeSum += P(lastNote)(j)
      if (cumulativeSum > randomValue) {
        return j
      }
    }

    87
  }

  override def playedNotes(): Seq[Int] = played.toSeq

  override def resetComposition(): Unit = {
    played.clear()
    played += firstNote
    played += pickNextNote(firstNote)
  }

  override def like(): Unit = {
    feedback.like(P, penultimate(), last())
  }

  override def dislike(): Unit = {
    feedback.dislike(P, penultimate(), last())
  }

  private def last(): Int = played.last
  private def penultimate(): Int = played(played.length - 2)
}

trait Markov1Feedback {
  def like(P: Array[Array[Double]], penultimateNote: Int, lastNote: Int): Unit
  def dislike(P: Array[Array[Double]], penultimateNote: Int, lastNote: Int): Unit
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
 *
 *
 * @param firstNote
 * @param lastNote
 */
class Keyboard(val firstNote: Int, val lastNote: Int) {
  require(firstNote > -1 && firstNote < 89, s"firstNote must be in range [0, 87], got $firstNote")
  require(lastNote > -1 && lastNote < 89, s"lastNote must be in range [0, 87], got $lastNote")
  require(firstNote <= lastNote, s"firstNote must be < lastNote, got $firstNote, $lastNote")

  def numKeys(): Int = 1 + lastNote - firstNote

  def toMidi(note: Int): Int = note + 21
}

object Keyboard {
  val Full = new Keyboard(0, 87)
  val Small = new Keyboard(19, 59)
}