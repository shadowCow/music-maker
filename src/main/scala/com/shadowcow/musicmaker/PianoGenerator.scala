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
    val incrementFactor = 0.5
    val pianoGenerator = new PianoGenerator(firstNote, incrementFactor, P)
    val pianoPlayer = new PianoPlayer(pianoGenerator, firstNote)

    playNotes(pianoPlayer.playedNotes())

    while (true) {
      print("Enter a command (replay (R), like (L), dislike (D), neutral (N)) or 'exit' to quit: ")
      val input = scala.io.StdIn.readLine()

      input match {
        case "R" => // Process replay option
          playNotes(pianoPlayer.playedNotes())
        case "L" => // Process like option
          pianoGenerator.like(pianoPlayer.penultimate(), pianoPlayer.last())
          persistTransitionProbabilities(pianoGenerator.P, filepath)
          pianoPlayer.playNextNote()
          playNotes(pianoPlayer.playedNotes())
        case "D" => // Process dislike option
          pianoGenerator.dislike(pianoPlayer.penultimate(), pianoPlayer.last())
          persistTransitionProbabilities(pianoGenerator.P, filepath)
          pianoPlayer.reset()
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
    val instrument = Instruments.PIANO
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


class PianoGenerator(val firstNote: Int,
                     val incrementFactor: Double,
                     val P: Array[Array[Double]] = Array.ofDim[Double](88, 88)) {

  def pickNextNote(lastNote: Int): Int = {
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

  def like(penultimateNote: Int, lastNote: Int): Unit = {
    P(penultimateNote)(lastNote) *= (1 + incrementFactor)
    val sum = P(penultimateNote).sum

    P(penultimateNote) = P(penultimateNote).map(p => p / sum)
  }

  def dislike(penultimateNote: Int, lastNote: Int): Unit = {
    P(penultimateNote)(lastNote) *= (1 - incrementFactor)
    val sum = P(penultimateNote).sum

    P(penultimateNote) = P(penultimateNote).map(p => p / sum)
  }
}

class PianoPlayer(val generator: PianoGenerator, val firstNote: Int) {
  private val played = mutable.ListBuffer[Int]()
  reset()

  def playNextNote(): Unit = {
    played += generator.pickNextNote(played.last)
  }

  def playedNotes(): Seq[Int] = played.toSeq

  def reset(): Unit = {
    played.clear()
    played += firstNote
    played += generator.pickNextNote(firstNote)
  }

  def last(): Int = played.last
  def penultimate(): Int = played(played.length - 2)
}