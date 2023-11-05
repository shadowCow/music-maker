package com.shadowcow.musicmaker

import javax.sound.midi
import javax.sound.midi.{MidiEvent, Sequence, ShortMessage}

import scala.collection.mutable
import scala.util.Random

object PianoGenerator {
  def main(args: Array[String]): Unit = {
    println("Welcome to Piano Generator")
    val middleC = 39
    val increment = (1 / 88) / 10
    val pianoGenerator = new PianoGenerator(middleC, increment)
    playNotes(pianoGenerator.playedNotes())

    while (true) {
      print("Enter a command (replay (R), like (L), dislike (D), neutral (N)) or 'exit' to quit: ")
      val input = scala.io.StdIn.readLine()

      input match {
        case "R" => // Process replay option
          playNotes(pianoGenerator.playedNotes())
        case "L" => // Process like option
          pianoGenerator.like()
          pianoGenerator.playNextNote()
          playNotes(pianoGenerator.playedNotes())
        case "D" => // Process dislike option
          pianoGenerator.dislike()
          pianoGenerator.playNextNote()
          playNotes(pianoGenerator.playedNotes())
        case "N" => // Process neutral option
          playNotes(pianoGenerator.playedNotes())
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
    val startTick = 0
    val tickInterval = 10
    val sequence = new Sequence(Sequence.PPQ, 10)

    val track1 = sequence.createTrack()
    // set instrument
    track1.add(new MidiEvent(new ShortMessage(ShortMessage.PROGRAM_CHANGE, 0, instrument, 0), 0))

    var lastTick = startTick
    notes.foreach { note =>
      lastTick += tickInterval
      track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, 0, note + 21, 93), lastTick))
      lastTick += tickInterval
      track1.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, 0, note + 21, 93), lastTick))
    }

    sequence
  }
}


class PianoGenerator(val firstNote: Int, val increment: Double) {
  val numRows = 88
  val numCols = 88

  // transition probability matrix
  val P: Array[Array[Double]] = Array.ofDim[Double](numRows, numCols)

  val Pij: Double = 1.0 / 88.0

  for (i <- 0 until numRows; j <- 0 until numCols) {
    P(i)(j) = Pij
  }

  private val played = mutable.ListBuffer[Int](firstNote)
  playNextNote()

  def playedNotes(): Seq[Int] = {
    played.toSeq
  }

  def lastNote: Int = played.last
  def penultimateNote: Int = played(played.length - 2)

  def playNextNote(): Unit = {
    played += pickNextNote()
  }

  def pickNextNote(): Int = {
    val randomValue = Random.nextDouble()  // Generate a random number between 0 and 1
    var cumulativeSum = 0.0

    for (j <- 0 until numCols) {
      cumulativeSum += P(lastNote)(j)
      if (cumulativeSum > randomValue) {
        return j
      }
    }

    87
  }

  def like(): Unit = {
    P(penultimateNote)(lastNote) += increment
    val sum = P(penultimateNote).sum

    P(penultimateNote) = P(penultimateNote).map(p => p / sum)
  }

  def dislike(): Unit = {
    val decrease = if (P(penultimateNote)(lastNote) < increment) {
      P(penultimateNote)(lastNote)
    } else {
      increment
    }

    P(penultimateNote)(lastNote) -= decrease
    val sum = P(penultimateNote).sum

    P(penultimateNote) = P(penultimateNote).map(p => p / sum)
  }
}