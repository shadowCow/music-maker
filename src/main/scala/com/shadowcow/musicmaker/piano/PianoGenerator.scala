package com.shadowcow.musicmaker.piano

import java.nio.file.Paths

import com.shadowcow.musicmaker.piano.adapters.driven.MidiAudio
import com.shadowcow.musicmaker.piano.adapters.driven.notepicker.{MarkovExponentialWithDecayNeighborsFeedback, P1CsvPersistence, P1NotePicker, P2CsvPersistence, P2NotePicker, P3CsvPersistence, P3NotePicker}
import com.shadowcow.musicmaker.piano.adapters.driving.Cli
import com.shadowcow.musicmaker.piano.domain.model.{Keyboard, Player}
import com.shadowcow.musicmaker.piano.domain.service.LearningLoop

object PianoGenerator {
  val keyboard = Keyboard.Small

  def main(args: Array[String]): Unit = {
    println("Welcome to Piano Generator")

//    val filepath = Paths.get("/Users/dwadeson/music_workspace/markov_piano/P.csv").toAbsolutePath
//    val feedback = new MarkovExponentialWithDecayNeighborsFeedback(0.5)
//    val persistence = new P1CsvPersistence(keyboard, filepath)
//    val notePicker = new P1NotePicker(keyboard, feedback, persistence)

//    val filepath = Paths.get("/Users/dwadeson/music_workspace/markov_piano/P2.csv").toAbsolutePath
//    val persistence = new P2CsvPersistence(keyboard, filepath)
//    val notePicker = new P2NotePicker(keyboard, persistence)

    val filepath = Paths.get("/Users/dwadeson/music_workspace/markov_piano/P3.csv").toAbsolutePath
    val persistence = new P3CsvPersistence(keyboard, filepath)
    val notePicker = new P3NotePicker(keyboard, persistence)

    val pianoPlayer = new Player(notePicker)
    val audio = new MidiAudio()

    val learningLoop = new LearningLoop(keyboard, pianoPlayer, audio)

    val cli = new Cli(learningLoop)

    cli.start()
  }
}




