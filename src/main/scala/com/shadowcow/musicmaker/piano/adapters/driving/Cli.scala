package com.shadowcow.musicmaker.piano.adapters.driving

import com.shadowcow.musicmaker.piano.domain.service.LearningLoop

class Cli(val learningLoop: LearningLoop) {

  def start(): Unit = {
    learningLoop.replay()

    while (true) {
      print("Enter a command (replay (R), like (L), dislike (D), neutral (N)) or 'exit' to quit: ")
      val input = scala.io.StdIn.readLine()

      input match {
        case "R" => // Process replay option
          learningLoop.replay()
        case "L" => // Process like option
          learningLoop.like()
        case "D" => // Process dislike option
          learningLoop.dislike()
        case "N" => // Process neutral option
          learningLoop.neutral()
        case "exit" =>
          println("Exiting the Piano Generator.")
          sys.exit(0)
        case _ =>
          println("Invalid input. Please enter a valid command.")
      }
    }
  }

}
