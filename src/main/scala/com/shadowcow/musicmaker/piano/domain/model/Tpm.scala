package com.shadowcow.musicmaker.piano.domain.model

/**
 * Transition Probability Matrix utilities
 */
object Tpm {
  def uniformP1(keyboard: Keyboard): Array[Array[Double]] = {
    val P = emptyP1(keyboard)

    val Pij: Double = 1.0 / 88.0

    for (i <- 0 until keyboard.numKeys(); j <- 0 until keyboard.numKeys()) {
      P(i)(j) = Pij
    }

    P
  }

  def distanceP1(keyboard: Keyboard): Array[Array[Double]] = {
    val P = emptyP1(keyboard)

    // seed the array with weights based on distance from key
    for (i <- 0 until keyboard.numKeys(); j <- 0 until keyboard.numKeys()) {
      val distance: Double = 1.0 * math.abs(i - j)

      val weight = 88.0 - (distance / 2)
      P(i)(j) = weight
    }

    // normalize each row
    for (i <- 0 until keyboard.numKeys()) {
      val sum = P(i).sum
      P(i) = P(i).map(v => v / sum)
    }

    P
  }

  def emptyP1(keyboard: Keyboard): Array[Array[Double]] =
    Array.ofDim[Double](keyboard.numKeys(), keyboard.numKeys())

  def emptyP2(keyboard: Keyboard): Array[Array[Double]] = {
    val numRows = keyboard.numKeys() * keyboard.numKeys()
    val numCols = keyboard.numKeys()

    Array.ofDim[Double](numRows, numCols)
  }
}
