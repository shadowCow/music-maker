package com.shadowcow.musicmaker.piano.domain.model

object Feedback {
  def likeOne(P: Array[Array[Double]], row: Int, column: Int, f: Double => Double): Unit = {
    val newVal = 1 + f(P(row)(column))
    P(row)(column) *= newVal

    val sum = P(row).sum
    P(row) = P(row).map(p => p / sum)
  }

  def dislikeOne(P: Array[Array[Double]], row: Int, column: Int, f: Double => Double): Unit = {
    val newVal = 1 - f(P(row)(column))
    P(row)(column) *= newVal

    val sum = P(row).sum
    P(row) = P(row).map(p => p / sum)
  }
}