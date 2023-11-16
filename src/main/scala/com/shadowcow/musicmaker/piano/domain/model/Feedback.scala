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

  def likeComposition(P: Array[Array[Double]],
                      rowColumnPairs: Seq[(Int, Int)],
                      f: Double => Double): Unit = {
    rowColumnPairs.foreach {
      case (row, column) =>
        likeOne(P, row, column, f)
    }
  }

  def dislikeComposition(P: Array[Array[Double]],
                         rowColumnPairs: Seq[(Int, Int)],
                         f: Double => Double): Unit = {
    rowColumnPairs.foreach {
      case (row, column) =>
        dislikeOne(P, row, column, f)
    }
  }
}