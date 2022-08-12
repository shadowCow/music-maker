package com.shadowcow.musicmaker

object Instruments {
  val PIANO = 0
  val HARPSICHORD = 6
  val MARIMBA = 12
  val TUBULAR_BELLS = 14
  val CHURCH_ORGAN = 19
  val DISTORTION_GUITAR = 30
  val ELECTRIC_BASS_PICKED = 34
  val VIOLIN = 40
  val SYNTH_STRINGS_1 = 50
  val TUBA = 58
  val SOPRANO_SAX = 64
  val PAN_FLUTE = 75
  val OCARINA = 79
  val LEAD_1_SQUARE_WAVE = 80
  val PAD_1 = 88
  val FX_1_RAIN = 96
  val FX_3_CRYSTAL = 98
  val SITAR = 104
  val BAG_PIPE = 109
  val STEEL_DRUMS = 114
  val TELEPHONE = 124
  val APPLAUSE = 126
  val GUNSHOT = 127

  final val ALL_INSTRUMENTS: Seq[Int] = (0 until 128)
}
