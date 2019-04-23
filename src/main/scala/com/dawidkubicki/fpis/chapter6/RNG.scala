package com.dawidkubicki.fpis.chapter6

trait RNG {
  def nextInt: (Int, RNG)
}
