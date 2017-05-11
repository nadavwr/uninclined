package com.github.nadavwr

import com.github.nadavwr.math._

package object uninclined {
  val G: Double = 6.67408e-11
  val π: Double = scala.math.Pi

  def inspectθ(θ: Double): String = f"∠${θ/π}%.2fπ"
  def inspect(v: Vector2): String = f"(${v.r}%.2f${inspectθ(v.θ)})"
}
