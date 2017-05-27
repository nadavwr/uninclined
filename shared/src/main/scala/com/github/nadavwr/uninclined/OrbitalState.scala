package com.github.nadavwr.uninclined

import com.github.nadavwr.math._

/**
  * @param r⃯    orbital position at epoch
  * @param v⃯    orbital velocity at epoch
  * @param t    time of epoch
  * @param μ    gravitational parameter of primary body
  */
case class OrbitalState(r⃯: Vector2,
                        v⃯: Vector2,
                        t: Double,
                        μ: Double) {
  val r: Double = r⃯.r
  val v: Double = v⃯.r

  override def toString: String =
    s"""OrbitalState(
       |  r⃯ = $r⃯,
       |  v⃯ = $v⃯,
       |  t = $t,
       |  μ = $μ)
     """.stripMargin
}
