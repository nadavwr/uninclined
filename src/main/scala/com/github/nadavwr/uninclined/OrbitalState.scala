package com.github.nadavwr.uninclined

import com.github.nadavwr.math._

/**
  * @param r͢ₒ    orbital position at epoch
  * @param v͢ₒ    orbital velocity at epoch
  * @param tₒ    time of epoch
  * @param μ     gravitational parameter of primary body
  */
case class OrbitalState(r͢ₒ: Vector2,
                        v͢ₒ: Vector2,
                        tₒ: Double,
                        μ: Double) {
  val rₒ: Double = r͢ₒ.r
  val vₒ: Double = v͢ₒ.r

  override def toString: String =
    s"""OrbitalState(
       |  r͢ₒ = $r͢ₒ,
       |  v͢ₒ = $v͢ₒ,
       |  tₒ = $tₒ,
       |  μ = $μ)
     """.stripMargin
}
