package com.github.nadavwr.uninclined

import com.github.nadavwr.math._

import scala.math._

object OrbitalElements {
  def fromState(state: OrbitalState): OrbitalElements = {
    import state._
    val h = r⃯⨯v⃯
    val e⃯ = Vector2(v⃯.y, -v⃯.x) * h/μ - r⃯.normalized
    val ϖ = e⃯.θ
    val e = e⃯.r
    def isElliptic = e < 1
    def isHyperbolic = e > 1
    val θₒ = r⃯.θ - ϖ
    val Mₒ = if (isElliptic) {
      val Eₒ = acos((e + cos(θₒ))/(1 + e*cos(θₒ)))
      Eₒ - e*sin(Eₒ)
    } else if (isHyperbolic) {
      val Fₒ = acosh((e + cos(θₒ))/(1 + e*cos(θₒ)))
      e*sin(Fₒ) - Fₒ
    } else {
      val D = tan(θₒ/2)
      D + pow(D, 3)/3
    }
    val vₑ = sqrt(2*μ/r) // will be used to detect radial trajectories
    val p = pow(h,2)/μ
    val rₚ = p/(1+e)
    OrbitalElements(Mₒ = Mₒ, rₚ = rₚ, ϖ = ϖ, e = e, tₒ = t, μ = μ,
                    direction = signum(h).toInt)
  }
}

/**
  * @param Mₒ    mean anomaly at epoch
  * @param rₚ    periapsis distance
  * @param ϖ     longitude of periapsis
  * @param e     eccentricity
  * @param tₒ    time of epoch
  * @param μ     gravitational parameter of primary body
  */
case class OrbitalElements(Mₒ: Double,
                           rₚ: Double,
                           ϖ: Double,
                           e: Double,
                           tₒ: Double,
                           μ: Double,
                           direction: Int) {

  override def toString: String = {
    s"""OrbitalElements(
       |  Mₒ = $Mₒ,
       |  rₚ = $rₚ,
       |  ϖ = $ϖ,
       |  e = $e,
       |  tₒ = 0,
       |  μ = $μ,
       |  direction = $direction)
     """.stripMargin
  }

  def isElliptic: Boolean = e < 1
  def isHyperbolic: Boolean = e > 1
}
