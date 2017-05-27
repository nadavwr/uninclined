package com.github.nadavwr.uninclined

import com.github.nadavwr.math._

import scala.language.implicitConversions
import scala.math.sqrt

object Orbit {
  def apply(elements: OrbitalElements): Option[Orbit] = {
    if (elements.isElliptic) Some(new EllipticOrbit(elements))
    else if (elements.isHyperbolic) Some(new HyperbolicOrbit(elements))
    else None //Some(new ParabolicOrbit(elements))
  }

  def determineElements(state: OrbitalState): OrbitalElements =
    OrbitalElements.fromState(state)

  def determine(state: OrbitalState): Option[Orbit] =
    Orbit(determineElements(state))

  def determineElliptic(elements: OrbitalElements): Option[EllipticOrbit] =
    if (elements.isElliptic) Some(new EllipticOrbit(elements))
    else None

  def determinHyperbolic(elements: OrbitalElements): Option[HyperbolicOrbit] =
    if (elements.isHyperbolic) Some(new HyperbolicOrbit(elements))
    else None

  def Δ͢͢vToCircular(state: OrbitalState): Vector2 = {
    import state._
    val h = r⃯⨯v⃯
    val direction = if (h <= 0) 1 else -1
    Vector2.polar(sqrt(μ/r), r⃯.θ + π/2*direction) - v⃯
  }
}

trait Orbit extends Trajectory {

  /** time of epoch */
  override def tₒ: Double

  /** orbital period (for closed orbits) */
  override def Tₒₚₜ: Option[Double] = None

  /** orbital position vector */
  override def r͢ₜ(t: Double): Vector2

  /** orbital velocity vector */
  override def v͢ₜ(t: Double): Vector2
}






