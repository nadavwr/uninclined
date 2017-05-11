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
    val v = sqrt(μ/rₒ)
    val ⟳ = Vector2.polar(v, r͢ₒ.θ - π/2)
    val ⟲ = Vector2.polar(v, r͢ₒ.θ + π/2)
    val Δ͢v = Seq(⟳, ⟲).map(v⃯ => v⃯ - v͢ₒ).minBy(Δ͢v => Δ͢v.r)
    Δ͢v
  }
}

trait Orbit extends Trajectory {

  def elements: OrbitalElements

  override def tₒ: Double = elements.tₒ
  override def Tₒₚₜ: Option[Double] = None

  /** orbital position vector */
  override def r͢ₜ(t: Double): Vector2

  /** orbital velocity vector */
  override def v͢ₜ(t: Double): Vector2
}






