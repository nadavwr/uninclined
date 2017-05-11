package com.github.nadavwr.uninclined

import com.github.nadavwr.math._

import scala.annotation.tailrec
import scala.math._

/**
  * @param elements   orbital elements
  * @param tolerance  error tolerance for eccentric anomaly
  */
class EllipticOrbit(override val elements: OrbitalElements,
                    tolerance: Double = 1e-9)
    extends Orbit {

  import elements._
  require(elements.isElliptic)
  require(e >= 0)
  require(e < 1)

  val a: Double = rₚ/(1-e)
  require(a > 0)

  /** apoapsis */
  val rₐ: Double = a*(1+e)
  require(rₐ > 0)

  /** semi-latus rectum, parameter of ellipse */
  val p: Double = rₚ*(1 + e)

  /** point of periapsis */
  val r͢ₚ: Vector2 = Vector2.polar(ϖ, rₚ)

  /** point of apoapsis */
  val r͢ₐ: Vector2 = Vector2.polar(ϖ+π, rₐ)

  /** semi-minor axis length */
  val b: Double = a*sqrt(1-pow(e, 2))
  require(b > 0, s"semi-minor axis ($b) must be positive")
  require(b <= a)

  /** orbital period */
  val T: Double = 2*π*sqrt(pow(a, 3)/μ)
  require(T > 0)

  /** orbital area */
  val A: Double = π*a*b

  /** mean angular motion */
  val n: Double = 2*π/T

  /** specific angular momentum */
  val h: Double = direction * sqrt(p*μ)

  /** time of periapsis passage */
  val tₚ: Double = tₒ - Mₒ/n

  /** mean anomaly */
  //def Mₜ(t: Double): Double = pow(μ,2)/pow(h,3)*pow(1-pow(e,2),3/2)*t
  def Mₜ(t: Double): Double = Mₒ + n*(t-tₒ)*direction

  /** eccentric anomaly */
  def Eₜ(t: Double, tolerance: Double = this.tolerance): Double = {
    val M = Mₜ(t)
    def θ(E: Double) = E - e*sin(E) - M
    def θ̇(E: Double) = 1 - e*cos(E)
    @tailrec def E(Eʹ: Double): Double = {
      val θʹ = θ(Eʹ)
      val θ̇ʹ = θ̇(Eʹ)
      val uncertainty = θʹ/θ̇ʹ
      if (uncertainty <= tolerance) Eʹ
      else {
        val Eʺ = Eʹ - θʹ / θ̇ʹ
        E(Eʺ)
      }
    }
    E(M + signum(π-M)*e/2)
  }

//  /** eccentric anomaly */
//  def Eₜ(t: Double, tolerance: Double = this.tolerance): Double = {
//    val M = Mₜ(t)
//    def θ(E: Double) = M - E + e*sin(E)
//    def θ̇(E: Double) = e*cos(E) - 1
//    @tailrec def E(Eʹ: Double): Double = {
//      val θʹ = θ(Eʹ)
//      val θ̇ʹ = θ̇(Eʹ)
//      val Eʺ = Eʹ - θʹ/θ̇ʹ
//      if (θʹ < tolerance) Eʺ // error: θʹ
//      else E(Eʺ)
//    }
//    E(M)
//  }
//


  /** true anomaly at time */
  def θₜ(t: Double): Double = {
    val E = Eₜ(t)
    atan2(a*sqrt(1-pow(e,2))*sin(E), a*(cos(E)-e))
  }
    //2*atan(sqrt((1+e)/(1-e))*tan(Eₜ(t)/2))

  val θₒ: Double = θₜ(tₒ)

  /** true longitude at true anomaly */
  def l(θ: Double): Double = ϖ + θ

  /** true longitude at time */
  def lₜ(t: Double): Double = l(θₜ(t))

  /** orbital distance at true anomaly */
  def r(θ: Double): Double = a*(1-pow(e, 2)) / (1+e*cos(θ))

  /** orbital distance at epoch */
  val rₒ: Double = r(θₒ)

  /** orbital distance at time */
  def rₜ(t: Double): Double = r(θₜ(t))

  /** orbital speed at true anomaly */
  def v(θ: Double): Double = sqrt(μ*(2/r(θ) - 1/a))

  /** orbital speed at epoch */
  val vₒ: Double = v(θₒ)

  /** orbital speed at time */
  def vₜ(t: Double): Double = v(θₜ(t))

  /** orbital position at true anomaly */
  def r⃯⃯(θ: Double): Vector2 = Vector2.polar(r(θ), l(θ))

  /** orbital position at time */
  override def r͢ₜ(t: Double): Vector2 = r⃯⃯(θₜ(t))

  /** angle of flight path at true anomaly */
  def ɸ(θ: Double): Double = acos(h/(r(θ)*v(θ)))

  /** angle of flight path at time */
  def ɸₜ(t: Double): Double = ɸ(θₜ(t))

  /** orbital velocity at true anomaly */
  def v⃯(θ: Double): Vector2 = Vector2.polar(v(θ), l(θ) + ɸ(θ))

  /** orbital velocity at time */
  override def v͢ₜ(t: Double): Vector2 = v⃯(θₜ(t))
}
