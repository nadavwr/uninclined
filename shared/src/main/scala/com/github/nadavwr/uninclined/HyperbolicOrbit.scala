package com.github.nadavwr.uninclined

import com.github.nadavwr.math._

import scala.annotation.tailrec
import scala.math._

class HyperbolicOrbit(val elements: OrbitalElements)
  extends Orbit {

  import elements._
  require(e > 1)

  override val tₒ: Double = elements.tₒ

  /** semimajor axis */
  val a: Double = -rₚ/(1-e)
  require(a < 0)

  /** semi-latus rectum */
  val ℓ: Double = a*(pow(e,2)-1)

  /** specific angular momentum */
  val h: Double = sqrt(μ*ℓ)

  /** impact parameter (semi-minor axis) */
  val b: Double = -a*sqrt(pow(2,e)-1)

  /** hyperbolic excess velocity */
  val `v∞`: Double = sqrt(-μ/a)

  /** specific orbital energy */
  val ε: Double = -μ/(2*a)

  /** true anomaly as distance tends to infinity */
  val `θ∞`: Double = acos(-1/e)

  def acosh(a: Double): Double = log(a + sqrt(pow(a,2)-1))

  def M(t: Double): Double = (t-tₒ)/sqrt(pow(-a, 3)/μ) + Mₒ

  /** hyperbolic eccentric anomaly */
  def F(t: Double, tolerance: Double = 1e-4): Double = {
    val Mₜ = M(t)
    def θᴱ(F: Double) = Mₜ + F - e*sinh(F)
    def θ̇ᴱ(F: Double) = 1 - e*cosh(F)
    @tailrec def Fᴱ(Fⱼ: Double): Double = {
      val θᴱⱼ = θᴱ(Fⱼ)
      val θ̇ᴱⱼ = θ̇ᴱ(Fⱼ)
      val `Fⱼ₊₁` = Fⱼ - θᴱⱼ/θ̇ᴱⱼ
      if (θᴱⱼ < tolerance) `Fⱼ₊₁` // error: θᴱⱼ
      else Fᴱ(`Fⱼ₊₁`)
    }
    Fᴱ(Mₜ)
  }

  def θₜ(t: Double): Double = {
    val k = cosh(F(t))
    acos((k+e)/(1+k*e))
  }

  def r(θ: Double): Double = ℓ/(1+e*cos(θ))

  def ϕ(θ: Double): Double = atan(e*sin(θ)/(1+e*cos(θ)))
  def v(θ: Double): Double = sqrt(μ*(2/r(θ) - 1/a))

  override protected def state(t: Double): State = {
    lazy val θ = θₜ(t)
    lazy val r⃯ = Vector2.polar(r(θ), ϖ+θ)
    lazy val v⃯ = Vector2.polar(v(θ), ϖ+ϕ(θ))
    new State(r⃯, v⃯)
  }

}
