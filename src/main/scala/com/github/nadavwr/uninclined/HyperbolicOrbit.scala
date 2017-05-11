package com.github.nadavwr.uninclined

import com.github.nadavwr.math._

import scala.annotation.tailrec
import scala.math._

class HyperbolicOrbit(override val elements: OrbitalElements)
  extends Orbit {

  import elements._
  require(e > 1)

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

  def θ(t: Double): Double = {
    val k = cosh(F(t))
    acos((k+e)/(1+k*e))
  }

  def r(t: Double): Double = ℓ/(1+e*cos(θ(t)))
  override def r͢ₜ(t: Double): Vector2 = Vector2.polar(r(t), ϖ+θ(t))
  def ϕ(t: Double): Double = {
    val θₜ = θ(t)
    atan(e*sin(θₜ)/(1+e*cos(θₜ)))
  }
  def v(t: Double): Double = sqrt(μ*(2/r(t) - 1/a))
  override def v͢ₜ(t: Double): Vector2 = Vector2.polar(v(t), ϖ+ϕ(t))
}
