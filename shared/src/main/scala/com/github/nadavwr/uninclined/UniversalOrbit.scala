package com.github.nadavwr.uninclined

import com.github.nadavwr.math._

import scala.annotation.tailrec
import scala.math._

object UniversalOrbit {

  val defaultTolerance: Double = 1e-6

  //noinspection TypeAnnotation
  case class Elements(r͢ₒ: Vector2,
                      v͢ₒ: Vector2,
                      μ: Double,
                      tₒ: Double,
                      tolerance: Double = defaultTolerance) {
    val vₒ = v͢ₒ.r
    val rₒ = r͢ₒ.r
    val h = r͢ₒ⨯v͢ₒ
    val e⃯ = Vector2(v͢ₒ.y, -v͢ₒ.x) * h/μ - r͢ₒ.normalized
    val e = e⃯.r
    val vᵣₒ = r͢ₒ⋅v͢ₒ/rₒ // or (μ/h)*e*sin(θₒ)
    val α = 2/rₒ - pow(vₒ,2)/μ//if (e != 1) (1-e)/rₚ else 0

    /*
    private def a = 1/α
    val θₒ = r͢ₒ.θ

    private def Dₒ = tan(θₒ/2)
    private def Eₒ = acos((e+cos(θₒ))/(1+e*cos(θₒ)))
    private def Fₒ = acosh((e+cos(θₒ))/(1+e*cos(θₒ)))

    val χₒ = {
      if (e == 1) h/sqrt(μ) * Dₒ
      else if (e < 1) sqrt(a) * Eₒ
      else sqrt(-a) * Fₒ
    }
    */

    // useful identities
    /** periapsis */
    lazy val rₚ = pow(h,2)/μ * 1/(1+e)
    /** specific energy */
    lazy val ε = -α*μ/2

    lazy val T = 2*π*sqrt(pow(1/α,3)/μ)
    lazy val Tₒₚₜ = if (e < 1) Some(T) else None

    def S(z: Double): Double = {
      if (z > 0) {
        val `√z` = sqrt(z)
        (`√z`-sin(`√z`)) / pow(`√z`, 3)
      } else if (z < 0) {
        val `√-z` = sqrt(-z)
        (sinh(`√-z`)-`√-z`) / pow(`√-z`, 3)
      } else {
        1.0/6.0
      }
    }

    def C(z: Double): Double = {
      if (z > 0) {
        (1 - cos(sqrt(z))) / z
      } else if (z < 0) {
        (cosh(sqrt(-z)) - 1) / (-z)
      } else 1.0/2.0
    }

    @tailrec
    final def χₑₛₜ(χ: Double, Δt: Double, tolerance: Double = tolerance, i: Int = 0): Double = {
      val z = α * pow(χ, 2)
      val f = rₒ * vᵣₒ / sqrt(μ) * pow(χ, 2) * C(z) + (1 - α * rₒ) * pow(χ, 3) * S(z) + rₒ * χ - sqrt(μ) * Δt
      val ḟ = rₒ * vᵣₒ / sqrt(μ) * χ * (1 - z * S(z)) + (1 - α * rₒ) * pow(χ, 2) * C(z) + rₒ
      val ratio = if (ḟ != 0) f/ḟ else 0 //f/ḟ
      if (abs(ratio) <= tolerance) χ
      else {
        if (i > 100) {
          println(this)
          println(s"χ = $χ")
          println(s"Δt = $Δt")
          χ
        } else χₑₛₜ(χ - ratio, Δt, tolerance, i + 1)
      }
    }

    def χₜ(Δt: Double): Double = {
      val χʹ = sqrt(μ)*abs(α)*Δt
      χₑₛₜ(χʹ, Δt, tolerance)
    }

    def sₜ(Δt: Double): OrbitalState = {
      val χ = χₜ(Δt)
      val `χ²` = pow(χ,2)
      val `χ³` = pow(χ,3)
      val `√μ` = sqrt(μ)
      val z = α*`χ²`
      val c = C(z)
      val s = S(z)
      val f = 1 - `χ²`/rₒ*c
      val g = Δt - 1/`√μ`*`χ³`*s
      val r͢ₜ = r͢ₒ*f + v͢ₒ*g
      val rₜ = r͢ₜ.r
      val ḟ = `√μ`/(rₜ*rₒ) * (z*χ*s - χ)
      val ġ = 1 - `χ²`/rₜ*c
      val v͢ₜ = r͢ₒ*ḟ + v͢ₒ*ġ
      OrbitalState(r͢ₜ, v͢ₜ, tₒ+Δt, μ)
    }
  }

  def determineElements(state: OrbitalState, tolerance: Double = defaultTolerance): Elements =
    Elements(state.r⃯, state.v⃯, state.μ, state.t, tolerance)

  def determine(state: OrbitalState, tolerance: Double = defaultTolerance): UniversalOrbit =
    new UniversalOrbit(determineElements(state, tolerance))
}

class UniversalOrbit(val elements: UniversalOrbit.Elements) extends Orbit {

  override def tₒ: Double = elements.tₒ
  val rₚ: Double = elements.rₚ
  override val Tₒₚₜ = elements.Tₒₚₜ

  /** orbital position vector */
  override def r͢ₜ(t: Double): Vector2 = elements.sₜ(t-tₒ).r⃯

  /** orbital velocity vector */
  override def v͢ₜ(t: Double): Vector2 = elements.sₜ(t-tₒ).v⃯
}
