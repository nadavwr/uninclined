package com.github.nadavwr.uninclined

import com.github.nadavwr.math._
import com.github.nadavwr.cspice.{State => SpiceState, _}

import scala.math._

object SpiceOrbit {

  def determineElements(state: OrbitalState): OrbitalElements = {
    val spiceState = {
      import state._
      SpiceState(r⃯.x/1000, r⃯.y/1000, 0, v⃯.x/1000, v⃯.y/1000, 0)
    }
    val elts = oscelt(spiceState, state.t, state.μ/1e9)
    val orbitalElements = {
      import elts._
      val direction = if (inc == 0) -1 else 1
      OrbitalElements(Mₒ = m0, rₚ = rp * 1000, e = ecc, tₒ = t0, μ = mu * 1e9,
                      ϖ = argp /*+ lnode + inc*/,
                      direction = direction)
    }
    orbitalElements
  }

  def determine(state: OrbitalState): Orbit = {
    new SpiceOrbit(determineElements(state))
  }
}

class SpiceOrbit(val elements: OrbitalElements) extends Orbit {

  import elements._

  val elts =
    Elts(
    rp = rₚ/1000,
    ecc = e,
    inc = if (direction == 1) π else 0,
    lnode = 0,
    argp = ϖ,
    m0 = Mₒ,
    t0 = elements.tₒ,
    mu = μ/1e9)

  override val tₒ: Double = elements.tₒ

  override val Tₒₚₜ: Option[Double] =
    if (e < 1) {
      val a: Double = rₚ/(1-e)
      Some(2*π*sqrt(pow(a,3)/μ))
    } else None


  override protected def state(t: Double): State = {
    lazy val spiceState = conics(elts, t)
    lazy val r⃯ = Vector2(spiceState.px, spiceState.py)*1000
    lazy val v⃯ = Vector2(spiceState.vx, spiceState.vy)*1000
    new State(r⃯, v⃯)
  }
}
