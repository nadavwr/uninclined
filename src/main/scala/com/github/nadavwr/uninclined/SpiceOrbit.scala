package com.github.nadavwr.uninclined

import com.github.nadavwr.math._
import com.github.nadavwr.cspice._

import scala.math._

object SpiceOrbit {
  def determineElements(state: OrbitalState): OrbitalElements = {
    val elts = {
      import state._
      val spiceState = State(r͢ₒ.x/1000, r͢ₒ.y/1000, 0, v͢ₒ.x/1000, v͢ₒ.y/1000, 0)
      oscelt(spiceState, tₒ, μ/1e9)
    }
    import elts._
    OrbitalElements(Mₒ = m0, rₚ = rp*1000, e = ecc, tₒ = t0, μ = mu*1e9,
                    ϖ = argp+lnode+inc,
                    direction = signum(lnode.toInt) /* correct? */)
  }
  def determine(state: OrbitalState): Orbit = {
    new SpiceOrbit(determineElements(state))
  }
}

class SpiceOrbit(override val elements: OrbitalElements) extends Orbit {

  import elements._

  val elts =
    Elts(
    rp = rₚ/1000,
    ecc = e,
    inc = 0,
    lnode = 0,
    argp = ϖ,
    m0 = Mₒ,
    t0 = tₒ,
    mu = μ/1e9)


  override val Tₒₚₜ: Option[Double] =
    if (e < 1) {
      val a: Double = rₚ/(1-e)
      Some(2*π*sqrt(pow(a,3)/μ))
    } else None



  /** orbital position vector */
  override def r͢ₜ(t: Double): Vector2 = {
    val state = conics(elts, t)
    Vector2(state.px, state.py)*1000
  }

  /** orbital velocity vector */
  override def v͢ₜ(t: Double): Vector2 = {
    val state = conics(elts, t)
    Vector2(state.vx, state.vy)*1000
  }
}
