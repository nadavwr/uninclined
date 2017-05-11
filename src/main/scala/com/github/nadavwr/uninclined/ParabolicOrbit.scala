package com.github.nadavwr.uninclined

import com.github.nadavwr.math._

class ParabolicOrbit(val elements: OrbitalElements)
  extends Orbit {

  import elements._
  require(e == 1)


  override def r͢ₜ(t: Double): Vector2 = ???
  override def v͢ₜ(t: Double): Vector2 = ???
}
