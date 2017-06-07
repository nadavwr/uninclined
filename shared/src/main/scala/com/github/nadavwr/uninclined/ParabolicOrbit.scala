package com.github.nadavwr.uninclined

import com.github.nadavwr.math._

class ParabolicOrbit(elements: OrbitalElements)
  extends Orbit {

  import elements._
  require(e == 1)

  override val tₒ: Double = elements.tₒ

  override protected def state(t: Double): State = ???
}
