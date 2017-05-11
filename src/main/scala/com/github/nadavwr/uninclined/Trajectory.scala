package com.github.nadavwr.uninclined

import com.github.nadavwr.math._

object Trajectory {
  object Origin extends Trajectory {
    override def tₒ: Double = 0
    override def Tₒₚₜ: Option[Double] = Some(0)

    override def r͢ₜ(t: Double): Vector2 = Vector2.zero
    override def v͢ₜ(t: Double): Vector2 = Vector2.zero
  }

  class ComposedTrajectory(primary: Trajectory, secondary: Trajectory) extends Trajectory {
    override def tₒ: Double = secondary.tₒ
    override def Tₒₚₜ: Option[Double] = secondary.Tₒₚₜ
    override def r͢ₜ(t: Double): Vector2 = secondary.r͢ₜ(t) + primary.r͢ₜ(t)
    override def v͢ₜ(t: Double): Vector2 = secondary.v͢ₜ(t) + primary.v͢ₜ(t)
  }
}

trait Trajectory {
  def tₒ: Double
  def Tₒₚₜ: Option[Double]

  def r͢ₜ(t: Double): Vector2
  def v͢ₜ(t: Double): Vector2

  def compose(focus: Trajectory): Trajectory = 
    new Trajectory.ComposedTrajectory(focus, this)
}
