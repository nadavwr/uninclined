package com.github.nadavwr.uninclined

import com.github.nadavwr.math._

object Trajectory {
  object Origin extends Trajectory {
    override def tₒ: Double = 0
    override def Tₒₚₜ: Option[Double] = Some(0)
    private val zero: State = new State(Vector2.zero, Vector2.zero)
    override protected def state(t: Double): State = zero
  }

  class ComposedTrajectory(primary: Trajectory, secondary: Trajectory) extends Trajectory {
    override def tₒ: Double = secondary.tₒ
    override def Tₒₚₜ: Option[Double] = secondary.Tₒₚₜ

    def state(t: Double): State = {
      val s1 = primary.state(t)
      val s2 = secondary.state(t)
      new State(s1.r⃯ + s2.r⃯, s1.v⃯ + s2.v⃯)
    }
  }
}

trait Trajectory {
  def tₒ: Double
  def Tₒₚₜ: Option[Double]

  protected class State(r⃯Lazy: => Vector2, v⃯Lazy: Vector2) {
    lazy val r⃯: Vector2 = r⃯Lazy
    lazy val v⃯: Vector2 = v⃯Lazy
  }
  protected def state(t: Double): State

  private var timestamp: Double = Double.MinValue
  private var cachedState: State = _
  private def cachingState(t: Double): State = {
    if (t != timestamp) {
      timestamp = t
      cachedState = state(t)
    }
    cachedState
  }

  final def r͢ₜ(t: Double): Vector2 = cachingState(t).r⃯
  final def v͢ₜ(t: Double): Vector2 = cachingState(t).v⃯

  final def compose(focus: Trajectory): Trajectory =
    new Trajectory.ComposedTrajectory(focus, this)
}
