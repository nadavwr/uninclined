package com.github.nadavwr.uninclined
import java.lang.{Double => JDouble}
import java.nio.file.{Files, Paths}

import com.github.nadavwr.math._

import scala.collection.JavaConverters._
import scala.collection.mutable

object SolarSystem {

  val elementsSeq: Seq[Map[String, String]] = {
    val (mutable.Buffer(headline), lines) =
      Files.readAllLines(Paths.get("elements.csv")).asScala
        .splitAt(1)

    val headers = headline.split(',')

    lines.map {
      line =>
        val values = line.split(',')
        headers.zip(values).toMap
    }
  }


  def orbitFromElements(elements: Map[String, String]): UniversalOrbit = {
    val periapsis = JDouble.valueOf(elements("Periapsis"))
    val longitude = JDouble.valueOf(elements("PeriapsisLongitude"))
    val relativeBearing = longitude + π / 2

    val mass = JDouble.valueOf(elements("Mass"))
    val angularMomentum = JDouble.valueOf(elements("OrbitalAngularMomentum"))
    val tangentialVelocity = angularMomentum / (mass * periapsis)

    val relativePosition = Vector2.polar(periapsis, longitude)
    val relativeVelocity = Vector2.polar(tangentialVelocity, relativeBearing)

    val time = JDouble.valueOf(elements("PeriapsisTimeLast"))

    val mu = G * mass

    val orbitalState =
      OrbitalState(relativePosition, relativeVelocity, time, mu)

    UniversalOrbit.determine(orbitalState, 1e13)
  }

  val relativeOrbits: Map[String, Trajectory] =
    elementsSeq
      .map { elts => elts("Name") -> orbitFromElements(elts) }
      .toMap


}
