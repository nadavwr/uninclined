package com.github.nadavwr.uninclined

import java.nio.file.{Files, Paths}

import scala.collection.JavaConverters._
import com.github.nadavwr.makeshift._
import com.github.nadavwr.math._

import scala.collection.mutable
import scala.language.implicitConversions
import scala.math._
import scala.util.Try

//noinspection TypeAnnotation
object UninclinedSpec extends App with Spec {
  case class Entry(name: String,
                   periapsisLongitude: Double,
                   mass: Double,
                   reducedMass: Double,
                   periapsisTime: Double,
                   eccentricity: Double,
                   periapsis: Double,
                   centralMass: Double,
                   radius: Double,
                   specificOrbitalAngularMomentum: Double,
                   orbitalAngularMomentum: Double,
                   periapsisArgument: Double,
                   semiminorAxis: Double,
                   orbitPeriod: Double,
                   hillRadius: Double,
                   semimajorAxis: Double,
                   averageOrbitDistance: Double,
                   averageOrbitVelocity: Double,
                   apoapsis: Double,
                   rocheLimit: Double,
                   orbitCenter: String,
                   color: Int,
                   symbolOpt: Option[String],
                   soiRadius: Double
                  )

  val (mutable.Buffer(headline), lines) =
    Files.readAllLines(Paths.get("elements.csv")).asScala.splitAt(1)
  val headers = headline.split(',')
  val elements: Seq[Map[String, String]] = lines.map {
    line =>
      val values = line.split(',')
      headers.zip(values).toMap
  }
  elements.foreach(println)

  trait EarthOrbitFixture extends Fixture {
    val earthElements = OrbitalElements(
      rₚ = 1.470980735498914E11/*m*/,
      e = 0.01671022,
      μ = 1.32712440018e20,
      direction = 1,
      ϖ = (-11.26064 + 102.94719)*2*π/360,
      Mₒ = 0,
      tₒ = 0)
    val earthOrbit = new EllipticOrbit(earthElements)
    val spiceOrbit = new SpiceOrbit(earthElements)

    val universalOrbit = {
      val state0 = OrbitalState(spiceOrbit.r͢ₜ(0), spiceOrbit.v͢ₜ(0), 0, earthElements.μ)
      UniversalOrbit.determine(state0)
    }
  }

  test("earth from perihelion to aphelion") runWith new EarthOrbitFixture {
    import earthOrbit._
    import earthElements._

    assertThat((T/86400 ~= 365.259636)(1e-5), "T ~= 365.259636 days") // anomalistic period
    assertThat((rₚ ~= 1.4709e11/*m*/)(1e-4), "rₚ ~= 1.4709e11")
    assertThat((rₚ ~= r(0))(1e-9), "rₚ ~= r(0)")
    assertThat((rₚ ~= rₜ(0))(1e-9), "rₚ ~= rₜ(0)")
    assertThat((rₚ ~= spiceOrbit.r͢ₜ(0).r)(1e-9), "rₚ ~= rₜ(0) (spice)")
    assertThat((rₚ ~= universalOrbit.r͢ₜ(0).r)(1e-9), "rₚ ~= rₜ(0) (universal)")
    assertThat((rₚ ~= r(2*π))(1e-9), "rₚ ~= r(2π)")
    assertThat((rₚ ~= rₜ(T))(1e-5), "rₚ ~= rₜ(T)")
    assertThat((rₐ ~= 1.5210e11/*m*/)(1e-4), "rₐ ~= 1.5210e11")
    assertThat((rₐ ~= r(π))(1e-9), "rₐ ~= r(π)")
    assertThat((rₐ ~= rₜ(T/2))(1e-9), "rₐ ~= rₜ(T/2)")
    assertThat((rₐ ~= spiceOrbit.r͢ₜ(T/2).r)(1e-9), "rₐ ~= rₜ(T/2) (spice)")
    assertThat((rₐ ~= universalOrbit.r͢ₜ(T/2).r)(1e-9), "rₐ ~= rₜ(T/2) (universal)")

    val vₚ = sqrt(μ/a*(1+e)/(1-e)) // based solely on orbital elements
    val vₐ = sqrt(μ/a*(1-e)/(1+e)) // based solely on orbital elements
    assertThat((vₚ ~= 3.029e4)(1e-3), "vₚ ~= (3.029e4) √(μ/a)(1+e)/(1-e)")
    assertThat((v(0) ~= vₚ)(1e-9), "v(0) ~= vₚ")
    assertThat((vₜ(0) ~= vₚ)(1e-9), s"vₜ(0) ~= vₚ")
    assertThat((spiceOrbit.v͢ₜ(0).r ~= vₚ)(1e-9), s"vₜ(0) ~= vₚ (spice)")
    assertThat((universalOrbit.v͢ₜ(0).r ~= vₚ)(1e-9), s"vₜ(0) ~= vₚ (universal)")
    assertThat((v(2*π) ~= vₚ)(1e-9), "v(2π) ~= vₚ")
    assertThat((vₜ(T) ~= vₚ)(1e-5), s"vₜ(T) ~= vₚ")
    assertThat((v(π) ~= vₐ)(1e-9), "v(π) ~= vₐ")
    assertThat((vₜ(T/2) ~= vₐ)(1e-9), "vₜ(T/2) ~= vₐ")
    assertThat((spiceOrbit.v͢ₜ(T/2).r ~= vₐ)(1e-9), "vₜ(T/2) ~= vₐ (spice)")
    assertThat((universalOrbit.v͢ₜ(T/2).r ~= vₐ)(1e-9), "vₜ(T/2) ~= vₐ (universal)")


    println(" ͟a͟p͟h͟e͟l͟i͟o͟n͟ ")

    println("orbital velocity at epoch+T/2 (aphelion): "+ vₜ(T/2))
    println("orbital velocity at θ = π (aphelion): "+ v(π))
  }

}
