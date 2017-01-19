/*
    MET-API

    Copyright (C) 2014 met.no
    Contact information:
    Norwegian Meteorological Institute
    Box 43 Blindern
    0313 OSLO
    NORWAY
    E-mail: met-api@met.no

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.
    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
    MA 02110-1301, USA
*/

package no.met.netcdf.simple

import scala.collection.JavaConversions._
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import scala.util._
import ucar.nc2.NetcdfFile
import scala.collection._
import scala.collection.Searching._
import scala.language.postfixOps
import scala.math.Ordering

/**
 * Performs conversions related to lat/lon and grid index.
 *
 * Assumes that longitudeList and latitudeList are ordered - ascending or descending.
 */
class SimpleCoordinateSystemLookup(longitudes: IndexedSeq[Double], latitudes: IndexedSeq[Double]) {

  private val longitudeOrdering: Ordering[Double] = {
    if (longitudes(0) < longitudes(1)) {
      Ordering[Double]
    } else {
      Ordering[Double].reverse
    }
  }

  private val latitudeOrdering: Ordering[Double] = {
    if (latitudes(0) < latitudes(1)) {
      Ordering[Double]
    } else {
      Ordering[Double].reverse
    }
  }

  /**
   * Get nearest x and y index in a grid using, or None if outside grid
   */
  def nearestPoint(longitude: Double, latitude: Double): Option[(Int, Int)] = {
    val ox = SimpleCoordinateSystemLookup.nearest(longitude, longitudes, longitudeOrdering)
    val oy = SimpleCoordinateSystemLookup.nearest(latitude, latitudes, latitudeOrdering)
    (ox, oy) match {
      case (Some(x), Some(y)) => Some((x, y))
      case _ => None
    }
  }

  override def toString: String = {
    "CoordinateSystemLookup[" + longitudes.length + "][" + latitudes.length + "]"
  }
}

object SimpleCoordinateSystemLookup {

  /**
   * Sanity-checking construction of a SimpleCoordinateSystemLookup object
   */
  def make(ncFile: NetcdfFile): Try[SimpleCoordinateSystemLookup] = Try {
    val lon = Option[ucar.nc2.Variable](ncFile.findVariable("Lon"))
    val lat = Option[ucar.nc2.Variable](ncFile.findVariable("Lat"))

    (lon, lat) match {
      case (Some(longitude), Some(latitude)) => {
        val lon = longitude.read()
        val lat = latitude.read()
        make(lon, lat).get
      }
      case _ =>
        throw new Exception("Unable to extract grid from file")
    }
  }

  def make(lon: ucar.ma2.Array, lat: ucar.ma2.Array): Try[SimpleCoordinateSystemLookup] = Try {
    new SimpleCoordinateSystemLookup(
      ensuredSortedSequence(lon),
      ensuredSortedSequence(lat))
  }

  private def ensuredSortedSequence(inputArray: ucar.ma2.Array): IndexedSeq[Double] = {
    val a = inputArray.get1DJavaArray(inputArray.getElementType).asInstanceOf[Array[Double]].toIndexedSeq

    if (a.size < 2) {
      throw new Exception("Too small grid")
    }
    val orderCheck = {
      if (a(0) < a(1)) {
        (x: Double, y: Double) => x < y
      } else {
        (x: Double, y: Double) => x > y
      }
    }
    a.sliding(2).foreach { value =>
      if (!orderCheck(value(0), value(1))) {
        throw new Exception("Unsorted input array")
      }
    }
    a
  }

  def nearest(target: Double, source: IndexedSeq[Double], ordering: Ordering[Double] = Ordering[Double]): Option[Int] = {
    source.search(target)(ordering) match {
      case Found(x) => Some(x)
      case InsertionPoint(x) => {
        if (x == 0 || x >= source.length) {
          None
        } else {
          Some {
            if (Math.abs(target - source(x - 1)) < Math.abs(target - source(x))) {
              x - 1
            } else {
              x
            }
          }
        }
      }
    }
  }
}
