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

import scala.util._
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

/**
 * Extract data from several sources at once.
 *
 * FIXME: (maybe) Assumes all data extractors have the same CoordinateSystemLookup
 */
class MultiFileExtractor(val extractors: Seq[DataExtractor]) {

  private lazy val csl = extractors.head.getCoordinateSystemLookup

  /**
   * List of available variables
   */
  val available = extractors.flatMap { f => f.availableVariables }.toSet

  /**
   * Extract data for the given variable and lon/lat coordinates
   */
  def extract(variable: String, longitude: Double, latitude: Double): Map[String, Float] = {
    if (!available.contains(variable)) {
      throw new IllegalArgumentException("No such variable: " + variable)
    }
    csl.nearestPoint(longitude, latitude) match {
      case None => Map.empty[String, Float]
      case Some((x, y)) => extractFromIndex(variable, x, y)
    }
  }

  private def extractFromIndex(variable: String, x: Int, y: Int): Map[String, Float] = {
    extractors.map { e =>
      val value = e.getData(variable, x, y)
      value match {
        case Some(v) => Some(e.sourceIdentifier -> v)
        case None => None
      }
    }.flatMap { x => x }.toMap
  }

  // Returns all durations available in the source files.
  def availableDurations: Set[Int] = {
    // ### hard-coded for now; eventually the values could be read from the source files (and maybe cached if the files are not expected to change)
    // scalastyle:off magic.number
    Set(10, 30, 60, 180, 360, 720)
    // scalastyle:on magic.number
  }

  // Returns all frequencies available in the source files.
  def availableFrequencies: Set[Int] = {
    // ### hard-coded for now; eventually the values could be read from the source files (and maybe cached if the files are not expected to change)
    // scalastyle:off magic.number
    Set(5, 10, 20, 25, 50, 100, 200)
    // scalastyle:on magic.number
  }

}
