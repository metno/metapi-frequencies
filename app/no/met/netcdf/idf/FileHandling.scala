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

package no.met.netcdf.idf

import java.time._
import scala.util.matching._

case class DurationFrequency(duration: Duration, frequency: Period)

/**
 * Application-specific handling of IDF-containing netcdf files.
 */
object FileHandling {

  /**
   * Parse file name and -path to find its duration and frequency
   */
  def parseName(fileName: String): DurationFrequency = {

    val ValidExpression = ".*/?SpatGEV\\.res\\.([0-9]+)([a-z]+)/posterior\\.grid_return_([0-9]+)\\.nc$".r
    fileName match {
      case ValidExpression(durationValue, durationUnits, returnPeriod) => {
        durationUnits match {
          case "min" =>
            DurationFrequency(Duration.ofMinutes(durationValue.toInt), Period.ofYears(returnPeriod.toInt))
          case "hr" | "hour" =>
            DurationFrequency(Duration.ofHours(durationValue.toInt), Period.ofYears(returnPeriod.toInt))
          case _ =>
            throw new Exception("Unable to understand duration unit: " + durationUnits)
        }
      }
      case _ => throw new Exception("Unable to parse file name")
    }
  }
}
