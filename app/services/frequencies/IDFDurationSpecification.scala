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

package services.frequencies

import scala.language.postfixOps
import scala.util._

/**
  * Parsing of IDF durations
  */
object IDFDurationSpecification {

  /** Creates a set of IDF duration entries from a string. Returns an empty set if None. Throws an exception upon invalid input.
    *
    * @param durations A comma-delimited list of IDF durations, each of which should be a positive integer.
    */
  def parse(durations: Option[String]): Set[Int] = {
    durations match {
      case Some(x) => {
        Try(x.split(",").map(_.trim.toInt).toSet) match {
          case Success(s) => {

            // ### hard coded for now; later, these values could be retrieved from a database at runtime
            // scalastyle:off magic.number
            val validDurations: Set[Int] = Set(1, 2, 3, 5, 10, 15, 20, 30, 45, 60, 90, 120,180, 360,720, 1440)

            if (s subsetOf validDurations) {
              s
            } else {
              throw new Exception(
                "Invalid query parameter: durations " +
                  s"(values (${(s -- validDurations).toSeq.sorted.mkString(", ")}) " +
                  s"not in supported set (${validDurations.toSeq.sorted.mkString(", ")}))")
            }
          }

          case Failure(e) => {
            throw new Exception("Malformed query parameter: durations (not a comma-separated list of integers)")
          }
        }
      }
      case _ => Set()
    }
  }

}

// scalastyle:on
