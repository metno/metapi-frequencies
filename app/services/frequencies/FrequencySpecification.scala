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
  * Parsing of frequencies (return periods)
  */
object FrequencySpecification {

  /** Creates a set of frequency entries from a string. Returns an empty set if None. Throws an exception upon invalid input.
    *
    * @param frequencies A comma-delimited list of frequencies (return periods), each of which should be a positive integer.
    */
  def parse(frequencies: Option[String]): Set[Int] = {
    frequencies match {
      case Some(x) => {
        Try(x.split(",").map(_.trim.toInt).toSet) match {
          case Success(s) => {

            // ### hard coded for now; later, these values could be retrieved from a database at runtime
            // scalastyle:off magic.number
            val validFrequencies: Set[Int] = Set(2, 5, 10, 20, 25, 50, 100, 200)

            if (s subsetOf validFrequencies) {
              s
            } else {
              throw new Exception(
                "Invalid query parameter: frequencies " +
                  s"(values (${(s -- validFrequencies).toSeq.sorted.mkString(", ")}) " +
                  s"not in supported set (${validFrequencies.toSeq.sorted.mkString(", ")}))")
            }
          }

          case Failure(e) => {
            throw new Exception("Malformed query parameter: frequencies (not a comma-separated list of integers)")
          }
        }
      }
      case _ => Set()
    }
  }

}

// scalastyle:on
