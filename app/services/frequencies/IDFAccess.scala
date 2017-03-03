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

import models.{RainfallIDF, RainfallIDFSource}
import scala.util._
import no.met.data._
import no.met.geometry._


/**
 * Holds query string parameters from the original request.
 */
case class QueryParameters(sources: Option[String] = None, types: Option[String] = None, fields: Option[String] = None, location: Option[String] = None,
  durations: Option[String] = None, frequencies: Option[String] = None, unit: Option[String] = None)


/**
  * Interface for overall IDF access. Implementations of this interface are injected in the controller in either production or
  * development mode.
  */
trait IDFAccess {

  /**
    * Extracts IDF values based on the query parameters. In production mode, qp.sources determines whether to extract from gridded data
    * or from specific station data. In dev mode, the mock implementation should mimic the production behaviour as closely as possible.
    */
  def idfValues(qp: QueryParameters): List[RainfallIDF]


  /**
    * Provides the specific reason text to be used when idfValues() returns an empty set.
    */
  protected def valuesNotFoundReason: String = ""


  /**
    * Overload used for selecting implementation based on query parameters.
    */
  def valuesNotFoundReason(qp: QueryParameters): String = ""


  /**
    * Provides the specific help text to be used when idfValues() returns an empty set.
    */
  protected def valuesNotFoundHelp: String = ""


  /**
    * Overload used for selecting implementation based on query parameters.
    */
  def valuesNotFoundHelp(qp: QueryParameters): String = ""


  /**
    * Extracts available IDF sources based on the query parameters. In production mode, qp.sources determines whether to extract from station data,
    * from gridded data, or (if qp.sources is omitted) from both. In dev mode, the mock implementation should mimic the production behaviour
    * as closely as possible.
    */
  def idfSources(qp: QueryParameters): List[RainfallIDFSource]


  /**
    * Template method that converts a string of comma-separated integers into a valid subset.
    * Throws a BadRequestException upon error.
    */
  private def extractSubset(available: Set[Int], requestedStr: Option[String], name: String): Set[Int] = {
    // convert the requested integers to a set
    val requested = requestedStr match {
      case Some(x) => Try(x.split(",").map(_.trim.toInt).toSet) match {
        case Success(req) => req
        case Failure(e) => throw new BadRequestException(s"Malformed query parameter: $name (not a comma-separated list of integers)")
      }
      case _ => Set[Int]()
    }

    // check if the requested integers is a subset of the available ones
    requested match {
      case req if req subsetOf available => req
      case req => throw new BadRequestException(
        s"Invalid query parameter: $name " +
          s"(values (${(req -- available).toSeq.sorted.mkString(", ")}) " +
          s"not in supported set (${available.toSeq.sorted.mkString(", ")}))")
    }
  }

  /**
    * Provides the available durations.
    */
  protected def availableDurations: Set[Int] = Set[Int]()

  /**
    * Converts a string of comma-separated durations into a set of integers.
    */
  protected def extractDurations(durations: Option[String]): Set[Int] = extractSubset(availableDurations, durations, "durations")

  /**
    * Provides the available frequencies.
    */
  protected def availableFrequencies: Set[Int] = Set[Int]()

  /**
    * Converts a string of comma-separated frequencies into a set of integers.
    */
  protected def extractFrequencies(frequencies: Option[String]): Set[Int] = extractSubset(availableFrequencies, frequencies, "frequencies")


  /**
    * Converts a WKT string to a (lon, lat) Point if possible. Otherwise throws a BadRequestException.
    */
  protected def extractLocation(location: Option[String]): Point = {
    def throwException: Point = throw new BadRequestException(
      location.getOrElse("").trim match {
        case "" => "Location not found"
        case loc => s"$loc is not a valid point"
      },
      Some("Supported syntax: location=POINT(<longitude degrees> <latitude degrees>)"))

    Try(Geometry.decode(location.getOrElse(""))) match {
      case Success(geom) => geom match {
        case point: Point if point.coordinates.length == 2 => point
        case _ => throwException
      }
      case Failure(_) => throwException
    }
  }
}
