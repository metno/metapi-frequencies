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
import no.met.data._
import scala.util._


//$COVERAGE-OFF$ Not testing production mode IDF access

/**
  * Class to handle IDF access in production mode. An instance of this class is injected in the controller. A request for an endpoint is
  * delegated to a subclass selected according query parameters: If the 'sources' parameter specifies a gridded dataset, an instance of
  * GridIDFAccess handles the request. Otherwise, an instande of StationIDFAccess handles the request (and throws an exception upon invalid
  * syntax etc.)
  */
class ProdIDFAccess extends IDFAccess {

  /**
    * Implements the interface by delegating to a specific implementation based on the query parameters.
    */
  def idfValues(qp: QueryParameters): List[RainfallIDF] = ProdIDFAccess.selectImplementation(qp).idfValues(qp)

  /**
    * Implements the interface by delegating to a specific implementation. If the 'sources' query parameter is specified,
    * available sources for either gridded datasets or stations are returned. Otherwise, available sources for both types
    * are returned.
    */
  def idfSources(qp: QueryParameters): List[RainfallIDFSource] = {
    qp.sources match {
      case Some(_) => ProdIDFAccess.selectImplementation(qp, false).idfSources(qp) // get available sources for one type only
      case None => ProdIDFAccess.gridAccess.idfSources(qp) ++ ProdIDFAccess.stationAccess.idfSources(qp) // get available sources for both types
    }
  }

  override def valuesNotFoundReason(qp: QueryParameters): String = ProdIDFAccess.selectImplementation(qp).valuesNotFoundReason

  override def valuesNotFoundHelp(qp: QueryParameters): String = ProdIDFAccess.selectImplementation(qp).valuesNotFoundHelp
}


/**
  * Companion object that holds one implementation of ProdIDFAccess for gridded datasets, and one for specific station access.
  */
object ProdIDFAccess {

  lazy val gridAccess = new GridIDFAccess
  lazy val stationAccess = new StationIDFAccess

  /**
    * Uses the sources query parameter to return either the gridded data implementation or the station implementation.
    */
  def selectImplementation(qp: QueryParameters, gridRequiresLocation: Boolean = true): ProdIDFAccess = {
    val reqGridName = qp.sources.getOrElse("")
    val gridNameMatch = GridIDFAccess.name.toUpperCase == reqGridName.toUpperCase

    if (gridRequiresLocation) {
      // assume grid case is requested iff location is specified or grid name matches
      if (qp.location.isDefined) {
        if (!gridNameMatch) throw new BadRequestException(s"invalid gridded dataset: $reqGridName, expected: ${GridIDFAccess.name}")
        gridAccess
      } else if (gridNameMatch) {
        throw new BadRequestException("no location found for gridded dataset")
      } else {
        stationAccess
      }
    } else if (gridNameMatch) {
      gridAccess
    } else {
      stationAccess
    }
  }
}


//$COVERAGE-OFF$
