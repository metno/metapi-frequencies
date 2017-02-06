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
    * Implements the interface by delegating to a specific implementation based on the 'sources' query parameter.
    */
  def idfValues(qp: QueryParameters): List[RainfallIDF] = ProdIDFAccess.selectImplementation(qp.sources).idfValues(qp)

  /**
    * Implements the interface by delegating to a specific implementation. If the 'sources' query parameter is specified,
    * available sources for either gridded datasets or stations are returned. Otherwise, available sources for both types
    * are returned.
    */
  def idfSources(qp: QueryParameters): List[RainfallIDFSource] = {
    qp.sources match {
      case Some(_) => ProdIDFAccess.selectImplementation(qp.sources).idfSources(qp) // get available sources for one type only
      case None => ProdIDFAccess.gridAccess.idfSources(qp) ++ ProdIDFAccess.stationAccess.idfSources(qp) // get available sources for both types
    }
  }

  override def valuesNotFoundReason(qp: Option[QueryParameters]): String = ProdIDFAccess.selectImplementation(qp.get.sources).valuesNotFoundReason

  override def valuesNotFoundHelp(qp: Option[QueryParameters]): String = ProdIDFAccess.selectImplementation(qp.get.sources).valuesNotFoundHelp
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
  def selectImplementation(sources: Option[String]): ProdIDFAccess = {
    if (GridIDFAccess.name.toUpperCase == sources.getOrElse("").toUpperCase) gridAccess else stationAccess
  }
}


//$COVERAGE-OFF$