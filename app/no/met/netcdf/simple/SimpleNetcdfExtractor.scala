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

import ucar.nc2.NetcdfFile
import scala.util.Try
import scala.collection.JavaConversions._
import ucar.nc2.Variable
import scala.util.Success
import scala.util.Failure

/**
 * Very simple netcdf reading. Assumes a file with a Lat and Lon variable,
 * containing ordered values specifying a curvilinear grid along longitude
 * and latitude lines. All other variables only have dimensions Lon, Lat.
 */
class SimpleNetcdfExtractor(val netcdfFileName: String, val isMissing: (Double) => Boolean = { _ < 0 }) extends DataExtractor {

  override def sourceIdentifier: String = netcdfFileName

  override def availableVariables: Seq[String] =
    ncFile.getVariables().
      filter { _.getDimensions().size() == 2 }.
      map { _.getShortName() }

  override def getData(variable: String, x: Int, y: Int): Option[Float] = {
    Option(ncFile.findVariable(variable)) match {
      case Some(v) => {
        val array = v.read(List(new ucar.ma2.Range(y, y), new ucar.ma2.Range(x, x)))
        val value = array.getFloat(0)
        if (isMissing(value)) {
          None
        } else {
          Some(value)
        }
      }
      case None => throw new Exception("No such variable: " + variable)
    }
  }

  override def getCoordinateSystemLookup: SimpleCoordinateSystemLookup = {
    SimpleCoordinateSystemLookup.make(ncFile)
  }

  val ncFile = NetcdfFile.open(netcdfFileName)
  validate()

  private def validate() = {
    validateCoordinates("Lon")
    validateCoordinates("Lat")
  }

  private def validateCoordinates(dimension: String) = {
    Option(ncFile.findVariable(dimension)) match {
      case None => throw new Exception("Missing " + dimension + " variable")
      case Some(variable) => {
      }
    }
  }
}
