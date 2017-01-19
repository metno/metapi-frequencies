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

/**
 * Interface for extracting data from a source
 */
trait DataExtractor {
  /**
   * Get a string identifying this data source
   */
  def sourceIdentifier: String

  /**
   * Get a list of available variables
   */
  def availableVariables: Seq[String]

  /**
   * Extract data for the given variable and x,y coordinates
   */
  def getData(variable: String, x: Int, y: Int): Option[Float]

  /**
   * Get a translation object, which can translate from lon/lat to the nearest
   * x,y in the current file.
   */
  def getCoordinateSystemLookup: SimpleCoordinateSystemLookup
}
