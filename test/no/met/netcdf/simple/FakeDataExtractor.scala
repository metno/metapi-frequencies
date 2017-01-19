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

// scalastyle:off magic.number
case class FakeDataExtractor(output: Int) extends DataExtractor {
  override def sourceIdentifier: String = "fake/SpatGEV.res." + output + "hour/posterior.grid_return_" + output * 10 + ".nc"
  override def availableVariables: Seq[String] = Seq("quant_0_5", "foo", "bar")
  override def getData(variable: String, x: Int, y: Int): Option[Float] = Some(output)
  override def getCoordinateSystemLookup: SimpleCoordinateSystemLookup =
    new SimpleCoordinateSystemLookup(Array(8, 9, 10, 11, 12), Array(70, 69, 68, 67, 66, 65, 64))
}
