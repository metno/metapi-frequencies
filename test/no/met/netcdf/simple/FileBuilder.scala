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
import ucar.nc2.NetcdfFileWriter
import ucar.nc2.NetcdfFileWriter.Version
import ucar.nc2.Dimension
import ucar.nc2.Variable
import ucar.ma2.DataType
import ucar.ma2.Index
import scala.collection.JavaConversions._
import scala.util._



/**
 * Creating a simple netcdf file for testing
 */
object FileBuilder {

  // scalastyle:off magic.number
  // scalastyle:off null

  def makeTestFile(): NetcdfFile = {
    val fileName = "/tmp/" + java.util.UUID.randomUUID().toString() + ".nc"
    makeTestFile(fileName)
  }

  def makeTestFile(fileName: String): NetcdfFile = {
    val nc = NetcdfFileWriter.createNew(Version.netcdf4, fileName)
    val lonDimension = nc.addDimension(null, "Lon", 20)
    val lon = nc.addVariable(null, "Lon", DataType.DOUBLE, "Lon")
    val latDimension = nc.addDimension(null, "Lat", 30)
    val lat = nc.addVariable(null, "Lat", DataType.DOUBLE, "Lat")
    val vdata = nc.addVariable(null, "quant_0_5", DataType.FLOAT, "Lat Lon")
    nc.create
    nc.write(lon, ucar.ma2.Array.makeArray(DataType.DOUBLE, lonDimension.getLength, 0, 1))
    nc.write(lat, ucar.ma2.Array.makeArray(DataType.DOUBLE, latDimension.getLength, 70, -1))
    val data = vdata.read

    (0 until 20).foreach { x =>
      (0 until 30).foreach { y =>
        data.setFloat(data.getIndex.set(y,x), -999)
      }
    }

    (5 to 15).foreach { x =>
      (10 to 20).foreach { y =>
        data.setFloat(data.getIndex.set(y,x), 1)
      }
    }
    nc.write(vdata, data)
    nc.getNetcdfFile
  }
}
