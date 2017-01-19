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

import no.met.netcdf.simple.MultiFileExtractor
import no.met.netcdf.simple.SimpleNetcdfExtractor
import java.time._
import no.met.netcdf.simple.DataExtractor
import scala.util.Random


/**
 * Extractor for IDF values for a given point.
 *
 * Initialized with all source files to be used, which must follow naming
 * convention described in FileHandling object.
 */
class IDFExtractor(sources: Seq[DataExtractor]) {

  private val extractor = new MultiFileExtractor(sources)
  private val variableName = "quant_0_5"

  /**
   * Extract data for the given point.
   *
   * Data may arrive in any order.
   */
  def extract(longitude: Double, latitude: Double): Seq[IDF] = {
    val data = extractor.extract(variableName, longitude, latitude)
    data.toList.map{
      entry => {
        val fileName = entry._1
        val value = entry._2
        val df = FileHandling.parseName(fileName)
        IDF(value, df.duration, df.frequency)
      }
    }
  }

  def extract(longitude: Double, latitude: Double, durations: Option[Set[Duration]], frequencies: Option[Set[Period]]): Seq[IDF] = {
    var ret = extract(longitude, latitude)
    durations match {
      case Some(d) => ret = ret.filter{idf => d.contains(idf.duration)}
      case None =>
    }
    frequencies match {
      case Some(f) => ret = ret.filter{idf => f.contains(idf.frequency)}
      case None =>
    }
    ret
  }
}


object IDFExtractor {

  def create(netcdfFiles: Seq[String]): IDFExtractor = {
    new IDFExtractor(netcdfFiles.map{ new SimpleNetcdfExtractor(_) })
  }

  // scalastyle:off
  def main(args: Array[String]): Unit = {
    val baseFolder = "/home/vegardb/metapi/data/"
    //val baseFolder = "/lustre/storeB/project/KSS/reidung/run.SpatGEVBMA/ResultaterNYE/"
    val files = Seq(
        "SpatGEV.res.10min/posterior.grid_return_5.nc",
        "SpatGEV.res.10min/posterior.grid_return_10.nc",
        "SpatGEV.res.10min/posterior.grid_return_20.nc",
        "SpatGEV.res.10min/posterior.grid_return_25.nc",
        "SpatGEV.res.10min/posterior.grid_return_50.nc",
        "SpatGEV.res.10min/posterior.grid_return_100.nc",
        "SpatGEV.res.10min/posterior.grid_return_200.nc",
        "SpatGEV.res.12hr/posterior.grid_return_5.nc",
        "SpatGEV.res.12hr/posterior.grid_return_10.nc",
        "SpatGEV.res.12hr/posterior.grid_return_20.nc",
        "SpatGEV.res.12hr/posterior.grid_return_25.nc",
        "SpatGEV.res.12hr/posterior.grid_return_50.nc",
        "SpatGEV.res.12hr/posterior.grid_return_100.nc",
        "SpatGEV.res.12hr/posterior.grid_return_200.nc",
        "SpatGEV.res.1hour/posterior.grid_return_5.nc",
        "SpatGEV.res.1hour/posterior.grid_return_10.nc",
        "SpatGEV.res.1hour/posterior.grid_return_20.nc",
        "SpatGEV.res.1hour/posterior.grid_return_25.nc",
        "SpatGEV.res.1hour/posterior.grid_return_50.nc",
        "SpatGEV.res.1hour/posterior.grid_return_100.nc",
        "SpatGEV.res.1hour/posterior.grid_return_200.nc",
        "SpatGEV.res.30min/posterior.grid_return_5.nc",
        "SpatGEV.res.30min/posterior.grid_return_10.nc",
        "SpatGEV.res.30min/posterior.grid_return_20.nc",
        "SpatGEV.res.30min/posterior.grid_return_25.nc",
        "SpatGEV.res.30min/posterior.grid_return_50.nc",
        "SpatGEV.res.30min/posterior.grid_return_100.nc",
        "SpatGEV.res.30min/posterior.grid_return_200.nc",
        "SpatGEV.res.3hr/posterior.grid_return_5.nc",
        "SpatGEV.res.3hr/posterior.grid_return_10.nc",
        "SpatGEV.res.3hr/posterior.grid_return_20.nc",
        "SpatGEV.res.3hr/posterior.grid_return_25.nc",
        "SpatGEV.res.3hr/posterior.grid_return_50.nc",
        "SpatGEV.res.3hr/posterior.grid_return_100.nc",
        "SpatGEV.res.3hr/posterior.grid_return_200.nc",
        "SpatGEV.res.6hr/posterior.grid_return_5.nc",
        "SpatGEV.res.6hr/posterior.grid_return_10.nc",
        "SpatGEV.res.6hr/posterior.grid_return_20.nc",
        "SpatGEV.res.6hr/posterior.grid_return_25.nc",
        "SpatGEV.res.6hr/posterior.grid_return_50.nc",
        "SpatGEV.res.6hr/posterior.grid_return_100.nc",
        "SpatGEV.res.6hr/posterior.grid_return_200.nc"
        ).
        map { baseFolder + _ }

    val extractor = create(files)

    val times = {
      Random.shuffle((6.0 to 13.0 by 0.1).toList) map { lon =>
        Random.shuffle((59.0 to 68.0 by 0.1).toList) map { lat =>
          val start = System.currentTimeMillis
          if (extractor.extract(lon, lat, Some(Set(Duration.ofMinutes(10))), Some(Set(Period.ofYears(50)))).isEmpty) {
            -1
          } else {
            val stop = System.currentTimeMillis.toDouble
            stop - start
          }
        }
      }
    }.flatten.filter{_>=0}
    println(times)
    println(times.length + " elements")
    println("Average time to fetch: " + times.sum / times.length)

    extractor.extract(10, 60, None, Some(Set(Period.ofYears(200)))).foreach(println)
  }
}
