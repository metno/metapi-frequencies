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

import play.api.Play.current
import java.time._
import models._
import scala.util._
import no.met.data._
import no.met.geometry._
import no.met.netcdf.idf._


//$COVERAGE-OFF$ Not testing disk access

/**
 * Overall IDF access from gridded data.
 */
class GridIDFAccess extends ProdIDFAccess {

  private val gridExtractor: IDFExtractor = {
    val confParam = "idf.netcdf.basedir"
    current.configuration.getString(confParam) match {
      case Some(baseDir) => IDFExtractor.createFromBaseDir(s"$baseDir/${GridIDFAccess.name}")
      case None => throw new Exception(s"Configuration parameter for gridded data base directory not found: $confParam")
    }
  }

  // scalastyle:off cyclomatic.complexity
  override def idfValues(qp: QueryParameters): List[RainfallIDF] = {

    val durationsSet = extractDurations(qp.durations)
    val frequenciesSet = extractFrequencies(qp.frequencies)
    val point = extractLocation(qp.location)
    val (lon, lat) = (point.coordinates(0), point.coordinates(1))

    // get IDF values for all combinations of durations and frequencies at this location
    val idfs = gridExtractor.extract(
      lon, lat,
      if (durationsSet.isEmpty) None else Some(durationsSet.map(x => Duration.ofMinutes(x))),
      if (frequenciesSet.isEmpty) None else Some(frequenciesSet.map(x => Period.ofYears(x)))) match {
      case Success(result) => result
      case Failure(e) => throw new Exception("Failed to extract values from gridded data: " + e)
    }

    val unitIsMm = qp.unit match {
      case None => false
      case Some(x) if x == "mm" => true
      case Some(x) if x == "l/s*Ha" => false
      case _ => throw new BadRequestException("Invalid intensity unit: " + qp.unit.get, Some(s"Supported units: 'mm' and 'l/s*Ha'"))
    }


    // format return value
    idfs match {
      case x if x.isEmpty => List[RainfallIDF]()
      case _ => List(RainfallIDF(
        qp.sources.get,
        Some(point),
        None,
        None,
        Some(if (unitIsMm) "mm" else "l/s*Ha"),
        idfs.map(x => IDFValue(
          if (unitIsMm) {
            x.intensity
          } else {
            x.duration.toMinutes match {
              case dmins if dmins > 0 => x.intensity / (0.006 * dmins)
              case dmins => throw new Exception("Non-positive duration found in gridded data: " + dmins)
            }
          },
          x.duration.toMinutes,
          x.frequency.getYears))
      ))
    }
  }
  // scalastyle:on cyclomatic.complexity


  override def idfSources(qp: QueryParameters): List[RainfallIDFSource] = GridIDFAccess.availableSources(qp)

  override def availableDurations: Set[Int] = gridExtractor.availableDurations

  override def availableFrequencies: Set[Int] = gridExtractor.availableFrequencies

  protected override def valuesNotFoundReason: String = "Could not find rainfall IDF data at this location in the gridded data"

  protected override def valuesNotFoundHelp: String = "Ensure that the location is over a region where IDF data exists"
}


object GridIDFAccess {
  // WARNING: Hard-coded values may need to be updated upon changes to source data.
  def name: String = "idf_bma1km_v1"
  // scalastyle:off magic.number
  def availableSources(qp: QueryParameters): List[RainfallIDFSource] = {
    val fieldsSet = FieldSpecification.parse(qp.fields)
    List(RainfallIDFSource(
      name,
      if (fieldsSet.isEmpty || fieldsSet.contains("validfrom")) Some("1957-01-01T00:00:00Z") else None,
      if (fieldsSet.isEmpty || fieldsSet.contains("validto")) Some("2016-01-01T00:00:00Z") else None,
      if (fieldsSet.isEmpty || fieldsSet.contains("numberofseasons")) Some(59) else None))
  }
  // scalastyle:on magic.number
}

//$COVERAGE-ON$
