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

import models._
import no.met.geometry.Point
import no.met.data._
import scala.util._


/**
  * Mocked IDF access.
  */
class MockIDFAccess extends IDFAccess {

  // scalastyle:off magic.number

  private val rainfallIDF = List[RainfallIDF](
    new RainfallIDF(
      "18700",
      Some(Point("Point", Seq(10.54, 60.1))),
      Some(Seq("1974-05-29T12:00:00Z/1977-09-03T06:00:00Z", "1982-06-01T12:00:00/2016-09-08T12:00:00Z")),
      Some(42),
      Some("l/s*Ha"),
      Seq(IDFValue(322.8f, 2.0f, 5), IDFValue(312.8f, 5.2f, 5))
    ),
    new RainfallIDF(
      "18701",
      Some(Point("Point", Seq(10.54, 60.1))),
      Some(Seq("1974-05-29T12:00:00Z/1977-09-03T06:00:00Z", "1982-06-01T12:00:00/2016-09-08T12:00:00Z")),
      Some(42),
      Some("l/s*Ha"),
      Seq(IDFValue(322.8f, 2.0f, 5), IDFValue(312.8f, 5.2f, 5))
    ),
    new RainfallIDF(
      "18702",
      Some(Point("Point", Seq(10.54, 60.1))),
      Some(Seq("1974-05-29T12:00:00Z/1977-09-03T06:00:00Z", "1982-06-01T12:00:00/2016-09-08T12:00:00Z")),
      Some(42),
      Some("l/s*Ha"),
      Seq(IDFValue(322.8f, 2.0f, 5), IDFValue(312.8f, 5.2f, 5))
    ),
    new RainfallIDF(
      "idf_bma1km_v1",
      Some(Point("Point", Seq(10.54, 60.1))),
      None,
      None,
      Some("mm"),
      Seq(IDFValue(322.8f, 2.0f, 5), IDFValue(312.8f, 5.2f, 5))
    )
  )

  def idfValues(qp: QueryParameters): List[RainfallIDF] = {
    extractDurations(qp.durations)
    extractFrequencies(qp.frequencies)
    val srcSpec = SourceSpecification(qp.sources)
    if (srcSpec.idfGridNames.contains(IDFGridConfig.name)) { // grid case
      extractLocation(qp.location)
      rainfallIDF.filter(x => IDFGridConfig.name == x.sourceId)
    } else { // station case
      val stationNumbers = srcSpec.stationNumbers
      FieldSpecification.parse(qp.fields)
      rainfallIDF.filter(x => stationNumbers.length == 0 || stationNumbers.contains(x.sourceId))
    }
  }

  private val rainfallIDFStationSources = List[RainfallIDFSource](
    new RainfallIDFSource("18700", Some("1974-05-29T12:00:00Z"), Some("1975-05-29T12:00:00Z"), Some(42)),
    new RainfallIDFSource("18701", Some("1976-05-29T12:00:00Z"), Some("1977-05-29T12:00:00Z"), Some(42)),
    new RainfallIDFSource("18702", Some("1978-05-29T12:00:00Z"), Some("1979-05-29T12:00:00Z"), Some(42))
  )

  private val rainfallIDFGridSources = List[RainfallIDFSource](
    new RainfallIDFSource(IDFGridConfig.name, None, None, None)
  )

  def idfSources(qp: QueryParameters): List[RainfallIDFSource] = {
    extractDurations(qp.durations)
    extractFrequencies(qp.frequencies)

    val srcSpec = SourceSpecification(qp.sources, qp.types)

    var sources = List[RainfallIDFSource]()

    if (srcSpec.includeStationSources) { // type 1
      val stationIds = srcSpec.stationNumbers
      sources = sources ++ rainfallIDFStationSources.filter(s => stationIds.isEmpty || stationIds.contains(s.sourceId))
    }

    if (srcSpec.includeIdfGridSources) { // type 2
      val idfGridIds = srcSpec.idfGridNames
      sources = sources ++ rainfallIDFGridSources.filter(s => idfGridIds.isEmpty || idfGridIds.contains(s.sourceId))
    }

    // type 3 ...

    sources
  }

  override def availableDurations: Set[Int] = Set(20)

  override def availableFrequencies: Set[Int] = Set(20)

  // scalastyle:on magic.number
}
