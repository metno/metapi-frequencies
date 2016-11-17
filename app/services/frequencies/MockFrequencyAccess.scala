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

import javax.inject.Singleton
import no.met.geometry.Point
import models._
import play.Logger

@Singleton
class MockFrequencyAccess extends FrequencyAccess("") {

  // scalastyle:off magic.number
  private val rainfallIDFs = List[RainfallIDF](
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
    )
  )
  // scalastyle:on

  def getRainfallIDFs(sources: Seq[String], durations: Set[Int], frequencies: Set[Int], unit: Option[String], fields: Set[String]): List[RainfallIDF] = {
    rainfallIDFs
      .filter (x => sources.length == 0 || sources.contains(x.sourceId.toUpperCase) )
  }

  // scalastyle:off magic.number
  private val rainfallIDFSources = List[RainfallIDFSource](
    new RainfallIDFSource(
      "18700",
      Some("1974-05-29T12:00:00Z"),
      Some("1975-05-29T12:00:00Z"),
      Some(42)
    ),
    new RainfallIDFSource(
      "18701",
      Some("1976-05-29T12:00:00Z"),
      Some("1977-05-29T12:00:00Z"),
      Some(42)
    ),
    new RainfallIDFSource(
      "18702",
      Some("1978-05-29T12:00:00Z"),
      Some("1979-05-29T12:00:00Z"),
      Some(42)
    )
  )
  // scalastyle:on

  def getRainfallIDFSources(sources: Seq[String], fields: Set[String]) : List[RainfallIDFSource] = {
    rainfallIDFSources
      .filter (x => sources.length == 0 || sources.contains(x.sourceId.toUpperCase) )
  }

}
