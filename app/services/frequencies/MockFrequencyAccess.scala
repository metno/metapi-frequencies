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
  val rainfallIDFs = List[RainfallIDF](
    new RainfallIDF(
      Some("SN18700"),
      Some(Point("Point", Seq(10.54, 60.1))),
      Some(Seq("1974-05-29T12:00:00Z/1977-09-03T06:00:00Z", "1982-06-01T12:00:00/2016-09-08T12:00:00Z")),
      Some(42),
      Some("l/s*Ha"),
      Seq(IDFValue(322.8f, 2.0f, 5), IDFValue(312.8f, 5.2f, 5))
    ),
    new RainfallIDF(
      Some("SN18701"),
      Some(Point("Point", Seq(10.54, 60.1))),
      Some(Seq("1974-05-29T12:00:00Z/1977-09-03T06:00:00Z", "1982-06-01T12:00:00/2016-09-08T12:00:00Z")),
      Some(42),
      Some("l/s*Ha"),
      Seq(IDFValue(322.8f, 2.0f, 5), IDFValue(312.8f, 5.2f, 5))
    ),
    new RainfallIDF(
      Some("SN18702"),
      Some(Point("Point", Seq(10.54, 60.1))),
      Some(Seq("1974-05-29T12:00:00Z/1977-09-03T06:00:00Z", "1982-06-01T12:00:00/2016-09-08T12:00:00Z")),
      Some(42),
      Some("l/s*Ha"),
      Seq(IDFValue(322.8f, 2.0f, 5), IDFValue(312.8f, 5.2f, 5))
    )

  )
  // scalastyle:on

  def getRainfallIDFs(sources: List[String], fields: Set[String]): List[RainfallIDF] = {
    rainfallIDFs
      .filter (x => sources.length == 0 || sources.contains(x.sourceId.get.toUpperCase) )
  }

}
