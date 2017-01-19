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

import org.specs2.mutable._

// scalastyle:off magic.number
class MultiFileExtractorSpec extends Specification {

  "MultiFileExtractor's constructor" should {

    "work when input list is empty" in {
      val mfe = new MultiFileExtractor(Seq.empty[DataExtractor])
      mfe.available must beEmpty
      mfe.extract("foo", 11, 59) must throwA[IllegalArgumentException]
    }

    "raise exception on request for nonexistent parameter" in {
      val mfe = new MultiFileExtractor(Seq(FakeDataExtractor(1), FakeDataExtractor(3), FakeDataExtractor(5)))
      mfe.extract("nosuchelement", 10, 68) must throwA[IllegalArgumentException]
    }

    "give output values" in {
      val mfe = new MultiFileExtractor(Seq(FakeDataExtractor(1), FakeDataExtractor(3), FakeDataExtractor(5)))
      mfe.extract("foo", 10, 68) must_== Map[String, Float](
        "fake/SpatGEV.res.1hour/posterior.grid_return_10.nc" -> 1,
        "fake/SpatGEV.res.3hour/posterior.grid_return_30.nc" -> 3,
        "fake/SpatGEV.res.5hour/posterior.grid_return_50.nc" -> 5)
    }
  }
}
