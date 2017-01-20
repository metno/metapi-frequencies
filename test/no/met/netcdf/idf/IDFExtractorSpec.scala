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

import org.specs2.mutable._

import no.met.netcdf.simple.FakeDataExtractor
import java.time._

// scalastyle:off magic.number
class IDFExtractorSpec extends Specification {
  "IDFExtractor's extract method" should {
    "return translated data" in {
      val x = new IDFExtractor(Seq(new FakeDataExtractor(1), new FakeDataExtractor(2), new FakeDataExtractor(3)))
      x.extract(11, 67).get.toSet must_== Set(
        IDF(1, Duration.ofHours(1), Period.ofYears(10)),
        IDF(2, Duration.ofHours(2), Period.ofYears(20)),
        IDF(3, Duration.ofHours(3), Period.ofYears(30)))
    }

    "filter durations" in {
      val x = new IDFExtractor(Seq(new FakeDataExtractor(1), new FakeDataExtractor(2), new FakeDataExtractor(3)))
      x.extract(11, 67, Some(Set(Duration.ofHours(1))), None).get.toSet must_== Set(
        IDF(1, Duration.ofHours(1), Period.ofYears(10)))
    }

    "filter frequencies" in {
      val x = new IDFExtractor(Seq(new FakeDataExtractor(1), new FakeDataExtractor(2), new FakeDataExtractor(3)))
      x.extract(11, 67, None, Some(Set(Period.ofYears(20)))).get.toSet must_== Set(
        IDF(2, Duration.ofHours(2), Period.ofYears(20)))
    }

  }
}
