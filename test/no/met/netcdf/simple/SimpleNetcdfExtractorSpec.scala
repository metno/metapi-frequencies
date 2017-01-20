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
import no.met.netcdf.simple._
import scala.util._
import org.specs2.specification.BeforeAfterEach


// scalastyle:off magic.number
// scalastyle:off null
class SimpleNetcdfExtractorSpec extends Specification with BeforeAfterEach {

  val ncFileName = "/tmp/" + java.util.UUID.randomUUID.toString + ".nc"
  var nc: SimpleNetcdfExtractor = null

  def before {
    FileBuilder.makeTestFile(ncFileName).close()
    nc = new SimpleNetcdfExtractor(ncFileName)
  }

  "SimpleNetcdfExtractor constructor" should {

    "raise error on nonexisting file" in {
      new SimpleNetcdfExtractor("no/such/file.nc") must throwA[Exception]
    }

    "know its name" in {
      nc.sourceIdentifier must_== ncFileName
    }

    "list available variables" in {
      nc.availableVariables must_== Seq("quant_0_5")
    }

    "provide coordinate system information" in {
      nc.getCoordinateSystemLookup must_!= null
    }

    "extract data" in {
      nc.getData("quant_0_5", 6, 19) must_== Some(1)
    }

    "return None for empty grid point" in {
      nc.getData("quant_0_5", 2, 27) must_== None
    }

    "raise error on nonexisting parameter" in {
      nc.getData("nonexisting", 6, 19) must(throwA[Exception])
    }
  }

  def after {
    new java.io.File(ncFileName).delete
  }
}
