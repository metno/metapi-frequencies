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

// scalastyle:off magic.number
class SimpleCoordinateSystemLookupSpec extends Specification {
  "CoordinateSystemLookup's nearest function" should {
    val evenSource = Vector[Double](0, 10, 20, 30, 40, 50)
    val oddSource = Vector[Double](0, 10, 20, 30, 40)

    evenSource foreach { value =>
      "work for even number of entries" in {
        SimpleCoordinateSystemLookup.nearest(value, evenSource) must_== Some(value / 10)
      }
    }

    oddSource foreach { value =>
      "work for odd number of entries" in {
        SimpleCoordinateSystemLookup.nearest(value, oddSource) must_== Some(value / 10)
      }
    }

    Seq(0, 10, 20, 30, 40) foreach { value =>
      "give nearest value downwards" in {
        SimpleCoordinateSystemLookup.nearest(value + 0.1, evenSource) must_== Some(value / 10)
      }
    }

    Seq(0, 10, 20, 30) foreach { value =>
      "give nearest value downwards" in {
        SimpleCoordinateSystemLookup.nearest(value + 0.1, oddSource) must_== Some(value / 10)
      }
    }

    Seq(10, 20, 30, 40, 50) foreach { value =>
      "give nearest value upwards" in {
        SimpleCoordinateSystemLookup.nearest(value - 0.2, evenSource) must_== Some(value / 10)
      }
    }

    Seq(10, 20, 30, 40) foreach { value =>
      "give nearest value upwards" in {
        SimpleCoordinateSystemLookup.nearest(value - 0.2, oddSource) must_== Some(value / 10)
      }
    }

    "give nearest value with reversed ordering" in {
      SimpleCoordinateSystemLookup.nearest(31, oddSource.reverse, Ordering[Double].reverse) must_== Some(1)
    }

    "give nearest value with reversed ordering" in {
      SimpleCoordinateSystemLookup.nearest(29, oddSource.reverse, Ordering[Double].reverse) must_== Some(1)
    }

    "give closest outside range in reversed grid" in {
      SimpleCoordinateSystemLookup.nearest(-3, oddSource.reverse, Ordering[Double].reverse) must_== None
    }

    "give closest outside range in reversed grid" in {
      SimpleCoordinateSystemLookup.nearest(340, oddSource.reverse, Ordering[Double].reverse) must_== None
    }

    "give closest outside range" in {
      SimpleCoordinateSystemLookup.nearest(-3, oddSource) must_== None
    }

    "give closest outside range" in {
      SimpleCoordinateSystemLookup.nearest(340, oddSource) must_== None
    }
  }

  "CoordinateSystemLookup's nearestPoint method" should {

    val csl = new SimpleCoordinateSystemLookup(
      Array[Double](11, 12, 13, 14, 15, 16),
      Array[Double](67, 66, 65, 64, 63, 62, 61))

    "gives no results for queries outside grid" in {
      csl.nearestPoint(10, 64) must be(None)
    }
    "gives no results for queries outside grid" in {
      csl.nearestPoint(17, 64) must be(None)
    }
    "gives no results for queries outside grid" in {
      csl.nearestPoint(13, 60) must be(None)
    }
    "gives no results for queries outside grid" in {
      csl.nearestPoint(13, 68) must be(None)
    }

    "gives correct result for queries inside grid (X)" in {
      csl.nearestPoint(13.2, 61.9) match {
        case Some((x, _)) => x must_== 2
        case None => false must_== true
      }
    }

    "gives correct result for queries inside grid (y)" in {
      csl.nearestPoint(13.2, 61.9) match {
        case Some((_, y)) => y must_== 5
        case None => false must_== true
      }
    }

  }

}
