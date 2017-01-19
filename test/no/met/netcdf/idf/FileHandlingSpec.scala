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

import no.met.netcdf.idf.FileHandling;

import java.time._

// scalastyle:off magic.number
class FileHandlingSpec extends Specification {
  "parseName" should {
    "parse hours duration" in {
      val df = FileHandling.parseName("/some/where/SpatGEV.res.6hr/posterior.grid_return_5.nc")
      df must_== DurationFrequency(Duration.ofHours(6), Period.ofYears(5))
    }
    "parse hours duration with alternative syntax" in {
      val df = FileHandling.parseName("/some/where/SpatGEV.res.1hour/posterior.grid_return_1.nc")
      df must_== DurationFrequency(Duration.ofHours(1), Period.ofYears(1))
    }
    "parse minutes duration" in {
      val df = FileHandling.parseName("/some/where/SpatGEV.res.1min/posterior.grid_return_200.nc")
      df must_== DurationFrequency(Duration.ofMinutes(1), Period.ofYears(200))
    }
    "allow relative path" in {
      val df = FileHandling.parseName("SpatGEV.res.10min/posterior.grid_return_100.nc")
      df must_== DurationFrequency(Duration.ofMinutes(10), Period.ofYears(100))
    }
    "fail on insane input" in {
      FileHandling.parseName("laks") must throwA[Exception]
    }
    "fail on unknown duration spec" in {
      FileHandling.parseName("SpatGEV.res.10foo/posterior.grid_return_100.nc") must throwA[Exception]
    }
    "fail on unknown character in duration value" in {
      FileHandling.parseName("SpatGEV.res.fmin/posterior.grid_return_100.nc") must throwA[Exception]
    }
    "fail on unknown character in frequency value" in {
      FileHandling.parseName("SpatGEV.res.1min/posterior.grid_return_kake.nc") must throwA[Exception]
    }
  }
}
