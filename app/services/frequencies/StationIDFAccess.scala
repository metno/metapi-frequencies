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

import scala.language.postfixOps
import play.api.Play.current
import play.api.db._
import anorm._
import anorm.SqlParser._
import models.{IDFValue, RainfallIDF, RainfallIDFSource}
import no.met.data._
import scala.collection.mutable.MutableList
import scala.util.{ Try, Success, Failure }


//$COVERAGE-OFF$ Not testing database queries

/**
 * Overall IDF access from stations.
 */
class StationIDFAccess extends ProdIDFAccess {

  private def getNSeasonsPerStation: Map[Int, Int] = {
    val parser: RowParser[(Int, Int)] = {
      get[Int]("stid") ~
        get[Int]("nseasons") map {
        case stid ~ nseasons => (stid, nseasons)
      }
    }

    DB.withConnection("kdvh") { implicit connection =>
      val query = "SELECT DISTINCT stnr AS stid, max(season) AS nseasons FROM t_rr_intensity GROUP BY stnr"
      SQL(query).as(parser *).toMap
    }
  }

  private def getValidPeriodPerStation: Map[Int, (String, String)] = {
    val parser: RowParser[(Int, String, String)] = {
      get[Int]("stid") ~
        get[String]("validFrom") ~
        get[String]("validTo") map {
        case stid~validFrom~validTo => (stid, validFrom, validTo)
      }
    }

    DB.withConnection("kdvh") { implicit connection =>
      val query =
        """
          |SELECT stnr AS stid, to_char(min(fdato), 'YYYY-MM-DDT00:00:00Z') AS validFrom, to_char(max(tdato), 'YYYY-MM-DDT00:00:00Z') AS validTo
          |FROM t_elem_pdata
          |GROUP BY stnr""".stripMargin
      SQL(query).as(parser *).map(x => x._1 -> (x._2, x._3)).toMap
    }
  }

  private case class IDF(intensity: Double, duration: Int, frequency: Int)

  private def getIDFsPerStation: Map[Int, List[IDF]] = {
    val parser: RowParser[(Int, Int, Int, Double)] = {
      get[Int]("stid") ~
        get[Int]("duration") ~
        get[Int]("frequency") ~
        get[Double]("intensity") map {
        case stid~duration~frequency~intensity => (stid, duration, frequency, intensity)
      }
    }

    DB.withConnection("kdvh") { implicit connection =>
      var idfs: Map[Int, MutableList[IDF]] = Map[Int, MutableList[IDF]]()
      val query =
        """
          |SELECT stnr AS stid, duration, returnperiod AS frequency, litre_sec_hectar AS intensity
          |FROM t_rr_returnperiod
          |WHERE dependency_or_not = 'D'""".stripMargin
      SQL(query).as(parser *).foreach(x => {
        val stid = x._1
        val duration = x._2
        val frequency = x._3
        val intensity = x._4
        if (!idfs.contains(stid)) idfs += (stid -> MutableList[IDF]())
        idfs(stid) += IDF(intensity, duration, frequency)
      })
      idfs.map(x => x._1 -> x._2.toList)
    }
  }

  private def getOperatingPeriodsPerStation: Map[Int, Option[List[String]]] = {
    val parser: RowParser[(Int, Option[List[String]])] = {
      get[Int]("stid") ~
        get[Option[List[String]]]("operatingPeriods") map {
        case stid~operatingPeriods => (stid, operatingPeriods)
      }
    }

    DB.withConnection("kdvh") { implicit connection =>
      val query =
        """
          |SELECT
            |stnr AS stid,
            |array_agg(to_char(fdato, 'YYYY-MM-DDT00:00:00Z') || '/' || to_char(tdato, 'YYYY-MM-DDT00:00:00Z') ORDER BY fdato,tdato) AS operatingPeriods
          |FROM t_elem_pdata
          |GROUP BY stnr""".stripMargin
      SQL(query).as(parser *).toMap
    }
  }

  // Handles the 'values' case.
  private object idfValuesExec {

    def apply(qp: QueryParameters): List[RainfallIDF] = {

     val stations = SourceSpecification(qp.sources).stationNumbers

      val fields = FieldSpecification.parse(qp.fields).map(_.toLowerCase)
      val validFields = Set("operatingPeriods", "numberOfSeasons", "unit", "values")
      val invalidFields = fields -- validFields.map(_.toLowerCase)
      if (invalidFields.nonEmpty) {
        throw new BadRequestException(
          "Invalid fields in the query parameter: " + invalidFields.mkString(","),
          Some(s"Supported fields: ${validFields.mkString(", ")}"))
      }
      val showOperatingPeriods = fields.isEmpty || fields.contains("operatingperiods")
      val showNumberOfSeasons = fields.isEmpty || fields.contains("numberofseasons")
      val showUnit = fields.isEmpty || fields.contains("unit")
      // note: values is always shown, but can be specified as a field to exclude other optional fields

      val durations = extractDurations(qp.durations)
      val frequencies = extractFrequencies(qp.frequencies)

      val nSeasons: Map[Int, Int] = getNSeasonsPerStation
      val opPeriods: Map[Int, Option[List[String]]] = getOperatingPeriodsPerStation
      val idfs: Map[Int, List[IDF]] = getIDFsPerStation
      val lsha = "l/s*Ha"
      val mm = "mm"
      val unit = qp.unit.getOrElse(lsha)
      if (unit.nonEmpty && !Set(lsha, mm).contains(unit)) {
        throw new BadRequestException("Invalid intensity unit: " + unit, Some(s"Supported units: 'mm' and 'l/s_Ha'"))
      }

      (nSeasons.keys ++ opPeriods.keys ++ idfs.keys).toList
        .filter(stid => stations.isEmpty || stations.contains(stid.toString))
        .map(stid => {
          val sourceId = s"SN$stid"
          val version = None
          val geometry = None
          val operatingPeriods = if (opPeriods.contains(stid)) Some(opPeriods(stid).get.sorted) else None
          val numberOfSeasons = if (nSeasons.contains(stid)) Some(nSeasons(stid)) else None
          val values = if (idfs.contains(stid)) {
            idfs(stid)
              .filter(idf => (durations.isEmpty || durations.contains(idf.duration)) && (frequencies.isEmpty || frequencies.contains(idf.frequency)))
              .map(idf =>
                IDFValue(
                  // the formula below is derived as follows:
                  //   l/s*Ha = lsh = litres per second per hectar
                  //   lmh = litres per minute per hectar = lsh * 60
                  //   lmd = litres per minute per square decimeter = lmh / 100000
                  //   mmm = millimetres per minute = lmd * 100 = lmh / 10000 = lsh * 60 / 10000 = lsh * 0.006
                  //   mmmd = mmm * duration = lsh * 0.006 * duration
                  if (unit == lsha) idf.intensity else idf.intensity * 0.006 * idf.duration,
                  idf.duration,
                  idf.frequency)
              )
          } else {
            Seq[IDFValue]()
          }
          RainfallIDF(sourceId, version, geometry, operatingPeriods, numberOfSeasons, Some(unit), values)
        })
        .map(r => r.copy( // remove fields from output as required
          operatingPeriods = if (showOperatingPeriods) r.operatingPeriods else None,
          numberOfSeasons = if (showNumberOfSeasons) r.numberOfSeasons else None,
          unit = if (showUnit) r.unit else None
        ))
        .sortBy(r =>
          Try(r.sourceId.replace("SN", "").toInt) match {
            case Success(v) => v // sort by integer value if possible
            case _ => 0 // otherwise, don't sort
          })
    }
  }


  // Handles the 'sources' case.
  private object idfSourcesExec {

    // scalastyle:off method.length
    def apply(qp: QueryParameters): List[RainfallIDFSource] = {

      val stations = SourceSpecification(qp.sources).stationNumbers

      val fields = FieldSpecification.parse(qp.fields).map(_.toLowerCase)
      val validFields = Set("validFrom", "validTo", "numberOfSeasons")
      val invalidFields = fields -- validFields.map(_.toLowerCase)
      if (invalidFields.nonEmpty) {
        throw new BadRequestException(
          "Invalid fields in the query parameter: " + invalidFields.mkString(","),
          Some(s"Supported fields: ${validFields.mkString(", ")}"))
      }
      val showValidFrom = fields.isEmpty || fields.contains("validfrom")
      val showValidTo = fields.isEmpty || fields.contains("validto")
      val showNumberOfSeasons = fields.isEmpty || fields.contains("numberofseasons")

      val nSeasons: Map[Int, Int] = getNSeasonsPerStation
      val validPeriod: Map[Int, (String, String)] = getValidPeriodPerStation

      (nSeasons.keys ++ validPeriod.keys).toList
        .filter(stid => stations.isEmpty || stations.contains(stid.toString))
        .map(stid => {
          val sourceId = s"SN$stid"
          val version = None
          val (validFrom, validTo) = if (validPeriod.contains(stid)) { val p = validPeriod(stid); (Some(p._1), Some(p._2)) } else (None, None)
          val numberOfSeasons = if (nSeasons.contains(stid)) Some(nSeasons(stid)) else None
          RainfallIDFSource(sourceId, version, validFrom, validTo, numberOfSeasons)
        })
        .map(s => s.copy( // remove fields from output as required
          validFrom = if (showValidFrom) s.validFrom else None,
          validTo = if (showValidTo) s.validTo else None,
          numberOfSeasons = if (showNumberOfSeasons) s.numberOfSeasons else None
        ))
        .sortBy(s =>
          Try(s.sourceId.replace("SN", "").toInt) match {
            case Success(v) => v // sort by integer value if possible
            case _ => 0 // otherwise, don't sort
          })
    }
  }

  override def idfValues(qp: QueryParameters): List[RainfallIDF] = idfValuesExec(qp)

  override def idfSources(qp: QueryParameters): List[RainfallIDFSource] = idfSourcesExec(qp)

  override def availableDurations: Set[Int] = {
    // ### hard-coded for now; eventually these values could be retrieved from a database at runtime
    // scalastyle:off magic.number
    Set(1, 2, 3, 5, 10, 15, 20, 30, 45, 60, 90, 120,180, 360,720, 1440)
    // scalastyle:on magic.number
  }

  override def availableFrequencies: Set[Int] = {
    // ### hard-coded for now; evenyually these values could be retrieved from a database at runtime
    // scalastyle:off magic.number
    Set(2, 5, 10, 20, 25, 50, 100, 200)
    // scalastyle:on magic.number
  }

  protected override def valuesNotFoundReason: String =
    "Could not find rainfall IDF data for any of the station source ids and/or for this combination of durations and frequencies"

  protected override def valuesNotFoundHelp: String =
    "Ensure that rainfall IDF data exists for at least one station source id and that the combination of durations and frequencies is valid"

  protected override def typeAllowed(srcSpec: SourceSpecification): Boolean = srcSpec.typeAllowed(StationConfig.typeName)
  protected override def typeName: String = StationConfig.typeName
}

//$COVERAGE-ON$
