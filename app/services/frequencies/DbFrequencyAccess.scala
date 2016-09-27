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
import play.api._
import play.api.db._
import anorm._
import anorm.SqlParser._
import java.sql.Connection
import javax.inject.Singleton

import scala.language.postfixOps
import no.met.data.BadRequestException
import models._

//$COVERAGE-OFF$Not testing database queries
object RainfallIDFs {

  private val parser: RowParser[RainfallIDF] = {
    get[String]("sourceId") ~
    get[Option[Array[String]]]("operatingperiods") ~
    get[Option[Int]]("numberofseasons") ~
    get[Option[String]]("unit") ~
    get[Double]("intensity") ~
    get[Double]("duration") ~
    get[Int]("frequency") map {
      case id~operatingPeriods~nSeasons~unit~intensity~duration~frequency
        => RainfallIDF(id,
                       None,
                       if (operatingPeriods.isEmpty) {
                         None
                       } else {
                         Some(operatingPeriods.get.toSeq.sorted)
                       },
                       nSeasons,
                       unit,
                       Seq(IDFValue(intensity, duration, frequency)))
    }
  }

  private def getSelectQuery(fields: Set[String]) : String = {
    val legalFields = Set("operatingperiods", "numberofseasons", "unit")
    val illegalFields = fields -- legalFields
    if (!illegalFields.isEmpty) {
      throw new BadRequestException("Invalid fields in the query parameter: " + illegalFields.mkString(","))
    }
    val fieldStr = "sourceid, intensity, duration, frequency, " + fields.mkString(", ")
    val missing = legalFields -- fields
    if (missing.isEmpty) {
      fieldStr
    } else {
      val missingStr = missing
        .map( x => "NULL AS " + x )
        .mkString(", ")
        .replace("NULL AS values", "NULL AS intensity, NULL AS duration, NULL AS frequency")
      fieldStr + "," + missingStr
    }
  }

  // scalastyle:off method.length
  def getResult(sources: Seq[String], durations: Set[Int], frequencies: Set[Int], fields: Set[String]): List[RainfallIDF] = {

    val selectQ = if (fields.isEmpty) "*" else getSelectQuery(fields)

    val sourceQ =
      if (sources.isEmpty) {
        "TRUE"
      } else {
        s"stnr IN (${sources.mkString(",")})"
      }

    val durationsQ =
      if (durations.isEmpty) {
        ""
      } else {
        s" AND t4.duration IN (${durations.mkString(",")})"
      }

    val frequenciesQ =
      if (frequencies.isEmpty) {
        ""
      } else {
        s" AND t4.returnperiod IN (${frequencies.mkString(",")})"
      }


    val query = s"""
                   |SELECT
                     |$selectQ
                   |FROM
                     |(SELECT
                       |'SN' || t3.stnr AS sourceId,
                       |t3.operatingPeriods,
                       |t3.nSeason AS numberOfSeasons,
                       |'l/s*Ha' AS unit,
                       |t4.litre_sec_hectar AS intensity,
                       |t4.duration,
                       |t4.returnperiod AS frequency
                     |FROM
                       |(SELECT
                         |t1.stnr, t1.operatingPeriods, t2.nSeason
                       |FROM
                         |(SELECT
                           |stnr, array_agg(TO_CHAR(fdato, 'YYYY-MM-DDT00:00:00Z') || '/' || TO_CHAR(tdato, 'YYYY-MM-DDT00:00:00Z')) AS operatingPeriods
                         |FROM
                           |t_elem_pdata
                         |WHERE
                           |$sourceQ
                         |GROUP BY stnr) t1
                       |LEFT OUTER JOIN
                         |(SELECT
                           |stnr, max(season) AS nSeason
                         |FROM
                           |t_rr_intensity
                         |GROUP BY
                           |stnr) t2 ON (t1.stnr = t2.stnr)) t3, t_rr_returnperiod t4
                     |WHERE t3.stnr = t4.stnr AND
                           |t4.dependency_or_not = 'I'
                           |$durationsQ
                           |$frequenciesQ
                     |ORDER BY sourceId, duration, frequency) t5
      """.stripMargin

    Logger.debug(query)

    DB.withConnection("kdvh") { implicit connection =>
      val sqlResult = SQL(query).as( parser * )
      // TODO: Quick and dirty implementation. Convert to idiomatic scala.
      // List append is not an efficient implementation, so this needs to be improved.
      var result = List[RainfallIDF]()
      for (res <- sqlResult) {
        if (!result.isEmpty && result.last.sourceId == res.sourceId)
          result.last.values = result.last.values ++ res.values
        else
          result = result :+ res
      }
      result
    }

  }
  // scalastyle:on
}


object RainfallIDFSources {

  private val parser: RowParser[RainfallIDFSource] = {
    get[String]("sourceId") ~
      get[Option[String]]("validfrom") ~
      get[Option[String]]("validto") ~
      get[Option[Int]]("numberofseasons") map {
      case id~validFrom~validTo~nSeasons
      => RainfallIDFSource(id, validFrom, validTo, nSeasons)
    }
  }

  private def getSelectQuery(fields: Set[String]) : String = {
    val legalFields = Set("validfrom", "validto", "numberofseasons")
    val illegalFields = fields -- legalFields
    if (!illegalFields.isEmpty) {
      throw new BadRequestException("Invalid fields in the query parameter: " + illegalFields.mkString(","))
    }
    val fieldStr = "sourceid, " + fields.mkString(", ")
    val missing = legalFields -- fields
    if (missing.isEmpty) {
      fieldStr
    } else {
      val missingStr = missing
        .map( x => "NULL AS " + x )
        .mkString(", ")
      fieldStr + "," + missingStr
    }
  }

  // scalastyle:off method.length
  def getResult(sources: Seq[String], fields: Set[String]): List[RainfallIDFSource] = {

    val selectQ = if (fields.isEmpty) "*" else getSelectQuery(fields)

    val sourceQ =
      if (sources.isEmpty) {
        "TRUE"
      } else {
        s"stnr IN (${sources.mkString(",")})"
      }

    val query = s"""
                   |SELECT DISTINCT
                     |$selectQ, stnr
                   |FROM
                     |(SELECT
                       | t3.stnr AS stnr,
                       |'SN' || t3.stnr AS sourceId,
                       |t3.validFrom,
                       |t3.validTo,
                       |t3.nSeason AS numberOfSeasons
                     |FROM
                       |(SELECT
                         |t1.stnr, t1.validFrom, t1.validTo, t2.nSeason
                     |FROM
                       |(SELECT
                         |stnr,
                         |TO_CHAR(min(fdato), 'YYYY-MM-DDT00:00:00Z') AS validFrom,
                         |TO_CHAR(max(tdato), 'YYYY-MM-DDT00:00:00Z') AS validTo
                       |FROM
                         |t_elem_pdata
                       |WHERE
                         |$sourceQ
                       |GROUP BY stnr) t1
                     |LEFT OUTER JOIN
                       |(SELECT
                         |stnr, max(season) AS nSeason
                       |FROM
                         |t_rr_intensity
                       |GROUP BY
                         |stnr) t2 ON (t1.stnr = t2.stnr)) t3, t_rr_returnperiod t4
                   |WHERE t3.stnr = t4.stnr AND
                         |t4.dependency_or_not = 'I'
                   |) t5 ORDER BY stnr
      """.stripMargin

    Logger.debug(query)

    DB.withConnection("kdvh") { implicit connection =>
      SQL(query).as( parser * )
    }

  }
  // scalastyle:on

}


@Singleton
class DbFrequencyAccess extends FrequencyAccess("") {

  def getRainfallIDFs(sources: Seq[String], durations: Set[Int], frequencies: Set[Int], fields: Set[String]): List[RainfallIDF] = {
    RainfallIDFs.getResult(sources, durations, frequencies, fields)
  }

  def getRainfallIDFSources(sources: Seq[String], fields: Set[String]): List[RainfallIDFSource] = {
    RainfallIDFSources.getResult(sources, fields)
  }

}

// $COVERAGE-ON$
