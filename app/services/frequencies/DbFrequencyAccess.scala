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
import models._

//$COVERAGE-OFF$Not testing database queries
@Singleton
class DbFrequencyAccess extends FrequencyAccess("") {

  val parser: RowParser[RainfallIDF] = {
    get[Option[String]]("sourceId") ~
    get[Option[Array[String]]]("operatingPeriods") ~
    get[Option[Int]]("numberOfSeasons") ~
    get[Option[String]]("unit") ~
    get[Double]("intensity") ~
    get[Double]("duration") ~
    get[Int]("frequency") map {
      case id~operatingPeriods~nSeasons~unit~intensity~duration~frequency
        => RainfallIDF(id,
                       None,
                       Some(operatingPeriods.get.toSeq.sorted),
                       nSeasons,
                       unit,
                       Seq(IDFValue(intensity, duration, frequency)))
    }
  }

  def getRainfallIDFs(sources: Seq[String], fields: Set[String]): List[RainfallIDF] = {
    val sourceList = sources.mkString(",")
    val sourceQ =
      if (sources.isEmpty)
        "TRUE"
      else
        s"stnr IN ($sourceList)"
    val query = s"""
                   |SELECT
                     |'SN' || t3.stnr AS sourceId,
                     |t3.operatingPeriods,
                     |t3.nSeason AS numberOfSeasons,
                     |'l/s*Ha' AS unit,
                     |t4.litre_sec_hectar AS intensity,
                     |t4.duration,
                     |t4.returnperiod AS frequency
                   |FROM
                     |(select
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
                   |ORDER BY sourceId, duration, frequency
      """.stripMargin

    Logger.debug(query)

    DB.withConnection("kdvh") { implicit connection =>
      val sqlResult = SQL(query).as( parser * )
      // TODO: Quick and dirty implementation. Convert to idiomatic scala.
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

  /*

  def getSelectQuery(fields: Set[String]) : String = {
    val legalFields = Set("id", "name", "description", "unit", "codetable", "legacymetnoconvention", "cfconvention")
    val fieldStr = fields
      .mkString(", ")
      .replace("legacymetnoconvention", "array_to_string(legacymetnoconvention_elemcodes, ','), legacymetnoconvention_category, legacymetnoconvention_unit")
      .replace("cfconvention", "cfconvention_standardname, cfconvention_cellmethod, cfconvention_unit, cfconvention_status")
    val missing = legalFields -- fields
    if (missing.isEmpty)
      fieldStr
    else {
      val missingStr = missing
        .map( x => "NULL AS " + x )
        .mkString(", ")
        .replace("NULL AS legacymetnoconvention", "NULL AS legacymetnoconvention_elemcodes, NULL AS legacymetnoconvention_category, NULL AS legacymetnoconvention_unit")
        .replace("NULL AS cfconvention", "NULL AS cfconvention_standardname, NULL AS cfconvention_cellmethod, NULL AS cfconvention_unit, NULL AS cfconvention_status")
      fieldStr + "," + missingStr
    }
  }
  */

}
// $COVERAGE-ON$
