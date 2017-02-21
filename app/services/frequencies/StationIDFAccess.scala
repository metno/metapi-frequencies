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


//$COVERAGE-OFF$ Not testing database queries

/**
 * Overall IDF access from stations.
 */
class StationIDFAccess extends ProdIDFAccess {

  /**
    * Generates fields to use in the SELECT clause.
    * @param optFieldsReq Requested optional fields (i.e. specified in the query parameter 'fields').
    * @param optFieldsSup Supported optional fields.
    * @param mndFields Mandatory fields.
    * @return A comma-separated list of fields.
    */
  private def getSelectQuery(optFieldsReq: Set[String], optFieldsSup: Set[String], mndFields: Set[String]): String = {
    // ensure that all requested optional fields are supported
    val unsupFields = optFieldsReq -- optFieldsSup
    if (unsupFields.nonEmpty) {
      throw new BadRequestException(s"Unsupported fields: ${unsupFields.mkString(",")}", Some(s"Supported fields: ${optFieldsSup.mkString(", ")}"))
    }

    // ensure that unrequested optional fields are output as null values
    val mndAndReqStr = (mndFields ++ optFieldsReq).mkString(", ") // mandatory fields and requested optional fields
    val optFieldsUnreq = optFieldsSup -- optFieldsReq // unrequested optional fields
    mndAndReqStr + (if (optFieldsUnreq.isEmpty) "" else ", " + optFieldsUnreq.map( x => "NULL AS " + x ).mkString(", "))
  }

  // Handles the 'values' case.
  private object idfValuesExec {

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

    // scalastyle:off method.length
    // scalastyle:off cyclomatic.complexity
    def apply(qp: QueryParameters): List[RainfallIDF] = {

      val stations = SourceSpecification(qp.sources).stationNumbers
      val fieldsSet = FieldSpecification.parse(qp.fields)
      val durationsSet = extractDurations(qp.durations)
      val frequenciesSet = extractFrequencies(qp.frequencies)

      val selectQ = if (fieldsSet.isEmpty) "*" else getSelectQuery(
        fieldsSet, Set("operatingperiods", "numberofseasons", "unit"), Set("sourceid", "intensity", "duration", "frequency"))

      val sourceQ =
        if (stations.isEmpty) {
          "TRUE"
        } else {
          s"stnr IN (${stations.mkString(",")})"
        }

      val durationsQ =
        if (durationsSet.isEmpty) {
          ""
        } else {
          s" AND t4.duration IN (${durationsSet.mkString(",")})"
        }

      val frequenciesQ =
        if (frequenciesSet.isEmpty) {
          ""
        } else {
          s" AND t4.returnperiod IN (${frequenciesSet.mkString(",")})"
        }

      val lsha = Array("l/s*Ha", "t4.litre_sec_hectar")
      // the formula below is derived as follows:
      //   l/s*Ha = lsh = litres per second per hectar
      //   lmh = litres per minute per hectar = lsh * 60
      //   lmd = litres per minute per square decimeter = lmh / 100000
      //   mmm = millimetres per minute = lmd * 100 = lmh / 10000 = lsh * 60 / 10000 = lsh * 0.006
      //   mmmd = mmm * duration = lsh * 0.006 * duration
      val mm = Array("mm", "t4.litre_sec_hectar * 0.006 * t4.duration")
      val unitQ = qp.unit match {
        case None => lsha
        case Some(x) if x == "l/s*Ha" => lsha
        case Some(x) if x == "mm" => mm
        case _ => {
          throw new BadRequestException(
            "Invalid intensity unit: " + qp.unit.get,
            Some(s"Supported units: 'mm' and 'l/s_Ha'"))
        }
      }

      val query = s"""
                     |SELECT
                     |$selectQ
                     |FROM
                     |(SELECT
                     |'SN' || t3.stnr AS sourceId,
                     |t3.operatingPeriods,
                     |t3.nSeason AS numberOfSeasons,
                     |'${unitQ(0)}' AS unit,
                     |${unitQ(1)} AS intensity,
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

      DB.withConnection("kdvh") { implicit connection =>
        val sqlResult = SQL(query).as( parser * )
        // TODO: Quick and dirty implementation. Convert to idiomatic scala.
        // List append is not an efficient implementation, so this needs to be improved.
        var result = List[RainfallIDF]()
        for (res <- sqlResult) {
          if (result.nonEmpty && result.last.sourceId == res.sourceId) {
            result.last.values = result.last.values ++ res.values
          } else {
            result = result :+ res
          }
        }
        result
      }
    }
    // scalastyle:on method.length
    // scalastyle:on cyclomatic.complexity
  }


  // Handles the 'sources' case.
  private object idfSourcesExec {

    private val parser: RowParser[RainfallIDFSource] = {
      get[String]("sourceId") ~
        get[Option[String]]("validfrom") ~
        get[Option[String]]("validto") ~
        get[Option[Int]]("numberofseasons") map {
        case id~validFrom~validTo~nSeasons
        => RainfallIDFSource(id, validFrom, validTo, nSeasons)
      }
    }

    // scalastyle:off method.length
    def apply(qp: QueryParameters): List[RainfallIDFSource] = {

      val stations = SourceSpecification(qp.sources).stationNumbers
      val fieldsSet = FieldSpecification.parse(qp.fields)

      val selectQ = if (fieldsSet.isEmpty) "*" else getSelectQuery(fieldsSet, Set("validfrom", "validto", "numberofseasons"), Set("sourceid", "stnr"))

      val sourceQ =
        if (stations.isEmpty) {
          "TRUE"
        } else {
          s"stnr IN (${stations.mkString(",")})"
        }

      val query = s"""
                     |SELECT DISTINCT
                     |$selectQ
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


      DB.withConnection("kdvh") { implicit connection =>
        SQL(query).as( parser * )
      }
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

  protected override def valuesNotFoundReason: String = "Could not find rainfall IDF data for any of the station source ids"

  protected override def valuesNotFoundHelp: String = "Ensure that rainfall IDF data exists for at least one station source id"
}

//$COVERAGE-ON$
