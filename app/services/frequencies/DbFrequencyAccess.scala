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

import java.sql.Connection
import javax.inject.Singleton

import scala.language.postfixOps
import no.met.geometry.Point
import models._

//$COVERAGE-OFF$Not testing database queries
@Singleton
class DbFrequencyAccess extends FrequencyAccess("") {

  val rainfallIDFs = List[RainfallIDF](
    new RainfallIDF(
      Some("SN18700"),
      Some(Point("Point", Seq(10.54, 60.1))),
      Some(Seq("1974-05-29T12:00:00Z/1977-09-03T06:00:00Z", "1982-06-01T12:00:00/2016-09-08T12:00:00Z")),
      Some(42),
      Some("l/s*Ha"),
      Seq(IDFValue(322.8f, 2.0f, 5), IDFValue(312.8f, 5.2f, 5))
    )
  )

  def getRainfallIDFs(sources: List[String], fields: Set[String]): List[RainfallIDF] = {
    rainfallIDFs
      .filter (x => sources.length == 0 || sources.contains(x.sourceId.get.toUpperCase) )
  }

  /*
  val parser: RowParser[Element] = {
    get[Option[String]]("id") ~
    get[Option[String]]("name") ~
    get[Option[String]]("description") ~
    get[Option[String]]("unit") ~
    get[Option[String]]("codetable") ~
    get[Option[Array[String]]]("legacymetnoconvention_elemcodes") ~
    get[Option[String]]("legacymetnoconvention_category") ~
    get[Option[String]]("legacymetnoconvention_unit") ~
    get[Option[String]]("cfconvention_standardname") ~
    get[Option[String]]("cfconvention_cellmethod") ~
    get[Option[String]]("cfconvention_unit") ~
    get[Option[String]]("cfconvention_status") map {
      case id~name~desc~unit~codeTable~kdvhCodes~kdvhCategory~kdvhUnit~cfName~cfMethod~cfUnit~cfStatus 
        => Element(id,
                  name,
                  desc,
                  unit,
                  codeTable,
                  if (kdvhCodes.isEmpty)
                    None
                  else
                    Some(LegacyMetNoConvention(Some(kdvhCodes.get.toSeq.sorted), kdvhCategory, kdvhUnit)),
                  if (cfName.isEmpty)
                    None
                  else
                    Some(CfConvention(cfName, cfMethod, cfUnit, cfStatus)))
    }
  }
  
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
  

  def getRainfallIDFs(ids: List[String], elemCodes: List[String], cfNames: List[String], fields: Set[String], lang: Option[String]): List[Element] = {
    Logger.debug(fields.isEmpty.toString)
    // Set up projection clause based on fields
    val selectQ = 
      if (fields.isEmpty)
        "id, name, description, unit, codetable, legacymetnoconvention_elemcodes, legacymetnoconvention_category, legacymetnoconvention_unit, cfconvention_standardname, cfconvention_cellmethod, cfconvention_unit, cfconvention_status" 
      else
        getSelectQuery(fields)
    // Filter for selected ids
    val idList = ids.mkString("','")
    val idQ =
      if (ids.isEmpty)
        "id IS NOT NULL"
      else
        s"LOWER(id) IN ('$idList')"
    // Filter for selected legacy codes
    val elemList = elemCodes.mkString("','")
    val elemQ =
      if (elemCodes.isEmpty)
        "TRUE"
      else
        s"legacymetnoconvention_elemcodes && ARRAY['$elemList']"
    // Filter for selected standard names
    val cfList = cfNames.mkString("','")
    val cfQ =
      if (cfNames.isEmpty)
        "TRUE"
      else
        s"cfconvention_standardname IN ('$cfList')"
    // Filter for Locale
    val localeQ = "locale = '" + lang.getOrElse("en-US") + "'";
    
    val query = s"""
      |SELECT
        |$selectQ
      |FROM
        |get_elements_v
      |WHERE
        |$idQ AND
        |$elemQ AND
        |$cfQ AND
        |$localeQ
      """.stripMargin

    Logger.debug(query)

    DB.withConnection("elements") { implicit connection =>
      SQL(query).as( parser * )
    }
  }
  */

}
// $COVERAGE-ON$
