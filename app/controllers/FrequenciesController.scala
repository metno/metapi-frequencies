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

package controllers

import play.api._
import play.api.mvc._
import play.api.http.Status._
import com.github.nscala_time.time.Imports._
import javax.inject.Inject
import io.swagger.annotations._
import scala.language.postfixOps
import util._
import no.met.data.SourceSpecification
import models.RainfallIDF
import services.frequencies.{FrequencyAccess, RainfallIDFJsonFormat}

// scalastyle:off magic.number

@Api(value = "frequencies")
class FrequenciesController @Inject()(frequencyService: FrequencyAccess) extends Controller {

  @ApiOperation(
    value = "Get rainfall IDF data.",
    notes = "Get rainfall IDF data. To be expanded.",
    response = classOf[models.RainfallIDFResponse],
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 400, message = "Invalid parameter value or malformed request."),
    new ApiResponse(code = 401, message = "Unauthorized client ID."),
    new ApiResponse(code = 404, message = "No data was found for the list of query Ids."),
    new ApiResponse(code = 500, message = "Internal server error.")))
  def getRainfallIDFs( // scalastyle:ignore public.methods.have.type
    @ApiParam(value = "The MET API sourceID(s) that you want IDF data for. Enter a comma-separated list to select multiple sources.")
              sources: Option[String],
    @ApiParam(value = "A comma-separated list of the fields that should be present in the response. The sourceId and values attributes will always be returned in the query result. Leaving this parameter empty returns all attributes; otherwise only those properties listed will be visible in the result set (in addition to the sourceId and values); e.g.: unit,numberOfSeasons will show only sourceId, unit, numberOfSeasons and values in the data set.")
              fields: Option[String],
    @ApiParam(value = "The output format of the result.",
              allowableValues = "jsonld",
              defaultValue = "jsonld")
              format: String) = no.met.security.AuthorizedAction {
    implicit request =>
    // Start the clock
    val start = DateTime.now(DateTimeZone.UTC)
    val sourceList = if (sources.isEmpty) Seq[String]() else SourceSpecification.parse(sources.get)
    val fieldList : Set[String] = fields match {
        case Some(x) => x.toLowerCase.split(",").map(_.trim).toSet
        case _ => Set()
    }
    Try  {
      frequencyService.getRainfallIDFs(sourceList, fieldList)
    } match {
      case Success(data) =>
        if (data isEmpty) {
          NotFound("Could not find any rainfall IDF data for source ids " + sources.getOrElse("<all>"))
        } else {
          format.toLowerCase() match {
            case "jsonld" => Ok(new RainfallIDFJsonFormat().format(start, data)) as "application/vnd.no.met.data.frequencies.rainfallidf-v0+json"
            case x        => BadRequest(s"Invalid output format: $x")
          }
        }
      case Failure(x) => BadRequest(x getLocalizedMessage)
    }
  }

}

// scalastyle:on
