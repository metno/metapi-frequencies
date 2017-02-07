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
import no.met.data._
import models.RainfallIDF
import services.frequencies._


@Api(value = "frequencies")
class FrequenciesController @Inject()(idfAccess: IDFAccess) extends Controller {

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
  def getRainfallIDF( // scalastyle:ignore public.methods.have.type
    // scalastyle:off line.size.limit
    @ApiParam(value = "The MET API source ID(s) that you want IDF data for. Enter either 1) a comma-separated list of one or more stations (each of the form SN&lt;number&gt;[:&lt;number&gt;|all]), or 2) the name of a gridded dataset.")
              sources: Option[String],
    @ApiParam(value = "The geographic position from which to get IDF data in case of a gridded dataset. Format: POINT(&lt;longitude degrees&gt; &lt;latitude degrees&gt). Data from the nearest grid point is returned.")
              location: Option[String],
    @ApiParam(value = "The MET API IDF duration(s), in minutes, that you want IDF data for. Enter a comma-separated list to select multiple durations.")
              durations: Option[String],
    @ApiParam(value = "The MET API IDF frequencies (return periods), in years, that you want IDF data for. Enter a comma-separated list to select multiple frequencies.")
              frequencies: Option[String],
    @ApiParam(value = "The unit of measure for the intensity. Specify 'mm' for millimetres per minute multiplied by the duration, or 'l/s*Ha' for litres per second per hectar. The default unit is 'l/s*Ha'")
              unit: Option[String],
    @ApiParam(value = "A comma-separated list of the fields that should be present in the response. The sourceId and values attributes will always be returned in the query result. Leaving this parameter empty returns all attributes; otherwise only those properties listed will be visible in the result set (in addition to the sourceId and values); e.g.: unit,numberOfSeasons will show only sourceId, unit, numberOfSeasons and values in the data set.")
              fields: Option[String],
    // scalastyle:on line.size.limit
    @ApiParam(value = "The output format of the result.",
              allowableValues = "jsonld",
              defaultValue = "jsonld")
              format: String) = no.met.security.AuthorizedAction { implicit request =>

    val start = DateTime.now(DateTimeZone.UTC) // start the clock
    val queryParams = QueryParameters(sources, fields, location, durations, frequencies, unit)

    Try  {
      // ensure that the query string contains supported fields only
      QueryStringUtil.ensureSubset(Set("sources", "fields", "location", "durations", "frequencies", "unit"), request.queryString.keySet)

      idfAccess.idfValues(queryParams)

    } match {
      case Success(data) =>
        if (data isEmpty) {
          Error.error(NOT_FOUND, Some(idfAccess.valuesNotFoundReason(queryParams)), Some(idfAccess.valuesNotFoundHelp(queryParams)), start)
        } else {
          format.toLowerCase() match {
            case "jsonld" => Ok(new RainfallIDFJsonFormat().format(start, data)) as "application/vnd.no.met.data.frequencies.rainfall-v0+json"
            case x        => Error.error(BAD_REQUEST, Some(s"Invalid output format: $x"), Some("Supported output formats: jsonld"), start)
          }
        }
      case Failure(x: BadRequestException) =>
        Error.error(BAD_REQUEST, Some(x getLocalizedMessage), x help, start)
      case Failure(x) => {
        //$COVERAGE-OFF$
        Logger.error(x.getLocalizedMessage)
        Error.error(INTERNAL_SERVER_ERROR, Some("An internal error occurred"), None, start)
        //$COVERAGE-ON$
      }
    }
  }


  @ApiOperation(
    value = "Get available sources for rainfall IDF data.",
    notes = "Get available sources for rainfall IDF data. To be expanded.",
    response = classOf[models.RainfallIDFSourcesResponse],
    httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 400, message = "Invalid parameter value or malformed request."),
    new ApiResponse(code = 401, message = "Unauthorized client ID."),
    new ApiResponse(code = 404, message = "No data was found for the list of query Ids."),
    new ApiResponse(code = 500, message = "Internal server error.")))
  def getRainfallIDFSources( // scalastyle:ignore public.methods.have.type
                       @ApiParam(value = "A comma-separated list of MET API sourceIDs that you want information for. If left out, information for all available sources is returned.")
                       sources: Option[String],
                       @ApiParam(value = "A comma-separated list of the fields that should be present in the response. The sourceId attribute will always be returned in the query result. Leaving this parameter empty returns all attributes; otherwise only those properties listed will be visible in the result set (in addition to the sourceId and values); e.g.: unit,numberOfSeasons will show only sourceId, unit, numberOfSeasons and values in the data set.")
                       fields: Option[String],
                       @ApiParam(value = "The output format of the result.",
                         allowableValues = "jsonld",
                         defaultValue = "jsonld")
                       format: String) = no.met.security.AuthorizedAction {
    implicit request =>

      val start = DateTime.now(DateTimeZone.UTC) // start the clock
      val fieldList = FieldSpecification.parse(fields)
      Try  {
        // ensure that the query string contains supported fields only
        QueryStringUtil.ensureSubset(Set("sources", "fields"), request.queryString.keySet)

        idfAccess.idfSources(QueryParameters(sources, fields))

      } match {
        case Success(data) =>
          if (data isEmpty) {
            Error.error(NOT_FOUND,
              Some("No information found for any of the source ids"),
              Some("Ensure that information exists for at least one source id"), start)
          } else {
            format.toLowerCase() match {
              case "jsonld" =>
                Ok(new RainfallIDFSourcesJsonFormat().format(start, data)) as "application/vnd.no.met.data.frequencies.rainfall.availablesources-v0+json"
              case x        => Error.error(BAD_REQUEST, Some(s"Invalid output format: $x"), Some("Supported output formats: jsonld"), start)
            }
          }
        case Failure(x: BadRequestException) =>
          Error.error(BAD_REQUEST, Some(x getLocalizedMessage), x help, start)
        case Failure(x) => {
          //$COVERAGE-OFF$
          Logger.error(x.getLocalizedMessage)
          Error.error(INTERNAL_SERVER_ERROR, Some("An internal error occurred"), None, start)
          //$COVERAGE-ON$
        }
      }
  }
}
