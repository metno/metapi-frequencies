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
import org.junit.runner._
import org.specs2.mutable._
import org.specs2.runner._
import play.api.mvc
import play.api.test._
import play.api.test.Helpers._
import play.api.libs.json._
import TestUtil._
import scala.concurrent.Future

// scalastyle:off magic.number
/*
 * Note that these tests primarily exercise the routes and very basic controller
 * functionality; they are no guarantee that the queries against the database
 * will actually return correct data, as they are being run against mock data
 */
@RunWith(classOf[JUnitRunner])
class ControllersSpec extends Specification {

  "metapi /frequencies/rainfall" should {

    "test empty query string" in new WithApplication(TestUtil.app) {
      val response = route(FakeRequest(GET, "/rainfall/v0.jsonld")).get
      status(response) must equalTo(OK)
    }

    "test unsupported format" in new WithApplication(TestUtil.app) {
      val response = route(FakeRequest(GET, "/rainfall/v0.jsonldx")).get
      status(response) must equalTo(BAD_REQUEST)
    }

    "test malformed version/format" in new WithApplication(TestUtil.app) {
      val response = route(FakeRequest(GET, "/rainfall/availableSources/v(0~jsonldx")).get
      status(response) must equalTo(NOT_FOUND)
    }

    "test single source" in new WithApplication(TestUtil.app) {
      val response = route(FakeRequest(GET, "/rainfall/v0.jsonld?sources=SN18700")).get
      status(response) must equalTo(OK)
      val json = Json.parse(contentAsString(response))
      contentType(response) must beSome.which(_ == "application/vnd.no.met.data.frequencies.rainfall-v0+json")
      (json \ "data").as[JsArray].value.size must equalTo(1)
    }

    "test single source and single, unsupported field" in new WithApplication(TestUtil.app) {
      val response = route(FakeRequest(GET, "/rainfall/v0.jsonld?sources=SN18700&fields=dummy")).get
      status(response) must equalTo(OK)
      val json = Json.parse(contentAsString(response))
      contentType(response) must beSome.which(_ == "application/vnd.no.met.data.frequencies.rainfall-v0+json")
      (json \ "data").as[JsArray].value.size must equalTo(1)
    }

    "test two sources" in new WithApplication(TestUtil.app) {
      val response = route(FakeRequest(GET, "/rainfall/v0.jsonld?sources=SN18700,SN18701")).get
      status(response) must equalTo(OK)
      val json = Json.parse(contentAsString(response))
      (json \ "data").as[JsArray].value.size must equalTo(2)
    }

    "test single duration" in new WithApplication(TestUtil.app) {
      val response = route(FakeRequest(GET, "/rainfall/v0.jsonld?durations=20")).get
      status(response) must equalTo(OK)
    }

    "test malformed durations" in new WithApplication(TestUtil.app) {
      val response = route(FakeRequest(GET, "/rainfall/v0.jsonld?durations=20,foo,30")).get
      status(response) must equalTo(BAD_REQUEST)
    }

    "test unsupported durations" in new WithApplication(TestUtil.app) {
      val response = route(FakeRequest(GET, "/rainfall/v0.jsonld?durations=123456")).get
      status(response) must equalTo(BAD_REQUEST)
    }

    "test single frequency" in new WithApplication(TestUtil.app) {
      val response = route(FakeRequest(GET, "/rainfall/v0.jsonld?frequencies=20")).get
      status(response) must equalTo(OK)
    }

    "test malformed frequencies" in new WithApplication(TestUtil.app) {
      val response = route(FakeRequest(GET, "/rainfall/v0.jsonld?frequencies=20,foo,25")).get
      status(response) must equalTo(BAD_REQUEST)
    }

    "test unsupported frequencies" in new WithApplication(TestUtil.app) {
      val response = route(FakeRequest(GET, "/rainfall/v0.jsonld?frequencies=123456")).get
      status(response) must equalTo(BAD_REQUEST)
    }

    "test unsupported source" in new WithApplication(TestUtil.app) {
      val response = route(FakeRequest(GET, "/rainfall/v0.jsonld?sources=SN00000")).get
      status(response) must equalTo(NOT_FOUND)
    }

    "test malformed source" in new WithApplication(TestUtil.app) {
      val response = route(FakeRequest(GET, "/rainfall/v0.jsonld?sources=foo")).get
      status(response) must equalTo(BAD_REQUEST)
    }

    "test unsupported field value" in new WithApplication(TestUtil.app) {
      val response = route(FakeRequest(GET, "/rainfall/v0.jsonld?fields=foo")).get
      status(response) must equalTo(OK)
    }

    "test malformed field value" in new WithApplication(TestUtil.app) {
      val response = route(FakeRequest(GET, "/rainfall/v0.jsonld?fields=(~")).get
      status(response) must equalTo(OK)
    }

    "test unsupported field name" in new WithApplication(TestUtil.app) {
      val response = route(FakeRequest(GET, "/rainfall/v0.jsonld?foo=bar")).get
      status(response) must equalTo(BAD_REQUEST)
    }

    "test gridded dataset valid request" in new WithApplication(TestUtil.app) {
      val response = route(FakeRequest(GET, "/rainfall/v0.jsonld?sources=idf_bma1km_v1&location=POINT(10.75 59.95)")).get
      status(response) must equalTo(OK)
    }

    "test gridded dataset omit location parameter" in new WithApplication(TestUtil.app) {
      val response = route(FakeRequest(GET, "/rainfall/v0.jsonld?sources=idf_bma1km_v1")).get
      status(response) must equalTo(BAD_REQUEST)
    }
  }

  "metapi /frequencies/rainfall/availableSources" should {

    "test empty query string" in new WithApplication(TestUtil.app) {
      val response = route(FakeRequest(GET, "/rainfall/availableSources/v0.jsonld")).get
      status(response) must equalTo(OK)
      (Json.parse(contentAsString(response)) \ "data").as[JsArray].value.size must equalTo(4) // return all sources for both grid and station access
    }

    "test unsupported format" in new WithApplication(TestUtil.app) {
      val response = route(FakeRequest(GET, "/rainfall/availableSources/v0.jsonldx")).get
      status(response) must equalTo(BAD_REQUEST)
    }

    "test malformed version/format" in new WithApplication(TestUtil.app) {
      val response = route(FakeRequest(GET, "/rainfall/availableSources/v(0~jsonldx")).get
      status(response) must equalTo(NOT_FOUND)
    }

    "test single source" in new WithApplication(TestUtil.app) {
      val response = route(FakeRequest(GET, "/rainfall/availableSources/v0.jsonld?sources=SN18701")).get
      status(response) must equalTo(OK)
    }

    "test unsupported source" in new WithApplication(TestUtil.app) {
      val response = route(FakeRequest(GET, "/rainfall/availableSources/v0.jsonld?sources=SN00000")).get
      status(response) must equalTo(NOT_FOUND)
    }

    "test malformed source" in new WithApplication(TestUtil.app) {
      val response = route(FakeRequest(GET, "/rainfall/availableSources/v0.jsonld?sources=foo")).get
      status(response) must equalTo(BAD_REQUEST)
    }

    "test unsupported field value" in new WithApplication(TestUtil.app) {
      val response = route(FakeRequest(GET, "/rainfall/availableSources/v0.jsonld?fields=foo")).get
      status(response) must equalTo(OK)
    }

    "test malformed field value" in new WithApplication(TestUtil.app) {
      val response = route(FakeRequest(GET, "/rainfall/availableSources/v0.jsonld?fields=(~")).get
      status(response) must equalTo(OK)
    }

    "test unsupported field name" in new WithApplication(TestUtil.app) {
      val response = route(FakeRequest(GET, "/rainfall/availableSources/v0.jsonld?foo=bar")).get
      status(response) must equalTo(BAD_REQUEST)
    }

    "test gridded dataset supported name" in new WithApplication(TestUtil.app) {
      val response = route(FakeRequest(GET, "/rainfall/availableSources/v0.jsonld?sources=idf_bma1km_v1")).get
      status(response) must equalTo(OK)
    }

    "test gridded dataset valid location" in new WithApplication(TestUtil.app) {
      val response = route(FakeRequest(GET, "/rainfall/v0.jsonld?sources=idf_bma1km_v1&location=POINT(10 60)")).get
      status(response) must equalTo(OK)
    }

    "test gridded dataset invalid location 1" in new WithApplication(TestUtil.app) {
      val response = route(FakeRequest(GET, "/rainfall/v0.jsonld?sources=idf_bma1km_v1&location=POINT(10)")).get
      status(response) must equalTo(BAD_REQUEST)
    }

    "test gridded dataset invalid location 2" in new WithApplication(TestUtil.app) {
      val response = route(FakeRequest(GET, "/rainfall/v0.jsonld?sources=idf_bma1km_v1&location=foobar")).get
      status(response) must equalTo(BAD_REQUEST)
    }
  }
}

// scalastyle:on
