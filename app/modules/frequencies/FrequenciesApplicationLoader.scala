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
package modules.frequencies

import play.api._
import play.api.inject.guice._
import play.api.inject.guice.GuiceableModule.fromGuiceModule
import play.api.inject.guice._
import com.google.inject.AbstractModule
import services.frequencies._

/**
 * Configurations for Production Mode
 */
// $COVERAGE-OFF$ Can't test the production binding in Test mode
class FrequenciesProdModule extends AbstractModule {

  def configure() {
    bind(classOf[IDFAccess]).to(classOf[ProdIDFAccess])
  }

}
// $COVERAGE-ON$

/**
 * Configurations for Development Mode
 */
class FrequenciesDevModule extends AbstractModule {

  def configure() {
    bind(classOf[IDFAccess]).to(classOf[MockIDFAccess])
  }

}

/**
 * Set up the Guice injector and provide the mechanism to return objects from the dependency graph.
 */
// $COVERAGE-OFF$ Can't test the production binding in Test mode
class FrequenciesApplicationLoader extends GuiceApplicationLoader() {

  override def builder(context: ApplicationLoader.Context): GuiceApplicationBuilder = {
    val builder = initialBuilder
      .in(context.environment)
      .loadConfig(context.initialConfiguration)
      .overrides(overrides(context): _*)

    context.environment.mode match {
      case Mode.Prod =>
        // start mode
        builder.bindings(new FrequenciesProdModule)
      case _ =>
        // run mode
        builder.bindings(new FrequenciesDevModule)
    }
  }

}
// $COVERAGE-ON$
