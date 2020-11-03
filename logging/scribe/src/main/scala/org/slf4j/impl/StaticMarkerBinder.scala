package org.slf4j.impl

import org.slf4j.IMarkerFactory
import org.slf4j.spi.MarkerFactoryBinder
import org.slf4j.helpers.BasicMarkerFactory

class StaticMarkerBinder private() extends MarkerFactoryBinder {
  val getMarkerFactory: IMarkerFactory = new BasicMarkerFactory

  val getMarkerFactoryClassStr: String = classOf[BasicMarkerFactory].getName
}

object StaticMarkerBinder extends StaticMarkerBinder {
  def getSingleton: StaticMarkerBinder = this
}