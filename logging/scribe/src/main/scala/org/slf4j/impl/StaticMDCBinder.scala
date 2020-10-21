package org.slf4j.impl

import java.util

import org.slf4j.spi.MDCAdapter

import scribe.MDC

class StaticMDCBinder private() {
  val getMDCA: MDCAdapter = new MDCAdapter {
    def put(key: String, value: String): Unit = MDC(key) = value

    def get(key: String): String = MDC.get(key).orNull

    def remove(key: String): Unit = MDC.remove(key)

    def clear(): Unit = MDC.clear()

    def getCopyOfContextMap: util.Map[String, String] = {
      import scala.jdk.CollectionConverters._

      MDC.map.map { case (k, f) => k -> f() }.asJava
    }

    def setContextMap(map: util.Map[String, String]): Unit = {
      clear()
      map.forEach(put(_, _))
    }
  }

  val getMDCAdapterClassStr: String = "TofuScribeMDCAdapter"
}

object StaticMDCBinder extends StaticMDCBinder {
  def getSingleton: StaticMDCBinder = this
}