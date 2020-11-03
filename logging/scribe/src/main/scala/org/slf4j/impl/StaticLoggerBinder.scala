package org.slf4j.impl

import java.util.concurrent.ConcurrentHashMap

import org.slf4j.{ILoggerFactory, Logger}
import org.slf4j.spi.LoggerFactoryBinder

import tofu.logging.scribe.internal.ScribeLoggerAdapter

class StaticLoggerBinder private() extends LoggerFactoryBinder {
  private[this] val Loggers = new ConcurrentHashMap[String, Logger]

  val getLoggerFactory: ILoggerFactory = name => Loggers.computeIfAbsent(
    if (name.equalsIgnoreCase(Logger.ROOT_LOGGER_NAME)) "" else name,
    new ScribeLoggerAdapter(_)
  )

  val getLoggerFactoryClassStr: String = "TofuScribeLoggerFactory"
}

object StaticLoggerBinder extends StaticLoggerBinder {
  def getSingleton: StaticLoggerBinder = this
}