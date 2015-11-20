package com.lvxingpai.configuration

import com.typesafe.config.ConfigOrigin

/**
 * Created by zephyre on 11/19/15.
 */
class ConfigurationException(val description: String, val cause: Option[Throwable], val origin: Option[ConfigOrigin])
    extends RuntimeException(description, cause.orNull) {

  def this(description: String, cause: Option[Throwable]) {
    this(description, cause, None)
  }

  def this(description: String) {
    this(description, None, None)
  }
}

object ConfigurationException {
  def apply(description: String): ConfigurationException = {
    new ConfigurationException(description)
  }

  def apply(description: String, cause: Option[Throwable]): ConfigurationException = {
    new ConfigurationException(description, cause)
  }

  def apply(description: String, cause: Option[Throwable], origin: Option[ConfigOrigin]): ConfigurationException = {
    new ConfigurationException(description, cause, origin)
  }
}
