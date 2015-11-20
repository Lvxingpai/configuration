package com.lvxingpai.configuration

import java.util.concurrent.TimeUnit

import com.typesafe.config._

import scala.collection.JavaConverters._
import scala.util.control.NonFatal

/**
 * This object provides a set of operations to create `Configuration` values.
 *
 * For example, to load a `Configuration` in a running application:
 * {{{
 * val config = Configuration.load()
 * val foo = config.getString("foo").getOrElse("boo")
 * }}}
 *
 * The underlying implementation is provided by https://github.com/typesafehub/config.
 */
object Configuration {
  private[Configuration] def asScalaList[A](l: java.util.List[A]): Seq[A] = asScalaBufferConverter(l).asScala.toList

  private[Configuration] def configError(origin: ConfigOrigin, message: String, e: Option[Throwable] = None): ConfigurationException = {
    ConfigurationException(message, e, Some(origin))
  }

  /**
   * Returns an empty Configuration object.
   */
  def empty = Configuration(ConfigFactory.empty())

  /**
   * Returns a default Configuration object.
   */
  def load() = Configuration(ConfigFactory.defaultApplication())

  /**
   * Create a new Configuration from the data passed as a Map.
   */
  def from(data: Map[String, Any]): Configuration = {

    def toJava(data: Any): Any = data match {
      case map: Map[_, _] => map.mapValues(toJava).asJava
      case iterable: Iterable[_] => iterable.map(toJava).asJava
      case v => v
    }

    Configuration(ConfigFactory.parseMap(toJava(data).asInstanceOf[java.util.Map[String, AnyRef]]))
  }

  /**
   * Create a new Configuration from the given key-value pairs.
   */
  def apply(data: (String, Any)*): Configuration = from(data.toMap)
}

/**
 * A full configuration set.
 *
 * The underlying implementation is provided by https://github.com/typesafehub/config.
 *
 * @param underlying the underlying Config implementation
 */
case class Configuration(underlying: Config) {

  import Configuration.asScalaList

  /**
   * Creates a configuration error for a specific configuration key.
   *
   * For example:
   * {{{
   * val configuration = Configuration.load()
   * throw configuration.reportError("engine.connectionUrl", "Cannot connect!")
   * }}}
   *
   * @param path the configuration key, related to this error
   * @param message the error message
   * @param e the related exception
   * @return a configuration exception
   */
  def reportError(path: String, message: String, e: Option[Throwable] = None): ConfigurationException = {
    Configuration.configError(if (underlying.hasPath(path)) underlying.getValue(path).origin else underlying.root.origin, message, e)
  }

  /**
   * Merge 2 configurations.
   */
  def ++(other: Configuration): Configuration = {
    Configuration(other.underlying.withFallback(underlying))
  }

  /**
   * Reads a value from the underlying implementation.
   * If the value is not set this will return None, otherwise returns Some.
   *
   * Does not check neither for incorrect type nor null value, but catches and wraps the error.
   */
  private def readValue[T](path: String, v: => T): Option[T] = {
    try {
      if (underlying.hasPathOrNull(path)) Some(v) else None
    } catch {
      case NonFatal(e) => throw reportError(path, e.getMessage, Some(e))
    }

  }

  /**
   * Retrieves a configuration value as a `String`.
   *
   * This method supports an optional set of valid values:
   * {{{
   * val config = Configuration.load()
   * val mode = config.getString("engine.mode", Some(Set("dev","prod")))
   * }}}
   *
   * A configuration error will be thrown if the configuration value does not match any of the required values.
   *
   * @param path the configuration key, relative to configuration root key
   * @param validValues valid values for this configuration
   * @return a configuration value
   */
  def getString(path: String, validValues: Option[Set[String]] = None): Option[String] =
    readValue(path, underlying.getString(path)).map { value =>
      validValues match {
        case Some(values) if values.contains(value) => value
        case Some(values) if values.isEmpty => value
        case Some(values) => throw reportError(path, "Incorrect value, one of " + (values.reduceLeft(_ + ", " + _)) + " was expected.")
        case None => value
      }
    }

  /**
   * Retrieves a configuration value as an `Int`.
   *
   * For example:
   * {{{
   * val configuration = Configuration.load()
   * val poolSize = configuration.getInt("engine.pool.size")
   * }}}
   *
   * A configuration error will be thrown if the configuration value is not a valid `Int`.
   *
   * @param path the configuration key, relative to the configuration root key
   * @return a configuration value
   */
  def getInt(path: String): Option[Int] = readValue(path, underlying.getInt(path))

  /**
   * Retrieves a configuration value as a `Boolean`.
   *
   * For example:
   * {{{
   * val configuration = Configuration.load()
   * val isEnabled = configuration.getBoolean("engine.isEnabled")
   * }}}
   *
   * A configuration error will be thrown if the configuration value is not a valid `Boolean`.
   * Authorized vales are yes/no or true/false.
   *
   * @param path the configuration key, relative to the configuration root key
   * @return a configuration value
   */
  def getBoolean(path: String): Option[Boolean] = readValue(path, underlying.getBoolean(path))

  /**
   * Retrieves a configuration value as `Milliseconds`.
   *
   * For example:
   * {{{
   * val configuration = Configuration.load()
   * val timeout = configuration.getMilliseconds("engine.timeout")
   * }}}
   *
   * The configuration must be provided as:
   *
   * {{{
   * engine.timeout = 1 second
   * }}}
   */
  def getMilliseconds(path: String): Option[Long] = readValue(path, underlying.getDuration(path, TimeUnit.MILLISECONDS))

  /**
   * Retrieves a configuration value as `Nanoseconds`.
   *
   * For example:
   * {{{
   * val configuration = Configuration.load()
   * val timeout = configuration.getNanoseconds("engine.timeout")
   * }}}
   *
   * The configuration must be provided as:
   *
   * {{{
   * engine.timeout = 1 second
   * }}}
   */
  def getNanoseconds(path: String): Option[Long] = readValue(path, underlying.getDuration(path, TimeUnit.NANOSECONDS))

  /**
   * Retrieves a configuration value as `Bytes`.
   *
   * For example:
   * {{{
   * val configuration = Configuration.load()
   * val maxSize = configuration.getString("engine.maxSize")
   * }}}
   *
   * The configuration must be provided as:
   *
   * {{{
   * engine.maxSize = 512k
   * }}}
   */
  def getBytes(path: String): Option[Long] = readValue(path, underlying.getBytes(path))

  /**
   * Retrieves a sub-configuration, i.e. a configuration instance containing all keys starting with a given prefix.
   *
   * For example:
   * {{{
   * val configuration = Configuration.load()
   * val engineConfig = configuration.getConfig("engine")
   * }}}
   *
   * The root key of this new configuration will be ‘engine’, and you can access any sub-keys relatively.
   *
   * @param path the root prefix for this sub-configuration
   * @return a new configuration
   */
  def getConfig(path: String): Option[Configuration] = readValue(path, underlying.getConfig(path)).map(Configuration(_))

  /**
   * Retrieves a configuration value as a `Double`.
   *
   * For example:
   * {{{
   * val configuration = Configuration.load()
   * val population = configuration.getDouble("world.population")
   * }}}
   *
   * A configuration error will be thrown if the configuration value is not a valid `Double`.
   *
   * @param path the configuration key, relative to the configuration root key
   * @return a configuration value
   */
  def getDouble(path: String): Option[Double] = readValue(path, underlying.getDouble(path))

  /**
   * Retrieves a configuration value as a `Long`.
   *
   * For example:
   * {{{
   * val configuration = Configuration.load()
   * val duration = configuration.getLong("timeout.duration")
   * }}}
   *
   * A configuration error will be thrown if the configuration value is not a valid `Long`.
   *
   * @param path the configuration key, relative to the configuration root key
   * @return a configuration value
   */
  def getLong(path: String): Option[Long] = readValue(path, underlying.getLong(path))

  /**
   * Retrieves a configuration value as a `Number`.
   *
   * For example:
   * {{{
   * val configuration = Configuration.load()
   * val counter = configuration.getNumber("foo.counter")
   * }}}
   *
   * A configuration error will be thrown if the configuration value is not a valid `Number`.
   *
   * @param path the configuration key, relative to the configuration root key
   * @return a configuration value
   */
  def getNumber(path: String): Option[Number] = readValue(path, underlying.getNumber(path))

  /**
   * Retrieves a configuration value as a List of `Boolean`.
   *
   * For example:
   * {{{
   * val configuration = Configuration.load()
   * val switches = configuration.getBooleanList("board.switches")
   * }}}
   *
   * The configuration must be provided as:
   *
   * {{{
   * board.switches = [true, true, false]
   * }}}
   *
   * A configuration error will be thrown if the configuration value is not a valid `Boolean`.
   * Authorized vales are yes/no or true/false.
   */
  def getBooleanList(path: String): Option[java.util.List[java.lang.Boolean]] = readValue(path, underlying.getBooleanList(path))

  /**
   * Retrieves a configuration value as a Seq of `Boolean`.
   *
   * For example:
   * {{{
   * val configuration = Configuration.load()
   * val switches = configuration.getBooleanSeq("board.switches")
   * }}}
   *
   * The configuration must be provided as:
   *
   * {{{
   * board.switches = [true, true, false]
   * }}}
   *
   * A configuration error will be thrown if the configuration value is not a valid `Boolean`.
   * Authorized vales are yes/no or true/false.
   */
  def getBooleanSeq(path: String): Option[Seq[java.lang.Boolean]] = getBooleanList(path).map(asScalaList)

  /**
   * Retrieves a configuration value as a List of `Bytes`.
   *
   * For example:
   * {{{
   * val configuration = Configuration.load()
   * val maxSizes = configuration.getBytesList("engine.maxSizes")
   * }}}
   *
   * The configuration must be provided as:
   *
   * {{{
   * engine.maxSizes = [512k, 256k, 256k]
   * }}}
   */
  def getBytesList(path: String): Option[java.util.List[java.lang.Long]] = readValue(path, underlying.getBytesList(path))

  /**
   * Retrieves a configuration value as a Seq of `Bytes`.
   *
   * For example:
   * {{{
   * val configuration = Configuration.load()
   * val maxSizes = configuration.getBytesSeq("engine.maxSizes")
   * }}}
   *
   * The configuration must be provided as:
   *
   * {{{
   * engine.maxSizes = [512k, 256k, 256k]
   * }}}
   */
  def getBytesSeq(path: String): Option[Seq[java.lang.Long]] = getBytesList(path).map(asScalaList)

  /**
   * Retrieves a List of sub-configurations, i.e. a configuration instance for each key that matches the path.
   *
   * For example:
   * {{{
   * val configuration = Configuration.load()
   * val engineConfigs = configuration.getConfigList("engine")
   * }}}
   *
   * The root key of this new configuration will be "engine", and you can access any sub-keys relatively.
   */
  def getConfigList(path: String): Option[java.util.List[Configuration]] = readValue[java.util.List[_ <: Config]](path, underlying.getConfigList(path)).map { configs => configs.asScala.map(Configuration(_)).asJava }

  /**
   * Retrieves a Seq of sub-configurations, i.e. a configuration instance for each key that matches the path.
   *
   * For example:
   * {{{
   * val configuration = Configuration.load()
   * val engineConfigs = configuration.getConfigSeq("engine")
   * }}}
   *
   * The root key of this new configuration will be "engine", and you can access any sub-keys relatively.
   */
  def getConfigSeq(path: String): Option[Seq[Configuration]] = getConfigList(path).map(asScalaList)

  /**
   * Retrieves a configuration value as a List of `Double`.
   *
   * For example:
   * {{{
   * val configuration = Configuration.load()
   * val maxSizes = configuration.getDoubleList("engine.maxSizes")
   * }}}
   *
   * The configuration must be provided as:
   *
   * {{{
   * engine.maxSizes = [5.0, 3.34, 2.6]
   * }}}
   */
  def getDoubleList(path: String): Option[java.util.List[java.lang.Double]] = readValue(path, underlying.getDoubleList(path))

  /**
   * Retrieves a configuration value as a Seq of `Double`.
   *
   * For example:
   * {{{
   * val configuration = Configuration.load()
   * val maxSizes = configuration.getDoubleSeq("engine.maxSizes")
   * }}}
   *
   * The configuration must be provided as:
   *
   * {{{
   * engine.maxSizes = [5.0, 3.34, 2.6]
   * }}}
   */
  def getDoubleSeq(path: String): Option[Seq[java.lang.Double]] = getDoubleList(path).map(asScalaList)

  /**
   * Retrieves a configuration value as a List of `Integer`.
   *
   * For example:
   * {{{
   * val configuration = Configuration.load()
   * val maxSizes = configuration.getIntList("engine.maxSizes")
   * }}}
   *
   * The configuration must be provided as:
   *
   * {{{
   * engine.maxSizes = [100, 500, 2]
   * }}}
   */
  def getIntList(path: String): Option[java.util.List[java.lang.Integer]] = readValue(path, underlying.getIntList(path))

  /**
   * Retrieves a configuration value as a Seq of `Integer`.
   *
   * For example:
   * {{{
   * val configuration = Configuration.load()
   * val maxSizes = configuration.getIntSeq("engine.maxSizes")
   * }}}
   *
   * The configuration must be provided as:
   *
   * {{{
   * engine.maxSizes = [100, 500, 2]
   * }}}
   */
  def getIntSeq(path: String): Option[Seq[java.lang.Integer]] = getIntList(path).map(asScalaList)

  /**
   * Gets a list value (with any element type) as a ConfigList, which implements java.util.List<ConfigValue>.
   *
   * For example:
   * {{{
   * val configuration = Configuration.load()
   * val maxSizes = configuration.getList("engine.maxSizes")
   * }}}
   *
   * The configuration must be provided as:
   *
   * {{{
   * engine.maxSizes = ["foo", "bar"]
   * }}}
   */
  def getList(path: String): Option[ConfigList] = readValue(path, underlying.getList(path))

  /**
   * Retrieves a configuration value as a List of `Long`.
   *
   * For example:
   * {{{
   * val configuration = Configuration.load()
   * val maxSizes = configuration.getLongList("engine.maxSizes")
   * }}}
   *
   * The configuration must be provided as:
   *
   * {{{
   * engine.maxSizes = [10000000000000, 500, 2000]
   * }}}
   */
  def getLongList(path: String): Option[java.util.List[java.lang.Long]] = readValue(path, underlying.getLongList(path))

  /**
   * Retrieves a configuration value as a Seq of `Long`.
   *
   * For example:
   * {{{
   * val configuration = Configuration.load()
   * val maxSizes = configuration.getLongSeq("engine.maxSizes")
   * }}}
   *
   * The configuration must be provided as:
   *
   * {{{
   * engine.maxSizes = [10000000000000, 500, 2000]
   * }}}
   */
  def getLongSeq(path: String): Option[Seq[java.lang.Long]] = getLongList(path).map(asScalaList)

  /**
   * Retrieves a configuration value as List of `Milliseconds`.
   *
   * For example:
   * {{{
   * val configuration = Configuration.load()
   * val timeouts = configuration.getMillisecondsList("engine.timeouts")
   * }}}
   *
   * The configuration must be provided as:
   *
   * {{{
   * engine.timeouts = [1 second, 1 second]
   * }}}
   */
  def getMillisecondsList(path: String): Option[java.util.List[java.lang.Long]] = readValue(path, underlying.getDurationList(path, TimeUnit.MILLISECONDS))

  /**
   * Retrieves a configuration value as Seq of `Milliseconds`.
   *
   * For example:
   * {{{
   * val configuration = Configuration.load()
   * val timeouts = configuration.getMillisecondsSeq("engine.timeouts")
   * }}}
   *
   * The configuration must be provided as:
   *
   * {{{
   * engine.timeouts = [1 second, 1 second]
   * }}}
   */
  def getMillisecondsSeq(path: String): Option[Seq[java.lang.Long]] = getMillisecondsList(path).map(asScalaList)

  /**
   * Retrieves a configuration value as List of `Nanoseconds`.
   *
   * For example:
   * {{{
   * val configuration = Configuration.load()
   * val timeouts = configuration.getNanosecondsList("engine.timeouts")
   * }}}
   *
   * The configuration must be provided as:
   *
   * {{{
   * engine.timeouts = [1 second, 1 second]
   * }}}
   */
  def getNanosecondsList(path: String): Option[java.util.List[java.lang.Long]] = readValue(path, underlying.getDurationList(path, TimeUnit.NANOSECONDS))

  /**
   * Retrieves a configuration value as Seq of `Nanoseconds`.
   *
   * For example:
   * {{{
   * val configuration = Configuration.load()
   * val timeouts = configuration.getNanosecondsSeq("engine.timeouts")
   * }}}
   *
   * The configuration must be provided as:
   *
   * {{{
   * engine.timeouts = [1 second, 1 second]
   * }}}
   */
  def getNanosecondsSeq(path: String): Option[Seq[java.lang.Long]] = getNanosecondsList(path).map(asScalaList)

  /**
   * Retrieves a configuration value as a List of `Number`.
   *
   * For example:
   * {{{
   * val configuration = Configuration.load()
   * val maxSizes = configuration.getNumberList("engine.maxSizes")
   * }}}
   *
   * The configuration must be provided as:
   *
   * {{{
   * engine.maxSizes = [50, 500, 5000]
   * }}}
   */
  def getNumberList(path: String): Option[java.util.List[java.lang.Number]] = readValue(path, underlying.getNumberList(path))

  /**
   * Retrieves a configuration value as a Seq of `Number`.
   *
   * For example:
   * {{{
   * val configuration = Configuration.load()
   * val maxSizes = configuration.getNumberSeq("engine.maxSizes")
   * }}}
   *
   * The configuration must be provided as:
   *
   * {{{
   * engine.maxSizes = [50, 500, 5000]
   * }}}
   */
  def getNumberSeq(path: String): Option[Seq[java.lang.Number]] = getNumberList(path).map(asScalaList)

  /**
   * Retrieves a configuration value as a List of `ConfigObject`.
   *
   * For example:
   * {{{
   * val configuration = Configuration.load()
   * val engineProperties = configuration.getObjectList("engine.properties")
   * }}}
   *
   * The configuration must be provided as:
   *
   * {{{
   * engine.properties = [{id: 5, power: 3}, {id: 6, power: 20}]
   * }}}
   */
  def getObjectList(path: String): Option[java.util.List[_ <: ConfigObject]] = readValue[java.util.List[_ <: ConfigObject]](path, underlying.getObjectList(path))

  /**
   * Retrieves a configuration value as a List of `String`.
   *
   * For example:
   * {{{
   * val configuration = Configuration.load()
   * val names = configuration.getStringList("names")
   * }}}
   *
   * The configuration must be provided as:
   *
   * {{{
   * names = ["Jim", "Bob", "Steve"]
   * }}}
   */
  def getStringList(path: String): Option[java.util.List[java.lang.String]] = readValue(path, underlying.getStringList(path))

  /**
   * Retrieves a configuration value as a Seq of `String`.
   *
   * For example:
   * {{{
   * val configuration = Configuration.load()
   * val names = configuration.getStringSeq("names")
   * }}}
   *
   * The configuration must be provided as:
   *
   * {{{
   * names = ["Jim", "Bob", "Steve"]
   * }}}
   */
  def getStringSeq(path: String): Option[Seq[java.lang.String]] = getStringList(path).map(asScalaList)

  /**
   * Retrieves a ConfigObject for this path, which implements Map<String,ConfigValue>
   *
   * For example:
   * {{{
   * val configuration = Configuration.load()
   * val engineProperties = configuration.getObject("engine.properties")
   * }}}
   *
   * The configuration must be provided as:
   *
   * {{{
   * engine.properties = {id: 1, power: 5}
   * }}}
   */
  def getObject(path: String): Option[ConfigObject] = readValue(path, underlying.getObject(path))

  /**
   * Returns available keys.
   *
   * For example:
   * {{{
   * val configuration = Configuration.load()
   * val keys = configuration.keys
   * }}}
   *
   * @return the set of keys available in this configuration
   */
  def keys: Set[String] = underlying.entrySet.asScala.map(_.getKey).toSet

  /**
   * Returns sub-keys.
   *
   * For example:
   * {{{
   * val configuration = Configuration.load()
   * val subKeys = configuration.subKeys
   * }}}
   * @return the set of direct sub-keys available in this configuration
   */
  def subKeys: Set[String] = underlying.root().keySet().asScala.toSet

  /**
   * Returns every path as a set of key to value pairs, by recursively iterating through the
   * config objects.
   */
  def entrySet: Set[(String, ConfigValue)] = underlying.entrySet().asScala.map(e => e.getKey -> e.getValue).toSet
}
