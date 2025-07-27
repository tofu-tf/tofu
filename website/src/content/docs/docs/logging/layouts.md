---
title: Logback Layouts
sidebar:
  order: 6
---


# Layouts

Tofu is built upon [Logback](http://logback.qos.ch/) so it needs a custom `logback.xml` file with contextual logging
support. Tofu uses mechanism called markers to store context in logs, so it won't work with existing Layouts.

Luckily for us, tofu brings a logging provider for _Logstash-encoder_:

```
"tf.tofu" %% "tofu-logging-logstash-logback" % "@VERSION"
```

* **TofuLoggingProvider** provides JSON logs for logstash-logback-encoder. See [README on github](https://github.com/logstash/logstash-logback-encoder) for more details. 

```xml
<appender name="STDOUT" class="ch.qos.logback.core.ConsoleAppender">
      <encoder class="net.logstash.logback.encoder.LoggingEventCompositeJsonEncoder">
          <providers>
              <pattern>
                  <pattern>
                      { "a": 1 }
                  </pattern>
              </pattern>
              <timestamp/>
              <logLevel/>
              <message/>
              <provider class="tofu.logging.TofuLoggingProvider"/>
          </providers>
      </encoder>
  </appender>
```

In addition, tofu has two own special Layouts:

```
"tf.tofu" %% "tofu-logging-layout" % "@VERSION"
```

* **ELKLayout**
  that outputs structured logs in JSON format. Example appender looks like that:

```xml

<appender name="STDOUT" class="ch.qos.logback.core.ConsoleAppender">
    <encoder class="ch.qos.logback.core.encoder.LayoutWrappingEncoder">
        <layout class="tofu.logging.ELKLayout">
          <!-- Optional tag for static fields which will be merged into structured json output. -->
          <customFields>{
            "a": 1,
            "b": {
                "c": true,
                "d": "great"
            }
          }</customFields>
        </layout>
    </encoder>
</appender>
  ```

* **ConsoleContextLayout**
  that outputs simple text logs. Example appender looks like that:

```xml

<appender name="STDOUT" class="ch.qos.logback.core.ConsoleAppender">
    <encoder class="ch.qos.logback.core.encoder.LayoutWrappingEncoder">
        <layout class="tofu.logging.logback.ConsoleContextLayout">
            <pattern>%d{HH:mm:ss} %-5level %logger{36} - %msg%n [%mdc]%n</pattern>
        </layout>
    </encoder>
</appender>
```
