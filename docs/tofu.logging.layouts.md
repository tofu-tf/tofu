---
id: tofu.logging.layouts
title: Logback Layouts
---


# Layouts

Tofu is built upon [Logback](http://logback.qos.ch/) so it needs a custom `logback.xml` file with contexual logging
support. Tofu uses mechanism called markers to store context in logs, so it won't work with existing Layouts e.g.
with [Logstash-encoder](https://github.com/logstash/logstash-logback-encoder).

Luckily for us, tofu has two special Layouts:

* [ELKLayout](https://github.com/tofu-tf/tofu/blob/master/logging/layout/src/main/scala/tofu/logging/ELKLayout.scala)
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

* [ConsoleContextLayout](https://github.com/tofu-tf/tofu/blob/master/logging/layout/src/main/scala/tofu/logging/ConsoleContextLayout.scala)
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