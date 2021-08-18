import tofu.logging.logback.ConsoleContextLayout
import tofu.logging.ELKLayout

appender("PLAIN-COLORED", ConsoleAppender) {
    encoder(LayoutWrappingEncoder) {
        layout(ConsoleContextLayout) {
            pattern = "%cyan(%d{HH:mm:ss} %-5level %logger{36} - %msg%n [%mdc]%n)"
        }
    }
}

appender("STRUCTURED", ConsoleAppender) {
    encoder(LayoutWrappingEncoder) {
        layout(ELKLayout)
    }
}

root(DEBUG, ["PLAIN-COLORED", "STRUCTURED"])