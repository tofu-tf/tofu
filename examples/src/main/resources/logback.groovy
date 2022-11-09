import tofu.logging.logback.ConsoleContextLayout
import tofu.logging.ELKLayout

//puts messages as plain text
appender("PLAIN-COLORED", ConsoleAppender) {
    encoder(LayoutWrappingEncoder) {
        layout(ConsoleContextLayout) {
            pattern = "%cyan(%d{HH:mm:ss} %-5level %logger{36} - %msg%n [%mdc]%n)"
        }
    }
}
//puts messages as JSONs
appender("STRUCTURED", ConsoleAppender) {
    encoder(LayoutWrappingEncoder) {
        layout(ELKLayout)
    }
}

root(DEBUG, ["STRUCTURED"])
