# Set up logging for this package.
# This creates a function to set up logging for different
# circumstances and calls for debug, warn, info, and error
# calls. It is set up in a way that should make it easier
# to switch which logging library we use, because there are
# many competing logging libraries.

#' This is the namespace in which all logging messages
#' are placed, so that they can be controlled as a group.
.baseLogger <- ifelse(is.null(utils::packageName()), "rampdata", utils::packageName())
#' Create a child namespace that also logs errors to a file.
.errorLogger <- paste0(.baseLogger, ".err")


.name_to_level <- list(
  "trace" = futile.logger::TRACE,
  "debug" = futile.logger::DEBUG,
  "info" = futile.logger::INFO,
  "warn" = futile.logger::WARN,
  "error" = futile.logger::ERROR,
  "fatal" = futile.logger::FATAL
)


string_log_level <- function(levelName) {
  if (tolower(levelName) %in% names(.name_to_level)) {
    .name_to_level[[tolower(levelName)]]
  } else {
    warning(paste(
      "Could not set logger because level", levelName, "isn't one of trace,",
      "debug, info, warn, error, or fatal."
    ))
    .name_to_level[["debug"]]
  }
}


#' Set up logging for use on a local machine.
#' @param level_name One of the strings (trace, debug, info, warn, error, fatal)
#'     Default value is "info".
#'
#' This creates a logger that is named after the package
#' so that all logging messages for this package can be turned on or off
#' together. This function makes everything go to the console on
#' standard out.
#' @export
local_logging <- function(level_name = "info") {
  invisible(futile.logger::flog.logger(
    .baseLogger,
    threshold=string_log_level(level_name),
    layout=futile.logger::layout.format('~l [~t] ~n:~f ~m')
  ))
}


#' Set up logging for running on the cluster.
#' @param level_name One of the strings (trace, debug, info, warn, error, fatal)
#'     Default value is "info".
#' @param error_file The name of the file. If NULL,
#'     the error file will be named with the current date and time.
#'
#' This sets logging so that warnings and above go to a file that is
#' named by the date and time, but all else goes to standard out.
#' It uses hierarchical loggers to control what goes to a file and what
#' goes to the console. Logging levels for warn, error, and fatal go
#' to a logging namespace called package-name.err, and that is directed to
#' a file.
#'
#' @export
cluster_logging <- function(level_name = "info", error_file = NULL) {
  if (!is.null(error_file)) {
    error_file <- format(Sys.time(), "%Y%m%d-%H%M%S.txt")
  }
  futile.logger::flog.logger(
    .baseLogger,
    threshold = string_log_level(level_name),
    layout = futile.logger::layout.format('~l [~t] ~n:~f ~m')
  )
  futile.logger::flog.logger(
    .errorLogger,
    threshold = string_log_level(level_name),
    appender = appender.file(error_file),
    layout = futile.logger::layout.format('~l [~t] ~n:~f ~m')
  )
}


set_logging_from_args <- function(package_name, function_name, vflag, qflag, localflag) {
  level_name <- names(.name_to_level)[3 + sum(c(-vflag, qflag))]
  if (localflag) {
    local_logging(level_name)
  } else {
    if (is.null(package_name)) {
      package_name <- unname(Sys.info()["user"])
    }
    cluster_logging(level_name, ramp_log_path(package_name, function_name))
  }
}


#' Set the logging level on some part of a package.
#'
#' @param module The string name of a section of the package.
#' @param level_name One of the strings (trace, debug, info, warn, error,
#'     fatal). The default is "debug".
#'
#' If you want to debug one module, then you can set its level differently
#' here from the rest of the package. If the package is named `macro`
#' and this function is called with `log_module("mosquito", "debug")`, then
#' this will set two loggers to debug level: `macro.mosquito` and
#' `macro.err.mosquito`, where the second one logs warnings and above to a file.
#' @export
log_module <- function(module, level_name = "debug") {
  level <- string_log_level(level_name)
  futile.logger::flog.threshold(level, name = paste0(.baseLogger, ".", module))
  futile.logger::flog.threshold(level, name = paste0(.errorLogger, ".", module))
}


#' Functions to emit logging messages.
#'
#' These functions accept a string and emit it through
#' logging mechanisms, either to the console or a file, as configured.
#'
#' The functions emit at different levels, from most-detailed to
#' least-detailed: trace, debug, info, warn, error, fatal. An error
#' can be non-fatal if the application has fallback behavior. A warning
#' will show up in the log file. Info messages are meant for the user
#' during normal operation. Debug messages are normally off but are left
#' in the code. Trace messages are for automated processing, such as
#' entry and exit of functions. These are often not left in code.
#' None of the logging functions are exported from the package because they
#' are not meant for users. They are for use inside the package because
#' they use the package's logging namespace to emit messages.
#'
#' @param message A vector of string arguments.
#' @param name An optional argument called which specifies that this
#'     logging message is part of the overall package. This is used to
#'     turn change logging level separately for a part of the package.
#' @return Nothing is returned.
#' @name localLoggingFunctions
NULL

#' @rdname localLoggingFunctions
logtrace <- function(...) {
  arglist <- list(...)
  if ("name" %in% names(arglist)) {
    arglist[names(arglist) == "name"] <- paste0(.baseLogger, ".", arglist$name)
    do.call(futile.logger::flog.trace, arglist)
  } else {
    futile.logger::flog.trace(..., name = .baseLogger)
  }
}


#' @rdname localLoggingFunctions
logdebug <- function(...) {
  arglist <- list(...)
  if ("name" %in% names(arglist)) {
    arglist[names(arglist) == "name"] <- paste0(.baseLogger, ".", arglist$name)
    do.call(futile.logger::flog.debug, arglist)
  } else {
    futile.logger::flog.debug(..., name = .baseLogger)
  }
}


#' @rdname localLoggingFunctions
loginfo <- function(...) {
  arglist <- list(...)
  if ("name" %in% names(arglist)) {
    arglist[names(arglist) == "name"] <- paste0(.baseLogger, ".", arglist$name)
    do.call(futile.logger::flog.info, arglist)
  } else {
    futile.logger::flog.info(..., name = .baseLogger)
  }
}


#' @rdname localLoggingFunctions
logwarn <- function(...) {
  arglist <- list(...)
  if ("name" %in% names(arglist)) {
    arglist[names(arglist) == "name"] <- paste0(.errorLogger, ".", arglist$name)
    do.call(futile.logger::flog.warn, arglist)
  } else {
    futile.logger::flog.warn(..., name = .errorLogger)
  }
}


#' @rdname localLoggingFunctions
logerror <- function(...) {
  arglist <- list(...)
  if ("name" %in% names(arglist)) {
    arglist[names(arglist) == "name"] <- paste0(.errorLogger, ".", arglist$name)
    do.call(futile.logger::flog.error, arglist)
  } else {
    futile.logger::flog.error(..., name = .errorLogger)
  }
}


#' @rdname localLoggingFunctions
logfatal <- function(...) {
  arglist <- list(...)
  if ("name" %in% names(arglist)) {
    arglist[names(arglist) == "name"] <- paste0(.errorLogger, ".", arglist$name)
    do.call(futile.logger::flog.fatal, arglist)
  } else {
    futile.logger::flog.fatal(..., name = .errorLogger)
  }
}
