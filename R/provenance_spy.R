# Not exported. Tracks input files used for those functions we track
# Global state for our package, but not globally available
.input.files <- list()
.output.files <- list()


.append.input.file <- function(metadata) {
  artifact_hash <- openssl::sha256(paste(unlist(metadata), collapse = ""))
  # we need to re-assign this due to how the package value (which is not a global value) is managed by R
  package_name <- utils::packageName()
  if (!is.null(package_name)) {
    .input.files[[artifact_hash]] <- metadata
    utils::assignInNamespace(".input.files", .input.files, ns = package_name)
  } else {
    .input.files[[artifact_hash]] <<- metadata
  }
}


.append.output.file <- function(metadata) {
  artifact_hash <- openssl::sha256(paste(unlist(metadata), collapse = ""))
  # we need to re-assign this due to how the package value (which is not a global value) is managed by R
  package_name <- utils::packageName()
  if (!is.null(package_name)) {
    .output.files[[artifact_hash]] <- metadata
    utils::assignInNamespace(".output.files", .output.files, ns = package_name)
  } else {
    .output.files[[artifact_hash]] <<- metadata
  }
}


#' Get the list of files that were recorded as inputs.
#' @param clear True in order to clear the list of input files.
#' @return A list with one entry for each file.
#' @export
get.input.files <- function(clear = FALSE) {
  result <- .input.files
  if (clear) {
    utils::assignInNamespace(".input.files", list(), ns = utils::packageName())
  }
  return(result)
}


#' Get the list of files that were recorded as outputs
#' @param clear True in order to clear the list of output files.
#' @return A list with one entry for each file.
#' @export
get.output.files <- function(clear = FALSE) {
  result <- .output.files
  if (clear) {
    utils::assignInNamespace(".output.files", list(), ns = utils::packageName())
  }
  return(result)
}


#' Clear provenance data for a new run.
#' @export
clear.provenance <- function() {
  get.input.files(clear = TRUE)
  get.output.files(clear = TRUE)
}


sys_calls_to_string <- function(calls) {
  each_call <- lapply(calls, function(c) {
    string_vector <- as.character(c)
    if (length(string_vector) > 1) {
      arguments <- paste(string_vector[2:length(string_vector)], collapse = ",")
    } else {
      arguments <- ""
    }
    paste(string_vector[1], "(", arguments, ")", sep = "", collapse = "")
  })
  unlist(each_call)
}


#' Record that this code used this input file.
#' @param input The filename of the input file.
#' @param role A string to identify what this file does for this program.
#' @export
prov.input.file <- function(input, role) {
  md <- get.metadata(input)
  md$role <- paste(as.character(role), collapse = " ")
  calls <- sys.calls()
  md$stack <- sys_calls_to_string(calls[1:(length(calls) - 1)])
  if (file.exists(input)) {
    md$sha256 <- as.character(openssl::sha256(file(input)))
  }
  .append.input.file(md)
}


#' Record that this code used this output file.
#' @param output The filename of the output file.
#' @param role A string to identify what this file does for this program.
#' @export
prov.output.file <- function(output, role) {
  md <- get.metadata(output)
  md$role <- paste(as.character(role), collapse = " ")
  calls <- sys.calls()
  md$stack <- sys_calls_to_string(calls[1:(length(calls) - 1)])
  if (file.exists(output)) {
    md$sha256 <- as.character(openssl::sha256(file(output)))
  }
  .append.output.file(md)
}


single_toml_item <- function(value) {
  length(value) == 1 | "POSIXlt" %in% class(value)
}


toml_quoting <- function(value) {
  if (is.numeric(value)) {
    value
  } else if (is.list(value) & "POSIXlt" %in% class(value)) {
    strftime(value, format = "%Y-%m-%dT%H:%M:%S")
  } else {
    if (grepl("\n", value, fixed = TRUE)) {
      paste("'''", value, "'''", sep = "", collapse = "")
    } else {
      paste("'", value, "'", sep = "", collapse = "")
    }
  }
}


write_provenance_section <- function(section_name, artifact_list, connection) {
  writeLines(paste("[", section_name, "]", sep = "", collapse = ""), con = connection)
  for (name in names(artifact_list)) {
    input <- artifact_list[[name]]
    writeLines(paste("  [", section_name, ".", name, "]", sep = ""), con = connection)
    for (key in names(input)) {
      value <- input[[key]]
      if (single_toml_item(value)) {
        writeLines(paste(" ", key, "=", toml_quoting(value), sep = " "), con = connection)
      } else {
        writeLines(paste("  ", key, " = [", sep = ""), con = connection)
        vlen <- length(value)
        for (item_idx in 1:(vlen - 1)) {
          item <- value[item_idx]
          writeLines(paste("    ", toml_quoting(item), ",", sep = ""), con = connection)
        }
        item <- value[vlen]
        writeLines(paste("    ", toml_quoting(item), sep = ""), con = connection)
        writeLines("  ]", con = connection)
      }
    }
    writeLines("", con = connection)
  }
}


#' Write the list of input files to a file on disk.
#' @param path The file path to which to write the information.
#' @export
write.meta.data <- function(path) {
  if (is.character(path)) {
    file_connection <- file(path, open = "wt")
  } else if (is.integer(path)) {
    file_connection <- path
  } else {
    message(paste("Cannot write metadata to", path))
    return
  }
  app_meta <- list(runtime = list(
    commandline_arguments = commandArgs(),
    rversion = getRversion(),
    user = unname(Sys.info()["user"]),
    node = unname(Sys.info()["nodename"])
  ))
  write_provenance_section("application", app_meta, file_connection)
  write_provenance_section("inputs", get.input.files(), file_connection)
  write_provenance_section("outputs", get.output.files(), file_connection)
  writeLines("", con = file_connection)
  flush(file_connection)
  if (is.character(path)) {
    close(file_connection)
  }
}

#' Returns metadata for a file
#' @param path The path to the file from which to get file info.
#' @return a list of file info containing creation time, last modified,
#'     and its path.
get.metadata <- function(path) {
  path <- normalizePath(path)

  info <- file.info(path)
  result <- list(
    creation_time = .format.metadata.date(info$ctime),
    last_modified = .format.metadata.date(info$mtime),
    # TODO:
    # owner? info$uname exists but is NA because we're in a container
    # md5?
    path = path
  )

  # since we know there's only 1 input, unlist result. then get last element
  ext <- utils::tail(
    unlist(strsplit(path, ".", fixed = TRUE)),
    1)

  if (ext == "shp") {
    result$extra <- .get.shp.metadata(path)
  }

  return(result)
}


.format.metadata.date <- function(datelike_double) {
  as.POSIXlt(datelike_double)
}


#' Return extra metadata associated with .shp file
#'
#' @param path The path to the shapefile.
#' @return A list of properties of that data, taken from the file.
#'
#' A "shapefile" is actually a collection of files. At minimum .shp, .shpx, and .dbf
#'
#' In addition, there are at least 13 optional files that may be included.
#'
#' https://en.wikipedia.org/wiki/Shapefile#Overview
.get.shp.metadata <- function(path) {
  # NOTE: we're being lazy and just grabbing everything that has the same prefix.

  # split on "."; unlist the result into a vector; take all but the last element, re-join with "."
  base <- paste(utils::head(unlist(strsplit(path, ".", fixed = TRUE)), n = -1), collapse = ".")
  # list files in directory which start with the filename (without extension)
  related <- list.files(dirname(base), full.names = TRUE, pattern = basename(base))

  result <- list()
  for (rel in related) {
    info <- file.info(rel)
    result[[basename(rel)]] <- .format.metadata.date(info$mtime)
  }
  return(result)
}


# methods we hijack
# NOTE: the fullest solution would be to actually take over the namespaces
# using assignInNamespace. However this is a more complicated solution and may
# introduce other issues, and probably is irrelevant as I don't think anyone
# *ever* uses namespaced calls in the covariate code except where Mike adds them.
#
# Docs for hooks including order of operations, .onLoad, "onLoad" hooks, and "attach" hook
# https://stat.ethz.ch/R-manual/R-patched/library/base/html/userhooks.html
#
# attach vs onLoad events
# https://stackoverflow.com/a/56538266
#
# assigning to other package namespaces
# https://stackoverflow.com/a/58238931
.spy.on.methods <- function() {
  attached.packages <- .packages()

  pkg.watch <- list(
    "data.table" = "fread",
    "maptools" = "readShapePoly",
    "readxl" = "read_excel",
    "utils" = "read.csv"
  )
  # sf::st_read, raster::brick

  for (pkg in names(pkg.watch)) {
    methods <- pkg.watch[[pkg]]

    if (pkg %in% attached.packages) {
      # replace now
      .replace.methods.with.spy(methods)
    } else {
      # replace them right after they're loaded
      setHook(
        packageEvent(pkg, "attach"),
        # parameters included for reader comprehension - we don't need or use them
        function(pkg.name, pkg.path) {
          # WARNING: you cannot use `methods` here. The reason is the value is
          # mutated within this loop and R's lazy evaluation means that ALL
          # hook functions will end up having a `methods` value from the final
          # iteration of the loop
          .replace.methods.with.spy(pkg.watch[[pkg.name]])
        },
        action = "append"
      )
    }
  }
}

#' Replace methods in global namespace with the spying methods defined below
#' @param methods These are names of functions to replace.
.replace.methods.with.spy <- function(methods) {
  # NOTE: google searches reveal that questions and answers are sadly conflating some terms
  # as.environment("package:PKGNAME") returns a namespace of exported values
  # getNamespace("PKGNAME") returns a namespace of all package contents
  this.ns <- getNamespace(utils::packageName())
  for (method in methods) {
    spy.method <- get(sprintf(".spy.on.%s", method), envir = this.ns)
    assign(method, spy.method, envir = globalenv())
  }
}


.spy.on.fread <- function(input, ...) {
  # fread has complex arguments. Punt on callers not using the standard first argument "input"
  if (missing("input")) {
    return(data.table::fread(...))
  }

  tryCatch({
    md <- get.metadata(input)
    md$call <- "fread"
    .append.input.file(md)
  },
  error = function(e) {
    message(sprintf("Errored recording metadata for %s - YOU ARE LACKING PROVENANCE", input))
  }, finally = {
    result <- data.table::fread(input, ...)
  })
  return(result)
}

.spy.on.read.csv <- function(file, ...) {
  tryCatch({
    md <- get.metadata(file)
    md$call <- "read.csv"
    .append.input.file(md)
  },
  error = function(e) {
    message(sprintf("Errored recording metadata for %s - YOU ARE LACKING PROVENANCE", file))
  }, finally = {
    result <- utils::read.csv(file, ...)
  })
  return(result)
}

.spy.on.readShapePoly <- function(fn, ...) {
  tryCatch({
    md <- get.metadata(fn)
    md$call <- "readShapePoly"
    .append.input.file(md)
  },
  error = function(e) {
    message(sprintf("Errored recording metadata for %s - YOU ARE LACKING PROVENANCE", fn))
  }, finally = {
    return(maptools::readShapePoly(fn, ...))
  })
}

.spy.on.read_excel <- function(path, ...) {
  tryCatch({
    md <- get.metadata(path)
    md$call <- "read_excel"
    .append.input.file(md)
  },
  error = function(e) {
    message(sprintf("Errored recording metadata for %s - YOU ARE LACKING PROVENANCE", path))
  }, finally = {
    return(readxl::read_excel(path, ...))
  })
}
