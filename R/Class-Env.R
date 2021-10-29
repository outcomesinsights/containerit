# Copyright 2018 Opening Reproducible Research (https://o2r.info)

#' Env-instruction class yet to be implemented
#' @include Class-Instruction.R
#'
#' See official documentation at \url{https://docs.docker.com/engine/reference/builder/#arg}.
#'
#' @slot key character.
#' @slot value character.
#' @family instruction classes
#' @family Env instruction
#' @return object of class Env
#' @export
setClass("Env",
         slots = list(key = "character",
                      value = "character"), contains = "Instruction")

#' Create objects representing a ENV instruction
#'
#' @param key character argument naming the argument
#' @param value default value for the argument, of class character
#' @family Env instruction
#' @return An S4 object of class Env
#' @export
Env <- function(key, value) {
  methods::new("Env", key = key, value = value)
}

setMethod("docker_arguments",
          signature(obj = "Env"),
          function(obj) {
            key <- methods::slot(obj, "key")
            value <- methods::slot(obj, "value")
            string <- ""
            if (!is.na(key)) {
              string <- paste0(string, key)
              if (!is.na(value)) {
                string <- paste0(string, "=")
              }
            }
            if (!is.na(value)) {
              string <- paste0(string, value)
            }

            return(string)
          })

setValidity("Env",
            method = function(object) {
              key <- methods::slot(object, "key")
              value <- methods::slot(object, "value")

              if (is.na(key) || stringr::str_length(key) == 0) {
                return(paste("key must be a non-empty string, given was: ", key))
              } else if (is.na(value) || stringr::str_length(value) == 0) {
                return("value must be a non-empty string, given was: ", value)
              } else {
                return(TRUE)
              }
            }
)
