# Copyright 2018 Opening Reproducible Research (https://o2r.info)

library("containerit")
context("ARG instruction")

test_that("A valid ARG instruction can be created" , {
  obj <- Arg("MYARG")
  expect_equal(toString(obj), "ARG MYARG")
})

test_that("A valid ARG instruction can be created with a default value" , {
  obj <- Arg("MYARG", value = "MYVAL")
  expect_equal(toString(obj), "ARG MYARG=MYVAL")
})

test_that("Invalid ARG instruction give error: no key" , {
    expect_error(Arg())
})

test_that("Invalid ARG instruction give error: empty key" , {
  expect_error(Arg(key = NA_character_))
  expect_error(Arg(key = ""))
})

test_that("Invalid ARG instruction give error: empty value" , {
  expect_error(Arg(key = "MYARG", value = ""))
})
