# Copyright 2018 Opening Reproducible Research (https://o2r.info)

library("containerit")
context("ENV instruction")

test_that("A valid ENV instruction can be created with a default value" , {
  obj <- Env("MYENV", value = "MYVAL")
  expect_equal(toString(obj), "ENV MYENV=MYVAL")
})

test_that("Invalid ENV instruction give error: no key" , {
    expect_error(Env())
})

test_that("Invalid ENV instruction give error: empty key" , {
  expect_error(Env(key = NA_character_))
  expect_error(Env(key = ""))
})

test_that("Invalid ENV instruction give error: empty value" , {
  expect_error(Env(key = "MYENV", value = NA_character_))
  expect_error(Env(key = "MYENV", value = ""))
})
