.fct.helper.inputchecks.fails.for.NULL <- function(fct, l.std.args, name.param, regexp){
  test_that("Fails for NULL", {
    expect_error(do.call(fct, modifyList(l.std.args, setNames(list(param=NULL), name.param),keep.null = TRUE)),
                 regexp=regexp)
  })
}

.fct.helper.inputchecks.fails.for.NA <- function(fct, l.std.args, name.param, NA_literal){
  test_that("Fails for NA", {
    expect_error(do.call(fct, modifyList(l.std.args, setNames(list(param=NA_literal), name.param))),
                 regexp="NA")
  })
}


.fct.helper.inputchecks.fails.if.not.allowed.type <- function(fct, l.std.args, name.param, name.allowed.type){

  l.illegal.inputs <- list(character = "1",
                           data.frame = data.frame(1),
                           list = list(1),
                           logical = TRUE,
                           numeric = 0.5,
                           Date = lubridate::ymd("2019-01-01"))

  # Remove legal type, only keep illegal types
  l.illegal.inputs <- l.illegal.inputs[which(names(l.illegal.inputs) != name.allowed.type)]

  test_that("Fails if not allowed type", {
    for(illegal.input in l.illegal.inputs){
      expect_error(do.call(fct, modifyList(l.std.args, setNames(list(param=illegal.input), name.param))),
                   regexp=name.allowed.type)
    }
  })
}



.fct.helper.inputchecks.fails.for.multiple <- function(fct, l.std.args, name.param, l.illegal.multiples){
  test_that("Fails for multiple", {
    for(illegal.arg in l.illegal.multiples){
      expect_error(do.call(fct, modifyList(l.std.args, setNames(list(param=illegal.arg), name.param))),
                   regexp="single")
    }
  })
}



.fct.helper.inputchecks.single.logical <- function(fct, l.std.args, name.param, null.allowed = FALSE){

  if(!null.allowed){
    .fct.helper.inputchecks.fails.for.NULL(fct=fct, l.std.args = l.std.args, name.param = name.param, regexp = "logical")
  }

  .fct.helper.inputchecks.fails.for.NA(fct=fct, l.std.args = l.std.args, name.param = name.param, NA_literal = NA)
  .fct.helper.inputchecks.fails.if.not.allowed.type(fct=fct, l.std.args = l.std.args, name.param = name.param,
                                                    name.allowed.type = "logical")

  .fct.helper.inputchecks.fails.for.multiple(fct=fct, l.std.args = l.std.args, name.param = name.param,
                                             l.illegal.multiples = list(c(TRUE,TRUE),
                                                                        c(TRUE,FALSE),
                                                                        c(FALSE,FALSE),
                                                                        c(FALSE,NA)))
}


fct.helper.inputcheck.single.numeric <- function(fct, l.std.args, name.param){

  .fct.helper.inputchecks.fails.for.NULL(fct=fct, l.std.args = l.std.args, name.param = name.param, regexp = "numeric")
  .fct.helper.inputchecks.fails.for.NA(fct=fct, l.std.args = l.std.args, name.param = name.param, NA_literal = NA_real_)

  .fct.helper.inputchecks.fails.if.not.allowed.type(fct=fct, l.std.args = l.std.args, name.param = name.param,
                                                    name.allowed.type = "numeric")

  .fct.helper.inputchecks.fails.for.multiple(fct=fct, l.std.args = l.std.args, name.param = name.param,
                                             l.illegal.multiples = list(c(0.1,0.1),
                                                                        c(0.1,1.1),
                                                                        c(1.5,0.1),
                                                                        c(0,0)))

}

fct.helper.inputcheck.single.character <- function(fct, l.std.args, name.param, null.allowed){
  if(!null.allowed){
    .fct.helper.inputchecks.fails.for.NULL(fct=fct, l.std.args = l.std.args, name.param = name.param, regexp = "character")
  }

  .fct.helper.inputchecks.fails.for.NA(fct=fct, l.std.args = l.std.args, name.param = name.param, NA_literal = NA_character_)

  .fct.helper.inputchecks.fails.if.not.allowed.type(fct=fct, l.std.args = l.std.args, name.param = name.param,
                                                    name.allowed.type = "character")

  .fct.helper.inputchecks.fails.for.multiple(fct=fct, l.std.args = l.std.args, name.param = name.param,
                                             l.illegal.multiples = list(c("a", "b"),
                                                                        c("Pareto", "NBD"),
                                                                        c("abc", ""),
                                                                        c("", "")))
}

