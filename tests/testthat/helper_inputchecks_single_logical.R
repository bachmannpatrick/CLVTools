.fct.helper.inputchecks.single.logical <- function(fct, l.std.args, name.param, null.allowed = FALSE){

  if(!null.allowed){
    test_that("Fails for NULL", {
      expect_error(do.call(fct, modifyList(l.std.args, setNames(list(param=NULL), name.param),keep.null = TRUE)),
                   regexp="logical")
    })
  }
  test_that("Fails for not logicals", {
    expect_error(do.call(fct, modifyList(l.std.args, setNames(list(param="1"), name.param))),
                 regexp="logical")
    expect_error(do.call(fct, modifyList(l.std.args, setNames(list(param=data.frame(TRUE)), name.param))),
                 regexp="logical")
    expect_error(do.call(fct, modifyList(l.std.args, setNames(list(param=list(TRUE)), name.param))),
                 regexp="logical")
    expect_error(do.call(fct, modifyList(l.std.args, setNames(list(param=1), name.param))),
                 regexp="logical")
  })
  test_that("Fails for NA", {
    expect_error(do.call(fct, modifyList(l.std.args, setNames(list(param=NA), name.param))),
                 regexp="cannot be NA")
  })
  test_that("Fails for multiple", {
    expect_error(do.call(fct, modifyList(l.std.args, setNames(list(param=c(TRUE,TRUE)), name.param))),
                 regexp="single element")
    expect_error(do.call(fct, modifyList(l.std.args, setNames(list(param=c(TRUE,FALSE)), name.param))),
                 regexp="single element")
    expect_error(do.call(fct, modifyList(l.std.args, setNames(list(param=c(FALSE,FALSE)), name.param))),
                 regexp="single element")
    expect_error(do.call(fct, modifyList(l.std.args, setNames(list(param=c(FALSE,NA)), name.param))),
                 regexp="single element")
  })
}
