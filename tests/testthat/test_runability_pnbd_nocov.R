skip_on_cran()

fct.testthat.runability.nocov(
  name.model = "PNBD",
  method = pnbd,
  has.cor = TRUE,
  start.params.model = c(r=0.5, alpha=15, s=0.5, beta=10),
  failed.optimization.methods.expected.message =
    "Gradient not computable after method|NA/Inf replaced by maximum positive"
)


