
fct.helper.correctness.estimationsplit <- function(data, estimation.number, estimation.char, time.unit, warn=FALSE){
  # not silent because may warn if data is in posix but use weeks/days/years time.unit

  if(warn){
    # integer
    expect_message(data.integer <- clvdata(estimation.split = as.integer(estimation.number), time.unit = time.unit,
                                           data.transactions = data, date.format = "ymd"), regexp = "ignored")

    # real
    expect_message(data.real <- clvdata(estimation.split = as.numeric(estimation.number), time.unit = time.unit,
                                       data.transactions = data, date.format = "ymd"), regexp = "ignored")
    # char
    expect_message(data.char <- clvdata(estimation.split = estimation.char, time.unit = time.unit,
                                       data.transactions = data, date.format = "ymd"), regexp = "ignored")
    # Date
    expect_message(data.date <- clvdata(estimation.split = as.Date(estimation.char), time.unit = time.unit,
                                       data.transactions = data, date.format = "ymd"), regexp = "ignored")
    # posixct
    expect_message(data.posix.utc <- clvdata(estimation.split = lubridate::parse_date_time(estimation.char,orders = "ymd", tz="UTC"),
                                          time.unit = time.unit, data.transactions = data, date.format = "ymd"), regexp = "ignored")
    expect_message(data.posix.acst <- clvdata(estimation.split = lubridate::parse_date_time(estimation.char,orders = "ymd", tz="Australia/Darwin"),
                                           time.unit = time.unit, data.transactions = data, date.format = "ymd"), regexp = "ignored")
  }else{
    expect_silent(data.integer <- clvdata(estimation.split = as.integer(estimation.number), time.unit = time.unit,
                                          data.transactions = data, date.format = "ymd"))

    expect_silent(data.real <- clvdata(estimation.split = as.numeric(estimation.number), time.unit = time.unit,
                                       data.transactions = data, date.format = "ymd"))
    expect_silent(data.char <- clvdata(estimation.split = estimation.char, time.unit = time.unit,
                                       data.transactions = data, date.format = "ymd"))
    expect_silent(data.date <- clvdata(estimation.split = as.Date(estimation.char), time.unit = time.unit,
                                       data.transactions = data, date.format = "ymd"))
    if(data[, lubridate::is.Date(Date)] | data[, is.character(Date)] ){ # actually: char + time unit implies Date
      expect_message(data.posix.utc <- clvdata(estimation.split = lubridate::parse_date_time(estimation.char,orders = "ymd", tz="UTC"),
                                              time.unit = time.unit, data.transactions = data, date.format = "ymd"), regexp = "ignored")
      expect_message(data.posix.acst <- clvdata(estimation.split = lubridate::parse_date_time(estimation.char,orders = "ymd", tz="Australia/Darwin"),
                                               time.unit = time.unit, data.transactions = data, date.format = "ymd"), regexp = "ignored")
    }else{
      expect_silent(data.posix.utc <- clvdata(estimation.split = lubridate::parse_date_time(estimation.char,orders = "ymd", tz="UTC"),
                                              time.unit = time.unit, data.transactions = data, date.format = "ymd"))
      expect_silent(data.posix.acst <- clvdata(estimation.split = lubridate::parse_date_time(estimation.char,orders = "ymd", tz="Australia/Darwin"),
                                               time.unit = time.unit, data.transactions = data, date.format = "ymd"))
    }
  }

  data.integer@call <- as.symbol("abc")
  data.real@call    <- as.symbol("abc")
  data.char@call    <- as.symbol("abc")
  data.date@call    <- as.symbol("abc")
  data.posix.utc@call   <- as.symbol("abc")
  data.posix.acst@call   <- as.symbol("abc")

  expect_equal(data.integer, data.real)
  expect_equal(data.real, data.char)
  expect_equal(data.char, data.date)
  expect_equal(data.date, data.posix.utc)
  expect_equal(data.posix.utc, data.posix.acst)
}

