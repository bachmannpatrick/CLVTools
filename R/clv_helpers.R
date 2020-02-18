# helper to convert list to printable array
.list2array <- function(l, col.n="", row.n=names(l), nsmall=4){
  disp           <- array(data=NA_character_, dim=list(length(l), 1))
  disp[, 1]      <- unlist(format(l, na.encode = FALSE, digits=nsmall, nsmall=nsmall, scientific=FALSE))
  rownames(disp) <- row.n
  colnames(disp) <- col.n
  return(disp)
}


.print.list <- function(l, col.n = "", row.n = names(l), nsmall=4){
  disp.arr <- .list2array(l, col.n=col.n, row.n=row.n, nsmall=nsmall)
  print(disp.arr, na.print = "",  quote = FALSE)
}


#helper function to to check if there are error messages and print+stop them
check_err_msg <- function(err.msg){
  if(length(err.msg) > 0)
    stop(c("\n",paste0(err.msg, sep="\n")),call. = FALSE)
}


.check_user_data_single_boolean <- function(b, var.name){
  err.msg <- c()
  if(!is.logical(b))
    return(paste0("The parameter ", var.name, " needs to be of type logical (True/False)!"))
  if(length(b)>1)
    err.msg <- c(err.msg, paste0("The parameter ", var.name, " can only contain a single element!"))
  if(anyNA(b))
    err.msg <- c(err.msg, paste0("The parameter ", var.name, " cannot be NA!"))
  return(err.msg)
}

.check_userinput_single_character <- function(char, var.name){
  err.msg <- c()
  if(!is.character(char))
    return(paste0(var.name, " needs to be of type character (text)!"))
  if(length(char) != 1)
    err.msg <- c(err.msg, paste0(var.name, " must contain exactly one single element!"))
  if(anyNA(char))
    err.msg <- c(err.msg, paste0(var.name, " may not contain any NA!"))
  if(length(err.msg) == 0){
      # is non empty vec, but check is not no text ("")
      if(nchar(char[[1]]) == 0)
        err.msg <- c(err.msg, paste0(var.name, " may not be empty text!"))
  }

  return(err.msg)
}

.check_user_data_single_numeric <- function(n, var.name){
  err.msg <- c()
  if(!is.numeric(n))
    return(paste0(var.name, " has to be numeric!"))
  if(length(n)!=1)
    err.msg <- c(err.msg, paste0(var.name," has to be exactly 1 single number!"))
  if(anyNA(n))
    err.msg <- c(err.msg, paste0(var.name," may not be NA!"))
  return(err.msg)
}


.convert_userinput_dataid <- function(id.data){
  return(as.character(id.data))
}


