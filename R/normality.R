normality <-
function(start, end, dataX, project) {

  # test for the argument
  if (is.numeric(start) == FALSE) {
    stop("The argument start need to be a numeric argument")
  }

  if (is.numeric(end) == FALSE) {
    stop("The argument end need to be a numeric argument")
  }

  if (end == start) {
    stop("You need to have at least 2 variable in your data frame or matrix")
  }

  if (is.matrix(dataX) == FALSE & is.data.frame(dataX) == FALSE) {
    stop("The argument group_1 need to be a data frame or a matrix")
  }

  if (is.character(project) == FALSE) {
    stop("The argument comparison need to be a characther")
  }

  # Start of the function
  test <- NULL
  for (i in start:end) {
    if (sd(dataX[, i], na.rm = T) == 0) {
      test <- rbind(test, c(colnames(dataX)[i], NA))
    } else {
      test <- rbind(test, c(colnames(dataX)[i], shapiro.test(dataX[, i])$p.value))
    }
  }
  return(test)
  write.csv(test, paste("normality test for", project, ".csv", sep = ""))
}
