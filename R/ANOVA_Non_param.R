ANOVA_Non_param <-
function(start, end, dataX, group, comparison) {
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
    stop("The argument data need to be a data frame or a matrix")
  }

  if (is.factor(group) == FALSE) {
    stop("The argument group need to be a factor")
  }

  if (is.character(comparison) == FALSE) {
    stop("The argument comparison need to be a characther")
  }


  # Start of the function
  test <- NULL
  for (i in start:end) {
    # Calculation of the P-value form a Kruskal-Wallis test and the mean and SD for each
    pvalue <- NULL
    pvalue <- c(variable <- colnames(dataX)[i], pvalue <- kruskal.test(dataX[, i] ~ group)$p.value)
    # Calculation of a mean and SD by group

    pvalue <- c(pvalue, Moy <- by(dataX[, i], group, mean, na.rm = T), SD <- by(dataX[, i], group, sd, na.rm = T))
    test <- rbind(test, pvalue)
  }
  write.csv(test, paste("Summary_", comparison, ".csv", sep = ""), row.names = F)
  return(test)
}
