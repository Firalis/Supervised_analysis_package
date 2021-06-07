student <-
function(start, end, group_1, group_2, log, base, comparison) {

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

  if (is.matrix(group_1) == FALSE & is.data.frame(group_1) == FALSE) {
    stop("The argument group_1 need to be a data frame or a matrix")
  }

  if (is.matrix(group_2) == FALSE & is.data.frame(group_2) == FALSE) {
    stop("The argument group_2 need to be a data frame or a matrix")
  }

  if (is.logical(log) == FALSE) {
    stop("The argument log need to be a logical argument")
  }

  if (is.numeric(base) == FALSE) {
    stop("The argument base need to be a numeric argument")
  }

  if (is.character(comparison) == FALSE) {
    stop("The argument comparison need to be a characther")
  }

  # Start of the function
  test <- NULL
  for (i in start:end) {
    # Calculation of the P-value form a Student test and the mean and SD for each
    pvalue <- NULL
    pvalue <- c(
      variable <- colnames(group_2)[i], moygroup2 <- mean(group_2[, i], na.rm = TRUE),
      sdgroup2 <- sd(group_2[, i], na.rm = TRUE), moygroup1 <- mean(group_1[, i], na.rm = TRUE),
      sdgroup1 <- sd(group_1[, i], na.rm = TRUE), pvalue <- student.test(group_1[, i], group_2[, i], na.rm = TRUE)$p.value
    )
    # Calculation of a foldchange depending f we do a log transformation or not
    foldchange <- NULL
    if (log == TRUE) {
      foldchange <- mean(base^group_1[, i], na.rm = TRUE) / mean(base^group_2[, i], na.rm = TRUE)
    } else {
      foldchange <- mean(group_1[, i], na.rm = TRUE) / mean(group_2[, i], na.rm = TRUE)
    }
    pvalue <- c(pvalue, foldchange <- foldchange)
    test <- rbind(test, pvalue)
  }
  write.csv(test, paste("Summary_", comparison, ".csv", sep = ""), row.names = FALSE)
  return(test)
}
