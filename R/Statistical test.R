#' @title wilcox
#' @author Matthieu COQ
#' Version: 1.0
#' Date: 23-Aug-2018
#' Objective: the Objective is to calculate a Wilcoxon test and the foldchange
#'
#'
#' @param start the start of your matrix or data frame
#' @param end the end of the matrix or data frame
#' @param group_1 data frame or matrix for  the first group of your comparison for the Wilcoxon test
#' @param group_2 data frame or matrix for  the second group of your comparison for the Wilcoxon test
#' @param log logical TRUE if your data are log transformed
#' @param base base for your log transformation
#' @param comparison name of your comparison that it will the title of your output
#' @return the function give a csv file with a mean, the standard deviation, the p-value of a Wilcoxon test and the foldchange for all variable in your matrix or data frame
#' @export


wilcox <- function(start, end, group_1, group_2, log, base, comparison) {

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

  # start of the function
  test <- NULL
  for (i in start:end) {
    # Calculation of the P-value form a Wilcoxon test and the mean and SD for each
    pvalue <- NULL
    pvalue <- c(
      variable <- colnames(group_2)[i], moygroup2 <- mean(group_2[, i], na.rm = TRUE),
      sdgroup2 <- sd(group_2[, i], na.rm = TRUE), moygroup1 <- mean(group_1[, i], na.rm = TRUE),
      sdgroup1 <- sd(group_1[, i], na.rm = TRUE), pvalue <- wilcox.test(group_1[, i], group_2[, i], na.rm = TRUE)$p.value
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


#' @title student
#' @author Matthieu COQ
#' Version: 2.0
#' Date: 03-Jun-21
#' Objective: the Objective is to calculate a Student test and the foldchange
#'
#'
#' @param start the start of your matrix or data frame
#' @param end the end of the matrix or data frame
#' @param group_1 data frame or matrix for  the first group of your comparison for the Student test
#' @param group_2 data frame or matrix for  the second group of your comparison for the Student test
#' @param log logical TRUE if your data are log transformed
#' @param base base for your log transformation
#' @param comparison name of your comparison that it will the title of your output
#' @return the function give a csv file with a mean, the standard deviation, the p-value of a Student test and the foldchange for all variable in your matrix or data frame
#' @export

student <- function(start, end, group_1, group_2, log, base, comparison) {

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
      sdgroup1 <- sd(group_1[, i], na.rm = TRUE), pvalue <- t.test(group_1[, i], group_2[, i], na.rm = TRUE)$p.value
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

#'  @title  normality
#'  @author Matthieu COQ
#' Version: 2.0
#' Date: 04-Jun-21
#' Objective: the Objective is to test the normality of the data
#'
#'
#' @param start the start of your matrix or data frame
#' @param end the end of the matrix or data frame
#' @param dataX data frame or matrix for the Shapiro-Wilk test
#' @param project name of your comparison that it will the title of your output
#' @return the function give a csv file with p-value of the Shapiro-Wilk test for all variable in your matrix or data frame
#' @export

normality <- function(start, end, dataX, project) {

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


#' @title ANOVA_Non_param
#' @author Matthieu COQ
#' Version: 2.0
#' Date: 04-Jun-21
#' Objective: the Objective is to calculate the non-parametric ANOVA
#'
#'
#' @param start the start of your matrix or data frame
#' @param end the end of the matrix or data frame
#' @param dataX data frame or matrix for the Kruskal Wallis test
#' @param group Group for the comparison
#' @param comparison name of your comparison that it will the title of your output
#' @return the function give a csv file with p-value of the Kruskal Waliis test and the mean and SD for each group for all variable in your matrix or data frame
#' @export

ANOVA_Non_param <- function(start, end, dataX, group, comparison) {
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

#' @title student_fdr
#' @author Matthieu COQ
#' Version: 2.0
#' Date: 04-Feb-2022
#' Objective: the Objective is to calculate a Student test and the foldchange
#'
#'
#' @param start the start of your matrix or data frame
#' @param end the end of the matrix or data frame
#' @param group_1 data frame or matrix for  the first group of your comparison for the Student test
#' @param group_2 data frame or matrix for  the second group of your comparison for the Student test
#' @param log logical TRUE if your data are log transformed
#' @param base base for your log transformation
#' @param comparison name of your comparison that it will the title of your output
#' @return the function give a csv file with a mean, the standard deviation, the p-value of a Student test and the foldchange for all variable in your matrix or data frame
#' @export

student_fdr <- function(start, end, group_1, group_2, log, base, comparison) {
  
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
      sdgroup1 <- sd(group_1[, i], na.rm = TRUE), pvalue <- t.test(group_1[, i], group_2[, i], na.rm = TRUE)$p.value
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
  test=cbind(test,p.adjust(test[,6],"fdr"))
  write.csv(test, paste("Summary_", comparison, ".csv", sep = ""), row.names = FALSE)
  return(test)
}


#' @title wilcox_fdr
#' @author Matthieu COQ
#' Version: 1.0
#' Date: 23-Aug-2018
#' Objective: the Objective is to calculate a Wilcoxon test and the foldchange
#'
#'
#' @param start the start of your matrix or data frame
#' @param end the end of the matrix or data frame
#' @param group_1 data frame or matrix for  the first group of your comparison for the Wilcoxon test
#' @param group_2 data frame or matrix for  the second group of your comparison for the Wilcoxon test
#' @param log logical TRUE if your data are log transformed
#' @param base base for your log transformation
#' @param comparison name of your comparison that it will the title of your output
#' @return the function give a csv file with a mean, the standard deviation, the p-value of a Wilcoxon test and the foldchange for all variable in your matrix or data frame
#' @export


wilcox_fdr <- function(start, end, group_1, group_2, log, base, comparison) {
  
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
  
  # start of the function
  test <- NULL
  for (i in start:end) {
    # Calculation of the P-value form a Wilcoxon test and the mean and SD for each
    pvalue <- NULL
    pvalue <- c(
      variable <- colnames(group_2)[i], moygroup2 <- mean(group_2[, i], na.rm = TRUE),
      sdgroup2 <- sd(group_2[, i], na.rm = TRUE), moygroup1 <- mean(group_1[, i], na.rm = TRUE),
      sdgroup1 <- sd(group_1[, i], na.rm = TRUE), pvalue <- wilcox.test(group_1[, i], group_2[, i], na.rm = TRUE)$p.value
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
  test=cbind(test,p.adjust(test[,6],"fdr"))
  write.csv(test, paste("Summary_", comparison, ".csv", sep = ""), row.names = FALSE)
  return(test)
}