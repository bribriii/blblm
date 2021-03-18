#' @import purrr
#' @import stats
#' @import furrr
#' @import future
#' @importFrom magrittr %>%
#' @details
#' Logistic Regression with Little Bag of Bootstraps
"_PACKAGE"

#' @param formula formula to use in blbglm
#' @param data data
#' @param m number of splits
#' @param B number of boostraps
#' @param family glm family to specify
#' @param Parallel boolean value to specify whether to use parallelization
#'
#' @return blbglm object
#' @export
#'
#' @examples
#' blbglm(Species ~ Sepal.Length * Sepal.Width, iris[1:100,], 3, 100, family = binomial)
#' blbglm(Species ~ Sepal.Length * Sepal.Width, iris[1:100,], 3, 100, binomial, TRUE)
blbglm <- function(formula, data, m = 10, B = 5000, family, Parallel = FALSE){
  data_list<-split_data(data,m)
  if(Parallel){
    estimates <- future_map(
      data_list,
      ~ glm_each_subsample(formula = formula, data = ., n = nrow(.), B = B , family)
    )
  }else{
    estimates <- map(
      data_list,
      ~ glm_each_subsample(formula = formula, data = ., n = nrow(.), B = B ,family)
    )
  }
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blbglm"
  invisible(res)
}

#' split data into m parts of approximated equal sizes
#'
#' @param data data
#' @param m number of splits
#'
#' @return list of splited data
#'
split_data <- function(data, m) {
  idx <- sample.int(m, nrow(data), replace = TRUE)
  data %>% split(idx)
}

#' compute glm for each bootstrap
#'
#' @param formula formula being passed from blbglm
#' @param data data
#' @param n number of rows for data
#' @param B number of bootstraps
#' @param family glm family to specify
#'
#' @return a list of glm objects
#' @export
#'
glm_each_subsample <- function(formula, data, n, B, family) {
  replicate(B, glm1(formula, data, n, family), simplify = FALSE)
}

#' compute the logistic regression estimates for a blb dataset
#'
#' @param formula formula to use in blblm
#' @param data data
#' @param n number of rows
#' @param family glm family to specify
#'
#' @return list of coefficients and sigma
#' @export
#'
glm1<-function(formula, data, n, family){
  environment(formula) <- environment()
  freqs <- rmultinom(1, n, rep(1, nrow(data)))
  fit <- glm(formula, data, family = family, weights = freqs)
  list(coef = blbcoef(fit), sigma = sigma(fit))
}

#' compute the coefficients from fit
#'
#' @param fit objects
#'
#' @return list of coefficients
blbcoef <- function(fit) {
  coef(fit)
}

