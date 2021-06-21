library(tercen)
library(dplyr)

# Set appropriate options
#options("tercen.serviceUri"="http://tercen:5400/api/v1/")
#options("tercen.workflowId"= "050e773677ecc404aa5d5a7580016b7d")
#options("tercen.stepId"= "6a509b68-33a3-4397-9b9c-12696ce2ffac")
#options("tercen.username"= "admin")
#options("tercen.password"= "admin")

eps <- 0.01

rowMean = function(df) {
  data.frame(.ri = df$.ri[1], rMean = mean(df$.y, na.rm = TRUE))
}

cvFit <- function(x, s0, s1) {
  sqrt(((s1^2)*(x^2) + (s0^2))/(x^2))
}

cvxfun <- function(df) {
  data.frame(df, pr.cvx = mean(df$cvx, na.rm = TRUE) )
}

ctx = tercenCtx()

quantile_low_signal_spots <- ifelse(is.null(ctx$op.value('Quantile for low signal spots')), 0.1, as.double(ctx$op.value('Quantile for low signal spots')))
high_signal_cv            <- ifelse(is.null(ctx$op.value('High signal CV')), 0.1, as.double(ctx$op.value('High signal CV')))
nominal_cv_max            <- ifelse(is.null(ctx$op.value('Nominal CV max')), 1, as.double(ctx$op.value('Nominal CV max')))

data <- ctx %>% 
  select(.ci, .ri, .y)

mean_data <- data %>%
  group_by(.ri) %>%
  do(rowMean(.))

mean_data <- mean_data[order(mean_data$rMean),]
n_spots   <- round(dim(mean_data)[1] * quantile_low_signal_spots)
spot_data <- subset(data, data$.ri %in% mean_data[1:n_spots,]$.ri )
fit_data  <- aov(.y ~ .ri, spot_data)
s0        <- sqrt(sum(fit_data$residuals^2) / fit_data$df.residual)

data %>%
  rename(cvx = .y) %>%
  mutate(cvx = ifelse(cvx < eps, eps, cvx)) %>%
  group_by(.ri) %>%
  do(cvxfun(.)) %>%
  ungroup()     %>%
  mutate(cv.estimate = cvFit(cvx, s0, high_signal_cv)) %>%
  mutate(pep.cv.estimate = cvFit(pr.cvx, s0, high_signal_cv)) %>%
  mutate(pep.cv.estimate = ifelse(pep.cv.estimate > nominal_cv_max, nominal_cv_max, pep.cv.estimate)) %>%
  ctx$addNamespace() %>%
  ctx$save()
