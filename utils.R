# Utils Functions
# Notebook author: Martin Saveski (msaveski@mit.edu)
# Copyright (c) Facebook, Inc. and its affiliates.
# This source code is licensed under the MIT license found in the LICENSE file in the root directory of this source tree.

#################################################################################
#
# Utils
#
#################################################################################

multi_spread <- function(df, key, value) {
  # quote key
  keyq <- rlang::enquo(key)
  # break value vector into quotes
  valueq <- rlang::enquo(value)
  s <- rlang::quos(!!valueq)
  df %>% gather(variable, value, !!!s) %>%
    unite(temp, !!keyq, variable) %>%
    spread(temp, value)
}

#################################################################################

per_change_delta <- function(
  m.test, m.control,
  n.test, n.control,
  std.test, std.control
) {
  
  se.test <- std.test / sqrt(n.test)
  
  se.control <- std.control / sqrt(n.control)
  
  mean <- (m.test - m.control) / abs(m.control) -
    se.control ^ 2 * m.test / abs(m.control) ^ 3
  
  var <- (se.test / abs(m.control)) ^ 2 +
    (se.control * abs(m.test)) ^ 2 / abs(m.control) ^ 4
  
  z95 <- stats::qnorm(1 - (1 - 0.95) / 2, 0, 1)
  
  return(data.frame(
    mean     = mean,
    lower95  = mean - z95 * sqrt(var),
    upper95  = mean + z95 * sqrt(var)
  ))
}

per_change_delta_se <- function(
  m.test, m.control,
  se.test, se.control
) {
  
  mean <- (m.test - m.control) / abs(m.control) -
    se.control ^ 2 * m.test / abs(m.control) ^ 3
  
  var <- (se.test / abs(m.control)) ^ 2 +
    (se.control * abs(m.test)) ^ 2 / abs(m.control) ^ 4
  
  z95 <- stats::qnorm(1 - (1 - 0.95) / 2, 0, 1)
  
  return(data.frame(
    mean     = mean,
    lower95  = mean - z95 * sqrt(var),
    upper95  = mean + z95 * sqrt(var)
  ))
}

# END
