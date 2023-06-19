## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(OLStrajr)

## -----------------------------------------------------------------------------
data(robins)

## -----------------------------------------------------------------------------
# Convert to long form
library(tidyr)

robinsL <- robins |> pivot_longer(cols = starts_with("aug"),
                                  names_to = "Year",
                                  values_to = "Ratio")

robinsL$Year = as.numeric(as.factor(robinsL$Year)) - 1

## -----------------------------------------------------------------------------
robins_mod <- cbc_lm(robinsL, Ratio ~ Year, .case = "site")

# Show class
class(robins_mod)

## -----------------------------------------------------------------------------
summary(robins_mod)

