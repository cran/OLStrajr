## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(OLStrajr)

## -----------------------------------------------------------------------------
data(rats)
print(rats)

## -----------------------------------------------------------------------------
# Run OLS traj
rats_lin <- OLStraj(data = rats,
                      idvarname = "Rat",
                      predvarname = "Week",
                      outvarname = "Weight",
                      varlist = c("t0", "t1", "t2", "t3", "t4"),
                      timepts = c(0, 1, 2, 3, 4),
                      regtype = "lin",
                      int_bins = 3,
                      lin_bins = 3)

# Show linear trajectories 
rats_lin$individual_plots

## -----------------------------------------------------------------------------
print(rats_lin$models)

## -----------------------------------------------------------------------------
OLStraj(data = rats,
        idvarname = "Rat",
        predvarname = "Week",
        outvarname = "Weight",
        varlist = c("t0", "t1", "t2", "t3", "t4"),
        timepts = c(0, 1, 2, 3, 4),
        regtype = "quad",
        int_bins = 3,
        lin_bins = 3)$individual_plots

## -----------------------------------------------------------------------------
OLStraj(data = rats,
        idvarname = "Rat",
        predvarname = "Week",
        outvarname = "Weight",
        varlist = c("t0", "t1", "t2", "t3", "t4"),
        timepts = c(0, 1, 2, 3, 4),
        regtype = "both",
        int_bins = 3,
        lin_bins = 3)$individual_plots

