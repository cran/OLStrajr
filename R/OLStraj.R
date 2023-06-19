#' OLStraj
#'
#' Implements the OLS trajectory analysis method as detailed in Carrig et al (2004).
#' The method uses case-by-case ordinary least squares (OLS) regression to estimate
#' individual growth curves over time. The function provides options for
#' group-level and individual-level plots and accommodates linear and quadratic
#' models.
#'
#' @param data A data frame
#' @param idvarname A quoted variable name identifying the column in data which
#' serves as the case identifier
#' @param predvarname A quoted predictor variable label.
#' @param outvarname A quoted outcome variable label.
#' @param varlist A vector of quoted variable names found in data
#' @param timepts A vector specifying how time points should be coded
#' @param inclmiss A logical specifying whether or not to use complete cases.
#' Set inclmiss to FALSE in order to filter data down to complete cases.
#' @param level Control which OLS trajectory plots to show.  If level is set to
#' "grp" then only group level plots will be shown, if level is set to "ind" then
#' only individual level plots will be shown, and if level is set to "both" then
#' both group and individual level plots will be shown.
#' @param regtype Set regtype to "quad" to include quadratic term in the cbc_lm
#' call or set regtype to "lin" to exclude the quadratic term.  Use regtype = "both"
#' to include the quadratic term in the cbc_lm call and to include both linear
#' and quadratic terms on the individual OLS-estimated trajectory plots.
#' @param numplot Specify an integer to subset the number of cases used in OLStraj
#' @param hist Set hist to  TRUE to include histograms or FALSE to exclude
#' @param int_bins Set the number of bins for the intercept term's histogram
#' @param lin_bins Set the number of bins for the linear term's histogram
#' @param quad_bins Set the number of bins for the quadratic term's histogram
#' @param box Set box to TRUE to include boxplots or FALSE to exclude
#' @param outds Set outds to TRUE to include the output as a data frame.  Output
#' will contain original data used in the OLStraj algorithm with the parameter
#' estimates obtained from cbc_lm
#' @param ... Pass additional arguments to cbc_lm
#'
#' @return A list containing an output data frame (if outds is set to TRUE), the
#' selected plots, and the case-by-case regression model object.
#' @export
#'
#' @references
#' Carrig, M.M., Wirth, R.J., & Curran, P.J. (2004). A SAS Macro for Estimating
#' and Visualizing Individual Growth Curves.  Structural Equation Modeling: A
#' Multidisciplinary Journal, 11(1), 132-149.
#' \doi{10.1207/S15328007SEM1101_9}
#'
#' @examples
#'   df <- data.frame(id = c(1,2,3,4,5),
#'                    var1 = c(3,7,4,5,8),
#'                    var2 = c(7,3,9,4,7),
#'                    var3 = c(8,5,3,9,7),
#'                    var4 = c(1,5,3,9,30))
#'
#'   olstraj_out <- OLStraj(data = df,
#'                          varlist = c("var1", "var2", "var3", "var4"),
#'                          regtype = "quad",
#'                          int_bins = 5,
#'                          lin_bins = 5,
#'                          quad_bins = 5)
OLStraj <- function(data, idvarname = "id", predvarname = "time",
                    outvarname = "score",
                    varlist = c("anti1", "anti2", "anti3", "anti4"),
                    timepts = c(0, 1, 2, 3), inclmiss = FALSE, level = "both", regtype = "lin",
                    numplot = NULL, hist = TRUE, int_bins = 30, lin_bins = 30,
                    quad_bins = 30, box = TRUE, outds = TRUE, ...) {

  # Check if data is a data.frame
  if (!is.data.frame(data)) {
    stop("data must be a data frame.")
  }

  # Check if idvarname, predvarname and outvarname exist in data
  if (!(idvarname %in% names(data))) {
    stop("idvarname must be a column name in the provided data frame.")
  }

  # Check if varlist elements are column names in data
  if (!all(varlist %in% names(data))) {
    stop("All elements of varlist must be column names in the provided data frame.")
  }

  # Check if length of timepts matches length of varlist
  if(length(timepts) != length(varlist)){
    stop("ERROR: NUMBER OF TIME POINTS DOES NOT EQUAL NUMBER OF REPEATED MEASURES")
  }

  # Check if inclmiss is logical
  if (!is.logical(inclmiss)) {
    stop("inclmiss must be of type logical.")
  }

  # Check if level is one of "both", "grp", or "ind"
  if (!level %in% c("both", "grp", "ind")) {
    stop("level must be either 'both', 'grp', or 'ind'.")
  }

  # Check if regtype is either "lin" or "quad"
  if (!regtype %in% c("lin", "quad", "both")) {
    stop("regtype must be either 'lin', 'quad', or 'both'.")
  }

  # Check if numplot is NULL or an integer
  if (!is.null(numplot) & (!is.numeric(tryCatch(numplot, error = function(e) NULL) |
                                       (tryCatch(numplot != round(numplot), error = function(e) NULL))))) {
    stop("numplot must be either NULL or an integer.")
  }

  # Check if hist is logical
  if (!is.logical(hist)) {
    stop("hist must be of type logical.")
  }

  # Check if box is logical
  if (!is.logical(box)) {
    stop("box must be of type logical.")
  }

  # Check if outds is logical
  if (!is.logical(outds)) {
    stop("outds must be of type logical.")
  }

  # Check if int_bins, lin_bins, quad_bins are positive integers
  if (!is.numeric(int_bins) | (int_bins != round(int_bins)) | (int_bins <= 0) |
      !is.numeric(lin_bins) | (lin_bins != round(lin_bins)) | (lin_bins <= 0) |
      !is.numeric(quad_bins) | (quad_bins != round(quad_bins)) | (quad_bins <= 0)) {
    stop("int_bins, lin_bins, quad_bins must be positive integers.")
  }

  # Create subsample
  if (!is.null(numplot)) {
    data <- data[1:numplot, ]
  }

  # Save a copy of data in original format
  data_orig <- data

  # Listwise deletion
  if (inclmiss == FALSE) {
    data <- data[stats::complete.cases(data), ]
  }

  # OLS case-by-case regressions

  #Lengthen data frame
  data <- tidyr::pivot_longer(data, cols = tidyselect::all_of(varlist),
                              names_to = predvarname,
                              values_to = outvarname)

  data[[predvarname]] <- timepts[match(data[[predvarname]], varlist)]

  # Get quadratic term
  if (regtype != "lin"){
    data[[paste0(predvarname, "_sq")]] <- data[[predvarname]]^2
  }

  if (regtype == "lin"){
    cbc_form <- stats::as.formula(paste(outvarname, "~", predvarname))
  } else {
    cbc_form <- stats::as.formula(paste(outvarname, "~", predvarname, "+",
                                        paste0(predvarname, "_sq")))
  }

  models <- cbc_lm(data = data, formula = cbc_form, .case = idvarname, ...)

  ols_dat <- lapply(seq_along(models$models), function(i){
    # Define model
    model <- models$models[[i]]
    id_val <- names(models$models)[[i]]

    # Add the estimated values to the data frame
    if (regtype == "lin"){
      stats::setNames(data.frame(id_val,
                                 stats::coef(model)[1],
                                 stats::coef(model)[2],
                                 summary(model)[[8]]),
                      c(eval(idvarname), "intercept", "linear", "rsquared"))
    } else {
      stats::setNames(data.frame(id_val,
                                 stats::coef(model)[1],
                                 stats::coef(model)[2],
                                 stats::coef(model)[3],
                                 summary(model)[[8]]),
                      c(eval(idvarname), "intercept", "linear", "quad", "rsquared"))
    }
  })

  estimated_values <- do.call(rbind, ols_dat)

  # Write output to a data frame
  if (outds == TRUE) {
    out_data <- merge(data_orig, estimated_values, by = eval(idvarname))
  }

  # Plotting section
  group_plots <- list()
  individual_plots <- list()
  histogram_plots <- list()

  # Prepare data for plotting
  data <- merge(data, estimated_values, by = eval(idvarname))

  if (level == "both" | level == "grp") {
    # Group-level plots

    # Simple-joined (noninterpolated) trajectories
    group_plots[["simple_joined"]] <- ggplot2::ggplot(data,
                                                      ggplot2::aes(x = .data[[predvarname]],
                                                                   y = .data[[outvarname]],
                                                                   group = .data[[idvarname]])) +
      ggplot2::geom_line() +
      ggplot2::ggtitle("Simple-Joined Trajectories")

    # OLS trajectories
    group_plots[["ols"]] <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[predvarname]], y = .data[[outvarname]],
                                                               group = .data[[idvarname]])) +
      ggplot2::ggtitle("OLS Trajectories")

    if (regtype == "lin"){
      group_plots[["ols"]] <- group_plots[["ols"]] +
        ggplot2::geom_smooth(se = FALSE, method = lm)

    } else {
      group_plots[["ols"]] <- group_plots[["ols"]] +
        ggplot2::stat_smooth(method = "lm", formula = y ~ x + I(x^2),
                             se = FALSE)
    }

  }

  if (level == "both" | level == "ind") {
    # Individual-level plots
    for (id in unique(data[[idvarname]])) {
      ind_data <- data[data[[idvarname]] == id, ]

      # OLS trajectories
      individual_plots[[paste("ols", id)]] <- ggplot2::ggplot(ind_data,
                                                              ggplot2::aes(x = .data[[predvarname]],
                                                                           y = .data[[outvarname]])) +
        ggplot2::ggtitle(paste("OLS Trajectory for", idvarname, ": ", id))

      if (regtype == "lin"){
        individual_plots[[paste("ols", id)]] <- individual_plots[[paste("ols", id)]] +
          ggplot2::geom_point() +
          ggplot2::geom_smooth(se = FALSE, method = lm)


      } else if (regtype == "quad"){
        individual_plots[[paste("ols", id)]] <- individual_plots[[paste("ols", id)]] +
          ggplot2::geom_point() +
          ggplot2::geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE)

      } else {
        individual_plots[[paste("ols", id)]] <- individual_plots[[paste("ols", id)]] +
          ggplot2::geom_point() +
          ggplot2::geom_smooth(se = FALSE, method = lm) +
          ggplot2::geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE,
                               linetype = "dashed")
      }
    }
  }

  if (hist == TRUE) {
    # Histogram
    intercepts <- ggplot2::ggplot(data, ggplot2::aes(x = intercept)) +
      ggplot2::geom_histogram(bins = int_bins) +
      ggplot2::ggtitle("Histogram of OLS Estimated Intercepts")

    slopes <- ggplot2::ggplot(data, ggplot2::aes(x = linear)) +
      ggplot2::geom_histogram(bins = lin_bins) +
      ggplot2::ggtitle("Histogram of OLS Estimated Slopes")

    if (regtype != "lin"){
      quads <- slopes <- ggplot2::ggplot(data, ggplot2::aes(x = quad)) +
        ggplot2::geom_histogram(bins = quad_bins) +
        ggplot2::ggtitle("Histogram of OLS Estimated Quadratic Terms")

      histogram_plots = list("intercepts" = intercepts,
                             "slopes" = slopes,
                             "quads" = quads)
    } else {
      histogram_plots = list("intercepts" = intercepts,
                             "slopes" = slopes)
    }
  }

  if (box == TRUE) {
    #Add mean to boxplot and label outliers:
    if (regtype == "lin"){
      data_box <- data |> tidyr::pivot_longer(cols = c(intercept, linear),
                                              names_to = "param",
                                              values_to = "est")
    } else {
      data_box <- data |> tidyr::pivot_longer(cols = c(intercept, linear, quad),
                                              names_to = "param",
                                              values_to = "est")
    }

    # calculate the upper and lower bounds for outliers
    boxplot_stats <- tapply(data_box$est, data_box$param, grDevices::boxplot.stats)
    outlier_vals <- lapply(boxplot_stats, `[[`, "out")

    # Initialize outlier column
    data_box$outlier <- NA

    # Loop over the parameters and identify outliers
    for(i in unique(data_box$param)) {
      pick_outliers <- data_box$param == i & data_box$est %in% outlier_vals[[i]] &
        is.na(data_box$outlier)

      data_box$outlier[pick_outliers] <- data_box[[idvarname]][pick_outliers]
    }

    # Convert outlier column to character and change missing to ""
    data_box$outlier <- as.character(data_box$outlier)
    data_box$outlier <- ifelse(is.na(data_box$outlier), "", data_box$outlier)

    #Create box plot with mean plotted and outliers labeled by idvarname
    data_box <- data_box |>
      ggplot2::ggplot(ggplot2::aes(x = param, y = est)) +
      ggplot2::geom_boxplot() +
      ggplot2::stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "blue") + # Add point for mean
      ggplot2::geom_text(ggplot2::aes(label = outlier), hjust = -0.1) +
      ggplot2::ylab("OLS Estimates") +
      ggplot2::xlab("") +
      ggplot2::ggtitle("Box Plots of OLS Estimates")
  }


  # Return a list containing estimated values and the plots
  result <- list(out_data = out_data,
                 group_plots = group_plots,
                 individual_plots = individual_plots,
                 histogram_plots = histogram_plots,
                 box_plot = data_box,
                 models = models)

  return(result)
}
