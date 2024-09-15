# Copyright (c) Meta Platforms, Inc. and its affiliates.

# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#  Thisis a temp file to modify the allocator by txtodor
#  1) fix denominator for max_allocation:  to have a denominator of 365/7 instead of na.mean

####################################################################
#' Budget Allocator
#'
#' \code{robyn_allocator()} function returns a new split of media
#' variable spends that maximizes the total media response.
#'
#' @inheritParams robyn_run
#' @inheritParams robyn_outputs
#' @param robyn_object Character or List. Path of the \code{Robyn.RDS} object
#' that contains all previous modeling information or the imported list.
#' @param select_build Integer. Default to the latest model build. \code{select_build = 0}
#' selects the initial model. \code{select_build = 1} selects the first refresh model.
#' @param InputCollect List. Contains all input parameters for the model.
#' Required when \code{robyn_object} is not provided.
#' @param OutputCollect List. Containing all model result.
#' Required when \code{robyn_object} is not provided.
#' @param select_model Character. A model \code{SolID}. When \code{robyn_object}
#' is provided, \code{select_model} defaults to the already selected \code{SolID}. When
#' \code{robyn_object} is not provided, \code{select_model} must be provided with
#' \code{InputCollect} and \code{OutputCollect}, and must be one of
#' \code{OutputCollect$allSolutions}.
#' @param optim_algo Character. Default to \code{"SLSQP_AUGLAG"}, short for "Sequential Least-Squares
#' Quadratic Programming" and "Augmented Lagrangian". Alternatively, "\code{"MMA_AUGLAG"},
#' short for "Methods of Moving Asymptotes". More details see the documentation of
#' NLopt \href{https://nlopt.readthedocs.io/en/latest/NLopt_Algorithms/}{here}.
#' @param scenario Character. Accepted options are: \code{"max_historical_response"} or
#' \code{"max_response_expected_spend"}. \code{"max_historical_response"} simulates the scenario
#' "what's the optimal media spend allocation given the same average spend level in history?",
#' while \code{"max_response_expected_spend"} simulates the scenario "what's the optimal media
#' spend allocation of a given future spend level for a given period?"
#' @param expected_spend Numeric. The expected future spend volume. Only applies when
#' \code{scenario = "max_response_expected_spend"}.
#' @param expected_spend_days Integer. The duration of the future spend volume in
#' \code{expected_spend}. Only applies when \code{scenario = "max_response_expected_spend"}.
#' @param channel_constr_low,channel_constr_up Numeric vectors. The lower and upper bounds
#' for each paid media variable when maximizing total media response. For example,
#' \code{channel_constr_low = 0.7} means minimum spend of the variable is 70% of historical
#' average, using non-zero spend values, within \code{date_min} and \code{date_max} date range.
#' Both constrains must be length 1 (same for all values) OR same length and order as
#' \code{paid_media_spends}. It's not recommended to 'exaggerate' upper bounds, especially
#' if the new level is way higher than historical level. Lower bound must be >=0.01,
#' and upper bound should be < 5.
#' @param maxeval Integer. The maximum iteration of the global optimization algorithm.
#' Defaults to 100000.
#' @param constr_mode Character. Options are \code{"eq"} or \code{"ineq"},
#' indicating constraints with equality or inequality.
#' @param date_min,date_max Character/Date. Date range to calculate mean (of non-zero
#' spends) and total spends. Default will consider all dates within modeled window.
#' Length must be 1 for both parameters.
#' @return A list object containing allocator result.
#' @examples
#' \dontrun{
#' # Having InputCollect and OutputCollect results
#' # Set your exported model location
#' robyn_object <- "~/Desktop/MyRobyn.RDS"
#'
#' # Check media summary for selected model from the simulated data
#' select_model <- "3_10_3"
#' OutputCollect$xDecompAgg[
#'   solID == select_model & !is.na(mean_spend),
#'   .(rn, coef, mean_spend, mean_response, roi_mean,
#'     total_spend,
#'     total_response = xDecompAgg, roi_total, solID
#'   )
#' ]
#'
#' # Run allocator with 'InputCollect' and 'OutputCollect'
#' # with 'scenario = "max_historical_response"'
#' AllocatorCollect <- robyn_allocator(
#'   InputCollect = InputCollect,
#'   OutputCollect = OutputCollect,
#'   select_model = select_model,
#'   scenario = "max_historical_response",
#'   channel_constr_low = c(0.7, 0.7, 0.7, 0.7, 0.7),
#'   channel_constr_up = c(1.2, 1.5, 1.5, 1.5, 1.5)
#' )
#'
#' # Run allocator with a 'robyn_object' from the second model refresh
#' # with 'scenario = "max_response_expected_spend"'
#' AllocatorCollect <- robyn_allocator(
#'   robyn_object = robyn_object,
#'   select_build = 2,
#'   scenario = "max_response_expected_spend",
#'   channel_constr_low = c(0.7, 0.7, 0.7, 0.7, 0.7),
#'   channel_constr_up = c(1.2, 1.5, 1.5, 1.5, 1.5),
#'   expected_spend = 100000,
#'   expected_spend_days = 90
#' )
#' }
#' @return List. Contains optimized allocation results and plots.
#' @export
#' #----------------------------------------------------------------
#' added by tgtod
#'#----------------------------------------------------------------
 
library(Robyn)
library(stringr)
#importFrom(stringr,str_extract)
library(lares)
#importFrom(lares,theme_lares)
library(patchwork)
#importFrom(patchwork,plot_annotation)

library(ggplot2)
library(dplyr)

robyn_allocator_temp <- function(robyn_object = NULL,    # chg to temp tgtod
                            select_build = 0,
                            InputCollect = NULL,
                            OutputCollect = NULL,
                            select_model = NULL,
                            json_file = NULL,
                            optim_algo = "SLSQP_AUGLAG",
                            scenario = "max_response_expected_spend",
                            expected_spend = NULL,
                            expected_spend_days = NULL,
                            channel_constr_low = 0.5,
                            channel_constr_up = 2,
                            maxeval = 100000,
                            constr_mode = "eq",
                            date_min = NULL,
                            date_max = NULL,
                            export = TRUE,
                            quiet = FALSE,
                            ui = FALSE,
                            ...)

{
  
  #####################################
  #### Set local environment
  
  ### Use previously exported model using json_file
  if (!is.null(json_file)) {
    if (is.null(InputCollect)) InputCollect <- robyn_inputs(json_file = json_file, ...)
    if (is.null(OutputCollect)) {
      OutputCollect <- robyn_run(
        json_file = json_file, plot_folder = robyn_object, ...
      )
    }
    if (is.null(select_model)) select_model <- OutputCollect$selectID
  }
  
  ## Collect inputs
  if (!is.null(robyn_object) & (is.null(InputCollect) & is.null(OutputCollect))) {
    if ("robyn_exported" %in% class(robyn_object)) {
      imported <- robyn_object
      robyn_object <- imported$robyn_object
    } else {
      imported <- robyn_load(robyn_object, select_build, quiet)
    }
    InputCollect <- imported$InputCollect
    OutputCollect <- imported$OutputCollect
    select_model <- imported$select_model
  } else if (any(is.null(InputCollect), is.null(OutputCollect), is.null(select_model))) {
    stop("When 'robyn_object' is not provided, then InputCollect, OutputCollect, select_model must be provided")
  }
  
  message(paste(">>> Running temp budget allocator for model ID", select_model, "..."))
  
  ## Set local data & params values
  if (TRUE) {
    dt_mod <- InputCollect$dt_mod
    paid_media_vars <- InputCollect$paid_media_vars
    paid_media_spends <- InputCollect$paid_media_spends
    startRW <- InputCollect$rollingWindowStartWhich
    endRW <- InputCollect$rollingWindowEndWhich
    adstock <- InputCollect$adstock
    media_order <- order(paid_media_spends)
    mediaVarSorted <- paid_media_vars[media_order]
    mediaSpendSorted <- paid_media_spends[media_order]
  }
  
  ## Check inputs and parameters
  check_allocator(
    OutputCollect, select_model, paid_media_spends, scenario,
    channel_constr_low, channel_constr_up,
    expected_spend, expected_spend_days, constr_mode
  )
  
  # Channels contrains
  # channel_constr_low <- rep(0.8, length(paid_media_spends))
  # channel_constr_up <- rep(1.2, length(paid_media_spends))
  if (length(channel_constr_low) == 1) {
    channel_constr_low <- rep(channel_constr_low, length(paid_media_spends))
  }
  if (length(channel_constr_up) == 1) {
    channel_constr_up <- rep(channel_constr_up, length(paid_media_spends))
  }
  names(channel_constr_low) <- paid_media_spends
  names(channel_constr_up) <- paid_media_spends
  channel_constr_low <- channel_constr_low[media_order]
  channel_constr_up <- channel_constr_up[media_order]
  
  # Hyper-parameters and results
  dt_hyppar <- filter(OutputCollect$resultHypParam, .data$solID == select_model)
  dt_bestCoef <- filter(OutputCollect$xDecompAgg, .data$solID == select_model, .data$rn %in% paid_media_spends)
  
  ## Sort table and get filter for channels mmm coef reduced to 0
  dt_coef <- select(dt_bestCoef, .data$rn, .data$coef)
  get_rn_order <- order(dt_bestCoef$rn)
  dt_coefSorted <- dt_coef[get_rn_order, ]
  dt_bestCoef <- dt_bestCoef[get_rn_order, ]
  coefSelectorSorted <- dt_coefSorted$coef > 0
  names(coefSelectorSorted) <- dt_coefSorted$rn
  
  ## Filter and sort all variables by name that is essential for the apply function later
  if (!all(coefSelectorSorted)) {
    chn_coef0 <- setdiff(names(coefSelectorSorted), mediaSpendSorted[coefSelectorSorted])
    message("Excluded in optimiser because their coefficients are 0: ", paste(chn_coef0, collapse = ", "))
  } else {
    chn_coef0 <- "None"
  }
  mediaSpendSortedFiltered <- mediaSpendSorted[coefSelectorSorted]
  dt_hyppar <- select(dt_hyppar, hyper_names(adstock, mediaSpendSortedFiltered)) %>%
    select(sort(colnames(.)))
  dt_bestCoef <- dt_bestCoef[dt_bestCoef$rn %in% mediaSpendSortedFiltered, ]
  channelConstrLowSorted <- channel_constr_low[mediaSpendSortedFiltered]
  channelConstrUpSorted <- channel_constr_up[mediaSpendSortedFiltered]
  
  ## Get adstock parameters for each channel
  getAdstockHypPar <- get_adstock_params(InputCollect, dt_hyppar)
  
  ## Get hill parameters for each channel
  hills <- get_hill_params(
    InputCollect, OutputCollect, dt_hyppar, dt_coef, mediaSpendSortedFiltered, select_model
  )
  alphas <- hills$alphas
  gammaTrans <- hills$gammaTrans
  coefsFiltered <- hills$coefsFiltered
  
  # Spend values based on date range set
  min_date <- as.Date(date_min)    # added tgt
  max_date <- as.Date(date_max)    # added tgt
  histDays <- as.numeric(max_date - min_date) # added tgt
  dt_optimCost <- slice(dt_mod, startRW:endRW)
  check_daterange(date_min, date_max, dt_optimCost$ds)
  if (is.null(date_min)) date_min <- min(dt_optimCost$ds)
  if (is.null(date_max)) date_max <- max(dt_optimCost$ds)
  if (date_min < min(dt_optimCost$ds)) date_min <- min(dt_optimCost$ds)
  if (date_max > max(dt_optimCost$ds)) date_max <- max(dt_optimCost$ds)
  histFiltered <- filter(dt_optimCost, .data$ds >= date_min & .data$ds <= date_max)
  nPeriod <- nrow(histFiltered)
  message(sprintf("Date Window: %s:%s (%s %ss)", date_min, date_max, nPeriod, InputCollect$intervalType))
  
  histSpendB <- select(histFiltered, any_of(mediaSpendSortedFiltered))
  histSpendC <- select(histFiltered, any_of(mediaSpendSortedFiltered))    # added tgt
  histSpendTotal <- sum(histSpendB)
  histSpendTotalC <- sum(histSpendC) # added TGT
  histSpend <- unlist(summarise_all(select(histFiltered, any_of(mediaSpendSortedFiltered)), sum))
  histSpendUnit <- unlist(summarise_all(histSpendB, function(x) sum(x) / sum(x > 0)))
  histSpendUnit[is.nan(histSpendUnit)] <- 0
  histSpendUnitTotal <- sum(histSpendUnit, na.rm = TRUE)
  histSpendShare <- histSpendUnit / histSpendUnitTotal
  # ADDED tgt
  histSpendC <- unlist(summarise_all(select(histFiltered, any_of(mediaSpendSortedFiltered)), sum))
  histSpendUnitC <- unlist(summarise_all(histSpendB, function(x) sum(x) /(histDays / InputCollect$dayInterval)))
  histSpendUnitC[is.nan(histSpendUnit)] <- 0
  histSpendUnitTotalC <- sum(histSpendUnit, na.rm = TRUE)
  histSpendShareC <- histSpendUnitC / histSpendUnitTotalC
  # end of added tgt
  
  
  # Response values based on date range -> mean spend   #tgt  changing response values based on number of days
  noSpendMedia <- histResponseUnitModelC <- NULL   # tgt added C to histResponseUnitModelC 9/8
  for (i in seq_along(mediaSpendSortedFiltered)) {
    if (histSpendUnitC[i] > 0) {
      val <- robyn_response(
        json_file = json_file,
        robyn_object = robyn_object,
        select_build = select_build,
        media_metric = mediaSpendSortedFiltered[i],
        select_model = select_model,
        metric_value = histSpendUnitC[i],
        dt_hyppar = OutputCollect$resultHypParam,
        dt_coef = OutputCollect$xDecompAgg,
        InputCollect = InputCollect,
        OutputCollect = OutputCollect,
        quiet = quiet
      )$response
    } else {
      val <- 0
      noSpendMedia <- c(noSpendMedia, mediaSpendSortedFiltered[i])
    }
    histResponseUnitModelC <- c(histResponseUnitModelC, val)
  }
  names(histResponseUnitModelC) <- mediaSpendSortedFiltered
  if (!is.null(noSpendMedia) & !quiet) {
    message("Media variables with 0 spending during this date window: ", v2t(noSpendMedia))
  }
  
  ## Build constraints function with scenarios
  if ("max_historical_response" %in% scenario) {
    expected_spend <- histSpendTotal
    expSpendUnitTotal <- histSpendUnitTotal
  } else {
  ##  added in correct denominator changing from mean of non-zeroes to 365/7   - tgtod002  
    
    min_date <- as.Date(date_min)
    max_date <- as.Date(date_max)
    histDays <- as.numeric(max_date - min_date)
    histSpendC <- select(histFiltered, any_of(mediaSpendSortedFiltered))
    histSpendTotalC <- sum(histSpendC)
    histSpendUnitC <- unlist(summarise_all(histSpendC, function(x) sum(x) / (histDays / InputCollect$dayInterval)))
    histSpendUnitC[is.nan(histSpendUnitC)] <- 0
    histSpendUnitTotalC <- sum(histSpendUnitC, na.rm = TRUE)
    histSpendShareC <- histSpendUnitC / histSpendUnitTotalC
    ##  end of added in correct denominator changing from mean of non-zeroes to 365/7   - tgtod002  
    
    expSpendUnitTotal <- expected_spend / (expected_spend_days / InputCollect$dayInterval)
  }
  
  # Gather all values that will be used internally on optim (nloptr)
  eval_list <- list(
    coefsFiltered = coefsFiltered,
    alphas = alphas,
    gammaTrans = gammaTrans,
    mediaSpendSortedFiltered = mediaSpendSortedFiltered,
    expSpendUnitTotal = expSpendUnitTotal
  )
  # So we can implicitly use these values within eval_f()
  options("ROBYN_TEMP" = eval_list)
  
  # eval_f(c(1,1))
  # $objective
  # [1] -0.02318446
  # $gradient
  # [1] -1.923670e-06 -8.148831e-06 -3.163465e-02 -3.553371e-05
  # $objective.channel
  # [1] -6.590166e-07 -3.087475e-06 -2.316821e-02 -1.250144e-05
  
  ## Set initial values and bounds
  x0 <- lb <- histSpendUnit * channelConstrLowSorted       #lowerbound
  x1 <- lb1 <- histSpendUnitC * channelConstrLowSorted   # tgt - lowerbound using spend/106 (not non-zero mean)
  ub <- histSpendUnit * channelConstrUpSorted              #upperbound
  ub1 <- histSpendUnitC * channelConstrUpSorted              #upperbound
  ## Set optim options
  if (optim_algo == "MMA_AUGLAG") {
    local_opts <- list(
      "algorithm" = "NLOPT_LD_MMA",
      "xtol_rel" = 1.0e-10
    )
  } else if (optim_algo == "SLSQP_AUGLAG") {
    local_opts <- list(
      "algorithm" = "NLOPT_LD_SLSQP",
      "xtol_rel" = 1.0e-10
    )
  }
  
  ## Run optim
  nlsMod <- nloptr::nloptr(
    x0 = x1,
    eval_f = eval_f,
    eval_g_eq = if (constr_mode == "eq") eval_g_eq else NULL,
    eval_g_ineq = if (constr_mode == "ineq") eval_g_ineq else NULL,
    lb = lb1, ub = ub1,
    opts = list(
      "algorithm" = "NLOPT_LD_AUGLAG",
      "xtol_rel" = 1.0e-10,
      "maxeval" = maxeval,
      "local_opts" = local_opts
    )
  )
  
  ## Collect output
  dt_optimOut <- data.frame(
    solID = select_model,
    dep_var_type = InputCollect$dep_var_type,
    channels = mediaSpendSortedFiltered,
    date_min = date_min,
    date_max = date_max,
    periods = sprintf("%s %ss", nPeriod, InputCollect$intervalType),
    constr_low = channelConstrLowSorted,
    constr_up = channelConstrUpSorted,
    # Initial
    histSpend = histSpend,
    histSpendTotal = histSpendTotal,
    initSpendUnitTotal = histSpendUnitTotalC,
    initSpendUnit = histSpendUnitC,
    initSpendShare = histSpendShareC,
    initResponseUnit = histResponseUnitModelC,
    initResponseUnitTotal = sum(histResponseUnitModelC),
    initRoiUnit = histResponseUnitModelC / histSpendUnitC,
    # Expected
    expSpendTotal = expected_spend,
    expSpendUnitTotal = expSpendUnitTotal,
    expSpendUnitDelta = expSpendUnitTotal / histSpendUnitTotalC - 1,
    # Optimized
    optmSpendUnit = nlsMod$solution,
    optmSpendUnitDelta = (nlsMod$solution / histSpendUnitC - 1),
    optmSpendUnitTotal = sum(nlsMod$solution),
    optmSpendUnitTotalDelta = sum(nlsMod$solution) / histSpendUnitTotalC - 1,
    optmSpendShareUnit = nlsMod$solution / sum(nlsMod$solution),
    optmResponseUnit = -eval_f(nlsMod$solution)[["objective.channel"]],
    optmResponseUnitTotal = sum(-eval_f(nlsMod$solution)[["objective.channel"]]),
    optmRoiUnit = -eval_f(nlsMod$solution)[["objective.channel"]] / nlsMod$solution,
    optmResponseUnitLift = (-eval_f(nlsMod$solution)[["objective.channel"]] / histResponseUnitModelC) - 1
  ) %>%
    mutate(optmResponseUnitTotalLift = (.data$optmResponseUnitTotal / .data$initResponseUnitTotal) - 1)
  .Options$ROBYN_TEMP <- NULL # Clean auxiliary method
  
  ## Plot allocator results
  plots <- allocation_plots(InputCollect, OutputCollect, dt_optimOut, select_model, scenario, export, quiet)
  
  ## Export results into CSV
  if (export) {
    export_dt_optimOut <- dt_optimOut
    if (InputCollect$dep_var_type == "conversion") {
      colnames(export_dt_optimOut) <- gsub("Roi", "CPA", colnames(export_dt_optimOut))
    }
    write.csv(export_dt_optimOut, paste0(OutputCollect$plot_folder, select_model, "_reallocated.csv"))
  }
  
  output <- list(
    dt_optimOut = dt_optimOut,
    nlsMod = nlsMod,
    plots = plots,
    scenario = scenario,
    expected_spend = expected_spend,
    expected_spend_days = expected_spend_days,
    skipped = chn_coef0,
    no_spend = noSpendMedia,
    ui = if (ui) plots else NULL
  )
  
  class(output) <- c("robyn_allocator", class(output))
  return(output)
}

#' @rdname robyn_allocator
#' @aliases robyn_allocator
#' @param x \code{robyn_allocator()} output.
#' @export
print.robyn_allocator <- function(x, ...) {
  temp <- x$dt_optimOut[!is.nan(x$dt_optimOut$optmRoiUnit), ]
  print(glued(
    "
Model ID: {x$dt_optimOut$solID[1]}
Scenario: {scenario}
Dep. Variable Type: {temp$dep_var_type[1]}
Media Skipped (coef = 0): {paste0(x$skipped, collapse = ',')} {no_spend}
Relative Spend Increase: {spend_increase_p}% ({spend_increase}{scenario_plus})
Total Response Increase (Optimized): {signif(100 * x$dt_optimOut$optmResponseUnitTotalLift[1], 3)}%
Window: {x$dt_optimOut$date_min[1]}:{x$dt_optimOut$date_max[1]} ({x$dt_optimOut$periods[1]})
Allocation Summary:
  {summary}
",
    scenario = ifelse(
      x$scenario == "max_historical_response",
      "Maximum Historical Response",
      "Maximum Response with Expected Spend"
    ),
    no_spend = ifelse(!is.null(x$no_spend), paste("| (spend = 0):", v2t(x$no_spend, quotes = FALSE)), ""),
    spend_increase_p = signif(100 * x$dt_optimOut$expSpendUnitDelta[1], 3),
    spend_increase = formatNum(
      sum(x$dt_optimOut$optmSpendUnitTotal) - sum(x$dt_optimOut$initSpendUnitTotal),
      abbr = TRUE, sign = TRUE
    ),
    scenario_plus = ifelse(
      x$scenario == "max_response_expected_spend",
      sprintf(" in %s days", x$expected_spend_days), ""
    ),
    summary = paste(sprintf(
      "
- %s:
  Optimizable Range (bounds): [%s%%, %s%%]
  Mean Spend Share (avg): %s%% -> Optimized = %s%%
  Mean Response: %s -> Optimized = %s
  Mean Spend (per time unit): %s -> Optimized = %s [Delta = %s%%]",
      temp$channels,
      100 * temp$constr_low - 100,
      100 * temp$constr_up - 100,
      signif(100 * temp$initSpendShare, 3),
      signif(100 * temp$optmSpendShareUnit, 3),
      formatNum(temp$initResponseUnit, 0),
      formatNum(temp$optmResponseUnit, 0),
      formatNum(temp$initSpendUnit, 3, abbr = TRUE),
      formatNum(temp$optmSpendUnit, 3, abbr = TRUE),
      formatNum(100 * temp$optmSpendUnitDelta, signif = 2)
    ), collapse = "\n  ")
  ))
}

#' @rdname robyn_allocator
#' @aliases robyn_allocator
#' @param x \code{robyn_allocator()} output.
#' @export
plot.robyn_allocator <- function(x, ...) plot(x$plots$plots, ...)

eval_f <- function(X) {
  
  # eval_list <- get("eval_list", pos = as.environment(-1))
  eval_list <- getOption("ROBYN_TEMP")
  # mm_lm_coefs <- eval_list[["mm_lm_coefs"]]
  coefsFiltered <- eval_list[["coefsFiltered"]]
  alphas <- eval_list[["alphas"]]
  gammaTrans <- eval_list[["gammaTrans"]]
  mediaSpendSortedFiltered <- eval_list[["mediaSpendSortedFiltered"]]
  # exposure_selectorSortedFiltered <- eval_list[["exposure_selectorSortedFiltered"]]
  # vmaxVec <- eval_list[["vmaxVec"]]
  # kmVec <- eval_list[["kmVec"]]
  
  fx_objective <- function(x, coeff, alpha, gammaTran
                           # , chnName, vmax, km, criteria
  ) {
    # Apply Michaelis Menten model to scale spend to exposure
    # if (criteria) {
    #   xScaled <- mic_men(x = x, Vmax = vmax, Km = km) # vmax * x / (km + x)
    # } else if (chnName %in% names(mm_lm_coefs)) {
    #   xScaled <- x * mm_lm_coefs[chnName]
    # } else {
    #   xScaled <- x
    # }
    
    # Adstock scales
    xAdstocked <- x
    # Hill transformation
    xOut <- coeff * sum((1 + gammaTran**alpha / xAdstocked**alpha)**-1)
    xOut
    return(xOut)
  }
  
  objective <- -sum(mapply(
    fx_objective,
    x = X,
    coeff = coefsFiltered,
    alpha = alphas,
    gammaTran = gammaTrans,
    # chnName = mediaSpendSortedFiltered,
    # vmax = vmaxVec,
    # km = kmVec,
    # criteria = exposure_selectorSortedFiltered,
    SIMPLIFY = TRUE
  ))
  
  # https://www.derivative-calculator.net/ on the objective function 1/(1+gamma^alpha / x^alpha)
  fx_gradient <- function(x, coeff, alpha, gammaTran
                          # , chnName, vmax, km, criteria
  ) {
    # Apply Michaelis Menten model to scale spend to exposure
    # if (criteria) {
    #   xScaled <- mic_men(x = x, Vmax = vmax, Km = km) # vmax * x / (km + x)
    # } else if (chnName %in% names(mm_lm_coefs)) {
    #   xScaled <- x * mm_lm_coefs[chnName]
    # } else {
    #   xScaled <- x
    # }
    
    # Adstock scales
    xAdstocked <- x
    xOut <- -coeff * sum((alpha * (gammaTran**alpha) * (xAdstocked**(alpha - 1))) / (xAdstocked**alpha + gammaTran**alpha)**2)
    return(xOut)
  }
  
  gradient <- c(mapply(
    fx_gradient,
    x = X,
    coeff = coefsFiltered,
    alpha = alphas,
    gammaTran = gammaTrans,
    # chnName = mediaSpendSortedFiltered,
    # vmax = vmaxVec,
    # km = kmVec,
    # criteria = exposure_selectorSortedFiltered,
    SIMPLIFY = TRUE
  ))
  
  fx_objective.chanel <- function(x, coeff, alpha, gammaTran
                                  # , chnName, vmax, km, criteria
  ) {
    # Apply Michaelis Menten model to scale spend to exposure
    # if (criteria) {
    #   xScaled <- mic_men(x = x, Vmax = vmax, Km = km) # vmax * x / (km + x)
    # } else if (chnName %in% names(mm_lm_coefs)) {
    #   xScaled <- x * mm_lm_coefs[chnName]
    # } else {
    #   xScaled <- x
    # }
    
    # Adstock scales
    xAdstocked <- x
    xOut <- -coeff * sum((1 + gammaTran**alpha / xAdstocked**alpha)**-1)
    return(xOut)
  }
  
  objective.channel <- mapply(
    fx_objective.chanel,
    x = X,
    coeff = coefsFiltered,
    alpha = alphas,
    gammaTran = gammaTrans,
    # chnName = mediaSpendSortedFiltered,
    # vmax = vmaxVec,
    # km = kmVec,
    # criteria = exposure_selectorSortedFiltered,
    SIMPLIFY = TRUE
  )
  
  optm <- list(objective = objective, gradient = gradient, objective.channel = objective.channel)
  return(optm)
}

eval_g_eq <- function(X) {
  eval_list <- getOption("ROBYN_TEMP")
  constr <- sum(X) - eval_list$expSpendUnitTotal
  grad <- rep(1, length(X))
  return(list(
    "constraints" = constr,
    "jacobian" = grad
  ))
}

eval_g_ineq <- function(X) {
  eval_list <- getOption("ROBYN_TEMP")
  constr <- sum(X) - eval_list$expSpendUnitTotal
  grad <- rep(1, length(X))
  return(list(
    "constraints" = constr,
    "jacobian" = grad
  ))
}

get_adstock_params <- function(InputCollect, dt_hyppar) {
  if (InputCollect$adstock == "geometric") {
    getAdstockHypPar <- unlist(select(dt_hyppar, na.omit(str_extract(names(dt_hyppar), ".*_thetas"))))
  } else if (InputCollect$adstock %in% c("weibull_cdf", "weibull_pdf")) {
    getAdstockHypPar <- unlist(select(dt_hyppar, na.omit(str_extract(names(dt_hyppar), ".*_shapes|.*_scales"))))
  }
  return(getAdstockHypPar)
}

get_hill_params <- function(InputCollect, OutputCollect, dt_hyppar, dt_coef, mediaSpendSortedFiltered, select_model) {
  hillHypParVec <- unlist(select(dt_hyppar, na.omit(str_extract(names(dt_hyppar), ".*_alphas|.*_gammas"))))
  alphas <- hillHypParVec[str_which(names(hillHypParVec), "_alphas")]
  gammas <- hillHypParVec[str_which(names(hillHypParVec), "_gammas")]
  startRW <- InputCollect$rollingWindowStartWhich
  endRW <- InputCollect$rollingWindowEndWhich
  chnAdstocked <- filter(
    OutputCollect$mediaVecCollect,
    .data$type == "adstockedMedia",
    .data$solID == select_model
  ) %>%
    select(all_of(mediaSpendSortedFiltered)) %>%
    slice(startRW:endRW)
  gammaTrans <- mapply(function(gamma, x) {
    round(quantile(seq(range(x)[1], range(x)[2], length.out = 100), gamma), 4)
  }, gamma = gammas, x = chnAdstocked)
  names(gammaTrans) <- names(gammas)
  coefs <- dt_coef$coef
  names(coefs) <- dt_coef$rn
  coefsFiltered <- coefs[mediaSpendSortedFiltered]
  return(list(
    alphas = alphas,
    gammaTrans = gammaTrans,
    coefsFiltered = coefsFiltered
  ))
}

#--------------------------------------------------------------------------------
#check_allocator
#--------------------------------------------------------------------------------
check_allocator <- function(OutputCollect, select_model, paid_media_spends, scenario,
                            channel_constr_low, channel_constr_up,
                            expected_spend, expected_spend_days, constr_mode) {
  dt_hyppar <- OutputCollect$resultHypParam[OutputCollect$resultHypParam$solID == select_model, ]
  if (!(select_model %in% OutputCollect$allSolutions)) {
    stop(
      "Provided 'select_model' is not within the best results. Try any of: ",
      paste(OutputCollect$allSolutions, collapse = ", ")
    )
  }
  if (any(channel_constr_low < 0)) {
    stop("Inputs 'channel_constr_low' must be >= 0")
  }
  if (any(channel_constr_up < channel_constr_low)) {
    stop("Inputs 'channel_constr_up' must be >= 'channel_constr_low'")
  }
  if (any(channel_constr_up > 5)) {
    warning("Inputs 'channel_constr_up' > 5 might cause unrealistic allocation")
  }
  opts <- c("max_historical_response", "max_response_expected_spend")
  if (!(scenario %in% opts)) {
    stop("Input 'scenario' must be one of: ", paste(opts, collapse = ", "))
  }
  
  if (length(channel_constr_low) != 1 & length(channel_constr_low) != length(paid_media_spends)) {
    stop(paste(
      "Input 'channel_constr_low' have to contain either only 1",
      "value or have same length as 'InputCollect$paid_media_spends':", length(paid_media_spends)
    ))
  }
  if (length(channel_constr_up) != 1 & length(channel_constr_up) != length(paid_media_spends)) {
    stop(paste(
      "Input 'channel_constr_up' have to contain either only 1",
      "value or have same length as 'InputCollect$paid_media_spends':", length(paid_media_spends)
    ))
  }
  
  if ("max_response_expected_spend" %in% scenario) {
    if (any(is.null(expected_spend), is.null(expected_spend_days))) {
      stop("When scenario = 'max_response_expected_spend', expected_spend and expected_spend_days must be provided")
    }
  }
  opts <- c("eq", "ineq")
  if (!(constr_mode %in% opts)) {
    stop("Input 'constr_mode' must be one of: ", paste(opts, collapse = ", "))
  }
}

check_daterange <- function(date_min, date_max, dates) {
  if (!is.null(date_min)) {
    if (length(date_min) > 1) stop("Set a single date for 'date_min' parameter")
    if (date_min < min(dates)) {
      warning(sprintf(
        "Parameter 'date_min' not in your data's date range. Changed to '%s'", min(dates)
      ))
    }
  }
  if (!is.null(date_max)) {
    if (length(date_max) > 1) stop("Set a single date for 'date_max' parameter")
    if (date_max > max(dates)) {
      warning(sprintf(
        "Parameter 'date_max' not in your data's date range. Changed to '%s'", max(dates)
      ))
    }
  }
}


allocation_plots <- function(InputCollect, OutputCollect, dt_optimOut, select_model,
                             scenario, export = TRUE, quiet = FALSE) {
  outputs <- list()

  subtitle <- sprintf(
    paste0(
      "Total spend increase: %s%%",
      "\nTotal response increase: %s%% with optimised spend allocation"
    ),
    round(mean(dt_optimOut$optmSpendUnitTotalDelta) * 100, 1),
    round(mean(dt_optimOut$optmResponseUnitTotalLift) * 100, 1)
  )

  # Calculate errors for subtitles
  plotDT_scurveMeanResponse <- filter(
    OutputCollect$xDecompAgg,
    .data$solID == select_model,
    .data$rn %in% InputCollect$paid_media_spends
  )

  rsq_train_plot <- round(plotDT_scurveMeanResponse$rsq_train[1], 4)
  nrmse_plot <- round(plotDT_scurveMeanResponse$nrmse[1], 4)
  decomp_rssd_plot <- round(plotDT_scurveMeanResponse$decomp.rssd[1], 4)
  mape_lift_plot <- ifelse(!is.null(InputCollect$calibration_input),
                           round(plotDT_scurveMeanResponse$mape[1], 4), NA
  )
  errors <- paste0(
    "R2 train: ", rsq_train_plot,
    ", NRMSE = ", nrmse_plot,
    ", DECOMP.RSSD = ", decomp_rssd_plot,
    ifelse(!is.na(mape_lift_plot), paste0(", MAPE = ", mape_lift_plot), "")
  )

  # 1. Response comparison plot
  plotDT_resp <- select(dt_optimOut, .data$channels, .data$initResponseUnit, .data$optmResponseUnit) %>%
    mutate(channels = as.factor(.data$channels))
  names(plotDT_resp) <- c("channel", "Initial Mean Response", "Optimised Mean Response")
  plotDT_resp <- tidyr::gather(plotDT_resp, "variable", "response", -.data$channel)
  outputs[["p12"]] <- p12 <- ggplot(plotDT_resp, aes(
    y = reorder(.data$channel, -as.integer(.data$channel)),
    x = .data$response,
    fill = reorder(.data$variable, as.numeric(as.factor(.data$variable)))
  )) +
    geom_bar(stat = "identity", width = 0.5, position = position_dodge2(reverse = TRUE, padding = 0)) +
    scale_fill_brewer(palette = 3) +
    geom_text(aes(x = 0, label = formatNum(.data$response, 0), hjust = -0.1),
              position = position_dodge2(width = 0.5, reverse = TRUE), fontface = "bold", show.legend = FALSE
    ) +
    theme_lares(legend = "top") +
    scale_x_abbr() +
    labs(
      title = "Initial vs. Optimised Mean Response",
      subtitle = subtitle,
      fill = NULL, x = "Mean Response [#]", y = NULL
    )

  # 2. Budget share comparison plot
  plotDT_share <- select(dt_optimOut, .data$channels, .data$initSpendShare, .data$optmSpendShareUnit) %>%
    mutate(channels = as.factor(.data$channels))
  names(plotDT_share) <- c("channel", "Initial Avg. Spend Share", "Optimised Avg. Spend Share")
  plotDT_share <- tidyr::gather(plotDT_share, "variable", "spend_share", -.data$channel)
  outputs[["p13"]] <- p13 <- ggplot(plotDT_share, aes(
    y = reorder(.data$channel, -as.integer(.data$channel)),
    x = .data$spend_share, fill = .data$variable
  )) +
    geom_bar(stat = "identity", width = 0.5, position = position_dodge2(reverse = TRUE, padding = 0)) +
    scale_fill_brewer(palette = 3) +
    geom_text(aes(x = 0, label = formatNum(.data$spend_share * 100, 1, pos = "%"), hjust = -0.1),
              position = position_dodge2(width = 0.5, reverse = TRUE), fontface = "bold", show.legend = FALSE
    ) +
    theme_lares(legend = "top") +
    scale_x_percent() +
    labs(
      title = "Initial vs. Optimised Budget Allocation",
      subtitle = subtitle,
      fill = NULL, x = "Budget Allocation [%]", y = NULL
    )

  ## 3. Response curves
  plotDT_saturation <- OutputCollect$mediaVecCollect %>%
    filter(.data$solID == select_model, .data$type == "saturatedSpendReversed") %>%
    select(.data$ds, all_of(InputCollect$paid_media_spends)) %>%
    tidyr::gather("channel", "spend", -.data$ds)

  plotDT_decomp <- OutputCollect$mediaVecCollect %>%
    filter(.data$solID == select_model, .data$type == "decompMedia") %>%
    select(.data$ds, all_of(InputCollect$paid_media_spends)) %>%
    tidyr::gather("channel", "response", -.data$ds)

  plotDT_scurve <- data.frame(plotDT_saturation, response = plotDT_decomp$response) %>%
    filter(.data$spend >= 0) %>%
    as_tibble()

  dt_optimOutScurve <- rbind(
    select(dt_optimOut, .data$channels, .data$initSpendUnit, .data$initResponseUnit) %>% mutate(x = "Initial") %>% as.matrix(),
    select(dt_optimOut, .data$channels, .data$optmSpendUnit, .data$optmResponseUnit) %>% mutate(x = "Optimised") %>% as.matrix()
  ) %>% as.data.frame()
  colnames(dt_optimOutScurve) <- c("channels", "spend", "response", "type")
  dt_optimOutScurve <- dt_optimOutScurve %>%
    mutate(spend = as.numeric(.data$spend), response = as.numeric(.data$response)) %>%
    group_by(.data$channels) %>%
    mutate(
      spend_dif = dplyr::last(.data$spend) - dplyr::first(.data$spend),
      response_dif = dplyr::last(.data$response) - dplyr::first(.data$response)
    )

  trim_rate <- 1.6 # maybe enable as a parameter
  if (trim_rate > 0) {
    plotDT_scurve <- plotDT_scurve %>%
      filter(
        .data$spend < max(dt_optimOutScurve$spend) * trim_rate,
        .data$response < max(dt_optimOutScurve$response) * trim_rate
      )
  }
  outputs[["p14"]] <- p14 <- ggplot(data = plotDT_scurve, aes(
    x = .data$spend, y = .data$response, color = .data$channel
  )) +
    geom_line() +
    geom_point(data = dt_optimOutScurve, aes(
      x = .data$spend, y = .data$response,
      color = .data$channels, shape = .data$type
    ), size = 2.5) +
    # geom_text(
    #   data = dt_optimOutScurve, aes(
    #     x = .data$spend, y = .data$response, color = .data$channels,
    #     hjust = .data$hjust,
    #     label = formatNum(.data$spend, 2, abbr = TRUE)
    #   ),
    #   show.legend = FALSE
    # ) +
    theme_lares(legend.position = c(0.9, 0), pal = 2) +
    theme(
      legend.position = c(0.87, 0.5),
      legend.background = element_rect(fill = alpha("grey98", 0.6), color = "grey90"),
      legend.spacing.y = unit(0.2, "cm")
    ) +
    labs(
      title = "Response Curve and Mean* Spend by Channel",
      x = "Spend", y = "Response", shape = NULL, color = NULL,
      caption = sprintf(
        "*Based on date range: %s to %s (%s)",
        dt_optimOut$date_min[1],
        dt_optimOut$date_max[1],
        dt_optimOut$periods[1]
      )
    ) +
    scale_x_abbr() +
    scale_y_abbr()

  # Gather all plots into a single one
  p13 <- p13 + labs(subtitle = NULL)
  p12 <- p12 + labs(subtitle = NULL)
  outputs[["plots"]] <- plots <- ((p13 + p12) / p14) + plot_annotation(
    title = paste0("Budget Allocator Optimum Result for Model ID ", select_model),
    subtitle = subtitle,
    theme = theme_lares(background = "white")
  )

  # Gather all plots
  if (export) {
    scenario <- ifelse(scenario == "max_historical_response", "hist", "respo")
    filename <- paste0(OutputCollect$plot_folder, select_model, "_reallocated_", scenario, ".png")
    if (!quiet) message("Exporting charts into file: ", filename)
    ggsave(
      filename = filename,
      plot = plots, limitsize = FALSE,
      dpi = 350, width = 15, height = 12
    )
  }

  return(invisible(outputs))
}

