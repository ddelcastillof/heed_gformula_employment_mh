run_mice <- function(wide_data, m = 5, 
                     maxit = 10, 
                     seed = 20260522,
                     outcome = c("MCS", "PCS")) {
  outcome <- match.arg(outcome)
  method_list <- mice::make.method(wide_data)

  # Method assignment by variable group
  method_by_group <- c(
    # time-invariant baseline confounders (columns end in _base)
    sex_dv               = "logreg",
    hiqual_dv_fact       = "pmm",
    race                 = "logreg",
    age_dv               = "norm",
    # time-varying confounders (NOT lagged), also time-invariant
    gor_dv_fact          = "pmm",
    # SF-12 scores: baseline (_base) and outcome
    sf12mcs_dv           = "pmm",
    sf12pcs_dv           = "pmm",
    # mediators / exposure
    econ_dist_bin_fact   = "logreg",
    log_income           = "pmm",
    econ_emp_bin_fact    = "logreg",   # exposure
    # lagged confounders mcs and pcs
    pcs_lagged           = "pmm",
    mcs_lagged           = "pmm",
    # other lagged confounders
    dnc_fact_lagged      = "pmm",
    home_owner_lagged    = "logreg",
    econ_benefits_lagged = "logreg",
    mastat_dv_lagged     = "logreg"
  )

  group   <- sub("_(\\d+|base)$", "", names(method_list))
  matched <- group %in% names(method_by_group)
  method_list[matched] <- method_by_group[group[matched]]

  # Congenial multiple imputation (effect modification)
  # Introducing this assumption into the imputation model
  y_cols <- c(attr(wide_data, "outcome_baseline"),
              attr(wide_data, "outcome_vars"))
  # every treatment wave, tagged by set_exposure() and already ordered by wave,
  # so the modifier terms scale with the wave-set instead of pinning wave 0
  a_cols <- attr(wide_data, "exposure_vars")

  # the passive age_dv/gor_dv_fact machinery below works on any wide frame, so a
  # frame without these attributes still imputes -- it just gets no interactions
  congenial <- length(y_cols) > 0L && length(a_cols) > 0L

  if (!congenial) {
    warning("run_mice: wide_data lacks the make_wide()/set_exposure() attributes ",
            "('outcome_baseline'/'outcome_vars'/'exposure_vars'), so the outcome ",
            "models keep their default formulas and carry no exposure ",
            "interactions. Pass the data element of the build_data() list, ",
            "not a subset of it")
  } else {
    stopifnot(all(c(y_cols, a_cols) %in% names(wide_data)))
  }

  S <- "sex_dv_base * race_base * hiqual_dv_fact_base"

  # gor_dv_fact has low variability in the sample. 
  # So a LOCF approach via passive imputation will be used for missing intermediate waves 
  # (respondents that return in posterior waves).

  # every passively imputed column, mapped to the column it is derived from, so the
  # predictor matrix can be repaired below
  passive_src <- character(0)

  gor_cols <- grep("^gor_dv_fact_\\d+$", names(method_list), value = TRUE)
  gor_cols <- gor_cols[order(as.integer(sub("^gor_dv_fact_", "", gor_cols)))]

  if (length(gor_cols) > 2L) {
    interior <- seq.int(2L, length(gor_cols) - 1L)
    if (anyNA(wide_data[[gor_cols[1L]]])) {
      warning("run_mice: ", gor_cols[1L], " has missing values, so the interior ",
              "gor_dv_fact waves keep their '", method_by_group[["gor_dv_fact"]], "' method")
    } else {
      method_list[gor_cols[interior]] <- paste0("~ I(", gor_cols[interior - 1L], ")")
      passive_src <- c(passive_src,
                       stats::setNames(gor_cols[interior - 1L], gor_cols[interior]))
    }
  }

  # age_dv_sq is a deterministic transform of age_dv: (age_dv - centre)^2, centred on
  # the person-wave mean in import_and_clean(). Imputed as a free variable the square
  # drifts away from the age it is supposed to encode, so it is derived passively.
  # The centring constant was computed on the long data and cannot be recomputed here,
  # so it is recovered from the observed rows: age^2 - age_sq = 2*centre*age - centre^2
  # is linear in age with slope 2*centre.

  sq_cols  <- grep("^age_dv_sq(_\\d+|_base)$", names(method_list), value = TRUE)
  age_cols <- sub("^age_dv_sq", "age_dv", sq_cols)
  has_age  <- age_cols %in% names(wide_data)

  if (any(!has_age)) {
    warning("run_mice: no age_dv counterpart for ", toString(sq_cols[!has_age]),
            ", so those columns keep their '",
            toString(unique(method_list[sq_cols[!has_age]])), "' method")
  }
  sq_cols  <- sq_cols[has_age]
  age_cols <- age_cols[has_age]

  if (length(sq_cols)) {
    keep_method <- toString(unique(method_list[sq_cols]))

    ok     <- !is.na(wide_data[[age_cols[1L]]]) & !is.na(wide_data[[sq_cols[1L]]])
    a      <- as.numeric(wide_data[[age_cols[1L]]][ok])
    s      <- as.numeric(wide_data[[sq_cols[1L]]][ok])
    centre <- if (length(unique(a)) > 1L) {
      unname(stats::coef(stats::lm(I(a^2 - s) ~ a))[[2L]]) / 2
    } else {
      NA_real_
    }

    if (is.na(centre) || !isTRUE(all.equal((a - centre)^2, s))) {
      warning("run_mice: could not recover the age_dv centring constant from ",
              age_cols[1L], "/", sq_cols[1L], ", so ", toString(sq_cols),
              " keep their '", keep_method, "' method")
    } else if (any(match(sq_cols, names(method_list)) < match(age_cols, names(method_list)))) {
      # mice visits columns in order, so a passive square placed before its age
      # column would read the previous iteration's value (wrong outright at maxit = 1)
      warning("run_mice: ", toString(sq_cols[match(sq_cols, names(method_list)) <
                                             match(age_cols, names(method_list))]),
              " precede their age_dv counterpart, so they keep their '",
              keep_method, "' method")
    } else {
      method_list[sq_cols] <- paste0("~ I((", age_cols, " - ",
                                     format(centre, digits = 15), ")^2)")
      passive_src <- c(passive_src, stats::setNames(age_cols, sq_cols))
    }
  }

  pred_mat <- mice::make.predictorMatrix(wide_data)
  if (length(passive_src)) {
    pred_mat[names(passive_src), ] <- 0
    pred_mat[cbind(unname(passive_src), names(passive_src))] <- 0
  }

  collinear <- grep("^(age_dv|gor_dv_fact|age_dv_sq)(_\\d+|_base)$", colnames(pred_mat), value = TRUE)
  pred_mat[, collinear] <- 0

  # make.formulas reads pred_mat, so it has to run after the passive/collinear
  # repairs above -- built earlier it would carry the arrows those edits removed
  form_list <- mice::make.formulas(wide_data, predictorMatrix = pred_mat)

  # Y model: one formula per outcome wave, not one for the whole vector.
  # `y_cols` holds the baseline plus every wave, so `form_list[[y_cols]]` would
  # index recursively (form_list[["..._base"]][["..._0"]]) and error.
  #
  # The baseline outcome is pre-exposure, so it gets no treatment terms. Outcome
  # wave t gets the treatment waves up to and including t: that is the exposure
  # history the second-stage gFormulaMI model sees for that node, so the
  # imputation model and the analysis model carry the same interactions.
  if (congenial) {
    wave_of  <- function(x) as.integer(sub("^.*_(\\d+)$", "\\1", x))
    out_cols <- attr(wide_data, "outcome_vars")
    a_waves  <- wave_of(a_cols)

    for (y in out_cols) {
      prior <- a_cols[a_waves <= wave_of(y)]
      if (!length(prior)) next
      form_list[[y]] <- stats::update.formula(
        form_list[[y]], stats::reformulate(c(".", paste(prior, "*", S)))
      )
    }
  }

  mids <-  mice::mice(
           data            = wide_data,
           m               = m,
           maxit           = maxit,
           seed            = seed,
           method          = method_list,
           predictorMatrix = pred_mat
          )

  le <- mids$loggedEvents
  if (is.null(le) || nrow(le) == 0) {
    message("run_mice: no logged events.")
  } else {
    message("run_mice: ", nrow(le), " logged event(s) during imputation:")
    message(paste(utils::capture.output(print(le)), collapse = "\n"))
  }

  mids
}
