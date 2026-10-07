## effect_modification(): stratified g-formula for effect modification, as one function
## with three steps. Each step is its own target, so mice and gFormulaMI keep separate
## SLURM tiers and checkpoints:
##
##   split -> run_mice() -> gform -> tar_combine -> contrast
##
## Why stratify: an interaction term in the analysis lm of a pooled gFormulaMI run erases
## effect modification, because the node models are main-effects only. Splitting on a
## baseline modifier fits every node model, and the missing-data mice, inside the
## stratum, so imputation and analysis are congenial by construction. Strata are
## disjoint and imputed separately, so their estimates are independent: that is what
## makes the delta SE and Cochran's Q in "contrast" valid.
##
## step = "split": cut one stratum out of the wide data
##   wide_data  build_data()$data: a data.table carrying the make_wide()/set_exposure()
##              attributes
##   column     baseline modifier column, e.g. "sex_dv_base"
##   level      the level of `column` that defines the stratum, e.g. "Male"
##   Returns the rows with column == level as a data.table (rows with NA in `column` are
##   dropped). Baseline columns that are constant inside the stratum, always `column`
##   itself, are dropped too, and the make_wide()/set_exposure() attributes are
##   restamped without them.
##
## step = "gform": counterfactual imputation, node-model fits and synthetic pooling
##   stratum                the "split" output
##   mids                   run_mice(stratum); m must be at least 2, because pooling needs
##                          the between-imputation variance
##   intervention_pattern   build_data()$intervention_pattern
##   M, nSim                passed to gFormulaMI::gFormulaImpute(); M is rebound to
##                          mids$m
##   labels                 named list of length-1 values, e.g. list(outcome = "mcs",
##                          window = "four_w7_w10", modifier = "sex", stratum = "male");
##                          they become the leading columns of the result
##   Returns one tibble of 2 x length(intervention_pattern) rows: the `labels` columns,
##   stratum_n (integer, nrow(stratum)), estimand ("marginal" then "diff"), term
##   (coefficient name), intervention, and mi_effect, mi_se, mi_df, mi_ll, mi_ul. In "diff"
##   row 1 is the (Intercept), the mean under the reference regime, and the other rows are
##   contrasts against it.
##   mi_df is the degrees of freedom of the t reference that gFormulaMI::syntheticPool()
##   uses for mi_ll and mi_ul: (M - 1) * (1 - M * v / ((M + 1) * b))^2 for the mean within
##   variance v and the between variance b. When syntheticPool() stops because a total
##   variance is <= 0, the same rule is computed inline and the bad terms get NA in mi_se,
##   mi_df, mi_ll and mi_ul, with a warning that names the stratum. Any other
##   syntheticPool() error is re-raised.
##
## step = "contrast": effect modification across the strata of each modifier
##   results    the row-bound "gform" outputs; needs outcome, window, modifier, stratum,
##              estimand, term, intervention, mi_effect, mi_se and mi_df. Two rows with the
##              same outcome, window, modifier, stratum, estimand and term are an error.
##   reference  named character vector, modifier -> reference stratum, e.g.
##              c(sex = "male", race = "white", hiqual = "high")
##   Works on the "diff" contrasts, never the intercepts. Returns list(delta, q):
##     delta  every non-reference stratum minus its reference, per outcome x window x
##            modifier x intervention. delta_se = sqrt(se_s^2 + se_ref^2). delta_df is the
##            Welch-Satterthwaite df of that difference,
##            (se_s^2 + se_ref^2)^2 / (se_s^4 / df_s + se_ref^4 / df_ref), and the CI and
##            p use the t reference on delta_df (infinite df give the normal). A window
##            that lacks the reference stratum gets NA cells, not an error.
##     q      Cochran's Q across all strata of a modifier, per outcome x window x
##            modifier x intervention, referred to chi-square(k - 1); q, q_df and q_p are
##            NA when the group has fewer than 2 strata. min_mi_df is the smallest mi_df
##            among the group's k strata (NA if any is NA): a small value marks a Q that
##            rests on noisy variances, which the chi-square reference does not allow for.
effect_modification <- function(step = c("split", "gform", "contrast"),
                                wide_data = NULL, column = NULL, level = NULL,
                                stratum = NULL, mids = NULL, intervention_pattern = NULL,
                                M = 50, nSim = NULL, labels = list(),
                                results = NULL, reference = NULL) {

  step <- rlang::arg_match(step)

  switch(step,

    split = {
      if (!data.table::is.data.table(wide_data)) {
        stop("effect_modification: `wide_data` must be a data.table. Pass build_data()$data, ",
             "e.g. wide_data = wide_data_mcs_four$data", call. = FALSE)
      }
      if (!is.character(column) || length(column) != 1L || !column %in% names(wide_data)) {
        stop("effect_modification: `column` must name one column of `wide_data`. Pass a ",
             "baseline modifier column, e.g. column = \"sex_dv_base\"", call. = FALSE)
      }
      if (length(level) != 1L || !level %in% wide_data[[column]]) {
        stop("effect_modification: `level` must be one value that occurs in ", column,
             " (got: ", toString(level), "). Pass one of: ",
             toString(sort(unique(wide_data[[column]]))), call. = FALSE)
      }

      n_na <- sum(is.na(wide_data[[column]]))
      if (n_na > 0L) {
        message("effect_modification: dropped ", n_na, " row(s) with missing ", column)
      }

      keep <- which(wide_data[[column]] == level)
      if (length(keep) == 0L) {
        stop("effect_modification: no row has ", column, " == ", level,
             ". Pass a non-missing `level` that occurs in ", column, call. = FALSE)
      }

      # Baseline columns that carry no information once the data is split. Subsetting
      # on sex_dv_base == "Male" leaves a complete, constant column: mice removes it as
      # a predictor and logs an event for every variable it touches, and
      # make_counterfactual_method() still hands it a real method, because it probes
      # with an all-NA row appended, so gFormulaImpute would then fit logreg against a
      # single observed level. Dropping it up front avoids both.
      #
      # Subsetting a data.table need not carry the make_wide()/set_exposure()
      # attributes, so they are saved first and restamped, with the dropped names
      # removed from the column-name attributes.
      vec_attrs    <- c("baseline_vars", "time_lagged", "time_varying",
                        "outcome_vars", "mediators", "exposure_vars")
      scalar_attrs <- c("outcome_final", "outcome_baseline", "time_points")
      saved <- purrr::map(rlang::set_names(c(vec_attrs, scalar_attrs)),
                          \(a) attr(wide_data, a))

      out   <- wide_data[keep]
      const <- saved$baseline_vars[purrr::map_lgl(saved$baseline_vars,
                                                  \(v) data.table::uniqueN(out[[v]]) <= 1L)]
      if (length(const) > 0L) {
        out <- out[, setdiff(names(out), const), with = FALSE]
        message("effect_modification: dropped constant baseline column(s): ", toString(const))
      }

      for (a in vec_attrs)    attr(out, a) <- setdiff(saved[[a]], const)
      for (a in scalar_attrs) attr(out, a) <- saved[[a]]

      message("effect_modification: ", column, " == ", level, ": ", nrow(out), " of ",
              nrow(wide_data), " rows")
      out
    },

    gform = {
      library(magrittr)

      trt_vars      <- attr(stratum, "exposure_vars")
      outcome_final <- attr(stratum, "outcome_final")

      # fail before the expensive impute, not after
      if (is.null(intervention_pattern)) {
        stop("effect_modification: `intervention_pattern` is NULL. Pass ",
             "build_data()$intervention_pattern, e.g. ",
             "intervention_pattern = wide_data_mcs_four$intervention_pattern", call. = FALSE)
      }
      if (is.null(trt_vars) || is.null(outcome_final)) {
        stop("effect_modification: `stratum` lacks the 'exposure_vars'/'outcome_final' ",
             "attributes. Pass the output of effect_modification(\"split\", ...), not a ",
             "plain subset", call. = FALSE)
      }
      if (length(intervention_pattern[[1]]) != length(trt_vars)) {
        stop("effect_modification: regime length (", length(intervention_pattern[[1]]),
             ") != number of treatment columns (", length(trt_vars), "): ",
             toString(trt_vars), ". Pass the intervention_pattern built for the same ",
             "number of waves as `stratum`", call. = FALSE)
      }
      # gFormulaImpute matches the predictor matrix to mids$data by name, so a mids
      # imputed from a different column set silently misaligns the DAG. Row count is
      # checked as well because two strata of one modifier share a schema exactly:
      # names alone would pass a mids from the wrong stratum and label the results with
      # the wrong group.
      if (!inherits(mids, "mids")) {
        stop("effect_modification: `mids` must be a mids object. Pass run_mice(stratum) ",
             "for this exact stratum", call. = FALSE)
      }
      if (!identical(names(mids$data), names(stratum)) || nrow(mids$data) != nrow(stratum)) {
        stop("effect_modification: `mids` was not imputed from this `stratum`: ",
             nrow(mids$data), " x ", ncol(mids$data), " vs ", nrow(stratum), " x ",
             ncol(stratum), ". Pass run_mice(stratum) for this exact stratum", call. = FALSE)
      }
      # one imputation has no between-imputation variance, so nothing can be pooled
      if (!isTRUE(mids$m >= 2L)) {
        stop("effect_modification: `mids` holds m = ", toString(mids$m), " imputation(s), ",
             "but at least 2 are needed to pool them. Pass run_mice(stratum, m = ...) with ",
             "m >= 2", call. = FALSE)
      }
      labels_ok <- is.list(labels) &&
        (length(labels) == 0L ||
           (!is.null(names(labels)) && all(nzchar(names(labels))) &&
              all(purrr::map_lgl(labels, \(x) is.atomic(x) && length(x) == 1L))))
      if (!labels_ok) {
        stop("effect_modification: `labels` must be a named list of length-1 values. ",
             "Pass e.g. labels = list(outcome = \"mcs\", stratum = \"male\")", call. = FALSE)
      }

      predictor_matrix <- make_counterfactual_matrix(stratum)
      method_vector    <- make_counterfactual_method(stratum)

      # gFormulaImpute hardcodes maxit = 1 and draws columns left to right, so an arrow
      # pointing rightward reads that parent's iteration-0 hot-deck seed instead of its
      # drawn value. Dropping a constant baseline column preserves the column order, but
      # the invariant is cheap to assert here and expensive to discover downstream.
      rightward <- which(predictor_matrix == 1 & upper.tri(predictor_matrix), arr.ind = TRUE)
      if (nrow(rightward) > 0L) {
        stop("effect_modification: predictor matrix is not lower-triangular, so ",
             nrow(rightward), " arrow(s) read hot-deck seeds: ",
             toString(paste0(rownames(predictor_matrix)[rightward[, "row"]], " <- ",
                             colnames(predictor_matrix)[rightward[, "col"]])),
             ". Reorder the columns of `stratum` so every parent precedes its child",
             call. = FALSE)
      }

      # gFormulaImpute calls mice() once per IMPUTATION, with all regimes stacked, and
      # keeps none of the loggedEvents. Trace the call to collect them: stratifying is
      # exactly what makes a small stratum's models degenerate, and the pooled run
      # never showed it.
      log_env <- rlang::env(calls = list())

      collect_logged <- function(res) {
        if (is.null(res$iteration) || res$iteration == 0L) return(invisible(NULL))
        log_env$calls <- c(log_env$calls, list(res$loggedEvents))
        invisible(NULL)
      }

      invisible(suppressMessages(trace(
        "mice",
        exit  = bquote(.(collect_logged)(returnValue())),
        print = FALSE,
        where = asNamespace("mice")
      )))
      on.exit(suppressMessages(untrace("mice", where = asNamespace("mice"))),
              add = TRUE)

      imps <- withCallingHandlers(
        gFormulaMI::gFormulaImpute(
          data            = mids,
          M               = M,
          trtVars         = trt_vars,
          trtRegimes      = intervention_pattern,
          nSim            = nSim,
          method          = method_vector,
          predictorMatrix = predictor_matrix,
          silent          = TRUE
        ),
        warning = \(w) {
          if (grepl("^Number of logged events", conditionMessage(w))) {
            rlang::cnd_muffle(w)
          }
        }
      )

      if (!is.null(mids$m) && mids$m != M) {
        message("effect_modification: gFormulaImpute reset M to ", mids$m, " (requested ",
                M, "): M is bound to the number of imputations in mids. Change m in ",
                "run_mice() to change it.")
      }

      n_mice_calls  <- length(log_env$calls)
      logged_events <- purrr::keep(log_env$calls, \(le) !is.null(le) && nrow(le) > 0)
      tag <- paste(names(labels), purrr::map_chr(labels, as.character),
                   sep = " = ", collapse = ", ")

      if (length(logged_events) == 0) {
        message("effect_modification: no logged events in ", n_mice_calls, " mice() call(s).")
      } else {
        tally <- logged_events |>
          purrr::list_rbind() |>
          dplyr::count(dep, meth, out, sort = TRUE)

        message("effect_modification: ", length(logged_events), " of ", n_mice_calls,
                " mice() call(s) logged events", if (nzchar(tag)) paste0(" for ", tag),
                ". dep | meth | out (n calls):")
        purrr::pwalk(tally, \(dep, meth, out, n)
                     message("  ", dep, " | ", meth, " | ", out, "  (", n, ")"))
      }

      # marginal and difference models across the M synthetic datasets
      fits_marginal <- imps %$% stats::lm(stats::reformulate("0 + factor(regime)", outcome_final))
      fits_diff     <- imps %$% stats::lm(stats::reformulate("factor(regime)",     outcome_final))

      # imps is M x 2^n_waves stacked datasets, the largest object here
      rm(imps); gc()

      regime_labels <- purrr::map_chr(intervention_pattern, paste, collapse = "-")

      # Taken here, outside the transmute() below: `stratum` is also a label name, and the
      # label column that call creates first would mask the data in it.
      stratum_rows <- nrow(stratum)

      # "[mcs/four/sex/female] " heads the fallback warning. In targets the target name
      # identifies the stratum; in a manual run nothing else does.
      stratum_tag <- if (length(labels) > 0L) {
        paste0("[", paste(purrr::map_chr(labels, as.character), collapse = "/"), "] ")
      } else {
        ""
      }

      # `term` is kept: in the difference fit row 1 is the intercept (the mean under the
      # reference regime) and rows 2+ are contrasts against it, so the intervention
      # label on row 1 does not mean the same thing as on the others.
      pool_and_label <- function(fits, estimand) {
        pooled <- tryCatch(
          gFormulaMI::syntheticPool(fits),
          error = \(e) {
            # syntheticPool() stop()s when any total variance is <= 0, and one near-null
            # contrast must not lose a whole cluster job. The identical rule is computed
            # here instead, with the offending terms blanked. M is the number of fits,
            # which is mids$m rather than the requested M.
            M      <- length(fits$analyses)
            ests   <- do.call(rbind, lapply(fits$analyses, stats::coef))
            vars   <- do.call(rbind, lapply(fits$analyses, \(f) diag(stats::vcov(f))))
            est    <- colMeans(ests)
            v      <- colMeans(vars)
            b      <- diag(stats::var(ests))
            total  <- (1 + 1 / M) * b - v
            df     <- (M - 1) * (1 - (M * v) / ((M + 1) * b))^2

            # a failure that is not a bad total is not this rule's to absorb
            bad <- is.na(total) | total <= 0
            if (!any(bad)) stop(e)

            warning("effect_modification: ", stratum_tag, estimand,
                    ": total variance <= 0 for term(s) ", toString(names(est)[bad]),
                    ", so mi_se, mi_df, mi_ll and mi_ul are NA. A larger M and/or nSim ",
                    "usually fixes it (syntheticPool: ", conditionMessage(e), ")",
                    call. = FALSE)
            total[bad] <- NA_real_
            df[bad]    <- NA_real_

            cbind(Estimate   = est,
                  Total      = total,
                  df         = df,
                  `95% CI L` = est - stats::qt(0.975, df) * sqrt(total),
                  `95% CI U` = est + stats::qt(0.975, df) * sqrt(total))
          }
        )

        pooled |>
          tibble::as_tibble(rownames = "term") |>
          dplyr::transmute(
            !!!labels,
            stratum_n    = stratum_rows,
            estimand     = estimand,
            term,
            intervention = regime_labels,
            mi_effect    = Estimate,
            mi_se        = sqrt(Total),
            mi_df        = df,
            mi_ll        = `95% CI L`,
            mi_ul        = `95% CI U`
          )
      }

      dplyr::bind_rows(pool_and_label(fits_marginal, "marginal"),
                       pool_and_label(fits_diff,     "diff"))
    },

    contrast = {
      need <- c("outcome", "window", "modifier", "stratum", "estimand", "term",
                "intervention", "mi_effect", "mi_se", "mi_df")
      if (!is.data.frame(results) || !all(need %in% names(results))) {
        stop("effect_modification: `results` must be a data frame with column(s) ",
             toString(need), if (is.data.frame(results)) {
               paste0(" (missing: ", toString(setdiff(need, names(results))), ")")
             }, ". Pass the row-bound \"gform\" outputs, e.g. results = em_results",
             call. = FALSE)
      }
      results <- tibble::as_tibble(results)

      # The same coefficient of the same stratum twice (a stratum bound in twice, a target
      # built twice) would join many-to-many below and be counted twice in Q.
      id_cols <- c("outcome", "window", "modifier", "stratum", "estimand", "term")
      dup     <- duplicated(results[id_cols])
      if (any(dup)) {
        first <- results[which(dup)[1L], id_cols]
        stop("effect_modification: `results` has duplicated rows: ", sum(dup),
             " row(s) repeat an earlier one, the first duplicated key being ",
             toString(paste0(id_cols, " = ", purrr::map_chr(first, as.character))),
             ". Pass each stratum's \"gform\" output once, e.g. ",
             "results = dplyr::distinct(em_results)", call. = FALSE)
      }

      modifiers <- unique(as.character(results$modifier))
      no_entry  <- setdiff(modifiers, names(reference))
      if (length(no_entry) > 0L) {
        stop("effect_modification: `reference` has no entry for modifier(s) ",
             toString(no_entry), ". Pass a named character vector, e.g. ",
             "reference = c(sex = \"male\", race = \"white\", hiqual = \"high\")",
             call. = FALSE)
      }
      absent <- modifiers[!purrr::map_lgl(modifiers, \(m)
        reference[[m]] %in% results$stratum[which(results$modifier == m)])]
      if (length(absent) > 0L) {
        stop("effect_modification: the reference stratum of modifier(s) ", toString(absent),
             " (", toString(paste0(absent, " = ", reference[absent])), ") is not in ",
             "`results`. Pass a `reference` stratum that occurs in results$stratum",
             call. = FALSE)
      }

      key <- c("outcome", "window", "modifier", "intervention")

      # only the contrasts against the reference regime: not the intercepts (the mean
      # under the reference regime) and not the marginal means
      d <- results[which(results$estimand == "diff" & results$term != "(Intercept)"), ]
      d$ref_stratum <- unname(reference[as.character(d$modifier)])
      is_ref <- d$stratum == d$ref_stratum

      # Columns are picked by name on both sides of the join, so nothing else that "gform"
      # adds (stratum_n) can turn into .x/.y columns.
      ref <- d[is_ref, c(key, "mi_effect", "mi_se", "mi_df")]
      names(ref) <- c(key, "ref_effect", "ref_se", "ref_df")

      # A difference of two independent strata has the Welch-Satterthwaite df, and each
      # stratum's own t reference is syntheticPool()'s, so delta is referred to t on
      # delta_df. An infinite df drops out of the sum (Inf, Inf gives Inf, the normal), and
      # an NA anywhere gives NA cells: nothing here uses na.rm. A window without the
      # reference stratum finds no row in the join, so its cells are NA too.
      delta <- d[!is_ref, c(key, "stratum", "ref_stratum", "mi_effect", "mi_se", "mi_df")] |>
        dplyr::left_join(ref, by = key) |>
        dplyr::transmute(
          outcome, window, modifier, intervention, stratum,
          reference = ref_stratum,
          delta     = mi_effect - ref_effect,
          delta_se  = sqrt(mi_se^2 + ref_se^2),
          delta_df  = (mi_se^2 + ref_se^2)^2 / (mi_se^4 / mi_df + ref_se^4 / ref_df),
          delta_ll  = delta - stats::qt(0.975, delta_df) * delta_se,
          delta_ul  = delta + stats::qt(0.975, delta_df) * delta_se,
          delta_p   = 2 * stats::pt(-abs(delta / delta_se), delta_df)
        )

      # Cochran's Q: inverse-variance weighted spread of the strata about their pooled
      # contrast, on chi-square(k - 1). sum() and min() without na.rm, so one NA mi_se
      # makes q and q_p NA and one NA mi_df makes min_mi_df NA. With a single stratum there
      # is no spread to test, so q, q_df and q_p are NA (typed, so the columns keep their
      # type even when every group has one stratum). min_mi_df is the smallest df among the
      # strata: a small one marks a Q that rests on noisy variances.
      q <- d |>
        dplyr::summarise(
          k = dplyr::n(),
          q = if (k < 2L) NA_real_ else {
            w <- 1 / mi_se^2
            sum(w * (mi_effect - sum(w * mi_effect) / sum(w))^2)
          },
          q_df      = if (k < 2L) NA_integer_ else k - 1L,
          q_p       = stats::pchisq(q, q_df, lower.tail = FALSE),
          min_mi_df = min(mi_df),
          .by = dplyr::all_of(key)
        )

      list(delta = tibble::as_tibble(delta), q = tibble::as_tibble(q))
    }
  )
}
