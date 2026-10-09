# Meta analysis of gFormulaMI estimates
# Pools the two four-wave analyses -- waves 3-6 and waves 7-10 -- one regime at a
# time, under either a fixed-effect or a random-effects model with
# Knapp-Hartung confidence intervals.
#
# Two caveats belong in the methods text alongside anything this produces:
#
#   1. k = 2. Knapp-Hartung rests on a t quantile with k - 1 degrees of freedom,
#      so here it is t on 1 df (12.71 against the normal 1.96). The random-effects
#      interval is therefore about 6.5 times the width of the fixed-effect one,
#      and tau2 estimated from two studies carries almost no information. That is
#      correct HK behaviour, not a defect, but it makes the random-effects arm a
#      deliberately conservative sensitivity check rather than a drop-in default.
#      The same k = 2 arithmetic bites from the other side too: the HK variance
#      rescales the pooled variance by Q / (k - 1), so two windows that happen to
#      agree exactly give Q = 0 and an interval of zero width. `adhoc.hakn.ci`
#      guards that -- see the argument below.
#   2. The two windows are not independent. They draw on overlapping individuals
#      from the same UKHLS panel, so their estimates are correlated and the
#      fixed-effect pooled standard error is anticonservative.

meta_analysis <- function(gform_early,
                          gform_late,
                          labels     = c("Waves 3-6", "Waves 7-10"),
                          effects    = c("fixed", "random"),
                          method.tau = "REML",
                          # IQWiG (2020) ad hoc correction: floor the Knapp-Hartung
                          # standard error at the classic random-effects one. meta's
                          # own default is "", which at k = 2 lets a zero-width
                          # interval through whenever the two windows agree. Set to
                          # "" for unguarded Knapp-Hartung.
                          adhoc.hakn.ci = "se",
                          sm         = "MRAW") {
  library(meta)

  effects <- match.arg(effects)

  if (length(labels) != 2L || anyNA(labels)) {
    stop("meta_analysis(): `labels` must be two study labels", call. = FALSE)
  }

  # run_gform() returns list(results = <tibble>); a bare tibble is accepted too so
  # the caller can pool an already-extracted table without unwrapping it first.
  as_results <- function(x, arg) {
    out <- if (is.data.frame(x)) x else x$results
    if (!is.data.frame(out)) {
      stop("meta_analysis(): `", arg, "` is neither a data frame nor a ",
           "run_gform() result carrying $results", call. = FALSE)
    }
    needed <- c("intervention", "mi_effect", "mi_se")
    missing <- setdiff(needed, names(out))
    if (length(missing)) {
      stop("meta_analysis(): `", arg, "` is missing column(s) ",
           paste(missing, collapse = ", "), call. = FALSE)
    }
    tibble::as_tibble(out)
  }

  early <- as_results(gform_early, "gform_early")
  late  <- as_results(gform_late,  "gform_late")

  # Same contract as pool_ltmle(): the two arms must describe the same regimes,
  # or the row-wise pairing below is meaningless.
  only_early <- setdiff(early$intervention, late$intervention)
  only_late  <- setdiff(late$intervention, early$intervention)
  if (length(only_early) || length(only_late)) {
    stop("meta_analysis(): the two wave-sets disagree on the regime set.",
         if (length(only_early)) paste0(" Only in gform_early: ",
                                        paste(only_early, collapse = ", "), "."),
         if (length(only_late))  paste0(" Only in gform_late: ",
                                        paste(only_late, collapse = ", "), "."),
         call. = FALSE)
  }
  if (anyDuplicated(early$intervention) || anyDuplicated(late$intervention)) {
    stop("meta_analysis(): regimes must be unique within each wave-set",
         call. = FALSE)
  }

  # Align on early's regime order, so pairing is positional from here on.
  late <- late[match(early$intervention, late$intervention), ]

  if (effects == "random") {
    message("meta_analysis: random effects on k = 2 studies. Knapp-Hartung ",
            "uses t on 1 df, so the intervals are wide by construction and ",
            "tau2 is barely identified.")
  }

  # metagen, not metamean: we hold an estimate and its synthetic-pool standard
  # error, not n / mean / sd. metamean(sm = "MRAW") would need sd back-computed
  # as mi_se * sqrt(n), at which point n cancels and the numbers are identical --
  # so this is the same meta-analysis of means with nothing fabricated.
  fits <- purrr::pmap(
    list(early$mi_effect, early$mi_se, late$mi_effect, late$mi_se),
    function(te_e, se_e, te_l, se_l) {
      meta::metagen(
        TE               = c(te_e, te_l),
        seTE             = c(se_e, se_l),
        studlab          = labels,
        sm               = sm,
        common           = effects == "fixed",
        random           = effects == "random",
        method.tau       = method.tau,
        method.random.ci = if (effects == "random") "HK" else "classic",
        adhoc.hakn.ci    = if (effects == "random") adhoc.hakn.ci else ""
      )
    }
  )
  names(fits) <- early$intervention

  # Pull the arm the caller asked for. metagen always computes both, so the
  # suffix is the only thing that changes. unname() because fits is a named list
  # and map_dbl would otherwise carry the regime names into the tibble columns.
  suffix <- if (effects == "fixed") "common" else "random"
  pull <- function(field) {
    unname(purrr::map_dbl(fits, \(m) as.numeric(m[[field]])))
  }
  pick <- function(field) pull(paste0(field, ".", suffix))

  results <- tibble::tibble(
    intervention = early$intervention,
    mi_effect    = round(pick("TE"), 3),
    mi_se        = round(pick("seTE"), 3),
    mi_ll        = round(pick("lower"), 3),
    mi_ul        = round(pick("upper"), 3),
    effects      = effects,
    tau2         = round(pull("tau2"), 3),
    I2           = round(pull("I2"), 3),
    Q            = round(pull("Q"), 3),
    p_het        = round(pull("pval.Q"), 3)
  )

  list(results = results, fits = fits)
}
