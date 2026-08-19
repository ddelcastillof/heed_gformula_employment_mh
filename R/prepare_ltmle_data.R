# Turn a mice::mids of wide data into the per-imputation list that ltmle::ltmle()
# consumes.

prepare_ltmle_data <- function(wide_mids, outcome, n_waves, y_scale = 100) {

  nodes <- ltmle_nodes(outcome, n_waves)

  # confounders at every wave: whatever is neither baseline, A, nor Y
  conf_cols <- setdiff(nodes$cols, c(nodes$baseline, nodes$Anodes, nodes$Ynodes))

  is_binary_factor <- function(x) is.factor(x) && setequal(levels(x), c("0", "1"))

  # ltmle compares Anodes against abar, so they must be numeric 0/1
  to_binary_int <- function(x, nm) {
    if (!is.factor(x)) return(as.integer(x))
    if (!is_binary_factor(x)) {
      stop("prepare_ltmle_data(): A node '", nm, "' is a factor with levels ",
           toString(levels(x)), "; ltmle needs a binary 0/1 node", call. = FALSE)
    }
    as.integer(as.character(x))
  }

  prep_one <- function(dat) {
    dat |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(nodes$Anodes),
                      \(x) to_binary_int(x, dplyr::cur_column())),
        dplyr::across(dplyr::all_of(conf_cols),
                      \(x) if (is_binary_factor(x)) as.integer(as.character(x)) else x),
        dplyr::across(dplyr::all_of(nodes$Ynodes), \(x) x / y_scale)
      ) |>
      dplyr::select(dplyr::all_of(nodes$cols))
  }

  imps <- mice::complete(wide_mids, action = "all")

  # every imputation shares the mids column set, so one check covers all of them
  absent <- setdiff(nodes$cols, names(imps[[1L]]))
  if (length(absent)) {
    stop("prepare_ltmle_data(): ", length(absent),
         " node column(s) missing from the imputed data: ", toString(absent),
         call. = FALSE)
  }

  out <- purrr::map(imps, prep_one)
  attr(out, "ltmle_nodes") <- nodes
  out
}
