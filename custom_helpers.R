accum <- function (pool, histvars, time_name, t, id_name)
{
  if (t == 0) {
    lapply(
      histvars,
      FUN = function(histvar) {
        pool[get(time_name) == t, `:=`((paste("cum_",
                                              histvar, sep = "")), as.double(pool[get(time_name) ==
                                                                                    t][[histvar]]))]
      }
    )
  }
  else {
    current_ids <- unique(pool[get(time_name) == t][[id_name]])
    lapply(
      histvars,
      FUN = function(histvar) {
        pool[get(time_name) == t, `:=`((paste("cum_",
                                              histvar, sep = "")), tapply(pool[get(id_name) %in%
                                                                                           current_ids &
                                                                                           get(time_name) <= t][[histvar]],
                                                                                    pool[get(id_name) %in% current_ids &
                                                                                                      get(time_name) <= t][[id_name]], FUN = sum))]
      }
    )
  }
  
}

increment <- function (pool, histvars, time_name, t, id_name) 
{
    if (t == 0) {
      current_ids <- unique(pool[pool[[time_name]] == t][[id_name]])
      lapply(histvars, FUN = function(histvar) {
        pool[pool[[time_name]] == t, `:=`((paste(histvar, "_incr", sep = "")), pool[pool[[time_name]] == 
                                                                                      t & get(id_name) %in% current_ids][[histvar]])]
      })
    } else {
      current_ids <- unique(pool[pool[[time_name]] == t][[id_name]])
      lapply(histvars, FUN = function(histvar) {
        pool[pool[[time_name]] == t, `:=`((paste(histvar, "_incr", sep = "")), pool[pool[[time_name]] == 
                                                                                                  t - 1 & get(id_name) %in% current_ids][[paste(histvar, "_incr", sep = "")]] + 1)]
      })
    }
}



random <- function (pool, histvars, time_name, t, id_name) 
{
  current_ids <- unique(pool[pool[[time_name]] == t][[id_name]])
  lapply(histvars, FUN = function(histvar) {
    pool[pool[[time_name]] == t, `:=`((paste("rand", "_", histvar, sep = "")), rnorm(nrow(pool[pool[[time_name]] == 0 & get(id_name) %in% current_ids])))]
  })
  # browser()
  
}

# (pool[pool[[time_name]] == 0 & get(id_name) %in% current_ids][[histvar]])


#  cuavg code
# function (pool, histvars, time_name, t, id_name, below_zero_indicator) 
# {
#   if (below_zero_indicator) {
#     denom <- t + 1 - min(pool[[time_name]])
#   }
#   else {
#     denom <- t + 1
#   }
#   if (t == 0 & !below_zero_indicator) {
#     lapply(histvars, FUN = function(histvar) {
#       pool[get(time_name) == t, `:=`((paste("cumavg_", 
#                                             histvar, sep = "")), as.double(pool[get(time_name) == 
#                                                                                   t][[histvar]]))]
#     })
#   }
#   else {
#     current_ids <- unique(pool[get(time_name) == t][[id_name]])
#     colnam <- colnames(pool)
#     if (!(paste("cumavg_", "_", histvars[1], sep = "") %in% 
#           colnam)) {
#       id_factor <- is.factor(pool[[id_name]])
#       if (id_factor) {
#         lapply(histvars, FUN = function(histvar) {
#           pool[get(time_name) == t, `:=`((paste("cumavg_", 
#                                                 histvar, sep = "")), as.double(tapply(pool[get(id_name) %in% 
#                                                                                              current_ids & get(time_name) <= t][[histvar]], 
#                                                                                       droplevels(pool[get(id_name) %in% current_ids & 
#                                                                                                         get(time_name) <= t][[id_name]]), FUN = mean)))]
#         })
#       }
#       else {
#         lapply(histvars, FUN = function(histvar) {
#           pool[get(time_name) == t, `:=`((paste("cumavg_", 
#                                                 histvar, sep = "")), as.double(tapply(pool[get(id_name) %in% 
#                                                                                              current_ids & get(time_name) <= t][[histvar]], 
#                                                                                       pool[get(id_name) %in% current_ids & get(time_name) <= 
#                                                                                              t][[id_name]], FUN = mean)))]
#         })
#       }
#     }
#     else {
#       for (histvar in histvars) {
#         pool[get(time_name) == t, `:=`((paste("cumavg_", 
#                                               histvar, sep = "")), as.double((pool[get(id_name) %in% 
#                                                                                      current_ids & get(time_name) == (t - 1)][[paste("cumavg_", 
#                                                                                                                                      histvar, sep = "")]] * (denom - 1) + pool[get(id_name) %in% 
#                                                                                                                                                                                  current_ids & get(time_name) == t][[histvar]])/denom))]
#       }
#     }
#   }
# }