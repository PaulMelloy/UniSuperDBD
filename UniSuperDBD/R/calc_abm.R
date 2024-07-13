calc_abm <- function(rate = 0.05, ten_year = FALSE, FTE = 1){

   abm <- data.table::fcase(rate == 0, 0.11,
                            rate == 0.02, 0.15,
                            rate == 0.03, 0.17,
                            rate == 0.04, 0.19,
                            rate == 0.05, 0.21,
                            rate == 0.06 & isFALSE(ten_year), 0.22,
                            rate == 0.07 & isFALSE(ten_year), 0.23,
                            rate == 0.08 & isFALSE(ten_year), 0.24,
                            rate == 0.09 & isFALSE(ten_year), 0.25,
                            rate == 0.10 & isFALSE(ten_year), 0.26,
                            rate == 0.06 & isTRUE(ten_year), 0.23,
                            rate == 0.07 & isTRUE(ten_year), 0.25,
                            rate == 0.08 & isTRUE(ten_year), 0.27,
                            rate == 0.09 & isTRUE(ten_year), 0.29,
                            rate == 0.10 & isTRUE(ten_year), 0.31)

   return(abm)
}
