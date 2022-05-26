calc_lsf <- function(age) {
   dat1 <- data.table(a = 40:65,
                      lsf = seq(from = 18,
                                to = 23,
                                by = 0.2))
   sapply(age, function(a1){
      if (a1 < 40) {
         return(18)
      } else{
         if (a1 > 65) {
            return(23)
         } else{
            return(dat1[a == a1, lsf])
         }
      }
      return(NA)
   })


}
