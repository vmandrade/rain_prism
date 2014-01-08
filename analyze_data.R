# analyze monthly data downloaded from PRISM
# estimate 2013 total as a percentile of 1895-2012 data

library(rgdal)

# prism data dimensions
prism_cols <- 1405
prism_rows <- 621

# get annual totals for each grid, from 1895 to 2012
beg_year <- 1895
end_year <- 2012
out_data <- array(data = NA, dim = c(prism_rows, prism_cols, length(seq(beg_year, end_year))))
for (eachYear in seq(beg_year, end_year)) {

  # read data
  prism_data <- readGDAL(paste0("data/PRISM_ppt_stable_4kmM2_", eachYear, "_bil.bil"),
                         silent = TRUE)
  
  # check data dimensions
  stopifnot(prism_data@grid@cells.dim[1] == prism_cols)
  stopifnot(prism_data@grid@cells.dim[2] == prism_rows)
  
  # reshape the data to a lat-lon matrix
  ann_tot <- prism_data@data$band1
  ann_tot <- matrix(ann_tot, nrow = prism_rows, ncol = prism_cols, byrow = TRUE)
  
  # update ouput
  out_data[, , eachYear - beg_year + 1] <- ann_tot
}

# monthly totals for 2013
out_2013 <- array(data = NA, dim = c(prism_rows, prism_cols, 12))
for (eachMonth in c(1:12)) {
  
  # read data
  if (eachMonth <= 6) {
    prism_data <- readGDAL(paste0("data/PRISM_ppt_stable_4kmM2_2013", 
                                  sprintf("%.2d", eachMonth), 
                                  "_bil.bil"),
                           silent = TRUE)
  } else {
    prism_data <- readGDAL(paste0("data/PRISM_ppt_provisional_4kmM2_2013", 
                                  sprintf("%.2d", eachMonth), 
                                  "_bil.bil"),
                           silent = TRUE)    
  }
  
  # check data dimensions
  stopifnot(prism_data@grid@cells.dim[1] == prism_cols)
  stopifnot(prism_data@grid@cells.dim[2] == prism_rows)
  
  # reshape the data to a lat-lon matrix
  mon_tot <- prism_data@data$band1
  mon_tot <- matrix(mon_tot, nrow = prism_rows, ncol = prism_cols, byrow = TRUE)
  
  # update ouput
  out_2013[, , eachMonth] <- mon_tot
}

# compute annual total for 2013
tot_2013 <- apply(out_2013, MARGIN = c(1, 2), FUN = sum, na.rm = TRUE)

# compute historical mean, 1895-2012
mean_hist <- apply(out_data, MARGIN = c(1, 2), FUN = mean, na.rm = TRUE)

# compute historical mean, 1983-2012
mean_hist_recent <- apply(out_data[, , c(89:118)], MARGIN = c(1, 2), FUN = mean, na.rm = TRUE)

# save relevant data
save(tot_2013, mean_hist, mean_hist_recent, file = "prism_summary.RData")


