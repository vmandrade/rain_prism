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


# some graphics

library(ggplot2)
library(reshape2)
library(gridExtra) # for plotting multiple panels within a page

# function to rotate a matrix 90 degress clockwise 
# for plotting only - used to counteract the "image" function default behavior
Fn_Rotate_Matrix <- function(mat) {
  return (t(mat)[, nrow(mat):1])
}

# for visualization discretize the color scale
color_breaks <- c(0, 100, 500, 1000, 1500, 2000, 3000, 4000, 6000, 9000)
color_labels <- c(100, 500, 1000, 1500, 2000, 3000, 4000, 6000, 9000)

Fn_Discretize_Rainfall <- function(in_mat) {
  in_mat <- Fn_Rotate_Matrix(in_mat)
  in_mat[in_mat <= 0] <- NA
  in_mat <- melt(in_mat)
  in_mat$colorScale <- cut(in_mat$value, 
                          breaks = color_breaks, 
                          labels = color_labels,
                          include.lowest = TRUE)
  
  return (in_mat)
}

# plot 2013 totals

tot_2013_gfx <- Fn_Discretize_Rainfall(tot_2013)

gfx_2013 <- ggplot(data = tot_2013_gfx)
gfx_2013 <- gfx_2013 + geom_raster(aes(Var1, Var2, fill = colorScale))
gfx_2013 <- gfx_2013 + scale_fill_brewer(palette = "RdYlBu", drop = FALSE)
gfx_2013 <- gfx_2013 + theme(axis.text = element_blank(), axis.ticks = element_blank())
gfx_2013 <- gfx_2013 + labs(x = NULL, y = NULL, fill = "Rain (mm/yr)") 

print(gfx_2013)

# plot mean for 1895-2012
mean_hist_gfx <- Fn_Discretize_Rainfall(mean_hist)

gfx_mean <- ggplot(data = mean_hist_gfx)
gfx_mean <- gfx_mean + geom_raster(aes(Var1, Var2, fill = colorScale))
gfx_mean <- gfx_mean + scale_fill_brewer(palette = "RdYlBu", drop = FALSE)
gfx_mean <- gfx_mean + theme(axis.text = element_blank(), axis.ticks = element_blank())
gfx_mean <- gfx_mean + labs(x = NULL, y = NULL, fill = "Rain (mm/yr)") 

print(gfx_mean)


# plot 2013 total as fraction of mean
Fn_Discretize_Fraction <- function(in_mat) {
  in_mat <- Fn_Rotate_Matrix(in_mat)
  in_mat[in_mat <= 0] <- NA
  in_mat <- melt(in_mat)
  in_mat$colorScale <- cut(in_mat$value, 
                           breaks = c(0, 0.1, 0.2, 0.3, 0.5, 0.75, 1, 1.5, 3), 
                           labels = c("10%", "20%", "30%", "50%", "75%", "100%", "150%", "300%"),
                           include.lowest = TRUE)
  
  return (in_mat)
}

# w.r.t. long term average
frac_2013 <- tot_2013 / mean_hist
frac_2013 <- Fn_Discretize_Fraction(frac_2013)

gfx_frac <- ggplot(data = frac_2013)
gfx_frac <- gfx_frac + geom_raster(aes(Var1, Var2, fill = colorScale))
gfx_frac <- gfx_frac + scale_fill_brewer(palette = "RdYlBu", drop = FALSE)
gfx_frac <- gfx_frac + theme(axis.text = element_blank(), axis.ticks = element_blank())
gfx_frac <- gfx_frac + labs(x = NULL, y = NULL, fill = "% Historical Avg") 

print(gfx_frac)

# w.r.t. past 30-yr average
frac_2013 <- tot_2013 / mean_hist_recent
frac_2013 <- Fn_Discretize_Fraction(frac_2013)

gfx_frac <- ggplot(data = frac_2013)
gfx_frac <- gfx_frac + geom_raster(aes(Var1, Var2, fill = colorScale))
gfx_frac <- gfx_frac + scale_fill_brewer(palette = "RdYlBu", drop = FALSE)
gfx_frac <- gfx_frac + theme(axis.text = element_blank(), axis.ticks = element_blank())
gfx_frac <- gfx_frac + labs(x = NULL, y = NULL, fill = "% Historical Avg") 

print(gfx_frac)

