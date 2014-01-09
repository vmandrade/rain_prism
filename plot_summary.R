# graphics on drought analysis using PRISM data

load("prism_summary.RData")

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
gfx_2013 <- gfx_2013 + ggtitle("Rainfall Total for 2013 (mm)")

print(gfx_2013)

# plot mean for 1895-2012
mean_hist_gfx <- Fn_Discretize_Rainfall(mean_hist)

gfx_mean <- ggplot(data = mean_hist_gfx)
gfx_mean <- gfx_mean + geom_raster(aes(Var1, Var2, fill = colorScale))
gfx_mean <- gfx_mean + scale_fill_brewer(palette = "RdYlBu", drop = FALSE)
gfx_mean <- gfx_mean + theme(axis.text = element_blank(), axis.ticks = element_blank())
gfx_mean <- gfx_mean + labs(x = NULL, y = NULL, fill = "Rain (mm/yr)") 
gfx_mean <- gfx_mean + ggtitle("Historical Rainfall Average for 1895-2012 (mm/year)")

print(gfx_mean)

# plot 2013 total as fraction of mean
Fn_Discretize_Fraction <- function(in_mat) {
  in_mat <- Fn_Rotate_Matrix(in_mat)
  in_mat[in_mat <= 0] <- NA
  in_mat <- melt(in_mat)
  in_mat$colorScale <- cut(in_mat$value, 
                           breaks = c(0, 0.1, 0.3, 0.5, 0.75, 1, 1.5, 3, 5), 
                           labels = c("10%", "30%", "50%", "75%", "100%", "150%", "300%", "500%"),
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
gfx_frac <- gfx_frac + ggtitle("Rainfall in 2013 as Percentage of Long-term Historical Average (1895-2012)")

print(gfx_frac)

# w.r.t. past 30-yr average
frac_2013 <- tot_2013 / mean_hist_recent
frac_2013 <- Fn_Discretize_Fraction(frac_2013)

gfx_frac <- ggplot(data = frac_2013)
gfx_frac <- gfx_frac + geom_raster(aes(Var1, Var2, fill = colorScale))
gfx_frac <- gfx_frac + scale_fill_brewer(palette = "RdYlBu", drop = FALSE)
gfx_frac <- gfx_frac + theme(axis.text = element_blank(), axis.ticks = element_blank())
gfx_frac <- gfx_frac + labs(x = NULL, y = NULL, fill = "% Historical Avg") 
gfx_frac <- gfx_frac + ggtitle("Rainfall in 2013 as Percentage of Near-term Historical Average (1983-2012)")

print(gfx_frac)
