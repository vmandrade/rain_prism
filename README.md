USA Drought of 2013: Analysis of High-Resolution Rainfall Data Using R
========================================================

The ongoing drought in California and other parts of Southwestern United States has been reported extensively by [newspapers](http://www.mercurynews.com/science/ci_24812749/california-drought-deepens-another-years-rains-stay-away) and [government sites](http://droughtmonitor.unl.edu/).

Although rainfall deficit is technically meteorological drought, and drought could be of several other types (such as hydrological, agricultural, etc.), the attempt here is to demonstrate the use of R in the analysis of high resolution rainfall data. Using 4km rainfall data from the [PRISM Climate Group](http://prism.oregonstate.edu/) for the time period 1895-2013, the annual total for 2013 is compared with the long-term and near-term historical averages from PRISM. 

This effort is part of [The Rain Project](http://rationshop.github.io/rain_r/) and the entire code is available on GitHub - https://github.com/RationShop/rain_prism

First, obtain the annual rainfall data from the PRISM website. Next, obtain the annual total for 2013 and also the long-term (1895-2012) and near-term (1983-2012) historical averages. Below lines commented out as they take about half hour or so to run.


```r
# source('obtain_data.R') source('analyze_data.R')
```


Load the relevant datasets and the required libraries.


```r
load("prism_summary.RData")

library(ggplot2)
library(reshape2)
```


Function to rotate a matrix 90 degress clockwise for plotting purposes. Used to counteract the "image" function default behavior.


```r
Fn_Rotate_Matrix <- function(mat) {
    return(t(mat)[, nrow(mat):1])
}
```


For visualization discretize rainfall.


```r
color_breaks <- c(0, 100, 500, 1000, 1500, 2000, 3000, 4000, 6000, 9000)
color_labels <- c(100, 500, 1000, 1500, 2000, 3000, 4000, 6000, 9000)

Fn_Discretize_Rainfall <- function(in_mat) {
    in_mat <- Fn_Rotate_Matrix(in_mat)
    in_mat[in_mat <= 0] <- NA
    in_mat <- melt(in_mat)
    in_mat$colorScale <- cut(in_mat$value, breaks = color_breaks, labels = color_labels, 
        include.lowest = TRUE)
    
    return(in_mat)
}
```


Plot total for 2013.


```r
tot_2013_gfx <- Fn_Discretize_Rainfall(tot_2013)

gfx_2013 <- ggplot(data = tot_2013_gfx)
gfx_2013 <- gfx_2013 + geom_raster(aes(Var1, Var2, fill = colorScale))
gfx_2013 <- gfx_2013 + scale_fill_brewer(palette = "RdYlBu", drop = FALSE)
gfx_2013 <- gfx_2013 + theme(axis.text = element_blank(), axis.ticks = element_blank())
gfx_2013 <- gfx_2013 + labs(x = NULL, y = NULL, fill = "Rain (mm/yr)")
gfx_2013 <- gfx_2013 + ggtitle("Rainfall Total for 2013 (mm)")

png("gfx_2013_total.png", width = 700, height = 450)
plot(gfx_2013)
garbage <- dev.off()
```

![figure 1] [prism1]

Plot long-term average rainfall.


```r
mean_hist_gfx <- Fn_Discretize_Rainfall(mean_hist)

gfx_mean <- ggplot(data = mean_hist_gfx)
gfx_mean <- gfx_mean + geom_raster(aes(Var1, Var2, fill = colorScale))
gfx_mean <- gfx_mean + scale_fill_brewer(palette = "RdYlBu", drop = FALSE)
gfx_mean <- gfx_mean + theme(axis.text = element_blank(), axis.ticks = element_blank())
gfx_mean <- gfx_mean + labs(x = NULL, y = NULL, fill = "Rain (mm/yr)")
gfx_mean <- gfx_mean + ggtitle("Historical Rainfall Average for 1895-2012 (mm/year)")

png("gfx_mean_longterm.png", width = 700, height = 450)
plot(gfx_mean)
garbage <- dev.off()
```

![figure 2] [prism2]

Plot 2013 total as fraction of the long-term mean. Discretize fractions for visualization. The spatial patterns in the below map are generally consistent with those from the [US Drought Monitor](http://droughtmonitor.unl.edu/).


```r
Fn_Discretize_Fraction <- function(in_mat) {
    in_mat <- Fn_Rotate_Matrix(in_mat)
    in_mat[in_mat <= 0] <- NA
    in_mat <- melt(in_mat)
    in_mat$colorScale <- cut(in_mat$value, breaks = c(0, 0.1, 0.3, 0.5, 0.75, 
        1, 1.5, 3, 5), labels = c("10%", "30%", "50%", "75%", "100%", "150%", 
        "300%", "500%"), include.lowest = TRUE)
    
    return(in_mat)
}

frac_2013 <- tot_2013/mean_hist
frac_2013 <- Fn_Discretize_Fraction(frac_2013)

gfx_frac <- ggplot(data = frac_2013)
gfx_frac <- gfx_frac + geom_raster(aes(Var1, Var2, fill = colorScale))
gfx_frac <- gfx_frac + scale_fill_brewer(palette = "RdYlBu", drop = FALSE)
gfx_frac <- gfx_frac + theme(axis.text = element_blank(), axis.ticks = element_blank())
gfx_frac <- gfx_frac + labs(x = NULL, y = NULL, fill = "% Historical Avg")
gfx_frac <- gfx_frac + ggtitle("Rainfall in 2013 as Percentage of Long-term Historical Average (1895-2012)")

png("gfx_frac_longterm.png", width = 700, height = 450)
plot(gfx_frac)
garbage <- dev.off()
```

![figure 3] [prism3]

Plot 2013 total as fraction of the near-term mean. Visually, not very different from the previous map.


```r
frac_2013 <- tot_2013/mean_hist_recent
frac_2013 <- Fn_Discretize_Fraction(frac_2013)

gfx_frac <- ggplot(data = frac_2013)
gfx_frac <- gfx_frac + geom_raster(aes(Var1, Var2, fill = colorScale))
gfx_frac <- gfx_frac + scale_fill_brewer(palette = "RdYlBu", drop = FALSE)
gfx_frac <- gfx_frac + theme(axis.text = element_blank(), axis.ticks = element_blank())
gfx_frac <- gfx_frac + labs(x = NULL, y = NULL, fill = "% Historical Avg")
gfx_frac <- gfx_frac + ggtitle("Rainfall in 2013 as Percentage of Near-term Historical Average (1983-2012)")

png("gfx_frac_nearterm.png", width = 700, height = 450)
plot(gfx_frac)
garbage <- dev.off()
```

![figure 4] [prism4]


[prism1]: gfx_2013_total.png "figure 1"
[prism2]: gfx_mean_longterm.png "figure 2"
[prism3]: gfx_frac_longterm.png "figure 3"
[prism4]: gfx_frac_nearterm.png "figure 4"
