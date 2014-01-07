# analyze monthly data downloaded from PRISM

library(rgdal)

# function to rotate a matrix 90 degress clockwise for plotting only
# used to counteract the "image" function default behavior
Fn_Rotate_Matrix <- function(mat) {
  return (t(mat)[, nrow(mat):1])
}

# read data and data dimensions
prism_data <- readGDAL("data/PRISM_ppt_stable_4kmM2_1895_bil.bil")

prism_cols <- prism_data@grid@cells.dim[1]
prism_rows <- prism_data@grid@cells.dim[2]

# reshape the data for a lat-lon matrix
ann_tot <- prism_data@data$band1
ann_tot <- matrix(ann_tot, nrow = prism_rows, ncol = prism_cols, byrow = TRUE)

# plot data
# image(ann_tot)
image(Fn_Rotate_Matrix(ann_tot))


