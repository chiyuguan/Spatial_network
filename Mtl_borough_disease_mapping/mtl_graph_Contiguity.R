## use traditional contiguity based neighbourhood matrix as the benchmark spatial model (rather based on graph edge mobility neighbour)
### generated weight mx

library(spdep)
nb <- spdep::poly2nb(mtl, queen = TRUE)
head(nb) # For nb[[i]], it will list the indices of the polygons that are adjacent to the i-th polygon.
# plot(st_geometry(mtl), border = "lightgray")
# plot.nb(nb, st_geometry(mtl), add = TRUE)
# nb

# Convert neighborhood list to spatial weights matrix
weights_matrix <- spdep::nb2mat(nb, style = "B", zero.policy = TRUE)

colnames(weights_matrix) <- rownames(weights_matrix)

# Order the row and column names
ordered_row_names <- rownames(weights_matrix)[order(as.numeric(rownames(weights_matrix)))]
ordered_col_names <- ordered_row_names

# Reorder the matrix
weights_matrix <- weights_matrix[ordered_row_names, ordered_col_names]


weights_matrix
