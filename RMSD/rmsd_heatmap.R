# ---------- rmsd_heatmap.R -------------------------------------------
# Requirements:  pheatmap  +  (readr OR base R read.csv)
# install.packages("pheatmap")   # if you don’t already have it

library(readr)      # much faster than base read.csv; remove if you like
library(pheatmap)

# ---- 1. load the CSV --------------------------------------------------
# Replace the path below with the actual file location
csv_path <- "rmsd_matrix.csv"

df  <- read_csv(csv_path, show_col_types = FALSE)   # first column = model names
mat <- as.matrix(df[ , -1])                         # numeric part
rownames(mat) <- df[[1]]

# ---- 2. tidy up -------------------------------------------------------
# If NA/blank values exist (e.g. skipped pairs), replace with max RMSD
if (anyNA(mat))
  mat[is.na(mat)] <- 0

# Symmetrize just in case (upper-triangle written but not lower, etc.)
mat <- (mat + t(mat)) / 2

# ---- 3. clustering ----------------------------------------------------
# Use the RMSD matrix itself as a distance object
dists <- as.dist(mat)                 # converts upper triangle → dist

hc <- hclust(dists, method = "average")   # UPGMA; try "ward.D2" if you prefer

# ---- 4. heat-map ------------------------------------------------------
pheatmap(
  mat,
  clustering_distance_rows = dists,     # rows & cols use same distance
  clustering_distance_cols = dists,
  clustering_method       = "average",
  treeheight_row = 40,
  treeheight_col = 40,
  main = "All-vs-All RMSD (Å)",
  fontsize = 7,                         # shrinks labels for many rows
  border_color = NA                     # cleaner look
)

# ---- 5. (optional) cut into k clusters and inspect --------------------
k <- 5                                  # adjust based on dendrogram
clusters <- cutree(hc, k)
print(table(clusters))                  # how many structures per cluster
# write.csv(data.frame(model = names(clusters), cluster = clusters),
#           "rmsd_clusters.csv", row.names = FALSE)

# ----------------------------------------------------------------------
