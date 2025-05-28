THRESH_PRESENT <- 25     # Å² ≥ this = “interface present”
N_CLUSTERS     <- 3      # change if you decide there are 4, 5…

# ───────────────────────────────────────────────────────────────
# 1.  Load & threshold to binary  ───────────────────────────────
# ───────────────────────────────────────────────────────────────
library(tidyverse)       # dplyr, readr, ggplot2 …
library(proxy)           # Jaccard distance
library(pheatmap)        # heat-map
library(dendextend)      # nicer dendrograms

df <- read_csv("S47_syt7_interfaces_FL.csv", show_col_types = FALSE) %>%
  rename(model = 1)

mat_bin <- df %>%
  select(-model) %>%
  mutate(across(everything(), ~ as.integer(.x >= THRESH_PRESENT))) %>%
  as.matrix()

rownames(mat_bin) <- df$model
interface_df <- df 

model_ids   <- interface_df$model
mat_numeric <- interface_df %>% select(-model)
# ───────────────────────────────────────────────────────────────
# 2.  Binary distance + hierarchical clustering ─────────────────
# ───────────────────────────────────────────────────────────────
dist_bin <- dist(mat_bin, method = "Jaccard")        # 1 − |∩|/|∪|
hc        <- hclust(dist_bin, method = "ward.D2")    # Ward on Jaccard

clusters  <- cutree(hc, k = N_CLUSTERS)

# ───────────────────────────────────────────────────────────────
# 3.  Quick silhouette check (optional)  ────────────────────────
# ───────────────────────────────────────────────────────────────
library(cluster)
sil <- silhouette(clusters, dist_bin)
mean_sil <- mean(sil[, "sil_width"])
message(sprintf("Average silhouette width = %.3f", mean_sil))

# ───────────────────────────────────────────────────────────────
# 4.  Dendrogram + heat-map side-by-side  ───────────────────────
# ───────────────────────────────────────────────────────────────
dend <- as.dendrogram(hc) %>% 
  color_branches(k = N_CLUSTERS)              # colour by cluster

annotation_row <- data.frame(Cluster = factor(clusters))
rownames(annotation_row) <- df$model

pheatmap(mat_bin,
         cluster_rows   = hc,            # <— use hc, not dend
         cluster_cols   = FALSE,
         annotation_row = annotation_row,
         show_rownames  = FALSE,
         legend         = TRUE,
         main = "Interface presence (1) / absence (0)",
         cutree_rows    = N_CLUSTERS)    # draws coloured bars for clusters


# ───────────────────────────────────────────────────────────────
# 5.  Export tidy table of results  ─────────────────────────────
# ───────────────────────────────────────────────────────────────
tidy_out <- df %>%
  mutate(cluster = clusters) %>%
  pivot_longer(-c(model, cluster),
               names_to  = "interface",
               values_to = "area") %>%
  mutate(state = if_else(area >= THRESH_PRESENT, 1L, 0L))

write_csv(tidy_out, "model_interface_clusters_binary.csv")




# ────────────────────────────────────────────────────────────────
# 1.  Build the numeric matrix (already have this)  ─────────────
# ────────────────────────────────────────────────────────────────
mat_abs <- as.matrix(mat_numeric)      # raw Å² values
rownames(mat_abs) <- model_ids

## OPTIONAL: log10-transform to compress very large areas
# mat_abs <- log10(mat_abs + 1)        # +1 to keep 0 → 0

# ────────────────────────────────────────────────────────────────
# 2.  Cluster rows on absolute data  (Euclidean + Ward)  ────────
# ────────────────────────────────────────────────────────────────
dist_abs <- dist(mat_abs, method = "euclidean")
hc_abs   <- hclust(dist_abs, method = "ward.D2")

row_anno   <- data.frame(Cluster = factor(cutree(hc_abs, k = N_CLUSTERS)))
rownames(row_anno) <- model_ids

# ────────────────────────────────────────────────────────────────
# 3.  Continuous heat-map  ───────────────────────────────────────
# ────────────────────────────────────────────────────────────────
library(pheatmap)

pheatmap(mat_abs,
         cluster_rows   = hc_abs,
         cluster_cols   = FALSE,             # keep columns in file order
         annotation_row = row_anno,
         show_rownames  = FALSE,             # set TRUE if you want IDs
         color          = colorRampPalette(c("white", "steelblue", "darkred"))(100),
         legend         = TRUE,
         main = "Buried interface area (Å²)"
)

