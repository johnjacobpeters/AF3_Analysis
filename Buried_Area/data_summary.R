library(tidyverse)          # ggplot2, readr, dplyr, tidyr, etc.

# ────────────────────────────────────────────────────────────
# 1.  Load & reshape data
# ────────────────────────────────────────────────────────────
df <- read_csv("interface_areas_ternary_fixed.csv",
               show_col_types = FALSE) %>%
  rename(model = 1)

data_long <- df %>%
  pivot_longer(-model, names_to = "Interface", values_to = "Area") %>%
  drop_na()

# ────────────────────────────────────────────────────────────
# 2.  Means & SDs for annotations
# ────────────────────────────────────────────────────────────
stats_tbl <- data_long %>%
  group_by(Interface) %>%
  summarise(mean = mean(Area),
            sd   = sd(Area),
            ymax = max(Area),           # highest observed value
            .groups = "drop") %>%
  mutate(label_y = pmax(ymax, mean + sd) + 0.03 * max(data_long$Area),
         label   = sprintf("mean = %.1f\ns.d. = %.1f", mean, sd))

# ────────────────────────────────────────────────────────────
# 3.  Combined bar-plot
# ────────────────────────────────────────────────────────────
p <- ggplot(data_long,
            aes(x = Interface, y = Area, fill = Interface)) +
  
  ## bar = mean
  stat_summary(fun = mean,
               geom = "bar",
               width = 0.65,
               colour = "black") +
  
  ## error bar = ± SD
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar",
               width = 0.25, colour = "black") +
  
  ## every raw point
  geom_jitter(width = 0.20, size = 2, alpha = 0.85, colour = "black") +
  
  ## μ & σ text
  geom_text(data = stats_tbl,
            aes(x = Interface, y = label_y, label = label),
            vjust = 0, size = 3.5, fontface = "italic") +
  
  ## cosmetics
  scale_fill_brewer(palette = "Set2", name = "Interface") +
  labs(title = "",
       y = "Area (Å²)", x = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x  = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        legend.position = "none",            # drop legend (colours self-explain)
        plot.title = element_text(size = 14, face = "bold"))

print(p)          # opens in the active graphics device
