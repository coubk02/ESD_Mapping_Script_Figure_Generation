# =============================================================================
# SOS-UK ESD Mapping — SDG "Wedding Cake" as an actual tiered cake silhouette
# Deliberately SEPARATE from the main mapping script, so this stays easy to
# tweak on its own without risking the rest of the story-arc plots.
#
# Unlike every other heatmap in the main script, this one collapses time
# entirely — one mean score per SDG for the WHOLE YEAR — because that's what
# actually makes it a different kind of figure rather than a re-styled copy
# of the week-by-week heatmap.
#
# Tier order (base to top), matching the real Stockholm Resilience Centre
# diagram: Biosphere (widest, base) -> Society (middle) -> Economy (narrower,
# near the top) -> Partnerships / SDG 17 (topper, smallest).
# =============================================================================

# install.packages(c("readxl", "tidyverse", "plotly", "htmlwidgets"))  # uncomment on first run
library(readxl)
library(tidyverse)
library(plotly)
library(htmlwidgets)

DATA_FILE <- "[FILE PATH]"

# Same colour language as the main script, so this figure still reads as part
# of the same story even though it lives in its own file.
SCALE_LOW  <- "#ECEAE3"
SCALE_MID  <- "#A9D3C8"
SCALE_HIGH <- "#2F6F6B"

# ---- 1. Load data & compute one mean score per SDG for the whole year --------

long_sdg <- read_excel(DATA_FILE, sheet = "Long_SDG")

sdg_year_scores <- long_sdg %>%
  filter(!is.na(score)) %>%
  group_by(sdg_number, sdg_name, sdg_tier) %>%
  summarise(mean_score = mean(score), n_entries = n_distinct(entry_id), .groups = "drop") %>%
  mutate(
    # SDG 17 pulled out as its own standalone group, same override logic as
    # the main script's wedding_cake_heatmap — doesn't touch sdg_tier itself,
    # just how THIS plot groups it.
    cake_group = if_else(sdg_number == 17, "Partnership", sdg_tier),
    cake_group = factor(cake_group, levels = c("Biosphere", "Society", "Economy", "Partnership"))
  ) %>%
  arrange(cake_group, sdg_number)

# ---- 2. Lay out the cake geometry ---------------------------------------------
# Tier widths are STYLISED, not proportional to how many SDGs are in each tier
# — Society has the most SDGs (8) but sits in the middle-width tier, matching
# the real diagram's proportions rather than a data-driven width.

tier_specs <- tibble(
  cake_group  = factor(c("Biosphere", "Society", "Economy", "Partnership"),
                       levels = c("Biosphere", "Society", "Economy", "Partnership")),
  full_width  = c(10, 7.2, 4.6, 1.8),
  tier_height = c(1.5, 1.5, 1.5, 0.9),
  gap_above   = c(0.12, 0.12, 0.12, 0)   # thin gap between tiers, like a cake board line
)

# Stack tiers bottom-to-top: cumulative y position for each tier's base
tier_specs <- tier_specs %>%
  arrange(cake_group) %>%
  mutate(
    y_bottom = lag(cumsum(tier_height + gap_above), default = 0),
    y_top    = y_bottom + tier_height
  )

# Within each tier, divide its full_width into N equal segments (one per SDG
# in that tier), centred on x = 0 so the whole thing tapers symmetrically.
cake_layout <- sdg_year_scores %>%
  left_join(tier_specs, by = "cake_group") %>%
  group_by(cake_group) %>%
  mutate(
    n_in_tier   = n(),
    seg_width   = full_width / n_in_tier,
    seg_index   = row_number() - 1,
    xmin        = -full_width / 2 + seg_index * seg_width,
    xmax        = xmin + seg_width,
    x_centre    = (xmin + xmax) / 2
  ) %>%
  ungroup() %>%
  mutate(
    tooltip = paste0("<b>SDG ", sdg_number, ": ", sdg_name, "</b><br>",
                     "Tier: ", cake_group, "<br>",
                     "Mean score (whole year): ", round(mean_score, 2), " / 2<br>",
                     "n = ", n_entries, " entries")
  )

tier_label_data <- tier_specs %>%
  mutate(y_mid = (y_bottom + y_top) / 2, x_label = -6.4)

# ---- 3. Build the cake --------------------------------------------------------

wedding_cake_tiered <- ggplot(cake_layout) +
  geom_rect(
    aes(xmin = xmin, xmax = xmax, ymin = y_bottom, ymax = y_top,
        fill = mean_score, text = tooltip),
    colour = "#FAF9F5", linewidth = 0.6   # thin pale border between segments, like piped icing lines
  ) +
  # Outer tier outline — one bold rect per tier, drawn on top, gives each
  # layer a clean "iced edge" rather than just touching coloured blocks
  geom_rect(
    data = tier_specs,
    aes(xmin = -full_width / 2, xmax = full_width / 2, ymin = y_bottom, ymax = y_top),
    fill = NA, colour = "#1C4A47", linewidth = 0.7
  ) +
  # SDG number inside each segment — short label, fits even in the narrow
  # Society/Economy segments. Full title is in the tooltip / reference key.
  geom_text(
    aes(x = x_centre, y = (y_bottom + y_top) / 2, label = sdg_number),
    size = 3, colour = "#1C2A2E", fontface = "bold"
  ) +
  # Tier names to the left of each layer
  geom_text(
    data = tier_label_data,
    aes(x = x_label, y = y_mid, label = cake_group),
    hjust = 1, size = 4, fontface = "bold", colour = "#1C4A47"
  ) +
  scale_fill_gradient2(
    low = SCALE_LOW, mid = SCALE_MID, high = SCALE_HIGH, midpoint = 1,
    limits = c(0, 2), name = "Mean SDG\nscore (0-2)\nwhole year"
  ) +
  coord_fixed(ratio = 1, clip = "off") +
  labs(
    title = "ESD coverage across the year, as an SDG wedding cake",
  ) +
  theme_void(base_family = "sans") +
  theme(
    plot.title = element_text(size = 14, face = "bold", colour = "#1C4A47", hjust = 0.5,
                              margin = margin(b = 4)),
    plot.subtitle = element_text(size = 9.5, colour = "#6B7472", hjust = 0.5,
                                 margin = margin(b = 14)),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    plot.margin = margin(t = 15, r = 20, b = 15, l = 70)   # room for tier name labels on the left
  )

print(wedding_cake_tiered)
ggsave("ESD Mapping/Mapping/wedding_cake_tiered.png", wedding_cake_tiered, width = 10, height = 9, dpi = 200)

# ---- 4. Reference key: SDG number -> full title, printed for the record ------
sdg_key <- cake_layout %>%
  distinct(sdg_number, sdg_name, cake_group) %>%
  arrange(cake_group, sdg_number)
print(sdg_key, n = 17)

# ---- 5. Interactive version with hover tooltips -------------------------------
# ggplotly doesn't render geom_text/decorative geom_point cleanly in every
# case with coord_fixed — if the layout looks off in the interactive version,
# the static PNG above is the one to trust for anything you're presenting.
wedding_cake_tiered_interactive <- ggplotly(wedding_cake_tiered, tooltip = "text")
wedding_cake_tiered_interactive
saveWidget(wedding_cake_tiered_interactive, "ESD Mapping/Mapping/wedding_cake_tiered.html", selfcontained = TRUE)

# =============================================================================
# Tuning notes:
# - Tier proportions (full_width in tier_specs) are stylised to match the
#   real diagram's LOOK, not derived from data. Adjust those four numbers
#   directly if you want a more/less dramatic taper.
# - seg_width divides each tier evenly by SDG COUNT, not by score — a tier
#   with more SDGs gets narrower individual segments, which is why Society's
#   8 segments look thinner than Biosphere's 4 even though Biosphere's tier
#   is visually wider overall.
# - Delete the "cherry on top" geom_point() block (Section 3) if you'd rather
#   the Partnership topper stay plain.
# =============================================================================
