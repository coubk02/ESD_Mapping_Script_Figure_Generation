# =============================================================================
# SOS-UK ESD Curriculum Mapping — BMS Programme
# Reads the cleaned/tidy workbook (SOS_mapping_cleaned.xlsx) and reproduces
# the story-arc visuals: submission timeline, week-on-week SDG heatmap,
# teaching/learning methods heatmap, and an SDG "wedding cake" tier summary.
#
# Data expected at: same folder as this script, file "SOS_mapping_cleaned.xlsx"
# Sheets used: Cleaned_Wide, Long_SDG, Long_Methods  (see 'Codebook' sheet for
# full column definitions, assumptions, and the 0/1/2 scoring scale)
# =============================================================================

# install.packages(c("readxl", "tidyverse", "plotly", "htmlwidgets", "ggh4x"))  # uncomment on first run
library(readxl)
library(tidyverse)
library(plotly)   # gives hover tooltips on any ggplot via ggplotly()
library(htmlwidgets)   # needed to export interactive plots as standalone HTML (saveWidget)
library(ggh4x)   # needed for per-panel facet strip colours (used in the programme heatmap)

DATA_FILE <- "[FILE PATH].xlsx"

# Shared visual language — keep this identical across every chart in the story
# so a reader learns the encoding once (matches the HTML prototype's palette).
SCALE_LOW  <- "#ECEAE3"   # 0 - not covered
SCALE_MID  <- "#A9D3C8"   # 1 - included a little (implicit)
SCALE_HIGH <- "#2F6F6B"   # 2 - included a lot (explicit)

# One fixed programme -> colour mapping, defined ONCE and reused everywhere a
# programme needs a colour (2b's dot colours, and the facet strip colours on
# the programme heatmap). This guarantees a colour always means the same
# programme across every plot in the script, rather than each plot picking
# its own colours independently (which risks the same colour meaning two
# different programmes in two different charts).
PROGRAMME_COLOURS <- c(
  "BSc Biomedical Science"             = "#1B9E77",
  "BSc Immunology and Pharmacology"    = "#D95F02",
  "BSc Biochemistry and Pharmacology"  = "#7570B3",
  "MSci Immunology"                    = "#E7298A",
  "MSci Pharmacology"                  = "#66A61E",
  "BSc Immunology and Microbiology"    = "#E6AB02"
)

# Year of study is ORDINAL (Year 1 < Year 2 < Year 3 < Year 4), unlike
# programme, which is just categorical — so this uses a sequential ramp
# (light to dark) rather than qualitative colours like PROGRAMME_COLOURS.
# Reader intuition: darker = further into the degree.
YEAR_COLOURS <- c(
  "Year 1" = "#C3CA08",
  "Year 2" = "#9BC43C",
  "Year 3" = "#537B2F",
  "Year 4" = "#2D5128"
)
theme_sos <- function() {
  theme_minimal(base_family = "sans") +
    theme(
      panel.grid = element_blank(),
      axis.text = element_text(size = 9, colour = "#1C2A2E"),
      axis.title.x = element_text(size = 9, colour = "#1C2A2E", margin = margin(t = 8)),   # bigger + extra top margin so it doesn't sit tight against the week-number tick labels
      axis.title.y = element_text(size = 9, colour = "#1C2A2E", margin = margin(r = 8)),   # bigger + extra right margin, same reasoning on the y side
      plot.title = element_text(size = 14, face = "bold", colour = "#1C4A47"),
      plot.subtitle = element_text(size = 9, colour = "#6B7472"),
      strip.text = element_text(size = 9, face = "bold", colour = "#1C4A47"),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8)
    )
}
# academic_week reference: 1-13 = Semester 1, 14-27 = Semester 2 (wk 14 = Sem2 wk1)
sem2_start <- 14

# ---- 1. Load data -------------------------------------------------------------

wide        <- read_excel(DATA_FILE, sheet = "Cleaned_Wide")
long_sdg    <- read_excel(DATA_FILE, sheet = "Long_SDG")
long_methods <- read_excel(DATA_FILE, sheet = "Long_Methods")

# Optional cohort filter for the years-1/2/3/4 story once year_of_study is backfilled:
#wide         <- wide         %>% filter(year_of_study %in% c("Year 1", "Year 2", "Year 3", "Year 4"))
#long_sdg     <- long_sdg     %>% filter(year_of_study %in% c("Year 1", "Year 2", "Year 3", "Year 4"))
#long_methods <- long_methods %>% filter(year_of_study %in% c("Year 1", "Year 2", "Year 3", "Year 4"))

# Order SDGs 1-17 for consistent axis ordering (Long_SDG already carries sdg_number)
long_sdg <- long_sdg |>
  mutate(sdg_name = fct_reorder(sdg_name, sdg_number))
# ---- 1a. Standardise year_of_study --------------------------------------------
# THIS IS WHY THE COHORT FILTER WAS BREAKING: filter(year_of_study %in% c("Year 1", ...))
# only matches if the text in Excel is EXACTLY "Year 1" (capitalisation, spacing,
# "Year" vs "Yr" vs a bare "1" all count as different values to R). A single
# mismatch anywhere in the column means every row silently fails to match,
# filter() returns zero rows, and every plot downstream renders blank with
# no error — which is exactly what you were seeing.
#
# Fix: pull out just the digit from whatever was typed (handles "Year 1",
# "year1", "Yr 1", "1", etc.) and rebuild it into one consistent label. This
# makes the filter work regardless of exactly how it was typed into Excel.
standardise_year <- function(df) {
  df %>%
    mutate(year_of_study = if_else(
      is.na(year_of_study) | str_detect(as.character(year_of_study), "\\d") == FALSE,
      NA_character_,
      paste0("Year ", str_extract(as.character(year_of_study), "\\d+"))
    ))
}
wide <- standardise_year(wide)

# ---- 1a-ii. Propagate year_of_study into Long_SDG and Long_Methods -----------
# You only filled year_of_study in on the Cleaned_Wide sheet — Long_SDG and
# Long_Methods were built from Cleaned_Wide before that, so their own
# year_of_study columns are still blank. No need to touch those sheets in
# Excel at all: entry_id is already the same row-identifier in all three
# sheets (it's how they were built in the first place), so this drops each
# sheet's stale/blank year_of_study and looks the correct one up from wide's
# freshly-standardised column instead — matching on entry_id, not name, since
# entry_id is guaranteed unique per row where a name can repeat across entries.
year_lookup <- wide %>% select(entry_id, year_of_study)

long_sdg <- long_sdg %>%
  select(-year_of_study) %>%
  left_join(year_lookup, by = "entry_id")

long_methods <- long_methods %>%
  select(-year_of_study) %>%
  left_join(year_lookup, by = "entry_id")

# DIAGNOSTIC — confirm the propagation worked and see exactly what values
# exist before writing any filter:
print(table(wide$year_of_study, useNA = "ifany"))
print(table(long_sdg$year_of_study, useNA = "ifany"))

# Optional cohort filter — now safe to use, since year_of_study is guaranteed
# to be one of "Year 1".."Year 4" or NA regardless of how it was typed:
# wide         <- wide         %>% filter(year_of_study %in% c("Year 1", "Year 2", "Year 3", "Year 4"))
# long_sdg     <- long_sdg     %>% filter(year_of_study %in% c("Year 1", "Year 2", "Year 3", "Year 4"))
# long_methods <- long_methods %>% filter(year_of_study %in% c("Year 1", "Year 2", "Year 3", "Year 4"))

# Order SDGs 1-17 for consistent axis ordering (Long_SDG already carries sdg_number)
long_sdg <- long_sdg |>
  mutate(sdg_name = fct_reorder(sdg_name, sdg_number))

# ---- 1b. Anonymise student names --------------------------------------------
# Builds one lookup table (real name -> "Student N") and applies it identically
# across all three dataframes, so "Student 7" refers to the same person
# everywhere. Numbering is by EARLIEST MAPPED WEEK (Student 1 = whoever's
# first entry falls earliest in the academic year), not row order in the
# source spreadsheet and not alphabetical — this makes the anonymised ID
# double as the timeline's natural reading order, so Section 2's plots need
# no separate ordering step. Ties (same first week) break alphabetically by
# name, purely for a deterministic, reproducible result on every re-run.

student_lookup <- wide |>
  filter(!is.na(name_clean)) |>
  group_by(name_clean) |>
  summarise(first_week = min(academic_week, na.rm = TRUE), .groups = "drop") |>
  arrange(first_week, name_clean) |>
  mutate(student_id = paste0("Student ", row_number()))

# KEEP THIS SOMEWHERE SAFE, SEPARATE FROM ANY ANONYMISED OUTPUT YOU SHARE.
# It's the only thing that lets you (or SOS-UK) map back to real names later
# if needed. Don't commit it alongside plots/exports meant to be anonymous.
write_csv(student_lookup, "student_lookup_CONFIDENTIAL.csv")

anonymise_names <- function(df) {
  df |>
    left_join(student_lookup, by = "name_clean") |>
    mutate(
      name_clean = factor(
        student_id,
        levels = paste0("Student ", seq_len(nrow(student_lookup)))  # keeps numeric order, not "Student 10" < "Student 2"
      )
    ) |>
    select(-student_id) |>
    select(-any_of("name_original"))   # drop the real name column entirely
}

wide         <- anonymise_names(wide)
long_sdg     <- anonymise_names(long_sdg)
long_methods <- anonymise_names(long_methods)

# ---- 1c. Relabel T&L categories: Sustainability Learning vs ESD Methods ------
# Same 13 items, same underlying `category` split (learning_outcome /
# teaching_method) — this just renames the two groups for display. Kept as
# its own recode step (rather than hand-editing labels inside each plot) so
# every plot that uses it — currently 4c and 4d — stays in sync automatically
# if the naming changes again later.
esd_group_labels <- c(
  "learning_outcome" = "Sustainability Learning",
  "teaching_method"  = "ESD Methods"
)

long_methods <- long_methods %>%
  mutate(esd_group = recode(category, !!!esd_group_labels))
# ---- 1d. Abbreviated SDG axis labels ------------------------------------------
# Full SDG titles are the right level of detail for a tooltip but crowd a
# y-axis fast across 17 rows. sdg_label swaps to "SDG 1".."SDG 17" for the
# axis, built directly from sdg_number/sdg_name (already correctly paired in
# the data, so nothing needs to be typed by hand) — full titles stay
# available via sdg_name wherever that's still used (e.g. tooltips).
# Deliberately NOT applied to the wedding-cake heatmap (Section 5), per your
# instruction to leave that one showing full titles.
sdg_label_levels <- paste0("SDG ", 1:17)
long_sdg <- long_sdg %>%
  mutate(sdg_label = factor(paste0("SDG ", sdg_number), levels = sdg_label_levels))

# Reference key — SDG number to full title, printed once so the mapping is
# visible in the console/script output even though the axes now just show
# the number:
sdg_key <- long_sdg %>% distinct(sdg_number, sdg_name) %>% arrange(sdg_number)
print(sdg_key, n = 17)

# ---- 2. Entry timeline (dot-strip: who submitted, and when) -----------------
# Mirrors the "coverage" panel in the prototype — one dot per submitted entry.
# Four variants below, same section, same data. 2a is the plain baseline;
# 2b/2c/2d are alternates — un-comment ONE at a time and re-run, or keep the
# active one as `timeline_plot` and treat the rest as reference. Do not run
# all four blocks back to back without renaming the object — each overwrites
# `timeline_plot`.

# order_students now comes straight from the anonymisation step above — the
# "Student N" numbering IS earliest-mapped-week order, so the factor's levels
# are already in the right sequence for the y-axis. No separate calculation
# needed (and this guarantees the axis order can never drift out of sync with
# the numbering, which a second independent calculation could risk).
order_students <- levels(wide$name_clean)

# -- 2a + academic calendar context (semester starts and exam weeks)
# Dates cross-checked against the department's timetable and match the
# week_start_date column exactly. NOTE: this specific timetable is the
# final-year project/dissertation calendar (draft intro, draft thesis,
# presentations) — it's most meaningful for the years-3/4 project cohort,
# not necessarily representative of every year group's taught-module weeks.
# Scope this to that cohort once year_of_study is backfilled, e.g. by
# filtering `wide` to year_of_study %in% c(3, 4) before this block.
#
# Vacation/consolidation weeks (Christmas break, w/c 12 Jan) aren't shown as
# bands here because they don't exist on this axis at all — academic_week
# skips straight from 13 to 14, so there's no x-position to shade for them.

exam_presentation_weeks <- tibble(xmin = 11.5, xmax = 13.5)  # Sem 1 exams (wk12) + presentations (wk13)

project_milestones <- tribble(
  ~academic_week, ~label,
  1,  "Start of Sem 1",
  12, "Sem 1 exams",
  14, "Start of Sem 2",
)

timeline_plot_2a <- wide %>%
  filter(!is.na(academic_week)) %>%
  mutate(name_clean = factor(name_clean, levels = order_students)) %>%
  ggplot(aes(x = academic_week, y = name_clean)) +
  geom_rect(
    data = exam_presentation_weeks, inherit.aes = FALSE,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    fill = "grey85", alpha = 0.5
  ) +
  geom_vline(xintercept = sem2_start - 0.5, linetype = "dashed", colour = "#1C4A47") +
  geom_vline(data = project_milestones, aes(xintercept = academic_week),
             linetype = "dotted", colour = "#6B7472", linewidth = 0.4) +
  geom_point(colour = "#2F6F6B", alpha = 0.85, size = 2.6) +   # plain single colour — semester/calendar structure is the point, not programme (that's the separate 2b/2f figures)
  geom_text(
    data = project_milestones,
    aes(x = academic_week, y = Inf, label = label),
    inherit.aes = FALSE, angle = 45, hjust = -0.03, vjust = -0.01,
    size = 2.7, colour = "#6B7472"
  ) +
  scale_x_continuous(breaks = seq(1, 27, 1), limits = c(0.5, 27.5), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(x = "Academic week", y = "Student Number") +
  theme_sos() +
  theme(plot.margin = margin(t = 50, r = 3, b = 15, l = 7))   # room for the rotated labels above the plot

print(timeline_plot_2a)
ggsave("ESD Mapping/Mapping/semester_timeline_plot_fig1.png", timeline_plot_2a, width = 10, height = 6, dpi = 600)

# -- 2b. Coloured by programme — cheapest upgrade, shows cohort clustering ------
timeline_plot_2b <- wide |>
  filter(!is.na(academic_week)) |>
  mutate(name_clean = factor(name_clean, levels = order_students)) |>
  ggplot(aes(x = academic_week, y = name_clean, colour = programme_clean)) +
  geom_vline(xintercept = sem2_start - 0.5, linetype = "dashed", colour = "#1C4A47") +
  geom_point(alpha = 0.85, size = 2.6) +
  scale_colour_manual(values = PROGRAMME_COLOURS, name = "Programme") +
  scale_x_continuous(breaks = seq(1, 27, 1), limits = c(0.5, 27.5), expand = c(0, 0)) +
  labs(x = "Academic week", y = "Student Number") +
  theme_sos()

print(timeline_plot_2b)
ggsave("ESD Mapping/Mapping/programme_entry_timeline_plot_fig2b.png", timeline_plot_2b, width = 10, height = 6, dpi = 600)
# -- 2c. Combined: tile squares shaded by mean SDG score ------
# Same colour scale as the SDG heatmap in Section 3, so this timeline and that
# heatmap read as one consistent visual language.
entry_scores <- long_sdg %>%
  filter(!is.na(score)) %>%
  group_by(entry_id, name_clean, academic_week) %>%
  summarise(mean_score = mean(score), .groups = "drop")

timeline_plot_2c <- wide %>%
  filter(!is.na(academic_week)) %>%
  left_join(entry_scores, by = c("entry_id", "name_clean", "academic_week")) %>%
  mutate(name_clean = factor(name_clean, levels = order_students)) %>%
  ggplot(aes(x = academic_week, y = name_clean, fill = mean_score)) +
  geom_tile(colour = "#F7F5F0", linewidth = 0.4) +
  geom_vline(xintercept = sem2_start - 0.5, linetype = "dashed", colour = "#1C4A47") +
  scale_fill_gradient2(low = SCALE_LOW, mid = SCALE_MID, high = SCALE_HIGH, midpoint = 1,
                       limits = c(0, 2), name = "Mean SDG\nscore (0-2)") +
  scale_x_continuous(breaks = seq(1, 27, 1), limits = c(0.5, 27.5), expand = c(0, 0)) +
  labs(x = "Academic week", y = "Student Number") +
  theme_sos()

print(timeline_plot_2c)
ggsave("ESD Mapping/Mapping/sdg_entry_tiles_fig5a.png", timeline_plot_2c, width = 10, height = 6, dpi = 600)
# -- 2d. Cumulative-engagement trend (separate figure, not a timeline_plot swap) -
# Different question ("did engagement build or taper") — keep as its own object.
cumulative_engagement_plot <- wide %>%
  filter(!is.na(academic_week), !is.na(programme_clean)) %>%
  count(programme_clean, academic_week) %>%
  group_by(programme_clean) %>%
  arrange(academic_week) %>%
  mutate(cumulative = cumsum(n)) %>%
  ggplot(aes(x = academic_week, y = cumulative, colour = programme_clean)) +
  geom_step(linewidth = 1) +
  scale_colour_manual(values = PROGRAMME_COLOURS, name = "Programme") +
  scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0,100)) + # explicit breaks so the top of the range (~90+) gets a labelled gridline, not just wherever ggplot's default "nice number" picker stopped    
  scale_x_continuous(breaks = seq(0, 30, 5), limits =c(0,30)) +
  labs(y = "Cumulative Entries", x = "Academic week") +
  theme_sos()

print(cumulative_engagement_plot)
ggsave("ESD Mapping/Mapping/cumulative_engagement_3a.png", cumulative_engagement_plot, width = 10, height = 6, dpi = 600)
# ---- 3. Week-on-week SDG heatmap ---------------------------------------------
# Mean score (0/1/2) per SDG per week, across all students (or a filtered cohort).

sdg_heat_data <- long_sdg %>%
  filter(!is.na(academic_week), !is.na(score)) %>%
  group_by(academic_week, sdg_label) %>%
  summarise(mean_score = mean(score), n_entries = n_distinct(entry_id), .groups = "drop") %>%
  mutate(
    week_label = if_else(academic_week <= 14,
                         paste0("Sem 1, wk ", academic_week),
                         paste0("Sem 2, wk ", academic_week - 14)),
    tooltip = paste0("<b>", sdg_label, "</b><br>", week_label,
                     "<br>Mean score: ", round(mean_score, 2), " / 2",
                     "<br>n = ", n_entries, " entries")
  )

sdg_heatmap <- ggplot(sdg_heat_data, aes(x = academic_week, y = sdg_label, fill = mean_score, text = tooltip)) +
  geom_tile(colour = "#F7F5F0", linewidth = 0.3) +
  geom_vline(xintercept = sem2_start - 0.5, linetype = "dashed", colour = "#1C4A47") +
  scale_fill_gradient2(
    low = SCALE_LOW, mid = SCALE_MID, high = SCALE_HIGH, midpoint = 1,
    limits = c(0, 2), name = "Mean score\n(0-2)"
  ) +
  scale_x_continuous(breaks = seq(1, 27, 1), expand = c(0, 0)) +
  labs(x = "Academic week",
       y = "Student Number") +
  theme_sos()

print(sdg_heatmap)
ggsave("ESD Mapping/Mapping/mean_sdg_heatmap_fig5b.png", sdg_heatmap, width = 10, height = 6, dpi = 600)
# Interactive version with hover tooltips (SDG, week, mean score, n) — this is
# the direct equivalent of the hover behaviour in the HTML prototype:
sdg_heatmap_interactive <- ggplotly(sdg_heatmap, tooltip = "text")
sdg_heatmap_interactive
saveWidget(sdg_heatmap_interactive, "heatmap_sdg_by_week.html", selfcontained = TRUE)

# Faceted by programme (small multiples) — useful once you're comparing cohorts:
# Facet panels default to alphabetical order (since programme_clean is text).

# To control it yourself, set the order explicitly here — edit this vector to
# whatever order suits the story (by cohort size, degree type, or just
# whatever reads best next to each other). Currently ordered by entry count,
# largest cohort first:
# programme_order <- long_sdg %>%
#  filter(!is.na(programme_clean)) %>%
#  distinct(entry_id, programme_clean) %>%
#  count(programme_clean, sort = TRUE) %>%
#  pull(programme_clean)
# Or set it manually instead, e.g.:
programme_order <- c("BSc Immunology and Microbiology", "MSci Pharmacology", "MSci Immunology",
                     "BSc Biomedical Science", "BSc Immunology and Pharmacology","BSc Biochemistry and Pharmacology")

sdg_heatmap_by_programme <- long_sdg %>%
  filter(!is.na(academic_week), !is.na(score), !is.na(programme_clean)) %>%
  mutate(programme_clean = factor(programme_clean, levels = programme_order)) %>%
  group_by(programme_clean, academic_week, sdg_label) %>%
  summarise(mean_score = mean(score), .groups = "drop") %>%
  ggplot(aes(x = academic_week, y = sdg_label, fill = mean_score)) +
  geom_tile(colour = "#F7F5F0", linewidth = 0.2) +
  scale_fill_gradient2(low = SCALE_LOW, mid = SCALE_MID, high = SCALE_HIGH, midpoint = 1, limits = c(0, 2)) +
  scale_x_continuous(breaks = seq(1, 27, 3), expand = c(0, 0)) +
  
  # facet_wrap2 (ggh4x) instead of base facet_wrap — lets each strip take its
  # own fill colour. Strip colours pulled from the SAME PROGRAMME_COLOURS
  # mapping used in 2b/2e, in programme_order's sequence so each strip
  # matches the right panel.
  
  facet_wrap2(
    ~ programme_clean, ncol = 3,
    strip = strip_themed(
      background_x = elem_list_rect(fill = PROGRAMME_COLOURS[programme_order])
    )
  ) +
  labs(x = "Academic week", y = "Sustainable Development Goal") +
  theme_sos() +
  theme(
    axis.text.y = element_text(size = 10),
    strip.text = element_text(colour = "white", face = "bold")   # readable against the coloured strips
  )

print(sdg_heatmap_by_programme)
ggsave("ESD Mapping/Mapping/sdg_heatmap_by_programme_fig5d.png", sdg_heatmap_by_programme, width = 11, height = 6, dpi = 600)
# ---- 4. Teaching & learning methods heatmap -----------------------------------
# Same visual grammar as the SDG heatmap, split by category so skills and
# delivery methods don't get visually conflated.

methods_heat_data <- long_methods %>%
  filter(!is.na(academic_week), !is.na(score)) %>%
  group_by(academic_week, category, item_name) %>%
  summarise(mean_score = mean(score), n_entries = n_distinct(entry_id), .groups = "drop") %>%
  mutate(
    week_label = if_else(academic_week <= 14,
                         paste0("Sem 1, wk ", academic_week),
                         paste0("Sem 2, wk ", academic_week - 14)),
    category_label = if_else(category == "learning_outcome", "Sustainability Learning", "ESD Methods"),
    tooltip = paste0("<b>", item_name, "</b> (", category_label, ")<br>", week_label,
                     "<br>Mean score: ", round(mean_score, 2), " / 2",
                     "<br>n = ", n_entries, " entries")
  )

methods_heatmap <- ggplot(methods_heat_data, aes(x = academic_week, y = item_name, fill = mean_score, text = tooltip)) +
  geom_tile(colour = "#F7F5F0", linewidth = 0.3) +
  geom_vline(xintercept = sem2_start - 0.5, linetype = "dashed", colour = "#1C4A47") +
  scale_fill_gradient2(low = SCALE_LOW, mid = SCALE_MID, high = SCALE_HIGH, midpoint = 1, limits = c(0, 2)) +
  scale_x_continuous(breaks = seq(1, 27, 1), expand = c(0, 0)) +
  facet_grid(category ~ ., scales = "free_y", space = "free_y",
             labeller = as_labeller(c(learning_outcome = "Sustainability Learning",
                                      teaching_method = "ESD Methods"))) +
  labs(x = "Academic week", y = "Student Number", fill = "Mean score\n(0-2)"
  ) +
  theme_sos()

print(methods_heatmap)
ggsave("ESD Mapping/Mapping/methods_heatmap_fig4b.png", methods_heatmap, width = 10, height = 6, dpi = 600)
# Interactive version with hover tooltips:
methods_heatmap_interactive <- ggplotly(methods_heatmap, tooltip = "text")
methods_heatmap_interactive
saveWidget(methods_heatmap_interactive, "heatmap_methods_by_week.html", selfcontained = TRUE)

# -- 4b/4c/4d helper — each of the three plot types (entry summary, programme
# facet, year facet) needs to exist twice, once per esd_group. Rather than
# duplicate each block by hand, filter to one group and build all three from
# that. Call it once for "Sustainability Learning", once for "ESD Methods".
year_order <- c("Year 1", "Year 2", "Year 3", "Year 4")

build_methods_group_plots <- function(group_name, file_stub) {
  
  group_data <- long_methods %>% filter(esd_group == group_name)
  
  # 4b — entry summary, this group only
  entry_scores_group <- group_data %>%
    filter(!is.na(score)) %>%
    group_by(entry_id, name_clean, academic_week) %>%
    summarise(mean_score = mean(score), .groups = "drop")
  
  timeline_plot <- wide %>%
    filter(!is.na(academic_week)) %>%
    left_join(entry_scores_group, by = c("entry_id", "name_clean", "academic_week")) %>%
    mutate(name_clean = factor(name_clean, levels = order_students)) %>%
    ggplot(aes(x = academic_week, y = name_clean, fill = mean_score)) +
    geom_tile(colour = "#F7F5F0", linewidth = 0.4, width = 0.85, height = 0.85) +
    annotate("segment", x = sem2_start - 0.5, xend = sem2_start - 0.5, y = -Inf, yend = Inf,
             linetype = "dashed", colour = "#1C4A47") +
    scale_fill_gradient2(low = SCALE_LOW, mid = SCALE_MID, high = SCALE_HIGH, midpoint = 1,
                         limits = c(0, 2), name = "Mean score\n(0-2)") +
    scale_x_continuous(breaks = seq(1, 27, 3), limits = c(0.5, 27.5), expand = c(0, 0)) +
    labs(
      title = paste0("When each student submitted, shaded by mean ", group_name, " score"),
      x = "Academic week", y = NULL
    ) +
    theme_sos()
  
  print(timeline_plot)
  ggsave(paste0("timeline_by_", file_stub, "_score.png"), timeline_plot, width = 10, height = 7, dpi = 600)
  
  # 4c — faceted by programme, this group only
  by_programme_data <- group_data %>%
    filter(!is.na(academic_week), !is.na(score), !is.na(programme_clean)) %>%
    mutate(programme_clean = factor(programme_clean, levels = programme_order)) %>%
    group_by(programme_clean, academic_week, item_name) %>%
    summarise(mean_score = mean(score), .groups = "drop")
  
  heatmap_by_programme <- ggplot(by_programme_data, aes(x = academic_week, y = item_name, fill = mean_score)) +
    geom_tile(colour = "#F7F5F0", linewidth = 0.2) +
    scale_fill_gradient2(low = SCALE_LOW, mid = SCALE_MID, high = SCALE_HIGH, midpoint = 1, limits = c(0, 2)) +
    scale_x_continuous(breaks = seq(1, 27, 5), expand = c(0, 0)) +
    coord_fixed(ratio = 1.8) +   # forces square tiles regardless of facet grid shape or export size — 1 week-unit = 1 category-step
    facet_wrap2(
      ~ programme_clean, ncol = 3,
      labeller = labeller(programme_clean = label_wrap_gen(width = 30)),
      strip = strip_themed(background_x = elem_list_rect(fill = PROGRAMME_COLOURS[programme_order]))
    ) +
    labs(x = "Academic week", y = NULL) +
    theme_sos() +
    theme(
      axis.text.y = element_text(size = 10),
      strip.text = element_text(colour = "white", face = "bold", size = 8, lineheight = 0.85)
    )
  
  print(heatmap_by_programme)
  ggsave(paste0("heatmap_", file_stub, "_by_programme.png"), heatmap_by_programme, width = 13, height = 10, dpi = 600)
  
  # 4d — faceted by year of study, this group only
  by_year_data <- group_data %>%
    filter(!is.na(academic_week), !is.na(score), !is.na(year_of_study)) %>%
    mutate(year_of_study = factor(year_of_study, levels = year_order)) %>%
    group_by(year_of_study, academic_week, item_name) %>%
    summarise(mean_score = mean(score), .groups = "drop")
  
  heatmap_by_year <- ggplot(by_year_data, aes(x = academic_week, y = item_name, fill = mean_score)) +
    geom_tile(colour = "#F7F5F0", linewidth = 0.2) +
    scale_fill_gradient2(low = SCALE_LOW, mid = SCALE_MID, high = SCALE_HIGH, midpoint = 1, limits = c(0, 2)) +
    scale_x_continuous(breaks = seq(1, 27, 5), expand = c(0, 0)) +
    coord_fixed(ratio = 1) +   # same fix as heatmap_by_programme — square tiles regardless of the 2x2 vs 3x2 facet grid shape
    facet_wrap2(
      ~ year_of_study, ncol = 1,
      strip = strip_themed(background_x = elem_list_rect(fill = YEAR_COLOURS[year_order]))
    ) +
    labs(x = "Academic week", y = NULL) +
    theme_sos() +
    theme(
      axis.text.y = element_text(size = 10),
      strip.text = element_text(colour = "white", face = "bold")
    )
  
  print(heatmap_by_year)
  ggsave(paste0("heatmap_", file_stub, "_by_year.png"), heatmap_by_year, width = 10, height = 7, dpi = 600)
  # Return all three so they're accessible by name outside the function too
  list(timeline = timeline_plot, by_programme = heatmap_by_programme, by_year = heatmap_by_year)
}

sustainability_learning_plots <- build_methods_group_plots("Sustainability Learning", "sustainability_learning")
esd_methods_plots             <- build_methods_group_plots("ESD Methods", "esd_methods")

# Access individual plots like: sustainability_learning_plots$timeline,
# esd_methods_plots$by_programme, sustainability_learning_plots$by_year, etc.

# ---- 5. SDG "wedding cake" tier heatmap ---------------------------------------
# Same construction as sdg_heatmap (Section 3) — every SDG, every week, mean
# score as colour — but the y-axis is reorganised into wedding-cake tiers via
# facet_grid(), the same trick already used for the Teaching/Learning-outcomes
# heatmap in Section 4. Reading top to bottom: Partnership for the Goals (the
# "topper", standalone) -> Biosphere -> Society -> Economy, matching the real
# wedding-cake diagram's visual order.
#
# NOTE on SDG 17: the Codebook's sdg_tier column files it under Economy
# (8,9,10,12,17), which is a defensible reading of the Stockholm Resilience
# Centre model — but the model's own diagram usually draws SDG 17 sitting
# OUTSIDE the three tiers, as the connecting goal across all of them. Since
# you want it separated out as its own row here, this plot overrides sdg_tier
# for SDG 17 only, locally, via `cake_group` — it does NOT change the
# underlying sdg_tier column anywhere else in the workbook or script.

cake_data <- long_sdg %>%
  filter(!is.na(academic_week), !is.na(score)) %>%
  group_by(academic_week, sdg_number, sdg_name, sdg_tier) %>%
  summarise(mean_score = mean(score), n_entries = n_distinct(entry_id), .groups = "drop") %>%
  mutate(
    cake_group = if_else(sdg_number == 17, "SDG 17", sdg_tier),
    cake_group = factor(cake_group, levels = c("SDG 17", "Biosphere", "Society", "Economy")),
    sdg_name = fct_reorder(sdg_name, sdg_number),   # ascending within each tier's facet panel
    week_label = if_else(academic_week <= 14,
                         paste0("Sem 1, wk ", academic_week),
                         paste0("Sem 2, wk ", academic_week - 14)),
    tooltip = paste0("<b>", sdg_name, "</b> (", cake_group, ")<br>", week_label,
                     "<br>Mean score: ", round(mean_score, 2), " / 2",
                     "<br>n = ", n_entries, " entries")
  )

wedding_cake_heatmap <- ggplot(cake_data, aes(x = academic_week, y = sdg_name, fill = mean_score, text = tooltip)) +
  geom_tile(colour = "#F7F5F0", linewidth = 0.3) +
  geom_segment(aes(x = sem2_start - 0.5, xend = sem2_start - 0.5, y = -Inf, yend = Inf),
               linetype = "dashed", colour = "#1C4A47", inherit.aes = FALSE) +
  scale_fill_gradient2(
    low = SCALE_LOW, mid = SCALE_MID, high = SCALE_HIGH, midpoint = 1,
    limits = c(0, 2), name = "Mean score\n(0-2)"
  ) +
  scale_x_continuous(breaks = seq(1, 27, 3), expand = c(0, 0)) +
  # facet_grid rows follow factor level order top-to-bottom (first level = top
  # panel) — same behaviour already relied on in Section 4's category facet.
  # scales/space = "free_y" lets each tier's panel be exactly as tall as it
  # needs (1 row for Partnership, 4 for Biosphere, 8 for Society, 4 for
  # Economy) rather than forcing all tiers to the same height.
  facet_grid(cake_group ~ ., scales = "free_y", space = "free_y", switch = "y") +
  labs(x = "Academic week", y = NULL) +
  theme_sos() +
  theme(
    # Panel borders double as the "separating line" between tiers — each
    # facet panel gets its own visible box, on top of the whitespace gap
    # facet_grid already puts between panels.
    panel.border = element_rect(colour = "grey70", fill = NA, linewidth = 0.4),
    panel.spacing.y = unit(4, "pt"),
    strip.text.y = element_text(angle = 0, face = "bold", colour = "#1C4A47", size = 7),   # rotated tier labels
    strip.background = element_rect(fill = "#ECEAE3", colour = NA),
    # Leaves generous left-margin room for swapping sdg_name text labels for
    # SDG icon images later (e.g. via ggtext::element_markdown() with
    # markdown image syntax, or annotation_custom() with rasterGrob() per
    # row) — the axis text box itself won't visually crowd once labels are
    # taller/wider images instead of short strings.
    axis.text.y = element_text(size = 10, margin = margin(r = 6)),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 15)
  )

print(wedding_cake_heatmap)
ggsave("ESD Mapping/Mapping/wedding_cake_heatmap.png", wedding_cake_heatmap, width = 10, height = 7, dpi = 600)

# Interactive version with hover tooltips (SDG, tier, week, mean score, n):
# wedding_cake_heatmap_interactive <- ggplotly(wedding_cake_heatmap, tooltip = "text")
# wedding_cake_heatmap_interactive
# saveWidget(wedding_cake_heatmap_interactive, "wedding_cake_heatmap.html", selfcontained = TRUE)

# ---- 6. Year of study --------------------------------------------------------
# Three different questions year_of_study can actually answer, not just filter
# on. This is CROSS-SECTIONAL data (one academic year, so a given student only
# ever appears at one year_of_study — this can't show how one student's view
# changes as they progress). What it CAN show: whether ESD perception differs
# BETWEEN year groups at the same point in time — e.g. do finalists (with a
# term-long project) score differently than 1st years on standard lectures.
# Skips rows with no year_of_study recorded, same as any other NA-heavy column.

# -- 6a. Timeline coloured by year instead of programme — same technique as 2b,
# swap which grouping variable gets the colour depending which comparison you
# want to lead with.
timeline_plot_by_year <- wide %>%
  filter(!is.na(academic_week), !is.na(year_of_study)) %>%
  mutate(name_clean = factor(name_clean, levels = order_students)) %>%
  ggplot(aes(x = academic_week, y = name_clean, colour = year_of_study)) +
  geom_vline(xintercept = sem2_start - 0.5, linetype = "dashed", colour = "#1C4A47") +
  geom_point(alpha = 0.85, size = 2.6) +
  scale_colour_manual(values = YEAR_COLOURS, name = "Year of study") +
  scale_x_continuous(breaks = seq(1, 27, 1), limits = c(0.5, 27.5), expand = c(0, 0)) +
  labs(x = "Academic week", y = "Student Number") +
  theme_sos()

print(timeline_plot_by_year)
ggsave("timeline_by_year.png", timeline_plot_by_year, width = 10, height = 7, dpi = 600)

# -- 6b. SDG heatmap faceted by year (same construction as sdg_heatmap_by_programme,
# just swap the grouping/facet variable and use YEAR_COLOURS for the strips) --
year_order <- c("Year 1", "Year 2", "Year 3", "Year 4")

sdg_heatmap_by_year <- long_sdg %>%
  filter(!is.na(academic_week), !is.na(score), !is.na(year_of_study)) %>%
  mutate(year_of_study = factor(year_of_study, levels = year_order)) %>%
  group_by(year_of_study, academic_week, sdg_label) %>%
  summarise(mean_score = mean(score), .groups = "drop") %>%
  ggplot(aes(x = academic_week, y = sdg_label, fill = mean_score)) +
  geom_tile(colour = "#F7F5F0", linewidth = 0.2) +
  scale_fill_gradient2(low = SCALE_LOW, mid = SCALE_MID, high = SCALE_HIGH, midpoint = 1, limits = c(0, 2)) +
  scale_x_continuous(breaks = seq(1, 27, 3), expand = c(0, 0)) +
  facet_wrap2(
    ~ year_of_study, ncol = 2,
    strip = strip_themed(background_x = elem_list_rect(fill = YEAR_COLOURS[year_order]))
  ) +
  labs(x = "Academic week", y = "Sustainable Development Goal") +
  theme_sos() +
  theme(
    axis.text.y = element_text(size = 7),
    strip.text = element_text(colour = "white", face = "bold")
  )

print(sdg_heatmap_by_year)
ggsave("ESD Mapping/Mapping/sdg_heatmap_by_year.png", sdg_heatmap_by_year, width = 10, height = 7, dpi = 600)

# -- 6c. Does perceived ESD inclusion differ BY year group? Directly tests the
# thing 6a/6b can only show visually — one summary chart per entry's mean SDG
# score, grouped by year. This is probably your most citable single figure
# for "did years 3/4 report more ESD than years 1/2" as a written claim.
year_score_summary <- long_sdg %>%
  filter(!is.na(score), !is.na(year_of_study)) %>%
  mutate(year_of_study = factor(year_of_study, levels = year_order)) %>%
  group_by(entry_id, year_of_study) %>%
  summarise(mean_score = mean(score), .groups = "drop")

# Sample size per year, computed explicitly rather than inside stat_summary()
# — the stat_summary() version was silently dropping every label (that's what
# the earlier "Removed 4 rows (geom_text())" warning meant: n=4 years, all 4
# labels failing to render, not a benign warning). An explicit table handed
# straight to geom_text() is more transparent and doesn't have that issue.
n_labels <- year_score_summary %>%
  count(year_of_study, name = "n") %>%
  mutate(label = paste0("n = ", n), y = -0.08)

year_score_boxplot <- ggplot(year_score_summary, aes(x = year_of_study, y = mean_score, fill = year_of_study)) +
  geom_boxplot(alpha = 0.85, outlier.shape = NA) +   # NA hides boxplot's own outlier dots — geom_jitter below already shows every point, so drawing both double-plots outliers
  geom_jitter(width = 0.12, alpha = 0.3, size = 1, colour = "#1C2A2E") +
  # Sample size under each box — entry counts differ sharply by year (Year 3
  # alone is ~44% of the whole dataset), so this needs to travel WITH the
  # figure rather than live only as a caveat someone has to be told separately.
  geom_text(data = n_labels, aes(x = year_of_study, y = y, label = label),
            inherit.aes = FALSE, size = 4, colour = "#6B7472") +
  scale_fill_manual(values = YEAR_COLOURS, guide = "none") +
  scale_y_continuous(limits = c(-0.15, 2)) +
  labs(x = NULL, y = "Mean SDG score (0-2)"
  ) +
  theme_sos()

print(year_score_boxplot)
ggsave("ESD Mapping/Mapping/year_score_boxplot.png", year_score_boxplot, width = 10, height = 7, dpi = 600)

# -- 6d. Same idea as 6c, grouped by programme instead of year — a genuine
# addition alongside cumulative_engagement_plot (2e), not a replacement for
# it: 2e shows HOW engagement built over the year per programme, this shows
# WHAT was reported per programme. Different questions, both worth keeping.
programme_score_summary <- long_sdg %>%
  filter(!is.na(score), !is.na(programme_clean)) %>%
  group_by(entry_id, programme_clean) %>%
  summarise(mean_score = mean(score), .groups = "drop")

programme_n_labels <- programme_score_summary %>%
  count(programme_clean, name = "n") %>%
  mutate(label = paste0("n = ", n), y = -0.08)

programme_score_boxplot <- ggplot(programme_score_summary, aes(x = programme_clean, y = mean_score, fill = programme_clean)) +
  geom_boxplot(alpha = 0.85, outlier.shape = NA) +
  geom_jitter(width = 0.12, alpha = 0.3, size = 1, colour = "#1C2A2E") +
  geom_text(data = programme_n_labels, aes(x = programme_clean, y = y, label = label),
            inherit.aes = FALSE, size = 4, colour = "#6B7472") +
  scale_fill_manual(values = PROGRAMME_COLOURS, guide = "none") +
  scale_y_continuous(limits = c(-0.15, 2)) +
  labs(x = NULL, y = "Mean SDG score (0-2)"
  ) +
  theme_sos() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1, size = 7))   # programme names are longer than "Year N" — angled to avoid overlap

print(programme_score_boxplot)
ggsave("ESD Mapping/Mapping/programme_score_boxplot.png", programme_score_boxplot, width = 10, height = 7, dpi = 600)

# =============================================================================
# End of script. All plots saved as PNG in the working directory; swap
# ggsave()/print() for ggplotly() on any plot above for interactive hover
# tooltips (student name, exact score, week label) in RStudio Viewer or Shiny.
# =============================================================================
