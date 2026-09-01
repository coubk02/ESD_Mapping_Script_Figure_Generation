# =============================================================================
# SOS-UK ESD Mapping — Significance testing on SDG scores
# Standalone script. Sources your main mapping script to reuse its data
# pipeline rather than rebuilding it, so these tests are always run against
# exactly the same data behind the boxplot figures, not a separate copy of it.
#
# Tests non-independence directly: student entries are collapsed to ONE
# value per student before any test runs, since testing all 239 entries
# directly would let your most frequent submitters dominate the result.
# =============================================================================

MAIN_SCRIPT <- "[FILE PATH].xlsx" # Script will run when full file path is uploaded in here
source(MAIN_SCRIPT)

# ---- 1. Collapse to one mean score per student -------------------------------
# This is the step your Limitations section describes in prose, done properly
# here: each student's own mean across all their entries, so every student
# counts once regardless of how many entries they submitted.

student_year_means <- long_sdg %>%
  filter(!is.na(score), !is.na(year_of_study)) %>%
  group_by(name_clean, year_of_study) %>%
  summarise(mean_score = mean(score), .groups = "drop") %>%
  mutate(year_of_study = factor(year_of_study, levels = year_order))

student_programme_means <- long_sdg %>%
  filter(!is.na(score), !is.na(programme_clean)) %>%
  group_by(name_clean, programme_clean) %>%
  summarise(mean_score = mean(score), .groups = "drop")

# Sample size check before testing anything — groups this small genuinely
# cannot support a meaningful test, regardless of which one you pick.
print(student_year_means %>% count(year_of_study))
print(student_programme_means %>% count(programme_clean))

# ---- 2. Year of study: overall difference across all four years --------------
# Kruskal-Wallis is the non-parametric equivalent of a one-way ANOVA — used
# here rather than ANOVA itself because a 0-2 bounded mean score is unlikely
# to be normally distributed, and Kruskal-Wallis doesn't assume it is.

year_kruskal <- kruskal.test(mean_score ~ year_of_study, data = student_year_means)
print(year_kruskal)

# If (and only if) the overall test above is significant, this tells you
# WHICH specific years differ from each other, with p-values already
# corrected for running multiple comparisons (Benjamini-Hochberg).
year_pairwise <- pairwise.wilcox.test(
  student_year_means$mean_score, student_year_means$year_of_study,
  p.adjust.method = "BH"
)
print(year_pairwise)

# install.packages("ggsignif")  # uncomment on first run
library(ggsignif)

year1_year3_p <- year_pairwise$p.value["Year 3", "Year 1"]

year_score_boxplot_annotated <- year_score_boxplot +
  geom_signif(
    comparisons = list(c("Year 1", "Year 3")),
    annotations = paste0("*"),
    y_position = 1.7,   # check this clears your highest data points once rendered, Year 1's whisker reaches close to 1.6
    tip_length = 0.02
  ) 

print(year_score_boxplot_annotated)
ggsave("ESD Mapping/Mapping/year_score_boxplot_annotated.png", year_score_boxplot_annotated, width = 9, height = 6, dpi = 900)

# ---- 3. Programme of study: overall difference across all six programmes -----
programme_kruskal <- kruskal.test(mean_score ~ programme_clean, data = student_programme_means)
print(programme_kruskal)

programme_pairwise <- pairwise.wilcox.test(
  student_programme_means$mean_score, student_programme_means$programme_clean,
  p.adjust.method = "BH"
)
print(programme_pairwise)

programme_score_boxplot_annotated <- programme_score_boxplot

print(programme_score_boxplot_annotated)
ggsave("ESD Mapping/Mapping/programme_score_boxplot_annotated.png", programme_score_boxplot_annotated, width = 11, height = 8, dpi = 900)

# =============================================================================
# Reading the output:
# - kruskal.test() gives one p-value: is there a significant difference
#   SOMEWHERE among the groups. p < .05 is the conventional (not universal)
#   threshold — justify whichever you use rather than stating it as a rule.
# - pairwise.wilcox.test() only tells you something trustworthy for pairs
#   with a reasonable n on both sides. Check the sample size printout in
#   Section 1 before reporting any specific pairwise result, a "significant"
#   p-value from a 2-student group is not evidence of anything.
# - Groups with 1-2 students (MSci Pharmacology, BSc Immunology and
#   Microbiology) will still appear in this output but should NOT be reported
#   as statistically tested findings — keep the descriptive "indicative
#   rather than representative" framing for those regardless of what the
#   test returns.
# =============================================================================

# ---- Staff vs student (dumbbell chart) comparison — NOT YET BUILT ------------
# This needs the crosswalk-matched staff/student long-format data from your
# staff mapping script, and I don't want to guess at object names from that
# script rather than verify them first, given how many stale-reference bugs
# we've already chased down in this project. Send me the staff script (or
# confirm the object name for the crosswalk-matched comparison data) and
# I'll add the Mann-Whitney U tests with Benjamini-Hochberg correction across
# all items, as discussed, as their own section below this one.

MAIN_STAFF_SCRIPT <- "[FILE PATH].xlsx" # Script will run when full file path is uploaded in here
source(MAIN_STAFF_SCRIPT)

# ---- 1. Build the per-observation (not collapsed) distribution for each side -

# Student side: one row per student per item — the stage stu_mean() itself
# computes internally but doesn't return.
student_by_student <- function(d, item_col) {
  d %>%
    filter(!is.na(score)) %>%
    group_by(name_clean, item = .data[[item_col]]) %>%
    summarise(value = mean(score, na.rm = TRUE), .groups = "drop")
}

student_dist <- bind_rows(
  student_by_student(student_sdg, "sdg_number") %>% mutate(item = paste("SDG", item)),
  student_by_student(student_methods, "item_name")
)

# Staff side: one row per module per item, already at the right granularity —
# just needs the same key used to build staff_means (SDG n / full item name).
staff_dist <- staff %>%
  filter(!is.na(value)) %>%
  mutate(key = ifelse(domain == "SDGs", paste("SDG", item_no), item)) %>%
  select(entry_id, key, value)

# ---- 2. Build the same 27-item list Figure 11 tests, reusing its own crosswalk

items_to_test <- bind_rows(
  tibble(domain = "SDGs", key = paste("SDG", 1:17), student_item = paste("SDG", 1:17)),   # domain = "SDGs" added
  crosswalk %>% transmute(domain, key = staff_item, student_item)
)

# ---- 3. Run one Wilcoxon rank-sum test per item -------------------------------

run_one_test <- function(staff_key, student_key) {
  s_vals <- staff_dist %>% filter(key == staff_key) %>% pull(value)
  t_vals <- student_dist %>% filter(item == student_key) %>% pull(value)
  if (length(s_vals) < 2 || length(t_vals) < 2) {
    return(tibble(staff_n = length(s_vals), student_n = length(t_vals), p_raw = NA_real_))
  }
  test <- wilcox.test(s_vals, t_vals, exact = FALSE)
  tibble(staff_n = length(s_vals), student_n = length(t_vals), p_raw = test$p.value)
}

sig_results <- items_to_test %>%
  rowwise() %>%
  mutate(run_one_test(key, student_item)) %>%
  ungroup() %>%
  mutate(p_adj = p.adjust(p_raw, method = "BH"),
         significant = !is.na(p_adj) & p_adj < 0.05) %>%
  arrange(p_adj)

print(sig_results, n = Inf)
write_csv(sig_results, "staff_student_significance.csv")

cat("\nItems with too few observations on one side to test at all:\n")
print(sig_results %>% filter(is.na(p_adj)))

cat("\nSignificant after BH correction (p_adj < .05):", sum(sig_results$significant, na.rm = TRUE),
    "of", sum(!is.na(sig_results$p_adj)), "testable items\n")

# ---- 4. Annotate the existing dumbbell figure with significance markers ------
# Joins the adjusted p-values back onto `cmp` (the data Figure 11 was already
# built from) via the same key logic, so the asterisks land on the same rows
# without needing to rebuild the plot from scratch.
library(ggnewscale)

cmp_sig <- cmp %>%
  mutate(key = as.character(label)) %>%
  left_join(
    sig_results %>% mutate(key = tidy_labels(key)) %>% select(key, p_adj, significant),
    by = "key"
  ) %>%
  mutate(significant = replace_na(significant, FALSE),
         sig_label = factor(if_else(significant, "p < .05", "Not significant"),
                            levels = c("p < .05", "Not significant")),
         midpoint = (staff + student) / 2)

print(nrow(cmp_sig %>% filter(significant)))   # should now read 17   # sanity check before plotting anything

fig11_annotated <- ggplot(cmp_sig) +
  geom_hline(data = sep, aes(yintercept = 0.5), linetype = "dashed",
             colour = "grey60", linewidth = 0.4) +
  # line colour now MAPPED to sig_label, not fixed, so it can carry its own legend
  geom_segment(aes(x = staff, xend = student, y = label, yend = label, colour = sig_label),
               linewidth = 1.1) +
  scale_colour_manual(values = c("p < .05" = "#70271f", "Not significant" = "grey70"),
                      name = "Staff-student difference") +
  # reset the colour scale so the points below get their OWN separate legend
  new_scale_colour() +
  geom_point(aes(staff,   label, colour = "Staff"),    size = 2.6) +
  geom_point(aes(student, label, colour = "Students"), size = 2.6) +
  scale_colour_manual(values = c(Staff = "#41644a", Students = "#f8c662"), name = NULL) +
  geom_text(
    data = cmp_sig %>% filter(significant),
    aes(x = midpoint, y = label, label = "*"),
    size = 5, colour = "white", fontface = "bold", vjust = 0.35
  ) +
  facet_grid(domain ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_x_continuous(limits = c(0, 2), breaks = 0:2,
                     labels = c("0", "1", "2")) +
  labs(x = "Mean inclusion score", y = NULL,) +
  theme_minimal(base_size = 10) +
  theme_minimal(base_size = 10) +
  theme(panel.grid.major.y = element_blank(),
        panel.spacing.y = unit(0.9, "lines"),
        strip.placement = "outside",
        strip.text.y.left = element_text(face = "bold", angle = 0, hjust = 0),
        legend.title = element_text(size = 8),
        legend.position = "inside",
        legend.position.inside = c(0.98, 0.98),
        legend.justification = c("right", "top"),
        legend.box = "vertical",
        legend.background = element_rect(fill = alpha("white", 0.75), colour = NA),
        plot.caption = element_text(hjust = 0, size = 7, colour = "grey30"))

print(fig11_annotated)
ggsave("ESD Mapping/Mapping/Figure11_staff_student_gap_annotated.png", fig11_annotated, width = 12, height = 8, dpi = 600, bg = "white")

# =============================================================================
# Reading the output:
# - staff_n / student_n are shown for every item — items with very few staff
#   modules or very few students recording that item (check against Table 1/
#   the staff Methods description) should be read with the same caution as
#   the small-cohort SDG/programme groups elsewhere in this analysis, a
#   significant-looking p-value from a handful of observations on either
#   side is not strong evidence.
# - p_adj is the one to report, not p_raw — p_raw is what a single test
#   would show in isolation and overstates confidence once you're running
#   27 of them together.
# - "too few observations to test" items print separately at the end —
#   these are real gaps in what can be statistically compared, not tests
#   that failed, worth naming explicitly in your Limitations rather than
#   silently dropping them from the write-up.
# =============================================================================

