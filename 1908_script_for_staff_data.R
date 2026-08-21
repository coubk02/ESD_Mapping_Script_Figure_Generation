# ---------------------------------------------------------------------------
# 01_prepare_staff_data.R
# Builds a tidy staff ESD mapping dataset from the raw SIPBS staff mapping tool.
#
# Input : ESD Mapping Tool  SCI Pharmacy  Biomedical Sciences  June 24_ALL MODULES.xlsx
#         (sheet "Modules List (Dept)"; headers sit on row 2, data from row 3)
# Output: staff_esd_long.csv          one row per entry x item  (the file to analyse)
#         staff_esd_wide.csv          one row per entry, one column per item
#         staff_esd_excluded_log.csv  every module dropped, with the reason
#
# Scoring is the same 0/1/2 logic as the SOS-UK student tool:
#   0 = not used / not covered
#   1 = used for other content / some mention of   (present, not framed as sustainability)
#   2 = used for ESD content / specific focus on
# ---------------------------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

raw_path <- "/Users/katiecoubrough/ESD Mapping/ESD Mapping Tool - SCI Pharmacy & Biomedical Sciences - June 24_ALL MODULES.xlsx"
out_dir  <- "."

# --- exclusions ------------------------------------------------------------
# Modules no longer offered (supplied by the author). Pharmacy (MP...) and all
# other non-BM codes are handled by the prefix rule below.
discontinued <- c("BM931","BM930","BM514","BM513","BM512","BM498","BM437","BM436",
                  "BM419","BM418","BM335","BM334","BM333","BM332","BM331","BM320",
                  "BM319","BM216","BM215","BM209","BM208","BM113","BM107","BM106")

# --- item definitions (positional; the sheet has duplicated header names) ---
meta_cols <- c("faculty","department","lecturer","study_level","study_year_reported",
               "module_name","module_code","module_status","project_module",
               "teaching_channel","class_size","module_descriptor_url")

skills <- c("Effective Communicator","Resilient","Value Creator","Future (Forward) Thinking",
            "Collaboration","Strategic Thinking","Systems Thinking","Critical Thinking",
            "Integrated Problem Solving","Norms & Values","Self-awareness")

sdgs <- c("SDG 1: No poverty","SDG 2: Zero hunger","SDG 3: Good health and wellbeing",
          "SDG 4: Quality education","SDG 5: Gender equality","SDG 6: Clean water and sanitation",
          "SDG 7: Affordable and clean energy","SDG 8: Decent work and economic growth",
          "SDG 9: Industry, innovation and infrastructure","SDG 10: Reduced inequalities",
          "SDG 11: Sustainable cities and communities",
          "SDG 12: Responsible consumption and production","SDG 13: Climate action",
          "SDG 14: Life below water","SDG 15: Life on land",
          "SDG 16: Peace, justice and strong institutions","SDG 17: Partnerships for the goals")

methods <- c("Case studies","Stimulus Activities",
             "Experiential project, placement or work-based learning","Simulation Activities",
             "Inquiry / Problem-Based Learning (PBL)","Participatory Learning (Curriculum Co-creation)",
             "Debate or Discussion","Field-based activities","Community engagement",
             "Small group tutorial","Laboratory practical","Flipped classroom",
             "Vertically-integrated Projects (VIP)")

# column positions in the raw sheet (1-indexed)
skill_pos  <- seq(13, 33, by = 2)   # score; evidence sits in the next column
sdg_pos    <- 35:51                 # score only, no evidence collected
method_pos <- seq(52, 76, by = 2)

# --- read -------------------------------------------------------------------
raw <- read_excel(raw_path, sheet = "Modules List (Dept)",
                  skip = 2, col_names = FALSE, col_types = "text",
                  .name_repair = "minimal")
names(raw) <- paste0("V", seq_len(ncol(raw)))

blank_to_na <- function(x) { x <- str_trim(x); ifelse(x == "" | is.na(x), NA_character_, x) }
raw <- raw %>% mutate(across(everything(), blank_to_na))

raw <- raw %>%
  mutate(source_row = row_number() + 2,
         module_code = str_replace_all(V7, "\\s", "")) %>%
  filter(!is.na(module_code))

# --- apply exclusions -------------------------------------------------------
resp_pos <- c(skill_pos, sdg_pos, method_pos)

raw <- raw %>%
  rowwise() %>%
  mutate(n_responses = sum(!is.na(c_across(all_of(paste0("V", resp_pos)))))) %>%
  ungroup() %>%
  mutate(exclusion_reason = case_when(
    !str_starts(module_code, "BM")   ~ "pharmacy_or_non_BM_code",
    module_code %in% discontinued    ~ "no_longer_offered",
    n_responses == 0                 ~ "no_mapping_data_returned",
    TRUE                             ~ NA_character_))

excluded <- raw %>%
  filter(!is.na(exclusion_reason)) %>%
  transmute(source_row, module_code, module_name = V6, lecturer = V3,
            n_responses, exclusion_reason)

kept <- raw %>%
  filter(is.na(exclusion_reason)) %>%
  mutate(entry_id = sprintf("S%03d", row_number()))

# --- metadata ---------------------------------------------------------------
stage_from_code <- function(code) {
  d <- str_sub(str_extract(code, "^BM\\d"), 3, 3)
  recode(d, "1" = "UG1", "2" = "UG2", "3" = "UG3", "4" = "UG4",
         "5" = "MSci", "9" = "PGT", .default = NA_character_)
}
stage_from_reported <- function(x) {
  recode(x, "1st Year UG" = "UG1", "2nd Year UG" = "UG2", "3rd Year UG" = "UG3",
         "4th Year UG" = "UG4", "5th+ Year UG" = "MSci",
         "1st Year PG" = "PGT", "2nd Year PG" = "PGT", .default = NA_character_)
}

entry_meta <- kept %>%
  transmute(entry_id, source_row, module_code,
            module_name          = V6,
            lecturer             = V3,
            study_level          = V4,
            study_year_reported  = V5,
            stage_from_code      = stage_from_code(module_code),
            stage                = coalesce(stage_from_reported(V5), stage_from_code(module_code)),
            module_status        = V8,
            project_module       = V9,
            teaching_channel     = V10,
            class_size           = V11,
            n_responses)

# --- scoring helpers --------------------------------------------------------
# "Not Used"/"Not Covered" is always 0, whatever number precedes it. This also
# repairs BM954, where the SDG number was entered in place of the score
# ("4 - Not Covered" etc).
score_of <- function(x) {
  ifelse(is.na(x), NA_integer_,
         ifelse(str_detect(x, "Not Used|Not Covered"), 0L,
                suppressWarnings(as.integer(str_extract(x, "^\\d+")))))
}
# The tool lets staff record a 2 for entrepreneurship rather than ESD. Kept at
# face value in `score`; downgraded to 1 in `score_esd` for ESD-specific claims.
esd_score_of <- function(x, s) {
  ifelse(!is.na(x) & str_detect(x, "Entrepreneurial content") & !str_detect(x, "BOTH"), 1L, s)
}

pull_block <- function(pos, items, domain, evidence = TRUE) {
  scores <- kept %>%
    select(entry_id, all_of(paste0("V", pos))) %>%
    setNames(c("entry_id", items)) %>%
    pivot_longer(-entry_id, names_to = "item", values_to = "response_raw")
  if (evidence) {
    ev <- kept %>%
      select(entry_id, all_of(paste0("V", pos + 1))) %>%
      setNames(c("entry_id", items)) %>%
      pivot_longer(-entry_id, names_to = "item", values_to = "evidence")
    scores <- left_join(scores, ev, by = c("entry_id", "item"))
  } else {
    scores$evidence <- NA_character_
  }
  scores %>%
    mutate(domain = domain,
           item = factor(item, levels = items),
           item_no = as.integer(item),
           item = as.character(item))
}

long <- bind_rows(
  pull_block(skill_pos,  skills,  "Skills & Competencies", evidence = TRUE),
  pull_block(sdg_pos,    sdgs,    "SDGs",                  evidence = FALSE),
  pull_block(method_pos, methods, "ESD Methods",           evidence = TRUE)
) %>%
  mutate(score = score_of(response_raw),
         score_esd = esd_score_of(response_raw, score))

# --- duplicate submissions --------------------------------------------------
# Three modules were submitted twice (BM330, BM424, BM940). Both entries are kept
# for transparency; is_primary marks the more complete one. Filter on is_primary
# for one row per module.
completeness <- long %>%
  group_by(entry_id) %>%
  summarise(n_scored = sum(!is.na(score)), .groups = "drop") %>%
  left_join(entry_meta %>% select(entry_id, module_code), by = "entry_id") %>%
  group_by(module_code) %>%
  mutate(duplicate_module = n() > 1,
         is_primary = rank(-n_scored, ties.method = "first") == 1) %>%
  ungroup() %>%
  select(entry_id, n_scored, is_primary, duplicate_module)

long <- entry_meta %>%
  left_join(long, by = "entry_id") %>%
  left_join(completeness, by = "entry_id") %>%
  mutate(domain = factor(domain, levels = c("SDGs", "Skills & Competencies", "ESD Methods")),
         stage  = factor(stage,  levels = c("UG1", "UG2", "UG3", "UG4", "MSci", "PGT"))) %>%
  select(entry_id, module_code, module_name, lecturer, study_level, study_year_reported,
         stage, stage_from_code, module_status, project_module, teaching_channel, class_size,
         domain, item, item_no, response_raw, score, score_esd, evidence,
         n_responses, n_scored, is_primary, duplicate_module, source_row)

wide <- long %>%
  select(entry_id, item, score) %>%
  pivot_wider(names_from = item, values_from = score) %>%
  left_join(long %>% distinct(entry_id, module_code, module_name, lecturer, study_level,
                              study_year_reported, stage, module_status, teaching_channel,
                              class_size, is_primary, duplicate_module),
            by = "entry_id") %>%
  relocate(entry_id, module_code, module_name, lecturer, study_level, study_year_reported,
           stage, module_status, teaching_channel, class_size, is_primary, duplicate_module)

write_csv(long,     file.path(out_dir, "staff_esd_long.csv"))
write_csv(wide,     file.path(out_dir, "staff_esd_wide.csv"))
write_csv(excluded, file.path(out_dir, "staff_esd_excluded_log.csv"))

# --- transparency numbers for the Methods section ---------------------------
cat("Entries kept        :", n_distinct(long$entry_id), "\n")
cat("Unique modules      :", n_distinct(long$module_code), "\n")
cat("Duplicate modules   :", paste(sort(unique(long$module_code[long$duplicate_module])), collapse = ", "), "\n")
cat("Modules excluded    :", nrow(excluded), "\n")
print(count(excluded, exclusion_reason))
print(long %>% filter(is_primary) %>% distinct(entry_id, stage) %>% count(stage))
  # ---------------------------------------------------------------------------
  # 02_staff_figures.R
  # Figures for the staff arm of the ESD mapping study.
  #
  #   Figure 10  Ranked "share of modules" bars: SDGs / Skills & Competencies /
  #              ESD Methods. The staff figure for the paper. Staff mapped each
  #              module once, so the data is a single snapshot with no time axis;
  #              the heatmap grammar used for the student figures encodes week-by-week
  #              change and would misrepresent that. The bars also keep the 1-vs-2
  #              distinction visible (recorded at all vs framed as explicit ESD).
  #   Figure S1  Composite staff heatmap by stage of study. Supplemental - retains
  #              the 0-2 gradient of Figures 6 and 8 for readers who want continuity.
  #   Figure 11  Staff vs student comparison (dumbbell). Reads the student objects
  #              from 0608_Mapping_Environment.RData.
  #
  # Also writes: table_staff_summary.csv, table_sdg_key.csv
  #
  # Run 01_prepare_staff_data.R first. Open the ESD Mapping .Rproj so the working
  # directory is the project folder.
  # ---------------------------------------------------------------------------
  
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(forcats)
  library(ggplot2)
  library(patchwork)
  library(scales)
  
  staff <- read_csv("staff_esd_long.csv", show_col_types = FALSE) %>%
    filter(is_primary) %>%                       # one entry per module
    mutate(domain = factor(domain, levels = c("SDGs", "Skills & Competencies", "ESD Methods")),
           stage  = factor(stage,  levels = c("UG1", "UG2", "UG3", "UG4", "MSci", "PGT")))
  
  # Set to TRUE to restrict to the stages the students actually mapped (UG1-MSci).
  ug_only <- FALSE
  if (ug_only) staff <- filter(staff, stage != "PGT")
  
  # Swap `score` for `score_esd` here to downgrade entrepreneurship-only responses.
  score_var <- "score"
  staff$value <- staff[[score_var]]
  
  # ---------------------------------------------------------------------------
  # Display labels
  # SDGs show as number only (full titles live in table_sdg_key.csv); the longest
  # competency and method names are shortened so no axis label wraps onto two lines.
  # Display only - the underlying `item` values are unchanged.
  # ---------------------------------------------------------------------------
  short_labels <- c(
    "Experiential project, placement or work-based learning" = "Experiential / work-based learning",
    "Participatory Learning (Curriculum Co-creation)"        = "Participatory learning",
    "Inquiry / Problem-Based Learning (PBL)"                 = "Problem-based learning (PBL)",
    "Vertically-integrated Projects (VIP)"                   = "Vertically-integrated projects",
    "Integrated Problem Solving"                             = "Integrated problem solving",
    "Future (Forward) Thinking"                              = "Future thinking"
  )
  
  tidy_labels <- function(x) {
    x <- ifelse(x %in% names(short_labels), short_labels[x], x)
    ifelse(str_starts(x, "SDG"), str_extract(x, "^SDG \\d+"), x)
  }
  
  # SDG key table (pairs with the number-only axis)
  staff %>%
    filter(domain == "SDGs") %>%
    distinct(item, item_no) %>%
    arrange(item_no) %>%
    transmute(SDG = paste("SDG", item_no), Goal = str_remove(item, "^SDG \\d+: ")) %>%
    write_csv("table_sdg_key.csv")
  
  # ===========================================================================
  # FIGURE 10 - ranked share of modules (0 / 1 / 2)
  # What proportion of mapped modules recorded each item, and whether staff framed
  # it as ESD (2) or as ordinary content (1). Items are ordered by mean score, so
  # the ranking is readable without the reader decoding a colour scale.
  # ===========================================================================
  
  share <- staff %>%
    filter(!is.na(value)) %>%
    count(domain, item, value) %>%
    group_by(domain, item) %>%
    mutate(prop = n / sum(n),
           mean_score = sum(value * n) / sum(n)) %>%
    ungroup() %>%
    mutate(item = fct_reorder(item, mean_score),
           value = factor(value, levels = c(2, 1, 0),
                          labels = c("Explicit ESD content",
                                     "Present, not framed as ESD",
                                     "Not covered")))
  
  fig10 <- ggplot(share, aes(prop, item, fill = value)) +
    geom_col(width = 0.75) +
    facet_wrap(~domain, scales = "free_y", ncol = 1) +
    scale_x_continuous(labels = percent, expand = expansion(c(0, 0.02))) +
    scale_y_discrete(labels = tidy_labels) +
    scale_fill_manual(values = c("Explicit ESD content"       = "#23361A",
                                 "Present, not framed as ESD" = "#9AAB64",
                                 "Not covered"                = "#ECE69D"), name = NULL) +
    labs(x = paste0("Share of mapped modules (n = ", n_distinct(staff$entry_id), ")")) +
    theme_minimal(base_size = 10) +
    theme(panel.grid.major.y = element_blank(),
          strip.text = element_text(face = "bold", hjust = 0),
          legend.position = "bottom",
          axis.title.y = element_blank())
  print(fig10)
  ggsave("Figure10_staff_share.png", fig10, width = 7.2, height = 9.5, dpi = 600, bg = "white")
 
  # ===========================================================================
  # FIGURE 11 - staff vs student comparison
  # ---------------------------------------------------------------------------
  # Student data is loaded into its own environment so it cannot overwrite the
  # staff objects (both scripts use names like `long`, `wide`, `raw`).
  #
  # The two tools do not share item lists (staff: 11 competencies + 13 methods;
  # students: 7 sustainability learning + 6 ESD methods), so non-SDG items are
  # paired through an explicit crosswalk. Unpaired items are dropped rather than
  # forced - see the note below.
  # ===========================================================================
  
  student_env <- new.env()
  load("~/ESD Mapping/0608_Mapping_Environment.RData", envir = student_env)
  
  student_sdg     <- student_env$long_sdg     %>% filter(respondent_type == "student")
  student_methods <- student_env$long_methods %>% filter(respondent_type == "student")
  
  # Two-stage mean: average within student first, then across students, so the
  # handful of prolific auditors do not dominate (see Section 3.1).
  stu_mean <- function(d, item_col) {
    d %>%
      group_by(name_clean, item = .data[[item_col]]) %>%
      summarise(m = mean(score, na.rm = TRUE), .groups = "drop") %>%
      group_by(item) %>%
      summarise(student = mean(m, na.rm = TRUE), .groups = "drop")
  }
  
  student_means <- bind_rows(
    stu_mean(student_sdg, "sdg_number") %>% mutate(item = paste("SDG", item)),
    stu_mean(student_methods, "item_name")
  )
  
  # Keyed on "SDG n" for the SDGs, full item name otherwise.
  staff_means <- staff %>%
    mutate(key = ifelse(domain == "SDGs", paste("SDG", item_no), item)) %>%
    group_by(key) %>%
    summarise(staff = mean(value, na.rm = TRUE), .groups = "drop")
  
  # NOTE ON PAIRING: staff "Systems Thinking" is paired here with the student item
  # "Seeing The Bigger Picture" as the closest conceptual match. The student item
  # "Understanding Sustainable Development" has no staff counterpart - the staff
  # tool contains no item asking whether a module builds sustainability literacy
  # as such. Also unpaired: student "Take Real World Action" and "Interdisciplinary
  # Learning"; staff "Effective Communicator" and "Resilient".
  crosswalk <- tribble(
    ~domain,                 ~staff_item,                              ~student_item,
    "Sustainability Learning", "Critical Thinking",                      "Critical Thinking Skills",
    "Sustainability Learning", "Systems Thinking",                       "Seeing The Bigger Picture",
    "Sustainability Learning", "Value Creator",                          "Challenging Business As Usual",
    "Sustainability Learning", "Norms & Values",                         "Ethics And Values",
    "Sustainability Learning", "Integrated Problem Solving",             "Collaborative Problem Solving",
    "ESD Methods",           "Inquiry / Problem-Based Learning (PBL)", "Problem Based Learning",
    "ESD Methods",           "Case studies",                           "Case Studies",
    "ESD Methods",           "Simulation Activities",                  "Simulation",
    "ESD Methods",           "Stimulus Activities",                    "Stimulus Activities",
    "ESD Methods",           "Experiential project, placement or work-based learning",
    "Experiential Project Work"
  )
  
  cmp <- bind_rows(
    tibble(domain = "SDGs", key = paste("SDG", 1:17)) %>%
      mutate(label = key) %>%
      left_join(staff_means, by = "key") %>%
      left_join(student_means, by = c("key" = "item")),
    crosswalk %>%
      left_join(staff_means,   by = c("staff_item" = "key")) %>%
      left_join(student_means, by = c("student_item" = "item")) %>%
      mutate(label = staff_item)
  ) %>%
  filter(!is.na(staff), !is.na(student)) %>%
  mutate(gap    = student - staff,
         label  = tidy_labels(label),
         label  = fct_reorder(label, gap),
         domain = factor(domain, levels = c("SDGs", "Sustainability Learning", "ESD Methods")))
  
  # how many pairs made it through - expect 17 SDGs + 10 crosswalk rows
  print(count(cmp, domain))
  
  # dashed rule under each panel except the last; 0.5 sits just below the lowest
  # item on each panel's own discrete scale (scales = "free_y")
  sep <- tibble(domain = factor(c("SDGs", "Sustainability Learning", "ESD Methods"),
                                levels = levels(cmp$domain)))
  
  fig11 <- ggplot(cmp) +
    geom_hline(data = sep, aes(yintercept = 0.5), linetype = "dashed",
               colour = "grey60", linewidth = 0.4) +
    geom_segment(aes(x = staff, xend = student, y = label, yend = label),
                 colour = "grey70", linewidth = 0.9) +
    geom_point(aes(staff,   label, colour = "Staff"),    size = 2.6) +
    geom_point(aes(student, label, colour = "Students"), size = 2.6) +
    facet_grid(domain ~ ., scales = "free_y", space = "free_y", switch = "y") +
    scale_colour_manual(values = c(Staff = "#41644a", Students = "#f8c662"), name = NULL) +
    scale_x_continuous(limits = c(0, 2), breaks = 0:2,
                       labels = c("Not covered (0)", "Implicit (1)", "Explicit (2)")) +
    labs(x = "Mean inclusion score", y = NULL,
         caption = paste("Staff means are per module (one return per module);",
                         "student means are per student, averaged within student first.",
                         "\nThe two are differently constructed and are not a like-for-like measure",
                         "of the same quantity.")) +
    theme_minimal(base_size = 10) +
    theme(panel.grid.major.y = element_blank(),
          panel.spacing.y = unit(0.9, "lines"),
          strip.placement = "outside",
          strip.text.y.left = element_text(face = "bold", angle = 0, hjust = 0),
          legend.position = "top",
          plot.caption = element_text(hjust = 0, size = 7, colour = "grey30"))
 
  print(fig11)
  ggsave("Figure11_staff_student_gap.png", fig11, width = 12, height = 8, dpi = 600, bg = "white")

# ---------------------------------------------------------------------------
# Numbers for the Results text
# ---------------------------------------------------------------------------
staff_summary <- staff %>%
  filter(!is.na(value)) %>%
  group_by(domain, item) %>%
  summarise(mean    = round(mean(value), 2),
            pct_any = round(100 * mean(value > 0)),    # % of modules recording it at all
            pct_esd = round(100 * mean(value == 2)),   # % framing it as explicit ESD
            .groups = "drop") %>%
  arrange(domain, desc(mean))

write_csv(staff_summary, "table_staff_summary.csv")
print(staff_summary, n = Inf, width = Inf)


# staff/student gaps, largest student-over-staff first
cmp %>%
  arrange(desc(gap)) %>%
  transmute(domain, item = as.character(label),
            staff = round(staff, 2), student = round(student, 2), gap = round(gap, 2)) %>%
  print(n = Inf)
gap_table <- cmp %>%
  arrange(desc(gap)) %>%
  transmute(Domain = domain, Item = as.character(label),
            `Staff mean` = round(staff, 2), `Student mean` = round(student, 2),
            `Gap (student - staff)` = round(gap, 2))

writexl::write_xlsx(list(`S1 Staff summary`       = staff_summary,
                         `S2 Staff-student gaps`  = gap_table,
                         `S3 SDG key`             = readr::read_csv("table_sdg_key.csv",
                                                                    show_col_types = FALSE)),
                    "Supplementary_tables.xlsx")
# ===========================================================================
# FIGURE S2 / TABLE S4 - the staff-student crosswalk
# Documents every item in both tools and how (or whether) it was paired.
# The unpaired rows are the informative part: they show where each instrument
# asks something the other does not.
# Writes both a rendered image (for use as a supplemental figure) and a CSV.
# ===========================================================================

library(gridExtra)
library(grid)

# every non-SDG staff item, with its domain
staff_items <- staff %>%
  filter(domain != "SDGs") %>%
  distinct(domain, item) %>%
  transmute(domain = as.character(domain), staff_item = item)

# every student item, with the block of the student tool it sits in
student_items <- student_env$long_methods %>%
  distinct(category, item_name) %>%
  transmute(student_block = as.character(category), student_item = item_name)

crosswalk_full <- bind_rows(
  # matched pairs
  crosswalk %>%
    left_join(student_items, by = "student_item") %>%
    mutate(status = "Paired"),
  # staff items with no student counterpart
  staff_items %>%
    filter(!staff_item %in% crosswalk$staff_item) %>%
    mutate(student_item = NA_character_, student_block = NA_character_,
           status = "Staff tool only"),
  # student items with no staff counterpart
  student_items %>%
    filter(!student_item %in% crosswalk$student_item) %>%
    mutate(staff_item = NA_character_, domain = NA_character_,
           status = "Student tool only")
) %>%
  transmute(
    Status          = status,
    `Staff tool item`   = coalesce(staff_item, "\u2014"),
    `Staff tool block`  = coalesce(domain, "\u2014"),
    `Student tool item` = coalesce(student_item, "\u2014"),
    `Student tool block`= coalesce(student_block, "\u2014")
  ) %>%
  arrange(factor(Status, levels = c("Paired", "Staff tool only", "Student tool only")),
          `Staff tool item`)

write_csv(crosswalk_full, "table_crosswalk.csv")
print(crosswalk_full, n = Inf, width = Inf)

# --- rendered table for use as a supplemental figure -----------------------
tt <- ttheme_minimal(
  base_size = 8,
  core = list(fg_params = list(hjust = 0, x = 0.02),
              bg_params = list(fill = c("grey97", "white"), col = NA)),
  colhead = list(fg_params = list(fontface = "bold", hjust = 0, x = 0.02),
                 bg_params = list(fill = "grey88", col = NA)))

tbl <- tableGrob(crosswalk_full, rows = NULL, theme = tt)

figS2 <- arrangeGrob(
  tbl,
  top = textGrob("Crosswalk between the staff and student mapping instruments",
                 x = 0.02, hjust = 0, gp = gpar(fontface = "bold", fontsize = 10)),
  bottom = textGrob(paste(
    "The 17 SDGs are common to both tools and are matched directly by goal number;",
    "they are not listed here.\nUnpaired items are excluded from Figure 11 rather than",
    "matched to an approximate counterpart."),
    x = 0.02, hjust = 0, gp = gpar(fontsize = 7, col = "grey30")))

ggsave("FigureS2_crosswalk.png", figS2,
       width = 9, height = 0.28 * nrow(crosswalk_full) + 1.6, dpi = 400, bg = "white")


