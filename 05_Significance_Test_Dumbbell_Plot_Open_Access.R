# =============================================================================
# SOS-UK ESD Mapping — Supplemental table: staff/student significance testing
# Standalone script. Sources your significance testing script (which itself
# sources the staff mapping script), rather than recomputing the tests here,
# so this table always reflects the exact same p-values behind Figure 11's
# annotations, not a second, separately-run copy of the analysis.
# =============================================================================

SIG_TEST_SCRIPT <- "[FILE PATH].xlsx" # Script will run when full file path is uploaded in here
source(SIG_TEST_SCRIPT)

# ---- Build the table ----------------------------------------------------------
# p_raw and p_adj are shown side by side deliberately — a reader of the
# supplement should be able to see both the uncorrected and the corrected
# value, not just be told to trust the corrected one.

supplemental_significance_table <- sig_results %>%
  transmute(
    Domain = domain,
    Item = student_item,
    `Staff n` = staff_n,
    `Student n` = student_n,
    `p (raw)` = round(p_raw, 4),
    `p (adjusted, BH)` = round(p_adj, 4),
    `Significant (p < .05)` = if_else(significant, "Yes", "No")
  ) %>%
  arrange(Domain, `p (adjusted, BH)`)

print(supplemental_significance_table, n = Inf)

# ---- Export ---------------------------------------------------------------------
write_csv(supplemental_significance_table, "table_S5_staff_student_significance.csv")

# If you're keeping supplemental tables together in one workbook (matching
# S1-S4 already built in your staff script), this appends as its own sheet
# rather than a separate file:
# writexl::write_xlsx(
#   list(`S1 Staff summary`      = staff_summary,
#        `S2 Staff-student gaps` = gap_table,
#        `S3 SDG key`            = readr::read_csv("table_sdg_key.csv", show_col_types = FALSE),
#        `S4 Crosswalk`          = crosswalk_full,
#        `S5 Significance tests` = supplemental_significance_table),
#   "Supplementary_tables.xlsx"
# )

# =============================================================================
# Rows with NA in either p-value column are the items that couldn't be
# tested at all (fewer than 2 observations on one side) — these still print
# in the table with blank p-values rather than being silently dropped, so
# the supplement itself documents the gap rather than just this analysis.
# =============================================================================

