library(dplyr)

df <- readRDS("combined_contracts.rds")

head(df)
View(df)

# ── Basic structure ───────────────────────────────────────────────────────────
cat("=== DIMENSIONS ===\n")
cat("Rows:", nrow(df), "| Cols:", ncol(df), "\n")

cat("\n=== COLUMN NAMES ===\n")
print(names(df))

cat("\n=== DATA TYPES ===\n")
print(sapply(df, class))

# ── Key financial columns ─────────────────────────────────────────────────────
# Pull a 5% sample to do your exploration
df_sample <- df %>% slice_sample(prop = 0.05)

# Now run everything on the sample safely
df_sample %>% select(all_of(financial_vars)) %>% summary() %>% print()

financial_vars <- c(
  "federal_action_obligation",
  "total_dollars_obligated",
  "base_and_exercised_options_value",
  "current_total_value_of_award",
  "base_and_all_options_value",
  "potential_total_value_of_award"
)

cat("\n=== FINANCIAL SUMMARY ===\n")
df %>% select(all_of(financial_vars)) %>% summary() %>% print()

# ── NA overview ───────────────────────────────────────────────────────────────
cat("\n=== NA COUNTS (all columns) ===\n")
na_summary <- df %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "na_count") %>%
  filter(na_count > 0) %>%
  arrange(desc(na_count))
print(na_summary)

# ── Categorical breakdowns ────────────────────────────────────────────────────
cat("\n=== AWARD TYPE DISTRIBUTION ===\n")
print(table(df$award_type_code, useNA = "ifany"))

cat("\n=== ACTION TYPE DISTRIBUTION ===\n")
print(table(df$action_type_code, useNA = "ifany"))

cat("\n=== EXTENT COMPETED ===\n")
print(table(df$extent_competed, useNA = "ifany"))

cat("\n=== CONTRACT PRICING TYPE ===\n")
print(table(df$type_of_contract_pricing, useNA = "ifany"))

# ── Agency breakdown ──────────────────────────────────────────────────────────
cat("\n=== TOP FUNDING AGENCIES ===\n")
df %>%
  count(funding_agency_name, sort = TRUE) %>%
  head(10) %>%
  print()

cat("\n=== TOP SUB-AGENCIES ===\n")
df %>%
  count(funding_sub_agency_name, sort = TRUE) %>%
  head(10) %>%
  print()

# ── Date range check ──────────────────────────────────────────────────────────
cat("\n=== ACTION DATE RANGE ===\n")
cat("Earliest:", as.character(min(df$action_date, na.rm = TRUE)), "\n")
cat("Latest:  ", as.character(max(df$action_date, na.rm = TRUE)), "\n")

cat("\n=== FISCAL YEARS COVERED ===\n")
print(table(df$action_date_fiscal_year, useNA = "ifany"))

# ── Top recipients ────────────────────────────────────────────────────────────
cat("\n=== TOP 10 RECIPIENTS BY TOTAL OBLIGATION ===\n")
df %>%
  group_by(recipient_name) %>%
  summarise(
    total_obligated = sum(total_dollars_obligated, na.rm = TRUE),
    n_contracts     = n()
  ) %>%
  arrange(desc(total_obligated)) %>%
  head(10) %>%
  print()

# ── State distribution ────────────────────────────────────────────────────────
cat("\n=== TOP 10 PERFORMANCE STATES ===\n")
df %>%
  count(primary_place_of_performance_state_name, sort = TRUE) %>%
  head(10) %>%
  print()