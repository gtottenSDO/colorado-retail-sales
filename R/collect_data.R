library(tidyverse)
library(janitor)
library(googlesheets4)
library(arrow)

gs4_deauth() # sheets are publicly shared

# ---------------------------------------------------------------------------
# Sheet IDs from cdor.colorado.gov/retail-sales-reports
# ---------------------------------------------------------------------------
cdor_sheets <- list(
  state = c(
    "2020_present" = "14bUpc2wr2G-oiFWsMiey_JuxDY8QCxnC",
    "2016_2019"    = "1UxCJLWjwH2nHruQvxrFr0WnFPPjpiVz3"
  ),
  county = c(
    "2020_present" = "14jmeYY3MGljgOEEVL3maLNrUb5H3Faln",
    "2016_2019"    = "1_c-W9_S7-5jpReahnk5PREzIyY-VbkQu"
  ),
  city = c(
    "2021_present" = "1WI2jvRCzBqYFIPA2vsUmCjHedKCQD911",
    "2016_2020"    = "1wo4WKzvlfdw47_3841jD2RUibbDwos3L"
  ),
  county_industry = c(
    "2025_present" = "17fjuS3i-jkL_0qW109m_URdHczclW2vO",
    "2022_2024"    = "1s1u_aKD3FlJuhgS-X94x6wY3Qmw8mLZX",
    "2016_2021"    = "1CI66-qv0ooK93asc21VyV-tJiYSc2J3c"
  ),
  city_industry = c(
    "2025_present" = "1vlYjz0LeQEg8cZ4DVVcAIjLkffAnML8t",
    "2022_2024"    = "1XoL9-WAa9LJecCJHAm4V9O8OqAqpKtJz",
    "2016_2021"    = "1ZKc0olDlChHyRiLlxL3ECxKqBYtyaUaB"
  ),
  state_industry = c(
    "2022_present" = "1cmTJ4ZRAjBFfevT0hyCatUXDIzIG393I",
    "2016_2021"    = "1WANzZQFE57J73daXvo1xu4mITy-1DmuP"
  )
)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Read one tab from a CDOR sheet, auto-detecting the header row.
# All values read as character first to avoid type coercion issues.
read_cdor_tab <- function(sheet_id, tab_name) {
  message("  Reading tab: ", tab_name)

  raw <- tryCatch(
    read_sheet(sheet_id, sheet = tab_name, col_types = "c", col_names = FALSE),
    error = \(e) { warning("Failed: ", tab_name, " — ", conditionMessage(e)); NULL }
  )
  if (is.null(raw) || nrow(raw) == 0) return(NULL)

  # Find first row that has both "Month" and "Year" as cell values
  header_row <- detect_index(seq_len(nrow(raw)), \(i) {
    cells <- as.character(unlist(raw[i, ]))
    any(cells == "Month") && any(cells == "Year")
  })

  if (header_row == 0) {
    warning("No header row found in tab: ", tab_name)
    return(NULL)
  }

  headers  <- as.character(unlist(raw[header_row, ]))
  data_raw <- raw[(header_row + 1):nrow(raw), ]
  names(data_raw) <- headers

  data_raw |>
    # Drop rows that are entirely NA or repeat the header
    filter(!is.na(Month), Month != "Month") |>
    janitor::clean_names() |>
    mutate(
      month = as.integer(month),
      year  = as.integer(year),
      # NR = not reported (cell suppression); treat as NA
      across(everything(), \(x) if_else(x == "NR", NA_character_, x)),
      # Parse all remaining numeric-looking columns
      across(
        -any_of(c("month", "year", "county", "city", "industry",
                  "x2022_naics_code", "naics_code", "sequence_number")),
        parse_number
      )
    ) |>
    mutate(date = lubridate::make_date(year, month, 1L))
}

# Read all tabs in a workbook and bind rows
read_cdor_sheet <- function(sheet_id) {
  meta      <- gs4_get(sheet_id)
  tab_names <- meta$sheets$name
  message("Sheet ", sheet_id, " has ", length(tab_names), " tab(s): ",
          paste(tab_names, collapse = ", "))

  map(tab_names, \(tab) read_cdor_tab(sheet_id, tab)) |>
    compact() |>
    list_rbind()
}

# Read and combine a named vector of sheet IDs, then deduplicate by key cols
read_and_bind <- function(ids, key_cols) {
  ids |>
    imap(\(id, period) {
      message("Fetching ", period, " (", id, ")")
      read_cdor_sheet(id)
    }) |>
    compact() |>
    list_rbind() |>
    distinct(across(all_of(key_cols)), .keep_all = TRUE) |>
    arrange(across(all_of(c("year", "month"))))
}

# ---------------------------------------------------------------------------
# Pull data
# ---------------------------------------------------------------------------

message("=== State ===")
state_raw <- read_and_bind(
  cdor_sheets$state,
  key_cols = c("year", "month")
)

message("=== County ===")
county_raw <- read_and_bind(
  cdor_sheets$county,
  key_cols = c("year", "month", "county")
)

message("=== City ===")
city_raw <- read_and_bind(
  cdor_sheets$city,
  key_cols = c("year", "month", "city")
)

message("=== County by Industry ===")
county_industry_raw <- read_and_bind(
  cdor_sheets$county_industry,
  key_cols = c("year", "month", "county", "industry")
)

message("=== City by Industry ===")
city_industry_raw <- read_and_bind(
  cdor_sheets$city_industry,
  key_cols = c("year", "month", "city", "industry")
)

message("=== State by Industry ===")
state_industry_raw <- read_and_bind(
  cdor_sheets$state_industry,
  key_cols = c("year", "month", "industry")
)

# ---------------------------------------------------------------------------
# Standardise column names across datasets
# ---------------------------------------------------------------------------

# Rename footnoted retailer/return columns to clean names
rename_common <- function(df) {
  df |>
    rename_with(\(x) str_replace(x, "number_of_retailers.*", "n_retailers")) |>
    rename_with(\(x) str_replace(x, "number_of_returns.*",   "n_returns"))
}

state_raw          <- rename_common(state_raw)
county_raw         <- rename_common(county_raw)
city_raw           <- rename_common(city_raw)
county_industry_raw <- rename_common(county_industry_raw)
city_industry_raw  <- rename_common(city_industry_raw)
state_industry_raw <- rename_common(state_industry_raw)

# ---------------------------------------------------------------------------
# Spot-check: Adams Co Feb 2026 Total should be $2,259,034,801
# ---------------------------------------------------------------------------
check_val <- county_industry_raw |>
  filter(year == 2026, month == 2, county == "Adams", industry == "Total") |>
  pull(retail_sales)
stopifnot(
  "Data integrity check failed: Adams County Feb 2026 Total" =
    length(check_val) == 1 && abs(check_val - 2259034801) < 1000
)
message("Integrity check passed: Adams Co Feb 2026 = $", format(check_val, big.mark = ","))

# ---------------------------------------------------------------------------
# Save as parquet
# ---------------------------------------------------------------------------
dir.create("data", showWarnings = FALSE)

write_parquet(state_raw,           "data/state.parquet")
write_parquet(county_raw,          "data/county.parquet")
write_parquet(city_raw,            "data/city.parquet")
write_parquet(county_industry_raw, "data/county_industry.parquet")
write_parquet(city_industry_raw,   "data/city_industry.parquet")
write_parquet(state_industry_raw,  "data/state_industry.parquet")

message("Done. Parquet files written to data/")
