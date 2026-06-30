library(tidyverse)
library(janitor)
library(readxl)
library(arrow)

# ---------------------------------------------------------------------------
# Sheet IDs from cdor.colorado.gov/retail-sales-reports
# Files are Excel workbooks stored in Google Drive (rtpof=true in share URLs).
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
    "2025_present" = "17fjuS3i-jkL_0qW109m_URdHczclW2vO",   # long: all counties
    "2022_2024"    = "1s1u_aKD3FlJuhgS-X94x6wY3Qmw8mLZX",   # wide: 11 major counties
    "2016_2021"    = "1CI66-qv0ooK93asc21VyV-tJiYSc2J3c"    # wide: 11 major counties
  ),
  city_industry = c(
    "2025_present" = "1vlYjz0LeQEg8cZ4DVVcAIjLkffAnML8t",   # long: all cities
    "2022_2024"    = "1XoL9-WAa9LJecCJHAm4V9O8OqAqpKtJz",   # wide: 13 major cities
    "2016_2021"    = "1ZKc0olDlChHyRiLlxL3ECxKqBYtyaUaB"    # wide: 13 major cities
  ),
  state_industry = c(
    "2022_present" = "1cmTJ4ZRAjBFfevT0hyCatUXDIzIG393I",
    "2016_2021"    = "1WANzZQFE57J73daXvo1xu4mITy-1DmuP"
  )
)

# Columns that uniquely identify wide-format county vs city sheets
WIDE_COUNTY_SENTINEL <- c("adams", "weld")          # only in county wide format
WIDE_CITY_SENTINEL   <- c("arvada", "thornton")     # only in city wide format
ALL_WIDE_COUNTY      <- c("adams", "arapahoe", "boulder", "denver", "douglas",
                           "el_paso", "jefferson", "larimer", "mesa", "pueblo", "weld")
ALL_WIDE_CITY        <- c("arvada", "aurora", "boulder", "centennial",
                           "colorado_springs", "denver", "fort_collins", "greeley",
                           "lakewood", "longmont", "pueblo", "thornton", "westminster")

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Original workbooks are kept here so they can be opened in Excel directly
RAW_XLSX_DIR <- "data/raw"

download_xlsx <- function(sheet_id, dest_name = NULL, force_refresh = FALSE) {
  url <- paste0("https://docs.google.com/spreadsheets/d/", sheet_id, "/export?format=xlsx")
  if (is.null(dest_name)) {
    dest <- tempfile(fileext = ".xlsx")
  } else {
    dir.create(RAW_XLSX_DIR, recursive = TRUE, showWarnings = FALSE)
    dest <- file.path(RAW_XLSX_DIR, paste0(dest_name, ".xlsx"))
    # Historical files are immutable — skip download if already cached
    if (!force_refresh && file.exists(dest)) {
      message("    cached: ", dest)
      return(dest)
    }
  }
  # Google's export endpoint fails transiently now and then (e.g. HTTP/2
  # stream errors took down the June 2026 scheduled refresh), so retry
  for (attempt in 1:3) {
    ok <- tryCatch(
      {
        download.file(url, dest, mode = "wb", quiet = TRUE)
        TRUE
      },
      error = function(e) FALSE,
      warning = function(w) FALSE
    )
    if (ok) return(dest)
    if (attempt < 3) {
      message("    download failed (attempt ", attempt, "/3), retrying...")
      Sys.sleep(5 * attempt)
    }
  }
  stop("Failed to download sheet ", sheet_id, " after 3 attempts")
}

read_cdor_tab <- function(path, tab_name) {
  raw <- tryCatch(
    read_excel(path, sheet = tab_name, col_names = FALSE, col_types = "text"),
    error = \(e) { warning("Failed tab: ", tab_name, " — ", conditionMessage(e)); NULL }
  )
  if (is.null(raw) || nrow(raw) == 0) return(NULL)

  # Auto-detect header row: first row with both "Month" and "Year" cells
  header_row <- detect_index(seq_len(nrow(raw)), \(i) {
    cells <- as.character(unlist(raw[i, ]))
    any(cells == "Month", na.rm = TRUE) && any(cells == "Year", na.rm = TRUE)
  })
  if (header_row == 0) { warning("No header row: ", tab_name); return(NULL) }

  df <- read_excel(path, sheet = tab_name, skip = header_row - 1, col_types = "text")

  result <- df |>
    filter(!is.na(Month), Month != "Month") |>
    janitor::clean_names() |>
    # Normalise footnote-suffixed dimension column names (Industry ¹ → industry_1 → industry)
    rename_with(\(x) str_replace(x, "^industry.*",           "industry")) |>
    rename_with(\(x) str_replace(x, "^(x2022_)?naics_code.*", "naics_code")) |>
    rename_with(\(x) str_replace(x, "^sequence_number.*",    "sequence_number")) |>
    rename_with(\(x) str_replace(x, "^number_of_retailers.*","n_retailers")) |>
    rename_with(\(x) str_replace(x, "^number_of_returns.*",  "n_returns")) |>
    # NR = suppressed small-n; replace before type conversion
    mutate(across(where(is.character), \(x) na_if(x, "NR"))) |>
    mutate(
      month = suppressWarnings(as.integer(month)),
      year  = suppressWarnings(as.integer(year))
    ) |>
    # Remove footer/header-repeat rows that aren't real data
    filter(!is.na(month), !is.na(year), between(month, 1L, 12L)) |>
    # Parse numeric cols; skip known dimension cols
    mutate(across(
      -any_of(c("month", "year", "county", "city", "industry",
                "naics_code", "sequence_number")),
      parse_number
    )) |>
    mutate(date = lubridate::make_date(year, month, 1L))

  # Pivot wide-format geo sheets → long format
  cols <- names(result)
  if (any(WIDE_COUNTY_SENTINEL %in% cols)) {
    result <- result |>
      tidyr::pivot_longer(
        cols      = any_of(ALL_WIDE_COUNTY),
        names_to  = "county",
        values_to = "retail_sales"
      ) |>
      mutate(county = str_to_title(str_replace_all(county, "_", " ")))
  } else if (any(WIDE_CITY_SENTINEL %in% cols)) {
    result <- result |>
      tidyr::pivot_longer(
        cols      = any_of(ALL_WIDE_CITY),
        names_to  = "city",
        values_to = "retail_sales"
      ) |>
      mutate(city = str_to_title(str_replace_all(city, "_", " ")))
  }

  message("    cols: ", paste(names(result), collapse = ", "))
  result
}

read_cdor_workbook <- function(sheet_id, dest_name = NULL, force_refresh = FALSE) {
  path      <- download_xlsx(sheet_id, dest_name, force_refresh = force_refresh)
  tab_names <- excel_sheets(path)
  message("    tabs: ", paste(tab_names, collapse = ", "))
  map(tab_names, \(tab) read_cdor_tab(path, tab)) |> compact() |> list_rbind()
}

read_and_bind <- function(ids, key_cols, dataset = NULL) {
  ids |>
    imap(\(id, period) {
      # Always re-download periods that extend into the current year so the
      # data reflects the latest published month.
      is_current <- grepl("present|2025", period, ignore.case = TRUE)
      message("  Fetching ", period, " (", id, ")",
              if (is_current) " [force refresh]" else " [cached if present]")
      read_cdor_workbook(
        id,
        dest_name     = if (is.null(dataset)) NULL else paste(dataset, period, sep = "_"),
        force_refresh = is_current
      )
    }) |>
    compact() |>
    list_rbind() |>
    distinct(across(any_of(key_cols)), .keep_all = TRUE) |>
    arrange(year, month)
}

# ---------------------------------------------------------------------------
# Pull data
# ---------------------------------------------------------------------------

message("=== State ===")
state_raw <- read_and_bind(cdor_sheets$state, c("year", "month"), dataset = "state")

message("=== County ===")
county_raw <- read_and_bind(cdor_sheets$county, c("year", "month", "county"), dataset = "county")

message("=== City ===")
city_raw <- read_and_bind(cdor_sheets$city, c("year", "month", "city"), dataset = "city")

message("=== County by Industry ===")
county_industry_raw <- read_and_bind(
  cdor_sheets$county_industry, c("year", "month", "county", "industry"),
  dataset = "county_industry"
)

message("=== City by Industry ===")
city_industry_raw <- read_and_bind(
  cdor_sheets$city_industry, c("year", "month", "city", "industry"),
  dataset = "city_industry"
)

message("=== State by Industry ===")
state_industry_raw <- read_and_bind(
  cdor_sheets$state_industry, c("year", "month", "industry"),
  dataset = "state_industry"
)

# ---------------------------------------------------------------------------
# Integrity check: Adams Co Feb 2026 Total = $2,259,034,801
# ---------------------------------------------------------------------------
check_val <- county_industry_raw |>
  filter(year == 2026, month == 2, county == "Adams", industry == "Total") |>
  pull(retail_sales)
stopifnot(
  "Integrity check failed: Adams County Feb 2026 Total" =
    length(check_val) == 1 && abs(check_val - 2259034801) < 1000
)
message("Integrity check passed: Adams Co Feb 2026 = $", format(check_val, big.mark = ","))

# ---------------------------------------------------------------------------
# Annualize current year
# ---------------------------------------------------------------------------
# For any year that is still in progress, annual roll-ups in the dashboard
# would undercount because not all 12 months are present.  We annotate every
# row with two columns so the dashboard can project partial years:
#
#   is_annualized        TRUE  for the current calendar year, FALSE otherwise
#   annualized_multiplier  12 / n_months_reported  for the current year,
#                          1 for all prior (complete) years
#
# To produce an annualized annual total from monthly data, multiply each
# month's retail_sales by annualized_multiplier then sum — or equivalently,
# sum the year and multiply the total by the multiplier.

cur_year   <- lubridate::year(Sys.Date())
n_months   <- state_raw |> filter(year == cur_year) |> distinct(month) |> nrow()
cur_mult   <- if (n_months > 0) 12 / n_months else 1L

message(sprintf(
  "Current year %d: %d month(s) reported → annualized multiplier = %.4f (%.1f×)",
  cur_year, n_months, cur_mult, cur_mult
))

annotate_annualized <- function(df) {
  df |> mutate(
    is_annualized         = (year == cur_year),
    annualized_multiplier = if_else(is_annualized, cur_mult, 1)
  )
}

state_raw           <- annotate_annualized(state_raw)
county_raw          <- annotate_annualized(county_raw)
city_raw            <- annotate_annualized(city_raw)
county_industry_raw <- annotate_annualized(county_industry_raw)
city_industry_raw   <- annotate_annualized(city_industry_raw)
state_industry_raw  <- annotate_annualized(state_industry_raw)

# ---------------------------------------------------------------------------
# Save
# ---------------------------------------------------------------------------
dir.create("data", showWarnings = FALSE)
write_parquet(state_raw,           "data/state.parquet")
write_parquet(county_raw,          "data/county.parquet")
write_parquet(city_raw,            "data/city.parquet")
write_parquet(county_industry_raw, "data/county_industry.parquet")
write_parquet(city_industry_raw,   "data/city_industry.parquet")
write_parquet(state_industry_raw,  "data/state_industry.parquet")

message("Done. Parquet files written to data/; original workbooks saved to ", RAW_XLSX_DIR, "/")
