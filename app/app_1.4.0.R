library(shiny)
library(R6)
library(readxl)
library(writexl)
library(dplyr)
library(lubridate)
library(stringr)
library(reactable)
library(htmltools)

# --- Helper: Parse Extract Datetimes (Handles POSIXct, 12-hr AM/PM, and 24-hr formats) ---
parse_extract_datetime <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(as.POSIXct(format(x, "%Y-%m-%d %H:%M:%S"), tz = "UTC"))
  }
  if (inherits(x, "Date")) {
    return(as.POSIXct(paste0(format(x, "%Y-%m-%d"), " 00:00:00"), tz = "UTC"))
  }
  chr <- str_trim(as.character(x))
  chr[chr == ""] <- NA_character_
  out <- rep(as.POSIXct(NA, origin = "1970-01-01", tz = "UTC"), length(chr))
  non_na <- which(!is.na(chr))
  if (length(non_na) == 0) return(out)

  has_ampm <- grepl("(am|pm)\\s*$", chr[non_na], ignore.case = TRUE)
  idx_12 <- non_na[has_ampm]
  idx_24 <- non_na[!has_ampm]

  if (length(idx_12) > 0) {
    out[idx_12] <- suppressWarnings(
      parse_date_time(chr[idx_12], orders = c("dmy IMSp", "dmy IMp", "ymd IMSp", "ymd IMp"), tz = "UTC", quiet = TRUE)
    )
  }
  if (length(idx_24) > 0) {
    out[idx_24] <- suppressWarnings(
      parse_date_time(chr[idx_24], orders = c("dmy HMS", "dmy HM", "ymd HMS", "ymd HM"), tz = "UTC", quiet = TRUE)
    )
  }
  out
}

# --- Helper: Standardize Displayed Datetimes to 24-hr Chronological Format ---
format_extract_datetime <- function(dt, raw_x) {
  out <- as.character(raw_x)
  ok  <- !is.na(dt)
  out[ok] <- format(dt[ok], "%Y-%m-%d %H:%M:%S")
  out
}

# --- Helper: Normalize Column Vector for Consistent Filter Matching ---
norm_filter_vec <- function(x) {
  chr <- as.character(x)
  chr[is.na(chr)] <- ""
  chr
}

# --- Helper: Extract YYYY-MM-DD Key for Date Columns ---
extract_date_key <- function(x) {
  chr <- norm_filter_vec(x)
  is_iso <- grepl("^\\d{4}-\\d{2}-\\d{2}", chr)
  chr[is_iso] <- substr(chr[is_iso], 1L, 10L)
  chr
}

# --- Helper: Sort Unique Filter Values Once in R (Numeric / % / Text) ---
sort_filter_values <- function(u) {
  if (length(u) == 0) return(character(0))
  u_chr <- unique(norm_filter_vec(u))
  blanks <- u_chr == ""
  non_blanks <- u_chr[!blanks]
  if (length(non_blanks) > 0) {
    num_part <- suppressWarnings(as.numeric(sub("%$", "", non_blanks)))
    if (all(!is.na(num_part))) {
      non_blanks <- non_blanks[order(num_part, non_blanks)]
    } else {
      non_blanks <- sort(non_blanks)
    }
  }
  if (any(blanks)) c(non_blanks, "") else non_blanks
}

# --- Helper: Parse legacy LTAT strings (e.g. "48 - 72 hrs", "5-8 days", "same day", "next day") ---
infer_rule_from_legacy_text <- function(ltat_str) {
  s <- str_to_lower(str_trim(as.character(ltat_str)))
  if (is.na(s) || s == "" || s == "n/a") {
    return(list(Rule_Type = "Duration", Dur_DD = 0L, Dur_HH = 0L, Dur_MM = 0L,
                Skip_Days = "", Skip_Holidays = FALSE, Cutoff_HHMM = "", Target_HHMM = ""))
  }
  if (str_detect(s, "same day")) {
    return(list(Rule_Type = "Same Day", Dur_DD = 0L, Dur_HH = 0L, Dur_MM = 0L,
                Skip_Days = "", Skip_Holidays = FALSE, Cutoff_HHMM = "", Target_HHMM = ""))
  }
  if (str_detect(s, "next day")) {
    return(list(Rule_Type = "Next Day", Dur_DD = 0L, Dur_HH = 0L, Dur_MM = 0L,
                Skip_Days = "", Skip_Holidays = FALSE, Cutoff_HHMM = "", Target_HHMM = ""))
  }
  nums <- as.numeric(str_extract_all(s, "\\d+\\.?\\d*")[[1]])
  max_val <- if (length(nums) > 0) max(nums) else 0
  if (str_detect(s, "day")) {
    return(list(Rule_Type = "Duration", Dur_DD = as.integer(floor(max_val)),
                Dur_HH = as.integer(round((max_val %% 1) * 24)), Dur_MM = 0L,
                Skip_Days = "", Skip_Holidays = FALSE, Cutoff_HHMM = "", Target_HHMM = ""))
  } else if (str_detect(s, "min")) {
    return(list(Rule_Type = "Duration", Dur_DD = 0L, Dur_HH = 0L, Dur_MM = as.integer(round(max_val)),
                Skip_Days = "", Skip_Holidays = FALSE, Cutoff_HHMM = "", Target_HHMM = ""))
  } else {
    return(list(Rule_Type = "Duration", Dur_DD = 0L, Dur_HH = as.integer(floor(max_val)),
                Dur_MM = as.integer(round((max_val %% 1) * 60)),
                Skip_Days = "", Skip_Holidays = FALSE, Cutoff_HHMM = "", Target_HHMM = ""))
  }
}

# --- Helper: Format human-readable rule summary for LTAT table ---
format_rule_summary <- function(rule_type, dd, hh, mm, skip_days, skip_hol, cutoff_hm, target_hm) {
  mapply(function(rt, d, h, m, sd, sh, chm, thm) {
    if (is.na(rt) || rt == "") return("Unconfigured")
    if (rt == "Same Day") {
      return("Same calendar date")
    } else if (rt == "Next Day") {
      return("Next calendar date (by 23:59:59)")
    } else if (rt == "Next Day Cutoff") {
      return(sprintf("Before %s -> Same day | >= %s -> Next day by %s", chm, chm, thm))
    } else {
      parts <- c()
      if (!is.na(d) && d > 0) parts <- c(parts, paste0(d, "d"))
      if (!is.na(h) && h > 0) parts <- c(parts, paste0(h, "h"))
      if (!is.na(m) && m > 0) parts <- c(parts, paste0(m, "m"))
      dur_txt <- if (length(parts) == 0) "0m" else paste(parts, collapse = " ")
      extra <- c()
      if (!is.na(sd) && nzchar(sd)) extra <- c(extra, paste0("Skip: ", sd))
      if (!is.na(sh) && isTRUE(sh)) extra <- c(extra, "Skip Holidays")
      if (length(extra) > 0) return(paste0(dur_txt, " (", paste(extra, collapse = "; "), ")"))
      return(dur_txt)
    }
  }, rule_type, dd, hh, mm, skip_days, skip_hol, cutoff_hm, target_hm, USE.NAMES = FALSE)
}

# --- Helper: Format column names ---
format_col_title <- function(cols) {
  vapply(cols, function(x) {
    words <- str_split(str_replace_all(x, "_", " "), "\\s+")[[1]]
    words <- vapply(words, function(w) {
      if (w == "") return("")
      if (str_detect(w, "^[A-Z0-9]+$")) return(w)
      paste0(toupper(substr(w, 1, 1)), substr(w, 2, nchar(w)))
    }, character(1))
    paste(words, collapse = " ")
  }, character(1), USE.NAMES = FALSE)
}

# --- Vectorized Helper: Calculate elapsed hours excluding specified weekdays (WebR Fast) ---
calc_effective_hrs <- function(dt_start, dt_end, skip_days_str) {
  raw_hrs <- as.numeric(difftime(dt_end, dt_start, units = "hours"))
  if (is.na(skip_days_str) || str_trim(skip_days_str) == "") {
    return(round(raw_hrs, 2))
  }
  skip_vec <- str_trim(str_split(skip_days_str, ",")[[1]])
  skip_vec <- skip_vec[skip_vec != ""]
  if (length(skip_vec) == 0) return(round(raw_hrs, 2))
  
  day_map <- c("Sun" = 0L, "Mon" = 1L, "Tue" = 2L, "Wed" = 3L, "Thu" = 4L, "Fri" = 5L, "Sat" = 6L)
  skip_wdays <- unname(day_map[skip_vec])
  skip_wdays <- skip_wdays[!is.na(skip_wdays)]
  if (length(skip_wdays) == 0) return(round(raw_hrs, 2))
  
  s_num <- as.numeric(dt_start)
  e_num <- as.numeric(dt_end)
  out   <- round((e_num - s_num) / 3600, 2)
  
  valid <- which(!is.na(s_num) & !is.na(e_num) & e_num > s_num)
  if (length(valid) == 0) return(out)
  
  tz_str <- attr(dt_start, "tzone")
  if (is.null(tz_str) || tz_str == "") tz_str <- "UTC"
  ref_epoch <- as.numeric(as.POSIXct("1970-01-01 00:00:00", tz = tz_str))
  
  for (i in valid) {
    s <- s_num[i] - ref_epoch
    e <- e_num[i] - ref_epoch
    d0 <- floor(s / 86400)
    d1 <- floor(e / 86400)
    days <- d0:d1
    wdays <- (days + 4L) %% 7L
    keep <- !(wdays %in% skip_wdays)
    if (all(keep)) next
    if (!any(keep)) {
      out[i] <- 0
      next
    }
    d_open  <- days[keep] * 86400
    d_close <- d_open + 86400
    seg_s   <- pmax(s, d_open)
    seg_e   <- pmin(e, d_close)
    out[i]  <- round(sum(pmax(0, seg_e - seg_s)) / 3600, 2)
  }
  out
}

# --- Vectorized Helper: Evaluate PASS / FAIL / N/A (WebR Fast) ---
evaluate_ltat_status <- function(dt_unit, dt_valid, eff_hrs, rule_type, dur_dd, dur_hh, dur_mm,
                                 cutoff_hm, target_hm, skip_days = "",
                                 exc_codes = rep("", length(dt_unit)),
                                 exc_ext_hrs = rep(0, length(dt_unit))) {
  n <- length(dt_unit)
  if (n == 0) return(character(0))
  
  status <- rep("N/A", n)
  valid  <- !is.na(rule_type) & rule_type != "" & !is.na(dt_unit) & !is.na(dt_valid)
  if (!any(valid)) return(status)
  
  has_c1 <- grepl("(^|,)1(,|$)", exc_codes)
  has_c2 <- grepl("(^|,)2(,|$)", exc_codes)
  has_c3 <- grepl("(^|,)3(,|$)", exc_codes)
  has_c4 <- grepl("(^|,)4(,|$)", exc_codes)
  
  u_eval <- dt_unit
  v_eval <- dt_valid
  if (any(has_c1 & valid)) {
    u_eval[has_c1] <- floor_date(u_eval[has_c1], "hour")
    v_eval[has_c1] <- floor_date(v_eval[has_c1], "hour")
  }
  
  extra_sec <- ifelse(has_c3 & !is.na(exc_ext_hrs), as.numeric(exc_ext_hrs) * 3600, 0)
  
  # 1. Same Day
  idx_sd <- which(valid & rule_type == "Same Day")
  if (length(idx_sd) > 0) {
    base_dl <- as.POSIXct(paste0(as.Date(u_eval[idx_sd]), " 23:59:59"), tz = tz(u_eval))
    base_dl <- base_dl + ifelse(has_c2[idx_sd], 86400, 0) + extra_sec[idx_sd]
    status[idx_sd] <- ifelse(v_eval[idx_sd] <= base_dl, "PASS", "FAIL")
  }
  
  # 1b. Next Day (Time-Ignoring Calendar Date)
  idx_nd_pure <- which(valid & rule_type == "Next Day")
  if (length(idx_nd_pure) > 0) {
    base_dl <- as.POSIXct(paste0(as.Date(u_eval[idx_nd_pure]) + 1, " 23:59:59"), tz = tz(u_eval))
    base_dl <- base_dl + ifelse(has_c2[idx_nd_pure], 86400, 0) + extra_sec[idx_nd_pure]
    status[idx_nd_pure] <- ifelse(v_eval[idx_nd_pure] <= base_dl, "PASS", "FAIL")
  }
  
  # 2. Next Day Cutoff
  idx_nd <- which(valid & rule_type == "Next Day Cutoff" & !is.na(cutoff_hm) & cutoff_hm != "" & !is.na(target_hm) & target_hm != "")
  if (length(idx_nd) > 0) {
    u_date    <- as.Date(u_eval[idx_nd])
    cutoff_dt <- suppressWarnings(as.POSIXct(paste0(u_date, " ", cutoff_hm[idx_nd], ":00"), tz = tz(u_eval)))
    same_dl   <- as.POSIXct(paste0(u_date, " 23:59:59"), tz = tz(u_eval))
    next_dl   <- suppressWarnings(as.POSIXct(paste0(u_date + 1, " ", target_hm[idx_nd], ":00"), tz = tz(u_eval)))
    
    dl <- ifelse(u_eval[idx_nd] < cutoff_dt, same_dl, next_dl)
    dl <- as.POSIXct(dl, origin = "1970-01-01", tz = tz(u_eval))
    
    c2_sub <- has_c2[idx_nd] & !is.na(dl)
    if (any(c2_sub)) {
      dl[c2_sub] <- as.POSIXct(paste0(as.Date(dl[c2_sub]) + 1, " 23:59:59"), tz = tz(u_eval))
    }
    dl <- dl + extra_sec[idx_nd]
    ok_dl <- !is.na(cutoff_dt) & !is.na(dl)
    status[idx_nd[ok_dl]] <- ifelse(v_eval[idx_nd[ok_dl]] <= dl[ok_dl], "PASS", "FAIL")
  }
  
  # 3. Duration
  idx_dur <- which(valid & !(rule_type %in% c("Same Day", "Next Day", "Next Day Cutoff")))
  if (length(idx_dur) > 0) {
    t_hrs <- (ifelse(is.na(dur_dd[idx_dur]), 0, dur_dd[idx_dur]) * 24) +
             ifelse(is.na(dur_hh[idx_dur]), 0, dur_hh[idx_dur]) +
             (ifelse(is.na(dur_mm[idx_dur]), 0, dur_mm[idx_dur]) / 60)
    
    ok_t <- t_hrs > 0
    idx_dur <- idx_dur[ok_t]
    t_hrs   <- t_hrs[ok_t]
    
    if (length(idx_dur) > 0) {
      eval_ehrs <- eff_hrs[idx_dur]
      c1_sub <- has_c1[idx_dur]
      if (any(c1_sub)) {
        sub_i <- idx_dur[c1_sub]
        eval_ehrs[c1_sub] <- round(as.numeric(difftime(v_eval[sub_i], u_eval[sub_i], units = "hours")), 2)
      }
      
      c2_sub <- has_c2[idx_dur]
      if (any(c2_sub)) {
        sub_i <- idx_dur[c2_sub]
        t_days <- ceiling(t_hrs[c2_sub] / 24)
        dl <- as.POSIXct(paste0(as.Date(u_eval[sub_i]) + t_days + 1, " 23:59:59"), tz = tz(u_eval)) + extra_sec[sub_i]
        status[sub_i] <- ifelse(v_eval[sub_i] <= dl, "PASS", "FAIL")
      }
      
      std_sub <- !c2_sub & !is.na(eval_ehrs)
      if (any(std_sub)) {
        sub_i <- idx_dur[std_sub]
        status[sub_i] <- ifelse(eval_ehrs[std_sub] <= (t_hrs[std_sub] + (extra_sec[sub_i] / 3600)), "PASS", "FAIL")
      }
    }
  }
  
  # 4. Sure Pass Override
  status[valid & has_c4] <- "PASS"
  status
}

# --- Helper: Render Color-Coded Circled Exception Badges ---
format_exception_badges_html <- function(exc_codes_vec, exc_hrs_vec,
                                         exc_nd_cutoff_vec = rep("", length(exc_codes_vec)),
                                         superscript = FALSE) {
  badge_meta <- list(
    "1" = list(sym = "1", color = "#0d6efd", bg = "#e7f1ff", label = "Ignore mm:ss"),
    "2" = list(sym = "2", color = "#6f42c1", bg = "#f3e8ff", label = "Extend to Next Day"),
    "3" = list(sym = "3", color = "#d97706", bg = "#fef3c7", label = "Extend Hrs"),
    "4" = list(sym = "4", color = "#198754", bg = "#d1e7dd", label = "Sure Pass")
  )
  
  mapply(function(ec, ehrs, ndc) {
    if (is.na(ec) || !nzchar(ec)) return("")
    codes <- sort(unique(str_split(ec, ",")[[1]]))
    codes <- codes[codes %in% names(badge_meta)]
    if (length(codes) == 0) return("")
    
    spans <- vapply(codes, function(c_id) {
      m <- badge_meta[[c_id]]
      tip <- if (c_id == "3") {
        sprintf("%s (+%sh)", m$label, ehrs)
      } else if (c_id == "2" && !is.na(ndc) && nzchar(ndc)) {
        sprintf("%s (>= %s)", m$label, ndc)
      } else {
        m$label
      }
      cls <- if (superscript) "exc-badge-sup" else "exc-badge-norm"
      sprintf(
        '<span class="%s" title="%s" style="color:%s; background:%s; border:1px solid %s;">%s</span>',
        cls, tip, m$color, m$bg, m$color, m$sym
      )
    }, character(1))
    
    inner <- paste(spans, collapse = " ")
    if (superscript) paste0("<sup>", inner, "</sup>") else inner
  }, exc_codes_vec, exc_hrs_vec, exc_nd_cutoff_vec, USE.NAMES = FALSE)
}

# --- SVG Rule-Book with Minus Sign Icon ---
rule_exception_icon_svg <- function() {
  HTML('
    <svg width="24" height="22" viewBox="0 0 28 24" fill="none" xmlns="http://www.w3.org/2000/svg" style="vertical-align: middle;">
      <rect x="1" y="3" width="22" height="18" rx="2" fill="#3b66c4" stroke="#274996" stroke-width="1.2"/>
      <path d="M3 4.5C6.5 3.5 10 4 12 5.2V19.5C10 18.3 6.5 17.8 3 18.8V4.5Z" fill="#ffffff"/>
      <path d="M21 4.5C17.5 3.5 14 4 12 5.2V19.5C14 18.3 17.5 17.8 21 18.8V4.5Z" fill="#f8fafc"/>
      <line x1="12" y1="5.2" x2="12" y2="19.5" stroke="#94a3b8" stroke-width="1"/>
      <line x1="4.8" y1="7.2" x2="10.2" y2="7.2" stroke="#94a3b8" stroke-width="1.1" stroke-linecap="round"/>
      <line x1="4.8" y1="9.6" x2="9.5" y2="9.6" stroke="#dc2626" stroke-width="1.6" stroke-linecap="round"/>
      <line x1="5.8" y1="12.0" x2="10.2" y2="12.0" stroke="#3b66c4" stroke-width="1.3" stroke-linecap="round"/>
      <line x1="5.8" y1="14.4" x2="9.8" y2="14.4" stroke="#3b66c4" stroke-width="1.3" stroke-linecap="round"/>
      <line x1="4.8" y1="16.8" x2="10.2" y2="16.8" stroke="#94a3b8" stroke-width="1.1" stroke-linecap="round"/>
      <line x1="13.8" y1="7.2" x2="19.2" y2="7.2" stroke="#94a3b8" stroke-width="1.1" stroke-linecap="round"/>
      <line x1="13.8" y1="9.6" x2="19.2" y2="9.6" stroke="#94a3b8" stroke-width="1.1" stroke-linecap="round"/>
      <line x1="13.8" y1="12.0" x2="19.2" y2="12.0" stroke="#94a3b8" stroke-width="1.1" stroke-linecap="round"/>
      <line x1="13.8" y1="14.4" x2="19.2" y2="14.4" stroke="#94a3b8" stroke-width="1.1" stroke-linecap="round"/>
      <line x1="13.8" y1="16.8" x2="19.2" y2="16.8" stroke="#94a3b8" stroke-width="1.1" stroke-linecap="round"/>
      <circle cx="21.5" cy="17.5" r="5.5" fill="#dc3545" stroke="#ffffff" stroke-width="1.3"/>
      <line x1="18.7" y1="17.5" x2="24.3" y2="17.5" stroke="#ffffff" stroke-width="2" stroke-linecap="round"/>
    </svg>
  ')
}

# --- Helper: Clear All Filters Bar (per Report Table) ---
clear_filters_bar <- function(table_id) {
  div(
    id = paste0("clear_filters_bar_", table_id),
    class = "table-filter-bar",
    style = "display: none;",
    tags$button(
      type = "button",
      class = "clear-all-filters-btn",
      onclick = sprintf("clearAllTableFilters('%s')", table_id),
      HTML('&#10006; Clear All Filters <span class="filter-count-badge"></span>')
    )
  )
}

# --- Helper: Safe Tabular Reader with Error Capture ---
read_tabular_safe <- function(file_input) {
  tryCatch({
    ext <- tolower(tools::file_ext(file_input$name))
    if (ext %in% c("xlsx", "xls")) {
      df <- read_excel(file_input$datapath, sheet = 1)
    } else if (ext == "csv") {
      df <- read.csv(file_input$datapath, stringsAsFactors = FALSE, check.names = FALSE)
    } else {
      return(list(ok = FALSE, error = paste0("Unsupported file extension: .", ext)))
    }
    list(ok = TRUE, data = as.data.frame(df, stringsAsFactors = FALSE))
  }, error = function(e) {
    list(ok = FALSE, error = paste("Could not read file:", conditionMessage(e)))
  })
}

# --- Schema Validator 1: Extract File ---
validate_extract_schema <- function(file_input) {
  res <- read_tabular_safe(file_input)
  if (!res$ok) return(res)
  df <- res$data
  
  if (nrow(df) == 0) {
    return(list(ok = FALSE, error = "The Extract file contains 0 rows."))
  }
  req_cols <- c("Accepted Date (Unit)", "Validation Date", "TestName", "Specimen Type")
  missing_cols <- setdiff(req_cols, names(df))
  if (length(missing_cols) > 0) {
    return(list(
      ok = FALSE,
      error = paste0(
        "Missing required column(s) in Extract File:\n• ",
        paste(missing_cols, collapse = "\n• "),
        "\n\nFound columns: ", paste(names(df), collapse = ", ")
      )
    ))
  }
  
  sample_ts <- na.omit(df$`Accepted Date (Unit)`)
  if (length(sample_ts) == 0 || all(is.na(parse_extract_datetime(head(sample_ts, 20))))) {
    return(list(
      ok = FALSE,
      error = "Column 'Accepted Date (Unit)' does not contain valid timestamps (e.g. dd/mm/yyyy hh:mm:ss or dd/mm/yyyy hh:mm:ss AM/PM)."
    ))
  }
  
  dt_unit_parsed  <- parse_extract_datetime(df$`Accepted Date (Unit)`)
  dt_valid_parsed <- parse_extract_datetime(df$`Validation Date`)
  
  df <- df %>%
    mutate(
      `Accepted Date (Unit)` = format_extract_datetime(dt_unit_parsed, `Accepted Date (Unit)`),
      `Validation Date`      = format_extract_datetime(dt_valid_parsed, `Validation Date`),
      t_clean                = str_trim(as.character(TestName)),
      s_clean                = str_trim(as.character(`Specimen Type`)),
      Extract_Key            = paste0(t_clean, " | ", s_clean)
    )
  list(ok = TRUE, data = df)
}

# --- Schema Validator 2: Unified Specification Map ---
validate_spec_map_schema <- function(file_input) {
  res <- read_tabular_safe(file_input)
  if (!res$ok) return(res)
  raw_df <- res$data
  
  if (nrow(raw_df) == 0 || ncol(raw_df) < 2) {
    return(list(
      ok = FALSE,
      error = "Specification Map must contain at least 1 data row and columns for LTAT_Test_Name, LTAT_Display (or LTAT), Rule_Type, and optional Extract_Key."
    ))
  }
  
  is_structured <- all(c("LTAT_Test_Name", "LTAT_Display", "Rule_Type") %in% names(raw_df))
  
  if (is_structured) {
    valid_rules <- c("Duration", "Same Day", "Next Day", "Next Day Cutoff")
    bad_rules <- setdiff(na.omit(str_trim(as.character(raw_df$Rule_Type))), valid_rules)
    if (length(bad_rules) > 0) {
      return(list(
        ok = FALSE,
        error = paste0(
          "Invalid Rule_Type value(s) in Specification Map: ", paste(bad_rules, collapse = ", "),
          "\nAllowed values: Duration, Same Day, Next Day, Next Day Cutoff"
        )
      ))
    }
    
    for (col in c("Dur_DD", "Dur_HH", "Dur_MM")) if (!(col %in% names(raw_df))) raw_df[[col]] <- 0L
    if (!("Skip_Days" %in% names(raw_df)))     raw_df$Skip_Days <- ""
    if (!("Skip_Holidays" %in% names(raw_df))) raw_df$Skip_Holidays <- FALSE
    if (!("Cutoff_HHMM" %in% names(raw_df)))   raw_df$Cutoff_HHMM <- ""
    if (!("Target_HHMM" %in% names(raw_df)))   raw_df$Target_HHMM <- ""
    if (!("Extract_Key" %in% names(raw_df)))   raw_df$Extract_Key <- ""
    
    full_df <- raw_df %>%
      transmute(
        LTAT_Test_Name = str_trim(as.character(LTAT_Test_Name)),
        LTAT_Display   = str_trim(as.character(LTAT_Display)),
        Rule_Type      = str_trim(as.character(Rule_Type)),
        Dur_DD         = suppressWarnings(as.integer(ifelse(is.na(Dur_DD), 0L, Dur_DD))),
        Dur_HH         = suppressWarnings(as.integer(ifelse(is.na(Dur_HH), 0L, Dur_HH))),
        Dur_MM         = suppressWarnings(as.integer(ifelse(is.na(Dur_MM), 0L, Dur_MM))),
        Skip_Days      = ifelse(is.na(Skip_Days), "", str_trim(as.character(Skip_Days))),
        Skip_Holidays  = as.logical(ifelse(is.na(Skip_Holidays), FALSE, Skip_Holidays)),
        Cutoff_HHMM    = ifelse(is.na(Cutoff_HHMM), "", str_trim(as.character(Cutoff_HHMM))),
        Target_HHMM    = ifelse(is.na(Target_HHMM), "", str_trim(as.character(Target_HHMM))),
        Extract_Key    = ifelse(is.na(Extract_Key), "", str_trim(as.character(Extract_Key)))
      )
  } else {
    if (ncol(raw_df) > 3) {
      return(list(
        ok = FALSE,
        error = paste0(
          "Unrecognized Specification Map schema.\n",
          "Expected columns: LTAT_Test_Name, LTAT_Display, Rule_Type, Dur_DD, Dur_HH, Dur_MM, Skip_Days, Skip_Holidays, Cutoff_HHMM, Target_HHMM, Extract_Key."
        )
      ))
    }
    name_col <- if ("LTAT_Test_Name" %in% names(raw_df)) raw_df$LTAT_Test_Name else raw_df[[1]]
    ltat_col <- if ("LTAT" %in% names(raw_df)) raw_df$LTAT else raw_df[[2]]
    ext_col  <- if ("Extract_Key" %in% names(raw_df)) raw_df$Extract_Key else if (ncol(raw_df) == 3) raw_df[[3]] else rep("", nrow(raw_df))
    
    parsed_list <- lapply(ltat_col, infer_rule_from_legacy_text)
    full_df <- tibble::tibble(
      LTAT_Test_Name = str_trim(as.character(name_col)),
      LTAT_Display   = str_trim(as.character(ltat_col)),
      Rule_Type      = sapply(parsed_list, `[[`, "Rule_Type"),
      Dur_DD         = as.integer(sapply(parsed_list, `[[`, "Dur_DD")),
      Dur_HH         = as.integer(sapply(parsed_list, `[[`, "Dur_HH")),
      Dur_MM         = as.integer(sapply(parsed_list, `[[`, "Dur_MM")),
      Skip_Days      = sapply(parsed_list, `[[`, "Skip_Days"),
      Skip_Holidays  = as.logical(sapply(parsed_list, `[[`, "Skip_Holidays")),
      Cutoff_HHMM    = sapply(parsed_list, `[[`, "Cutoff_HHMM"),
      Target_HHMM    = sapply(parsed_list, `[[`, "Target_HHMM"),
      Extract_Key    = ifelse(is.na(ext_col), "", str_trim(as.character(ext_col)))
    )
  }
  
  full_df <- full_df %>%
    filter(!is.na(LTAT_Test_Name), LTAT_Test_Name != "")
  
  if (nrow(full_df) == 0) {
    return(list(ok = FALSE, error = "Specification Map contains no valid test names in LTAT_Test_Name."))
  }
  
  non_empty_keys <- full_df$Extract_Key[full_df$Extract_Key != ""]
  bad_keys <- non_empty_keys[!str_detect(non_empty_keys, "\\|")]
  if (length(bad_keys) > 0) {
    return(list(
      ok = FALSE,
      error = paste0(
        "Invalid Extract_Key format (must be 'TestName | Specimen Type'):\n• ",
        paste(head(bad_keys, 10), collapse = "\n• ")
      )
    ))
  }
  
  cat_df <- full_df %>%
    select(-Extract_Key) %>%
    distinct(LTAT_Test_Name, .keep_all = TRUE)
  
  map_df <- full_df %>%
    filter(Extract_Key != "") %>%
    select(Extract_Key, LTAT_Test_Name) %>%
    distinct(Extract_Key, .keep_all = TRUE)
  
  list(ok = TRUE, catalog = cat_df, mapping = map_df)
}

# --- Excel-style Header Filter Helper ---
excel_header <- function(table_id, col_name, display_name = col_name) {
  function(value) {
    div(
      style = "display: flex; align-items: center; justify-content: space-between; position: relative;",
      span(display_name),
      tags$button(
        type = "button", class = "excel-filter-btn",
        `data-table` = table_id, `data-col` = col_name,
        title = paste("Filter", display_name),
        onclick = sprintf("openExcelFilter(event, '%s', '%s')", table_id, col_name),
        HTML("▾")
      )
    )
  }
}

# ==============================================================================
# CENTRALIZED TABLE CLASS (Bitmask Engine + Cascading + Date Hierarchy)
# ==============================================================================
AppTable <- R6Class(
  "AppTable",
  public = list(
    id                = NULL,
    mode              = NULL,
    page_size         = NULL,
    page_size_options = NULL,
    selected_idx      = 1L,
    # Bitmask filter state (report mode)
    row_state         = integer(0),
    col_bits          = integer(0),
    date_cols         = character(0),
    unfiltered_vals   = list(),
    last_flt          = list(),
    ignore_cols       = character(0),
    n_rows            = 0L,

    initialize = function(id, mode = c("report", "selector"), page_size = NULL,
                          page_size_options = NULL) {
      self$id   <- id
      self$mode <- match.arg(mode)
      self$page_size <- if (!is.null(page_size)) page_size else if (self$mode == "report") 15L else 12L
      self$page_size_options <- if (!is.null(page_size_options)) {
        page_size_options
      } else if (self$mode == "report") {
        c(15, 25, 50, 100)
      } else {
        c(12, 25, 50, 100)
      }
    },

    col_filter_vec = function(df, col) {
      if (col %in% self$date_cols) {
        extract_date_key(df[[col]])
      } else {
        norm_filter_vec(df[[col]])
      }
    },

    set_data = function(df, ignore_cols = character(0)) {
      if (self$mode != "report" || is.null(df)) return(invisible(NULL))
      self$ignore_cols <- ignore_cols
      target_cols      <- setdiff(names(df), ignore_cols)
      self$n_rows      <- nrow(df)
      self$row_state   <- integer(self$n_rows) # All 0L = visible
      self$last_flt    <- list()

      if (length(target_cols) > 0) {
        # Auto-detect YYYY-MM-DD date/timestamp columns
        is_dt <- vapply(target_cols, function(cn) {
          v <- norm_filter_vec(df[[cn]])
          non_empty <- v[v != ""]
          if (length(non_empty) == 0) return(FALSE)
          samp <- head(non_empty, 50L)
          mean(grepl("^\\d{4}-\\d{2}-\\d{2}", samp)) >= 0.8
        }, logical(1))
        self$date_cols <- target_cols[is_dt]

        self$col_bits <- setNames(
          bitwShiftL(1L, seq_along(target_cols) - 1L),
          target_cols
        )
        self$unfiltered_vals <- setNames(
          lapply(target_cols, function(cn) {
            I(sort_filter_values(self$col_filter_vec(df, cn)))
          }),
          target_cols
        )
      } else {
        self$date_cols       <- character(0)
        self$col_bits        <- integer(0)
        self$unfiltered_vals <- list()
      }
      invisible(NULL)
    },

    sync_filter_values = function(session, df, ignore_cols = character(0)) {
      if (self$mode != "report" || is.null(df)) return(invisible(NULL))
      self$set_data(df, ignore_cols = ignore_cols)
      session$sendCustomMessage("updateColValues", list(
        table    = self$id,
        values   = self$unfiltered_vals,
        dateCols = I(self$date_cols)
      ))
    },

    update_filters = function(df, flt_list) {
      if (self$mode != "report" || is.null(df)) return(invisible(NULL))
      if (nrow(df) != self$n_rows || !all(setdiff(names(df), self$ignore_cols) %in% names(self$col_bits))) {
        self$set_data(df, ignore_cols = self$ignore_cols)
      }

      if (is.null(flt_list) || length(flt_list) == 0) {
        if (length(self$last_flt) > 0) {
          self$row_state <- integer(self$n_rows)
          self$last_flt  <- list()
        }
        return(invisible(NULL))
      }

      new_cols     <- names(flt_list)
      old_cols     <- names(self$last_flt)
      removed_cols <- setdiff(old_cols, new_cols)

      # 1. Clear bits for removed column filters
      for (col in removed_cols) {
        if (col %in% names(self$col_bits)) {
          self$row_state <- bitwAnd(self$row_state, bitwNot(self$col_bits[[col]]))
        }
      }

      # 2. Update bits only for added or modified column filters
      for (col in new_cols) {
        if (!(col %in% names(self$col_bits)) || !(col %in% names(df))) next
        chosen <- as.character(flt_list[[col]])
        if (!identical(chosen, self$last_flt[[col]])) {
          b <- self$col_bits[[col]]
          # Step A: wipe this column's bit everywhere
          self$row_state <- bitwAnd(self$row_state, bitwNot(b))
          # Step B: set this column's bit on hidden rows
          hidden <- !(self$col_filter_vec(df, col) %in% chosen)
          if (any(hidden)) {
            self$row_state[hidden] <- bitwOr(self$row_state[hidden], b)
          }
        }
      }

      self$last_flt <- lapply(flt_list, as.character)
      invisible(NULL)
    },

    get_col_values = function(df, col, flt_list = NULL) {
      if (self$mode != "report" || is.null(df) || !(col %in% names(df))) {
        return(I(character(0)))
      }
      self$update_filters(df, flt_list)
      if (!(col %in% names(self$col_bits))) {
        return(I(character(0)))
      }
      # "Exclude Self": temporarily ignore this column's own bit
      b <- self$col_bits[[col]]
      keep <- bitwAnd(self$row_state, bitwNot(b)) == 0L
      if (!any(keep)) return(I(character(0)))
      I(sort_filter_values(self$col_filter_vec(df, col)[keep]))
    },

    apply_state = function(df, active_filters, sort_state) {
      if (self$mode != "report") return(df)
      flt <- if (!is.null(active_filters)) active_filters$filters[[self$id]] else NULL
      self$update_filters(df, flt)
      df <- df[self$row_state == 0L, , drop = FALSE]

      if (!is.null(sort_state) && identical(sort_state$table, self$id) && sort_state$col %in% names(df)) {
        col_vec <- df[[sort_state$col]]
        num_key <- if (is.numeric(col_vec)) col_vec else suppressWarnings(as.numeric(sub("%$", "", as.character(col_vec))))
        use_num <- !all(is.na(num_key)) && sum(!is.na(num_key)) == sum(!is.na(col_vec) & as.character(col_vec) != "" & as.character(col_vec) != "N/A")
        
        ord <- if (use_num) {
          order(num_key, as.character(col_vec), decreasing = (sort_state$dir == "desc"), na.last = TRUE)
        } else {
          order(as.character(col_vec), decreasing = (sort_state$dir == "desc"), na.last = TRUE)
        }
        df <- df[ord, , drop = FALSE]
      }
      df
    },

    render = function(df, selected = NULL, extra_cols = list(), override_cols = list(),
                      output_id = NULL, extra_js = "") {
      if (self$mode == "report") {
        cur_page  <- if (!is.null(output_id)) isolate(getReactableState(output_id, "page")) else NULL
        cur_psize <- if (!is.null(output_id)) isolate(getReactableState(output_id, "pageSize")) else NULL

        psize <- if (!is.null(cur_psize) && length(cur_psize) > 0 && !is.na(cur_psize[1])) {
          as.integer(cur_psize[1])
        } else {
          self$page_size
        }

        max_page <- max(1L, ceiling(nrow(df) / psize))
        start_page <- if (!is.null(cur_page) && length(cur_page) > 0 && !is.na(cur_page[1])) {
          min(max(1L, as.integer(cur_page[1])), max_page)
        } else {
          1L
        }

        std_names <- setdiff(names(df), c(names(extra_cols), names(override_cols)))
        cols <- setNames(
          lapply(std_names, function(cn) colDef(header = excel_header(self$id, cn))),
          std_names
        )
        cols <- c(cols, override_cols, extra_cols)

        w <- reactable(
          df,
          columns               = cols,
          defaultPageSize       = psize,
          showPageSizeOptions   = TRUE,
          pageSizeOptions       = self$page_size_options,
          bordered              = TRUE,
          striped               = TRUE,
          highlight             = TRUE,
          resizable             = TRUE
        )

        if (!is.null(output_id)) {
          restore_js <- sprintf(
            "function(el, x) {
              window.savedTablePages = window.savedTablePages || {};
              var maxPages = %d;
              var savedZeroIdx = (typeof window.savedTablePages['%s'] === 'number')
                ? window.savedTablePages['%s']
                : %d;
              if (savedZeroIdx >= maxPages) savedZeroIdx = Math.max(0, maxPages - 1);

              if (window.syncTableFilterUI) {
                syncTableFilterUI('%s');
              }

              var isRestoring = (savedZeroIdx > 0);
              var stateHooked = false;

              function hookState() {
                if (stateHooked) return true;
                if (window.Reactable && Reactable.onStateChange) {
                  try {
                    Reactable.onStateChange('%s', function(state) {
                      if (state && typeof state.page === 'number') {
                        if (isRestoring && state.page !== savedZeroIdx) return;
                        isRestoring = false;
                        window.savedTablePages['%s'] = state.page;
                      }
                    });
                    stateHooked = true;
                    return true;
                  } catch (e) {}
                }
                return false;
              }

              function attemptRestore(tries) {
                hookState();

                if (savedZeroIdx <= 0) {
                  %s
                  return;
                }

                var pageBtns = el.querySelectorAll('.rt-page-button');
                if (pageBtns.length === 0 && tries < 50) {
                  setTimeout(function() { attemptRestore(tries + 1); }, 40);
                  return;
                }

                if (window.Reactable && Reactable.gotoPage) {
                  try {
                    Reactable.gotoPage('%s', savedZeroIdx);
                  } catch (e) {}
                }

                setTimeout(function() {
                  var targetLabel = String(savedZeroIdx + 1);
                  var activeBtn = el.querySelector('.rt-page-button-active');
                  if (!activeBtn || activeBtn.innerText.trim() !== targetLabel) {
                    var btns = Array.from(el.querySelectorAll('.rt-page-button'));
                    var match = btns.find(function(b) { return b.innerText.trim() === targetLabel; });
                    if (match) {
                      match.click();
                    } else if (tries < 50) {
                      attemptRestore(tries + 1);
                      return;
                    }
                  }
                  isRestoring = false;
                  %s
                }, 40);
              }

              attemptRestore(0);
            }",
            max_page, output_id, output_id, start_page - 1L,
            self$id,
            output_id, output_id,
            extra_js,
            output_id,
            extra_js
          )
          w <- htmlwidgets::onRender(w, restore_js)
        }
        w
      } else {
        sel <- if (!is.null(selected) && nrow(df) > 0) min(max(1L, selected), nrow(df)) else NULL
        reactable(
          df,
          selection             = "single",
          onClick               = "select",
          defaultSelected       = sel,
          searchable            = TRUE,
          defaultPageSize       = self$page_size,
          showPageSizeOptions   = TRUE,
          pageSizeOptions       = self$page_size_options,
          bordered              = TRUE,
          highlight             = TRUE,
          compact               = TRUE,
          theme                 = reactableTheme(
            rowSelectedStyle = list(backgroundColor = "#cfe2ff", fontWeight = "bold")
          )
        )
      }
    }
  )
)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .excel-filter-btn {
        border: 1px solid #ccc; background: #f8f9fa; color: #333;
        border-radius: 3px; padding: 1px 5px; margin-left: 6px;
        cursor: pointer; font-size: 11px; line-height: 1.2;
        display: inline-flex; align-items: center; justify-content: center;
        transition: background-color 0.12s, border-color 0.12s;
      }
      .excel-filter-btn:hover {
        background: #e9ecef; border-color: #adb5bd;
      }
      .excel-filter-btn.excel-filter-active {
        background: #e7f1ff; border-color: #0d6efd; color: #0d6efd;
      }
      .excel-filter-btn.excel-filter-active:hover {
        background: #cfe2ff;
      }
      .table-filter-bar {
        display: flex; justify-content: flex-end; align-items: center;
        margin-bottom: 8px;
      }
      .clear-all-filters-btn {
        background: #fff5f5; color: #dc3545; border: 1px solid #f5c2c7;
        border-radius: 4px; padding: 4px 10px; font-size: 12.5px; font-weight: 600;
        cursor: pointer; display: inline-flex; align-items: center; gap: 5px;
        transition: background-color 0.12s, border-color 0.12s;
      }
      .clear-all-filters-btn:hover {
        background: #dc3545; color: #fff; border-color: #dc3545;
      }
      .clear-all-filters-btn .filter-count-badge {
        background: #dc3545; color: #fff; border-radius: 10px;
        padding: 0 6px; font-size: 11px; line-height: 1.5;
      }
      .clear-all-filters-btn:hover .filter-count-badge {
        background: #fff; color: #dc3545;
      }
      .excel-popup {
        display: none; position: fixed; z-index: 9999; background: #fff;
        border: 1px solid #b5b5b5; box-shadow: 0 4px 12px rgba(0,0,0,0.18);
        width: 250px; padding: 8px; font-size: 13px; border-radius: 4px;
      }
      .excel-popup-item { padding: 5px 8px; cursor: pointer; display: flex; align-items: center; gap: 8px; }
      .excel-popup-item:hover { background: #e8f0fe; }
      .excel-select-all-bar {
        border: 1px solid #dcdcdc; border-bottom: none; background: #f8f9fa;
        padding: 4px 6px; margin-top: 6px; font-size: 13px;
      }
      .excel-select-all-bar label {
        display: flex; align-items: center; gap: 6px; margin: 0; cursor: pointer;
      }
      .excel-checkbox-list {
        height: 180px; overflow-y: auto; border: 1px solid #dcdcdc;
        margin-bottom: 6px; background: #fff; position: relative;
      }
      .excel-val-item {
        display: flex; align-items: center; gap: 6px;
        height: 22px; line-height: 22px; padding: 0 6px; margin: 0;
        font-weight: normal; cursor: pointer; box-sizing: border-box;
        white-space: nowrap; overflow: hidden; text-overflow: ellipsis;
      }
      .excel-val-item:hover { background: #f1f5f9; }
      .excel-val-item input[type=\"checkbox\"] { margin: 0; flex-shrink: 0; cursor: pointer; }
      .excel-val-text { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
      /* Date Tree Hierarchy Styles */
      .excel-tree-row {
        display: flex; align-items: center; gap: 5px;
        height: 22px; line-height: 22px; padding-right: 6px;
        font-size: 13px; user-select: none; box-sizing: border-box;
      }
      .excel-tree-row:hover { background: #f1f5f9; }
      .excel-tree-toggle {
        display: inline-flex; align-items: center; justify-content: center;
        width: 14px; height: 14px; cursor: pointer; color: #555;
        font-size: 10px; flex-shrink: 0;
      }
      .excel-tree-toggle:hover { color: #0d6efd; }
      .excel-tree-spacer { display: inline-block; width: 14px; flex-shrink: 0; }
      .excel-tree-label {
        display: flex; align-items: center; gap: 5px; margin: 0;
        font-weight: normal; cursor: pointer; flex: 1;
        white-space: nowrap; overflow: hidden; text-overflow: ellipsis;
      }
      .excel-tree-label input[type=\"checkbox\"] { margin: 0; flex-shrink: 0; cursor: pointer; }
      .transfer-box select { height: 420px !important; font-size: 12px; }
      .tab-toolbar {
        background: #f8f9fa; border: 1px solid #e3e6ea; border-radius: 5px;
        padding: 10px 15px; margin-bottom: 14px;
      }
      .tab-toolbar .form-group { margin-bottom: 0; }
      .inline-days .shiny-options-group { display: flex; flex-wrap: wrap; gap: 10px; }
      .inline-days .checkbox { margin-top: 0; margin-bottom: 0; }
      .checklist-item { display: inline-flex; align-items: center; gap: 6px; margin-right: 18px; font-size: 14px; }
      .obsolete-banner {
        background: #fff3cd; color: #664d03; border: 1px solid #ffecb5;
        border-left: 5px solid #ffc107; border-radius: 4px;
        padding: 10px 14px; margin-bottom: 14px; font-size: 14px;
        display: flex; align-items: center; justify-content: space-between; gap: 12px;
      }
      .exc-badge-norm {
        display: inline-flex; align-items: center; justify-content: center;
        width: 20px; height: 20px; border-radius: 50%;
        font-size: 12px; font-weight: 700; line-height: 1; margin: 0 2px;
      }
      .exc-badge-sup {
        display: inline-flex; align-items: center; justify-content: center;
        width: 15px; height: 15px; border-radius: 50%;
        font-size: 10px; font-weight: 700; line-height: 1; margin-left: 2px;
      }
      .exc-header-btn {
        background: transparent; border: 1px solid transparent; border-radius: 4px;
        padding: 2px 5px; cursor: pointer; transition: background 0.15s, border-color 0.15s;
      }
      .exc-header-btn:hover {
        background: #e7f1ff; border-color: #9ec5fe;
      }
      .exc-modal-section {
        background: #f8f9fa; border: 1px solid #dee2e6; border-radius: 6px;
        padding: 12px 14px; margin-bottom: 14px;
      }
      .exc-section-title {
        font-size: 12px; font-weight: 700; text-transform: uppercase;
        letter-spacing: 0.04em; color: #495057; margin-bottom: 10px;
      }
      .exc-option-row {
        display: flex; align-items: center; justify-content: space-between; flex-wrap: wrap; gap: 8px;
        padding: 8px 12px; margin-bottom: 8px; border: 1px solid #dee2e6;
        border-radius: 6px; cursor: pointer; background: #fff; font-weight: normal;
        transition: box-shadow 0.12s, background-color 0.12s;
      }
      .exc-option-row:last-child { margin-bottom: 0; }
      .exc-option-row:hover {
        box-shadow: 0 2px 6px rgba(0,0,0,0.09);
        background: #fcfcfd;
      }
      .exc-option-row input[type='checkbox'] {
        width: 16px; height: 16px; margin-right: 10px; cursor: pointer; flex-shrink: 0;
      }
      .exc-nd-controls {
        display: flex; align-items: center; gap: 12px; margin-top: 6px; margin-left: 60px;
        font-size: 12.5px; color: #333; flex-wrap: wrap;
      }
      .exc-nd-radio-lbl {
        display: inline-flex; align-items: center; gap: 5px; margin: 0;
        cursor: pointer; font-weight: normal;
      }
      .exc-nd-radio-lbl input[type='radio'] { margin: 0; cursor: pointer; }
      .exc-scope-box .checkbox { margin-top: 4px; margin-bottom: 6px; }
      .exc-scope-box label { font-size: 13px; color: #212529; }
      .exc-num-circle {
        display: inline-flex; align-items: center; justify-content: center;
        width: 24px; height: 24px; border-radius: 50%; font-weight: 700;
        font-size: 13px; line-height: 1; margin-right: 10px; flex-shrink: 0;
      }
      .selectize-dropdown { z-index: 10050 !important; }
      .exc-selected-header {
        display: flex; justify-content: space-between; align-items: center;
        margin-top: 8px; margin-bottom: 4px; font-size: 12px; color: #495057;
      }
      .exc-header-actions {
        display: inline-flex; align-items: center; gap: 6px;
      }
      .exc-select-all-link {
        color: #0d6efd; cursor: pointer; font-weight: 600; text-decoration: none;
      }
      .exc-select-all-link:hover { text-decoration: underline; }
      .exc-action-sep {
        color: #adb5bd; user-select: none;
      }
      .exc-clear-link {
        color: #dc3545; cursor: pointer; font-weight: 600; text-decoration: none;
      }
      .exc-clear-link:hover { text-decoration: underline; }
      .exc-selected-list {
        max-height: 165px; overflow-y: auto; border: 1px solid #dee2e6;
        border-radius: 4px; background: #fff;
      }
      .exc-selected-row {
        display: flex; justify-content: space-between; align-items: center; gap: 10px;
        padding: 6px 10px; border-bottom: 1px solid #f1f3f5; font-size: 12.5px; color: #212529;
      }
      .exc-selected-row:last-child { border-bottom: none; }
      .exc-selected-row:hover { background: #f8f9fa; }
      .exc-remove-btn {
        border: none; background: transparent; color: #6c757d; font-size: 16px;
        font-weight: bold; line-height: 1; padding: 0 4px; cursor: pointer; flex-shrink: 0;
      }
      .exc-remove-btn:hover { color: #dc3545; }
    ")),
    tags$script(HTML(r"---(
      window.excelFilters = {};
      window.excelColValues = {};
      window.excelDateCols = {};

      var activeTable = null, activeCol = null;
      var excelAllVals = [];
      var excelFilteredVals = [];
      var excelSelectedSet = new Set();
      var EXCEL_ROW_H = 22;
      var EXCEL_OVERSCAN = 8;
      var excelLastRange = { start: -1, end: -1 };
      var excelReqSeq = 0;
      var pendingFilterReq = null;
      var isExcelLoading = false;

      // Phase 3: Date Tree State
      var isExcelDateMode = false;
      var excelDateTree = null;
      var excelExpandedYears = new Set();
      var excelExpandedMonths = new Set();
      var MONTH_NAMES = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];

      var FILTER_FUNNEL_SVG =
        '<svg width="11" height="11" viewBox="0 0 16 16" fill="currentColor" style="display:block;">' +
        '<path d="M1.5 1.5A.5.5 0 0 1 2 1h12a.5.5 0 0 1 .5.5v2a.5.5 0 0 1-.128.334L10 8.692V13.5a.5.5 0 0 1-.342.474l-3 1A.5.5 0 0 1 6 14.5V8.692L1.628 3.834A.5.5 0 0 1 1.5 3.5v-2z"/>' +
        '</svg>';

      function syncTableFilterUI(tableId) {
        if (!tableId) return;
        var tblFilters = window.excelFilters[tableId] || {};
        var activeCols = Object.keys(tblFilters);
        var activeCount = activeCols.length;

        document.querySelectorAll('.excel-filter-btn[data-table="' + tableId + '"]').forEach(function(btn) {
          var col = btn.getAttribute('data-col');
          var isActive = Object.prototype.hasOwnProperty.call(tblFilters, col);
          if (isActive) {
            btn.classList.add('excel-filter-active');
            btn.innerHTML = FILTER_FUNNEL_SVG;
            btn.title = 'Filter active on ' + col + ' (' + tblFilters[col].length + ' selected)';
          } else {
            btn.classList.remove('excel-filter-active');
            btn.innerHTML = '&#9662;';
            btn.title = 'Filter ' + col;
          }
        });

        var bar = document.getElementById('clear_filters_bar_' + tableId);
        if (bar) {
          if (activeCount > 0) {
            bar.style.display = 'flex';
            var badge = bar.querySelector('.filter-count-badge');
            if (badge) badge.textContent = String(activeCount);
          } else {
            bar.style.display = 'none';
          }
        }
      }

      function clearAllTableFilters(tableId) {
        if (window.excelFilters[tableId]) {
          delete window.excelFilters[tableId];
        }
        syncTableFilterUI(tableId);
        pushExcelFilters();
      }

      function escapeExcelHtml(str) {
        return String(str)
          .replace(/&/g, '&amp;')
          .replace(/</g, '&lt;')
          .replace(/>/g, '&gt;')
          .replace(/"/g, '&quot;');
      }

      function normalizeArray(arr) {
        if (Array.isArray(arr)) return arr;
        if (arr === null || arr === undefined) return [];
        return [arr];
      }

      function closeExcelPopup() {
        var popup = document.getElementById('excelFilterPopup');
        if (popup) popup.style.display = 'none';
        pendingFilterReq = null;
        isExcelLoading = false;
      }

      function positionExcelPopup(popup, rect) {
        var popupH = popup.offsetHeight;
        var popupW = popup.offsetWidth;
        var leftPos = Math.max(8, Math.min(rect.left, window.innerWidth - popupW - 12));
        var topPos  = rect.bottom + 4;

        if (topPos + popupH > window.innerHeight - 8) {
          var abovePos = rect.top - popupH - 4;
          topPos = (abovePos >= 8) ? abovePos : Math.max(8, window.innerHeight - popupH - 8);
        }

        popup.style.left = leftPos + 'px';
        popup.style.top  = topPos + 'px';
        popup.style.visibility = 'visible';
      }

      // --- Phase 3: Build YYYY -> MMM -> DD Hierarchy ---
      function buildDateTree(vals) {
        var yearsMap = {};
        var others = [];
        var hasBlanks = false;

        for (var i = 0; i < vals.length; i++) {
          var v = vals[i];
          if (v === '' || v === null) {
            hasBlanks = true;
            continue;
          }
          var m = /^(\d{4})-(\d{2})-(\d{2})$/.exec(String(v));
          if (!m) {
            others.push(v);
            continue;
          }
          var y = m[1];
          var mo = m[2];
          var d = m[3];
          if (!yearsMap[y]) yearsMap[y] = { year: y, months: {}, allVals: [] };
          yearsMap[y].allVals.push(v);

          var moKey = y + '-' + mo;
          if (!yearsMap[y].months[mo]) {
            var moIdx = parseInt(mo, 10) - 1;
            var moLabel = (moIdx >= 0 && moIdx < 12) ? MONTH_NAMES[moIdx] : mo;
            yearsMap[y].months[mo] = { key: moKey, num: mo, label: moLabel, days: [] };
          }
          yearsMap[y].months[mo].days.push({ day: d, val: v });
        }

        var yearsArr = Object.keys(yearsMap).sort().map(function(y) {
          var yObj = yearsMap[y];
          var monthsArr = Object.keys(yObj.months).sort().map(function(mo) {
            var moObj = yObj.months[mo];
            moObj.days.sort(function(a, b) { return a.day.localeCompare(b.day); });
            return moObj;
          });
          return { year: y, months: monthsArr, allVals: yObj.allVals };
        });

        return { years: yearsArr, others: others, hasBlanks: hasBlanks };
      }

      function renderExcelDateTree() {
        var spacer = document.getElementById('excelVirtualSpacer');
        var viewport = document.getElementById('excelVirtualViewport');
        spacer.style.height = 'auto';
        viewport.style.position = 'static';
        viewport.style.transform = 'none';

        if (excelFilteredVals.length === 0) {
          viewport.innerHTML = '<div style="padding: 8px; color: #888; font-style: italic; font-size: 12px;">No matching values</div>';
          return;
        }

        excelDateTree = buildDateTree(excelFilteredVals);
        var parts = [];
        var indetIds = [];

        for (var yi = 0; yi < excelDateTree.years.length; yi++) {
          var yObj = excelDateTree.years[yi];
          var yExp = excelExpandedYears.has(yObj.year);
          var yTotal = yObj.allVals.length;
          var ySel = 0;
          for (var k = 0; k < yTotal; k++) {
            if (excelSelectedSet.has(yObj.allVals[k])) ySel++;
          }
          var yChk = (ySel === yTotal && yTotal > 0) ? 'checked' : '';
          var yCbId = 'exc_yr_' + yObj.year;
          if (ySel > 0 && ySel < yTotal) indetIds.push(yCbId);

          parts.push(
            '<div class="excel-tree-row" style="padding-left: 4px;">' +
              '<span class="excel-tree-toggle" onclick="toggleDateYearExpand(\'' + yObj.year + '\')">' + (yExp ? '&#9662;' : '&#9656;') + '</span>' +
              '<label class="excel-tree-label">' +
                '<input type="checkbox" id="' + yCbId + '" ' + yChk + ' onchange="toggleDateYearCheck(\'' + yObj.year + '\', this.checked)">' +
                '<span><b>' + escapeExcelHtml(yObj.year) + '</b></span>' +
              '</label>' +
            '</div>'
          );

          if (yExp) {
            for (var mi = 0; mi < yObj.months.length; mi++) {
              var mObj = yObj.months[mi];
              var mExp = excelExpandedMonths.has(mObj.key);
              var mTotal = mObj.days.length;
              var mSel = 0;
              for (var dk = 0; dk < mTotal; dk++) {
                if (excelSelectedSet.has(mObj.days[dk].val)) mSel++;
              }
              var mChk = (mSel === mTotal && mTotal > 0) ? 'checked' : '';
              var mCbId = 'exc_mo_' + mObj.key;
              if (mSel > 0 && mSel < mTotal) indetIds.push(mCbId);

              parts.push(
                '<div class="excel-tree-row" style="padding-left: 20px;">' +
                  '<span class="excel-tree-toggle" onclick="toggleDateMonthExpand(\'' + mObj.key + '\')">' + (mExp ? '&#9662;' : '&#9656;') + '</span>' +
                  '<label class="excel-tree-label">' +
                    '<input type="checkbox" id="' + mCbId + '" ' + mChk + ' onchange="toggleDateMonthCheck(\'' + yObj.year + '\', \'' + mObj.num + '\', this.checked)">' +
                    '<span>' + escapeExcelHtml(mObj.label) + '</span>' +
                  '</label>' +
                '</div>'
              );

              if (mExp) {
                for (var di = 0; di < mTotal; di++) {
                  var dObj = mObj.days[di];
                  var dChk = excelSelectedSet.has(dObj.val) ? 'checked' : '';
                  parts.push(
                    '<div class="excel-tree-row" style="padding-left: 36px;">' +
                      '<span class="excel-tree-spacer"></span>' +
                      '<label class="excel-tree-label" title="' + escapeExcelHtml(dObj.val) + '">' +
                        '<input type="checkbox" ' + dChk + ' onchange="toggleDateDayCheck(\'' + escapeExcelHtml(dObj.val) + '\', this.checked)">' +
                        '<span>' + escapeExcelHtml(dObj.day) + '</span>' +
                      '</label>' +
                    '</div>'
                  );
                }
              }
            }
          }
        }

        // Any non-ISO values
        for (var oi = 0; oi < excelDateTree.others.length; oi++) {
          var ov = excelDateTree.others[oi];
          var oChk = excelSelectedSet.has(ov) ? 'checked' : '';
          parts.push(
            '<div class="excel-tree-row" style="padding-left: 4px;">' +
              '<span class="excel-tree-spacer"></span>' +
              '<label class="excel-tree-label" title="' + escapeExcelHtml(ov) + '">' +
                '<input type="checkbox" ' + oChk + ' onchange="toggleDateDayCheck(\'' + escapeExcelHtml(ov) + '\', this.checked)">' +
                '<span>' + escapeExcelHtml(ov) + '</span>' +
              '</label>' +
            '</div>'
          );
        }

        // Blanks
        if (excelDateTree.hasBlanks) {
          var bChk = excelSelectedSet.has('') ? 'checked' : '';
          parts.push(
            '<div class="excel-tree-row" style="padding-left: 4px;">' +
              '<span class="excel-tree-spacer"></span>' +
              '<label class="excel-tree-label">' +
                '<input type="checkbox" ' + bChk + ' onchange="toggleDateDayCheck(\'\', this.checked)">' +
                '<span>(Blanks)</span>' +
              '</label>' +
            '</div>'
          );
        }

        viewport.innerHTML = parts.join('');

        for (var ii = 0; ii < indetIds.length; ii++) {
          var el = document.getElementById(indetIds[ii]);
          if (el) el.indeterminate = true;
        }
      }

      function toggleDateYearExpand(year) {
        if (excelExpandedYears.has(year)) {
          excelExpandedYears.delete(year);
        } else {
          excelExpandedYears.add(year);
        }
        renderExcelDateTree();
      }

      function toggleDateMonthExpand(moKey) {
        if (excelExpandedMonths.has(moKey)) {
          excelExpandedMonths.delete(moKey);
        } else {
          excelExpandedMonths.add(moKey);
        }
        renderExcelDateTree();
      }

      function toggleDateYearCheck(year, checked) {
        if (!excelDateTree) return;
        for (var i = 0; i < excelDateTree.years.length; i++) {
          if (excelDateTree.years[i].year === year) {
            var vals = excelDateTree.years[i].allVals;
            for (var k = 0; k < vals.length; k++) {
              if (checked) excelSelectedSet.add(vals[k]);
              else excelSelectedSet.delete(vals[k]);
            }
            break;
          }
        }
        renderExcelDateTree();
        updateSelectAllState();
      }

      function toggleDateMonthCheck(year, moNum, checked) {
        if (!excelDateTree) return;
        for (var i = 0; i < excelDateTree.years.length; i++) {
          if (excelDateTree.years[i].year === year) {
            var months = excelDateTree.years[i].months;
            for (var j = 0; j < months.length; j++) {
              if (months[j].num === moNum) {
                var days = months[j].days;
                for (var k = 0; k < days.length; k++) {
                  if (checked) excelSelectedSet.add(days[k].val);
                  else excelSelectedSet.delete(days[k].val);
                }
                break;
              }
            }
            break;
          }
        }
        renderExcelDateTree();
        updateSelectAllState();
      }

      function toggleDateDayCheck(val, checked) {
        if (checked) excelSelectedSet.add(val);
        else excelSelectedSet.delete(val);
        renderExcelDateTree();
        updateSelectAllState();
      }

      function populateExcelPopupList(tableId, colName, vals) {
        isExcelLoading = false;
        excelAllVals = vals;
        excelFilteredVals = vals;

        var saved = window.excelFilters[tableId] && window.excelFilters[tableId][colName];
        excelSelectedSet = new Set(Array.isArray(saved) ? saved : vals);

        var okBtn = document.getElementById('excelOkBtn');
        var selAll = document.getElementById('excelSelectAll');
        var searchEl = document.getElementById('excelSearchInput');
        if (okBtn) okBtn.disabled = false;
        if (selAll) selAll.disabled = false;
        if (searchEl) searchEl.disabled = false;

        document.getElementById('excelCheckboxList').scrollTop = 0;

        if (isExcelDateMode) {
          excelExpandedYears.clear();
          excelExpandedMonths.clear();
          renderExcelDateTree();
        } else {
          resetExcelVirtualList();
        }
        updateSelectAllState();
      }

      function openExcelFilter(e, tableId, colName) {
        e.stopPropagation();
        activeTable = tableId;
        activeCol = colName;
        pendingFilterReq = null;

        var dateColsSet = window.excelDateCols[tableId];
        isExcelDateMode = !!(dateColsSet && dateColsSet.has(colName));

        var popup = document.getElementById('excelFilterPopup');
        var btn = e.currentTarget || e.target.closest('.excel-filter-btn') || e.target;
        var rect = btn.getBoundingClientRect();

        document.getElementById('excelFilterTitle').innerText = 'Filter: ' + colName;
        var searchEl = document.getElementById('excelSearchInput');
        searchEl.value = '';

        var tblFilters = window.excelFilters[tableId] || {};
        var hasOtherFilter = Object.keys(tblFilters).some(function(c) { return c !== colName; });

        popup.style.visibility = 'hidden';
        popup.style.display = 'block';

        if (!hasOtherFilter) {
          var preloaded = (window.excelColValues[tableId] && window.excelColValues[tableId][colName]) || [];
          populateExcelPopupList(tableId, colName, preloaded);
          positionExcelPopup(popup, rect);
        } else {
          isExcelLoading = true;
          excelAllVals = [];
          excelFilteredVals = [];
          excelSelectedSet = new Set();

          var okBtn = document.getElementById('excelOkBtn');
          var selAll = document.getElementById('excelSelectAll');
          if (okBtn) okBtn.disabled = true;
          if (selAll) { selAll.disabled = true; selAll.checked = false; selAll.indeterminate = false; }
          if (searchEl) searchEl.disabled = true;

          var spacer = document.getElementById('excelVirtualSpacer');
          var viewport = document.getElementById('excelVirtualViewport');
          spacer.style.height = '0px';
          viewport.style.position = 'absolute';
          viewport.style.transform = 'translateY(0px)';
          viewport.innerHTML = '<div style="padding: 10px 8px; color: #6c757d; font-style: italic; font-size: 12px;">Loading visible values...</div>';

          positionExcelPopup(popup, rect);

          var reqId = ++excelReqSeq;
          pendingFilterReq = { table: tableId, col: colName, reqId: reqId };
          Shiny.setInputValue('excel_request_col_values', {
            table: tableId,
            col: colName,
            reqId: reqId,
            filters: tblFilters
          }, { priority: 'event' });
        }
      }

      function resetExcelVirtualList() {
        var spacer = document.getElementById('excelVirtualSpacer');
        var viewport = document.getElementById('excelVirtualViewport');
        viewport.style.position = 'absolute';
        var totalH = Math.max(excelFilteredVals.length * EXCEL_ROW_H, 0);
        spacer.style.height = totalH + 'px';
        excelLastRange = { start: -1, end: -1 };
        renderExcelVirtualSlice(true);
      }

      function renderExcelVirtualSlice(force) {
        if (isExcelLoading || isExcelDateMode) return;
        var listEl = document.getElementById('excelCheckboxList');
        var viewport = document.getElementById('excelVirtualViewport');
        var total = excelFilteredVals.length;

        if (total === 0) {
          viewport.style.transform = 'translateY(0px)';
          viewport.innerHTML = '<div style="padding: 8px; color: #888; font-style: italic; font-size: 12px;">No matching values</div>';
          excelLastRange = { start: 0, end: 0 };
          return;
        }

        var scrollTop = listEl.scrollTop;
        var viewH = listEl.clientHeight || 180;
        var startIdx = Math.max(0, Math.floor(scrollTop / EXCEL_ROW_H) - EXCEL_OVERSCAN);
        var endIdx   = Math.min(total, Math.ceil((scrollTop + viewH) / EXCEL_ROW_H) + EXCEL_OVERSCAN);

        if (!force && startIdx === excelLastRange.start && endIdx === excelLastRange.end) {
          return;
        }
        excelLastRange = { start: startIdx, end: endIdx };

        viewport.style.transform = 'translateY(' + (startIdx * EXCEL_ROW_H) + 'px)';

        var parts = new Array(endIdx - startIdx);
        for (var i = startIdx; i < endIdx; i++) {
          var v = excelFilteredVals[i];
          var isChecked = excelSelectedSet.has(v) ? 'checked' : '';
          var labelText = (v === '' || v === null) ? '(Blanks)' : escapeExcelHtml(v);
          parts[i - startIdx] =
            '<label class="excel-val-item" title="' + labelText + '">' +
            '<input type="checkbox" data-idx="' + i + '" ' + isChecked + ' onchange="toggleExcelItem(this)">' +
            '<span class="excel-val-text">' + labelText + '</span>' +
            '</label>';
        }
        viewport.innerHTML = parts.join('');
      }

      function toggleExcelItem(cb) {
        var idx = parseInt(cb.getAttribute('data-idx'), 10);
        var v = excelFilteredVals[idx];
        if (cb.checked) {
          excelSelectedSet.add(v);
        } else {
          excelSelectedSet.delete(v);
        }
        updateSelectAllState();
      }

      function updateSelectAllState() {
        var selectAll = document.getElementById('excelSelectAll');
        if (!selectAll) return;
        var total = excelFilteredVals.length;
        if (total === 0) {
          selectAll.checked = false;
          selectAll.indeterminate = false;
          return;
        }
        var checkedCount = 0;
        for (var i = 0; i < total; i++) {
          if (excelSelectedSet.has(excelFilteredVals[i])) checkedCount++;
        }
        selectAll.checked = (checkedCount === total);
        selectAll.indeterminate = (checkedCount > 0 && checkedCount < total);
      }

      function toggleSelectAll(cb) {
        if (isExcelLoading) return;
        if (cb.checked) {
          for (var i = 0; i < excelFilteredVals.length; i++) {
            excelSelectedSet.add(excelFilteredVals[i]);
          }
        } else {
          for (var i = 0; i < excelFilteredVals.length; i++) {
            excelSelectedSet.delete(excelFilteredVals[i]);
          }
        }
        cb.indeterminate = false;
        if (isExcelDateMode) {
          renderExcelDateTree();
        } else {
          renderExcelVirtualSlice(true);
        }
      }

      function filterExcelSearch() {
        if (isExcelLoading) return;
        var q = document.getElementById('excelSearchInput').value.trim().toLowerCase();
        if (q === '') {
          excelFilteredVals = excelAllVals;
          if (isExcelDateMode) {
            excelExpandedYears.clear();
            excelExpandedMonths.clear();
          }
        } else {
          excelFilteredVals = [];
          for (var i = 0; i < excelAllVals.length; i++) {
            var v = excelAllVals[i];
            var match = false;
            if (v === '' || v === null) {
              match = '(blanks)'.indexOf(q) !== -1;
            } else if (isExcelDateMode) {
              var s = String(v);
              var m = /^(\d{4})-(\d{2})-(\d{2})$/.exec(s);
              if (m) {
                var moIdx = parseInt(m[2], 10) - 1;
                var moLbl = (moIdx >= 0 && moIdx < 12) ? MONTH_NAMES[moIdx].toLowerCase() : '';
                var searchable = s + ' ' + m[1] + '-' + moLbl + '-' + m[3] + ' ' + m[3] + ' ' + moLbl + ' ' + m[1];
                match = searchable.indexOf(q) !== -1;
              } else {
                match = s.toLowerCase().indexOf(q) !== -1;
              }
            } else {
              match = String(v).toLowerCase().indexOf(q) !== -1;
            }
            if (match) excelFilteredVals.push(v);
          }
          excelSelectedSet = new Set(excelFilteredVals);

          // Auto-expand matching years & months while searching
          if (isExcelDateMode) {
            excelExpandedYears.clear();
            excelExpandedMonths.clear();
            for (var j = 0; j < excelFilteredVals.length; j++) {
              var dm = /^(\d{4})-(\d{2})-(\d{2})$/.exec(String(excelFilteredVals[j]));
              if (dm) {
                excelExpandedYears.add(dm[1]);
                excelExpandedMonths.add(dm[1] + '-' + dm[2]);
              }
            }
          }
        }
        document.getElementById('excelCheckboxList').scrollTop = 0;
        if (isExcelDateMode) {
          renderExcelDateTree();
        } else {
          resetExcelVirtualList();
        }
        updateSelectAllState();
      }

      function applyExcelFilter() {
        if (isExcelLoading || excelAllVals.length === 0) {
          closeExcelPopup();
          return;
        }
        if (!window.excelFilters[activeTable]) window.excelFilters[activeTable] = {};
        var q = document.getElementById('excelSearchInput').value.trim();
        var sourceList = (q === '') ? excelAllVals : excelFilteredVals;
        var chosen = [];

        for (var i = 0; i < sourceList.length; i++) {
          if (excelSelectedSet.has(sourceList[i])) {
            chosen.push(sourceList[i]);
          }
        }

        if (chosen.length === excelAllVals.length && q === '') {
          delete window.excelFilters[activeTable][activeCol];
          if (Object.keys(window.excelFilters[activeTable]).length === 0) {
            delete window.excelFilters[activeTable];
          }
        } else {
          window.excelFilters[activeTable][activeCol] = chosen;
        }
        syncTableFilterUI(activeTable);
        pushExcelFilters();
        closeExcelPopup();
      }

      function sortExcelCol(dir) {
        Shiny.setInputValue('excel_sort', { table: activeTable, col: activeCol, dir: dir, nonce: Math.random() });
        closeExcelPopup();
      }

      function clearExcelColFilter() {
        if (window.excelFilters[activeTable]) {
          delete window.excelFilters[activeTable][activeCol];
          if (Object.keys(window.excelFilters[activeTable]).length === 0) {
            delete window.excelFilters[activeTable];
          }
        }
        syncTableFilterUI(activeTable);
        pushExcelFilters();
        closeExcelPopup();
      }

      function pushExcelFilters() {
        Shiny.setInputValue('excel_active_filters', { filters: window.excelFilters, nonce: Math.random() });
      }

      document.addEventListener('click', function(e) {
        var popup = document.getElementById('excelFilterPopup');
        if (!popup || popup.style.display === 'none') return;
        var path = e.composedPath ? e.composedPath() : [];
        var clickedInside = path.indexOf(popup) !== -1 || popup.contains(e.target);
        if (!clickedInside && !e.target.closest('.excel-filter-btn')) {
          closeExcelPopup();
        }
      });

      // Receive preloaded unfiltered values & date column metadata (already sorted in R)
      Shiny.addCustomMessageHandler('updateColValues', function(msg) {
        var colMap = {};
        if (msg.values) {
          Object.keys(msg.values).forEach(function(col) {
            colMap[col] = normalizeArray(msg.values[col]);
          });
        }
        window.excelColValues[msg.table] = colMap;
        window.excelDateCols[msg.table] = new Set(normalizeArray(msg.dateCols));
      });

      // Receive lazy on-click cascading values for a single column (already sorted in R)
      Shiny.addCustomMessageHandler('receiveColValues', function(msg) {
        if (!pendingFilterReq) return;
        if (pendingFilterReq.table !== msg.table ||
            pendingFilterReq.col !== msg.col ||
            pendingFilterReq.reqId !== msg.reqId) {
          return;
        }
        pendingFilterReq = null;
        var popup = document.getElementById('excelFilterPopup');
        if (!popup || popup.style.display === 'none') {
          isExcelLoading = false;
          return;
        }

        populateExcelPopupList(msg.table, msg.col, normalizeArray(msg.values));
      });

      Shiny.addCustomMessageHandler('resetTableFilters', function(msg) {
        var tables = Array.isArray(msg.tables) ? msg.tables : [msg.tables];
        tables.forEach(function(tid) {
          if (window.excelFilters[tid]) delete window.excelFilters[tid];
          syncTableFilterUI(tid);
        });
        pushExcelFilters();
      });

      Shiny.addCustomMessageHandler('resetFileInput', function(id) {
        var el = document.getElementById(id);
        if (el) {
          el.value = '';
          var group = el.closest('.form-group');
          if (group) {
            var textInput = group.querySelector('input[type="text"]');
            if (textInput) textInput.value = '';
            var progress = group.querySelector('.progress-bar');
            if (progress) {
              progress.style.width = '0%';
              progress.innerText = '';
            }
          }
        }
      });

      Shiny.addCustomMessageHandler('downloadBase64File', function(msg) {
        var byteChars = atob(msg.base64);
        var byteNumbers = new Array(byteChars.length);
        for (var i = 0; i < byteChars.length; i++) {
          byteNumbers[i] = byteChars.charCodeAt(i);
        }
        var blob = new Blob([new Uint8Array(byteNumbers)], { type: msg.mime });
        var url = URL.createObjectURL(blob);
        var a = document.createElement('a');
        a.href = url;
        a.download = msg.filename;
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        URL.revokeObjectURL(url);
      });

      Shiny.addCustomMessageHandler('downloadTextFile', function(msg) {
        var blob = new Blob([msg.content], { type: msg.mime || 'text/csv;charset=utf-8;' });
        var url = URL.createObjectURL(blob);
        var a = document.createElement('a');
        a.href = url;
        a.download = msg.filename;
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        URL.revokeObjectURL(url);
      });

      // --- Outliers Table Checkbox & Visible Scope State ---
      window.outlierCheckedSet = new Set();
      window.savedTablePages = {};

      function syncOutlierDOMCheckboxes() {
        document.querySelectorAll('.outlier-row-cb').forEach(function(cb) {
          var key = decodeURIComponent(cb.getAttribute('data-key'));
          cb.checked = window.outlierCheckedSet.has(key);
        });
        syncOutlierHeaderCheckbox();
      }

      function pushOutlierSelection() {
        var visibleChecked = [];
        document.querySelectorAll('.outlier-row-cb').forEach(function(cb) {
          var key = decodeURIComponent(cb.getAttribute('data-key'));
          if (cb.checked) {
            window.outlierCheckedSet.add(key);
            visibleChecked.push(key);
          } else {
            window.outlierCheckedSet.delete(key);
          }
        });
        Shiny.setInputValue('outliers_checked_rows', Array.from(window.outlierCheckedSet), { priority: 'event' });
        Shiny.setInputValue('outliers_visible_checked_rows', visibleChecked, { priority: 'event' });
      }

      function syncOutlierHeaderCheckbox() {
        var headerCb = document.getElementById('outlier_select_all_cb');
        if (!headerCb) return;
        var rowCbs = Array.from(document.querySelectorAll('.outlier-row-cb'));
        if (rowCbs.length === 0) {
          headerCb.checked = false;
          headerCb.indeterminate = false;
          return;
        }
        var checkedCount = rowCbs.filter(function(cb) { return cb.checked; }).length;
        headerCb.checked = (checkedCount === rowCbs.length);
        headerCb.indeterminate = (checkedCount > 0 && checkedCount < rowCbs.length);
      }

      function toggleOutlierRow(cb) {
        var key = decodeURIComponent(cb.getAttribute('data-key'));
        if (cb.checked) {
          window.outlierCheckedSet.add(key);
        } else {
          window.outlierCheckedSet.delete(key);
        }
        syncOutlierHeaderCheckbox();
        pushOutlierSelection();
      }

      function toggleAllVisibleOutliers(headerCb) {
        var rowCbs = document.querySelectorAll('.outlier-row-cb');
        rowCbs.forEach(function(cb) {
          cb.checked = headerCb.checked;
          var key = decodeURIComponent(cb.getAttribute('data-key'));
          if (headerCb.checked) {
            window.outlierCheckedSet.add(key);
          } else {
            window.outlierCheckedSet.delete(key);
          }
        });
        headerCb.indeterminate = false;
        pushOutlierSelection();
      }

      function openOutlierExceptionDialog() {
        var visibleChecked = [];
        document.querySelectorAll('.outlier-row-cb:checked').forEach(function(cb) {
          visibleChecked.push(decodeURIComponent(cb.getAttribute('data-key')));
        });
        Shiny.setInputValue('open_outlier_exc_modal', {
          visible_checked: visibleChecked,
          nonce: Math.random()
        }, { priority: 'event' });
      }

      function syncExcRules() {
        var checked = [];
        document.querySelectorAll('.exc-rule-cb:checked').forEach(function(cb) {
          checked.push(cb.value);
        });
        Shiny.setInputValue('exc_rules_selected', checked, { priority: 'event' });
      }

      function selectExcNextDayMode(mode) {
        var cb2 = document.getElementById('exc_rule_cb_2');
        if (cb2) cb2.checked = true;
        var radioEl = document.getElementById(mode === 'cutoff' ? 'exc_nd_mode_cutoff' : 'exc_nd_mode_all');
        if (radioEl) radioEl.checked = true;
        var cutoffInput = document.getElementById('exc_nd_cutoff_val');
        if (cutoffInput) {
          cutoffInput.disabled = (mode !== 'cutoff');
          Shiny.setInputValue('exc_nd_cutoff_input', cutoffInput.value, { priority: 'event' });
        }
        Shiny.setInputValue('exc_nd_mode_input', mode, { priority: 'event' });
        syncExcRules();
      }

      Shiny.addCustomMessageHandler('evalResetExcRules', function(msg) {
        Shiny.setInputValue('exc_rules_selected', [], { priority: 'event' });
        Shiny.setInputValue('exc_extend_hrs_input', '12', { priority: 'event' });
        Shiny.setInputValue('exc_nd_mode_input', 'all', { priority: 'event' });
        Shiny.setInputValue('exc_nd_cutoff_input', '15:00', { priority: 'event' });
      });

      Shiny.addCustomMessageHandler('clearOutlierSelection', function(msg) {
        window.outlierCheckedSet.clear();
        window.savedTablePages = {};
        syncOutlierDOMCheckboxes();
        pushOutlierSelection();
      });
    )---"))
  ),
  
  # Floating Excel-style Filter Dialog
  div(
    id = "excelFilterPopup", class = "excel-popup",
    div(id = "excelFilterTitle", style = "font-weight: bold; margin-bottom: 6px; color: #333;"),
    div(class = "excel-popup-item", onclick = "sortExcelCol('asc')", HTML("↑↓ Sort A to Z")),
    div(class = "excel-popup-item", onclick = "sortExcelCol('desc')", HTML("↓↑ Sort Z to A")),
    tags$hr(style = "margin: 5px 0;"),
    div(class = "excel-popup-item", onclick = "clearExcelColFilter()", HTML("✖ Clear Filter From Column")),
    tags$input(
      id = "excelSearchInput", type = "text", placeholder = "Search...",
      oninput = "filterExcelSearch()",
      style = "width: 100%; padding: 4px 6px; margin-top: 5px; border: 1px solid #ccc; box-sizing: border-box;"
    ),
    div(
      class = "excel-select-all-bar",
      tags$label(
        tags$input(id = "excelSelectAll", type = "checkbox", onchange = "toggleSelectAll(this)"),
        tags$b("(Select All)")
      )
    ),
    div(
      id = "excelCheckboxList", class = "excel-checkbox-list",
      onscroll = "renderExcelVirtualSlice(false)",
      div(
        id = "excelVirtualSpacer", style = "position: relative; width: 100%;",
        div(id = "excelVirtualViewport", style = "position: absolute; top: 0; left: 0; right: 0; will-change: transform;")
      )
    ),
    div(
      style = "display: flex; justify-content: flex-end; gap: 6px; margin-top: 6px;",
      tags$button("OK", id = "excelOkBtn", onclick = "applyExcelFilter()", style = "padding: 3px 14px; background: #0d6efd; color: white; border: none; border-radius: 3px; cursor: pointer;"),
      tags$button("Cancel", onclick = "closeExcelPopup()", style = "padding: 3px 10px; border: 1px solid #ccc; background: #f8f9fa; border-radius: 3px; cursor: pointer;")
    )
  ),
  
  titlePanel("Microbiology LTAT Performance Dashboard"),
  
  tabsetPanel(
    id = "main_tabs",
    
    # ==========================================================================
    # TAB 1: ALL SPECIMEN RECORDS
    # ==========================================================================
    tabPanel(
      "All Specimen Records",
      br(),
      div(
        class = "tab-toolbar",
        fluidRow(
          column(5, fileInput("extract_file", "1. Upload Extract File (.xlsx):", buttonLabel = "Upload", accept = c(".xlsx", ".xls"), width = "100%")),
          column(7, style = "padding-top: 28px;", uiOutput("extract_tab_hint"))
        )
      ),
      clear_filters_bar("all"),
      reactableOutput("all_table")
    ),
    
    # ==========================================================================
    # TAB 2: SPECIFICATION MAP
    # ==========================================================================
    tabPanel(
      "Specification Map",
      br(),
      div(
        class = "tab-toolbar",
        fluidRow(
          column(5, fileInput("spec_map_file", "2. Upload Specification Map (.csv / .xlsx):", buttonLabel = "Upload", accept = c(".csv", ".xlsx"), width = "100%")),
          column(
            7,
            style = "padding-top: 25px; display: flex; justify-content: flex-end; align-items: center; gap: 12px;",
            uiOutput("spec_map_status_badge"),
            actionButton("download_spec_map", "Save Specification Map CSV", icon = icon("download"), class = "btn-outline-primary")
          )
        )
      ),
      
      tabsetPanel(
        id = "spec_map_nested_tabs",
        
        tabPanel(
          "Test Mapping",
          br(),
          div(
            style = "display: flex; justify-content: flex-end; margin-bottom: 10px;",
            actionButton("btn_check_unmapped", "Check Unmapped Tests", icon = icon("search"), class = "btn-warning", style = "font-weight: bold;")
          ),
          fluidRow(
            column(
              4,
              h4("1. Test Name with LTAT (Select Row)"),
              reactableOutput("ltat_master_table")
            ),
            column(
              3,
              class = "transfer-box",
              h4("2. Mapped Extract Tests"),
              uiOutput("selected_ltat_badge"),
              selectInput("mapped_list", label = NULL, choices = NULL, multiple = TRUE, selectize = FALSE, width = "100%")
            ),
            column(
              2,
              style = "display: flex; flex-direction: column; justify-content: center; align-items: center; height: 480px; gap: 15px;",
              actionButton("btn_add_map", HTML("◀ Add"), class = "btn-primary", style = "width: 110px; font-weight: bold;"),
              actionButton("btn_remove_map", HTML("Remove ▶"), class = "btn-danger", style = "width: 110px; font-weight: bold;")
            ),
            column(
              3,
              class = "transfer-box",
              h4("3. Unmapped Extract Tests"),
              tags$small(style = "color: #666;", "Format: TestName | Specimen Type"),
              selectInput("unmapped_list", label = NULL, choices = NULL, multiple = TRUE, selectize = FALSE, width = "100%")
            )
          )
        ),
        
        tabPanel(
          "LTAT Editor",
          br(),
          fluidRow(
            column(
              7,
              h4("LTAT Rules (Select Row to Edit)", style = "margin-top: 0;"),
              reactableOutput("catalog_editor_table")
            ),
            column(
              5,
              wellPanel(
                style = "background: #f8f9fa; margin-top: 28px;",
                h4("Edit / Add LTAT Entry"),
                tags$small(style = "color: #666; display: block; margin-bottom: 12px;",
                           "Note: Selecting another row before clicking Save discards unsaved edits."),
                
                textInput("cat_edit_name", "Test Name with LTAT:", width = "100%",
                          placeholder = "e.g. Culture & Sensitivity - Urine - URINE"),
                
                textInput("cat_edit_display", "LTAT Display:", width = "100%",
                          placeholder = "e.g. 48 - 72 hrs, 5 working days, Same day, or Next day"),
                
                selectInput(
                  "cat_rule_type", "Rule Type:",
                  choices = c("Duration", "Same Day", "Next Day", "Next Day Cutoff"),
                  selected = "Duration",
                  width = "100%"
                ),
                
                conditionalPanel(
                  condition = "input.cat_rule_type == 'Duration'",
                  div(
                    style = "background: #fff; border: 1px solid #dee2e6; border-radius: 4px; padding: 10px; margin-bottom: 10px;",
                    tags$label("Target Duration (Integer Only):"),
                    fluidRow(
                      column(4, numericInput("cat_dur_dd", "dd (Days):", value = 0, min = 0, step = 1)),
                      column(4, numericInput("cat_dur_hh", "hh (Hours):", value = 0, min = 0, step = 1)),
                      column(4, numericInput("cat_dur_mm", "mm (Mins):", value = 0, min = 0, step = 1))
                    ),
                    div(
                      class = "inline-days",
                      checkboxGroupInput(
                        "cat_skip_days", "Days to Skip (Elapsed Time Paused):",
                        choices = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
                        inline = TRUE
                      )
                    ),
                    checkboxInput("cat_skip_holidays", "Skip Holidays (holiday list placeholder)", value = FALSE)
                  )
                ),
                
                conditionalPanel(
                  condition = "input.cat_rule_type == 'Same Day'",
                  div(
                    style = "background: #e7f1ff; border-left: 4px solid #0d6efd; padding: 8px 10px; margin-bottom: 10px; font-size: 13px;",
                    "Validates PASS if ", strong("Validation Date"), " is on the exact same calendar date as ", strong("Accepted Date (Unit)"), "."
                  )
                ),
                
                conditionalPanel(
                  condition = "input.cat_rule_type == 'Next Day'",
                  div(
                    style = "background: #f3e8ff; border-left: 4px solid #6f42c1; padding: 8px 10px; margin-bottom: 10px; font-size: 13px;",
                    "Time-ignoring calendar rule: validates PASS if ", strong("Validation Date"),
                    " is on or before ", strong("23:59:59 of the next calendar day"),
                    " regardless of what time the specimen was accepted."
                  )
                ),
                
                conditionalPanel(
                  condition = "input.cat_rule_type == 'Next Day Cutoff'",
                  div(
                    style = "background: #fff; border: 1px solid #dee2e6; border-radius: 4px; padding: 10px; margin-bottom: 10px;",
                    tags$small(style = "color: #555; display: block; margin-bottom: 8px;",
                               "Same day if received before cut-off hour; otherwise next calendar day by target hour."),
                    fluidRow(
                      column(6, textInput("cat_cutoff_hhmm", "Cut-off Hour (hh:mm):", value = "12:00", placeholder = "e.g. 12:00")),
                      column(6, textInput("cat_target_hhmm", "Next-Day Target Hour (hh:mm):", value = "17:00", placeholder = "e.g. 17:00"))
                    )
                  )
                ),
                
                hr(),
                div(
                  style = "display: flex; gap: 10px; flex-wrap: wrap;",
                  actionButton("cat_btn_save", "Save", class = "btn-primary", style = "min-width: 90px; font-weight: bold;"),
                  actionButton("cat_btn_add", "Add", class = "btn-success", style = "min-width: 90px; font-weight: bold;"),
                  actionButton("cat_btn_delete", "Delete", class = "btn-danger", style = "min-width: 90px; font-weight: bold;")
                )
              )
            )
          )
        )
      )
    ),
    
    # ==========================================================================
    # TAB 3: PROCESS
    # ==========================================================================
    tabPanel(
      "Process",
      br(),
      div(
        class = "tab-toolbar",
        fluidRow(
          column(
            8,
            h5(strong("Required Files Checklist for Processing:"), style = "margin-top: 0; margin-bottom: 8px;"),
            uiOutput("process_checklist_ui")
          ),
          column(
            4,
            style = "display: flex; align-items: center; justify-content: flex-end; gap: 10px; padding-top: 8px;",
            actionButton("process_btn", "Process", class = "btn-primary", style = "font-weight: bold; min-width: 110px;"),
            uiOutput("export_xlsx_btn_ui")
          )
        )
      ),
      
      uiOutput("obsolete_spec_banner_ui"),
      uiOutput("kpi_cards_ui"),
      
      tabsetPanel(
        id = "summary_nested_tabs",
        tabPanel(
          "Processed Extract",
          br(),
          clear_filters_bar("proc_ext"),
          reactableOutput("processed_extract_table")
        ),
        tabPanel(
          "LTAT Report",
          br(),
          clear_filters_bar("summary"),
          reactableOutput("summary_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  tbl_all        <- AppTable$new(id = "all",        mode = "report")
  tbl_proc_ext   <- AppTable$new(id = "proc_ext",   mode = "report")
  tbl_summary    <- AppTable$new(id = "summary",    mode = "report")
  tbl_outliers   <- AppTable$new(id = "outliers",   mode = "report")
  tbl_map_master <- AppTable$new(id = "map_master", mode = "selector")
  tbl_cat_editor <- AppTable$new(id = "cat_editor", mode = "selector")
  
  empty_catalog <- tibble::tibble(
    LTAT_Test_Name = character(),
    LTAT_Display   = character(),
    Rule_Type      = character(),
    Dur_DD         = integer(),
    Dur_HH         = integer(),
    Dur_MM         = integer(),
    Skip_Days      = character(),
    Skip_Holidays  = logical(),
    Cutoff_HHMM    = character(),
    Target_HHMM    = character()
  )
  
  empty_map <- tibble::tibble(
    Extract_Key    = character(),
    LTAT_Test_Name = character()
  )
  
  rv <- reactiveValues(
    is_processed           = FALSE,
    spec_dirty             = FALSE,
    outliers_added         = FALSE,
    has_extract            = FALSE,
    has_spec_map           = FALSE,
    valid_map              = NULL,
    raw_df                 = NULL,
    processed_df           = NULL,
    catalog                = empty_catalog,
    mapping                = empty_map,
    cat_sel_idx            = 1L,
    map_sel_idx            = 1L,
    pending_edit           = NULL,
    pending_delete         = NULL,
    outlier_checked        = character(0),
    outlier_visible_keys   = character(0),
    modal_visible_checked  = character(0),
    modal_filtered_keys    = character(0),
    modal_spec_choices     = character(0),
    modal_selected_tests   = character(0)
  )
  
  get_extract_unmapped_keys <- function() {
    if (!rv$has_extract || is.null(rv$raw_df)) return(character(0))
    extract_keys <- sort(unique(rv$raw_df$Extract_Key))
    setdiff(extract_keys, rv$mapping$Extract_Key)
  }
  
  refresh_mapping_lists <- function() {
    extract_keys   <- if (rv$has_extract && !is.null(rv$raw_df)) rv$raw_df$Extract_Key else character(0)
    valid_map_keys <- if (!is.null(rv$valid_map)) rv$valid_map$Extract_Key else character(0)
    all_keys       <- sort(unique(c(extract_keys, valid_map_keys)))
    
    unmapped_choices <- setdiff(all_keys, rv$mapping$Extract_Key)
    
    if (nrow(rv$catalog) > 0) {
      idx <- min(max(1L, rv$map_sel_idx), nrow(rv$catalog))
      sel_name <- rv$catalog$LTAT_Test_Name[idx]
      mapped_choices <- rv$mapping %>%
        filter(LTAT_Test_Name == sel_name) %>%
        pull(Extract_Key) %>%
        sort()
    } else {
      mapped_choices <- character(0)
    }
    
    updateSelectInput(session, "mapped_list", choices = mapped_choices, selected = character(0))
    updateSelectInput(session, "unmapped_list", choices = unmapped_choices, selected = character(0))
  }
  
  set_outliers_tab <- function(show) {
    if (show && !rv$outliers_added) {
      insertTab(
        inputId = "summary_nested_tabs",
        tab = tabPanel(
          "Outliers / Failed Specimens",
          br(),
          clear_filters_bar("outliers"),
          reactableOutput("outliers_table")
        ),
        target = "LTAT Report",
        position = "after"
      )
      rv$outliers_added <- TRUE
    } else if (!show && rv$outliers_added) {
      removeTab(inputId = "summary_nested_tabs", target = "Outliers / Failed Specimens")
      rv$outliers_added <- FALSE
    }
  }
  
  reset_processed_state <- function() {
    rv$is_processed         <- FALSE
    rv$spec_dirty           <- FALSE
    rv$processed_df         <- NULL
    rv$outlier_checked      <- character(0)
    rv$outlier_visible_keys <- character(0)
    set_outliers_tab(FALSE)
    session$sendCustomMessage("clearOutlierSelection", list())
    session$sendCustomMessage("resetTableFilters", list(tables = c("proc_ext", "summary", "outliers")))
  }
  
  mark_spec_changed <- function() {
    if (rv$is_processed) {
      rv$spec_dirty <- TRUE
    }
  }
  
  reject_upload <- function(input_id, file_label, error_msg) {
    session$sendCustomMessage("resetFileInput", input_id)
    showModal(modalDialog(
      title = paste("Invalid File Structure —", file_label),
      tags$p("The uploaded file was discarded because its schema does not match what the app requires:"),
      tags$pre(
        style = "background: #f8d7da; color: #842029; border: 1px solid #f5c2c7; padding: 10px; white-space: pre-wrap;",
        error_msg
      ),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
  }
  
  populate_editor_fields <- function(idx) {
    if (is.null(idx) || idx < 1 || idx > nrow(rv$catalog)) return()
    row <- rv$catalog[idx, ]
    updateTextInput(session, "cat_edit_name", value = row$LTAT_Test_Name)
    updateTextInput(session, "cat_edit_display", value = row$LTAT_Display)
    updateSelectInput(session, "cat_rule_type", selected = row$Rule_Type)
    updateNumericInput(session, "cat_dur_dd", value = ifelse(is.na(row$Dur_DD), 0L, row$Dur_DD))
    updateNumericInput(session, "cat_dur_hh", value = ifelse(is.na(row$Dur_HH), 0L, row$Dur_HH))
    updateNumericInput(session, "cat_dur_mm", value = ifelse(is.na(row$Dur_MM), 0L, row$Dur_MM))
    
    sd_vec <- if (!is.na(row$Skip_Days) && nzchar(row$Skip_Days)) str_trim(str_split(row$Skip_Days, ",")[[1]]) else character(0)
    updateCheckboxGroupInput(session, "cat_skip_days", selected = sd_vec)
    updateCheckboxInput(session, "cat_skip_holidays", value = isTRUE(row$Skip_Holidays))
    
    updateTextInput(session, "cat_cutoff_hhmm", value = ifelse(is.na(row$Cutoff_HHMM) || row$Cutoff_HHMM == "", "12:00", row$Cutoff_HHMM))
    updateTextInput(session, "cat_target_hhmm", value = ifelse(is.na(row$Target_HHMM) || row$Target_HHMM == "", "17:00", row$Target_HHMM))
  }
  
  # ============================================================================
  # IMMEDIATE SCHEMA VALIDATION & AUTO-POPULATION ON UPLOAD
  # ============================================================================
  
  observeEvent(input$extract_file, {
    reset_processed_state()
    session$sendCustomMessage("resetTableFilters", list(tables = c("all")))
    rv$has_extract <- FALSE
    rv$raw_df      <- NULL
    
    check <- validate_extract_schema(input$extract_file)
    if (!check$ok) {
      reject_upload("extract_file", "1. Extract File", check$error)
    } else {
      rv$raw_df      <- check$data
      rv$has_extract <- TRUE
      refresh_mapping_lists()
      showNotification(paste0("Extract File verified & loaded (", nrow(check$data), " rows)."), type = "message")
    }
  })
  
  observeEvent(input$spec_map_file, {
    check <- validate_spec_map_schema(input$spec_map_file)
    if (!check$ok) {
      reject_upload("spec_map_file", "2. Specification Map", check$error)
      return()
    }
    
    rv$catalog      <- check$catalog
    rv$valid_map    <- check$mapping
    rv$mapping      <- check$mapping
    rv$has_spec_map <- TRUE
    rv$cat_sel_idx  <- 1L
    rv$map_sel_idx  <- 1L
    
    mark_spec_changed()
    
    if (nrow(rv$catalog) > 0) populate_editor_fields(1L)
    refresh_mapping_lists()
    
    showNotification(
      paste0("Specification Map verified & loaded (", nrow(check$catalog), " rules, ", nrow(check$mapping), " mappings)."),
      type = "message"
    )
  })
  
  observeEvent(input$outliers_checked_rows, {
    rv$outlier_checked <- if (is.null(input$outliers_checked_rows)) character(0) else input$outliers_checked_rows
  }, ignoreNULL = FALSE)

  observeEvent(input$outliers_visible_checked_rows, {
    rv$outlier_visible_keys <- if (is.null(input$outliers_visible_checked_rows)) character(0) else input$outliers_visible_checked_rows
  }, ignoreNULL = FALSE)
  
  output$spec_map_status_badge <- renderUI({
    if (!rv$has_spec_map) {
      tags$span(style = "color: #666; font-size: 13px;", "No Specification Map loaded")
    } else {
      tags$span(
        style = "color: #155724; font-size: 13px; font-weight: bold;",
        sprintf("Loaded: %d LTAT Rules | %d Test Mappings", nrow(rv$catalog), nrow(rv$mapping))
      )
    }
  })
  
  output$extract_tab_hint <- renderUI({
    if (!rv$has_extract) {
      tags$span(style = "color: #666; font-size: 13px;", "Upload your Extract (.xlsx) file to preview raw specimen records.")
    } else if (!rv$is_processed) {
      tags$span(
        style = "color: #084298; font-size: 13px;",
        strong(nrow(rv$raw_df)), " raw specimen records loaded. Go to ", strong("Process"), " and click ", strong("Process"), " to evaluate LTAT."
      )
    } else {
      tags$span(
        style = "color: #155724; font-size: 13px;",
        "Showing raw uploaded records (", strong(nrow(rv$raw_df)), " rows). View evaluated results under the ", strong("Process"), " tab."
      )
    }
  })
  
  # ============================================================================
  # CHECKLIST, OBSOLETE BANNER, & PROCESS BUTTON
  # ============================================================================
  
  output$process_checklist_ui <- renderUI({
    badge <- function(ok, label, detail) {
      icon_html <- if (ok) {
        tags$span(style = "color: #198754; font-weight: bold; font-size: 16px;", "✔")
      } else {
        tags$span(style = "color: #dc3545; font-weight: bold; font-size: 16px;", "✖")
      }
      txt_style <- if (ok) "color: #155724;" else "color: #842029;"
      div(
        class = "checklist-item",
        icon_html,
        tags$span(style = txt_style, strong(label), paste0(" (", detail, ")"))
      )
    }
    
    div(
      badge(rv$has_extract,  "1. Extract File",      if (rv$has_extract)  paste0(nrow(rv$raw_df), " rows") else "Upload in 'All Specimen Records'"),
      badge(rv$has_spec_map, "2. Specification Map", if (rv$has_spec_map) paste0(nrow(rv$catalog), " rules, ", nrow(rv$mapping), " mappings") else "Upload in 'Specification Map'")
    )
  })
  
  output$obsolete_spec_banner_ui <- renderUI({
    req(rv$is_processed, rv$spec_dirty)
    div(
      class = "obsolete-banner",
      div(
        strong("⚠ Obsolete Processed Data: "),
        "The Specification Map (LTAT rules or test mappings) has been modified since the last run. ",
        "The tables below still reflect the previous specification until you re-process."
      ),
      actionButton("reprocess_banner_btn", "Re-Process Now", class = "btn-warning btn-sm", style = "font-weight: bold; white-space: nowrap;")
    )
  })
  
  run_ltat_processing <- function() {
    missing <- c()
    if (!rv$has_extract)  missing <- c(missing, "• 1. Extract File (upload in 'All Specimen Records' tab)")
    if (!rv$has_spec_map) missing <- c(missing, "• 2. Specification Map (upload in 'Specification Map' tab)")
    
    if (length(missing) > 0) {
      showModal(modalDialog(
        title = "Missing or Invalid Required Files",
        tags$p("Please upload all required valid files before clicking Process:"),
        tags$pre(style = "background: #f8d7da; color: #842029; border: 1px solid #f5c2c7; padding: 10px;",
                 paste(missing, collapse = "\n")),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    tryCatch({
      df_in <- rv$raw_df
      if (!("Lab. No" %in% names(df_in)))     df_in$`Lab. No`     <- as.character(seq_len(nrow(df_in)))
      if (!("Ward/Clinic" %in% names(df_in))) df_in$`Ward/Clinic` <- "N/A"
      
      map_lookup <- rv$mapping %>%
        inner_join(rv$catalog, by = "LTAT_Test_Name")
      
      df_joined <- df_in %>%
        left_join(map_lookup, by = "Extract_Key") %>%
        mutate(
          .row_id                = row_number(),
          dt_unit                = parse_extract_datetime(`Accepted Date (Unit)`),
          dt_valid               = parse_extract_datetime(`Validation Date`),
          `Accepted Date (Unit)` = format_extract_datetime(dt_unit, `Accepted Date (Unit)`),
          `Validation Date`      = format_extract_datetime(dt_valid, `Validation Date`),
          .row_key               = paste(`Lab. No`, TestName, `Accepted Date (Unit)`, row_number(), sep = " | "),
          raw_tat_hrs            = round(as.numeric(difftime(dt_valid, dt_unit, units = "hours")), 2),
          unit_tat_hrs           = raw_tat_hrs
        )
      
      skip_idx <- which(!is.na(df_joined$Rule_Type) & df_joined$Rule_Type == "Duration" &
                        !is.na(df_joined$Skip_Days) & df_joined$Skip_Days != "")
      if (length(skip_idx) > 0) {
        for (sd_val in unique(df_joined$Skip_Days[skip_idx])) {
          sub_i <- skip_idx[df_joined$Skip_Days[skip_idx] == sd_val]
          df_joined$unit_tat_hrs[sub_i] <- calc_effective_hrs(df_joined$dt_unit[sub_i], df_joined$dt_valid[sub_i], sd_val)
        }
      }
      
      df_joined <- df_joined %>%
        mutate(
          LTAT_Target        = ifelse(is.na(LTAT_Display) | LTAT_Display == "", "N/A", LTAT_Display),
          Mapped_LTAT_Test   = ifelse(is.na(LTAT_Test_Name), "Unmapped", LTAT_Test_Name),
          exc_codes          = "",
          exc_ext_hrs        = 0,
          exc_nextday_cutoff = "",
          orig_status        = evaluate_ltat_status(
            dt_unit, dt_valid, unit_tat_hrs,
            Rule_Type, Dur_DD, Dur_HH, Dur_MM, Cutoff_HHMM, Target_HHMM, Skip_Days
          ),
          Status             = orig_status
        ) %>%
        select(-any_of(c("t_clean", "s_clean", "raw_tat_hrs", "LTAT_Display")))
      
      rv$processed_df         <- df_joined
      rv$is_processed         <- TRUE
      rv$spec_dirty           <- FALSE
      rv$outlier_checked      <- character(0)
      rv$outlier_visible_keys <- character(0)
      set_outliers_tab(TRUE)
      session$sendCustomMessage("clearOutlierSelection", list())
      session$sendCustomMessage("resetTableFilters", list(tables = c("proc_ext", "summary", "outliers")))
      
      refresh_mapping_lists()
      unmapped_in_extract <- get_extract_unmapped_keys()
      
      if (length(unmapped_in_extract) > 0) {
        showNotification(
          paste0("Processed with ", length(unmapped_in_extract), " unmapped test(s) in Extract File."),
          type = "warning", duration = 10
        )
      } else {
        showNotification("LTAT processing complete! All extract tests are mapped.", type = "message")
      }
    }, error = function(e) {
      showModal(modalDialog(
        title = "Processing Error",
        tags$p("An error occurred while processing the dataset:"),
        tags$pre(style = "background: #f8d7da; color: #842029; border: 1px solid #f5c2c7; padding: 10px; white-space: pre-wrap;",
                 conditionMessage(e)),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    })
  }
  
  observeEvent(input$process_btn,          { run_ltat_processing() })
  observeEvent(input$reprocess_banner_btn, { run_ltat_processing() })
  
  # ============================================================================
  # OUTLIER EXCEPTION DIALOG (TWO-ROW MULTI-SELECT CONFIGURATOR)
  # ============================================================================
  
  resolve_modal_target_keys <- function(scopes, selected_tests) {
    if (is.null(scopes) || length(scopes) == 0) return(character(0))
    keys <- character(0)
    all_fail_df <- rv$processed_df %>% filter(orig_status == "FAIL")

    if ("current_page" %in% scopes) {
      keys <- c(keys, rv$modal_visible_checked)
    }
    if ("filtered" %in% scopes) {
      keys <- c(keys, rv$modal_filtered_keys)
    }
    if ("spec_tests" %in% scopes && length(selected_tests) > 0) {
      test_keys <- all_fail_df %>%
        filter(Mapped_LTAT_Test %in% selected_tests) %>%
        pull(.row_key)
      keys <- c(keys, test_keys)
    }
    if ("all" %in% scopes) {
      keys <- c(keys, all_fail_df$.row_key)
    }
    unique(keys)
  }

  observeEvent(input$open_outlier_exc_modal, {
    req(rv$is_processed, rv$processed_df)

    all_fail_df <- rv$processed_df %>% filter(orig_status == "FAIL")
    if (nrow(all_fail_df) == 0) {
      showNotification("There are no failed specimens to configure exceptions for.", type = "message")
      return()
    }

    vis_checked <- input$open_outlier_exc_modal$visible_checked
    if (is.null(vis_checked)) vis_checked <- character(0)

    filtered_tbl <- tbl_outliers$apply_state(
      outliers_base_df(),
      input$excel_active_filters,
      input$excel_sort
    )

    rv$modal_visible_checked <- vis_checked
    rv$modal_filtered_keys   <- filtered_tbl$.row_key

    spec_choices <- setNames(
      rv$catalog$LTAT_Test_Name,
      paste0(rv$catalog$LTAT_Test_Name, "  —  ", rv$catalog$LTAT_Display)
    )
    rv$modal_spec_choices   <- spec_choices
    rv$modal_selected_tests <- character(0)

    scope_choices <- setNames(
      c("current_page", "filtered", "spec_tests", "all"),
      c(
        sprintf("Checked Rows on Current Page (%d)", length(vis_checked)),
        sprintf("All Filtered Outliers (%d)", nrow(filtered_tbl)),
        "Selected Test(s) from Specification",
        sprintf("All Outliers — Unfiltered (%d)", nrow(all_fail_df))
      )
    )

    default_scope <- if (length(vis_checked) > 0) "current_page" else "filtered"

    showModal(modalDialog(
      size = "l",
      title = tagList(
        rule_exception_icon_svg(),
        span(style = "margin-left: 8px;", "Configure Outlier Rule Exceptions")
      ),

      div(
        class = "exc-modal-section",
        div(class = "exc-section-title", "1. Select Exception Rule(s) to Apply"),

        tags$label(
          class = "exc-option-row",
          style = "border-left: 5px solid #0d6efd;",
          div(
            style = "display: flex; align-items: center;",
            tags$input(type = "checkbox", class = "exc-rule-cb", value = "1", onchange = "syncExcRules()"),
            span(class = "exc-num-circle", style = "color: #0d6efd; background: #e7f1ff; border: 1.5px solid #0d6efd;", "1"),
            div(
              strong("Ignore mm:ss", style = "color: #0d6efd; font-size: 14px;"),
              tags$div(style = "font-size: 12px; color: #666;", "Truncate timestamps to the hour (ignore minute/second overrun)")
            )
          )
        ),

        div(
          class = "exc-option-row",
          style = "border-left: 5px solid #6f42c1; display: block; cursor: default;",
          tags$label(
            style = "display: flex; align-items: center; margin: 0; cursor: pointer; font-weight: normal;",
            tags$input(type = "checkbox", class = "exc-rule-cb", id = "exc_rule_cb_2", value = "2", onchange = "syncExcRules()"),
            span(class = "exc-num-circle", style = "color: #6f42c1; background: #f3e8ff; border: 1.5px solid #6f42c1;", "2"),
            div(
              strong("Extend to Next Day", style = "color: #6f42c1; font-size: 14px;"),
              tags$div(style = "font-size: 12px; color: #666;", "Extend allowed target deadline to 23:59:59 of the next calendar day")
            )
          ),
          div(
            class = "exc-nd-controls",
            tags$label(
              class = "exc-nd-radio-lbl",
              tags$input(
                type = "radio", name = "exc_nd_mode", id = "exc_nd_mode_all", value = "all", checked = "checked",
                onchange = "selectExcNextDayMode('all')"
              ),
              "Apply to All"
            ),
            tags$label(
              class = "exc-nd-radio-lbl",
              tags$input(
                type = "radio", name = "exc_nd_mode", id = "exc_nd_mode_cutoff", value = "cutoff",
                onchange = "selectExcNextDayMode('cutoff')"
              ),
              "After Cutoff (Accepted Time >=):"
            ),
            tags$input(
              id = "exc_nd_cutoff_val",
              type = "text",
              value = "15:00",
              placeholder = "hh:mm",
              disabled = "disabled",
              onfocus = "selectExcNextDayMode('cutoff')",
              oninput = "selectExcNextDayMode('cutoff')",
              style = "width: 72px; padding: 2px 6px; border: 1px solid #6f42c1; border-radius: 4px; font-weight: bold; text-align: center;"
            ),
            tags$small(style = "color: #6c757d;", "(24-hr hh:mm)")
          )
        ),

        tags$label(
          class = "exc-option-row",
          style = "border-left: 5px solid #d97706;",
          div(
            style = "display: flex; align-items: center;",
            tags$input(type = "checkbox", class = "exc-rule-cb", id = "exc_rule_cb_3", value = "3", onchange = "syncExcRules()"),
            span(class = "exc-num-circle", style = "color: #d97706; background: #fef3c7; border: 1.5px solid #d97706;", "3"),
            div(
              style = "display: flex; align-items: center; gap: 8px;",
              strong("Extend Hours:", style = "color: #d97706; font-size: 14px;"),
              tags$input(
                id = "exc_extend_hrs_val",
                type = "number",
                value = "12",
                min = "1",
                step = "1",
                onclick = "event.stopPropagation();",
                oninput = "document.getElementById('exc_rule_cb_3').checked = true; syncExcRules(); Shiny.setInputValue('exc_extend_hrs_input', this.value);",
                style = "width: 72px; padding: 2px 6px; border: 1px solid #d97706; border-radius: 4px; font-weight: bold; text-align: center;"
              )
            )
          )
        ),

        tags$label(
          class = "exc-option-row",
          style = "border-left: 5px solid #198754;",
          div(
            style = "display: flex; align-items: center;",
            tags$input(type = "checkbox", class = "exc-rule-cb", value = "4", onchange = "syncExcRules()"),
            span(class = "exc-num-circle", style = "color: #198754; background: #d1e7dd; border: 1.5px solid #198754;", "4"),
            div(
              strong("Sure Pass", style = "color: #198754; font-size: 14px;"),
              tags$div(style = "font-size: 12px; color: #666;", "Force override selected specimen(s) directly to PASS")
            )
          )
        )
      ),

      div(
        class = "exc-modal-section exc-scope-box",
        style = "margin-bottom: 0;",
        div(class = "exc-section-title", "2. Select Target Outlier Rows (Multi-Select)"),
        checkboxGroupInput(
          inputId  = "exc_scope_selected",
          label    = NULL,
          choices  = scope_choices,
          selected = default_scope,
          width    = "100%"
        ),
        conditionalPanel(
          condition = "input.exc_scope_selected && input.exc_scope_selected.indexOf('spec_tests') !== -1",
          div(
            style = "margin-top: 8px; padding: 10px; background: #fff; border: 1px solid #ced4da; border-radius: 5px;",
            selectizeInput(
              inputId  = "exc_test_picker",
              label    = "Add Test from Specification (Test Name — LTAT):",
              choices  = c("Search and select a test..." = "", spec_choices),
              selected = "",
              multiple = FALSE,
              width    = "100%",
              options  = list(dropdownParent = "body")
            ),
            uiOutput("exc_selected_tests_ui")
          )
        )
      ),

      easyClose = TRUE,
      footer = tagList(
        actionButton("clear_outlier_exceptions_btn", "Clear Exceptions on Target Scope", class = "btn-outline-danger"),
        modalButton("Cancel"),
        actionButton("apply_outlier_exceptions_btn", "Apply Selected Rules", class = "btn-primary", style = "font-weight: bold;")
      )
    ))

    session$sendCustomMessage("evalResetExcRules", list())
  })

  refresh_exc_test_picker <- function() {
    remaining <- rv$modal_spec_choices[!(rv$modal_spec_choices %in% rv$modal_selected_tests)]
    updateSelectizeInput(
      session,
      "exc_test_picker",
      choices  = c("Search and select a test..." = "", remaining),
      selected = ""
    )
  }

  observeEvent(input$exc_test_picker, {
    val <- input$exc_test_picker
    req(!is.null(val), nzchar(val))
    if (!(val %in% rv$modal_selected_tests)) {
      rv$modal_selected_tests <- c(rv$modal_selected_tests, val)
    }
    refresh_exc_test_picker()
  })

  observeEvent(input$exc_remove_test, {
    val <- input$exc_remove_test$key
    req(!is.null(val), nzchar(val))
    rv$modal_selected_tests <- setdiff(rv$modal_selected_tests, val)
    refresh_exc_test_picker()
  })

  observeEvent(input$exc_select_all_tests, {
    rv$modal_selected_tests <- unname(rv$modal_spec_choices)
    refresh_exc_test_picker()
  })

  observeEvent(input$exc_clear_all_tests, {
    rv$modal_selected_tests <- character(0)
    refresh_exc_test_picker()
  })

  output$exc_selected_tests_ui <- renderUI({
    sel   <- rv$modal_selected_tests
    n_sel <- length(sel)
    n_tot <- length(rv$modal_spec_choices)
    show_select_all <- (n_tot > 0 && n_sel < n_tot)
    show_clear_all  <- (n_sel > 0)

    actions_ui <- div(
      class = "exc-header-actions",
      if (show_select_all) {
        tags$a(
          class = "exc-select-all-link",
          onclick = "Shiny.setInputValue('exc_select_all_tests', Math.random(), {priority: 'event'});",
          "Select All"
        )
      },
      if (show_select_all && show_clear_all) {
        tags$span(class = "exc-action-sep", "|")
      },
      if (show_clear_all) {
        tags$a(
          class = "exc-clear-link",
          onclick = "Shiny.setInputValue('exc_clear_all_tests', Math.random(), {priority: 'event'});",
          "Clear All"
        )
      }
    )

    header_ui <- div(
      class = "exc-selected-header",
      tags$strong(sprintf("Selected Tests (%d)", n_sel)),
      actions_ui
    )

    if (n_sel == 0) {
      return(tagList(
        header_ui,
        div(
          class = "exc-selected-list",
          div(style = "padding: 10px; color: #888; font-style: italic; font-size: 12px;", "No tests selected yet.")
        )
      ))
    }

    label_lookup <- setNames(names(rv$modal_spec_choices), unname(rv$modal_spec_choices))

    rows_ui <- lapply(sel, function(val) {
      disp_lbl <- unname(label_lookup[val])
      if (is.na(disp_lbl)) disp_lbl <- val
      encoded_val <- URLencode(val, reserved = TRUE)
      div(
        class = "exc-selected-row",
        tags$span(disp_lbl),
        tags$button(
          type    = "button",
          class   = "exc-remove-btn",
          title   = "Remove test",
          onclick = sprintf(
            "Shiny.setInputValue('exc_remove_test', { key: decodeURIComponent('%s'), nonce: Math.random() }, { priority: 'event' });",
            encoded_val
          ),
          HTML("&times;")
        )
      )
    })

    tagList(
      header_ui,
      div(class = "exc-selected-list", rows_ui)
    )
  })

  observeEvent(input$apply_outlier_exceptions_btn, {
    req(rv$is_processed, rv$processed_df)

    rules  <- input$exc_rules_selected
    scopes <- input$exc_scope_selected

    if (is.null(rules) || length(rules) == 0) {
      showNotification("Please check at least one exception rule in Section 1.", type = "warning")
      return()
    }
    if (is.null(scopes) || length(scopes) == 0) {
      showNotification("Please select at least one target scope in Section 2.", type = "warning")
      return()
    }
    if ("spec_tests" %in% scopes && length(rv$modal_selected_tests) == 0 && length(scopes) == 1) {
      showNotification("Please select at least one test from the Specification list.", type = "warning")
      return()
    }

    nd_mode <- if (is.null(input$exc_nd_mode_input)) "all" else input$exc_nd_mode_input
    nd_cutoff_fmt <- ""
    cutoff_sec <- 0
    if ("2" %in% rules && identical(nd_mode, "cutoff")) {
      nd_raw <- str_trim(ifelse(is.null(input$exc_nd_cutoff_input), "", as.character(input$exc_nd_cutoff_input)))
      hhmm_regex <- "^([01]?[0-9]|2[0-3]):[0-5][0-9]$"
      if (!str_detect(nd_raw, hhmm_regex)) {
        showNotification("Rule 2 cutoff time must be a valid 24-hour time in hh:mm format (e.g. 15:00).", type = "error")
        return()
      }
      parts <- as.integer(str_split(nd_raw, ":")[[1]])
      nd_cutoff_fmt <- sprintf("%02d:%02d", parts[1], parts[2])
      cutoff_sec <- (parts[1] * 3600L) + (parts[2] * 60L)
    }

    target_keys <- resolve_modal_target_keys(scopes, rv$modal_selected_tests)
    if (length(target_keys) == 0) {
      showNotification("No outlier rows match the selected target scope.", type = "warning")
      return()
    }

    ext_hrs <- suppressWarnings(as.numeric(input$exc_extend_hrs_input))
    if (is.null(ext_hrs) || length(ext_hrs) == 0 || is.na(ext_hrs) || ext_hrs < 0) ext_hrs <- 12

    df  <- rv$processed_df
    idx <- which(df$.row_key %in% target_keys)
    applied_idx <- integer(0)

    if (length(idx) > 0) {
      for (i in idx) {
        row_rules <- rules
        if ("2" %in% row_rules && identical(nd_mode, "cutoff")) {
          dt_i <- df$dt_unit[i]
          meets_cutoff <- !is.na(dt_i) &&
            ((hour(dt_i) * 3600L + minute(dt_i) * 60L + as.integer(second(dt_i))) >= cutoff_sec)
          if (!meets_cutoff) {
            row_rules <- setdiff(row_rules, "2")
          }
        }
        if (length(row_rules) == 0) next

        applied_idx <- c(applied_idx, i)
        cur <- if (!is.na(df$exc_codes[i]) && nzchar(df$exc_codes[i])) str_split(df$exc_codes[i], ",")[[1]] else character(0)
        df$exc_codes[i] <- paste(sort(unique(c(cur, row_rules))), collapse = ",")
        if ("2" %in% row_rules) {
          df$exc_nextday_cutoff[i] <- if (identical(nd_mode, "cutoff")) nd_cutoff_fmt else ""
        }
        if ("3" %in% row_rules) {
          df$exc_ext_hrs[i] <- ext_hrs
        }
      }

      if (length(applied_idx) == 0) {
        showNotification(
          sprintf("No outlier rows in the target scope had Accepted Time >= %s.", nd_cutoff_fmt),
          type = "warning"
        )
        return()
      }

      df$Status[applied_idx] <- evaluate_ltat_status(
        df$dt_unit[applied_idx], df$dt_valid[applied_idx], df$unit_tat_hrs[applied_idx],
        df$Rule_Type[applied_idx], df$Dur_DD[applied_idx], df$Dur_HH[applied_idx], df$Dur_MM[applied_idx],
        df$Cutoff_HHMM[applied_idx], df$Target_HHMM[applied_idx], df$Skip_Days[applied_idx],
        df$exc_codes[applied_idx], df$exc_ext_hrs[applied_idx]
      )
      rv$processed_df <- df
    }

    removeModal()
    showNotification(
      sprintf("Applied rule(s) %s to %d outlier row(s).", paste0("#", rules, collapse = ", "), length(applied_idx)),
      type = "message"
    )
  })

  observeEvent(input$clear_outlier_exceptions_btn, {
    req(rv$is_processed, rv$processed_df)
    target_keys <- resolve_modal_target_keys(input$exc_scope_selected, rv$modal_selected_tests)

    if (length(target_keys) == 0) {
      showNotification("No outlier rows match the selected target scope.", type = "warning")
      return()
    }

    df  <- rv$processed_df
    idx <- which(df$.row_key %in% target_keys)

    if (length(idx) > 0) {
      df$exc_codes[idx]          <- ""
      df$exc_ext_hrs[idx]        <- 0
      df$exc_nextday_cutoff[idx] <- ""
      df$Status[idx]             <- df$orig_status[idx]
      rv$processed_df            <- df
    }

    removeModal()
    showNotification(sprintf("Cleared exceptions on %d outlier row(s).", length(idx)), type = "warning")
  })
  
  output$export_xlsx_btn_ui <- renderUI({
    req(rv$is_processed)
    actionButton("download_xlsx", "Export Excel", icon = icon("file-excel"), class = "btn-success")
  })
  
  output$kpi_cards_ui <- renderUI({
    req(rv$is_processed)
    fluidRow(
      column(3, wellPanel(style = "padding: 10px;", h5("Total Specimens"), strong(textOutput("kpi_total")))),
      column(3, wellPanel(style = "padding: 10px;", h5("Within LTAT (PASS)"), strong(textOutput("kpi_pass")))),
      column(3, wellPanel(style = "padding: 10px;", h5("Exceeded LTAT (FAIL)"), strong(textOutput("kpi_fail")))),
      column(3, wellPanel(style = "padding: 10px;", h5("Overall Compliance"), strong(textOutput("kpi_pct"))))
    )
  })
  
  # ============================================================================
  # UNIFIED SPECIFICATION MAP EXPORT
  # ============================================================================
  
  observeEvent(input$download_spec_map, {
    req(nrow(rv$catalog) > 0)
    merged_out <- rv$catalog %>%
      left_join(rv$mapping, by = "LTAT_Test_Name") %>%
      mutate(Extract_Key = ifelse(is.na(Extract_Key), "", Extract_Key)) %>%
      as.data.frame()
    
    tc <- textConnection("csv_str", "w", local = TRUE)
    write.csv(merged_out, tc, row.names = FALSE)
    close(tc)
    
    session$sendCustomMessage("downloadTextFile", list(
      filename = paste0("LTAT_Specification_Map_", Sys.Date(), ".csv"),
      content  = paste(csv_str, collapse = "\n"),
      mime     = "text/csv;charset=utf-8;"
    ))
  })
  
  # ============================================================================
  # NESTED TAB 2A: TEST MAPPING LOGIC
  # ============================================================================
  
  output$ltat_master_table <- renderReactable({
    req(nrow(rv$catalog) > 0)
    tbl <- rv$catalog %>% select(`Test Name with LTAT` = LTAT_Test_Name, `LTAT Display` = LTAT_Display)
    tbl_map_master$render(tbl, selected = isolate(rv$map_sel_idx))
  })
  
  observeEvent(getReactableState("ltat_master_table", "selected"), {
    idx <- getReactableState("ltat_master_table", "selected")
    if (!is.null(idx) && length(idx) > 0 && !is.na(idx[1]) && idx[1] <= nrow(rv$catalog)) {
      rv$map_sel_idx <- idx[1]
    }
    refresh_mapping_lists()
  }, ignoreNULL = FALSE)
  
  selected_ltat_row <- reactive({
    req(nrow(rv$catalog) > 0)
    idx <- min(max(1L, rv$map_sel_idx), nrow(rv$catalog))
    rv$catalog[idx, ]
  })
  
  output$selected_ltat_badge <- renderUI({
    req(nrow(rv$catalog) > 0)
    row <- selected_ltat_row()
    rule_desc <- format_rule_summary(
      row$Rule_Type, row$Dur_DD, row$Dur_HH, row$Dur_MM,
      row$Skip_Days, row$Skip_Holidays, row$Cutoff_HHMM, row$Target_HHMM
    )
    tags$div(
      style = "background: #e7f1ff; border-left: 4px solid #0d6efd; padding: 5px 8px; margin-bottom: 6px; font-size: 12px;",
      strong(row$LTAT_Test_Name), tags$br(),
      span("Display: ", strong(row$LTAT_Display)), tags$br(),
      span("Rule: ", rule_desc)
    )
  })
  
  observeEvent(input$btn_check_unmapped, {
    if (!rv$has_extract || is.null(rv$raw_df)) {
      showModal(modalDialog(
        title = "Extract File Required",
        "Please upload the Extract File (.xlsx) in the 'All Specimen Records' tab first so unmapped extract tests can be checked.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    refresh_mapping_lists()
    unmapped_keys <- get_extract_unmapped_keys()
    
    if (length(unmapped_keys) == 0) {
      showModal(modalDialog(
        title = "All Extract Tests Mapped",
        tags$div(
          style = "color: #155724; background: #d4edda; border: 1px solid #c3e6cb; padding: 10px; border-radius: 4px;",
          strong("✔ 100% Mapped: "), "Every TestName | Specimen Type combination in the uploaded Extract File is mapped."
        ),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    } else {
      counts_df <- rv$raw_df %>%
        filter(Extract_Key %in% unmapped_keys) %>%
        count(Extract_Key, name = "Specimens") %>%
        arrange(desc(Specimens), Extract_Key)
      
      summary_lines <- paste0("• ", counts_df$Extract_Key, "  (", counts_df$Specimens, " specimen", ifelse(counts_df$Specimens == 1, "", "s"), ")")
      
      showModal(modalDialog(
        title = paste0("Unmapped Extract Tests Found (", length(unmapped_keys), ")"),
        tags$p("The following ", strong(length(unmapped_keys)), " test(s) from the Extract File are currently unmapped (also listed in '3. Unmapped Extract Tests'):"),
        tags$pre(
          style = "max-height: 280px; overflow-y: auto; background: #fff3cd; color: #664d03; border: 1px solid #ffecb5; padding: 10px; font-size: 12px;",
          paste(summary_lines, collapse = "\n")
        ),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
  })
  
  observeEvent(input$btn_add_map, {
    req(input$unmapped_list, nrow(rv$catalog) > 0)
    sel_name <- selected_ltat_row()$LTAT_Test_Name
    new_rows <- tibble::tibble(
      Extract_Key    = input$unmapped_list,
      LTAT_Test_Name = sel_name
    )
    rv$mapping <- bind_rows(
      rv$mapping %>% filter(!(Extract_Key %in% input$unmapped_list)),
      new_rows
    )
    mark_spec_changed()
    refresh_mapping_lists()
  })
  
  observeEvent(input$btn_remove_map, {
    req(input$mapped_list)
    rv$mapping <- rv$mapping %>%
      filter(!(Extract_Key %in% input$mapped_list))
    mark_spec_changed()
    refresh_mapping_lists()
  })
  
  # ============================================================================
  # NESTED TAB 2B: LTAT EDITOR LOGIC
  # ============================================================================
  
  read_editor_form <- function() {
    nm   <- str_trim(input$cat_edit_name)
    disp <- str_trim(input$cat_edit_display)
    rt   <- input$cat_rule_type
    
    if (nm == "" || disp == "") {
      return(list(error = "Both 'Test Name with LTAT' and 'LTAT Display' are required."))
    }
    
    if (rt == "Duration") {
      dd <- suppressWarnings(as.integer(input$cat_dur_dd))
      hh <- suppressWarnings(as.integer(input$cat_dur_hh))
      mm <- suppressWarnings(as.integer(input$cat_dur_mm))
      if (any(is.na(c(dd, hh, mm))) || any(c(dd, hh, mm) < 0) ||
          any(c(input$cat_dur_dd, input$cat_dur_hh, input$cat_dur_mm) %% 1 != 0)) {
        return(list(error = "Duration dd, hh, and mm must be non-negative whole integers."))
      }
      if ((dd + hh + mm) <= 0) {
        return(list(error = "Duration must be greater than 0 (set at least one of dd, hh, or mm)."))
      }
      sd <- paste(input$cat_skip_days, collapse = ",")
      sh <- isTRUE(input$cat_skip_holidays)
      return(list(row = tibble::tibble(
        LTAT_Test_Name = nm, LTAT_Display = disp, Rule_Type = "Duration",
        Dur_DD = dd, Dur_HH = hh, Dur_MM = mm,
        Skip_Days = sd, Skip_Holidays = sh,
        Cutoff_HHMM = "", Target_HHMM = ""
      )))
      
    } else if (rt == "Same Day") {
      return(list(row = tibble::tibble(
        LTAT_Test_Name = nm, LTAT_Display = disp, Rule_Type = "Same Day",
        Dur_DD = 0L, Dur_HH = 0L, Dur_MM = 0L,
        Skip_Days = "", Skip_Holidays = FALSE,
        Cutoff_HHMM = "", Target_HHMM = ""
      )))
      
    } else if (rt == "Next Day") {
      return(list(row = tibble::tibble(
        LTAT_Test_Name = nm, LTAT_Display = disp, Rule_Type = "Next Day",
        Dur_DD = 0L, Dur_HH = 0L, Dur_MM = 0L,
        Skip_Days = "", Skip_Holidays = FALSE,
        Cutoff_HHMM = "", Target_HHMM = ""
      )))
      
    } else if (rt == "Next Day Cutoff") {
      chm <- str_trim(input$cat_cutoff_hhmm)
      thm <- str_trim(input$cat_target_hhmm)
      hhmm_regex <- "^([01]?[0-9]|2[0-3]):[0-5][0-9]$"
      if (!str_detect(chm, hhmm_regex) || !str_detect(thm, hhmm_regex)) {
        return(list(error = "Both Cut-off Hour and Next-Day Target Hour must be valid 24-hour times in hh:mm format (e.g., 12:00, 17:30)."))
      }
      return(list(row = tibble::tibble(
        LTAT_Test_Name = nm, LTAT_Display = disp, Rule_Type = "Next Day Cutoff",
        Dur_DD = 0L, Dur_HH = 0L, Dur_MM = 0L,
        Skip_Days = "", Skip_Holidays = FALSE,
        Cutoff_HHMM = chm, Target_HHMM = thm
      )))
    }
  }
  
  output$catalog_editor_table <- renderReactable({
    req(nrow(rv$catalog) > 0)
    tbl <- rv$catalog %>%
      mutate(Rule_Summary = format_rule_summary(Rule_Type, Dur_DD, Dur_HH, Dur_MM, Skip_Days, Skip_Holidays, Cutoff_HHMM, Target_HHMM)) %>%
      select(`Test Name with LTAT` = LTAT_Test_Name, `LTAT Display` = LTAT_Display, `Rule Type` = Rule_Type, `Rule Details` = Rule_Summary)
    
    tbl_cat_editor$render(tbl, selected = isolate(rv$cat_sel_idx))
  })
  
  observeEvent(getReactableState("catalog_editor_table", "selected"), {
    idx <- getReactableState("catalog_editor_table", "selected")
    if (is.null(idx) || length(idx) == 0) return()
    if (idx[1] <= nrow(rv$catalog)) {
      rv$cat_sel_idx <- idx[1]
      populate_editor_fields(idx[1])
    }
  }, ignoreNULL = FALSE)
  
  commit_catalog_save <- function(idx, old_name, new_row) {
    rv$catalog[idx, ] <- new_row
    if (old_name != new_row$LTAT_Test_Name) {
      rv$mapping <- rv$mapping %>%
        mutate(LTAT_Test_Name = ifelse(LTAT_Test_Name == old_name, new_row$LTAT_Test_Name, LTAT_Test_Name))
    }
    mark_spec_changed()
    refresh_mapping_lists()
    showNotification("LTAT rule updated.", type = "message")
  }
  
  observeEvent(input$cat_btn_save, {
    req(nrow(rv$catalog) > 0)
    idx <- rv$cat_sel_idx
    if (is.null(idx) || idx < 1 || idx > nrow(rv$catalog)) return()
    
    parsed <- read_editor_form()
    if (!is.null(parsed$error)) {
      showModal(modalDialog(title = "Validation Error", parsed$error, easyClose = TRUE))
      return()
    }
    new_row  <- parsed$row
    old_name <- rv$catalog$LTAT_Test_Name[idx]
    
    if (new_row$LTAT_Test_Name != old_name && new_row$LTAT_Test_Name %in% rv$catalog$LTAT_Test_Name) {
      showModal(modalDialog(title = "Duplicate Test Name", "Another LTAT row already uses this Test Name.", easyClose = TRUE))
      return()
    }
    
    affected_tests <- rv$mapping %>%
      filter(LTAT_Test_Name == old_name) %>%
      pull(Extract_Key)
    
    if (length(affected_tests) > 0) {
      rv$pending_edit <- list(idx = idx, old_name = old_name, new_row = new_row)
      rule_desc <- format_rule_summary(
        new_row$Rule_Type, new_row$Dur_DD, new_row$Dur_HH, new_row$Dur_MM,
        new_row$Skip_Days, new_row$Skip_Holidays, new_row$Cutoff_HHMM, new_row$Target_HHMM
      )
      showModal(modalDialog(
        title = "Confirm LTAT Change (Mapped Tests Affected)",
        tags$p(
          "Updating ", strong(old_name), " → ", strong(new_row$LTAT_Test_Name),
          " (", strong(new_row$LTAT_Display), " | ", rule_desc, ") ",
          "will affect the following ", strong(length(affected_tests)), " mapped extract test(s):"
        ),
        tags$pre(
          style = "max-height: 200px; overflow-y: auto; background: #fff3cd; border: 1px solid #ffeeba; padding: 8px;",
          paste(paste0("• ", affected_tests), collapse = "\n")
        ),
        tags$p("Do you want to accept this change?"),
        footer = tagList(
          actionButton("confirm_cat_save", "Accept", class = "btn-primary"),
          modalButton("Cancel")
        )
      ))
    } else {
      commit_catalog_save(idx, old_name, new_row)
    }
  })
  
  observeEvent(input$confirm_cat_save, {
    req(rv$pending_edit)
    pe <- rv$pending_edit
    commit_catalog_save(pe$idx, pe$old_name, pe$new_row)
    rv$pending_edit <- NULL
    removeModal()
  })
  
  observeEvent(input$cat_btn_add, {
    parsed <- read_editor_form()
    if (!is.null(parsed$error)) {
      showModal(modalDialog(title = "Validation Error", parsed$error, easyClose = TRUE))
      return()
    }
    new_row <- parsed$row
    if (new_row$LTAT_Test_Name %in% rv$catalog$LTAT_Test_Name) {
      showModal(modalDialog(title = "Duplicate Entry", "This Test Name already exists. Use Save to update it.", easyClose = TRUE))
      return()
    }
    rv$catalog      <- bind_rows(rv$catalog, new_row)
    rv$has_spec_map <- TRUE
    rv$cat_sel_idx  <- nrow(rv$catalog)
    updateReactable("catalog_editor_table", selected = rv$cat_sel_idx)
    mark_spec_changed()
    refresh_mapping_lists()
    showNotification(paste("Added:", new_row$LTAT_Test_Name), type = "message")
  })
  
  commit_catalog_delete <- function(idx, target_name) {
    rv$catalog <- rv$catalog[-idx, , drop = FALSE]
    rv$mapping <- rv$mapping %>% filter(LTAT_Test_Name != target_name)
    
    if (nrow(rv$catalog) > 0) {
      new_idx <- min(idx, nrow(rv$catalog))
      rv$cat_sel_idx <- new_idx
      rv$map_sel_idx <- min(rv$map_sel_idx, nrow(rv$catalog))
      updateReactable("catalog_editor_table", selected = new_idx)
      populate_editor_fields(new_idx)
    } else {
      rv$has_spec_map <- FALSE
      rv$cat_sel_idx  <- 1L
      rv$map_sel_idx  <- 1L
      updateTextInput(session, "cat_edit_name", value = "")
      updateTextInput(session, "cat_edit_display", value = "")
    }
    mark_spec_changed()
    refresh_mapping_lists()
    showNotification(paste("Deleted:", target_name), type = "warning")
  }
  
  observeEvent(input$cat_btn_delete, {
    req(nrow(rv$catalog) > 0)
    idx <- rv$cat_sel_idx
    if (is.null(idx) || idx < 1 || idx > nrow(rv$catalog)) return()
    
    target_name <- rv$catalog$LTAT_Test_Name[idx]
    affected_tests <- rv$mapping %>%
      filter(LTAT_Test_Name == target_name) %>%
      pull(Extract_Key)
    
    rv$pending_delete <- list(idx = idx, target_name = target_name)
    
    affected_ui <- if (length(affected_tests) > 0) {
      tagList(
        tags$p(strong(length(affected_tests)), " mapped extract test(s) will become unmapped (Status = N/A):"),
        tags$pre(
          style = "max-height: 200px; overflow-y: auto; background: #f8d7da; color: #842029; border: 1px solid #f5c2c7; padding: 8px;",
          paste(paste0("• ", affected_tests), collapse = "\n")
        )
      )
    } else {
      tags$p("No extract tests are currently mapped to this LTAT entry.")
    }
    
    showModal(modalDialog(
      title = "Confirm LTAT Deletion",
      tags$p("Are you sure you want to delete ", strong(target_name), " from the LTAT list?"),
      affected_ui,
      footer = tagList(
        actionButton("confirm_cat_delete", "Accept Delete", class = "btn-danger"),
        modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$confirm_cat_delete, {
    req(rv$pending_delete)
    pd <- rv$pending_delete
    commit_catalog_delete(pd$idx, pd$target_name)
    rv$pending_delete <- NULL
    removeModal()
  })
  
  # ============================================================================
  # FROZEN PROCESSED DATA & DASHBOARD TABLES
  # ============================================================================
  
  raw_preview_df <- reactive({
    req(rv$has_extract, rv$raw_df)
    df <- rv$raw_df %>% select(-any_of(c("t_clean", "s_clean", "Extract_Key")))
    names(df) <- format_col_title(names(df))
    df
  })
  
  proc_extract_base_df <- reactive({
    req(rv$is_processed, rv$processed_df)
    df <- rv$processed_df %>%
      mutate(
        .exc_sup = ifelse(
          Status == "PASS" & nzchar(exc_codes),
          format_exception_badges_html(exc_codes, exc_ext_hrs, exc_nextday_cutoff, superscript = TRUE),
          ""
        )
      ) %>%
      select(`Lab. No`, `Ward/Clinic`, Mapped_LTAT_Test, TestName, `Specimen Type`, 
             `Accepted Date (Unit)`, `Validation Date`, `Unit TAT Hrs` = unit_tat_hrs, LTAT_Target, Status, .exc_sup)
    names(df)[names(df) != ".exc_sup"] <- format_col_title(names(df)[names(df) != ".exc_sup"])
    df
  })
  
  summary_base_df <- reactive({
    req(rv$is_processed, rv$processed_df)
    df <- rv$processed_df %>%
      group_by(Mapped_LTAT_Test, TestName, `Specimen Type`, LTAT_Target) %>%
      summarise(
        Total          = n(),
        Within_LTAT    = sum(Status == "PASS"),
        Exceeded_LTAT  = sum(Status == "FAIL"),
        Compliance_Pct = if (first(LTAT_Target) == "N/A" || (Within_LTAT + Exceeded_LTAT) == 0) "N/A"
                         else paste0(round(100 * Within_LTAT / (Within_LTAT + Exceeded_LTAT), 1), "%"),
        .groups = "drop"
      ) %>%
      arrange(desc(Total))
    names(df) <- format_col_title(names(df))
    df
  })
  
  outliers_base_df <- reactive({
    req(rv$is_processed, rv$processed_df)
    df <- rv$processed_df %>%
      filter(orig_status == "FAIL") %>%
      mutate(
        .exc_norm = format_exception_badges_html(exc_codes, exc_ext_hrs, exc_nextday_cutoff, superscript = FALSE)
      ) %>%
      select(`Lab. No`, `Ward/Clinic`, Mapped_LTAT_Test, TestName, `Specimen Type`, 
             `Accepted Date (Unit)`, `Validation Date`, `Unit TAT Hrs` = unit_tat_hrs, LTAT_Target, .row_key, .exc_norm)
    idx_rename <- !(names(df) %in% c(".row_key", ".exc_norm"))
    names(df)[idx_rename] <- format_col_title(names(df)[idx_rename])
    df
  })
  
  # Preload unfiltered values & initialize bitmask state when base data changes
  observe({
    req(rv$has_extract, rv$raw_df)
    tbl_all$sync_filter_values(session, raw_preview_df())
  })
  
  observe({
    req(rv$is_processed, rv$processed_df)
    tbl_proc_ext$sync_filter_values(session, proc_extract_base_df(), ignore_cols = c(".exc_sup"))
    tbl_summary$sync_filter_values(session, summary_base_df())
    tbl_outliers$sync_filter_values(session, outliers_base_df(), ignore_cols = c(".row_key", ".exc_norm"))
  })

  # Lazy on-click cascading filter lookup (single column via bitmask "exclude self")
  observeEvent(input$excel_request_col_values, {
    req_info <- input$excel_request_col_values
    req(req_info$table, req_info$col)

    tbl_flt <- req_info$filters

    vals <- switch(
      req_info$table,
      "all" = {
        if (rv$has_extract && !is.null(rv$raw_df)) {
          tbl_all$get_col_values(raw_preview_df(), req_info$col, tbl_flt)
        } else I(character(0))
      },
      "proc_ext" = {
        if (rv$is_processed && !is.null(rv$processed_df)) {
          tbl_proc_ext$get_col_values(proc_extract_base_df(), req_info$col, tbl_flt)
        } else I(character(0))
      },
      "summary" = {
        if (rv$is_processed && !is.null(rv$processed_df)) {
          tbl_summary$get_col_values(summary_base_df(), req_info$col, tbl_flt)
        } else I(character(0))
      },
      "outliers" = {
        if (rv$is_processed && !is.null(rv$processed_df)) {
          tbl_outliers$get_col_values(outliers_base_df(), req_info$col, tbl_flt)
        } else I(character(0))
      },
      I(character(0))
    )

    session$sendCustomMessage("receiveColValues", list(
      table  = req_info$table,
      col    = req_info$col,
      reqId  = as.integer(req_info$reqId),
      values = vals
    ))
  })
  
  filtered_processed_extract <- reactive({
    req(rv$is_processed, rv$processed_df)
    tbl_proc_ext$apply_state(proc_extract_base_df(), input$excel_active_filters, input$excel_sort)
  })
  
  output$kpi_total <- renderText({ if (!rv$is_processed) "-" else nrow(filtered_processed_extract()) })
  output$kpi_pass  <- renderText({ if (!rv$is_processed) "-" else sum(filtered_processed_extract()$Status == "PASS", na.rm = TRUE) })
  output$kpi_fail  <- renderText({ if (!rv$is_processed) "-" else sum(filtered_processed_extract()$Status == "FAIL", na.rm = TRUE) })
  output$kpi_pct   <- renderText({
    if (!rv$is_processed) return("-")
    d <- filtered_processed_extract()
    denom <- sum(d$Status %in% c("PASS", "FAIL"))
    if (denom == 0) return("N/A")
    paste0(round(100 * sum(d$Status == "PASS") / denom, 1), "%")
  })
  
  # Tab 1: Raw Extract Only
  output$all_table <- renderReactable({
    req(rv$has_extract, rv$raw_df)
    tbl <- tbl_all$apply_state(raw_preview_df(), input$excel_active_filters, input$excel_sort)
    tbl_all$render(tbl, output_id = "all_table")
  })
  
  # Tab 3 (Nested 1): Processed Extract
  output$processed_extract_table <- renderReactable({
    req(rv$is_processed, rv$processed_df)
    tbl <- filtered_processed_extract()
    
    tbl$Status <- ifelse(
      tbl$Status == "PASS" & nzchar(tbl$.exc_sup),
      paste0("PASS ", tbl$.exc_sup),
      tbl$Status
    )
    tbl$.exc_sup <- NULL
    
    status_override <- list(
      Status = colDef(
        header = excel_header("proc_ext", "Status"),
        html   = TRUE
      )
    )
    
    tbl_proc_ext$render(tbl, override_cols = status_override, output_id = "processed_extract_table")
  })
  
  # Tab 3 (Nested 2): LTAT Report
  output$summary_table <- renderReactable({
    req(rv$is_processed, rv$processed_df)
    tbl <- tbl_summary$apply_state(summary_base_df(), input$excel_active_filters, input$excel_sort)
    tbl_summary$render(tbl, output_id = "summary_table")
  })
  
  # Tab 3 (Nested 3): Outliers / Failed Specimens
  output$outliers_table <- renderReactable({
    req(rv$is_processed, rv$processed_df)
    tbl <- tbl_outliers$apply_state(outliers_base_df(), input$excel_active_filters, input$excel_sort)
    checked_now <- isolate(rv$outlier_checked)
    
    tbl$Exception <- tbl$.exc_norm
    
    tbl$Select <- vapply(tbl$.row_key, function(k) {
      chk_attr <- if (k %in% checked_now) "checked" else ""
      sprintf(
        '<input type="checkbox" class="outlier-row-cb" data-key="%s" %s onclick="event.stopPropagation(); toggleOutlierRow(this);" />',
        URLencode(k, reserved = TRUE),
        chk_attr
      )
    }, character(1))
    
    tbl$.row_key  <- NULL
    tbl$.exc_norm <- NULL
    
    extra_cols_def <- list(
      Exception = colDef(
        name = "Exception",
        header = HTML(paste0(
          '<div style="display: flex; justify-content: center; align-items: center;">',
          '<button type="button" class="exc-header-btn" title="Configure Outlier Rule Exceptions" ',
          'onclick="event.stopPropagation(); openOutlierExceptionDialog();">',
          as.character(rule_exception_icon_svg()),
          '</button></div>'
        )),
        html     = TRUE,
        sortable = FALSE,
        align    = "center",
        width    = 85
      ),
      Select = colDef(
        name   = "Select",
        header = function(value) {
          tags$div(
            style = "display: flex; justify-content: center; align-items: center;",
            tags$input(
              id      = "outlier_select_all_cb",
              type    = "checkbox",
              title   = "Select / Unselect all visible rows",
              onclick = "event.stopPropagation(); toggleAllVisibleOutliers(this);"
            )
          )
        },
        html     = TRUE,
        sortable = FALSE,
        align    = "center",
        width    = 60
      )
    )
    
    checkbox_sync_js <- "
      if (window.syncOutlierDOMCheckboxes) syncOutlierDOMCheckboxes();
      var observer = new MutationObserver(function() {
        if (window.syncOutlierDOMCheckboxes) syncOutlierDOMCheckboxes();
      });
      observer.observe(el, { childList: true, subtree: true });
    "
    
    tbl_outliers$render(
      tbl,
      extra_cols = extra_cols_def,
      output_id  = "outliers_table",
      extra_js   = checkbox_sync_js
    )
  })
  
  observeEvent(input$download_xlsx, {
    req(rv$is_processed, rv$processed_df)
    export_df <- filtered_processed_extract() %>% select(-any_of(".exc_sup"))
    tmp <- tempfile(fileext = ".xlsx")
    write_xlsx(export_df, tmp)
    raw_bytes <- readBin(tmp, "raw", file.info(tmp)$size)
    unlink(tmp)

    session$sendCustomMessage("downloadBase64File", list(
      filename = paste0("LTAT_Filtered_", Sys.Date(), ".xlsx"),
      base64   = jsonlite::base64_enc(raw_bytes),
      mime     = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    ))
  })
}

shinyApp(ui, server)