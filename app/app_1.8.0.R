library(shiny)
library(R6)
library(readxl)
library(writexl)
library(reactable)
library(htmltools)

# --- Helper: Parse Multi-Format Timestamps in Base R ---
parse_multi_strptime <- function(chr_vec, fmts, tz = "UTC") {
  out <- rep(as.POSIXct(NA, origin = "1970-01-01", tz = tz), length(chr_vec))
  rem <- seq_along(chr_vec)
  for (fmt in fmts) {
    if (length(rem) == 0) break
    parsed <- suppressWarnings(as.POSIXct(strptime(chr_vec[rem], format = fmt, tz = tz)))
    ok <- !is.na(parsed)
    if (any(ok)) {
      out[rem[ok]] <- parsed[ok]
      rem <- rem[!ok]
    }
  }
  out
}

# --- Helper: Parse Extract Datetimes (Handles POSIXct, 12-hr AM/PM, and 24-hr formats) ---
parse_extract_datetime <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(as.POSIXct(format(x, "%Y-%m-%d %H:%M:%S"), tz = "UTC"))
  }
  if (inherits(x, "Date")) {
    return(as.POSIXct(paste0(format(x, "%Y-%m-%d"), " 00:00:00"), tz = "UTC"))
  }
  chr <- trimws(as.character(x))
  chr[chr == ""] <- NA_character_
  out <- rep(as.POSIXct(NA, origin = "1970-01-01", tz = "UTC"), length(chr))
  non_na <- which(!is.na(chr))
  if (length(non_na) == 0) return(out)

  has_ampm <- grepl("(am|pm)\\s*$", chr[non_na], ignore.case = TRUE, perl = TRUE)
  idx_12 <- non_na[has_ampm]
  idx_24 <- non_na[!has_ampm]

  if (length(idx_12) > 0) {
    chr_12 <- sub("\\s*(am|pm)\\s*$", " \\U\\1", chr[idx_12], ignore.case = TRUE, perl = TRUE)
    out[idx_12] <- parse_multi_strptime(
      chr_12,
      c("%d/%m/%Y %I:%M:%S %p", "%d/%m/%Y %I:%M %p",
        "%d-%m-%Y %I:%M:%S %p", "%d-%m-%Y %I:%M %p",
        "%Y-%m-%d %I:%M:%S %p", "%Y-%m-%d %I:%M %p",
        "%Y/%m/%d %I:%M:%S %p", "%Y/%m/%d %I:%M %p"),
      tz = "UTC"
    )
  }
  if (length(idx_24) > 0) {
    out[idx_24] <- parse_multi_strptime(
      chr[idx_24],
      c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M",
        "%d/%m/%Y %H:%M:%S", "%d/%m/%Y %H:%M",
        "%d-%m-%Y %H:%M:%S", "%d-%m-%Y %H:%M",
        "%Y/%m/%d %H:%M:%S", "%Y/%m/%d %H:%M"),
      tz = "UTC"
    )
  }
  out
}

# --- Helper: Timezone & Hour Truncation in Base R ---
get_tz <- function(x) {
  tz_val <- attr(x, "tzone")
  if (is.null(tz_val) || tz_val[1] == "") "UTC" else tz_val[1]
}

floor_to_hour <- function(dt) {
  tz_str <- get_tz(dt)
  lt <- as.POSIXlt(dt, tz = tz_str)
  lt$min <- 0L
  lt$sec <- 0
  as.POSIXct(lt, tz = tz_str)
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
  is_iso <- grepl("^\\d{4}-\\d{2}-\\d{2}", chr, perl = TRUE)
  chr[is_iso] <- substr(chr[is_iso], 1L, 10L)
  chr
}

# --- Helper: Sort Unique Filter Values Once in R (Numeric / % / Text) ---
sort_filter_values <- function(u) {
  if (length(u) == 0) return(character(0))
  u_chr <- unique(norm_filter_vec(u))
  blanks <- u_chr == ""
  na_txt <- u_chr == "N/A"
  regular <- u_chr[!blanks & !na_txt]
  if (length(regular) > 0) {
    num_part <- suppressWarnings(as.numeric(sub("%$", "", regular, perl = TRUE)))
    if (all(!is.na(num_part))) {
      regular <- regular[order(num_part, regular)]
    } else {
      regular <- sort(regular)
    }
  }
  c(regular, if (any(na_txt)) "N/A", if (any(blanks)) "")
}

# --- Helper: Base R Regex Group Matcher ---
rx_match <- function(s, pattern, ignore.case = FALSE) {
  m <- regexec(pattern, s, perl = TRUE, ignore.case = ignore.case)
  res <- regmatches(s, m)[[1]]
  if (length(res) == 0) return(NULL)
  res
}

# --- Helper: Locale-Independent Date Parsers ---
parse_ymd_flex <- function(s) {
  parts <- strsplit(gsub("/", "-", s, fixed = TRUE), "-", fixed = TRUE)[[1]]
  if (length(parts) != 3) return(as.Date(NA))
  nums <- suppressWarnings(as.integer(parts))
  if (any(is.na(nums))) return(as.Date(NA))
  suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", nums[1], nums[2], nums[3]), format = "%Y-%m-%d"))
}

parse_dmy_flex <- function(s) {
  parts <- strsplit(gsub("/", "-", s, fixed = TRUE), "-", fixed = TRUE)[[1]]
  if (length(parts) != 3) return(as.Date(NA))
  nums <- suppressWarnings(as.integer(parts))
  if (any(is.na(nums))) return(as.Date(NA))
  suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", nums[3], nums[2], nums[1]), format = "%Y-%m-%d"))
}

parse_dmy_text_flex <- function(s) {
  month_map <- c(
    jan = 1L, january = 1L, feb = 2L, february = 2L, mar = 3L, march = 3L,
    apr = 4L, april = 4L, may = 5L, jun = 6L, june = 6L,
    jul = 7L, july = 7L, aug = 8L, august = 8L, sep = 9L, sept = 9L, september = 9L,
    oct = 10L, october = 10L, nov = 11L, november = 11L, dec = 12L, december = 12L
  )
  parts <- strsplit(trimws(s), "\\s+", perl = TRUE)[[1]]
  if (length(parts) != 3) return(as.Date(NA))
  dd <- suppressWarnings(as.integer(parts[1]))
  mm <- unname(month_map[tolower(parts[2])])
  yy <- suppressWarnings(as.integer(parts[3]))
  if (is.na(dd) || is.na(mm) || is.na(yy)) return(as.Date(NA))
  suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", yy, mm, dd), format = "%Y-%m-%d"))
}

# --- Helper: Parse Pasted / Saved Holiday Entries (Date + Optional Label) ---
parse_holiday_text <- function(raw_text, default_year = format(Sys.Date(), "%Y")) {
  empty_df <- data.frame(Date = as.Date(character()), Label = character(), stringsAsFactors = FALSE)
  if (is.null(raw_text) || length(raw_text) == 0) {
    return(list(valid = empty_df, invalid = character(0)))
  }

  lines <- unlist(strsplit(paste(raw_text, collapse = "\n"), "\\r?\\n|;", perl = TRUE))
  lines <- trimws(lines)
  lines <- lines[lines != ""]

  expanded <- character(0)
  for (ln in lines) {
    if (grepl(",", ln, fixed = TRUE) && !grepl("[A-Za-z]", ln, perl = TRUE)) {
      parts <- trimws(strsplit(ln, ",", fixed = TRUE)[[1]])
      expanded <- c(expanded, parts[parts != ""])
    } else {
      expanded <- c(expanded, ln)
    }
  }

  if (length(expanded) == 0) {
    return(list(valid = empty_df, invalid = character(0)))
  }

  dates_out  <- as.Date(character())
  labels_out <- character()
  invalid    <- character()
  wday_pat   <- "(?:Mon|Tue|Wed|Thu|Fri|Sat|Sun|Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday)"

  for (item in expanded) {
    clean_item <- trimws(sub("^[•*-]+\\s*", "", item, perl = TRUE))
    if (clean_item == "") next
    if (grepl("^date\\b.*\\bholiday\\b", clean_item, ignore.case = TRUE, perl = TRUE)) next

    m_noyear_wday <- rx_match(
      clean_item,
      paste0("^(\\d{1,2}\\s+[A-Za-z]{3,9})\\s+", wday_pat, "\\b\\s*[-:,|\\t]?\\s*(.*)$"),
      ignore.case = TRUE
    )
    m_noyear <- rx_match(clean_item, "^(\\d{1,2}\\s+[A-Za-z]{3,9})(?:\\s*[-:,|\\t]\\s*(.*))?$")
    m_iso    <- rx_match(clean_item, "^(\\d{4}[-/]\\d{1,2}[-/]\\d{1,2})(?:\\s*[-:,|\\t]?\\s*(.*))?$")
    m_dmy    <- rx_match(clean_item, "^(\\d{1,2}[-/]\\d{1,2}[-/]\\d{4})(?:\\s*[-:,|\\t]?\\s*(.*))?$")
    m_txt    <- rx_match(clean_item, "^(\\d{1,2}\\s+[A-Za-z]{3,9}\\s+\\d{4})(?:\\s*[-:,|\\t]?\\s*(.*))?$")

    parsed_dt <- as.Date(NA)
    lbl_raw   <- ""

    if (!is.null(m_noyear_wday)) {
      parsed_dt <- parse_dmy_text_flex(paste(m_noyear_wday[2], default_year))
      lbl_raw   <- ifelse(length(m_noyear_wday) >= 3, m_noyear_wday[3], "")
    } else if (!is.null(m_noyear)) {
      parsed_dt <- parse_dmy_text_flex(paste(m_noyear[2], default_year))
      lbl_raw   <- ifelse(length(m_noyear) >= 3, m_noyear[3], "")
    } else if (!is.null(m_iso)) {
      parsed_dt <- parse_ymd_flex(m_iso[2])
      lbl_raw   <- ifelse(length(m_iso) >= 3, m_iso[3], "")
    } else if (!is.null(m_dmy)) {
      parsed_dt <- parse_dmy_flex(m_dmy[2])
      lbl_raw   <- ifelse(length(m_dmy) >= 3, m_dmy[3], "")
    } else if (!is.null(m_txt)) {
      parsed_dt <- parse_dmy_text_flex(m_txt[2])
      lbl_raw   <- ifelse(length(m_txt) >= 3, m_txt[3], "")
    }

    if (is.na(parsed_dt)) {
      invalid <- c(invalid, clean_item)
    } else {
      lbl <- trimws(lbl_raw)
      lbl <- trimws(sub(paste0("^", wday_pat, "\\b\\s*[-:,|\\t]?\\s*"), "", lbl, ignore.case = TRUE, perl = TRUE))
      if (grepl("^\\(.*\\)$", lbl, perl = TRUE)) {
        lbl <- trimws(sub("^\\((.*)\\)$", "\\1", lbl, perl = TRUE))
      }
      dates_out  <- c(dates_out, parsed_dt)
      labels_out <- c(labels_out, lbl)
    }
  }

  if (length(dates_out) == 0) {
    return(list(valid = empty_df, invalid = invalid))
  }

  res_df <- data.frame(Date = dates_out, Label = labels_out, stringsAsFactors = FALSE)
  res_df <- res_df[order(res_df$Date), , drop = FALSE]
  u_dates <- unique(res_df$Date)
  u_labels <- vapply(u_dates, function(d) {
    lbls <- res_df$Label[res_df$Date == d]
    non_empty <- lbls[nzchar(lbls)]
    if (length(non_empty) > 0) non_empty[1] else ""
  }, character(1))
  res_df <- data.frame(Date = u_dates, Label = u_labels, stringsAsFactors = FALSE)

  list(valid = res_df, invalid = invalid)
}

format_holidays_csv <- function(hol_df) {
  if (is.null(hol_df) || nrow(hol_df) == 0) return("")
  hol_df <- hol_df[order(hol_df$Date), , drop = FALSE]
  entries <- ifelse(
    nzchar(hol_df$Label),
    paste0(format(hol_df$Date, "%Y-%m-%d"), " (", hol_df$Label, ")"),
    format(hol_df$Date, "%Y-%m-%d")
  )
  paste(entries, collapse = "; ")
}

# --- Helper: Parse legacy LTAT strings (e.g. "48 - 72 hrs", "5-8 days", "same day", "next day") ---
infer_rule_from_legacy_text <- function(ltat_str) {
  s <- tolower(trimws(as.character(ltat_str)))
  if (is.na(s) || s == "" || s == "n/a") {
    return(list(Rule_Type = "Duration", Dur_DD = 0L, Dur_HH = 0L, Dur_MM = 0L,
                Skip_Days = "", Skip_Holidays = FALSE, Cutoff_HHMM = "", Target_HHMM = ""))
  }
  if (grepl("same day", s, fixed = TRUE)) {
    return(list(Rule_Type = "Same Day", Dur_DD = 0L, Dur_HH = 0L, Dur_MM = 0L,
                Skip_Days = "", Skip_Holidays = FALSE, Cutoff_HHMM = "", Target_HHMM = ""))
  }
  if (grepl("next day", s, fixed = TRUE)) {
    return(list(Rule_Type = "Next Day", Dur_DD = 0L, Dur_HH = 0L, Dur_MM = 0L,
                Skip_Days = "", Skip_Holidays = FALSE, Cutoff_HHMM = "", Target_HHMM = ""))
  }
  m <- gregexpr("\\d+(?:\\.\\d+)?", s, perl = TRUE)
  nums <- as.numeric(regmatches(s, m)[[1]])
  max_val <- if (length(nums) > 0) max(nums) else 0
  if (grepl("day", s, fixed = TRUE)) {
    return(list(Rule_Type = "Duration", Dur_DD = as.integer(floor(max_val)),
                Dur_HH = as.integer(round((max_val %% 1) * 24)), Dur_MM = 0L,
                Skip_Days = "", Skip_Holidays = FALSE, Cutoff_HHMM = "", Target_HHMM = ""))
  } else if (grepl("min", s, fixed = TRUE)) {
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
    words <- strsplit(gsub("_", " ", x, fixed = TRUE), "\\s+", perl = TRUE)[[1]]
    words <- vapply(words, function(w) {
      if (w == "") return("")
      if (grepl("^[A-Z0-9]+$", w, perl = TRUE)) return(w)
      paste0(toupper(substr(w, 1, 1)), substr(w, 2, nchar(w)))
    }, character(1))
    paste(words, collapse = " ")
  }, character(1), USE.NAMES = FALSE)
}

# --- Vectorized Helper: Calculate elapsed hours excluding specified weekdays & holidays (WebR Fast) ---
calc_effective_hrs <- function(dt_start, dt_end, skip_days_str = "",
                               skip_holidays = FALSE, holiday_dates = as.Date(character(0))) {
  raw_hrs <- as.numeric(difftime(dt_end, dt_start, units = "hours"))

  skip_wdays <- integer(0)
  if (!is.na(skip_days_str) && trimws(skip_days_str) != "") {
    skip_vec <- trimws(strsplit(skip_days_str, ",", fixed = TRUE)[[1]])
    skip_vec <- skip_vec[skip_vec != ""]
    if (length(skip_vec) > 0) {
      day_map <- c("Sun" = 0L, "Mon" = 1L, "Tue" = 2L, "Wed" = 3L, "Thu" = 4L, "Fri" = 5L, "Sat" = 6L)
      skip_wdays <- unname(day_map[skip_vec])
      skip_wdays <- skip_wdays[!is.na(skip_wdays)]
    }
  }

  use_holidays <- isTRUE(skip_holidays) && length(holiday_dates) > 0
  if (length(skip_wdays) == 0 && !use_holidays) {
    return(round(raw_hrs, 2))
  }

  holiday_day_ints <- if (use_holidays) unique(as.integer(as.Date(holiday_dates))) else integer(0)

  s_num <- as.numeric(dt_start)
  e_num <- as.numeric(dt_end)
  out   <- round((e_num - s_num) / 3600, 2)

  valid <- which(!is.na(s_num) & !is.na(e_num) & e_num > s_num)
  if (length(valid) == 0) return(out)

  tz_str <- get_tz(dt_start)
  ref_epoch <- as.numeric(as.POSIXct("1970-01-01 00:00:00", tz = tz_str))

  for (i in valid) {
    s <- s_num[i] - ref_epoch
    e <- e_num[i] - ref_epoch
    d0 <- floor(s / 86400)
    d1 <- floor(e / 86400)
    days <- d0:d1
    keep <- rep(TRUE, length(days))
    if (length(skip_wdays) > 0) {
      wdays <- (days + 4L) %% 7L
      keep <- keep & !(wdays %in% skip_wdays)
    }
    if (use_holidays) {
      keep <- keep & !(days %in% holiday_day_ints)
    }
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
  
  has_c1 <- grepl("(^|,)1(,|$)", exc_codes, perl = TRUE)
  has_c2 <- grepl("(^|,)2(,|$)", exc_codes, perl = TRUE)
  has_c3 <- grepl("(^|,)3(,|$)", exc_codes, perl = TRUE)
  has_c4 <- grepl("(^|,)4(,|$)", exc_codes, perl = TRUE)
  
  u_eval <- dt_unit
  v_eval <- dt_valid
  if (any(has_c1 & valid)) {
    u_eval[has_c1] <- floor_to_hour(u_eval[has_c1])
    v_eval[has_c1] <- floor_to_hour(v_eval[has_c1])
  }
  
  tz_str <- get_tz(u_eval)
  extra_sec <- ifelse(has_c3 & !is.na(exc_ext_hrs), as.numeric(exc_ext_hrs) * 3600, 0)
  
  # 1. Same Day
  idx_sd <- which(valid & rule_type == "Same Day")
  if (length(idx_sd) > 0) {
    base_dl <- as.POSIXct(paste0(as.Date(u_eval[idx_sd]), " 23:59:59"), tz = tz_str)
    base_dl <- base_dl + ifelse(has_c2[idx_sd], 86400, 0) + extra_sec[idx_sd]
    status[idx_sd] <- ifelse(v_eval[idx_sd] <= base_dl, "PASS", "FAIL")
  }
  
  # 1b. Next Day (Time-Ignoring Calendar Date)
  idx_nd_pure <- which(valid & rule_type == "Next Day")
  if (length(idx_nd_pure) > 0) {
    base_dl <- as.POSIXct(paste0(as.Date(u_eval[idx_nd_pure]) + 1, " 23:59:59"), tz = tz_str)
    base_dl <- base_dl + ifelse(has_c2[idx_nd_pure], 86400, 0) + extra_sec[idx_nd_pure]
    status[idx_nd_pure] <- ifelse(v_eval[idx_nd_pure] <= base_dl, "PASS", "FAIL")
  }
  
  # 2. Next Day Cutoff
  idx_nd <- which(valid & rule_type == "Next Day Cutoff" & !is.na(cutoff_hm) & cutoff_hm != "" & !is.na(target_hm) & target_hm != "")
  if (length(idx_nd) > 0) {
    u_date    <- as.Date(u_eval[idx_nd])
    cutoff_dt <- suppressWarnings(as.POSIXct(paste0(u_date, " ", cutoff_hm[idx_nd], ":00"), tz = tz_str))
    same_dl   <- as.POSIXct(paste0(u_date, " 23:59:59"), tz = tz_str)
    next_dl   <- suppressWarnings(as.POSIXct(paste0(u_date + 1, " ", target_hm[idx_nd], ":00"), tz = tz_str))
    
    dl <- ifelse(u_eval[idx_nd] < cutoff_dt, same_dl, next_dl)
    dl <- as.POSIXct(dl, origin = "1970-01-01", tz = tz_str)
    
    c2_sub <- has_c2[idx_nd] & !is.na(dl)
    if (any(c2_sub)) {
      dl[c2_sub] <- as.POSIXct(paste0(as.Date(dl[c2_sub]) + 1, " 23:59:59"), tz = tz_str)
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
        dl <- as.POSIXct(paste0(as.Date(u_eval[sub_i]) + t_days + 1, " 23:59:59"), tz = tz_str) + extra_sec[sub_i]
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
    codes <- sort(unique(strsplit(ec, ",", fixed = TRUE)[[1]]))
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
  
  df$`Accepted Date (Unit)` <- format_extract_datetime(dt_unit_parsed, df$`Accepted Date (Unit)`)
  df$`Validation Date`      <- format_extract_datetime(dt_valid_parsed, df$`Validation Date`)
  df$t_clean                <- trimws(as.character(df$TestName))
  df$s_clean                <- trimws(as.character(df$`Specimen Type`))
  df$Extract_Key            <- paste0(df$t_clean, " | ", df$s_clean)
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

  has_hol_col <- "Holidays" %in% names(raw_df)
  hol_parsed <- data.frame(Date = as.Date(character()), Label = character(), stringsAsFactors = FALSE)
  if (has_hol_col) {
    hol_cells <- na.omit(trimws(as.character(raw_df$Holidays)))
    hol_cells <- hol_cells[hol_cells != ""]
    if (length(hol_cells) > 0) {
      hol_parsed <- parse_holiday_text(hol_cells)$valid
    }
    raw_df$Holidays <- NULL
  }
  
  is_structured <- all(c("LTAT_Test_Name", "LTAT_Display", "Rule_Type") %in% names(raw_df))
  
  if (is_structured) {
    valid_rules <- c("Duration", "Same Day", "Next Day", "Next Day Cutoff")
    non_empty_rt <- trimws(as.character(raw_df$Rule_Type))
    non_empty_rt <- non_empty_rt[!is.na(non_empty_rt) & non_empty_rt != ""]
    bad_rules <- setdiff(non_empty_rt, valid_rules)
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
    
    rt_clean <- trimws(as.character(raw_df$Rule_Type))
    full_df <- data.frame(
      LTAT_Test_Name = trimws(as.character(raw_df$LTAT_Test_Name)),
      LTAT_Display   = trimws(as.character(raw_df$LTAT_Display)),
      Rule_Type      = rt_clean,
      Dur_DD         = suppressWarnings(as.integer(ifelse(is.na(raw_df$Dur_DD), 0L, raw_df$Dur_DD))),
      Dur_HH         = suppressWarnings(as.integer(ifelse(is.na(raw_df$Dur_HH), 0L, raw_df$Dur_HH))),
      Dur_MM         = suppressWarnings(as.integer(ifelse(is.na(raw_df$Dur_MM), 0L, raw_df$Dur_MM))),
      Skip_Days      = ifelse(is.na(raw_df$Skip_Days), "", trimws(as.character(raw_df$Skip_Days))),
      Skip_Holidays  = ifelse(
        !is.na(rt_clean) & rt_clean == "Duration" & !is.na(raw_df$Skip_Holidays),
        toupper(trimws(as.character(raw_df$Skip_Holidays))) %in% c("TRUE", "T", "1", "YES", "Y"),
        FALSE
      ),
      Cutoff_HHMM    = ifelse(is.na(raw_df$Cutoff_HHMM), "", trimws(as.character(raw_df$Cutoff_HHMM))),
      Target_HHMM    = ifelse(is.na(raw_df$Target_HHMM), "", trimws(as.character(raw_df$Target_HHMM))),
      Extract_Key    = ifelse(is.na(raw_df$Extract_Key), "", trimws(as.character(raw_df$Extract_Key))),
      stringsAsFactors = FALSE
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
    full_df <- data.frame(
      LTAT_Test_Name = trimws(as.character(name_col)),
      LTAT_Display   = trimws(as.character(ltat_col)),
      Rule_Type      = vapply(parsed_list, `[[`, character(1), "Rule_Type"),
      Dur_DD         = as.integer(vapply(parsed_list, `[[`, integer(1), "Dur_DD")),
      Dur_HH         = as.integer(vapply(parsed_list, `[[`, integer(1), "Dur_HH")),
      Dur_MM         = as.integer(vapply(parsed_list, `[[`, integer(1), "Dur_MM")),
      Skip_Days      = vapply(parsed_list, `[[`, character(1), "Skip_Days"),
      Skip_Holidays  = as.logical(vapply(parsed_list, `[[`, logical(1), "Skip_Holidays")),
      Cutoff_HHMM    = vapply(parsed_list, `[[`, character(1), "Cutoff_HHMM"),
      Target_HHMM    = vapply(parsed_list, `[[`, character(1), "Target_HHMM"),
      Extract_Key    = ifelse(is.na(ext_col), "", trimws(as.character(ext_col))),
      stringsAsFactors = FALSE
    )
  }
  
  full_df <- full_df[!is.na(full_df$LTAT_Test_Name) & full_df$LTAT_Test_Name != "", , drop = FALSE]
  
  if (nrow(full_df) == 0 && nrow(hol_parsed) == 0) {
    return(list(ok = FALSE, error = "Specification Map contains no valid test names in LTAT_Test_Name."))
  }
  
  non_empty_keys <- full_df$Extract_Key[full_df$Extract_Key != ""]
  bad_keys <- non_empty_keys[!grepl("\\|", non_empty_keys, perl = TRUE)]
  if (length(bad_keys) > 0) {
    return(list(
      ok = FALSE,
      error = paste0(
        "Invalid Extract_Key format (must be 'TestName | Specimen Type'):\n• ",
        paste(head(bad_keys, 10), collapse = "\n• ")
      )
    ))
  }
  
  cat_cols <- setdiff(names(full_df), "Extract_Key")
  cat_df <- full_df[!duplicated(full_df$LTAT_Test_Name), cat_cols, drop = FALSE]
  rownames(cat_df) <- NULL
  
  map_sub <- full_df[full_df$Extract_Key != "", c("Extract_Key", "LTAT_Test_Name"), drop = FALSE]
  map_df <- map_sub[!duplicated(map_sub$Extract_Key), , drop = FALSE]
  rownames(map_df) <- NULL
  
  list(ok = TRUE, catalog = cat_df, mapping = map_df, holidays = hol_parsed, has_holidays_col = has_hol_col)
}

# --- Schema Validator 3: Holiday File (.csv / .xlsx / .xls) ---
validate_holiday_file <- function(file_input) {
  res <- read_tabular_safe(file_input)
  if (!res$ok) return(res)
  df <- res$data

  if (nrow(df) == 0) {
    return(list(ok = FALSE, error = "The uploaded Holiday file contains 0 rows."))
  }

  if ("Holidays" %in% names(df)) {
    hol_cells <- na.omit(trimws(as.character(df$Holidays)))
    hol_cells <- hol_cells[hol_cells != ""]
    if (length(hol_cells) > 0) {
      parsed_spec <- parse_holiday_text(hol_cells)
      if (nrow(parsed_spec$valid) > 0) {
        return(list(ok = TRUE, holidays = parsed_spec$valid, skipped = length(parsed_spec$invalid)))
      }
    }
  }

  cn_low <- tolower(trimws(names(df)))
  date_idx <- which(cn_low %in% c("date", "holiday_date", "holiday date", "tarikh"))
  date_idx <- if (length(date_idx) > 0) date_idx[1] else 1L

  label_idx <- which(
    cn_low %in% c("holiday_name", "holiday name", "holiday", "label", "name", "description", "perkara", "cuti") &
      seq_along(cn_low) != date_idx
  )
  label_col <- if (length(label_idx) > 0) {
    label_idx[1]
  } else if (ncol(df) >= 2) {
    other_cols <- setdiff(seq_len(ncol(df)), date_idx)
    c2_samp <- head(na.omit(trimws(as.character(df[[other_cols[1]]]))), 15)
    is_wday_col <- length(c2_samp) > 0 &&
      mean(grepl("^(Mon|Tue|Wed|Thu|Fri|Sat|Sun|Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday)$",
                 c2_samp, ignore.case = TRUE, perl = TRUE)) >= 0.6
    if (is_wday_col && length(other_cols) >= 2) other_cols[2] else other_cols[1]
  } else {
    NA_integer_
  }

  raw_dates <- df[[date_idx]]
  if (inherits(raw_dates, "POSIXt") || inherits(raw_dates, "Date")) {
    date_strs <- format(raw_dates, "%Y-%m-%d")
  } else {
    date_strs <- trimws(as.character(raw_dates))
  }
  date_strs[is.na(date_strs)] <- ""

  if (!is.na(label_col)) {
    lbl_strs <- trimws(as.character(df[[label_col]]))
    lbl_strs[is.na(lbl_strs)] <- ""
    keep <- date_strs != ""
    lines <- ifelse(
      lbl_strs[keep] != "",
      paste0(date_strs[keep], "\t", lbl_strs[keep]),
      date_strs[keep]
    )
  } else {
    lines <- date_strs[date_strs != ""]
  }

  if (length(lines) == 0) {
    return(list(ok = FALSE, error = "No date values found in the uploaded Holiday file."))
  }

  parsed <- parse_holiday_text(lines)
  if (nrow(parsed$valid) == 0) {
    return(list(
      ok = FALSE,
      error = paste0(
        "Could not parse any valid holiday dates from the uploaded file.\n",
        "Expected columns: Date (e.g. YYYY-MM-DD or DD/MM/YYYY) and optional Holiday_Name."
      )
    ))
  }

  list(ok = TRUE, holidays = parsed$valid, skipped = length(parsed$invalid))
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
        onmousedown = "event.stopPropagation()",
        onclick = sprintf("openExcelFilter(event, '%s', '%s')", table_id, col_name),
        HTML("▾")
      )
    )
  }
}

# ==============================================================================
# CENTRALIZED TABLE CLASS (Bitmask Engine + Cascading + Date Hierarchy + Isolation)
# ==============================================================================
AppTable <- R6Class(
  "AppTable",
  public = list(
    id                = NULL,
    mode              = NULL,
    page_size         = NULL,
    page_size_options = NULL,
    selected_idx      = 1L,
    row_state         = integer(0),
    col_bits          = integer(0),
    date_cols         = character(0),
    pct_cols          = character(0),
    unfiltered_vals   = list(),
    last_flt          = list(),
    ignore_cols       = character(0),
    n_rows            = 0L,

    initialize = function(id, mode = c("report", "selector"), page_size = NULL,
                          page_size_options = NULL, pct_cols = character(0)) {
      self$id       <- id
      self$mode     <- match.arg(mode)
      self$pct_cols <- pct_cols
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
      } else if (col %in% self$pct_cols) {
        v <- df[[col]]
        ifelse(is.na(v), "N/A", paste0(v, "%"))
      } else {
        norm_filter_vec(df[[col]])
      }
    },

    set_data = function(df, ignore_cols = character(0)) {
      if (self$mode != "report" || is.null(df)) return(invisible(NULL))
      self$ignore_cols <- ignore_cols
      target_cols      <- setdiff(names(df), ignore_cols)
      self$n_rows      <- nrow(df)
      self$row_state   <- integer(self$n_rows)
      self$last_flt    <- list()

      if (length(target_cols) > 0) {
        is_dt <- vapply(target_cols, function(cn) {
          if (cn %in% self$pct_cols) return(FALSE)
          v <- norm_filter_vec(df[[cn]])
          non_empty <- v[v != ""]
          if (length(non_empty) == 0) return(FALSE)
          samp <- head(non_empty, 50L)
          mean(grepl("^\\d{4}-\\d{2}-\\d{2}", samp, perl = TRUE)) >= 0.8
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

      for (col in removed_cols) {
        if (col %in% names(self$col_bits)) {
          self$row_state <- bitwAnd(self$row_state, bitwNot(self$col_bits[[col]]))
        }
      }

      for (col in new_cols) {
        if (!(col %in% names(self$col_bits)) || !(col %in% names(df))) next
        chosen <- as.character(flt_list[[col]])
        if (!identical(chosen, self$last_flt[[col]])) {
          b <- self$col_bits[[col]]
          self$row_state <- bitwAnd(self$row_state, bitwNot(b))
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
      b <- self$col_bits[[col]]
      keep <- bitwAnd(self$row_state, bitwNot(b)) == 0L
      if (!any(keep)) return(I(character(0)))
      I(sort_filter_values(self$col_filter_vec(df, col)[keep]))
    },

    apply_state = function(df, table_filter_input, table_sort_input) {
      if (self$mode != "report") return(df)
      flt <- if (!is.null(table_filter_input)) table_filter_input$filters else NULL
      self$update_filters(df, flt)
      df <- df[self$row_state == 0L, , drop = FALSE]

      if (!is.null(table_sort_input) && !is.null(table_sort_input$col) && table_sort_input$col %in% names(df)) {
        col_vec <- df[[table_sort_input$col]]
        num_key <- if (is.numeric(col_vec)) col_vec else suppressWarnings(as.numeric(sub("%$", "", as.character(col_vec), perl = TRUE)))
        use_num <- !all(is.na(num_key)) && sum(!is.na(num_key)) == sum(!is.na(col_vec) & as.character(col_vec) != "" & as.character(col_vec) != "N/A")
        
        ord <- if (use_num) {
          order(num_key, as.character(col_vec), decreasing = (table_sort_input$dir == "desc"), na.last = TRUE)
        } else {
          order(as.character(col_vec), decreasing = (table_sort_input$dir == "desc"), na.last = TRUE)
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
    tags$title("LTAT App"),
    tags$style(HTML("
      .rt-resizer {
        width: 6px !important;
        right: -3px !important;
        z-index: 1;
      }
      .excel-filter-btn {
        border: 1px solid #ccc; background: #f8f9fa; color: #333;
        border-radius: 3px; padding: 1px 5px; margin-left: 6px; margin-right: 6px;
        cursor: pointer; font-size: 11px; line-height: 1.2;
        display: inline-flex; align-items: center; justify-content: center;
        position: relative; z-index: 2;
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
      /* Holiday Editor Tab Styles */
      .holiday-list-box {
        max-height: 465px; overflow-y: auto; border: 1px solid #dee2e6;
        border-radius: 4px; background: #fff;
      }
      .holiday-row {
        display: flex; justify-content: space-between; align-items: center; gap: 8px;
        padding: 6px 12px; border-bottom: 1px solid #f1f3f5; font-size: 13px;
      }
      .holiday-row:last-child { border-bottom: none; }
      .holiday-row:hover { background: #f8f9fa; }
      .holiday-date-pill {
        font-family: monospace; font-weight: 700; color: #0d6efd;
        background: #e7f1ff; border-radius: 3px; padding: 2px 7px; font-size: 12.5px;
      }
      .holiday-wday-tag {
        color: #6c757d; font-size: 11.5px; margin-left: 2px;
      }
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
        pushExcelFilters(tableId);
      }

      function escapeExcelHtml(str) {
        return String(str)
          .replace(/&/g, '&amp;')
          .replace(/</g, '&lt;')
          .replace(/>/g, '&gt;')
          .replace(/"/g, '&quot;')
          .replace(/'/g, '&#39;');
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
        pushExcelFilters(activeTable);
        closeExcelPopup();
      }

      function sortExcelCol(dir) {
        if (!activeTable) return;
        var outMap = {
          all: 'all_table',
          proc_ext: 'processed_extract_table',
          summary: 'summary_table',
          outliers: 'outliers_table'
        };
        if (window.Reactable && Reactable.setSort && outMap[activeTable]) {
          try { Reactable.setSort(outMap[activeTable], []); } catch (e) {}
        }
        Shiny.setInputValue('excel_sort_' + activeTable, {
          col: activeCol,
          dir: dir,
          nonce: Math.random()
        });
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
        pushExcelFilters(activeTable);
        closeExcelPopup();
      }

      function pushExcelFilters(tableId) {
        if (!tableId) return;
        Shiny.setInputValue('excel_filter_' + tableId, {
          filters: window.excelFilters[tableId] || {},
          nonce: Math.random()
        });
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
          pushExcelFilters(tid);
        });
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
  
  titlePanel("LTAT Performance Dashboard", windowTitle = "LTAT App"),
  
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
                    div(
                      style = "margin-top: 4px;",
                      checkboxInput("cat_skip_holidays", "Skip Public Holidays (uses Holiday Editor tab)", value = FALSE),
                      uiOutput("holiday_rule_hint_ui")
                    )
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
        ),

        tabPanel(
          "Holiday Editor",
          br(),
          fluidRow(
            column(
              5,
              wellPanel(
                style = "background: #f8f9fa;",
                h4("Add Public Holiday", style = "margin-top: 0;"),
                tags$small(
                  style = "color: #666; display: block; margin-bottom: 12px;",
                  "Elapsed time pauses on these dates when 'Skip Public Holidays' is enabled on a Duration rule in LTAT Editor."
                ),
                dateInput("holiday_add_date", "Holiday Date:", value = Sys.Date(), format = "yyyy-mm-dd", width = "100%"),
                textInput("holiday_add_note", "Holiday Name / Label (Optional):", placeholder = "e.g. Hari Merdeka", width = "100%"),
                actionButton("btn_add_holiday", "Add Holiday", icon = icon("plus"), class = "btn-primary", style = "font-weight: bold;"),
                hr(),
                h5(strong("Import & Export Holidays")),
                tags$small(
                  style = "color: #666; display: block; margin-bottom: 10px;",
                  "Upload a holiday file (.csv / .xlsx), paste holidays from text/web, or download the current holiday list."
                ),
                fileInput(
                  "holiday_file_upload",
                  "Upload Holiday File (.csv / .xlsx):",
                  buttonLabel = "Upload",
                  accept = c(".csv", ".xlsx", ".xls"),
                  width = "100%"
                ),
                div(
                  style = "display: flex; gap: 8px; flex-wrap: wrap;",
                  actionButton("btn_open_paste_holidays", "Paste Holidays", icon = icon("paste"), class = "btn-outline-primary", style = "font-weight: bold;"),
                  actionButton("btn_download_holidays", "Download Holiday CSV", icon = icon("download"), class = "btn-outline-success", style = "font-weight: bold;")
                )
              )
            ),
            column(
              7,
              wellPanel(
                style = "background: #f8f9fa;",
                div(
                  style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
                  div(
                    style = "display: flex; align-items: center; gap: 8px;",
                    h4("Public Holidays List", style = "margin: 0;"),
                    uiOutput("holiday_count_badge_ui", inline = TRUE)
                  ),
                  actionButton("btn_clear_holidays", "Clear All", icon = icon("trash"), class = "btn-outline-danger btn-sm")
                ),
                uiOutput("holiday_list_ui")
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
  tbl_summary    <- AppTable$new(id = "summary",    mode = "report", pct_cols = c("Compliance Pct"))
  tbl_outliers   <- AppTable$new(id = "outliers",   mode = "report")
  tbl_map_master <- AppTable$new(id = "map_master", mode = "selector")
  tbl_cat_editor <- AppTable$new(id = "cat_editor", mode = "selector")
  
  empty_catalog <- data.frame(
    LTAT_Test_Name = character(),
    LTAT_Display   = character(),
    Rule_Type      = character(),
    Dur_DD         = integer(),
    Dur_HH         = integer(),
    Dur_MM         = integer(),
    Skip_Days      = character(),
    Skip_Holidays  = logical(),
    Cutoff_HHMM    = character(),
    Target_HHMM    = character(),
    stringsAsFactors = FALSE
  )
  
  empty_map <- data.frame(
    Extract_Key    = character(),
    LTAT_Test_Name = character(),
    stringsAsFactors = FALSE
  )

  empty_holidays <- data.frame(
    Date  = as.Date(character()),
    Label = character(),
    stringsAsFactors = FALSE
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
    holidays               = empty_holidays,
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
      mapped_choices <- sort(rv$mapping$Extract_Key[rv$mapping$LTAT_Test_Name == sel_name])
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
    
    sd_vec <- if (!is.na(row$Skip_Days) && nzchar(row$Skip_Days)) trimws(strsplit(row$Skip_Days, ",", fixed = TRUE)[[1]]) else character(0)
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
    if (isTRUE(check$has_holidays_col)) {
      rv$holidays <- check$holidays
    }
    rv$has_spec_map <- nrow(check$catalog) > 0
    rv$cat_sel_idx  <- 1L
    rv$map_sel_idx  <- 1L
    
    mark_spec_changed()
    
    if (nrow(rv$catalog) > 0) populate_editor_fields(1L)
    refresh_mapping_lists()
    
    n_skip_hol <- sum(rv$catalog$Rule_Type == "Duration" & !is.na(rv$catalog$Skip_Holidays) & rv$catalog$Skip_Holidays)
    showNotification(
      sprintf(
        "Specification Map verified & loaded (%d rules [%d skip holidays], %d mappings, %d holidays).",
        nrow(check$catalog), n_skip_hol, nrow(check$mapping), nrow(rv$holidays)
      ),
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
    if (!rv$has_spec_map && nrow(rv$holidays) == 0) {
      tags$span(style = "color: #666; font-size: 13px;", "No Specification Map loaded")
    } else {
      n_skip_hol <- sum(rv$catalog$Rule_Type == "Duration" & !is.na(rv$catalog$Skip_Holidays) & rv$catalog$Skip_Holidays)
      tags$span(
        style = "color: #155724; font-size: 13px; font-weight: bold;",
        sprintf("Loaded: %d LTAT Rules (%d Skip Holidays) | %d Mappings | %d Holidays",
                nrow(rv$catalog), n_skip_hol, nrow(rv$mapping), nrow(rv$holidays))
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

    n_skip_hol <- sum(rv$catalog$Rule_Type == "Duration" & !is.na(rv$catalog$Skip_Holidays) & rv$catalog$Skip_Holidays)
    n_hol      <- nrow(rv$holidays)
    hol_ok     <- (n_skip_hol == 0) || (n_hol > 0)
    hol_detail <- if (n_skip_hol > 0 && n_hol == 0) {
      sprintf("Missing — required by %d rule(s) with 'Skip Public Holidays'", n_skip_hol)
    } else if (n_skip_hol > 0) {
      sprintf("%d dates loaded — active on %d rule(s)", n_hol, n_skip_hol)
    } else if (n_hol > 0) {
      sprintf("%d dates loaded — 0 rules require holidays", n_hol)
    } else {
      "Optional — 0 rules require holidays"
    }
    
    div(
      badge(rv$has_extract,  "1. Extract File",      if (rv$has_extract)  paste0(nrow(rv$raw_df), " rows") else "Upload in 'All Specimen Records'"),
      badge(rv$has_spec_map, "2. Specification Map", if (rv$has_spec_map) sprintf("%d rules, %d mappings", nrow(rv$catalog), nrow(rv$mapping)) else "Upload in 'Specification Map'"),
      badge(hol_ok,          "3. Public Holidays",   hol_detail)
    )
  })
  
  output$obsolete_spec_banner_ui <- renderUI({
    req(rv$is_processed, rv$spec_dirty)
    div(
      class = "obsolete-banner",
      div(
        strong("⚠ Obsolete Processed Data: "),
        "The Specification Map (LTAT rules, test mappings, or holidays) has been modified since the last run. ",
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

    hol_req_rules <- rv$catalog$LTAT_Test_Name[
      rv$catalog$Rule_Type == "Duration" & !is.na(rv$catalog$Skip_Holidays) & rv$catalog$Skip_Holidays
    ]

    if (length(hol_req_rules) > 0 && nrow(rv$holidays) == 0) {
      req_keys <- rv$mapping$Extract_Key[rv$mapping$LTAT_Test_Name %in% hol_req_rules]
      in_ext   <- rv$raw_df$Extract_Key %in% req_keys
      mapped_test_names <- rv$mapping$LTAT_Test_Name[match(rv$raw_df$Extract_Key[in_ext], rv$mapping$Extract_Key)]
      tab_cnt  <- table(mapped_test_names)

      rule_lines <- vapply(hol_req_rules, function(rn) {
        cnt <- if (rn %in% names(tab_cnt)) as.integer(tab_cnt[[rn]]) else 0L
        if (cnt == 0L) paste0("• ", rn, "  (0 specimens in current extract)")
        else paste0("• ", rn, "  (", cnt, " specimen", ifelse(cnt == 1L, "", "s"), " in current extract)")
      }, character(1))

      showModal(modalDialog(
        title = "Holiday Data Required Before Processing",
        tags$p(
          "The following LTAT test(s) are marked with ", strong("Skip Public Holidays"),
          ", but the ", strong("Public Holidays List"), " is currently empty:"
        ),
        tags$pre(
          style = "max-height: 220px; overflow-y: auto; background: #fff3cd; color: #664d03; border: 1px solid #ffecb5; padding: 10px; white-space: pre-wrap;",
          paste(rule_lines, collapse = "\n")
        ),
        tags$p(
          "Please add, paste, or upload holiday dates under ",
          strong("Specification Map → Holiday Editor"),
          " (or uncheck ", strong("Skip Public Holidays"), " in ", strong("LTAT Editor"),
          ") before clicking ", strong("Process"), "."
        ),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    tryCatch({
      df_in <- rv$raw_df
      if (!("Lab. No" %in% names(df_in)))     df_in$`Lab. No`     <- as.character(seq_len(nrow(df_in)))
      if (!("Ward/Clinic" %in% names(df_in))) df_in$`Ward/Clinic` <- "N/A"
      
      cat_idx <- match(rv$mapping$LTAT_Test_Name, rv$catalog$LTAT_Test_Name)
      valid_m <- !is.na(cat_idx)
      map_lookup <- cbind(
        rv$mapping[valid_m, , drop = FALSE],
        rv$catalog[cat_idx[valid_m], setdiff(names(rv$catalog), "LTAT_Test_Name"), drop = FALSE]
      )
      
      m_idx <- match(df_in$Extract_Key, map_lookup$Extract_Key)
      lookup_cols <- setdiff(names(map_lookup), "Extract_Key")
      df_joined <- cbind(
        df_in,
        map_lookup[m_idx, lookup_cols, drop = FALSE]
      )
      rownames(df_joined) <- NULL

      row_seq <- seq_len(nrow(df_joined))
      df_joined$.row_id                <- row_seq
      df_joined$dt_unit                <- parse_extract_datetime(df_joined$`Accepted Date (Unit)`)
      df_joined$dt_valid               <- parse_extract_datetime(df_joined$`Validation Date`)
      df_joined$`Accepted Date (Unit)` <- format_extract_datetime(df_joined$dt_unit, df_joined$`Accepted Date (Unit)`)
      df_joined$`Validation Date`      <- format_extract_datetime(df_joined$dt_valid, df_joined$`Validation Date`)
      df_joined$.row_key               <- paste(df_joined$`Lab. No`, df_joined$TestName, df_joined$`Accepted Date (Unit)`, row_seq, sep = " | ")
      df_joined$raw_tat_hrs            <- round(as.numeric(difftime(df_joined$dt_valid, df_joined$dt_unit, units = "hours")), 2)
      df_joined$unit_tat_hrs           <- df_joined$raw_tat_hrs
      
      has_hol <- nrow(rv$holidays) > 0
      skip_idx <- which(
        !is.na(df_joined$Rule_Type) & df_joined$Rule_Type == "Duration" &
        ((!is.na(df_joined$Skip_Days) & df_joined$Skip_Days != "") |
         (!is.na(df_joined$Skip_Holidays) & df_joined$Skip_Holidays & has_hol))
      )
      if (length(skip_idx) > 0) {
        sd_vec_all <- ifelse(is.na(df_joined$Skip_Days[skip_idx]), "", df_joined$Skip_Days[skip_idx])
        sh_vec_all <- !is.na(df_joined$Skip_Holidays[skip_idx]) & df_joined$Skip_Holidays[skip_idx]
        grp_df     <- unique(data.frame(sd = sd_vec_all, sh = sh_vec_all, stringsAsFactors = FALSE))

        for (g in seq_len(nrow(grp_df))) {
          sd_val <- grp_df$sd[g]
          sh_val <- grp_df$sh[g]
          sub_i  <- skip_idx[sd_vec_all == sd_val & sh_vec_all == sh_val]
          df_joined$unit_tat_hrs[sub_i] <- calc_effective_hrs(
            df_joined$dt_unit[sub_i],
            df_joined$dt_valid[sub_i],
            skip_days_str = sd_val,
            skip_holidays = sh_val,
            holiday_dates = rv$holidays$Date
          )
        }
      }
      
      df_joined$LTAT_Target        <- ifelse(is.na(df_joined$LTAT_Display) | df_joined$LTAT_Display == "", "N/A", df_joined$LTAT_Display)
      df_joined$Mapped_LTAT_Test   <- ifelse(is.na(df_joined$LTAT_Test_Name), "Unmapped", df_joined$LTAT_Test_Name)
      df_joined$exc_codes          <- ""
      df_joined$exc_ext_hrs        <- 0
      df_joined$exc_nextday_cutoff <- ""
      df_joined$orig_status        <- evaluate_ltat_status(
        df_joined$dt_unit, df_joined$dt_valid, df_joined$unit_tat_hrs,
        df_joined$Rule_Type, df_joined$Dur_DD, df_joined$Dur_HH, df_joined$Dur_MM,
        df_joined$Cutoff_HHMM, df_joined$Target_HHMM, df_joined$Skip_Days
      )
      df_joined$Status             <- df_joined$orig_status

      drop_cols <- intersect(names(df_joined), c("t_clean", "s_clean", "LTAT_Display"))
      if (length(drop_cols) > 0) {
        df_joined <- df_joined[, setdiff(names(df_joined), drop_cols), drop = FALSE]
      }
      
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
    all_fail_df <- rv$processed_df[!is.na(rv$processed_df$orig_status) & rv$processed_df$orig_status == "FAIL", , drop = FALSE]

    if ("current_page" %in% scopes) {
      keys <- c(keys, rv$modal_visible_checked)
    }
    if ("filtered" %in% scopes) {
      keys <- c(keys, rv$modal_filtered_keys)
    }
    if ("spec_tests" %in% scopes && length(selected_tests) > 0) {
      test_keys <- all_fail_df$.row_key[all_fail_df$Mapped_LTAT_Test %in% selected_tests]
      keys <- c(keys, test_keys)
    }
    if ("all" %in% scopes) {
      keys <- c(keys, all_fail_df$.row_key)
    }
    unique(keys)
  }

  observeEvent(input$open_outlier_exc_modal, {
    req(rv$is_processed, rv$processed_df)

    all_fail_df <- rv$processed_df[!is.na(rv$processed_df$orig_status) & rv$processed_df$orig_status == "FAIL", , drop = FALSE]
    if (nrow(all_fail_df) == 0) {
      showNotification("There are no failed specimens to configure exceptions for.", type = "message")
      return()
    }

    vis_checked <- input$open_outlier_exc_modal$visible_checked
    if (is.null(vis_checked)) vis_checked <- character(0)

    filtered_tbl <- tbl_outliers$apply_state(
      outliers_base_df(),
      input$excel_filter_outliers,
      input$excel_sort_outliers
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
      nd_raw <- trimws(ifelse(is.null(input$exc_nd_cutoff_input), "", as.character(input$exc_nd_cutoff_input)))
      hhmm_regex <- "^([01]?[0-9]|2[0-3]):[0-5][0-9]$"
      if (!grepl(hhmm_regex, nd_raw, perl = TRUE)) {
        showNotification("Rule 2 cutoff time must be a valid 24-hour time in hh:mm format (e.g. 15:00).", type = "error")
        return()
      }
      parts <- as.integer(strsplit(nd_raw, ":", fixed = TRUE)[[1]])
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
      tz_str <- get_tz(df$dt_unit)
      for (i in idx) {
        row_rules <- rules
        if ("2" %in% row_rules && identical(nd_mode, "cutoff")) {
          dt_i <- df$dt_unit[i]
          if (!is.na(dt_i)) {
            lt_i <- as.POSIXlt(dt_i, tz = tz_str)
            meets_cutoff <- ((lt_i$hour * 3600L + lt_i$min * 60L + as.integer(lt_i$sec)) >= cutoff_sec)
          } else {
            meets_cutoff <- FALSE
          }
          if (!meets_cutoff) {
            row_rules <- setdiff(row_rules, "2")
          }
        }
        if (length(row_rules) == 0) next

        applied_idx <- c(applied_idx, i)
        cur <- if (!is.na(df$exc_codes[i]) && nzchar(df$exc_codes[i])) strsplit(df$exc_codes[i], ",", fixed = TRUE)[[1]] else character(0)
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
    req(nrow(rv$catalog) > 0 || nrow(rv$holidays) > 0)
    hol_str <- format_holidays_csv(rv$holidays)

    if (nrow(rv$catalog) > 0) {
      rows_list <- lapply(seq_len(nrow(rv$catalog)), function(i) {
        cat_row <- rv$catalog[i, , drop = FALSE]
        keys <- rv$mapping$Extract_Key[rv$mapping$LTAT_Test_Name == cat_row$LTAT_Test_Name]
        if (length(keys) == 0) keys <- ""
        out_block <- cat_row[rep(1L, length(keys)), , drop = FALSE]
        out_block$Extract_Key <- keys
        out_block$Holidays <- ""
        out_block
      })
      merged_out <- do.call(rbind, rows_list)
      rownames(merged_out) <- NULL
      merged_out$Holidays[1] <- hol_str
    } else {
      merged_out <- data.frame(
        LTAT_Test_Name = "",
        LTAT_Display   = "",
        Rule_Type      = "",
        Dur_DD         = 0L,
        Dur_HH         = 0L,
        Dur_MM         = 0L,
        Skip_Days      = "",
        Skip_Holidays  = FALSE,
        Cutoff_HHMM    = "",
        Target_HHMM    = "",
        Extract_Key    = "",
        Holidays       = hol_str,
        stringsAsFactors = FALSE
      )
    }
    
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
    tbl <- data.frame(
      `Test Name with LTAT` = rv$catalog$LTAT_Test_Name,
      `LTAT Display`        = rv$catalog$LTAT_Display,
      stringsAsFactors      = FALSE,
      check.names           = FALSE
    )
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
      unmapped_vec <- rv$raw_df$Extract_Key[rv$raw_df$Extract_Key %in% unmapped_keys]
      tab <- table(unmapped_vec)
      counts_df <- data.frame(
        Extract_Key = names(tab),
        Specimens   = as.integer(tab),
        stringsAsFactors = FALSE
      )
      counts_df <- counts_df[order(-counts_df$Specimens, counts_df$Extract_Key), , drop = FALSE]
      
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
    new_rows <- data.frame(
      Extract_Key    = input$unmapped_list,
      LTAT_Test_Name = sel_name,
      stringsAsFactors = FALSE
    )
    rv$mapping <- rbind(
      rv$mapping[!(rv$mapping$Extract_Key %in% input$unmapped_list), , drop = FALSE],
      new_rows
    )
    rownames(rv$mapping) <- NULL
    mark_spec_changed()
    refresh_mapping_lists()
  })
  
  observeEvent(input$btn_remove_map, {
    req(input$mapped_list)
    rv$mapping <- rv$mapping[!(rv$mapping$Extract_Key %in% input$mapped_list), , drop = FALSE]
    rownames(rv$mapping) <- NULL
    mark_spec_changed()
    refresh_mapping_lists()
  })
  
  # ============================================================================
  # NESTED TAB 2B & 2C: LTAT EDITOR & HOLIDAY EDITOR LOGIC
  # ============================================================================

  merge_holidays <- function(new_df) {
    if (is.null(new_df) || nrow(new_df) == 0) return(invisible(NULL))
    combined <- rbind(rv$holidays, new_df)
    combined <- combined[order(combined$Date), , drop = FALSE]
    u_dates <- unique(combined$Date)
    u_labels <- vapply(u_dates, function(d) {
      lbls <- combined$Label[combined$Date == d]
      non_empty <- lbls[nzchar(lbls)]
      if (length(non_empty) > 0) non_empty[length(non_empty)] else ""
    }, character(1))
    rv$holidays <- data.frame(Date = u_dates, Label = u_labels, stringsAsFactors = FALSE)
    mark_spec_changed()
  }

  output$holiday_count_badge_ui <- renderUI({
    n <- nrow(rv$holidays)
    tags$span(
      style = "background: #e7f1ff; color: #0d6efd; border: 1px solid #b6d4fe; border-radius: 10px; padding: 1px 8px; font-size: 11.5px; font-weight: 700;",
      sprintf("%d date%s", n, ifelse(n == 1, "", "s"))
    )
  })

  output$holiday_rule_hint_ui <- renderUI({
    n <- nrow(rv$holidays)
    if (isTRUE(input$cat_skip_holidays) && n == 0) {
      tags$div(
        style = "color: #856404; font-size: 11.5px; margin-top: -6px; margin-bottom: 4px;",
        "⚠ Holiday list is currently empty — add, paste, or upload dates in the Holiday Editor tab."
      )
    } else if (n > 0) {
      tags$div(
        style = "color: #198754; font-size: 11.5px; margin-top: -6px; margin-bottom: 4px;",
        sprintf("✔ %d holiday date%s configured in Holiday Editor tab.", n, ifelse(n == 1, "", "s"))
      )
    } else {
      NULL
    }
  })

  output$holiday_list_ui <- renderUI({
    hol <- rv$holidays
    if (nrow(hol) == 0) {
      return(div(
        class = "holiday-list-box",
        div(style = "padding: 16px; color: #888; font-style: italic; font-size: 12.5px; text-align: center;",
            "No holidays added yet. Pick a date on the left, upload a file, or click 'Paste Holidays'.")
      ))
    }

    rows <- lapply(seq_len(nrow(hol)), function(i) {
      d_str   <- format(hol$Date[i], "%Y-%m-%d")
      w_str   <- format(hol$Date[i], "%a")
      lbl_str <- hol$Label[i]
      div(
        class = "holiday-row",
        div(
          style = "display: flex; align-items: center; gap: 8px; overflow: hidden;",
          span(class = "holiday-date-pill", d_str),
          span(class = "holiday-wday-tag", paste0("(", w_str, ")")),
          if (nzchar(lbl_str)) {
            span(style = "color: #212529; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;", title = lbl_str, lbl_str)
          }
        ),
        tags$button(
          type    = "button",
          class   = "exc-remove-btn",
          title   = paste("Remove", d_str),
          onclick = sprintf(
            "Shiny.setInputValue('remove_holiday_item', { date: '%s', nonce: Math.random() }, { priority: 'event' });",
            d_str
          ),
          HTML("&times;")
        )
      )
    })

    div(class = "holiday-list-box", rows)
  })

  observeEvent(input$btn_add_holiday, {
    d <- input$holiday_add_date
    if (is.null(d) || length(d) == 0 || is.na(d)) {
      showNotification("Please select a valid holiday date.", type = "warning")
      return()
    }
    lbl <- trimws(ifelse(is.null(input$holiday_add_note), "", input$holiday_add_note))
    already_exists <- as.Date(d) %in% rv$holidays$Date
    merge_holidays(data.frame(Date = as.Date(d), Label = lbl, stringsAsFactors = FALSE))
    updateTextInput(session, "holiday_add_note", value = "")
    showNotification(
      if (already_exists) paste("Updated holiday:", format(as.Date(d), "%Y-%m-%d"))
      else paste("Added holiday:", format(as.Date(d), "%Y-%m-%d")),
      type = "message"
    )
  })

  observeEvent(input$holiday_file_upload, {
    check <- validate_holiday_file(input$holiday_file_upload)
    if (!check$ok) {
      reject_upload("holiday_file_upload", "Holiday File", check$error)
      return()
    }

    before_n <- nrow(rv$holidays)
    merge_holidays(check$holidays)
    added_n <- nrow(rv$holidays) - before_n
    session$sendCustomMessage("resetFileInput", "holiday_file_upload")

    msg <- sprintf("Imported %d holiday(s) from file (%d new).", nrow(check$holidays), max(0L, added_n))
    if (!is.null(check$skipped) && check$skipped > 0) {
      msg <- paste0(msg, sprintf(" Skipped %d unrecognized row(s).", check$skipped))
    }
    showNotification(
      msg,
      type = if (!is.null(check$skipped) && check$skipped > 0) "warning" else "message",
      duration = 6
    )
  })

  observeEvent(input$btn_download_holidays, {
    if (nrow(rv$holidays) == 0) {
      showNotification("Public Holidays List is empty. Add or import holidays before downloading.", type = "warning")
      return()
    }

    hol_sorted <- rv$holidays[order(rv$holidays$Date), , drop = FALSE]
    hol_out <- data.frame(
      Date         = format(hol_sorted$Date, "%Y-%m-%d"),
      Holiday_Name = hol_sorted$Label,
      stringsAsFactors = FALSE
    )

    tc <- textConnection("csv_str", "w", local = TRUE)
    write.csv(hol_out, tc, row.names = FALSE)
    close(tc)

    session$sendCustomMessage("downloadTextFile", list(
      filename = paste0("Public_Holidays_", Sys.Date(), ".csv"),
      content  = paste(csv_str, collapse = "\n"),
      mime     = "text/csv;charset=utf-8;"
    ))
  })

  observeEvent(input$remove_holiday_item, {
    d_str <- input$remove_holiday_item$date
    req(!is.null(d_str), nzchar(d_str))
    target_d <- as.Date(d_str)
    rv$holidays <- rv$holidays[rv$holidays$Date != target_d, , drop = FALSE]
    mark_spec_changed()
    showNotification(paste("Removed holiday:", d_str), type = "warning")
  })

  observeEvent(input$btn_clear_holidays, {
    if (nrow(rv$holidays) == 0) return()
    rv$holidays <- empty_holidays
    mark_spec_changed()
    showNotification("Cleared all holidays from list.", type = "warning")
  })

  observeEvent(input$btn_open_paste_holidays, {
    showModal(modalDialog(
      title = "Paste Public Holidays",
      fluidRow(
        column(
          4,
          numericInput("holiday_paste_year", "Year (when pasted dates omit year):",
                       value = as.integer(format(Sys.Date(), "%Y")), min = 2000, max = 2100, step = 1)
        ),
        column(
          8,
          style = "padding-top: 24px;",
          tags$small(style = "color: #555;", "Supports direct table copy-paste from publicholidays.com.my, Excel, or YYYY-MM-DD.")
        )
      ),
      textAreaInput(
        "holiday_paste_area",
        label       = NULL,
        rows        = 8,
        width       = "100%",
        placeholder = "17 Jan\tSat\tIsrak and Mikraj\n18 Jan\tSun\tIsrak and Mikraj Holiday\n— or —\n2026-01-17\tIsrak and Mikraj"
      ),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("btn_confirm_paste_holidays", "Import Holidays", class = "btn-primary", style = "font-weight: bold;")
      )
    ))
  })

  observeEvent(input$btn_confirm_paste_holidays, {
    raw_txt <- input$holiday_paste_area
    if (is.null(raw_txt) || trimws(raw_txt) == "") {
      showNotification("Please paste at least one holiday date.", type = "warning")
      return()
    }
    yr <- suppressWarnings(as.integer(input$holiday_paste_year))
    if (is.na(yr) || yr < 1900 || yr > 2200) yr <- as.integer(format(Sys.Date(), "%Y"))

    parsed <- parse_holiday_text(raw_txt, default_year = as.character(yr))
    if (nrow(parsed$valid) == 0) {
      showNotification("No valid dates found. Use YYYY-MM-DD, DD/MM/YYYY, or '17 Jan  Sat  Name' format.", type = "error")
      return()
    }

    before_n <- nrow(rv$holidays)
    merge_holidays(parsed$valid)
    added_n  <- nrow(rv$holidays) - before_n
    removeModal()

    msg <- sprintf("Imported %d holiday(s) (%d new).", nrow(parsed$valid), max(0L, added_n))
    if (length(parsed$invalid) > 0) {
      msg <- paste0(msg, sprintf(" Skipped %d unrecognized line(s).", length(parsed$invalid)))
    }
    showNotification(msg, type = if (length(parsed$invalid) > 0) "warning" else "message", duration = 6)
  })
  
  read_editor_form <- function() {
    nm   <- trimws(input$cat_edit_name)
    disp <- trimws(input$cat_edit_display)
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
      return(list(row = data.frame(
        LTAT_Test_Name = nm, LTAT_Display = disp, Rule_Type = "Duration",
        Dur_DD = dd, Dur_HH = hh, Dur_MM = mm,
        Skip_Days = sd, Skip_Holidays = sh,
        Cutoff_HHMM = "", Target_HHMM = "",
        stringsAsFactors = FALSE
      )))
      
    } else if (rt == "Same Day") {
      return(list(row = data.frame(
        LTAT_Test_Name = nm, LTAT_Display = disp, Rule_Type = "Same Day",
        Dur_DD = 0L, Dur_HH = 0L, Dur_MM = 0L,
        Skip_Days = "", Skip_Holidays = FALSE,
        Cutoff_HHMM = "", Target_HHMM = "",
        stringsAsFactors = FALSE
      )))
      
    } else if (rt == "Next Day") {
      return(list(row = data.frame(
        LTAT_Test_Name = nm, LTAT_Display = disp, Rule_Type = "Next Day",
        Dur_DD = 0L, Dur_HH = 0L, Dur_MM = 0L,
        Skip_Days = "", Skip_Holidays = FALSE,
        Cutoff_HHMM = "", Target_HHMM = "",
        stringsAsFactors = FALSE
      )))
      
    } else if (rt == "Next Day Cutoff") {
      chm <- trimws(input$cat_cutoff_hhmm)
      thm <- trimws(input$cat_target_hhmm)
      hhmm_regex <- "^([01]?[0-9]|2[0-3]):[0-5][0-9]$"
      if (!grepl(hhmm_regex, chm, perl = TRUE) || !grepl(hhmm_regex, thm, perl = TRUE)) {
        return(list(error = "Both Cut-off Hour and Next-Day Target Hour must be valid 24-hour times in hh:mm format (e.g., 12:00, 17:30)."))
      }
      return(list(row = data.frame(
        LTAT_Test_Name = nm, LTAT_Display = disp, Rule_Type = "Next Day Cutoff",
        Dur_DD = 0L, Dur_HH = 0L, Dur_MM = 0L,
        Skip_Days = "", Skip_Holidays = FALSE,
        Cutoff_HHMM = chm, Target_HHMM = thm,
        stringsAsFactors = FALSE
      )))
    }
  }
  
  output$catalog_editor_table <- renderReactable({
    req(nrow(rv$catalog) > 0)
    rule_sum <- format_rule_summary(
      rv$catalog$Rule_Type, rv$catalog$Dur_DD, rv$catalog$Dur_HH, rv$catalog$Dur_MM,
      rv$catalog$Skip_Days, rv$catalog$Skip_Holidays, rv$catalog$Cutoff_HHMM, rv$catalog$Target_HHMM
    )
    tbl <- data.frame(
      `Test Name with LTAT` = rv$catalog$LTAT_Test_Name,
      `LTAT Display`        = rv$catalog$LTAT_Display,
      `Rule Type`           = rv$catalog$Rule_Type,
      `Rule Details`        = rule_sum,
      stringsAsFactors      = FALSE,
      check.names           = FALSE
    )
    
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
      rv$mapping$LTAT_Test_Name <- ifelse(
        rv$mapping$LTAT_Test_Name == old_name,
        new_row$LTAT_Test_Name,
        rv$mapping$LTAT_Test_Name
      )
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
    
    affected_tests <- rv$mapping$Extract_Key[rv$mapping$LTAT_Test_Name == old_name]
    
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
    rv$catalog      <- rbind(rv$catalog, new_row)
    rownames(rv$catalog) <- NULL
    rv$has_spec_map <- TRUE
    rv$cat_sel_idx  <- nrow(rv$catalog)
    updateReactable("catalog_editor_table", selected = rv$cat_sel_idx)
    mark_spec_changed()
    refresh_mapping_lists()
    showNotification(paste("Added:", new_row$LTAT_Test_Name), type = "message")
  })
  
  commit_catalog_delete <- function(idx, target_name) {
    rv$catalog <- rv$catalog[-idx, , drop = FALSE]
    rownames(rv$catalog) <- NULL
    rv$mapping <- rv$mapping[rv$mapping$LTAT_Test_Name != target_name, , drop = FALSE]
    rownames(rv$mapping) <- NULL
    
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
    affected_tests <- rv$mapping$Extract_Key[rv$mapping$LTAT_Test_Name == target_name]
    
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
    keep_cols <- setdiff(names(rv$raw_df), c("t_clean", "s_clean", "Extract_Key"))
    df <- rv$raw_df[, keep_cols, drop = FALSE]
    names(df) <- format_col_title(names(df))
    df
  })
  
  proc_extract_base_df <- reactive({
    req(rv$is_processed, rv$processed_df)
    p_df <- rv$processed_df
    exc_sup <- ifelse(
      p_df$Status == "PASS" & nzchar(p_df$exc_codes),
      format_exception_badges_html(p_df$exc_codes, p_df$exc_ext_hrs, p_df$exc_nextday_cutoff, superscript = TRUE),
      ""
    )
    df <- data.frame(
      `Lab. No`              = p_df$`Lab. No`,
      `Ward/Clinic`          = p_df$`Ward/Clinic`,
      Mapped_LTAT_Test       = p_df$Mapped_LTAT_Test,
      TestName               = p_df$TestName,
      `Specimen Type`        = p_df$`Specimen Type`,
      `Accepted Date (Unit)` = p_df$`Accepted Date (Unit)`,
      `Validation Date`      = p_df$`Validation Date`,
      `Raw TAT Hrs`          = p_df$raw_tat_hrs,
      `Unit TAT Hrs`         = p_df$unit_tat_hrs,
      LTAT_Target            = p_df$LTAT_Target,
      Status                 = p_df$Status,
      .exc_sup               = exc_sup,
      stringsAsFactors       = FALSE,
      check.names            = FALSE
    )
    names(df)[names(df) != ".exc_sup"] <- format_col_title(names(df)[names(df) != ".exc_sup"])
    df
  })
  
  summary_base_df <- reactive({
    req(rv$is_processed, rv$processed_df)
    n_hol <- nrow(rv$holidays)
    p_df  <- rv$processed_df

    grp_key <- paste(
      p_df$Mapped_LTAT_Test,
      p_df$TestName,
      p_df$`Specimen Type`,
      p_df$LTAT_Target,
      sep = "\r"
    )
    idx_list <- split(seq_len(nrow(p_df)), grp_key)

    rows_list <- lapply(idx_list, function(ii) {
      first_i  <- ii[1]
      ltat_tgt <- p_df$LTAT_Target[first_i]
      skip_hol <- p_df$Skip_Holidays[first_i]
      st_vec   <- p_df$Status[ii]
      n_tot    <- length(ii)
      n_pass   <- sum(st_vec == "PASS", na.rm = TRUE)
      n_fail   <- sum(st_vec == "FAIL", na.rm = TRUE)
      denom    <- n_pass + n_fail

      hol_rule <- if (ltat_tgt == "N/A") {
        "N/A"
      } else if (isTRUE(skip_hol)) {
        sprintf("Yes (%d dates)", n_hol)
      } else {
        "No"
      }

      comp_pct <- if (ltat_tgt == "N/A" || denom == 0L) {
        NA_real_
      } else {
        round(100 * n_pass / denom, 1)
      }

      data.frame(
        Mapped_LTAT_Test = p_df$Mapped_LTAT_Test[first_i],
        TestName         = p_df$TestName[first_i],
        `Specimen Type`  = p_df$`Specimen Type`[first_i],
        LTAT_Target      = ltat_tgt,
        Holiday_Rule     = hol_rule,
        Total            = n_tot,
        Within_LTAT      = n_pass,
        Exceeded_LTAT    = n_fail,
        Compliance_Pct   = comp_pct,
        stringsAsFactors = FALSE,
        check.names      = FALSE
      )
    })

    df <- do.call(rbind, rows_list)
    rownames(df) <- NULL
    df <- df[order(df$Total, decreasing = TRUE), , drop = FALSE]
    names(df) <- format_col_title(names(df))
    df
  })
  
  outliers_base_df <- reactive({
    req(rv$is_processed, rv$processed_df)
    fail_df <- rv$processed_df[!is.na(rv$processed_df$orig_status) & rv$processed_df$orig_status == "FAIL", , drop = FALSE]
    exc_norm <- format_exception_badges_html(fail_df$exc_codes, fail_df$exc_ext_hrs, fail_df$exc_nextday_cutoff, superscript = FALSE)
    df <- data.frame(
      `Lab. No`              = fail_df$`Lab. No`,
      `Ward/Clinic`          = fail_df$`Ward/Clinic`,
      Mapped_LTAT_Test       = fail_df$Mapped_LTAT_Test,
      TestName               = fail_df$TestName,
      `Specimen Type`        = fail_df$`Specimen Type`,
      `Accepted Date (Unit)` = fail_df$`Accepted Date (Unit)`,
      `Validation Date`      = fail_df$`Validation Date`,
      `Unit TAT Hrs`         = fail_df$unit_tat_hrs,
      LTAT_Target            = fail_df$LTAT_Target,
      .row_key               = fail_df$.row_key,
      .exc_norm              = exc_norm,
      stringsAsFactors       = FALSE,
      check.names            = FALSE
    )
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
    tbl_proc_ext$apply_state(proc_extract_base_df(), input$excel_filter_proc_ext, input$excel_sort_proc_ext)
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
    tbl <- tbl_all$apply_state(raw_preview_df(), input$excel_filter_all, input$excel_sort_all)
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
    tbl <- tbl_summary$apply_state(summary_base_df(), input$excel_filter_summary, input$excel_sort_summary)
    pct_override <- list(
      `Compliance Pct` = colDef(
        header     = excel_header("summary", "Compliance Pct"),
        cell       = function(value) if (is.na(value)) "N/A" else paste0(value, "%"),
        sortNALast = TRUE,
        align      = "right"
      )
    )
    tbl_summary$render(tbl, override_cols = pct_override, output_id = "summary_table")
  })
  
  # Tab 3 (Nested 3): Outliers / Failed Specimens
  output$outliers_table <- renderReactable({
    req(rv$is_processed, rv$processed_df)
    tbl <- tbl_outliers$apply_state(outliers_base_df(), input$excel_filter_outliers, input$excel_sort_outliers)
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
    export_df <- filtered_processed_extract()
    export_df <- export_df[, setdiff(names(export_df), ".exc_sup"), drop = FALSE]
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
