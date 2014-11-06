con = dbConnect(dbDriver("PostgreSQL"), dbname = "rdrreddit_development")

query = function(sql) { fetch(dbSendQuery(con, sql), n = 1e7) }

convert_timezones_to_utc = function(data_frame) {
  for (i in 1:ncol(data_frame)) {
    if (all(class(data_frame[, i]) == c("POSIXct", "POSIXt"))) {
      data_frame[, i] = as.POSIXct(as.character(data_frame[, i]), tz = "UTC")
    }
  }
  
  return(data_frame)
}

ordered_table = function(x) { sort(table(x), decreasing = TRUE) }

scale_x_top_100 = function(name, breaks = seq(0, 100, by = 25), n_spaces = 16) {
  spaces = paste(rep(" ", n_spaces), collapse = " ")
  labels = breaks
  line_break = ifelse(diff(breaks)[1] > 5, "\n", "\n\n")
  
  for (i in 1:length(breaks)) {
    break_num = as.numeric(breaks[i])
    if (break_num %% 25 == 0 & break_num < 100) {
      labels[i] = paste0(breaks[i], line_break, spaces, "page ", break_num / 25 + 1)
    }
  }
  
  scale_x_continuous(name, breaks = breaks, labels = labels, lim = range(breaks))
}

cut_quantile = function(x, n = 10) {
  cut(x, breaks = quantile(x, (0:n) / n), include.lowest = TRUE)
}
