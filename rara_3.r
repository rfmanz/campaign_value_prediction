#imports----
library(data.table)
library(Hmisc)
library(plotly)
library(DataExplorer)
library(summarytools)
library(reticulate)
library(xray)
library(plyr)
library(dplyr)
# detach("package:plyr", unload=TRUE)
# library(streamgraph)
library(highcharter)
library(lubridate)
py_run_string("import sys; sys.path.append('utils')")
amg_utils <- import("amg_utils")
#load----
mnt_data = fread("marketer_data_only_us_240315.csv")
# current_date <- Sys.Date()
# formatted_date <- format(current_date, "%y%m%d")
# file_name <- paste0("marketer_data_only_us_", formatted_date, ".csv")
# r_to_py(amg_utils$eq(paste(readLines("marketer_data.sql"), collapse = "\n")))$to_csv(file_name, index = FALSE)
# mnt_data_new = fread(file_name)

#transforms----
for (j in grep("_id", names(mnt_data), ignore.case = TRUE)) {
  set(mnt_data, j=j, value=as.character(mnt_data[[j]]))
}
mnt_data[, buyer_name := as.character(buyer_name)]
mnt_data[, roas := as.numeric(roas)]
mnt_data[, eventdate := as.Date(eventdate, format = "%Y-%m-%d")]
mnt_data[, campaign_start_date := min(eventdate), ad_id]
mnt_data[, campaign_end_date := max(eventdate), ad_id]
mnt_data[, year_month := format(as.Date(eventdate), "%Y-%m")] 
mnt_data[, total_campaign_contrib := sum(contrib, na.rm = TRUE), ad_id]
mnt_data[, total_campaign_revenue := sum(net_revenue, na.rm = TRUE), ad_id]
mnt_data[, total_campaign_spend := sum(fb_spend, na.rm = TRUE), ad_id]
mnt_data[, total_campaign_roas := total_campaign_revenue / total_campaign_spend, ad_id]
mnt_data[, average_roas_of_campaign := mean(roas, na.rm = TRUE), ad_id]
mnt_data[, campaign_duration := as.numeric(difftime(max(eventdate), min(eventdate), units = "days")), by = .(campaign_start_date, ad_id)]
mnt_data[, campaign_active_days := .N,ad_id]
mnt_data[,estimated_budget := round_any(fb_spend,10)]

mnt_data[, sequence_break := fifelse(eventdate - shift(eventdate, type = "lag") == 1, 0L, as.integer(eventdate - shift(eventdate, type = "lag"))), by = ad_id]


# filter dataset to only include campaigns between x and y days
mnt_data = mnt_data[between(campaign_active_days, 4, 90)]
setcolorder(mnt_data,sort(names(mnt_data)))

contrib = mnt_data[,sum(contrib),ad_id]
positive_contrib =  contrib[V1 >= 10]

non_vaccine_winning_campaigns = mnt_data[(ad_id %in% positive_contrib$ad_id & !grepl("vaccine", campaign_name, ignore.case = TRUE) & !duplicated(ad_id)), ad_id]

winner_buyers = c("Lorenzo Talia", "David Vion", "Simantika Mandloi") 

#metric comparison winner vs losers ----
# if you're going to do anything with roas then filter outliers 

mnt_data[!duplicated(ad_id) & ad_id %in% positive_contrib$ad_id ,.N]
mnt_data[!duplicated(ad_id) & !ad_id %in% positive_contrib$ad_id ,.N]
mnt_data[!duplicated(ad_id) & ad_id %in% positive_contrib$ad_id,.N] / mnt_data[!duplicated(ad_id) & !ad_id %in% positive_contrib$ad_id ,.N]

names(mnt_data)

perf_metrics = mnt_data[,.(
  roas, 
  fb_spend, 
  estimated_budget, 
  total_campaign_contrib, 
  total_campaign_revenue,
  total_campaign_spend, 
  total_campaign_roas, 
  average_roas_of_campaign, 
  campaign_duration, 
  campaign_active_days, 
  sequence_break, 
  eventdate, ad_id)]%>% names



variables <- c("roas", "fb_spend", "estimated_budget")
days <- c(1, 2)
result <- data.table()
for (var in variables) {
  for (day in days) {
    metric <- paste(var, "day", day, sep = "_")
    
    temp <- mnt_data[, {
      var_day <- shift(get(var), day - 1)
      ad_id_winners <- ad_id %in% positive_contrib$ad_id
      .(
        Metric = rep(metric, 6),
        Level = c("min", "25%", "50%", "75%", "mean", "max"),
        winners = .(
          min(var_day[ad_id_winners], na.rm = TRUE),
          quantile(var_day[ad_id_winners], 0.25, na.rm = TRUE),
          quantile(var_day[ad_id_winners], 0.50, na.rm = TRUE),
          quantile(var_day[ad_id_winners], 0.75, na.rm = TRUE),
          mean(var_day[ad_id_winners], na.rm = TRUE),
          max(var_day[ad_id_winners], na.rm = TRUE)
        ),
        losers = .(
          min(var_day[!ad_id_winners], na.rm = TRUE),
          quantile(var_day[!ad_id_winners], 0.25, na.rm = TRUE),
          quantile(var_day[!ad_id_winners], 0.50, na.rm = TRUE),
          quantile(var_day[!ad_id_winners], 0.75, na.rm = TRUE),
          mean(var_day[!ad_id_winners], na.rm = TRUE),
          max(var_day[!ad_id_winners], na.rm = TRUE)
        )
      )
    }][, `:=`(winners = as.numeric(winners), losers = as.numeric(losers))][
      , `:=`(Delta = winners - losers 
            # delta_pc = (winners / losers - 1)    
            )]

    result <- rbindlist(list(result, temp))
  }
}

result[, (names(result)[sapply(result, is.numeric)]) := lapply(.SD, round, 2), .SDcols = is.numeric]
result[,delta_pct:= fifelse(winners>=losers, paste0("+", scales::percent(winners / losers - 1,0.1)), paste0("-", scales::percent(1 - winners / losers)))]
result

#adhoc queries----
# relationship between time, fb_spend and leads 
# how many campaigns have different buyers attached to them
# breaks in campaigns, create break counter... how many days was the campaign paused
ads_with_sqnce_breaks = mnt_data[!is.na(sequence_break) & sequence_break > 0, unique(ad_id)]
mnt_data[!is.na(sequence_break) & sequence_break > 0, uniqueN(ad_id)]
(mnt_data[!is.na(sequence_break) & sequence_break > 0, uniqueN(ad_id)]/ uniqueN(mnt_data$ad_id))%>% scales::percent(.,0.01)
# mnt_data[ad_id==23857110226780464,.(ad_id, eventdate, sequence_break)]

mnt_data[ad_id %in% ads_with_sqnce_breaks,.(
  min_break = min(sequence_break, na.rm = TRUE),
  max_break = max(sequence_break, na.rm = TRUE),
  mean_break = mean(sequence_break, na.rm = TRUE),

  median_break = median(sequence_break, na.rm = TRUE)
), ad_id]

mnt_data[ad_id %in% ads_with_sqnce_breaks, .(
  min_break = min(sequence_break, na.rm = TRUE),
  max_break = max(sequence_break, na.rm = TRUE),
  mean_break = mean(sequence_break, na.rm = TRUE),
  median_break = median(sequence_break, na.rm = TRUE)
), ad_id][, .(
  avg_min_break = mean(min_break),
  avg_max_break = mean(max_break),
  avg_mean_break = mean(mean_break),
  avg_median_break = mean(median_break)
)]
