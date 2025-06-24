#imports---
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
# mnt_data = amg_utils$eq(paste(readLines("marketer_data.sql"), collapse = "\n"))
# r_to_py(mnt_data)$to_csv("marketer_data_only_us.csv", index = FALSE)
mnt_data = fread("marketer_data_only_us.csv")

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
mnt_data[,sequence_break_above_2 := fifelse(lead(eventdate) - eventdate > 2, 1, 0), ad_id]
mnt_data[,estimated_budget := round_any(fb_spend,10)]
# filter dataset to only include campaigns between x and y days
mnt_data = mnt_data[between(campaign_active_days, 4, 90)]
setcolorder(mnt_data,sort(names(mnt_data)))
#winners----
# 2286 campaigns total 
mnt_data[!duplicated(ad_id),.N]
mnt_data[,range(campaign_active_days)]
# number of campaigns per month 
mnt_data[,.N,.(ad_id,year_month)][,.N,year_month][order(-N),.(year_month,N, pct = scales::percent(N/sum(N)))][order(year_month)]
# average roas per month
# distribution of campaign active days
ggplot(mnt_data[!duplicated(ad_id), .N, by = .(campaign_active_days = as.factor(campaign_active_days))][order(-campaign_active_days)], aes(x = campaign_active_days, y = N)) +
  geom_bar(stat = "identity") +
  labs(x = "Campaign Active Days", y = "Count")


contrib = mnt_data[,sum(contrib),ad_id]
positive_contrib =  contrib[V1 >= 10]
length(positive_contrib$V1)  
# 574 winner campaings. defined as all generating $10 or more over lifetime 

rank_by_total_campaign_roas = mnt_data[ad_id %in% positive_contrib$ad_id, .(ad_id, 
total_campaign_roas, 
# campaign_name, 
adset_name
# total_campaign_contrib
)][!duplicated(ad_id)][order(-total_campaign_roas)][1:100]

cols = c(
    "ad_id", 
    "adset_id", 
    "eventdate", 
    "campaign_name",
    "adset_name",
    "campaign_start_date",
    "campaign_end_date",
    "year_month",
    "total_campaign_contrib",
    "total_campaign_revenue",
    "total_campaign_spend",
    "total_campaign_roas",
    "average_roas_of_campaign",
    "campaign_duration",
    "campaign_active_days",
    "buyer_name",
    "roas",
    "net_revenue",
    "fb_spend",
    "contrib",
    "fb_clicks_all",
    "fb_impressions",
    "fb_leads",
    "fb_link_click" 
    )

mnt_data[(ad_id %in% positive_contrib$ad_id & grepl("vaccine", campaign_name, ignore.case = TRUE) & !duplicated(ad_id)), .(ad_id, total_campaign_contrib, total_campaign_roas,campaign_name)][order(-total_campaign_roas)]  %>%kbl%>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))


# 35% of winners are vaccine campaigns 197
mnt_data[(ad_id %in% positive_contrib$ad_id & grepl("vaccine", campaign_name, ignore.case = TRUE) & !duplicated(ad_id)), .(ad_id, total_campaign_contrib, total_campaign_roas,campaign_name)][order(-total_campaign_roas)] 
# [,.N]/nrow(positive_contrib)

mnt_data[(ad_id %in% positive_contrib$ad_id & grepl("vaccine", campaign_name, ignore.case = TRUE) & !duplicated(ad_id)), .(.N),year_month] 

mnt_data[(ad_id %in% positive_contrib$ad_id & grepl("vaccine", campaign_name, ignore.case = TRUE) & !duplicated(ad_id)), .(.N),buyer_name][order(-N)]



mnt_data[,.N,.(ad_id,year_month)][,.N,year_month][order(-N),.(year_month,N, pct = scales::percent(N/sum(N)))][order(year_month)]

mnt_data[(ad_id %in% positive_contrib$ad_id & grepl("vaccine", campaign_name, ignore.case = TRUE) & !duplicated(ad_id)), .N, buyer_name][order(-N)][, pct:= scales::percent(N/sum(N))][]

# of the top 100 winner campaigns ranked by total_campaign_roas (total_campaign_revenue / total_campaign_spend) 77 are vaccine campaigns
rank_by_total_campaign_roas[grepl("vaccine", adset_name, ignore.case = TRUE),.N]

# non vaccine winning campaigns 380 
non_vaccine_winning_campaigns = mnt_data[(ad_id %in% positive_contrib$ad_id & !grepl("vaccine", campaign_name, ignore.case = TRUE) & !duplicated(ad_id)), ad_id]




mnt_data[ad_id %in% non_vaccine_winning_campaigns,fb_spend]%>% describe()

mnt_data[(ad_id %in% non_vaccine_winning_campaigns & between(campaign_active_days, min(campaign_active_days), 21)),.(eventdate,fb_spend),ad_id] %>% 
hchart(., "line", hcaes(x = eventdate, y = fb_spend, group = ad_id)) %>%
  hc_xAxis(type = "datetime") %>%
  hc_yAxis(title = list(text = "Facebook Spend")) %>%
  hc_add_theme(hc_theme_smpl())


ggplot(., aes(x = eventdate, y = fb_spend, group = ad_id)) +
  geom_line() +
  labs(x = "Event Date", y = "Facebook Spend") +
  theme_minimal()

mnt_data[mnt_data[ad_id %in% non_vaccine_winning_campaigns,.I[.GRP == 1],ad_id]$V1, .(eventdate,fb_spend, first_budget = floor(fb_spend[1]))]

mnt_data[ad_id %in% non_vaccine_winning_campaigns, .(fb_spend[1]), ad_id]%>% plot_histogram()
# [,V1] %>%  describe()

mnt_data[ad_id==120204213196530592, .SD, .SDcols = cols]

mnt_data[ad_id %in% non_vaccine_winning_campaigns] %>%
  group_by(buyer_name) %>%
  summarise(contrib = sum(contrib, na.rm = TRUE)) %>%
  arrange(desc(contrib)) %>%
  hchart("bar", hcaes(x = buyer_name, y = contrib)) %>%
  hc_xAxis(type = "category") %>%
  hc_plotOptions(series = list(color = 'rgba(50, 171, 96, 0.6)', borderColor = 'rgba(50, 171, 96, 1.0)', borderWidth = 1)) %>%  hc_tooltip(pointFormat = '${point.y:,.0f}')


mnt_data %>%
  group_by(eventdate) %>%
  summarise(mean_roas = mean(roas, na.rm = TRUE)) %>%
  hchart("line", hcaes(x = eventdate, y = mean_roas)) %>% hc_tooltip(pointFormat = '{series.name}: <b>${point.y:,.0f}</b><br/>') %>% 
  hc_title(text = "Mean ROAS") %>% 
  hc_navigator(enabled = TRUE) %>% hc_rangeSelector(enabled = TRUE)

mnt_data[,.(eventdate,ad_id),roas][order(-roas)][1:10]

mnt_data[ad_id == 120200424266500656, .(ad_id, eventdate, fb_spend, net_revenue, roas, adset_name,fb_clicks_all, rev_clicks)] 
 
mnt_data[ad_id==120203606790010368,.(ad_id, eventdate, fb_spend, net_revenue,roas,adset_name, fb_clicks_all, rev_clicks)]




# mnt_data[mnt_data[ad_id %in% non_vaccine_winning_campaigns,.I[.GRP == 1],ad_id]$V1, .(eventdate,fb_spend)]%>% 
# ggplot(., aes(x = eventdate, y = fb_spend)) +
#   geom_line() +
#   labs(x = "Event Date", y = "Facebook Spend") +
#   theme_minimal()

mnt_data[(ad_id %in% non_vaccine_winning_campaigns & !duplicated(ad_id)),.N,campaign_active_days][order(campaign_active_days)]

mnt_data[(ad_id %in% non_vaccine_winning_campaigns),.SD, .SDcols = cols]



#join fb hourly----
fb_hourly_p[,ad_id:= as.character(ad_id)]
fb_hourly_p = fread("fb_hourly_p.csv")
# setkey(fb_hourly_p, ad_id, eventdate)
# setkey(mnt_data, ad_id, eventdate)
# mnt_data_hourly = fb_hourly_p[mnt_data]
#fb hourly analysis----
# for the most succesful campaigns, what are the most succesful hours? how many hours did they have.
# graphs of waves
#keyword analysis----
pixy = fread("pixy.csv")

str(pixy)



# for the most succesful campaigns, what are the most succesful keywords? how many keywords did they have. 

# distribution over campaign lifetime of fb_impressions 
# secuential campaigns marker  
# fitler out werid stuff ... examples 

# metric start campaign, 25%, 50%, 75%, end campaign 
# 



mnt_data[ad_id %in% non_vaccine_winning_campaigns, .(fb_spend = mean(fb_spend, na.rm = TRUE), total_campaign_roas = mean(total_campaign_roas, na.rm = TRUE)),.(buyer_name)][order(-total_campaign_roas)]



# Lorenzo Talia 
# David Vion 
# Darren Nicastro 

mnt_data %>%
  group_by(eventdate) %>%
  summarise(mean_roas = mean(roas, na.rm = TRUE)) %>%
  hchart("line", hcaes(x = eventdate, y = mean_roas)) %>% hc_tooltip(pointFormat = '{series.name}: <b>${point.y:,.0f}</b><br/>') %>% 
  hc_title(text = "Total Contribution (fb_spend - net_rev) over Time") %>% 
  hc_navigator(enabled = TRUE) %>% hc_rangeSelector(enabled = TRUE)  


mnt_data[,fb_spend[1], .(ad_id)][,V1]%>% describe()
mnt_data[,fb_spend[1], .(ad_id)][,V1]%>% summary()


mnt_data[(buyer_name %in% c("Lorenzo Talia", "David Vion", "Simantika Mandloi") & ad_id %in% non_vaccine_winning_campaigns & !duplicated(ad_id)), .(
    # first_budget = .SD[,mean(fb_spend[1]), .(ad_id)];
#    .N   
    min_campaign_duration = describe(estimated_budget),
    max_campaign_duration = max(campaign_duration),
    mean_campaign_duration = mean(campaign_duration)
      # first_budget = first_budget
    )
, buyer_name]


mnt_data[(ad_id %in% non_vaccine_winning_campaigns & !duplicated(ad_id)), .N , buyer_name][order(-N)]


mnt_data[!duplicated(ad_id), .N, .(total_campaign_contrib>10, buyer_name)]%>% dcast(.,buyer_name ~ total_campaign_contrib  , value.var = "N", fill = 0)[]

mnt_data[!duplicated(ad_id), .(
  num_true = sum(total_campaign_contrib > 10),
  num_false = sum(!(total_campaign_contrib > 10)),
  pct_true = sum(total_campaign_contrib > 10) / .N
), by = buyer_name][order(-pct_true)][, pct_true := scales::percent(pct_true)][]

names(mnt_data)
mnt_data[!duplicated(ad_id), .N]


mnt_data[buyer_name=="Lorenzo Talia" & !duplicated(ad_id) & ad_id %in% non_vaccine_winning_campaigns,.(total_campaign_roas,ad_id,campaign_duration)][order(-total_campaign_roas)] 

mnt_data[ad_id==120202333439710656,.(eventdate, fb_spend,estimated_budget, net_revenue,contrib, fb_impressions, roas,total_campaign_contrib,campaign_duration)]

mnt_data[,sequence_break:= fifelse(lead(eventdate) - eventdate == 1,0,1), ad_id]

mnt_data[,round_any(fb_spend,10)]


mnt_data[,.N,sequence_break_above_2]
mnt_data[sequence_break_above_2==1,.N,ad_id]
mnt_data[ad_id==120206191683430496,.(eventdate, sequence_break_above_2)]

library(plyr); library(dplyr)

winner_buyers = c("Lorenzo Talia", "David Vion", "Simantika Mandloi") 

mnt_data[(buyer_name %in% winner_buyers & ad_id %in% non_vaccine_winning_campaigns),.(  campaign_active_days, start_budget = round_any(fb_spend[2],10),total_campaign_roas, total_campaign_contrib),.(ad_id,buyer_name)][!duplicated(ad_id)][order(buyer_name)]

mnt_data[(buyer_name %in% winner_buyers[1] & ad_id %in% non_vaccine_winning_campaigns),.(
# campaign_active_days, 
# start_budget = round_any(fb_spend[2],10),
# total_campaign_roas, 
# eventdate,
budget_changes = sum(fifelse(fb_spend - lag(fb_spend) > 0, 1, 0)),
fb_spend,
estimated_budget
# total_campaign_contrib
),.(ad_id,buyer_name)][!duplicated(ad_id)][order(buyer_name)]

# budget changes 

budget_by_top_buyer[,mean(budget_changes),buyer_name]

budget_by_top_buyer =mnt_data[(buyer_name %in% winner_buyers & ad_id %in% non_vaccine_winning_campaigns), .(
  first_budget = estimated_budget[1],
  last_budget = last(lag(estimated_budget)),  
  mean_budget = mean(estimated_budget, na.rm = TRUE) %>% round(),
  budget_changes = sum(fifelse(estimated_budget[-c(1, .N)] - shift(estimated_budget[-c(1, .N)]) > 0, 1, 0), na.rm = TRUE),  
  campaign_active_days,
  total_campaign_spend
  ), .(ad_id, buyer_name)][!duplicated(ad_id)]


for (x in 1:length(winner_buyers)) {
  print(mnt_data[(buyer_name %in% winner_buyers[x] & ad_id %in% non_vaccine_winning_campaigns),
                 .(first_budget = estimated_budget[1],
                   last_budget = last(lag(estimated_budget)),
                   mean_budget = mean(estimated_budget, na.rm = TRUE) %>% round(),
                   min_budget = min(estimated_budget, na.rm = TRUE),
                   max_budget = max(estimated_budget, na.rm = TRUE),
                   budget_changes = sum(fifelse(estimated_budget[-c(1, .N)] - shift(estimated_budget[-c(1, .N)]) > 0, 1, 0), na.rm = TRUE), 
                   campaign_active_days,
                   total_campaign_spend,
                   total_campaign_revenue,
                   total_campaign_roas),
                 .(ad_id, buyer_name)][!duplicated(ad_id)])
  cat("\n") 
}

top_buyers = mnt_data[(buyer_name %in% winner_buyers & ad_id %in% non_vaccine_winning_campaigns),
                 .(first_budget = estimated_budget[1],
                   last_budget = last(lag(estimated_budget)),
                   mean_budget = mean(estimated_budget, na.rm = TRUE) %>% round(),
                   budget_changes = sum(fifelse(estimated_budget[-c(1, .N)] - shift(estimated_budget[-c(1, .N)]) > 0, 1, 0), na.rm = TRUE),
                   campaign_active_days,
                   total_campaign_spend,
                   total_campaign_revenue,
                   total_campaign_roas),
                 .(ad_id, buyer_name)][!duplicated(ad_id)]

top_buyers[,.(mean_budget = mean(mean_budget, na.rm = TRUE), 
              mean_budget_changes = mean(budget_changes, na.rm = TRUE), 
              mean_campaign_active_days = mean(campaign_active_days, na.rm = TRUE), 
              mean_total_campaign_spend = mean(total_campaign_spend, na.rm = TRUE), 
              mean_total_campaign_revenue = mean(total_campaign_revenue, na.rm = TRUE), 
              mean_total_campaign_roas = mean(total_campaign_roas, na.rm = TRUE)),
            by = buyer_name]





tb[ad_id==buyer_name,.(roas,eventdate), ad_id]%>% ggplot(aes(x = eventdate, y = roas, group=ad_id)) + geom_line() + labs(x = "Event Date", y = "ROAS") + theme_minimal()


mnt_data[ad_id==23852749089160128,.(ad_id,eventdate, buyer_name, fb_spend, estimated_budget)]

tb = mnt_data[(buyer_name %in% winner_buyers & ad_id %in% non_vaccine_winning_campaigns),]
data.table::setorder(tb, -total_campaign_contrib, ad_id)
tb[,mean_budget := mean(estimated_budget, na.rm = TRUE) %>% round()]

top_campaigns_talia = tb[(between(mean_budget,10,40) & buyer_name %in% winner_buyers[1])][!duplicated(ad_id), ad_id][5]

tb[ad_id %in% top_campaigns_talia, .(fb_impressions, eventdate, ad_id)][
  , row_num := rowid(ad_id)][
  , total_rows := .N, by = ad_id][
  , normalized_position := (row_num - 1) / (total_rows - 1)][
  , hchart(.SD, "line", hcaes(x = normalized_position, y = fb_impressions, group = ad_id)) %>%
    hc_xAxis(title = list(text = "Normalized Position")) %>%
    hc_yAxis(title = list(text = "Facebook Spend")) %>%
    hc_add_theme(hc_theme_smpl())]

cols2

tb[ad_id %in% top_campaigns_talia, .(roas, eventdate, ad_id)][
  , row_num := rowid(ad_id)][
  , total_rows := .N, by = ad_id][
  , normalized_position := (row_num - 1) / (total_rows - 1)][
  , hchart(.SD, "line", hcaes(x = normalized_position, y = roas, group = ad_id)) %>%
    hc_xAxis(title = list(text = "Normalized Position")) %>%
    hc_yAxis(title = list(text = "Facebook Spend")) %>%
    hc_add_theme(hc_theme_smpl())]

cols2 <- c("buy_side_ctr", "cpp", "cpc", "rpc", "rpp", "buy_side_cpm", "prftble", 
           "fb_impressions", "fb_clicks_all", "fb_leads", "fb_link_click")


cols3 <- c("buy_side_ctr", "cpp", "cpc", "rpc", "rpp", "buy_side_cpm", 
           "fb_impressions", "fb_clicks_all", "fb_leads", "fb_link_click")

filtered_data <- tb[ad_id %in% top_campaigns_talia, .SD, .SDcols = c(cols3, "eventdate", "ad_id")]
# print(head(filtered_data))

filtered_data[, {
  hc <- highchart() %>%
    hc_xAxis(type = "datetime", title = list(text = "Event Date"))
  
  for (col in cols3) {
    hc <- hc %>%
      hc_add_series(data = .SD, type = "line", hcaes(x = eventdate, y = .SD[[col]], group = ad_id),
                    name = col)
  }
  
  hc %>%
    hc_add_theme(hc_theme_smpl())
}]


library(data.table)
library(highcharter)

cols3 <- c(
  "buy_side_ctr", 
  "cpp", 
  "cpc", 
  "rpc", 
  "rpp", 
  "buy_side_cpm",
  "roas"
  # "fb_impressions", 
  # "fb_clicks_all", 
  # "fb_leads", 
  # "fb_link_click"
  )

# filtered_data <- tb[ad_id %in% top_campaigns_talia, .SD, .SDcols = c(cols3, "eventdate", "ad_id")]

# filtered_data[, {
#   hc <- highchart() %>%
#     hc_xAxis(type = "datetime", title = list(text = "Event Date"))
  
#   hc <- hc %>%
#     hc_add_series(data = .SD, type = "line", hcaes(x = eventdate, y = buy_side_ctr, group = ad_id), name = "buy_side_ctr") %>%
#     hc_add_series(data = .SD, type = "line", hcaes(x = eventdate, y = cpp, group = ad_id), name = "cpp") %>%
#     hc_add_series(data = .SD, type = "line", hcaes(x = eventdate, y = cpc, group = ad_id), name = "cpc") %>%
#     hc_add_series(data = .SD, type = "line", hcaes(x = eventdate, y = rpc, group = ad_id), name = "rpc") %>%
#     hc_add_series(data = .SD, type = "line", hcaes(x = eventdate, y = rpp, group = ad_id), name = "rpp") %>%
#     hc_add_series(data = .SD, type = "line", hcaes(x = eventdate, y = buy_side_cpm, group = ad_id), name = "buy_side_cpm") %>%
#     hc_add_series(data = .SD, type = "line", hcaes(x = eventdate, y = fb_impressions, group = ad_id), name = "fb_impressions") %>%
#     hc_add_series(data = .SD, type = "line", hcaes(x = eventdate, y = fb_clicks_all, group = ad_id), name = "fb_clicks_all") %>%
#     hc_add_series(data = .SD, type = "line", hcaes(x = eventdate, y = fb_leads, group = ad_id), name = "fb_leads") %>%
#     hc_add_series(data = .SD, type = "line", hcaes(x = eventdate, y = fb_link_click, group = ad_id), name = "fb_link_click") %>% 
#     hc_add_series(data = .SD, type = "line", hcaes(x = eventdate, y = fb_leads, group = ad_id), name = "fb_leads") %>%
  
#   hc %>%
#     hc_add_theme(hc_theme_smpl())
# }]


top_campaigns_talia = tb[(between(mean_budget,10,40) & buyer_name %in% winner_buyers[1])][!duplicated(ad_id), ad_id][2]


cols3 <- c(
  "buy_side_ctr", 
  "cpp", 
  "cpc", 
  "rpc", 
  "rpp", 
  "buy_side_cpm",
  "roas"
  # "fb_impressions", 
  # "fb_clicks_all", 
  # "fb_leads", 
  # "fb_link_click"
  )

filtered_data <- tb[ad_id %in% top_campaigns_talia, .SD, .SDcols = c(cols3, "eventdate", "ad_id")]

filtered_data[, {
  hc <- highchart() %>%
    hc_xAxis(type = "datetime", title = list(text = "Event Date"))
  
hc <- hc %>%
  hc_add_series(data = .SD, type = "line", hcaes(x = eventdate, y = buy_side_ctr, group = ad_id), name = "buy_side_ctr") %>%
  hc_add_series(data = .SD, type = "line", hcaes(x = eventdate, y = cpp, group = ad_id), name = "cpp") %>%
  hc_add_series(data = .SD, type = "line", hcaes(x = eventdate, y = cpc, group = ad_id), name = "cpc") %>%
  hc_add_series(data = .SD, type = "line", hcaes(x = eventdate, y = rpc, group = ad_id), name = "rpc") %>%
  hc_add_series(data = .SD, type = "line", hcaes(x = eventdate, y = rpp, group = ad_id), name = "rpp") %>%
  # hc_add_series(data = .SD, type = "line", hcaes(x = eventdate, y = buy_side_cpm, group = ad_id), name = "buy_side_cpm") %>%
  hc_add_series(data = .SD, type = "line", hcaes(x = eventdate, y = roas, group = ad_id), name = "roas") %>%
  hc_add_theme(hc_theme_smpl())
}]





# 




library(data.table)
library(plotly)

cols3 <- c("buy_side_ctr", "cpp", "cpc", "rpc", "rpp", "buy_side_cpm", "prftble",
           "fb_impressions", "fb_clicks_all", "fb_leads", "fb_link_click")

filtered_data <- tb[ad_id %in% top_campaigns_talia, .SD, .SDcols = c(cols3, "eventdate", "ad_id")]

# Normalize each column separately
normalized_data <- copy(filtered_data)
for (col in cols3) {
  normalized_data[, (col) := (get(col) - min(get(col), na.rm = TRUE)) / (max(get(col), na.rm = TRUE) - min(get(col), na.rm = TRUE))]
}

# Convert eventdate to character
normalized_data[, eventdate := as.character(eventdate)]

# Create the plot
plot_ly(normalized_data, x = ~eventdate, color = ~ad_id, type = "scatter", mode = "lines") %>%
  add_trace(y = ~buy_side_ctr, name = "buy_side_ctr") %>%
  add_trace(y = ~cpp, name = "cpp") %>%
  add_trace(y = ~cpc, name = "cpc") %>%
  add_trace(y = ~rpc, name = "rpc") %>%
  add_trace(y = ~rpp, name = "rpp") %>%
  add_trace(y = ~buy_side_cpm, name = "buy_side_cpm") %>%
  add_trace(y = ~fb_impressions, name = "fb_impressions") %>%
  add_trace(y = ~fb_clicks_all, name = "fb_clicks_all") %>%
  add_trace(y = ~fb_leads, name = "fb_leads") %>%
  add_trace(y = ~fb_link_click, name = "fb_link_click") %>%
  layout(
    xaxis = list(title = "Event Date"),
    yaxis = list(title = "Normalized Value"),
    title = "Normalized Variables over Time"
  )


mnt_data[ad_id==23857252958520460,.(ad_id,eventdate, fb_spend,adset_name, fb_impressions, fb_clicks_all, fb_leads, fb_link_click, roas, net_revenue, contrib)]

mnt_data[ad_id %in% non_vaccine_winning_campaigns, .(fb_spend, fb_impressions, roas, buyer_name)]%>% 
hchart(., "column", hcaes(x = fb_spend, y = Value, group = Type)) %>%
  hc_xAxis(title = list(text = "Category")) %>%
  hc_yAxis(title = list(text = "Value")) %>%
  hc_plotOptions(column = list(stacking = "normal")) %>%
  hc_tooltip(pointFormat = "{series.name}: <b>{point.y}</b>")

mnt_data[ad_id == 120200424266500656, .(ad_id, eventdate, fb_spend, net_revenue, roas, adset_name, fb_impressions, fb_clicks_all, fb_leads, fb_link_click)] 



names(mnt_data)
mnt_data[ad_id ==23861798569440608,.(ad_id, eventdate, roas, buy_side_ctr, buy_side_cpm, cpp, cpc, rpc ,rpp)] 

pixy[SUBID2 == mnt_data[ad_id ==23861798569440608,campaign_name][1]][,.N,.(EVENTDATE, KEYWORD)]

str(pixy)
# count keywords by 



tb = mnt_data[(buyer_name %in% winner_buyers & ad_id %in% non_vaccine_winning_campaigns),campaign_name]
tb = mnt_data[(buyer_name %in% winner_buyers & ad_id %in% non_vaccine_winning_campaigns)]
fwrite(tb, "tb.csv")

names(tb)
tb = tb[,.(ad_id, eventdate, net_revenue, adset_name, fb_impressions,  fb_leads, fb_link_click, fb_spend, roas, net_revenue, contrib, fb_clicks_all, fb_impressions, fb_leads, fb_link_click, buy_side_ctr, buy_side_cpm, cpp, cpc, rpc, rpp, buy_side_cpm, buy_side_ctr, campaign_active_days)]


tb[,.(total_campaign_contrib, campaign_active_days)] %>% DataExplorer::plot_scatterplot(by = "campaign_active_days")


setkey(tb, campaign_name)
setkey(pixy, SUBID2)

str(pixy)

 pixy[SUBID2 == mnt_data[ad_id == 23861798569440608, campaign_name][1]][
  ,
  # .(keywords = paste(KEYWORD, collapse = ", ")),
  by = EVENTDATE
 ]


top_buyers_kws = pixy[SUBID2 %in% tb[,.(campaign_name)]]
top_buyers_kws = pixy[SUBID2 %in% tb]

fwrite(top_buyers_kws, "top_buyers_kws.csv")

str(top_buyers_kws)


top_buyers_kws[, .(sum_actual_net_revenue = sum(ACTUAL_NET_REVENUE,na.rm = TRUE)),.(CAMPAIGN_NAME,KEYWORD)][order(-sum_actual_net_revenue)][1:20]
# [!duplicated(CAMPAIGN_NAME),.N]

top_buyers_kws[,.N,adset_name][order(-N)]

top_buyers_kws[, .(sum_actual_net_revenue = sum(ACTUAL_NET_REVENUE,na.rm = TRUE)),.(CAMPAIGN_NAME)][order(-sum_actual_net_revenue)]




# top_buyers_kws[, .(
#   sum_actual_net_revenue = sum(ACTUAL_NET_REVENUE,na.rm = TRUE),
#   keywords = paste(KEYWORD, collapse = ", ")  
#   ),
#   .(CAMPAIGN_NAME)][order(-sum_actual_net_revenue)]

# pixy[SUBID2 == mnt_data[ad_id == 23861798569440608, campaign_name][1]][
# ,
# .(keywords = paste(KEYWORD, collapse = ", ")),
# by = EVENTDATE



# most revenue producing keywords -- maybe we can use to jump to similar 
# check correlation between ... early warning 
# correlation between the first x days of whatever metric and correlation with profitability 



install.packages("correlationfunnel")
library(correlationfunnel)
correlationfunnel::binarize()

correlationfunnel::binarize(tb[, .("adset_name", "fb_spend", "net_revenue", "roas", "fb_impressions", "fb_clicks_all", "fb_leads", "fb_link_click", "buy_side_ctr", "buy_side_cpm", "cpp", "cpc", "rpc", "rpp", "buy_side_cpm", "buy_side_ctr", "campaign_active_days"))

tb[, .SD, .SDcols = c("adset_name", "fb_spend", "net_revenue", "roas", "fb_impressions", "fb_clicks_all", "fb_leads", "fb_link_click", "buy_side_ctr", "buy_side_cpm", "cpp", "cpc", "rpc", "rpp", "buy_side_cpm", "buy_side_ctr")]


grepl("vaccine", campaign_name, ignore.case = TRUE) 

contrib = mnt_data[,sum(contrib),ad_id]
positive_contrib =  contrib[V1 >= 10]
length(positive_contrib$V1)   


mnt_data[, prftble:= fifelse(sum(contrib,na.rm = TRUE) >=10, 1, 0), ad_id] 
mnt_data[!duplicated(ad_id), .N, prftble,] 

nafill(mnt_data)
setnafill(mnt_data, fill = 0)

for (col in names(dt)) set(dt, i = which(is.na(dt[[col]])), j = col, value = 0)

library(ggplot2)

mnt_data_binarized <- mnt_data[, .SD, .SDcols = c("roas", "fb_impressions", "fb_clicks_all",
                                                  "fb_leads", "fb_link_click", "buy_side_ctr", "cpp", "cpc", "rpc", "rpp",
                                                  "buy_side_cpm", "campaign_active_days", "prftble")] %>%
    na.omit() %>%
    correlationfunnel::binarize(n_bins = 20, thresh_infreq = 0.01,)

marketing_campaign_correlated_tbl <- mnt_data_binarized %>%
    correlationfunnel::correlate(target = prftble__1)


marketing_campaign_correlated_tbl

t = as.data.table(mnt_data_correlated)
t_2 = t[feature != "prftble"][order(-correlation)]

t_2%>% 
  hchart("bar", hcaes(x = feature_bin, y = correlation)) %>%
  hc_xAxis(type = "category") %>%
  hc_plotOptions(series = list(color = 'rgba(50, 171, 96, 0.6)', borderColor = 'rgba(50, 171, 96, 1.0)', borderWidth = 1)) %>%
  hc_tooltip(pointFormat = '%{point.y:,.4f}') %>% 
  hc_title(text = "Correlation of Features and Bins with Profitability filtered to first 3 days")



t_posit = t[correlation > 0]

DT = data.table(x1 = 1:5, x2 = 6:10, y1 = letters[1:5], y2 = letters[6:10])
# melt all columns that begin with 'x' & 'y', respectively, into separate columns
DT[, .SD, .SDcols = patterns("^x")]
melt(DT, measure.vars = patterns("^x", "^y", cols=names(DT)))

names(mnt_data_binarized)

setDT(mnt_data_binarized)
mnt_data_binarized[, .SD, .SDcols = patterns("^buy_side_ctr")]%>% str


print(mnt_data_correlated, n=230)

correlation_funnel_plot <- marketing_campaign_correlated_tbl %>%
    correlationfunnel::plot_correlation_funnel(interactive = FALSE)

ggsave("correlation_funnel_plot.png", plot = correlation_funnel_plot, width = 10, height = 8, dpi = 300)



# 1. Filter the data to include only the first 3 days of each campaign
mnt_data_filtered <- mnt_data[, .SD[eventdate <= min(eventdate) + 3], by = ad_id]
mnt_data[, prftble:= fifelse(sum(contrib,na.rm = TRUE) >=10, 1, 0), ad_id] 
# 2. Aggregate the metrics for each campaign during the initial days
mnt_data_aggregated <- mnt_data_filtered[, .(
  fb_impressions = sum(fb_impressions),
  fb_clicks_all = sum(fb_clicks_all),
  fb_leads = sum(fb_leads),
  fb_link_click = sum(fb_link_click),
  buy_side_ctr = sum(fb_link_click) / sum(fb_impressions),
  cpp = sum(fb_spend) / sum(fb_leads),
  cpc = sum(fb_spend) / sum(fb_clicks_all),
  rpc = sum(net_revenue) / sum(fb_clicks_all),
  rpp = sum(net_revenue) / sum(fb_leads),
  buy_side_cpm = sum(fb_spend) / sum(fb_impressions) * 1000,
  prftble
), by = ad_id]

library(recipes)
cols2= c("buy_side_ctr", "cpp", "cpc", "rpc", "rpp",
          "buy_side_cpm", "prftble", "fb_impressions", "fb_clicks_all","fb_leads", "fb_link_click")

install.packages("correlationfunnel")
# 3. Binarize the aggregated data
mnt_data_binarized <- mnt_data_aggregated[, .SD, .SDcols = cols2] %>% na.omit() %>% correlationfunnel::binarize(n_bins = 20, thresh_infreq = 0.01)

# 4. Perform correlation analysis
mnt_data_correlated <- mnt_data_binarized %>%
  correlationfunnel::correlate(target = prftble__1)

# 5. Visualize the correlation funnel
correlation_funnel_plot <- mnt_data_correlated %>%
  correlationfunnel::plot_correlation_funnel(interactive = FALSE)

ggsave("early_campaign_correlation_funnel.png", plot = correlation_funnel_plot, width = 10, height = 8, dpi = 300)


install.packages("rpart")
library(rpart)

# 1. Filter the data to include only the first 3 days of each campaign
mnt_data_filtered <- mnt_data[, .SD[eventdate <= min(eventdate) + 3], by = ad_id]

# 2. Aggregate the metrics for each campaign during the initial days
mnt_data_aggregated <- mnt_data_filtered[, .SD, .SDcols= cols2]

# 3. Build the decision tree
decision_tree <- rpart(prftble ~ ., data = mnt_data_aggregated, method = "class")

# 4. Print the decision tree
print(decision_tree)

# 5. Visualize the decision tree
plot(decision_tree, uniform = TRUE, main = "Decision Tree")
text(decision_tree, use.n = TRUE, all = TRUE, cex = 0.8)


# Install rpart.plot package if not already installed
# install.packages("rpart.plot")

# Load rpart.plot package
library(rpart.plot)

# Visualize the decision tree using rpart.plot
rpart.plot(decision_tree, main = "Decision Tree", box.palette = "RdBu", fallen.leaves = TRUE)


# Save the plot as a PNG file
png("decision_tree_plot.png", width = 800, height = 600)
rpart.plot(decision_tree, main = "Decision Tree", box.palette = "RdBu", fallen.leaves = TRUE)
dev.off()


 install.packages("partykit")

library(partykit)

# Convert rpart object to partykit object
decision_tree_party <- as.party(decision_tree)

# Visualize the decision tree using partykit
plot(decision_tree_party, main = "Decision Tree")



install.packages("corrplot")
library(corrplot)

corrplot(cor(mnt_data_aggregated[, .SD, .SDcols = cols2] %>%na.omit() ), method = 'shade', order = 'AOE', diag = FALSE)   



plot_correlation_funnel <- function(data, interactive = FALSE, limits = c(-1, 1), alpha = 1, show_values = FALSE) {
    # ... (keep the existing code)
    
    if (!interactive) {
        g <- data %>%
            ggplot2::ggplot(ggplot2::aes(x = correlation, y = feature, text = bin)) +
            
            # Geometries
            ggplot2::geom_vline(xintercept = 0, linetype = 2, color = "red") +
            ggplot2::geom_point(color = "#2c3e50", alpha = alpha) +
            ggrepel::geom_text_repel(ggplot2::aes(label = bin), size = 3, color = "#2c3e50")
        
        if (show_values) {
            g <- g + ggplot2::geom_text(ggplot2::aes(label = round(correlation, 3)), hjust = -0.2, size = 3)
        }
        
        g <- g +
            # Formatting
            ggplot2::scale_x_continuous(limits = limits) +
            theme_tq() +
            ggplot2::labs(title = "Correlation Funnel")
        
        return(g)
    }
    
    # ... (keep the existing code for interactive plot)
}
library(dplyr)
library(ggplot2)
library(patchwork)

# Create a new column combining feature and bin
mnt_data_correlated <- mnt_data_correlated %>%
  mutate(feature_bin = paste(feature, bin, sep = "_"))

# Filter out the "prftble" rows
mnt_data_filtered <- mnt_data_correlated %>%
  filter(!grepl("prftble", feature))

# Add a new column "group" based on the sign of correlation
mnt_data_filtered <- mnt_data_filtered %>%
  mutate(group = ifelse(correlation >= 0, "Positive", "Negative"))

# Determine the maximum absolute correlation value
max_correlation <- max(abs(mnt_data_filtered$correlation))

# Create the pyramid plot
pyramid_chart(
  data = mnt_data_filtered,
  x = feature_bin,
  y = correlation,
  group = group,
  bar_colors = c("#FF7F0E", "#1F77B4"),
  sort = "descending",
  xlab = "Feature and Bin",
  title = "Correlation of Features and Bins with Profitability"
) +
  scale_y_continuous(
    name = "Correlation",
    limits = c(-max_correlation, max_correlation)
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

  library(ggplot2)  

# Assuming 'mnt_data_correlated' is already loaded in your R environment
# Filter the data for the 'prftble' feature
prftble_data <- mnt_data_correlated %>%
  filter(feature == "prftble") %>%
  arrange(feature, bin)

# Create a pyramid chart
ggplot(prftble_data, aes(x = reorder(feature_bin, correlation), y = correlation, fill = bin)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("0" = "blue", "1" = "red")) +
  labs(x = "Feature Bin", y = "Correlation", fill = "Profitable") +
  theme_minimal()




library(rpart)
cols2= c("buy_side_ctr", "cpp", "cpc", "rpc", "rpp",
          "buy_side_cpm", "prftble", "fb_impressions", "fb_clicks_all","fb_leads", "fb_link_click")
mnt_data[, prftble:= fifelse(sum(contrib,na.rm = TRUE) >=10, 1, 0), ad_id] 
mnt_data_filtered <- mnt_data[, .SD[eventdate <= min(eventdate) + 3], by = ad_id]
mnt_data_aggregated <- mnt_data_filtered[, .(
  fb_impressions = sum(fb_impressions),
  fb_clicks_all = sum(fb_clicks_all),
  fb_leads = sum(fb_leads),
  fb_link_click = sum(fb_link_click),
  buy_side_ctr = sum(fb_link_click) / sum(fb_impressions),
  cpp = sum(fb_spend) / sum(fb_leads),
  cpc = sum(fb_spend) / sum(fb_clicks_all),
  rpc = sum(net_revenue) / sum(fb_clicks_all),
  rpp = sum(net_revenue) / sum(fb_leads),
  buy_side_cpm = sum(fb_spend) / sum(fb_impressions) * 1000,
  prftble
), by = ad_id]
mnt_data_binarized <- mnt_data_aggregated[, .SD, .SDcols = cols2] %>% na.omit() %>% correlationfunnel::binarize(n_bins = 20, thresh_infreq = 0.01)
mnt_data_correlated <- mnt_data_binarized %>%
  correlationfunnel::correlate(target = prftble__1)
correlation_funnel_plot <- mnt_data_correlated %>%
  correlationfunnel::plot_correlation_funnel(interactive = FALSE)
correlation_funnel_plot

mnt_data[ad_id==120202333439710656 ]
