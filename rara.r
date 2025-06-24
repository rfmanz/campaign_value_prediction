#snippets folder----
# /C:/Users/rafit/AppData/Roaming/Cursor/User/snippets/r.json
#imports----
library(data.table)
library(Hmisc)
library(plotly)
library(DataExplorer)
library(summarytools)
library(reticulate)
library(xray)
library(dplyr)
# library(streamgraph)
library(highcharter)
library(lubridate)
py_run_string("import sys; sys.path.append('utils')")
amg_utils <- import("amg_utils")
#load----
# mnt_data = fread("marketeer_data_all.csv")
# fb_hourly = fread("fb_hourly.csv", drop =  "actions")
fb_hourly_p = fread("fb_hourly_p.csv")
#pull data from sql
# mnt_data = amg_utils$eq(paste(readLines("marketer_data.sql"), collapse = "\n"))
# r_to_py(mnt_data)$to_csv("marketeer_data_all_2.csv", index = FALSE)
# mnt_data = fread("marketeer_data_all_2.csv")
# r_to_py(mnt_data)$to_csv("marketer_data_only_us.csv", index = FALSE)
mnt_data = fread("marketer_data_only_us.csv")
#transforms----
mnt_data[, ad_id := as.character(ad_id)]
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
# filter dataset to only include campaigns between 5 and 30 days
# mnt_data[!duplicated(ad_id),.N]
# mnt_data[,.N,campaign_duration][order(campaign_duration)]
# mnt_data[,.N,campaign_active_days][order(campaign_active_days)]
# mnt_data = mnt_data[between(campaign_duration, 5, 90)]
mnt_data = mnt_data[between(campaign_active_days, 5, 90)]
# mnt_data[!duplicated(ad_id),.N]
#  from august 2023 to january 2024
# mnt_data[!duplicated(ad_id),.N]
# mnt_data = mnt_data[between(eventdate, as.Date("2023-05-01"), as.Date("2024-01-31"))]
# mnt_data[!duplicated(ad_id),.N]
setcolorder(mnt_data,sort(names(mnt_data)))
# str(mnt_data)
#join with fb_hourly----
fb_hourly_p[,ad_id:= as.character(ad_id)]
setkey(fb_hourly_p, ad_id, eventdate)
setkey(mnt_data, ad_id, eventdate)
mnt_data_hourly = fb_hourly_p[mnt_data]
# str(mnt_data_hourly)
# View(mnt_data_hourly[ad_id=="23852576758740191"])

#fb_hourly_graphs----
# Calculate average clicks from clicks_1 to clicks_24 by ad_id
clicks_table = melt(mnt_data_hourly[, .SD, .SDcols = paste0("clicks_", 1:24)])
impressions_table = melt(mnt_data_hourly[, .SD, .SDcols = paste0("impressions_", 1:24)])
spend_table = melt(mnt_data_hourly[, .SD, .SDcols = paste0("spend_", 1:24)])
ctr_table = melt(mnt_data_hourly[, .SD, .SDcols = paste0("ctr_", 1:24)])
cpc_table = melt(mnt_data_hourly[, .SD, .SDcols = paste0("cpc_", 1:24)])
cpm_table = melt(mnt_data_hourly[, .SD, .SDcols = paste0("cpm_", 1:24)])

clicks_mean <- clicks_table[, .(mean_value = mean(value, na.rm = TRUE)), by = variable] 
impressions_mean <- impressions_table[, .(mean_value = mean(value, na.rm = TRUE)), by = variable]
spend_mean <- spend_table[, .(mean_value = mean(value, na.rm = TRUE)), by = variable]
ctr_mean <- ctr_table[, .(mean_value = mean(value, na.rm = TRUE)), by = variable]
cpc_mean <- cpc_table[, .(mean_value = mean(value, na.rm = TRUE)), by = variable]
cpm_mean <- cpm_table[, .(mean_value = mean(value, na.rm = TRUE)), by = variable]

highchart() %>% 
  hc_chart(type = "line") %>% 
  hc_xAxis(categories = paste0("Hour ", 1:24)) %>%
  hc_add_series(data = clicks_mean, hcaes(x = variable, y = mean_value), type = "line", name = "Clicks Mean") %>% 
  hc_add_series(data = spend_mean, hcaes(x = variable, y = mean_value), type = "line", name = "Spend Mean") %>%
  hc_add_series(data = ctr_mean, hcaes(x = variable, y = mean_value), type = "line", name = "CTR Mean") %>%
  hc_add_series(data = cpc_mean, hcaes(x = variable, y = mean_value), type = "line", name = "CPC Mean") %>%
  hc_add_series(data = cpm_mean, hcaes(x = variable, y = mean_value), type = "line", name = "CPM Mean")
  # hc_add_series(data = impressions_mean, hcaes(x = variable, y = mean_value), type = "line", name = "Impressions Mean") %>% 

mnt_data %>%
  group_by(buyer_name) %>%
  summarise(total_contrib = sum(contrib, na.rm = TRUE)) %>%
  top_n(10, total_contrib) %>%
  arrange(desc(total_contrib)) %>%
  inner_join(mnt_data, by = "buyer_name") %>%
  group_by(eventdate, buyer_name) %>%
  summarise(contrib = sum(contrib, na.rm = TRUE)) %>%
  hchart(., type = "line", hcaes(x = eventdate, y = contrib, group = buyer_name))
  # hc_plotOptions(series = list(enableMouseTracking = TRUE, showInLegend = TRUE, visible = FALSE)) %>%
  hc_yAxis(opposite = FALSE, labels = list(format = "${value}")) %>% 
  hc_tooltip(pointFormat = '{point.x: %Y-%m-%d} ${point.y:.4f}')

mnt_data_hourly[, .(avg_clicks = rowMeans(.SD, na.rm = TRUE)), .SDcols = paste0("clicks_", 1:24), by = .(group = 1:nrow(mnt_data_hourly))] %>%
  melt(id.vars = "group", variable.name = "clicks", value.name = "avg_clicks") %>%
  hchart("column", hcaes(x = clicks, y = avg_clicks, group = group)) %>%
  hc_xAxis(title = list(text = "Clicks")) %>%
  hc_yAxis(title = list(text = "Average Clicks")) %>%
  hc_tooltip(pointFormat = 'Clicks: <b>{point.x}</b><br/>Average Clicks: <b>{point.y}</b>')

hchart( "column", hcaes(x = ad_id, y = avg_clicks)) %>%
  hc_title(text = "Average Clicks by Ad ID") %>%
  hc_xAxis(title = list(text = "Ad ID")) %>%
  hc_yAxis(title = list(text = "Average Clicks")) %>%
  hc_tooltip(pointFormat = 'Ad ID: <b>{point.x}</b><br/>Average Clicks: <b>{point.y}</b>')
#graphs ----
## steamgraph ----
# mnt_data %>%
#   group_by(buyer_name, eventdate) %>%
#   summarise(contrib = sum(contrib, na.rm = TRUE)) %>%
#   arrange(desc(contrib)) %>%
#   streamgraph("buyer_name", "contrib", "eventdate", offset="zero", interpolate="cardinal")

# aggregate contribution by buyer_name
mnt_data %>%
  group_by(buyer_name) %>%
  summarise(contrib = sum(contrib, na.rm = TRUE)) %>%
  arrange(desc(contrib)) %>%
  hchart("bar", hcaes(x = buyer_name, y = contrib)) %>%
  hc_xAxis(type = "category") %>%
  hc_plotOptions(series = list(color = 'rgba(50, 171, 96, 0.6)', borderColor = 'rgba(50, 171, 96, 1.0)', borderWidth = 1))

## campaigns per month  -----
mnt_data %>%
  group_by(eventdate) %>%
  summarise(unique_ads = n_distinct(ad_id)) %>%
  hchart("column", hcaes(x = eventdate, y = unique_ads))%>% hc_navigator(enabled = TRUE) %>% hc_rangeSelector(enabled = TRUE) 

# average line is wrong 
# mnt_data %>%
#   mutate(month_year = format(eventdate, "%Y-%m")) %>%
#   group_by(month_year) %>%
#   summarise(campaigns_per_month = n_distinct(ad_id)) %>%
#   arrange(month_year) %>%
#   plot_ly(x = ~month_year, y = ~campaigns_per_month, type = 'bar',  marker = list(color = 'rgba(0, 0, 255, 1.0)', line = list(color = 'rgba(0, 0, 255, 1.0)', width = 1)), name = "Campaigns per Month") %>%
#   layout(showlegend = FALSE)

## campaings by buyer ----
mnt_data %>%
  group_by(buyer_name) %>%
  summarise(total_contrib = sum(contrib, na.rm = TRUE)) %>%
  top_n(10, total_contrib) %>%
  inner_join(mnt_data, by = "buyer_name") %>%
  count(buyer_name) %>%
  arrange(n) %>%
  hchart("column", hcaes(x = buyer_name, y = n)) %>%
  hc_xAxis(type = "category") %>%
  hc_plotOptions(series = list(color = 'rgba(50, 171, 96, 0.6)', borderColor = 'rgba(50, 171, 96, 1.0)', borderWidth = 1))

# mnt_data[,.(N=.N, total_contrib = sum(contrib, na.rm = TRUE)), buyer_name][order(total_contrib, decreasing = TRUE)][1:10]

## contribution total by buyer ----
mnt_data %>%
  group_by(buyer_name) %>%
  summarise(contrib = sum(contrib, na.rm = TRUE)) %>%
  arrange(desc(contrib)) %>%
  hchart("bar", hcaes(x = buyer_name, y = contrib)) %>%
  hc_xAxis(type = "category") %>%
  hc_plotOptions(series = list(color = 'rgba(50, 171, 96, 0.6)', borderColor = 'rgba(50, 171, 96, 1.0)', borderWidth = 1))

## total contribution over time line graph  ----
# mnt_data %>%
#   group_by(eventdate) %>%
#   summarise(total_contrib = sum(contrib, na.rm = TRUE)) %>%
#   plot_ly(x = ~eventdate, y = ~total_contrib, type = 'scatter', mode = 'lines') %>%
#   layout(yaxis = list(title = 'Total Contribution'), xaxis = list(title = 'Event Date'))
  
mnt_data %>%
  group_by(eventdate) %>%
  summarise(total_contrib = sum(contrib, na.rm = TRUE)) %>%
  hchart("area", hcaes(x = eventdate, y = total_contrib)) %>% hc_tooltip(pointFormat = '{series.name}: <b>${point.y:,.0f}</b><br/>') %>%
  hc_title(text = "Total Contribution over Time")
## hchart top 10 buyers by total contribution over time ----
mnt_data %>%
  group_by(buyer_name) %>%
  summarise(total_contrib = sum(contrib, na.rm = TRUE)) %>%
  top_n(10, total_contrib) %>%
  arrange(desc(total_contrib)) %>%
  inner_join(mnt_data, by = "buyer_name") %>%
  group_by(eventdate, buyer_name) %>%
  summarise(contrib = sum(contrib, na.rm = TRUE)) %>%
  hchart(., type = "line", hcaes(x = eventdate, y = contrib, group = buyer_name)) %>% 
  # hc_plotOptions(series = list(enableMouseTracking = TRUE, showInLegend = TRUE, visible = FALSE)) %>%
  hc_yAxis(opposite = FALSE, labels = list(format = "${value}")) %>% 
  hc_tooltip(pointFormat = '{point.x: %Y-%m-%d} ${point.y:.4f}')

##revenue vs spend----
data = mnt_data[, .(total_fb_spend = sum(fb_spend, na.rm = TRUE), total_net_revenue = sum(net_revenue, na.rm = TRUE), total_contrib = sum(contrib, na.rm = TRUE)), by = .(year_month)][order(year_month)]

hchart(data, "bar", hcaes(x = year_month, y = total_net_revenue), name = "Net Revenue") %>%
  hc_add_series(data, "bar", hcaes(x = year_month, y = total_fb_spend), name = "FB Spend") %>%
  hc_title(text = "FB SPEND vs Net Revenue") %>%
  hc_yAxis(title = list(text = "$")) %>%
  hc_xAxis(type = "category") %>%
  hc_tooltip(pointFormat = '{series.name}: <b>${point.y:,.0f}</b><br/>') %>%
  hc_annotations(list(    
    labels = list(
      list(point = list(x = 80, y = 350, yAxis = 100, xAxis = 380),
        text = 'Total_contrib: $10,987'),
      list(point = list(x = 200 , y = 350, yAxis = 240, xAxis = 350),
        text = 'Total_contrib: $1,462'),
      list(point = list(x = 300 , y = 330, yAxis = 400, xAxis = 330),
        text = 'Total_contrib: $15,569'),
        list(point = list(x = 420 , y = 500, yAxis = 550, xAxis = 500),
        text = 'Total_contrib:  $-1,385'),
            list(point = list(x = 520 , y = 800, yAxis = 710, xAxis = 800),
        text = 'Total_contrib:  $98,174'),
        list(point = list(x = 670 , y = 440, yAxis = 870, xAxis = 500),
        text = 'Total_contrib: $56,926')
  )))

##chart to look at winners and losers----
#global roas----
mnt_data %>%
  group_by(eventdate) %>%
  summarise(mean_roas = mean(roas, na.rm = TRUE)) %>%
  hchart("line", hcaes(x = eventdate, y = mean_roas)) %>% hc_tooltip(pointFormat = '{series.name}: <b>${point.y:,.0f}</b><br/>') %>% 
  hc_title(text = "Total Contribution (fb_spend - net_rev) over Time") %>% 
  hc_navigator(enabled = TRUE) %>% hc_rangeSelector(enabled = TRUE)  
#stats ----
## spend per month ----
mnt_data[,sum(fb_spend, na.rm = TRUE),year_month][order(year_month)]
## campaigns per month ----
# mnt_data[,.N,.(ad_id,year_month)][,.N,year_month][order(year_month)]
mnt_data[,.N,.(ad_id,year_month)][,.N,year_month][order(-N),.(year_month,N, pct = scales::percent(N/sum(N)))][order(year_month)]
## ad_id monthly mean contrib ----
mnt_data[, .(avg_contrib_by_campaign = sum(contrib, na.rm = TRUE), eventdate), by = .(year_month = format(as.Date(eventdate), "%Y-%m"), ad_id)][,mean(avg_contrib_by_campaign, na.rm = TRUE), by = .(year_month = format(as.Date(eventdate), "%Y-%m"))][order(year_month)]

## total contrib per month ----
mnt_data[,sum(contrib, na.rm = TRUE),year_month][order(year_month)]

## campaigns per buyer ----
mnt_data[,.N,.(ad_id,buyer_name,year_month)][,.N,.(buyer_name)][order(-N)]

mnt_data[,.N,.(ad_id,buyer_name, year_month)][,.N,.(buyer_name, year_month)][order(-N)][1:10]%>% dcast(buyer_name~year_month, value.var="N", fill = 0)

mnt_data[,.N,.(ad_id,buyer_name,year_month)][,.N,.(buyer_name, year_month)][order(-N),.(buyer_name,N, pct = scales::percent(N/sum(N)),year_month)]%>% dcast(buyer_name~year_month,value.var="N", fill = 0)

## global spend vs revenue ----
#made 
scales::dollar(mnt_data[,sum(contrib, na.rm = TRUE)])
#in
as.period(interval(mnt_data[,min(eventdate)], mnt_data[,max(eventdate)]))
# as.period(interval(mnt_data[,min(eventdate)], mnt_data[,max(eventdate)])) %>% month()
mnt_data[,.(total_fb_spend = sum(fb_spend,na.rm = TRUE), total_net_revenue = sum(net_revenue,na.rm = TRUE))][,.(total_fb_spend, total_net_revenue, total_profit = total_net_revenue - total_fb_spend)]
 
#positive contribution campaigns ----

# mnt_data[campaign_duration > 4, .SD[.GRP %in% c(1)], by = ad_id][,.(eventdate, ad_id, roas, net_revenue, fb_spend, total_campaign_revenue, total_campaign_roas, total_campaign_spend,total_campaign_contrib,campaign_duration)]
mnt_data[!duplicated(ad_id),.N]
contrib = mnt_data[,sum(contrib),ad_id]
positive_contrib =  contrib[V1 > 10]
length(positive_contrib$V1)
# distribution
mnt_data[!total_campaign_contrib %in% boxplot.stats(total_campaign_contrib)$out][ad_id %in% positive_contrib$ad_id, positive_contrib$V1[match(ad_id, positive_contrib$ad_id)]]%>% descr

plot_histogram(mnt_data[!total_campaign_contrib %in% boxplot.stats(total_campaign_contrib)$out][ad_id %in% positive_contrib$ad_id, positive_contrib$V1[match(ad_id, positive_contrib$ad_id)]])
plot_histogram(positive_contrib[V1<2000,V1], geom_histogram_args = list(binwidth = 5))
boxplot(positive_contrib$V1)

cuts = c(2.5,5,10,20,40,100)
cut2(positive_contrib$V1, cuts, formatfun = scales::label_comma(accuracy = 1, style_positive = "space")) %>% table

# total_campaign_contrib vs campaign_active_days
mnt_data[(ad_id %in% positive_contrib$ad_id) & !duplicated(ad_id) , .(total_campaign_contrib, campaign_active_days)] %>% DataExplorer::plot_scatterplot(by = "campaign_active_days")

# total_campaign_contrib vs total_campaign_spend 
mnt_data[(ad_id %in% positive_contrib$ad_id) & !duplicated(ad_id) , .(total_campaign_contrib, total_campaign_spend)] %>% DataExplorer::plot_scatterplot(by = "total_campaign_spend")

# mnt_data[!total_campaign_contrib %in% boxplot.stats(total_campaign_contrib)$out][(ad_id %in% positive_contrib$ad_id) & !duplicated(ad_id), .(total_campaign_contrib, total_campaign_spend, ad_id)][] %>%  
#   hchart("scatter", hcaes(x = total_campaign_contrib, y = total_campaign_spend, tooltip = ad_id)) %>%
#   hc_title(text = "Scatter Plot of Total Campaign Contribution vs. Total Campaign Spend") %>%
#   hc_yAxis(title = list(text = "Total Campaign Spend")) %>%
#   hc_xAxis(title = list(text = "Total Campaign Contribution")) %>%
#   hc_tooltip(pointFormat = 'Ad ID: {point.tooltip}<br>Total Campaign Spend: ${point.x}<br>Total Campaign Contribution: ${point.y}')

# look at the most succesful campaigns 
mnt_data[,ad_id:= as.character(ad_id)]

mnt_data[ad_id %in% positive_contrib$ad_id, .(ad_id, 
total_campaign_roas, 
campaign_name, 
adset_name,
total_campaign_contrib)][!duplicated(ad_id)][order(-total_campaign_roas)][1:40]


str(mnt_data)
mnt_data[ad_id == mnt_data[ad_id %in% positive_contrib$ad_id, .(ad_id, roas)][order(-roas)][1:20, ad_id][1], ]
mnt_data[ad_id == mnt_data[ad_id %in% positive_contrib$ad_id, .(ad_id, roas)][order(-roas)][1:20, ad_id][3], ]

# top roas campaigns 
# mnt_data[ad_id== positive_contrib[which.max(V1),ad_id],.(eventdate, ad_id, roas, net_revenue, fb_spend, total_campaign_revenue, total_campaign_roas, total_campaign_spend,total_campaign_contrib,campaign_duration, campaign_active_days, campaign_name, fb_clicks_all,fb_impressions, fb_leads)]


# mnt_data[ad_id== positive_contrib[which.max(V1),ad_id],.(eventdate, ad_id, roas, net_revenue, fb_spend, total_campaign_revenue, total_campaign_roas, total_campaign_spend,total_campaign_contrib,campaign_duration, campaign_active_days, campaign_name, fb_clicks_all,fb_impressions, fb_leads)]

mnt_data[ad_id==23858616155560464]
mnt_data[ad_id==23858646616450464, sum(net_revenue, na.rm = TRUE)]
mnt_data[ad_id==23858646616450464, sum(fb_spend, na.rm = TRUE)]
333.898889 / 87.46000
#kws----

kws = fread("/mnt/d/Downloads/pixy.csv", showProgress = TRUE,nrows = 1000)


#notes----

# Only work with campaings with a lifetime between 5 and 90 days from august 2023 to january 2024

#  1:    2023-03   72  0.36%
#  2:    2023-04  545  2.76%
#  3:    2023-05  801  4.05%
#  4:    2023-06 1051  5.32%
#  5:    2023-07 1108  5.61%
#  6:    2023-08 2099 10.63%
#  7:    2023-09 2507 12.69%
#  8:    2023-10 2048 10.37%
#  9:    2023-11 2899 14.68%
# 10:    2023-12 3173 16.06%
# 11:    2024-01 2014 10.20%
# 12:    2024-02 1437  7.27%

# 5:    2023-07  366  4.72%
#  7:    2023-09  972 12.53%
#  8:    2023-10  727  9.37%
#  6:    2023-08  754  9.72%
#  9:    2023-11 1076 13.87%
# 10:    2023-12 1335 17.21%
# 11:    2024-01  860 11.09%

# that 3365 campaigns 
# of thos 3365 campaigns 


#TODO----

# relation between revenue and 

# make a summary table 

# separate into winners and losers 

# metricis losers, winners , combined 

# break even roas campaigns 

# If your ROAS ranges from 0.80 to 1.20, it means that for every dollar you spend on FB ads, you are generating between 80 cents and $1.20 in net revenue.
# A ROAS of 0.80 indicates that you're losing 20 cents for every dollar spent, suggesting the campaign is not profitable.
# A ROAS of 1.00 means you're breaking even, with the revenue generated exactly matching the ad spend.
# A ROAS of 1.20 signifies a profitable campaign, where you earn $1.20 for every dollar spent, making a 20% profit on your investment.

# problematic data tab 

# first separate into winners and losers. levels of both. for instance we might be interested in understanding how we can close to break even roas campaigns 

#  for each level of winner, when did they convert?
#  analysis grouped into three main categories: 
#  winner 
#  loser 
#  both 
#  and then for each level of winner and loser 


mnt_data[!duplicated(ad_id), .N, eventdate] %>%
  hchart('area', hcaes(x = eventdate, y = N))

mnt_data[,{
  campaigns = .SD[!duplicated(ad_id),.N,];  
  list(rows= .N,
        columns = length(.SD),
        min_eventdate = min(eventdate), 
        max_eventdate = max(eventdate),         
        campaigns = campaigns, 
        total_profit = scales::dollar(sum(contrib, na.rm = TRUE)),
        time_span= as.period(interval(min(eventdate), max(eventdate))),        
        range_campaign_duration = paste(min(campaign_duration),max(campaign_duration), sep = " - "),
        range_campaign_active_days = paste(min(campaign_active_days),max(campaign_active_days), sep = " - ")
        )}]


# spend - based on marketing - trend on hourly 
# only US campaigns 
# focus analysis by marketer. understand succesful behaviour 
# try different groupings: by buyer, winner vs loser, ... 
# variables to add: keyword categories 
# methods to try: individual time series [patterns by campaign & grouping] 
# combination of rules based on trigger points (constructed from marketer exp) / in combo with forecasting model 

# to do a grouped analysis you gotta group. to group we gotta define the groups. 
# 2 approaches:
# look at the most succesful campaigns 
# split it into winners, losers and compare metrics. which metrics ? 

# winning campaigns start with a fb spend of x, hourly metrics look like this 


# Description

# Perform EDA analysis on Fb → Tonic Marketers data to draw insights and patterns that can guide prediction of campaign performance.

# Possible analysis areas-

# Identify distinct categories of campaigns based on volume, revenue, keyword-topics etc.

# Analyze time series trends per campaign

 

# DoD: A jupyter notebook with a data story of historical marketers data and insights.