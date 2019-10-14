install.packages("xts")
install.packages("flexdashboard")
install.packages('DBI')
install.packages('RMySQL')
install.packages("tidyquant")
install.packages("PerformanceAnalytics")
install.packages("ggplot")
install.packages("rmarkdown")
install.packages("shiny")
install.packages("timetk")
install.packages("tibbletime")
install.packages("highcharter")
install.packages("ludridate")
library(highcharter)
library(tibbletime)
library(timetk)
library(shiny)
library(rmarkdown)
library(ggplot2)
library(xts)
library(DBI)
library(scales)
library(RMySQL)
library(purrr)
library(tidyverse)
library(tidyquant)
library(ludridate)
#???ĳɳ?֤ȯͶ?ʻ??? 000001
#?к???ת??ծȯծȯ??֤ȯͶ?ʻ???A?? 000003
#??��????֤ҽҩ100ָ??֤ȯͶ?ʻ??? 000059
#?㷢?��????ز?ָ??֤ȯͶ?ʻ???????Ԫ??000180
mydb= dbConnect(MySQL(),user='ktruc002', password='35442fed', dbname='cn_stock_quote', host='172.19.3.250')
SQL_statement<- "SELECT  `end_date`,  `fund_code`, `acc_net_value`
FROM `cn_fund`.`net_value`
WHERE fund_code IN (000001,000003,000059,000180)
ORDER BY `end_date` DESC"

funds <- dbGetQuery(mydb,SQL_statement)

funds <- reshape(funds, idvar = "end_date", timevar = "fund_code", direction = "wide")
funds <- zoo::na.locf(funds)
time <- funds$end_date %>% as.Date()
funds <- xts(funds[2:5],time)
# get asset monthly return seires
funds <- to.period(funds,period = "months")
asset_returns_xts <-
  Return.calculate(funds,
                   method = "log") %>%
  na.omit()

asset_returns_xts <- asset_returns_xts["201309/201909"]
names(asset_returns_xts) <- c("funds.000001","funds.000003","funds.000059","funds.000180")
###4.0
#compute portfolio covariance matrix
covariance_matrix <- cov(asset_returns_xts)
round(covariance_matrix,5)

# genertate  weights and calculate portfolio standard deviation
w = c(0.25,0.3,0.25,0.2)
# calculate standard deviation of portfolio
sd_matrix_algebra <- sqrt(t(w) %*% covariance_matrix %*% w)
#calculate std in percent
sd_matrix_algebra_percent <-
  round(sd_matrix_algebra * 100, 2) %>%
  `colnames<-`("standard deviation")
sd_matrix_algebra_percent

###4.1 
# calculate standard deviation using Performance Analytics
portfolio_sd_xts_builtin <-
  PerformanceAnalytics::StdDev(asset_returns_xts, weights = w)
#std in percent
portfolio_sd_xts_builtin_percent <-
  round(portfolio_sd_xts_builtin * 100, 2)

portfolio_sd_xts_builtin_percent

###4.2
# calculate standard deviation using tidyverse
assets_returns_dplyr <- tk_tbl(asset_returns_xts)
portfolio_returns_dplyr_byhand <- assets_returns_dplyr[1]
portfolio_returns_dplyr_byhand <- add_column(portfolio_returns_dplyr_byhand,returns = 0)
for (i in c(1,2,3,4)){
  portfolio_returns_dplyr_byhand["returns"] = portfolio_returns_dplyr_byhand["returns"] + w[i]*assets_returns_dplyr[i+1]
}

portfolio_sd_tidy_builtin_percent <-
  portfolio_returns_dplyr_byhand %>%
  summarise(
    sd = sd(returns),
    sd_byhand =
      sqrt(sum((returns - mean(returns))^2)/(nrow(.)-1))) %>%
  mutate(dplyr = round(sd, 4) * 100,
         dplyr_byhand = round(sd_byhand, 4) * 100)

portfolio_sd_tidy_builtin_percent %>%
  select(dplyr, dplyr_byhand)
portfolio_sd_tidy_builtin_percent
###4.3
#calculate standard deviation using tidyquant
asset_returns_long <- assets_returns_dplyr %>% gather (assets,returns,-index) %>% group_by(assets)

portfolio_returns_tq_rebalanced_monthly  <- asset_returns_long %>%
      tq_portfolio(assets_col = assets,
                   returns_col = returns,
                   weights = w,
                   col_rename = "returns",
                   rebalance_on = "months")

portfolio_sd_tidyquant_builtin_percent <- tq_performance(portfolio_returns_tq_rebalanced_monthly,
                                             Ra = returns,
                                             Rb = NULL,
                                             performance_fun = table.Stats) %>%
                                             select(Stdev) %>% 
                                             mutate(tq_sd = round(Stdev,4)*100)
# demonstrate several Stds calculated by different methods
portfolio_sd_tidy_builtin_percent %>%
  select(dplyr, dplyr_byhand) %>%
  mutate(xts_builtin = portfolio_sd_xts_builtin_percent,
         matrix = sd_matrix_algebra_percent,
         tq = portfolio_sd_tidyquant_builtin_percent$tq_sd)


###4.4
#Visualizing Standard Deviation
#shows the scatter plot of monthly returns
portfolio_returns_dplyr_byhand %>%
  ggplot(aes(x = index, y = returns)) +
  geom_point(color = "cornflowerblue") +
  scale_x_date(breaks = pretty_breaks(n = 6)) +
  ggtitle("Scatterplot of Returns by Date") +
  theme(plot.title = element_text(hjust = 0.5))

#create indicators sd_plot/mean_plot
sd_plot <-
  sd(portfolio_returns_tq_rebalanced_monthly$returns)
mean_plot <-
  mean(portfolio_returns_tq_rebalanced_monthly$returns)
# set the returns that deviate from the mean too much to red/green
portfolio_returns_tq_rebalanced_monthly %>%
  mutate(hist_col_red =
           if_else(returns < (mean_plot - sd_plot),
                   returns, as.numeric(NA)),
         hist_col_green =
           if_else(returns > (mean_plot + sd_plot),
                   returns, as.numeric(NA)),
         hist_col_blue =
           if_else(returns > (mean_plot - sd_plot) &
                     returns < (mean_plot + sd_plot),
                   returns, as.numeric(NA))) %>%
  ggplot(aes(x = index)) +
  geom_point(aes(y = hist_col_red),
             color = "green") +
  geom_point(aes(y = hist_col_green),
             color = "red") +
  geom_point(aes(y = hist_col_blue),
             color = "blue") +
  labs(title = "Colored Scatter", y = "monthly returns") +
  scale_x_date(breaks = pretty_breaks(n = 8)) +
  theme(plot.title = element_text(hjust = 0.5))

## add a line for the value that is one standard deviation above and below the mean
portfolio_returns_tq_rebalanced_monthly %>%
  mutate(hist_col_red =
           if_else(returns < (mean_plot - sd_plot),
                   returns, as.numeric(NA)),
         hist_col_green =
           if_else(returns > (mean_plot + sd_plot),
                   returns, as.numeric(NA)),
         hist_col_blue =
           if_else(returns > (mean_plot - sd_plot) &
                     returns < (mean_plot + sd_plot),
                   returns, as.numeric(NA))) %>%
  ggplot(aes(x = index)) +
  geom_point(aes(y = hist_col_red),
             color = "red") +
  geom_point(aes(y = hist_col_green),
             color = "green") +
  geom_point(aes(y = hist_col_blue),
             color = "blue") +
  geom_hline(yintercept = (mean_plot + sd_plot),
             color = "purple",
             linetype = "dotted") +
  geom_hline(yintercept = (mean_plot-sd_plot),
             color = "purple",
             linetype = "dotted") +
  labs(title = "Colored Scatter with Line", y = "monthly returns") +
  scale_x_date(breaks = pretty_breaks(n = 8)) +
  theme(plot.title = element_text(hjust = 0.5))

## visualize the actual standard deviation of our portfolio
asset_returns_long %>%
  group_by(assets) %>%
  summarize(sd = 100 *sd(returns)) %>%
  add_row(assets = "Portfolio",
          sd = portfolio_sd_tidy_builtin_percent$dplyr) %>%
  ggplot(aes(x = assets,
             y = sd,
             colour = assets)) +
  geom_point() +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_text(
    aes(x = "Portfolio",
        y =
          portfolio_sd_tidy_builtin_percent$dplyr + .2),
    label = "Portfolio",
    color = "cornflowerblue") +
  labs(y = "standard deviation")

## visualizing expected monthly returns 
asset_returns_long %>%
  group_by(assets) %>%
  summarise(expected_return = mean(returns),
            stand_dev = sd(returns)) %>%
  add_row(assets = "Portfolio",
          stand_dev =
            sd(portfolio_returns_tq_rebalanced_monthly$returns),
          expected_return =
            mean(portfolio_returns_tq_rebalanced_monthly$returns)) %>%
  ggplot(aes(x = stand_dev,
             y = expected_return,
             color = assets)) +
  geom_point(size = 2) +
  geom_text(
    aes(x =
          sd(portfolio_returns_tq_rebalanced_monthly$returns) * 1.11,
        y =
          mean(portfolio_returns_tq_rebalanced_monthly$returns),
        label = "Portfolio")) +
  ylab("expected return") +
  xlab("standard deviation") +
  ggtitle("Expected Monthly Returns versus Risk") +
  scale_y_continuous(labels = function(x){ paste0(x, "%")}) +
  # The next line centers the title
  theme_update(plot.title = element_text(hjust = 0.5))

###4.5 Rolling Standard Deviation
###4.6 Rolling Standard Deviation in the xts world
window <- 24
# calculate the portfolio's monthly return based on xts
portfolio_returns_xts_rebalanced_monthly <-
  Return.portfolio(asset_returns_xts,
                   weights = w,
                   rebalance_on = "months") %>%
  `colnames<-`("returns")
# calculate rolling standard deviation based on xts
port_rolling_sd_xts <-
  rollapply(portfolio_returns_xts_rebalanced_monthly,
            FUN = sd,
            width = window) %>%
            na.omit() %>%
            `colnames<-`("rolling_sd")
###4.7 rolling standard deviation based on tidyverse
port_rolling_sd_tidy_does_not_work <-
  portfolio_returns_dplyr_byhand %>%
  mutate(rolling_sd = rollapply(returns,
                                FUN = sd,
                                width = window,
                                fill = NA)) %>%
  select(index, rolling_sd) %>%
  na.omit()

###4.8 Rolling Standard Deviation with the tidyverse

# define a rolling sd function based on rollify of tibbletime package
sd_roll_24 <-
  rollify(sd, window = window)
# calculate the rolling standard deviation using tibbletime
port_rolling_sd_tidy_tibbletime <-
  portfolio_returns_tq_rebalanced_monthly %>%
  as_tbl_time(index = index) %>%
  mutate(sd = sd_roll_24(returns)) %>%
  select(-returns) %>%
  na.omit()
tail(port_rolling_sd_tidy_tibbletime, 3)

###4.9 Rolling Standard Deviation in the tidyquant world
## calculate std using tidyquant
port_rolling_sd_tq <- 
  portfolio_returns_tq_rebalanced_monthly %>%
  tq_mutate(mutate_fun = rollapply,
            width = window,
            FUN = sd,
            col_rename = "rolling_sd") %>%
  select(index, rolling_sd) %>%
  na.omit()
## confirm three different methods
port_rolling_sd_tidy_tibbletime %>%
  mutate(sd_tq = port_rolling_sd_tq$rolling_sd,
         sd_xts = round(port_rolling_sd_xts$rolling_sd, 4)) %>%
  tail(3)

###4.10 Visualizing Rolling Standard Deviation in the xts world
##


port_rolling_sd_xts_hc <-
  round(port_rolling_sd_xts, 4) * 100

highchart(type = "stock") %>%
  hc_title(text = "24-Month Rolling Volatility") %>%
  hc_add_series(port_rolling_sd_xts_hc,
                color = "cornflowerblue") %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_yAxis(
    labels = list(format = "{value}%"),
    opposite = FALSE) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_exporting(enabled= TRUE) %>%
  hc_legend(enabled = TRUE)
###4.11 Visualizing Rolling Standard Deviation in the tidyverse
port_rolling_sd_tq %>%
  ggplot(aes(x = index)) +
  geom_line(aes(y = rolling_sd), color = "cornflowerblue") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(breaks = pretty_breaks(n = 8)) +
  labs(title = "Rolling Standard Deviation", y = "") +
  theme(plot.title = element_text(hjust = 0.5))


###4.12 Shiny App Starndard Deviation

