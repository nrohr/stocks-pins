## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(message=FALSE, warning=FALSE)

library(tidyquant)
library(tidyverse)
library(timetk)


## ------------------------------------------------------------------------
# The symbols vector holds our tickers. 
symbols <- c("SPY","EFA", "IJS", "EEM","AGG")

etf_monthly_returns <- 
  symbols %>% 
  tq_get(get = "stock.prices", from = "2003-10-01") %>% 
  group_by(symbol) %>% 
  tq_transmute(adjusted, periodReturn, period = "monthly", col_rename = "returns")

mult_monthly_returns_stocks <- tq_repeat_df(etf_monthly_returns, n = 10)


## ------------------------------------------------------------------------

weights <-  c(0.15, 0.15, 0.10, 0.10, 0.50,
              0.15, 0.15, 0.15, 0.10, 0.45,
              0.15, 0.15, 0.15, 0.15, 0.40,
              0.20, 0.15, 0.15, 0.15, 0.35,
              0.20, 0.20, 0.15, 0.15, 0.30,
              0.20, 0.20, 0.20, 0.15, 0.25,
              0.20, 0.20, 0.20, 0.20, 0.20,
              0.25, 0.20, 0.20, 0.20, 0.15,
              0.25, 0.25, 0.20, 0.20, 0.10,
              0.25, 0.25, 0.25, 0.20, 0.05)

weights_table <- tibble(symbols) %>%
    tq_repeat_df(n = 10) %>%
    bind_cols(tibble(weights))%>%
    group_by(portfolio)

portfolio_returns_risk_levels <- 
  mult_monthly_returns_stocks %>%
  tq_portfolio(assets_col = symbol,
               returns_col = returns,
               weights = weights_table,
               col_rename = NULL,
               wealth.index = FALSE) %>% 
  spread(portfolio, portfolio.returns)


## ----Conservative Portfolio----------------------------------------------
w_cons <- c(0.25, 0.10, 0.10, 0.10, 0.45)
conservative_portfolio_returns <- 
  etf_monthly_returns %>% 
  tq_portfolio(assets_col = symbol, 
               returns_col = returns, 
               weights = w_cons,
               col_rename = "returns")


## ----Balanced Portfolio--------------------------------------------------
w_bal <- c(0.20, 0.20, 0.20, 0.20, 0.20)

balanced_portfolio_returns <- 
  etf_monthly_returns %>% 
  tq_portfolio(assets_col = symbol, 
               returns_col = returns, 
               weights = w_bal,
               col_rename = "returns")


## ----Aggressive Portfolio------------------------------------------------
w_agg <- c(0.25, 0.25, 0.25, 0.20, 0.05)

aggressive_portfolio_returns <- 
  etf_monthly_returns %>% 
  tq_portfolio(assets_col = symbol, 
               returns_col = returns, 
               weights = w_agg,
               col_rename = "returns")



## ------------------------------------------------------------------------
portfolio_allocations <- 
  aggressive_portfolio_returns %>% 
  mutate(conservative = conservative_portfolio_returns$returns,
         balanced = balanced_portfolio_returns$returns) %>% 
  rename(aggressive = returns)

all_returns <- conservative_portfolio_returns %>%
  mutate(balanced = balanced_portfolio_returns$returns + 1, 
         aggressive = aggressive_portfolio_returns$returns + 1,
         conservative = returns + 1) %>% 
  select(-date, -returns) %>% 
  map_dfc(., accumulate,  `*`) %>% 
  mutate(date = conservative_portfolio_returns$date) %>% 
  select(date, everything())


## ------------------------------------------------------------------------
library(pins)
board_register_rsconnect(name="rsconnect", server="https://colorado.rstudio.com/rsc/", key = Sys.getenv("rsc-api"))

pin(all_returns, "all_returns", board = "rsconnect")
pin(conservative_portfolio_returns, "conservative_portfolio_returns", board = "rsconnect")
pin(balanced_portfolio_returns, "balanced_portfolio_returns", board = "rsconnect")
pin(aggressive_portfolio_returns, "aggressive_portfolio_returns", board = "rsconnect")
pin(portfolio_returns_risk_levels, "portfolio_returns_risk_levels", board = "rsconnect")
pin(portfolio_allocations, "portfolio_allocations", board = "rsconnect")


## ------------------------------------------------------------------------
 all_returns %>% 
  gather(portfolio, return, -date) %>% 
  group_by(portfolio) %>% 
  ggplot(aes(x = date, y = return, color = portfolio)) +      
  geom_line() 



## ------------------------------------------------------------------------
portfolio_returns_risk_levels %>%
  gather(risk, return, -date) %>% 
  ggplot(aes(x = date, y = return, color = risk)) +
  geom_point() +
  facet_wrap(~risk, nrow = 3)


## ----email_setup, include=FALSE------------------------------------------
library(blastula)

all_returns %>% 
  write.csv("returns.csv", row.names = FALSE)

render_connect_email(input = "stocks-etl-email-template.Rmd") %>% 
  attach_connect_email(
    subject = "New stock return data available",
    attach_output = TRUE,
    attachments = c("returns.csv")
  )

