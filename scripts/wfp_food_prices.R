
set_rhdx_config(hdx_site = "prod") # from the rhdx package
get_rhdx_config()

search_datasets("Niger - Food Prices", rows = 5)

food_prices <- pull_dataset("wfp-food-prices-for-niger") %>%
  get_resource(1) %>%
  read_resource() %>% 
  mutate(year = year(date)) %>% 
  filter(year >= 2010 & year < 2023) %>% 
  mutate(wet_season = if_else(month(date) %in% c(07:09), 1, 0)) # wet season = july, august, september

market_coverage <- food_prices %>% summarize(max = max(date), min = min(date), .by = "market")
# filter out markets with coverage <= 2010

food_prices <- food_prices %>% 
  filter(market %in% c('Chetimari', 'Falwel', 'Dan-Goulbi', 'Chadakori', 'Kanembakache', 
                       'Maidjirgui', 'Jataka', 'Bonfeba', 'Soubdou', 'Sassoumbroum', 'Guidimouni'))

food_prices_avg_year_deviation <- food_prices %>% 
  filter(priceflag == 'actual' &  pricetype == 'Retail') %>%
  group_by(month = floor_date(date, "month"), commodity) %>% 
  mutate(mean_year = mean(price),
         dev_mean_year = 100 * (price - mean_year) / mean_year,
         median_year = median(price),
         dev_median_year = 100 * (price - median_year) / median_year
         )

food_prices_year_variability <- food_prices %>% 
  filter(priceflag == 'actual' &  pricetype == 'Retail') %>%
  group_by(market, commodity, year) %>% 
  summarize(infra_annual_variability = sd(price))

# Total
food_prices %>%
  mutate(year = year(date)) %>% 
  filter(year >= 2010 & pricetype == 'Retail') %>% 
  group_by(month = floor_date(date, "month"), commodity) %>% 
  summarize(sd_retail_total = sd(price))
  
  
  
  group_by(month = floor_date(date, "month")) %>% 
  summarize(avg_price = mean(price))

# Category

# Commodity

# Rice local vs. imported
# retail vs. wholesome

# Wet season

# Retail vs. Wholesome
  



