grappe_gps_ner2018 <- read_dta('data_raw/ner_2018_ehcvm/grappe_gps_ner2018.dta')
# agric
s16a_me_agric <- read_dta('data_raw/ner_2018_ehcvm/s16a_me_ner2018.dta')
s16c_me_agric <- read_dta('data_raw/ner_2018_ehcvm/s16c_me_ner2018.dta')

## STOCK ##

stock <- s16c_me_agric %>% select(grappe, menage, 
                                  champ = s16cq02, 
                                  parcelle = s16cq03, 
                                  culture = s16cq04, 
                                  methd_stockage = s16cq20,
                                  alrdy_stock = s16cq21,
                                  alrdy_sold = s16cq15,
                                  qte_conso = s16cq13a,
                                  qte_sold = s16cq16a,
                                  qte_stock = s16cq22a)

# Share of total production stocked by grappe
stock_prod <- stock %>% 
  mutate(qte_stock = if_else(alrdy_stock == 2, 0, qte_stock),
         qte_sold = if_else(alrdy_sold == 2, 0, qte_sold),
         qte_total = qte_conso + qte_sold + qte_stock) %>% 
  group_by(grappe) %>% 
  summarize(qte_stock = sum(qte_stock),
            qte_total = sum(qte_total),
            prod_stock = 100 * (qte_stock / qte_total)
  )

stock_prod <- left_join(grappe_gps_ner2018 %>% select(grappe), stock_prod, by = 'grappe') %>% 
  replace_na(list(qte_stock = 0, qte_total = 0, pct_stock = 0))