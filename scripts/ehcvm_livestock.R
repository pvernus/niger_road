---
title: "Tropical Livestock Unit"
format: html
editor: visual
---

```{r}

s17_me_agric <- read_dta('data_raw/ner_2018_ehcvm/s17_me_ner2018.dta') %>% 
  select(grappe, menage, espece_code1 = s17q01, espece_code2 = s17q02, qte_livestock = s17q05) %>% 
  mutate(espece_code1 = as_factor(espece_code1),
         espece_code2 = as_factor(espece_code2))

livestock <- left_join(survey_welfare %>% select(grappe, menage, hhid), s17_me_agric, by = c("grappe", "menage")) %>% 
  mutate(
    espece_code2 = as_factor(espece_code2),
    TLU = case_when(
      espece_code2 == 'Bovins (boeufs)' ~ .5,
      espece_code2 == 'Ovins (Moutons)' ~ .1,
      espece_code2 == 'Caprins (Chèvres)' ~ .1,
      espece_code2 == 'Camelins (Chameaux)' ~ .7,
      espece_code2 == 'Equins (Chevaux)' ~ .5,
      espece_code2 == 'Asins (Anes)' ~ .6,
      espece_code2 == 'Porcins (Cochons)' ~ .2,
      espece_code2 == 'Lapins' ~ .02,
      espece_code2 == 'Poules / poulets' ~ .01,
      espece_code2 == 'Pintades' ~ .01,
      espece_code2 == 'Autres volailles' ~ .01
    )
  )

livestock <- livestock %>% 
  mutate(livestock_tlu = sum(qte_livestock * TLU), .by = c(grappe, hhid)) %>% 
  mutate(large_livestock = if_else(espece_code2 %in% c('Bovins (boeufs)', 'Camelins (Chameaux)'), 1, 0)) %>% 
  mutate(
    livestock_tlu_total = sum(livestock_tlu, na.rm = TRUE),
    livestock_tlu_avg = mean(livestock_tlu, na.rm = TRUE),
    livestock_tlu_median = median(livestock_tlu, na.rm = TRUE),
    livestock_large_pct = 100 * sum(large_livestock, na.rm = TRUE) / n(),
    .by = grappe
  )







  
save(livestock, file = here('data', 'livestock.RData'))

```

| Animal category        | TLU (Ahmed and Mesfin, 2017) | TLU (Rout, Kumar, and Behera, 2019) |
|------------------------|------------------------------|-------------------------------------|
| Calf                   | 0.25                         |                                     |
| Donkey (young)         | 0.35                         |                                     |
| Weaned Calf            | 0.34                         |                                     |
| Camel                  | 1.25                         | 0.70                                |
| Heifer                 | 0.75                         |                                     |
| Sheep and goat (adult) | 0.13                         | 0.10                                |
| Cow and ox             | 1.00                         | 0.50                                |
| Sheep and goat (young) | 0.06                         |                                     |
| Horse                  | 1.10                         | 0.50                                |
| Chicken                | 0.013                        | 0.01                                |
| Donkey (adult)         | 0.70                         | 0.60                                |
| Rabbit                 |                              | 0.02                                |
| Pig                    |                              | 0.20                                |

Source: (1) [The impact of agricultural cooperatives membership on the wellbeing of smallholder farmers: empirical evidence from eastern Ethiopia \| Agricultural and Food Economics \| Full Text (springeropen.com)](https://agrifoodecon.springeropen.com/articles/10.1186/s40100-017-0075-z); (2) [Goat production challenges to food security. \| Goat production and supply chain management in the tropics (cabidigitallibrary.org)](https://www.cabidigitallibrary.org/doi/abs/10.1079/9781789240139.0001)
