# Data access

## Database

The data used for the analysis is stocked in a database (`data/niger_data.db`).

To connect to the database:

```{r}
library(DBI)
con <- dbConnect(RSQLite::SQLite(), "data/niger_data.db")
```

To list all the tables available in the database:

```{r}
dbListTables(con)
```

To extract a table (e.g. `tablename`) from the database:

```{r}
table_name <- dbGetQuery(con, "SELECT * FROM table_name")
```

## Primary/Foreign keys

|              |                  |     |                     |     |                          |     |                      |
|--------------|------------------|-----|---------------------|-----|--------------------------|-----|----------------------|
| **Key var.** | **Country code** |     | **ADM 1** (Regions) |     | **ADM 2** (Departements) |     | **ADM 3** (Communes) |
|              |                  |     | *[1;8]*             |     | *[1;67]*                 |     |                      |
| `id_adm1`    | NER              | 00  | X                   |     |                          |     |                      |
| `id_adm2`    | NER              | 00  | X                   | 0   | XX                       |     |                      |
| `id_adm3`    | NER              | 00  | X                   | 0   | XX                       | 0   | XX                   |

: Source: Humdata/OSM

| Key var. | Grappe    |     | Menage   |
|----------|-----------|-----|----------|
|          | *[1;710]* |     | *[1;16]* |
| `hhid`   | [X;XXX]   |     | XX       |

: World Bank LSMS Niger 2018-2019

|              |            |
|--------------|------------|
| **Key var.** |            |
|              | *[1;4318]* |
| `id_grid`    | XXXX       |

: DDGRID

# Data sources

See `niger_road/README_niger_connectivity_food_security.xlsx`

# Workplan

See `niger_road/readme_niger_connectivity_food_security_project.xlsx`

## Survey sample

source: [EHCVM Basic Information Document 2018/19](https://microdata.worldbank.org/index.php/catalog/4296/download/52570); [NER-INS-DER-EHCVM-2018-2019-V01](https://www.stat-niger.org/anado/index.php/catalog/162/study-description#metadata-sampling)

### **Stratified Two-stage Cluster Random Sampling**

#### **Strata**

Enumerations areas (EAs)/Zones de denombrement (ZD) have been determined crossing agroecological zones (`zae`) and ADM1 (`region`).

#### **Two-stage Cluster Random Sampling**

1.  **PSU**: random selection of 504 EAs, weighted by the 2001 census data population distribution.
2.  **SSU**: creation of 504 clusters (`grappe`) with the random selection of 12 households in each EAs, with equal weight.

Total of 6024 households - 1577 from urban areas and 4447 from rural areas.

Household weights are calculated at the cluster-level (grappes).

    svydesign(id = ~ grappe+hhid, strata = ~ region+zae, weights = ~ hhweight, data = survey_welfare, nest = TRUE)

    Stratified 2 - level Cluster Sampling design (with replacement)
    With (504, 6024) clusters.
    svydesign(id = ~grappe + hhid, strata = ~region + zae, weights = ~hhweight, 
        data = survey_welfare, nest = TRUE)

surve
