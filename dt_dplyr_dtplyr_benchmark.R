library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)

library(tidyverse)

dtplyr_mtcars <- lazy_dt(mtcars)
dt_mtcars <- data.table(mtcars)
dplyr_mtcars <- as_tibble(mtcars)

mtcars %>% 
  filter(wt < 5) %>%
  mutate(l100k = 235.21 / mpg) %>% # liters / 100 km
  group_by(cyl) %>%
  summarise(l100k = mean(l100k)) %>% 
  system.time()

system.time()

# load data
df = flight
dim(df)

flights

# benchmark set ====
require(data.table)
N <- 1e5
K <- 100
set.seed(1)

# _data.table ====


DT <- data.table(
  id1 = sample(sprintf("id%03d", 1:K), N, TRUE), # large groups (char)
  id2 = sample(sprintf("id%03d", 1:K), N, TRUE), # large groups (char)
  id3 = sample(sprintf("id%010d", 1:(N / K)), N, TRUE), # small groups (char)
  id4 = sample(K, N, TRUE), # large groups (int)
  id5 = sample(K, N, TRUE), # large groups (int)
  id6 = sample(N / K, N, TRUE), # small groups (int)
  v1 = sample(5, N, TRUE), # int in range [1,5]
  v2 = sample(5, N, TRUE), # int in range [1,5]
  v3 = sample(round(runif(100, max = 100), 4), N, TRUE) # numeric e.g. 23.5749
)

q1a <- system.time(DT[, sum(v1), keyby = id1])[3]
q1b <- system.time(DT[, sum(v1), keyby = id1])[3]
q2a <- system.time(DT[, sum(v1), keyby = "id1,id2"])[3]
q2b <- system.time(DT[, sum(v1), keyby = "id1,id2"])[3]
q3a <- system.time(DT[, list(sum(v1), mean(v3)), keyby = id3])[3]
q3b <- system.time(DT[, list(sum(v1), mean(v3)), keyby = id3])[3]
q4a <- system.time(DT[, lapply(.SD, mean), keyby = id4, .SDcols = 7:9])[3]
q4b <- system.time(DT[, lapply(.SD, mean), keyby = id4, .SDcols = 7:9])[3]
q5a <- system.time(DT[, lapply(.SD, sum), keyby = id6, .SDcols = 7:9])[3]
q5b <- system.time(DT[, lapply(.SD, sum), keyby = id6, .SDcols = 7:9])[3]
data_table_results <- list(
  q1a = q1a, q1b = q1b,
  q2a = q2a, q2b = q2b,
  q3a = q3a, q3b = q3b,
  q4a = q4a, q4b = q4b,
  q5a = q5a, q5b = q5b
)

data_table_results

# _dtplyr ----
DF <- data.frame(
  stringsAsFactors = FALSE,
  id1 = sample(sprintf("id%03d", 1:K), N, TRUE),
  id2 = sample(sprintf("id%03d", 1:K), N, TRUE),
  id3 = sample(sprintf("id%010d", 1:(N / K)), N, TRUE),
  id4 = sample(K, N, TRUE),
  id5 = sample(K, N, TRUE),
  id6 = sample(N / K, N, TRUE),
  v1 = sample(5, N, TRUE),
  v2 = sample(5, N, TRUE),
  v3 = sample(round(runif(100, max = 100), 4), N, TRUE)
)
q1a <- system.time(DF %>% lazy_dt() %>% group_by(id1) %>%
                     summarise(sum(v1)) %>% as_tibble())[3]
q1b <- system.time(DF %>% lazy_dt() %>% group_by(id1) %>%
                     summarise(sum(v1)) %>% as_tibble())[3]
q2a <- system.time(DF %>% lazy_dt() %>% group_by(id1, id2) %>%
                     summarise(sum(v1)) %>% as_tibble())[3]
q2b <- system.time(DF %>% lazy_dt() %>% group_by(id1, id2) %>%
                     summarise(sum(v1)) %>% as_tibble())[3]
q3a <- system.time(DF %>% lazy_dt() %>% group_by(id3) %>% summarise(sum(v1), mean(v3)) %>% as_tibble())[3]
q3b <- system.time(DF %>% lazy_dt() %>% group_by(id3) %>%
                     summarise(sum(v1), mean(v3)) %>% as_tibble())[3]
q4a <- system.time(DF %>% lazy_dt() %>% group_by(id4) %>%
                     summarise_at(vars(v1:v3), mean) %>% as_tibble())[3]
q4b <- system.time(DF %>% lazy_dt() %>% group_by(id4) %>%
                     summarise_at(vars(v1:v3), mean) %>% as_tibble())[3]
q5a <- system.time(DF %>% lazy_dt() %>% group_by(id6) %>%
                     summarise_at(vars(v1:v3), sum) %>% as_tibble())[3]
q5b <- system.time(DF %>% lazy_dt() %>% group_by(id6) %>%
                     summarise_at(vars(v1:v3), sum) %>% as_tibble())[3]
dtplyr_results <- list(
  q1a = q1a, q1b = q1b,
  q2a = q2a, q2b = q2b,
  q3a = q3a, q3b = q3b,
  q4a = q4a, q4b = q4b,
  q5a = q5a, q5b = q5b
)

# _dplyr ----
DF <- data.frame(
  stringsAsFactors = FALSE,
  id1 = sample(sprintf("id%03d", 1:K), N, TRUE),
  id2 = sample(sprintf("id%03d", 1:K), N, TRUE),
  id3 = sample(sprintf("id%010d", 1:(N / K)), N, TRUE),
  id4 = sample(K, N, TRUE),
  id5 = sample(K, N, TRUE),
  id6 = sample(N / K, N, TRUE),
  v1 = sample(5, N, TRUE),
  v2 = sample(5, N, TRUE),
  v3 = sample(round(runif(100, max = 100), 4), N, TRUE)
)
q1a <- system.time(DF %>% group_by(id1) %>% summarise(sum(v1)) %>% as_tibble())[3]
q1b <- system.time(DF %>% group_by(id1) %>% summarise(sum(v1)) %>% as_tibble())[3]
q2a <- system.time(DF %>% group_by(id1, id2) %>% summarise(sum(v1)) %>% as_tibble())[3]
q2b <- system.time(DF %>% group_by(id1, id2) %>% summarise(sum(v1)) %>% as_tibble())[3]
q3a <- system.time(DF %>% group_by(id3) %>% summarise(sum(v1), mean(v3)) %>% as_tibble())[3]
q3b <- system.time(DF %>% group_by(id3) %>%
                     summarise(sum(v1), mean(v3)) %>% as_tibble())[3]
q4a <- system.time(DF %>% group_by(id4) %>%
                     summarise_at(vars(v1:v3), mean) %>% as_tibble())[3]
q4b <- system.time(DF %>% group_by(id4) %>%
                     summarise_at(vars(v1:v3), mean) %>% as_tibble())[3]
q5a <- system.time(DF %>% group_by(id6) %>%
                     summarise_at(vars(v1:v3), sum) %>% as_tibble())[3]
q5b <- system.time(DF %>% group_by(id6) %>%
                     summarise_at(vars(v1:v3), sum) %>% as_tibble())[3]
dplyr_results <- list(
  q1a = q1a, q1b = q1b,
  q2a = q2a, q2b = q2b,
  q3a = q3a, q3b = q3b,
  q4a = q4a, q4b = q4b,
  q5a = q5a, q5b = q5b
)

data_table_results %>% as_tibble()
dplyr_results %>% as_tibble()

map_dr()


benchmark_result <- bind_rows(
  data_table_results, 
  dplyr_results,
  dtplyr_results) 

benchmark_result <- bind_cols("type" = c("data_table_results",
            "dplyr_results",
            "dtplyr_results"),
          benchmark_result)

benchmark_result %>% 
  gather(`q1a`:`q5b`,key = "asdf", value = "df") %>% 
  ggplot(aes(x = type, y = df, fill = type)) + 
  geom_col() +
  coord_flip() +
  facet_wrap(~asdf, ncol = 1)

benchmark_result %>% 
  ggplot(aes(x = type, y = q1a)) + 
  geom_col()
