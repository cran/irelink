## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = '#>'
)

## ----spec---------------------------------------------------------------------
library(irelink)

spec <- il_spec() |>
  il_compare(first_name, cl_jaro_winkler(0.9, 0.7)) |>
  il_compare(surname, cl_jaro_winkler(0.9, 0.7)) |>
  il_compare(dob, cl_exact()) |>
  il_block_on(surname) |>
  il_block_on(first_name)

spec

## ----model, message = FALSE---------------------------------------------------
df <- fake_20
con <- DBI::dbConnect(duckdb::duckdb())

model <- il_model(df, spec = spec, con = con)
model

## ----train-u------------------------------------------------------------------
model <- il_estimate_u(model)

## ----train-em-----------------------------------------------------------------
model <- il_estimate_em(model, block_on(surname))

## ----weights------------------------------------------------------------------
il_weights(model)

## ----predict------------------------------------------------------------------
pairs <- predict(model, threshold = 0.5)
head(pairs)

## ----cluster------------------------------------------------------------------
clusters <- il_cluster(pairs)
head(clusters)

## ----cleanup------------------------------------------------------------------
il_cleanup(model)
DBI::dbDisconnect(con, shutdown = TRUE)

