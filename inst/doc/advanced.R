## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = '#>'
)

## ----setup--------------------------------------------------------------------
library(irelink)
library(ggplot2)

df <- fake_1000

## ----setup-con, message = FALSE-----------------------------------------------
con <- DBI::dbConnect(duckdb::duckdb())

## ----setup-model--------------------------------------------------------------
spec <- il_spec() |>
  il_compare(first_name, cl_name()) |>
  il_compare(surname, cl_name()) |>
  il_compare(dob, cl_dob()) |>
  il_compare(city, cl_exact(term_frequency = TRUE)) |>
  il_compare(email, cl_email()) |>
  il_block_on(first_name) |>
  il_block_on(surname) |>
  il_block_on(city)

model <- il_model(df, spec = spec, con = con)
model <- il_estimate_prior(
  model,
  block_on(first_name, surname),
  block_on(email),
  recall = 0.6
)
model <- il_estimate_u(model, max_pairs = 1e5)
model <- il_estimate_em(model, block_on(first_name))
model <- il_estimate_em(model, block_on(dob))

pairs <- predict(model, threshold = 0.5)
clusters <- il_cluster(pairs, threshold = 0.85)

## ----history, fig.width = 7, fig.height = 5-----------------------------------
hist <- il_training_history(model)
autoplot(hist)

## ----compare-records----------------------------------------------------------
rec_a <- fake_1000[1, ]
rec_b <- fake_1000[5, ]

il_compare_records(rec_a, rec_b, spec = model$spec, con = con)

## ----waterfall, fig.width = 6, fig.height = 3---------------------------------
autoplot(pairs, which = 1)

## ----lazy---------------------------------------------------------------------
pairs_lazy <- predict(model, threshold = 0.5, collect = FALSE)
pairs_lazy

## ----lazy-cluster-------------------------------------------------------------
clusters_lazy <- il_cluster(pairs_lazy, threshold = 0.85)
nrow(clusters_lazy)

## ----chunked-u, eval = FALSE--------------------------------------------------
# model <- il_estimate_u(
#   model,
#   max_pairs = 5e6,
#   chunk_size = 250000,
#   min_count_per_level = 100
# )
# model$params$u_estimation

## ----sql-profile, eval = FALSE------------------------------------------------
# pairs <- predict(model, threshold = 0.5, profile_sql = TRUE)
# attr(pairs, 'sql_profile')

## ----graph-metrics------------------------------------------------------------
metrics <- il_graph_metrics(pairs, clusters)

## ----graph-clusters-----------------------------------------------------------
metrics$clusters

## ----graph-nodes--------------------------------------------------------------
head(metrics$nodes)

## ----phonetic-spec------------------------------------------------------------
spec_phon <- il_spec() |>
  il_compare(first_name, cl_name()) |>
  il_compare(surname, cl_name()) |>
  il_compare(dob, cl_dob()) |>
  il_block_on(first_name, .transform = il_soundex) |>
  il_block_on(surname, .transform = il_soundex)

## ----phonetic-train-----------------------------------------------------------
model_phon <- il_model(df, spec = spec_phon, con = con)
model_phon <- il_estimate_u(model_phon, max_pairs = 1e5)
model_phon <- il_estimate_em(
  model_phon,
  block_on(first_name, .transform = il_soundex)
)

## ----transform-spec-----------------------------------------------------------
spec_tr <- il_spec() |>
  il_compare(first_name, cl_jaro_winkler(0.9, 0.7), transform = tolower) |>
  il_compare(surname, cl_jaro_winkler(0.9, 0.7), transform = tolower) |>
  il_compare(dob, cl_exact()) |>
  il_block_on(first_name) |>
  il_block_on(surname)

model_tr <- il_model(df, spec = spec_tr, con = con)
model_tr <- il_estimate_u(model_tr, max_pairs = 1e5)
model_tr <- il_estimate_em(model_tr, block_on(surname))

## ----find-matches-------------------------------------------------------------
new_df <- data.frame(
  first_name = c('Jhon', 'Alice'),
  surname    = c('Smith', 'Jones'),
  dob        = c('1990-01-15', '1985-06-20'),
  city       = c('London', 'Manchester'),
  email      = c(NA, 'ajones@example.com')
)

matches <- il_find_matches(model, new_df, threshold = 0.5)
matches

## ----cleanup------------------------------------------------------------------
il_cleanup(model)
il_cleanup(model_phon)
il_cleanup(model_tr)
DBI::dbDisconnect(con, shutdown = TRUE)

