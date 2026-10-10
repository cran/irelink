## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = '#>'
)

## ----setup--------------------------------------------------------------------
library(irelink)
library(ggplot2)

## ----load-data----------------------------------------------------------------
df <- fake_1000
head(df)

## ----completeness, message = FALSE--------------------------------------------
con <- DBI::dbConnect(duckdb::duckdb())
comp <- il_completeness(df, con = con)
comp

## ----completeness-plot, fig.width = 6, fig.height = 3-------------------------
autoplot(comp)

## ----profile------------------------------------------------------------------
il_profile(df[, c('first_name', 'surname', 'city')], con = con, top_n = 5)

## ----suggest-blocking---------------------------------------------------------
il_suggest_blocking(df, con = con)

## ----spec---------------------------------------------------------------------
spec <- il_spec() |>
  il_compare(first_name, cl_name()) |>
  il_compare(surname, cl_name()) |>
  il_compare(dob, cl_dob()) |>
  il_compare(city, cl_exact(term_frequency = TRUE)) |>
  il_compare(email, cl_email()) |>
  il_block_on(first_name) |>
  il_block_on(surname) |>
  il_block_on(city)

spec

## ----count-pairs--------------------------------------------------------------
il_count_pairs(
  df,
  block_on(first_name),
  block_on(surname),
  block_on(city),
  con = con
)

## ----model--------------------------------------------------------------------
model <- il_model(df, spec = spec, con = con)

## ----prior--------------------------------------------------------------------
model <- il_estimate_prior(
  model,
  block_on(first_name, surname),
  block_on(email),
  recall = 0.6
)

## ----train--------------------------------------------------------------------
model <- il_estimate_u(model, max_pairs = 1e5)
model <- il_estimate_em(model, block_on(first_name))
model <- il_estimate_em(model, block_on(dob))

## ----summary------------------------------------------------------------------
summary(model)

## ----weights-plot, fig.width = 6, fig.height = 3.5----------------------------
autoplot(model)

## ----params-plot, fig.width = 7, fig.height = 4-------------------------------
autoplot(model, type = 'parameters')

## ----save---------------------------------------------------------------------
path <- tempfile(fileext = '.rds')
il_save(model, path)

## ----attach, message = FALSE--------------------------------------------------
con2 <- DBI::dbConnect(duckdb::duckdb())
loaded <- il_load(path)
model2 <- il_attach(loaded, fake_1000, con = con2)
head(predict(model2, threshold = 0.85))
DBI::dbDisconnect(con2, shutdown = TRUE)

## ----predict------------------------------------------------------------------
predictions <- predict(model, threshold = 0.5)
nrow(predictions)

## ----histogram, fig.width = 6, fig.height = 3---------------------------------
autoplot(predictions)

## ----waterfall, fig.width = 6, fig.height = 3---------------------------------
autoplot(predictions, which = 1)

## ----cluster------------------------------------------------------------------
clusters <- il_cluster(predictions, threshold = 0.85)
head(clusters)

## ----labels-------------------------------------------------------------------
# Use the bundled clerical labels from splink
labels_raw <- fake_1000_labels

# Rename to match irelink's evaluation convention
labels <- data.frame(
  unique_id_l = labels_raw$unique_id_l,
  unique_id_r = labels_raw$unique_id_r,
  is_match = as.integer(labels_raw$clerical_match_score)
)

nrow(labels)
sum(labels$is_match)

## ----accuracy-----------------------------------------------------------------
acc <- il_accuracy(model, labels = labels)
acc

## ----accuracy-plot, fig.width = 6, fig.height = 3.5---------------------------
autoplot(acc)

## ----roc, fig.width = 5, fig.height = 4---------------------------------------
roc <- il_roc(model, labels = labels)
autoplot(roc)

## ----pr, fig.width = 5, fig.height = 4----------------------------------------
pr <- il_precision_recall(model, labels = labels)
autoplot(pr)

## ----errors-------------------------------------------------------------------
errors <- il_errors(model, labels = labels, threshold = 0.85)
head(errors)

## ----unlinkables, fig.width = 6, fig.height = 3-------------------------------
unlink <- il_unlinkables(model)
autoplot(unlink)

## ----cleanup------------------------------------------------------------------
il_cleanup(model)
DBI::dbDisconnect(con, shutdown = TRUE)

