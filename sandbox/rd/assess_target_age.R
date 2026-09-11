library(targets)
library(tarchetypes)

tracked_age_targets <- list()

tar_age <- function(name, age, ...) {
  tracked_age_targets[[length(tracked_age_targets) + 1]] <<- list(
    name = as.character(substitute(name)),
    age_expr = deparse(substitute(age)),
    age_value = tryCatch(age, error = function(e) NA) # forces/evaluates age
  )
  mc <- match.call()
  mc[[1]] <- quote(tarchetypes::tar_age)
  eval.parent(mc)
}

tar_cue_age <- function(name, age, ...) {
  tracked_age_targets[[length(tracked_age_targets) + 1]] <<- list(
    name = as.character(substitute(name)),
    age_expr = deparse(substitute(age)),
    age_value = tryCatch(age, error = function(e) NA)
  )
  mc <- match.call()
  mc[[1]] <- quote(tarchetypes::tar_cue_age)
  eval.parent(mc)
}

tar_cue_age_raw <- function(name, age, ...) {
  tracked_age_targets[[length(tracked_age_targets) + 1]] <<- list(
    name = name, # already character
    age_expr = deparse(substitute(age)),
    age_value = tryCatch(age, error = function(e) NA)
  )
  mc <- match.call()
  mc[[1]] <- quote(tarchetypes::tar_cue_age_raw)
  eval.parent(mc)
}

source("inst/_targets.R", local = new.env())

rm(tar_age, tar_cue_age, tar_cue_age_raw)

age_df <- do.call(rbind.data.frame, tracked_age_targets)
age_df


meta_df <- tar_meta(
  names = any_of(age_df$name),
  fields = c("name", "time")
)

combined <- merge(age_df, meta_df, by = "name", all.x = TRUE)
combined
combined$days_since_update <- as.numeric(difftime(
  Sys.time(),
  combined$time,
  units = "days"
))
combined$days_until_stale <- combined$age_value - combined$days_since_update
combined[
  order(combined$days_until_stale),
  c("name", "time", "age_value", "days_until_stale")
]
