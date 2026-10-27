# _helpers/dyadify_ucdp.R
#
# Convert the UCDP/PRIO Dyadic Armed Conflict dataset (v26.1) into a
# genuinely actor-dyadic, directed dataset.
#
# WHY THIS IS NEEDED
# -------------------
# In the raw "dyadic" file, a single row can bundle multiple actors on
# either side of a conflict-year:
#   * The primary actor fields (side_a / side_b and their gwno_a / gwno_b
#     codes) occasionally list more than one actor, comma-separated, when
#     the number of comma-separated GW codes matches the number of
#     comma-separated names (e.g. "Government of Egypt, Government of
#     Iraq, ..." paired with "651, 645, ..."). When the GW code field has
#     only one value (or is NA) the name is left intact even if it
#     contains an internal comma (e.g. "Military faction (forces of
#     Honasan, Abenina & Zumel)"), since that comma is part of the name,
#     not an actor separator.
#   * Secondary/joining actors are listed separately in side_a_2nd /
#     gwno_a_2nd and side_b_2nd / gwno_b_2nd, again as comma-separated
#     lists that line up position-for-position with their GW codes.
#
# This function expands every row into one record per (actor on side A,
# actor on side B) combination -- true dyads of individual actors -- and
# then mirrors each pair so the result is directed: for every (side1,
# side2) row there is a matching (side2, side1) row.
#
# CAVEATS / ASSUMPTIONS
# ----------------------
# * intensity_level and type_of_conflict are attributes of the original
#   dyad row (they can legitimately differ across dyads within the same
#   conflict-year), so they are carried through unchanged for every actor
#   pair derived from that row.
# * The dyadic file has no explicit conflict episode start/end dates (that
#   information lives in the separate non-dyadic UCDP/PRIO Armed Conflict
#   dataset). `onset_year` and `ongoing` are therefore derived as
#   approximations from the years actually observed for each conflict_id:
#     - onset_year = the first year conflict_id appears in the data.
#     - ongoing    = TRUE if the last year conflict_id appears in the data
#                    equals the final year covered by the dataset overall
#                    (i.e. the conflict had not dropped out of UCDP's
#                    coding by the most recent year of coverage), FALSE
#                    otherwise. This is a presence-based proxy, not a
#                    coded termination date -- treat it accordingly.

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)

#' Split a (possibly) multi-actor name/GW-code field pair into individual actors
#'
#' @param name_str A single string, possibly with comma-separated actor names.
#' @param code_str A single string, possibly with comma-separated GW codes,
#'   or NA if no GW code(s) are coded for this actor/field.
#' @return A tibble with one row per actor: columns `actor` (chr) and
#'   `gwno` (dbl).
#' @keywords internal
.split_actor_field <- function(name_str, code_str) {
  if (is.na(name_str) || !nzchar(name_str)) {
    return(tibble(actor = character(0), gwno = double(0)))
  }

  names_split <- str_trim(str_split(name_str, ",")[[1]])

  if (is.na(code_str) || !nzchar(code_str)) {
    # No GW code(s) coded at all: treat the whole field as ONE actor, even
    # if its name happens to contain a comma (e.g. non-state actor names).
    return(tibble(actor = name_str, gwno = NA_real_))
  }

  codes_split <- as.numeric(str_trim(str_split(code_str, ",")[[1]]))

  if (length(names_split) == length(codes_split)) {
    tibble(actor = names_split, gwno = codes_split)
  } else {
    # Counts don't line up (e.g. an internal comma in a single actor's
    # name coincided with a single GW code) -- don't guess, keep the name
    # field intact and pair it with the first GW code.
    tibble(actor = name_str, gwno = codes_split[1])
  }
}

#' Build the list of actors (primary + secondary) for one side of one row
#'
#' @param primary_name,primary_code Primary actor name/GW-code strings.
#' @param secondary_name,secondary_code Secondary (joining) actor
#'   name/GW-code strings, or NA if none.
#' @return A tibble with columns `actor`, `gwno`.
#' @keywords internal
.side_actors <- function(primary_name, primary_code, secondary_name, secondary_code) {
  bind_rows(
    .split_actor_field(primary_name, primary_code),
    .split_actor_field(secondary_name, secondary_code)
  ) |>
    distinct()
}

#' Expand one raw dyad-year row into its (side A actor x side B actor) pairs
#' @keywords internal
.expand_one_row <- function(dyad_id, conflict_id, year, side_a, gwno_a,
                             side_a_2nd, gwno_a_2nd, side_b, gwno_b,
                             side_b_2nd, gwno_b_2nd, intensity_level,
                             type_of_conflict) {
  actors_a <- .side_actors(side_a, gwno_a, side_a_2nd, gwno_a_2nd)
  actors_b <- .side_actors(side_b, gwno_b, side_b_2nd, gwno_b_2nd)

  # Cross every actor on side A with every actor on side B, joining by
  # position within each side (not by value, since names/codes can repeat).
  grid <- expand.grid(idx_a = seq_len(nrow(actors_a)), idx_b = seq_len(nrow(actors_b)))

  tibble(
    dyad_id = dyad_id,
    conflict_id = conflict_id,
    year = year,
    side1_name = actors_a$actor[grid$idx_a],
    side2_name = actors_b$actor[grid$idx_b],
    gwcode1 = actors_a$gwno[grid$idx_a],
    gwcode2 = actors_b$gwno[grid$idx_b],
    intensity_level = intensity_level,
    type_of_conflict = type_of_conflict
  )
}

#' Convert the raw UCDP/PRIO Dyadic dataset into a directed, actor-level dyadic dataset
#'
#' @param path Path to the raw `Dyadic_v*.csv` file (or a data frame already
#'   read in with the standard UCDP Dyadic column names).
#' @param label_codes If `TRUE` (default), add human-readable factor
#'   versions of `intensity_level` and `type_of_conflict` alongside the
#'   original numeric codes.
#'
#' @return A tibble, directed and dyadic at the (conflict_id, dyad_id, year,
#'   side1 actor, side2 actor) level, with columns:
#'   \describe{
#'     \item{conflict_id}{UCDP conflict identifier.}
#'     \item{dyad_id}{Original UCDP dyad identifier the row was derived from.}
#'     \item{year}{Calendar year.}
#'     \item{side1_name, side2_name}{Actor names; every pair also appears
#'       with side1/side2 swapped (directed).}
#'     \item{gwno_side1, gwno_side2}{Gleditsch-Ward numeric codes for each
#'       actor (NA for actors without one, e.g. most non-state rebel
#'       groups).}
#'     \item{onset_year}{First year conflict_id appears in the data.}
#'     \item{ongoing}{Logical; see "CAVEATS" note in the file header.}
#'     \item{intensity_level}{Original numeric code (1 = minor, 2 = war).}
#'     \item{type_of_conflict}{Original numeric code (1 = extrasystemic,
#'       2 = interstate, 3 = internal, 4 = internationalized internal).}
#'     \item{intensity_level_lbl, type_of_conflict_lbl}{Labelled factor
#'       versions, included when `label_codes = TRUE`.}
#'   }
#' @export
dyadify_ucdp <- function(path, label_codes = TRUE) {

  raw <- if (is.data.frame(path)) {
    path
  } else {
    read_csv(path, show_col_types = FALSE, na = c("", "NA"))
  }

  required_cols <- c(
    "dyad_id", "conflict_id", "side_a", "side_a_2nd", "side_b", "side_b_2nd",
    "gwno_a", "gwno_a_2nd", "gwno_b", "gwno_b_2nd", "year",
    "intensity_level", "type_of_conflict"
  )
  missing_cols <- setdiff(required_cols, names(raw))
  if (length(missing_cols) > 0) {
    stop("Missing expected column(s): ", paste(missing_cols, collapse = ", "))
  }

  raw <- raw |>
    mutate(across(c(side_a, side_a_2nd, side_b, side_b_2nd,
                     gwno_a, gwno_a_2nd, gwno_b, gwno_b_2nd), as.character))

  # Expand each row into one record per (side A actor, side B actor) pair.
  expanded <- pmap(
    list(
      raw$dyad_id, raw$conflict_id, raw$year, raw$side_a, raw$gwno_a,
      raw$side_a_2nd, raw$gwno_a_2nd, raw$side_b, raw$gwno_b,
      raw$side_b_2nd, raw$gwno_b_2nd, raw$intensity_level, raw$type_of_conflict
    ),
    .expand_one_row
  ) |>
    list_rbind()

  # Mirror every pair so the dataset is directed: (A, B) and (B, A).
  flipped <- expanded |>
    rename(
      side1_name = side2_name, side2_name = side1_name,
      gwcode1 = gwcode2, gwcode2 = gwcode1
    )

  directed <- bind_rows(expanded, flipped) |>
    distinct()

  # Conflict-level onset year / ongoing flag (see header note on caveats).
  max_year_overall <- max(raw$year, na.rm = TRUE)
  conflict_span <- raw |>
    group_by(conflict_id) |>
    summarise(
      onset_year = min(year, na.rm = TRUE),
      last_year = max(year, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(ongoing = last_year == max_year_overall) |>
    select(conflict_id, onset_year, ongoing)

  out <- directed |>
    left_join(conflict_span, by = "conflict_id") |>
    relocate(conflict_id, dyad_id, year, side1_name, side2_name,
             gwcode1, gwcode2, onset_year, ongoing,
             intensity_level, type_of_conflict)
  
  out <- out |>
    mutate(
      acd_onset = (year == onset_year) + 0,
      acd_ongoing = 1
    )

  if (label_codes) {
    out <- out |>
      mutate(
        intensity_level_lbl = factor(
          intensity_level,
          levels = c(1, 2),
          labels = c("Minor (25-999 battle deaths)", "War (>=1,000 battle deaths)")
        ),
        type_of_conflict_lbl = factor(
          type_of_conflict,
          levels = c(1, 2, 3, 4),
          labels = c("Extrasystemic", "Interstate", "Internal",
                     "Internationalized internal")
        )
      )
  }

  out |>
    arrange(conflict_id, dyad_id, year, side1_name, side2_name)
}
