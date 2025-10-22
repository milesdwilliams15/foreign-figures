# -------------------------------------------------------------------------
# Functions with extra goodies for {peacesciencer}
# -------------------------------------------------------------------------


## add_ucdp_acd_new ----

# I've some issues with the built-in version of this
# function in {peacesciencer}. It only uses side a, and
# it doesn't accomodate dyadic analysis. And sometimes
# I'd like to have counts of conflicts; not just a dummy
# for whether one occured. This function fixes all that.

add_ucdp_acd_new <- function(
    data, type, issue, only_wars = FALSE
) {
  # check for GW system codes:
  yes <- any(colnames(data) %in% c("gwcode", "gwcode1"))
  if(!yes) {
    stop("You can only use this function with GW system codes.")
  }
  
  # place holder:
  dt <- peacesciencer::ucdp_acd
  
  # if you want a specific conflict type:
  if(!missing(type)) {
    dt <- dt |>
      filter(type_of_conflict %in% type)
  }
  
  # if you want a specifict issue:
  if(!missing(issue)) {
    dt <- dt |>
      filter(incompatibility %in% issue)
  }
  
  # if you only want wars:
  if(only_wars) {
    dt <- dt |>
      filter(intensity_level == 2)
  }
  
  # for state-year data:
  if(any(colnames(data) %in% c("gwcode"))) {
    bind_rows(
      dt |>
        transmute(
          gwcode = gwno_a,
          year,
          conflict_id
        ) |>
        arrange(year) |>
        group_by(gwcode, conflict_id) |>
        mutate(
          primary_party = 1,
          onset = (row_number() == 1) + 0
        ) |>
        ungroup(),
      dt |>
        transmute(
          gwcode = gwno_a_2nd,
          year,
          conflict_id
        ) |>
        arrange(year) |>
        group_by(gwcode, conflict_id) |>
        mutate(
          primary_party = 0,
          onset = (row_number() == 1) + 0
        ),
      dt |>
        transmute(
          gwcode = gwno_b,
          year,
          conflict_id
        ) |>
        arrange(year) |>
        group_by(gwcode, conflict_id) |>
        mutate(
          primary_party = 1,
          onset = (row_number() == 1) + 0
        ),
      dt |>
        transmute(
          gwcode = gwno_b_2nd,
          year,
          conflict_id
        ) |>
        arrange(year) |>
        group_by(gwcode, conflict_id) |>
        mutate(
          primary_party = 0,
          onset = (row_number() == 1) + 0
        )
    ) |>
      drop_na(gwcode) |>
      group_by(gwcode, year) |>
      summarize(
        primary_party = max(primary_party),
        ucdp_onset = max(onset),
        ucdp_ongoing = 1,
        ucdp_ongoing_total = length(unique(conflict_id)),
        .groups = "drop"
      ) -> dt
    
    data |>
      left_join(dt, by = c("gwcode", "year")) -> dt
    
    dt |>
      mutate(
        across(c(primary_party:ucdp_ongoing_total), ~ replace_na(.x, 0))
      ) -> dt
    
  } else {
    
    bind_rows(
      dt |>
        transmute(
          gwcode1 = gwno_a,
          gwcode2 = gwno_b,
          year,
          conflict_id
        ) |>
        group_by(gwcode1, gwcode2, year, conflict_id) |>
        summarize(
          primary_party1 = 1,
          primary_party2 = 1,
          .groups = "drop"
        ) |>
        arrange(year) |>
        group_by(gwcode1, gwcode2, conflict_id) |>
        mutate(
          ucdp_onset = (row_number() == 1) +0
        ),
      dt |> 
        transmute(
          gwcode1 = gwno_a_2nd,
          gwcode2 = gwno_b_2nd,
          year,
          conflict_id
        ) |>
        group_by(gwcode1, gwcode2, year, conflict_id) |>
        summarize(
          primary_party1 = 0,
          primary_party2 = 0,
          .groups = "drop"
        ) |> 
        arrange(year) |>
        group_by(gwcode1, gwcode2, conflict_id) |>
        mutate(
          ucdp_onset = (row_number() == 1) +0
        ),
      dt |> 
        transmute(
          gwcode1 = gwno_a,
          gwcode2 = gwno_b_2nd,
          year,
          conflict_id
        ) |>
        group_by(gwcode1, gwcode2, year, conflict_id) |>
        summarize(
          primary_party1 = 1,
          primary_party2 = 0,
          .groups = "drop"
        ) |>
        arrange(year) |>
        group_by(gwcode1, gwcode2, conflict_id) |>
        mutate(
          ucdp_onset = (row_number() == 1) +0
        ),
      dt |> 
        transmute(
          gwcode1 = gwno_a_2nd,
          gwcode2 = gwno_b,
          year,
          conflict_id
        ) |>
        group_by(gwcode1, gwcode2, year, conflict_id) |>
        summarize(
          primary_party1 = 0,
          primary_party2 = 1,
          .groups = "drop"
        ) |>
        arrange(year) |>
        group_by(gwcode1, gwcode2, conflict_id) |>
        mutate(
          ucdp_onset = (row_number() == 1) +0
        )
    ) |> 
      drop_na(gwcode1, gwcode2) |>
      group_by(gwcode1, gwcode2, year) |>
      summarize(
        primary_party1 = max(primary_party1),
        primary_party2 = max(primary_party2),
        ucdp_onset = max(ucdp_onset),
        ucdp_ongoing = 1,
        ucdp_ongoing_total = length(unique(conflict_id)),
        .groups = "drop"
      ) -> dt
    
    # dyads need to be mirrored:
    bind_rows(
      dt,
      dt |> rename(gwcode1 = gwcode2, gwcode2 = gwcode1)
    ) -> dt
    
    data |>
      left_join(
        dt, by = c("gwcode1", "gwcode2", "year")
      ) |>
      mutate(
        across(
          primary_party1:ucdp_ongoing_total,
          ~ replace_na(.x, 0)
        )
      ) -> dt
    
    # now fix the fact that the original data doesn't have all
    # possible dyads
    dt |>
      group_by(gwcode1, gwcode2, year) |>
      mutate(
        across(
          primary_party1:ucdp_ongoing_total
        )
      )
  }
  
  # return the data
  dt
}

# test it out 
# library(peacesciencer)
# library(tidyverse)

# create_stateyears(system = "gw", subset_years = 1946:2024) |>
#  add_ucdp_acd_new() -> sy

# create_dyadyears(system = "gw", subset_years = 1946:2024) |>
#   add_ucdp_acd_new() -> dy

## add_gml_mic ----

# A custom function to populate a state-year or dyad-year dataset with
# the new and improved militarized interstate confrontations dataset.

add_gml_mic <- function(
    data, level
) {
  # check for CoW system codes:
  yes <- any(colnames(data) %in% c("ccode", "ccode1"))
  if(!yes) {
    stop("You can only use this function with CoW system codes.")
  }
  
  # whittle MIE data to MIC level:
  dt <- suppressMessages(
    read_csv(
      "https://raw.githubusercontent.com/milesdwilliams15/foreign-figures/refs/heads/main/_data/mie-1.0.csv"
    )
  )
  
  dt |>
    group_by(
      ccode1, ccode2, styear, micnum
    ) |>
    summarize(
      init = max((eventnum == 1), na.rm = T),
      events_total = n(),
      events1 = sum(sidea1),
      events2 = sum(1 - sidea1),
      across(fatalmin1:fatalmax2, sum),
      hostlev = max(hostlev),
      .groups = "drop"
    ) |> 
    group_by(micnum) |>
    mutate(
      gml_mic_onset = (min(styear) == styear) + 0,
      hostlev = max(hostlev)
    ) -> dt
  
  # only keep those of certain hostlev:
  if(!missing(level)) {
    dt |>
      filter(hostlev >= level) -> dt
  }
  
  # some more aggregating:
  dt |>
    group_by(ccode1, ccode2, styear) |>
    summarize(
      gml_mic_ongoing = 1,
      gml_mic_onset = max(gml_mic_onset),
      gml_mic_ongoing_init1 = max(init),
      gml_mic_onset_init1 = max(init) * gml_mic_onset,
      gml_mic_events = sum(events_total),
      gml_mic_events1 = sum(events1),
      gml_mic_events2 = sum(events2),
      across(fatalmin1:fatalmax2, sum),
      .groups = "drop"
    ) -> dt
  
  # the data isn't mirrored:
  bind_rows(
    dt,
    dt |>
      rename(
        ccode2 = ccode1,
        ccode1 = ccode2,
        gml_mic_ongoing_init2 = gml_mic_ongoing_init1,
        gml_mic_onset_init2 = gml_mic_onset_init1,
        gml_mic_events2 = gml_mic_events1,
        gml_mic_events1 = gml_mic_events2,
        fatalmin2 = fatalmin1,
        fatalmin1 = fatalmin2,
        fatalmax2 = fatalmax1,
        fatalmax1 = fatalmax2
      )
  ) |> 
    rename(year = styear) |>
    mutate(
      across(everything(), ~ replace_na(.x, 0))
    ) |> 
    select(
      ccode1, 
      ccode2, 
      year, 
      starts_with("gml_"), 
      everything()
    ) -> dt
  
  # if the data is dyadic:
  if(any(colnames(data) %in% c("ccode1"))) {
    # merge:
    data |>
      left_join(
        dt, by = c("ccode1", "ccode2", "year")
      ) -> dt
    
    # fill in missings:
    dt |>
      mutate(
        across(
          starts_with("gml_") | starts_with("fatal"),
          ~ replace_na(.x, 0)
        )
      ) -> dt
  } else {
    dt |>
      rename(ccode = ccode1) |>
      group_by(ccode, year) |>
      summarize(
        gml_mic_ongoing = 1,
        gml_mic_onset = max(gml_mic_onset),
        gml_mic_ongoing_init = max(gml_mic_ongoing_init1),
        gml_mic_onset_init = max(gml_mic_onset_init1),
        gml_mic_events = sum(gml_mic_events),
        gml_mic_events_init = sum(gml_mic_events1),
        fatalmin = sum(fatalmin1),
        fatalmax = sum(fatalmax1),
        fatalmin_total = sum(fatalmin1 + fatalmin2),
        fatalmax_total = sum(fatalmax1 + fatalmax2),
        .groups = "drop"
      ) -> dt
    
    # merge:
    data |>
      left_join(
        dt,
        by = c("ccode", "year")
      ) -> dt
    
    # fill missings:
    dt |>
      mutate(
        across(
          starts_with("gml") | starts_with("fatal"),
          ~ replace_na(.x, 0)
        )
      ) -> dt
  }
  
  # return
  dt
}


# create_dyadyears() |>
#   add_gml_mic() -> dy

# create_stateyears() |>
#   add_gml_mic() -> sy
