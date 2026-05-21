# Script loads the following:
# 1. data_catalogue - all datasets evaluated for inclusion in GEA
# 2. sources_catalogue - datasets that passed GEA inclusion criteria
# 3. sources_table - detailed info on data processing for each source *INTERNAL ONLY*


library(tidyverse)  #for data wrangling etc
library(googlesheets4)
#rm(list=ls())

# 1. data_catalogue (sheet 1)
data_catalogue <- read_sheet("https://docs.google.com/spreadsheets/d/1P-xus66mTxY-9-a8jZgzwh3KVlL2Yvh6HzJ8rbmT1bY/edit?gid=291742732#gid=291742732", 
                             sheet = "Data_Catalogue_MASTER", trim_ws = TRUE)   |> 
  select(Source_ID,
         data_id_code,
         title,
         language,
         abstract,
         provider,
         published_year,
         data_public,
         licence,
         dataset_citation,
         domain_class,
         DOI,
         URL)



#write_csv(data_catalogue, file = here::here("resources/01_data_catalogue_v0.0.XX.csv")) # Change XX to required vsn number

# 2. sources_catalogue (sheet 2, but need some fields from sheet 1)
sources_catalogue <- 
    read_sheet("https://docs.google.com/spreadsheets/d/1P-xus66mTxY-9-a8jZgzwh3KVlL2Yvh6HzJ8rbmT1bY/edit?gid=396521023#gid=396521023", 
             sheet = "Data_Review_MASTER", trim_ws = TRUE)  |> 
  # Need to convert GEA_pass from list into logical
  mutate(GEA_pass = map(GEA_pass, \(x) {
    if (is.null(x) || length(x) == 0) return(NA)
    as.logical(x[[1]])
  })) |> 
  filter(GEA_pass == TRUE)  |> 
  group_by(Source_ID, data_id_code) |>
  summarise(
    n_bands = if (all(is.na(band_layer_name))) NA_integer_ 
    else n_distinct(na.omit(band_layer_name)),
    .groups = "drop"
  ) |> 
  # Combine with fields from sheet 1
  left_join(data_catalogue |> 
              select(data_id_code,
                     title,
                     abstract,
                     provider,
                     published_year,
                     dataset_citation, 
                     URL)
  )

#write_csv(sources_catalogue, file = here::here("resources/02_sources_catalogue_v0.0.XX.csv")) # Change XX to required vsn number


# 3. sources_table (from Data_Geoprocessing_MASTER, Sheet3 )
geo_proc_tab <- 
  read_sheet("https://docs.google.com/spreadsheets/d/1P-xus66mTxY-9-a8jZgzwh3KVlL2Yvh6HzJ8rbmT1bY/edit?gid=396521023#gid=396521023", 
           sheet = "Data_Geoprocessing_MASTER", trim_ws = TRUE)  |> 
  filter(!is.na(Source_ID))

glimpse(geo_proc_tab)

# Join with fields from overlap_index

sources_table <- 
  geo_proc_tab |> 
  # need to pull in year_end from sources_full
  left_join(sources_full |> 
              select(data_id_code,
                     band_layer_name,
                     year_end)) |> 
  left_join(overlap_index |> 
              select(data_id_code,
                     band_layer_name,
                     OI)) |> 
  mutate(Charlie_staged = map(Charlie_staged, ~ifelse(is.null(.x), NA, .x)) |>   # Fix list var
           unlist())



# Also write to synthesis repo
write_csv(sources_table, file = here::here("../synthesis/SourcesTable/sources_table_v0.0.XX.csv"))


# End of script


