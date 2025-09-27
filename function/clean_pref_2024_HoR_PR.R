###############################################################################
# Clean 2024 House of Representatives election data (Proportional Representation)
# 2024年衆議院選挙（比例代表）データのクリーニング
###############################################################################

#' Clean 2024 House of Representatives PR election data
#' @param data raw HoR PR data from download_2024_HoR_PR
#' @return cleaned data frame with municipality-level results
clean_pref_2024_HoR_PR <- function(data) {
  
  # Skip header rows and get to actual data (starts at row 5)
  # Column structure: 
  # [1] Municipality name
  # [2-12] Party votes (11 parties)
  # [13] Total votes
  
  # Define column names based on the 2024 election parties
  col_names <- c("mun_name",
                "nv_ishin",      # 日本維新の会
                "nv_ldp",        # 自由民主党
                "nv_reiwa",      # れいわ新選組
                "nv_sansei",     # 参政党
                "nv_anraku",     # 安楽死制度を考える会
                "nv_sdp",        # 社会民主党
                "nv_komei",      # 公明党
                "nv_hoshu",      # 日本保守党
                "nv_jcp",        # 日本共産党
                "nv_dpfp",       # 国民民主党
                "nv_cdp",        # 立憲民主党
                "total_votes")
  
  # Extract data starting from row 5
  cleaned_data <- data %>%
    slice(5:n()) %>%
    select(1:13) %>%
    setNames(col_names) %>%
    # Remove rows with NA municipality names (empty rows)
    filter(!is.na(mun_name)) %>%
    # Convert vote columns to numeric
    mutate(across(starts_with("nv_"), as.numeric),
          across(starts_with("total_"), as.numeric))
  
  # Handle split wards (e.g., 札幌市北区第１区, 札幌市北区第２区)
  # Add a column for base municipality name and ward number
  cleaned_data <- cleaned_data %>%
    mutate(
      # Check if municipality name contains "第" (indicating split ward)
      is_split = grepl("第[０-９0-9]+区", mun_name),
      # Extract base municipality name (without 第X区)
      mun_name_base = if_else(
        is_split,
        gsub("第[０-９0-9]+区", "", mun_name),
        mun_name
      ),
      # Extract ward number if split
      ward_number = if_else(
        is_split,
        as.numeric(gsub(".*第([０-９0-9]+)区.*", "\\1", mun_name)),
        NA_real_
      )
    )
  
  return(cleaned_data)
}

#' Aggregate split wards to match census municipality units
#' @param data cleaned HoR PR data with split wards
#' @return aggregated data at municipality level
aggregate_split_wards <- function(data) {
  
  # Aggregate data by base municipality name
  aggregated <- data %>%
    group_by(mun_name_base) %>%
    summarise(
      # Sum all vote columns
      across(starts_with("nv_"), sum, na.rm = TRUE),
      across(starts_with("total_"), sum, na.rm = TRUE),
      # Keep track of whether this was aggregated
      was_aggregated = n() > 1,
      # Record original ward names if aggregated
      original_wards = if_else(
        n() > 1,
        paste(mun_name, collapse = ", "),
        mun_name[1]
      ),
      .groups = "drop"
    ) %>%
    # Rename back to standard mun_name
    rename(mun_name = mun_name_base)
  
  return(aggregated)
}

#' Match and harmonize HoR party votes with HoC baseline format
#' @param hor_2024 cleaned 2024 HoR PR data
#' @return data frame with harmonized party columns matching HoC format
harmonize_HoR_to_HoC_format <- function(hor_2024) {
  
  # The HoC baseline uses these party columns (from clean_pref_HoC_PR):
  # nv_ldp, nv_komei, nv_cdp, nv_ishin, nv_jcp, nv_dpfp, nv_reiwa, nv_sdp, nv_sansei
  # Plus: nv_others (for other parties)
  
  harmonized <- hor_2024 %>%
    mutate(
      # Calculate "others" by summing parties not in the main set
      nv_others = nv_anraku + nv_hoshu,
      # Keep the main parties as is
      # nv_ldp, nv_komei, nv_cdp, nv_ishin, nv_jcp, nv_dpfp, nv_reiwa, nv_sdp, nv_sansei
      # are already in the correct format
    ) %>%
    # Select only the columns matching HoC format
    select(mun_name, 
          nv_ldp, nv_komei, nv_cdp, nv_ishin, nv_jcp, 
          nv_dpfp, nv_reiwa, nv_sdp, nv_sansei, nv_others,
          was_aggregated, original_wards)
  
  return(harmonized)
}