## TODO
# Will need revision to harmonise the 3 methods for consistent handling of TOW etc. For now, suggest only using method = "ons_bua"
# Many HabCodes fall as Uncategorised and don't count towards final results: A3s, A3, C codes, I codes, unclassified. Can we assign them reasonable background values as they would receive deposition?

## Issues with ONS_LA:
# We might have no rates for a LA if a given habitat is not present. Luke revised the ONS figures elsewhere to give missing habitats in LAs the average values of neighbouring ones - could use similar approach.

## NCRAT method (national):
# come up with more elegant fix for parkland/hedges/TOW; currently receive average of wood and farmland. In ONS method we give them woodland codes
# Apart from that the results are very close to the Excel results, so we can trust the model.


#' Air Quality
#'
#' Runs the NCRAT v1.2 Air Quality using a habitat map for your place as input. It models the following pollutants: PM2.5, PM10, SO2, NO2, O3. It also reports the total, which is the sum of all pollutants (excluding PM2.5 which is a subset of PM10). The recommended method is the ONS one which assigns removal rates to habitats by local authority, offering a spatially-aware valuation.
#'
#' @param x a habitat basemap for an area of interest
#' @param habcode the name of the attribute containing the habitat classification. Must be "HabCode_B" for use with an ecoservR basemap (default), or "NEA_hab" if using the broad NCRAT classification (not implemented yet).
#' @param method one of "ons_la", "ons_bua" or "ncrat". Method "ons_la" uses the ONS Natural Capital Accounts 2024 data (removal rates by broad habitat and by local authority) - best for baseline assessment, but not suited to scenario modelling / uplift assessments. Method "ons_bua" uses a generalised version of the ONS data where removal rates have been averaged by country for urban vs rural areas. Best for scenario modelling.Method "ncrat" uses the national NCRAT methodology (one rate per broad habitat, not spatially variable). Default is ons_bua.
#' @param country UK country in which the study area is located (currently only EN (England) or SC (Scotland))
#' @param acc_year Accounting year to use for economic values.
#' @returns A nested list (per pollutant) of annual physical value, annual economic value, net present value (100y) and the unit of the biophysical value.
#' @export
ncrat_air_quality <- function(x,
                              habcode = "HabCode_B", # currently only accept HabCode_B
                              method = "ons_bua", # or "ons_la" or "ncrat"
                              country = "SC", # EN or SC
                              acc_year = format(Sys.Date(), "%Y")){
  
  # Checks ------------------------------------------------------------------
  
  # Method valid?
  if (!method %in% c("ons_la", "ons_bua", "ncrat")) {
    stop('Invalid method. Choose one of "ons" or "ncrat" as your evidence base for pollution removal rates.')
  }
  
  if (method %in% c("ons_la", "ncrat")){
    warning("This model is not finished / quality-checked; use with caution.")
  }
  
  # keep only relevant attribute
  x <- x[habcode]
  
  if (!acc_year %in% unique(lookup_deflator$year)) stop("Cannot provide value for accounting year ", acc_year)
  
  if (any(is.na(x$HabCode_B))){
    warning("Removing ", sum(is.na(x$HabCode_B)), " rows without habitat codes.")
    x <- x[!is.na(x$HabCode_B),]
  }
  if (any(!unique(x$HabCode_B) %in% unique(lookup_habitat$HabCode_B))){
    problem_codes <- setdiff(unique(x$HabCode_B), unique(lookup_habitat$HabCode_B))
    warning("Undocumented habitat codes, these will be ignored: \n", paste0(problem_codes, collapse = ", "))
    x <- x[!x$HabCode_B %in% problem_codes,]
  }
  
  
  # Load lookups ------------------------------------------------------------
  
  if (method == "ncrat"){
    
    ## Load the main NCRAT lookup (for England or Scotland - which is a modified version of the English one)
    value_lookup <-  if (country == "EN") ncrat_unit_value_lookup_EN else ncrat_unit_value_lookup_SC
    
    value_lookup <- value_lookup  %>%
      dplyr::filter(grepl("Air Quality", service)) %>%
      dplyr::filter(!grepl("(Local)", service)) # remove local rates (woodland PM2.5)
    
    warning("This model currently only offers valuation with national removal rates. To use local woodland PM2.5 removal rates, please use the Excel version.")
    
    # we want to end up with a "rates" object that has at least a "habitat", "pollutant", and "phys_flow_tonnes" columns
    rates <- value_lookup %>%
      dplyr::filter(data_type != "Input to monetary flow calculation") %>%  # remove input to monetary calcs
      
      dplyr::mutate(
        
        pollutant = dplyr::case_when(
          grepl("PM2.5", service) ~ "PM2.5",
          grepl("NO2", service) ~ "NO2",
          grepl("SO2", service) ~ "SO2",
          grepl("O3", service) ~ "O3"),
        
        indicator = dplyr::case_when(
          grepl("removal rate", indicator) ~ "phys_flow_t_ha",
          grepl("Total annual value", indicator) ~ "ann_val_t",    # annual value per tonne
          grepl("Total asset value", indicator) ~ "asset_val_t"    # npv value per tonne
        )
      ) %>%
      dplyr::select(service, pollutant, habitat = value_scope, indicator,value)
    
    # the monetary values have "all habitats"; instead we explode them to the actual habitats
    monet_exploded <- rates %>%
      dplyr::filter(habitat == "All habitats") %>%
      dplyr::select(-habitat) %>%
      tidyr::crossing(habitat = unique(rates$habitat)) # recycle values for each habitat
    
    # final object: recombine, get rid of "all habitats"
    rates <- rbind(rates, monet_exploded) %>%
      dplyr::filter(habitat != "All habitats") %>%
      tidyr::pivot_wider(names_from = indicator, values_from = value)
    
    rates$habitat <- tolower(rates$habitat) # forcing all lowercase as was having join problems
    
  }
  
  
  if (method == "ons_la"){
    
    ## Load the ONS lookup (removal rates and value by local authority)
    value_lookup <- lookup_air_ons
    
    ## Identify which LAs we need
    la <- LA %>% dplyr::filter(country_code == country)
    la <- la[unique(unlist(sf::st_intersects(sf::st_bbox(x) %>% sf::st_as_sfc(), la))),]
    
    ## Load the ONS lookup and filter to relevant data
    rates <- value_lookup %>%
      dplyr::filter(country_code == country) %>%
      dplyr::filter(la_code %in% !!la$la_code)   # weird !! to allow selection by external vec
    
    ## Clip map to LA to apply the rates
    if (nrow(la) > 1){
      x <- st_intersection(x, la)
    } else {
      x$la_code <- la$la_code
    }
    
  }
  
  
  if (method == "ons_bua"){
    
    ## Load the ONS lookup (removal rates and value for country, split by urban/rural)
    rates <- lookup_air_ons_bua %>% dplyr::filter(country_code == country)
    
    ## Split map between urban and rural, using BUA dataset
    ## Pragmatic approach to avoid slow intersection: anything fully within a BUA is urban, otherwise rural
    x$type <- "rural"  # start with default, will update
    bua_sub <- BUA[unique(unlist(sf::st_intersects(sf::st_as_sfc(sf::st_bbox(x)) ,BUA))),] #subset BUA to where we're working
    within_bua <- unique(unlist(sf::st_contains(bua_sub, x)))
    
    if (length(within_bua) > 0){
      x[within_bua,]$type <- "urban"  # update when we have features in BUA
    }
    
  }
  
  # Key values --------------------------------------------------------------
  
  ## Extract constants needed from lookups
  
  # Price year of the value
  if (method == "ncrat"){
    base_year <- dplyr::filter(value_lookup,
                               !grepl("(Local)", service),
                               data_type == "Monetary flow")$price_year %>% unique()
  } else {
    base_year = "2023"  # ONS Nat Cap Accounts 2024 used a 2023 value
  }
  
  # Inflation rate (deflator at desired year divided by deflator at price year)
  infl_rate <- dplyr::filter(lookup_deflator, year == acc_year)$deflator/
    dplyr::filter(lookup_deflator, year == base_year)$deflator
  
  
  
  # Process spatial data ----------------------------------------------------
  
  ## Classify habitats into broader NCRAT groups
  x <- dplyr::left_join(x, lookup_habitat, by = "HabCode_B")
  
  if (method == "ncrat"){
    x <- x %>% dplyr::rename(habitat = uknea)  # rename to ensure join later
    x$habitat <- tolower(x$habitat)        # fix join issue with value lookup
    
  } else if (grepl("ons", method)){
    
    ## ONS and NCRAT use slightly different wording for habitat types, ensure a match with the relevant lookup
    x <- x %>%
      dplyr::mutate(habitat = dplyr::case_when(
        HabCode_B %in% c("TOW", "J21", "J22", "J23") ~ "TOW", # trees outside woods; will be updated in next clause
        grepl("Broadleaved", uknea_sub) ~ "Broadleaf woodland",
        grepl("Coniferous", uknea_sub) ~ "Coniferous woodland",
        uknea == "Woodlands" ~ "Broadleaf woodland",  # unknown woods, assume BL
        uknea == "Mountains, moorlands and heaths"  ~ "Mountains, moorland, and heath", # small difference!
        uknea == "Semi-natural grasslands" ~ "Semi-natural grassland", # small difference!
        grepl("Freshwaters", uknea) ~ "Freshwater, wetlands, and floodplains",
        TRUE ~ uknea
      ))
  }
  
  if (method == "ons_la"){  # TODO: THIS STILL NEEDS REVIEWING; not using this method in the meantime
    
    ## For ONS (la version), we further want to identify the "urban trees" and "urban grass" categories
    bua <- BUA %>% dplyr::filter(country_code == country)
    bua <- bua[unique(unlist(sf::st_intersects(sf::st_bbox(x) %>% sf::st_as_sfc(), bua))),]
    
    ## TODO review, I don't think this is quite right. Shouldn't we be updating habitat instead of uknea? And Luke advises treating woodland as such, and only letting TOW be Urban trees.
    # if (nrow(bua) > 0){
    #
    #   x <- x %>%
    #     mutate(uknea = case_when(
    #       grepl("woodland", uknea, ignore.case=TRUE) & lengths(st_intersects(., bua)) > 0 ~ "Urban trees",
    #       grepl("grassland", uknea) ~ "Urban grassland",  # should this also pull out specifically amenity J12? (otherwise buried in "Urban")
    #       TRUE ~ uknea
    #     ))
    # }
  }
  
  
  if (method == "ons_bua"){
    # When using the BUA method, we always know if the habitats are urban or rural, so we can recode with precision
    x <- x %>%
      mutate(habitat = case_when(
        
        habitat == "TOW" & type == "urban" ~ "Urban trees",
        habitat == "TOW" & type == "rural" ~ "Broadleaf woodland" , # assume woodland capacity
        grepl("grassland", habitat) & type == "urban" ~ "Urban grassland",
        substr(HabCode_B, 1, 3) == "J12" & type == "urban" ~ "Urban grassland", # amenity and road verges
        TRUE ~ habitat
      ))
  }
  # some habitats don't get updated from urban, which doesn't have a rate
  
  # Calculate area, drop geom
  
  x <- x %>%
    dplyr::mutate(area_ha = as.numeric(sf::st_area(.))/10000) %>%
    sf::st_drop_geometry()
  
  
  # Prep rates --------------------------------------------------------------
  
  # (for ons_la method)
  # We might have no rates for a LA if a given habitat is not present.
  # What should we do? Give the average national rate?
  
  
  
  
  
  # Biophysical value -------------------------------------------------------
  
  # Regardless of method (ONS or NCRAT), join rates to habitats (by habitat, and also by type (urban vs rural) for ons_bua)
  x <-  dplyr::left_join(x, rates, #by = "habitat",
                         relationship = "many-to-many") # each poly expanded to all pollutants
  
  
  # Assign provisional rates to parkland and TOW
  if (method == "ncrat" & any(substr(x$HabCode_B, 1, 2) %in% c("A3", "TO"))){
    
    parkland_rates <- rates %>%
      dplyr::filter(habitat %in% c("enclosed farmland", "woodlands")) %>%
      dplyr::group_by(pollutant) %>%
      dplyr::summarise(dplyr::across(c("phys_flow_t_ha", "ann_val_t", "asset_val_t"),
                                     ~mean(.x, na.rm=TRUE))) %>%
      tidyr::crossing(HabCode_B = unique(x$HabCode_B[substr(x$HabCode_B, 1, 2) %in% c("A3", "TO")])) # assign to relevant habcodes
    
    parkland <- dplyr::filter(x, substr(HabCode_B, 1, 2) %in% c("A3", "TO")) %>%
      dplyr::select(-pollutant, -phys_flow_t_ha, -ann_val_t, -asset_val_t) %>%  # remove the NAs
      dplyr::left_join(parkland_rates, by = c("HabCode_B"), relationship="many-to-many")
    
    x <- x %>%
      dplyr::filter(!substr(HabCode_B, 1, 2) %in% c("A3", "TO")) %>%  # remove old rows
      rbind(parkland) # add new
  }
  
  
  # Calculate quantities (multiply area by removal rate) : phys_tonnes
  x <- x %>%
    dplyr::mutate(phys_tonnes = area_ha * phys_flow_t_ha) %>%
    dplyr::filter(!is.na(pollutant)) # remove rows that had no values (e.g. urban stuff)
  
  
  # Summarise by pollutant
  
  result_biophys <- x %>%
    dplyr::group_by(pollutant) %>%
    dplyr::summarise(phys = sum(phys_tonnes, na.rm=TRUE)) %>%
    dplyr::ungroup()
  
  
  
  # Economic value ----------------------------------------------------------
  
  ## This model is a bit different to other NCRAT models as it extracts the valuation results from another study (either Jones, from the NCRAT UVL, or the ONS NC Accounts value lookup). User doesn't get a say in NPV duration.
  
  ## Adjust the lookup monetary value for inflation
  x$ann_val_t <- infl_rate * x$ann_val_t
  x$asset_val_t <- infl_rate * x$asset_val_t
  
  
  ## Calculate and summarise by pollutant
  result_econ <- x %>%
    dplyr::mutate(econ = ann_val_t * phys_tonnes,
                  npv = asset_val_t * phys_tonnes) %>%
    dplyr::group_by(pollutant) %>%
    dplyr::summarise(
      econ = sum(econ, na.rm=TRUE),
      npv = sum(npv, na.rm = TRUE)
    )
  
  
  # # Return result -----------------------------------------------------------
  
  # In one df:
  final <- dplyr::left_join(result_biophys, result_econ, by = "pollutant") %>%
    rbind(data.frame(
      pollutant = "total",
      phys = if ("PM10" %in% unique(result_biophys$pollutant)) sum(dplyr::filter(result_biophys, pollutant != "PM2.5")$phys) else sum(result_biophys$phys),
      econ = sum(result_econ$econ),
      npv = sum(result_econ$npv)
    ))
  
  return(final)
  
  # # Or as lists
  # return(list(
  #   phys = setNames(final$phys, final$pollutant),
  #   econ = setNames(final$econ, final$pollutant),
  #   npv =  setNames(final$npv, final$pollutant),
  #   unit_phys = "tonnes/y"
  # ))
  
}