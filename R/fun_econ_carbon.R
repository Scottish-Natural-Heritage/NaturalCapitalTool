## TODO: CORRECT FOR MIN/MED/MAX BIOPHYS ESTIMATES  (NCRAT assigns different rates to heath, saltmarsh and mudflats to reflect uncertainty, and gives a range of biophysical estimates. These can then be used in the economic stuff too)



#' Carbon Sequestration
#'
#' Runs a carbon sequestration model (different values to NCRAT v1.2) and generates an econonomic value (same as NCRAT v1.2).
#'
#' @param x a habitat basemap for an area of interest
#' @param habcode the name of the attribute containing the habitat classification. Must be "HabCode_B" for use with an ecoservR basemap (default), or "NEA_hab" if using the broad NCRAT classification (not implemented yet).
#' @param peatcondition the name of the attribute containing peat condition information (for peatland polygons). The values must be among "eroding", "drained", "modified", "natural". Use this to provide local knowledge; otherwise the polygons will be queried against a national peat map (not implemented yet).
#' @param country UK country in which the study area is located (currently only EN (England) or SC (Scotland))
#' @param acc_year Accounting year to use for economic values.
#' @returns A list of annual physical value, annual economic value (nested list with low/central/high projection), net present value for chosen period  (nested list with low/central/high projection), and the unit of the biophysical value.
#' @export
econ_carbon <- function(x,
                         habcode = c("HabCode_B", "NEA_hab"), # currently only accept HabCode_B
                         peatcondition = NULL,                # currently no way of assigning peat automatically, must be column provided by user
                         default_peat = "modified",   # TODO convert into a df of proportions of each type
                         country = "EN", #c("EN", "SC"),
                         acc_year = as.numeric(format(Sys.Date(), "%Y")),
                         npv_period = 100){



  # Load lookups ------------------------------------------------------------

  ## Load the main NCRAT lookup (for England or Scotland - which is a modified version of the English one)
  # TODO remove mutate line once lookup is a clean package object
  value_lookup <- if (country == "EN") ncrat_unit_value_lookup_EN else ncrat_unit_value_lookup_SC

  value_lookup <- value_lookup  %>%
    dplyr::filter(service == "Ecosystem Service - Climate Regulation")

  ## We don't actually use NCRAT's sequestration rates by habitat, we will use ecoservR ones (mostly from NE report), already built into package as internal object

  ## Extract the peat rates by condition
  peat_rates <- value_lookup %>%
    dplyr::filter(indicator == "Emission rates of peatland by condition") %>%
    dplyr::select(condition = value_scope, rate = single_value)  # in tCO2e/ha/yr

  peat_rates$condition <- factor(peat_rates$condition,
                                 levels = c("Actively eroding", "Drained", "Modified", "Near natural"),
                                 labels = c("eroding", "drained", "modified", "natural"))


  ## Discount rates
  discount <- lookup_discount %>%
    dplyr::select(year, discount = std_rate) %>%  # select standard or health rate as appropriate for model
    dplyr::mutate(discount_factor = purrr::accumulate(discount, ~ .x / (1 + .y), .init = 1)[-100])  # calc discount factor



  # Checks ------------------------------------------------------------------

  # keep only relevant attribute
  x <- x[c(habcode, peatcondition)]
  
  # Remove NA values
  x <- na.omit(x)
  
  

  # check year
  if (!acc_year %in% unique(lookup_deflator$year)) stop("Cannot provide value for accounting year ", acc_year)

  if (any(!unique(x$HabCode_B) %in% unique(lookup_habitat$HabCode_B))){
    problem_codes <- setdiff(unique(x$HabCode_B), unique(lookup_habitat$HabCode_B))
    stop("Undocumented habitat codes: \n", paste0(problem_codes, collapse = ", "))
  }

  if (!dplyr::between(npv_period, 0, 100)) stop("Period for Net Present Value must be between 0-100 years.")


  # check peatland condition values
  if (!is.null(peatcondition)){
    x <- x %>%
      dplyr::rename(peat_condition = !!dplyr::sym(peatcondition)) # rename attribute

  unrecognised_condition <- setdiff(unique(x$peat_condition), c("eroding", "drained", "modified", "natural"))
  unrecognised_condition <- unrecognised_condition[!is.na(unrecognised_condition)]

  if (length(unrecognised_condition) > 0) stop("Unrecognized peat condition values: ", paste0(unrecognised_condition, collapse = ", "), " . Please recode to these terms: natural / eroding / modified / drained ")


  }else{
    x$peat_condition <- NA
  }


  # Key values --------------------------------------------------------------

  ## Extract constants needed from lookups

  # Price year of the value
  base_year <- dplyr::filter(value_lookup, data_type == "Monetary flow")$price_year

  # Inflation rate (deflator at desired year divided by deflator at price year)
  infl_rate <- dplyr::filter(lookup_deflator, year == acc_year)$deflator/
    dplyr::filter(lookup_deflator, year == base_year)$deflator



  # Process spatial data ----------------------------------------------------

  # TODO revise this so would also work with NCRAT hab codes (currently just coded for HabCode_B)
  # embed check against peatland map (when/if available) and use that condition data when no condition info has been supplied
  # (wherever there is a condition attribute for a polygon, trust it over the map)

  ## If there is any peat, check or assign condition and revise rates accordingly
  x_peat <- dplyr::filter(x,
                          substr(HabCode_B, 1, 1) == "E")  # E codes are bogs and fens: peaty soils

  x <- dplyr::filter(x, substr(HabCode_B, 1, 1) != "E")  # remove from main habitat map

  if (nrow(x_peat) > 0){

  ## When there is no condition attribute, or there is one but it has some missing values, query the map
  if (is.null(peatcondition) | any(is.na(x_peat$peat_condition))){

  ## Intersect with peat condition map (TBC March 2025 for England - no Scotland equivalent?)


  ## Consolidate the condition attribute (trust map when no data only)


  if (any(is.na(x_peat$peat_condition))){   #workaround for now- set global default given by user

    ## TODO change in a clever bit that assigns condition class to certain proportions (input by user)
    # for now just give same value everywhere
    x_peat[is.na(x_peat$peat_condition),]$peat_condition <- default_peat
    message("Assigning ", default_peat, " condition to unclassified peat")
    }

  }

  ## Now we have a peat object with a condition column, assign rates
  x_peat <- dplyr::left_join(x_peat, peat_rates, by = c("peat_condition" = "condition")) %>%
    dplyr::mutate(area_ha = as.numeric(sf::st_area(.))/10000) %>% # convert to ha
    sf::st_drop_geometry() %>%  # no longer needed
    dplyr::mutate(carbon_seq = area_ha*rate) %>%
    dplyr::select(HabCode_B, carbon_seq)



  } # end peat condition assessment


  ## Join the sequestration by habitat rates to the data, calculate area
  x <- dplyr::left_join(x, lookup_carbon_seq) %>%
    dplyr::mutate(area_ha = as.numeric(sf::st_area(.))/10000) %>% # convert to ha
    sf::st_drop_geometry() %>%  # no longer needed
    dplyr::mutate(carbon_seq = area_ha*seq_tCO2_ha_y) %>%
    dplyr::select(HabCode_B, carbon_seq)


  ## Recombine
  if (nrow(x_peat > 0)){
    x <- rbind(x, x_peat)
  }



  # Biophysical value -------------------------------------------------------

  ## TODO we should have low/central/high physical estimates too (based on some habitats receiving a low/med/high estimate to reflect uncertainty)

  # This is the total amount of carbon sequestered by all habitats in the input data
  # negative is sequestration, positive is emissions; flip it! so that econ values make intuitive sense

  result_biophys <- sum(x$carbon_seq, na.rm=TRUE)*-1


  ## TODO enable the low/central/high values, for now just set it all to same
  result_biophys <- list(
    low = sum(x$carbon_seq, na.rm=TRUE)*-1,
    central = sum(x$carbon_seq, na.rm=TRUE)*-1,
    high = sum(x$carbon_seq, na.rm=TRUE)*-1
  )


  # Economic value ----------------------------------------------------------

  # We produce a low/med/high estimate, adjusting for inflation to selected accounting year

  econ <- lookup_carbon_val %>%   # internal object: carbon values in 2020 prices (see base_year)
        dplyr::mutate(
      dplyr::across(.cols = c("low", "central", "high"),
                    ~.x * infl_rate)) %>%
    dplyr::filter(year >= acc_year)  # start at accounting year, for NPV calc


  # Annual flow is the biophysical value multiplied by the annual monetary value.
  ## TODO we should have low/central/high physical estimates too, multiplied by the low/central/high rates
  result_econ <- list(
    low = result_biophys[["low"]] * econ[econ$year == acc_year,]$low,      #result_biophys[LOW]...
    med = result_biophys[["central"]] * econ[econ$year == acc_year,]$central,
    high= result_biophys[["high"]] * econ[econ$year == acc_year,]$high
)


  # Net present value -------------------------------------------------------

  # This uses the projections from the carbon value lookup up to 2050.
  # After 2050, a real annual growth rate of 1.5% is applied starting at the most recently published value for 2050.

  # when year <= 2050, value in Y is from the lookup;
  # when year > 2050, value in Y is [value in Y-1] * (1 + 1.5%)


  if (npv_period == 0){
    result_npv <- NA
  } else if (npv_period == 1){
    result_npv = result_econ

  } else{  # BEGIN NPV CALCULATION ---

    # Create mini lookup of the years we want NPV for (calendar years and indexed year starting 0)
    npv_years <- data.frame(
      year = c(acc_year:(acc_year+npv_period)),
      forecast_year = 0:npv_period  # will be one longer than requested, but that's how NCRAT does it
    )

    ## Join the values from the non-traded list
    npv_years <- dplyr::left_join(npv_years, econ)  # will fill up to 2050 but not beyond


    ## if the period goes beyond 2050, apply the 1.5% increase
    if (max(npv_years$year) > max(econ$year)){

      npv_increase <- dplyr::filter(npv_years, year >= max(econ$year)) %>%  # subset to just years we need; we need the last value
        dplyr::arrange(year) %>%  # important to have in order
        dplyr::mutate(
          across(c("low", "central", "high"), ~purrr::accumulate(.x, ~.x*1.015)))


    ## Recombine with main data
    npv_years <- rbind(npv_years %>% dplyr::filter(year < max(econ$year)),
                       npv_increase)

    }


    ## Now we have the full NPV dataframe; we can apply the economic value and discount factor to the physical values
    ## TODO we should have low/central/high physical estimates too

    npv <- dplyr::left_join(npv_years,
                            discount, by = c("forecast_year" = "year"))  # join discount factor to each row


    # Final calc     ## TODO: CORRECT FOR MIN/MED/MAX BIOPHYS ESTIMATES
    result_npv <- npv %>%
      dplyr::mutate(across(c("low", "central", "high"), ~.x * result_biophys[["central"]] * discount_factor)) %>%
      dplyr::summarise(across(c("low", "central", "high"), ~sum(.x) )) %>%
      as.list()

  }

  # Return result -----------------------------------------------------------


  # return(list(
  #   phys = result_biophys,   # should be low, central, high as well
  #   econ = result_econ,
  #   npv = result_npv,
  #   unit_phys = "tCO2/y"
  # ))

   return(list(
     phys = result_biophys[["central"]],
     econ = result_econ[["med"]],
     npv = result_npv[["central"]],
     unit_phys = "tCO2/y"
   ))




}


