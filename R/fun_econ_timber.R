#' Timber Production
#'
#' Runs the NCRAT v1.2 Timber model using a habitat map for your place as input.
#'
#' @param x a habitat basemap for an area of interest
#' @param habcode the name of the attribute containing the habitat classification. Must be "HabCode_B" for use with an ecoservR basemap (default), or "NEA_hab" if using the broad NCRAT classification (not implemented yet).
#' @param country UK country in which the study area is located (currently only EN (England) or SC (Scotland))
#' @param acc_year Accounting year to use for economic values.
#' @returns A dataframe of annual yield, annual value, and discounted value over 100 years, split by produce type, like NCRAT returns as summary.
#' @export
econ_timber <- function(x,
                         habcode = c("HabCode_B", "NEA_hab"), # currently only accept HabCode_B
                         country = c("EN", "SC"),
                         acc_year = format(Sys.Date(), "%Y"),
                         npv_period = 100){



  # Load lookups ------------------------------------------------------------

  ## Load the main NCRAT lookup (for England or Scotland - which is a modified version of the English one)
  value_lookup <- if (country == "EN") ncrat_unit_value_lookup_EN else ncrat_unit_value_lookup_SC

  value_lookup <- value_lookup  %>%
    dplyr::filter(service == "Ecosystem Service - Timber")

  ## Discount rates
  discount <- lookup_discount %>%
    dplyr::select(year, discount = std_rate) # select standard or health rate as appropriate for model

  # Checks ------------------------------------------------------------------

  # keep only relevant attribute
  x <- x[habcode]

  # Remove NA values
  x <- na.omit(x)

  if (!acc_year %in% unique(lookup_deflator$year)) stop("Cannot provide value for accounting year ", acc_year)

  if (any(!unique(x$HabCode_B) %in% unique(lookup_habitat$HabCode_B))){
    problem_codes <- setdiff(unique(x$HabCode_B), unique(lookup_habitat$HabCode_B))
    stop("Undocumented habitat codes: \n", paste0(problem_codes, collapse = ", "))
  }

  if (!dplyr::between(npv_period, 0, 100)) stop("Period for Net Present Value must be between 0-100 years.")


  # Key values --------------------------------------------------------------

  ## Extract constants needed from lookups

  # Price year of the value
  base_year <- dplyr::filter(value_lookup, data_type == "Monetary flow")$price_year

  # Inflation rate (deflator at desired year divided by deflator at price year)
  infl_rate <- dplyr::filter(lookup_deflator, year == acc_year)$deflator/
    dplyr::filter(lookup_deflator, year == base_year)$deflator

  ## Adjust the lookup monetary value for inflation
  adj_value <- infl_rate * dplyr::filter(value_lookup, data_type == "Monetary flow")$value

  ## The biophysical rate
  biophys_rate <- dplyr::filter(value_lookup, data_type == "Physical quantities")$value # in m3/ha/yr


  # sense check message
  message("Working with annual biophysical rate of ", round(biophys_rate,2), " ",
          dplyr::filter(value_lookup, data_type == "Physical quantities")$unit,
          " and a monetary flow of GBP ", round(adj_value,2), "/yr (", acc_year, " value)")




  # Process spatial data ----------------------------------------------------

  ## Join NCRAT habitats
  landcover <- dplyr::left_join(x, lookup_habitat, by = "HabCode_B")


  ## (Any other model-specific step)




  ## Keep only relevant habitats for model: for timber we consider woodland only
  woodland <- dplyr::filter(landcover,
                            grepl("A1", HabCode_B)) %>%  # to discuss: this considers woodland ONLY rather than the broad ncrat "woodland" category where we also capture parkland etc.
    dplyr::mutate(area_ha = as.numeric(sf::st_area(.))/10000) %>% # convert to ha
    sf::st_drop_geometry() # no longer needed


  # Biophysical value -------------------------------------------------------

  # This is the total amount of eligible habitat (ha) multiplied by the unit rate (m3 of timber per ha per yr)

  result_biophys <- sum(woodland$area_ha)*biophys_rate

  # NB: value will be 0 if the filtering of woodland yields an empty sf object; as expected.


  # Economic value ----------------------------------------------------------

  # Annual flow is the biophysical value multiplied by the annual monetary value.

  result_econ <- result_biophys * adj_value



  # Net present value -------------------------------------------------------

  # If desired and feasible for this model

  if (npv_period == 0){
    result_npv <- NA
  } else if (npv_period == 1){
    result_npv = result_econ
  } else{

    ## as per NCRAT excel sheet: result of previous year, divided by (1 + rate for this year)

    discount <- discount %>%
      dplyr::filter(dplyr::between(year, 1, npv_period-1)) %>% # lookup starts at 0 but we discount to start at 1
      dplyr::arrange(year) %>%  # important to have in order
      dplyr::mutate(discount_factor = purrr::accumulate(discount, ~ .x / (1 + .y), .init = 1)[-1])

    discount <- discount %>% rbind(data.frame(year =0, discount=NA, discount_factor=1)) # add back this current year


    ## Multiply each year value (here assumed constant) by its discount factor, sum
    result_npv <- sum(discount$discount_factor*result_econ)

  }


  # Return result -----------------------------------------------------------


  return(list(
    phys = result_biophys,
    econ = result_econ,
    npv = result_npv,
    unit_phys = "m3/yr" #dplyr::filter(value_lookup, data_type == "Physical quantities")$unit
  ))



}


