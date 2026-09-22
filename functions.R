#### process historical climate data ####
# needs file path and shape_file
preprocess <- function(file_path, shape_file){
  get_measure <- terra::rast(file_path)
  
  # make sure time is in col name
  times <- terra::time(get_measure)
  names(get_measure) <- as.character(times)
  
  e           <- terra::ext(vect(shape_file))
  cropped     <- terra::crop(get_measure, e)
  
  # Resample weights to match the value raster's grid exactly
  weight_aligned <- terra::resample(HSL_GER, cropped, method = "sum")
  
  extract     <- exact_extract(cropped, shape_file,
                               weights = weight_aligned,
                               fun="weighted_mean")
}
# function to calculate heat_index ---------------------------------------------------------------
## obtained from https://github.com/benmarhnia-lab/heat_index_nws/blob/main/heat_index.R
## for testing: ## temperature = hosp_daily$mean_temp; dewpoint = hosp_daily$dew_point; dp = TRUE; dp_f = FALSE; t_f = FALSE
heat_index <- function(
    temperature, ## air temperature
    rh, ## percentage of relative humidity; NA if using dew point
    dewpoint, ## dew point temperature; NA if using relative humidity
    dp, ## TRUE or FALSE; TRUE if dew point temperature is used
    t_f, ## TRUE or FALSE; if TRUE, air temperature is provided in Fahrenheit
    dp_f, ## TRUE or FALSE; if TRUE, dew point temperature is provided in Fahrenheit
    rh_original ## TRUE or FALSE; if TURE, relative humidity ranges from 0 to 1 and will be transformed to a range of 0 to 100
) {
  ## get temperature in right unit
  if (t_f) {
    temperature_c <- (temperature - 32) * 5 / 9
  } else {
    temperature_c <- temperature
    temperature <- (temperature_c * 9 / 5) + 32
  }
  
  ## get relative humidity from dew point or wise versa: August-Roche-Magnus approximation
  a <- 17.62
  b <- 243.12
  if (dp) { ## transformation based on simplified equation (works for -45 to 60 celsius): https://www.npl.co.uk/resources/q-a/dew-point-and-relative-humidity
    if (dp_f) {
      dewpoint_c <- (dewpoint - 32) * 5 / 9
    } else {
      dewpoint_c <- dewpoint
      dewpoint <- (dewpoint_c * 9 / 5) + 32
    }
    rh <- 100 * exp(a * dewpoint_c / (b + dewpoint_c)) / exp(a * temperature_c / (b + temperature_c))
  } else {
    if (rh_original) {
      rh <- rh * 100
    }
    dewpoint_c <- (b*(log(rh/100) + a*temperature_c/(b+temperature_c))) / (a - log(rh/100) - a*temperature_c/(b+temperature_c))
    dewpoint <- (dewpoint_c * 9 / 5) + 32
  }
  
  
  ## calculate heat index using the NWS equation for fahrenheit based on the NWS equation: https://www.wpc.ncep.noaa.gov/html/heatindex_equation.shtml
  hi_sim <- 0.5 * (temperature + 61.0 + ((temperature-68.0)*1.2) + (rh*0.094))
  hi_full <- -42.379 + 2.04901523*temperature + 10.14333127*rh - .22475541*temperature*rh - .00683783*temperature*temperature - .05481717*rh*rh + .00122874*temperature*temperature*rh + .00085282*temperature*rh*rh - .00000199*temperature*temperature*rh*rh
  
  loc_lowrh <- which(rh < 13 & temperature > 80 & temperature < 112)
  lowrh_adjustement <- ((13-rh)/4)* sqrt((17-abs(temperature-95))/17)
  hi_full[loc_lowrh] <- hi_full[loc_lowrh] - lowrh_adjustement[loc_lowrh]
  
  loc_highrh <- which(rh > 85 & temperature < 87 & temperature > 80)
  highrh_adjustement <- ((rh-85)/10) * ((87-temperature)/5)
  hi_full[loc_highrh] <- hi_full[loc_highrh] + highrh_adjustement[loc_highrh]
  
  hi <- hi_sim
  loc <- which(hi>=80)
  hi[loc] <- hi_full[loc]
  hi_c <- (hi - 32) * 5 / 9
  
  ## Caldulate the apparent temperature using Table 1 of Kalkstein and Valimont 1986: https://ciesin.columbia.edu/docs/001-609/001-609.html
  ## apparent temperature would be outrageous when dew point temperature <0 celsius
  at <- -2.653 + 0.994 * temperature_c + 0.0153 * dewpoint_c * dewpoint_c 
  at_f <- (at * 9 / 5) + 32
  
  ## replace heat index <68 by air temperature--should use wind chill but don't have the wind data
  loc_68 <- which(temperature<68)
  hi_final <- hi
  hi_final[loc_68] <- temperature[loc_68]
  hi_final_c <- hi_c
  hi_final_c[loc_68] <- temperature_c[loc_68]
  at_final <- at_f
  at_final[loc_68] <- temperature[loc_68]
  at_final_c <- at
  at_final_c[loc_68] <- temperature_c[loc_68]
  
  ## test results--everything included
  # return(data.frame(hi=hi_final, at=at_final, hi=hi, hi_sim=hi_sim, hi_full=hi_full, at=at_f, temperature=temperature, dp=dewpoint, hi_c=hi_c, at_c=at_final_c, hi_c=hi_c, at_c=at, temperature_c = temperature_c, dp_c=dewpoint_c, rh=rh))
  ## results with cutpoint at 68--air temperature used when smaller than this
  return(data.frame(hi=hi_final, at=at_final, hi_c=hi_final_c, at_c=at_final_c))
  ## results without cutpoint at 68
  # return(data.frame(hi=hi, at=at_f, hi_c=hi_c, at_c=at))
  ## reporting one set of results (heat index in F with cut point in 68F)
  # return(hi=hi_final)
}



# TE-VIM function ----------------------------------------------------------
## see https://pubmed.ncbi.nlm.nih.gov/41437933/ 
# where po are individual DR scores
# cate is 
# sub_cate is CATE based on leaving one var out
te_vim <- function(po, cate, sub_cate) {
  # average treatment effect
  n <- length(po)
  ate <- sum(po) / n
  
  # three residual like terms
  r_ate <- (po - ate)^2
  r_cate <- (po - cate)^2
  r_subcate <- (po - sub_cate)^2
  
  # evaluate TE-VIM (Theta_s in the paper)
  tevim <- sum(r_subcate - r_cate) / n
  infl <- r_subcate - r_cate - tevim
  std_err <- sqrt(sum(infl^2)) / n
  
  # evaluate scaled TE-VIM (Psi_s in the paper)
  vte <- sum(r_ate - r_cate) / n
  tevim_s <- tevim / vte
  infl <- (r_subcate - tevim_s * r_ate + (tevim_s - 1) * r_cate) / vte
  std_err_s <- sqrt(sum(infl^2)) / n
  
  list(
    tevim = tevim,
    std_err = std_err,
    tevim_s = tevim_s,
    std_err_s = std_err_s
  )
}
