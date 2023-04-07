thousands <- function(l) {
  res <- l
  bitmap <- (l >= 1000000)
  res[!is.na(l) & bitmap] <- round(l[!is.na(l) & bitmap]/1000000, 1)
  res[!is.na(l) & bitmap] <- paste0(res[!is.na(l) & bitmap], "M")
  
  bitmap <- (l >= 1000 & l < 1000000)
  res[!is.na(l) & bitmap] <- round(l[!is.na(l) & bitmap]/1000, 1)
  res[!is.na(l) & bitmap] <- paste0(res[!is.na(l) & bitmap], "K")
  res[!is.na(l) & res=="0K"] <- "0"
  return(res)
}

outcome_titles <- list("log_median_income"="Median Household Income",
                       "income_percentage_below_25K"="Fraction Household Income Below 25K",
                       "unemployment_rate"="Unemployment Rate (%)",
                       "kir_top20_pooled_pooled_p25"="Prob Reaching Top 20% From Bottom 25%",
                       "kir_pooled_pooled_p25"="% Rank of Child's Income From Bottom 25%",
                       "phone_price"="Mobile Phone Price",
                       "num_devices"="Number of Unique Devices",
                       "num_countries_visited"="Number of Countries Visited Short Trips",
                       "zip_log_income_median_family"="Zipcode Log Median Family Income",
                       "zip_log_income_mean_family"="Zipcode Log Mean Family Income",
                       "num_donations"="Number of Donations")
agg_var_titles <- list("fraction_long_edges"="Zipcode Fraction of Long Edges",
                       "fraction_weighted_long_edges"="Zipcode Weighted Fraction of Long Edges")
indiv_var_titles <- list("fraction_long_edges"="Fraction of Long Edges",
                         "fraction_weighted_long_edges"="Weighted Fraction of Long Edges",
                         "fraction_long_edges_bracket_mean"="Fraction of Long Edges",
                         "weighted_long_edges_bracket_mean"="Weighted Fraction of Long Edges")

life_event_labels <- list("degree"="Degree",
                          "degree_in_region"="Current State Degree",
                          "degree_out_college"="Outside-College Degree",
                          "degree_out_high_school"="Outside-School Degree",
                          "fraction_long_edges"="Long Ties Fraction",
                          "fraction_long_edges_in_region"="Current State Long Ties Fraction",
                          "fraction_long_edges_out_college"="Outside-College Long Ties Fraction",
                          "fraction_long_edges_out_high_school"="Outside-School Long Ties Fraction",
                          "in_region"="Current State Ties Only",
                          "out_college"="Outside-College Ties Only",
                          "out_high_school"="Outside-School Ties Only",
                          "during_closure"="School Closure")