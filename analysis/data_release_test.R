library(dplyr)
library(ggplot2)
library(reshape2)
library(binsreg)
library(ggmap)
library(usmap)
library(mxmaps)
setwd("~/research/long_ties_economic_outcomes/")

process_release_geounit_data <- function(data,
                                         num_cc_brackets,
                                         num_le_brackets,
                                         num_degrees_brackets,
                                         num_pop_brackets) {
  
  colnames(data) <- make.unique(colnames(data))
  if("income_mean_househod" %in% colnames(data)) {
    data <- data[!is.na(data$income_mean_household),]
  }
  if("unemployment_rate" %in% colnames(data)) {
    data <- data[!is.na(data$unemployment_rate),]
  }
  if("income_median_household" %in% colnames(data)) {
    data$log_income <- log(data$income_median_household)
  }
  if(!("race_white" %in% colnames(data))) {
    data$race_white <- 1
  }
  data$log_population <- log(data$population)
  
  
  data <- data %>% mutate(clustering_coef_bracket=as.factor(ntile(clustering_coef, num_cc_brackets)),
                          weighted_clustering_coef_bracket=as.factor(ntile(weighted_clustering_coef, num_cc_brackets)),
                          fraction_long_edges_bracket=as.factor(ntile(fraction_long_edges, num_le_brackets)),
                          weighted_fraction_long_edges_bracket=as.factor(ntile(weighted_fraction_long_edges, num_le_brackets)),
                          mean_degree_bracket=as.factor(ntile(mean_degree, num_degrees_brackets)),
                          population_bracket=as.factor(ntile(population, num_pop_brackets)),
                          race_white_bracket=as.factor(ntile(race_white, num_pop_brackets)))
  if("mean_degree_out" %in% colnames(data)) {
    data <- data %>% mutate(mean_degree_out_bracket=as.factor(ntile(mean_degree_out, num_degrees_brackets)))
  }
  return(data)
}


#######################################################################################

data_dir <- './data/'

#####
# Note on data:
# The zipcode and county level data on frequency of long ties can be downloaded at the following link:
# https://socialmediaarchive.org/record/27
# You can find the following files at the above link:
# mx_zipcode_long_ties.csv, us_zipcode_long_ties.csv, mx_county_long_ties.csv, us_county_long_ties.csv
#
# The outcome data below is not released, but can be easily constructed using third party sources, like
# the ACS from the census bureau or the opportunity insights page: https://opportunityinsights.org/
# The outcome data mainly contains the following information per geo-unit: population, median income,
# racial composition, unemployment rate and the probability of social upward mobility
#
##### Zipcode level data
mx_zipcode_data <- read.csv(file.path(data_dir, 'release', 'mx_zipcode_long_ties.csv'),
                            colClasses=c('unit_id'='character'))
us_zipcode_data <- read.csv(file.path(data_dir, 'release', 'us_zipcode_long_ties.csv'),
                            colClasses=c('unit_id'='character'))

us_zipcode_outcomes_data <- read.csv('./data/zipcode_data.csv', colClasses=c('zipcode'='character'))
us_zipcode_outcomes_data <- us_zipcode_outcomes_data[,c("zipcode", "income_median_household", "race_white",
                                                        "kir_top20_pooled_pooled_p25", "kir_pooled_pooled_p25",
                                                        "unemployment_rate", "population")]
us_zipcode_outcomes_data$population <- as.numeric(us_zipcode_outcomes_data$population)
us_zipcode_data <- inner_join(us_zipcode_outcomes_data, us_zipcode_data, by=join_by(zipcode == unit_id))
us_zipcode_data <- us_zipcode_data[!is.na(us_zipcode_data$population),]
us_zipcode_data <- us_zipcode_data[us_zipcode_data$population > 150, ]

mx_zipcode_outcomes_data <- read.csv('./data/release/mx_zipcode_data.csv', colClasses=c('zipcode'='character'))
mx_zipcode_data <- inner_join(mx_zipcode_outcomes_data, mx_zipcode_data, by=join_by(zipcode == unit_id))
mx_zipcode_data <- mx_zipcode_data[!is.na(mx_zipcode_data$population),]
mx_zipcode_data <- mx_zipcode_data[mx_zipcode_data$population > 150, ]

##### County level data
mx_county_data <- read.csv(file.path(data_dir, 'release', 'mx_county_long_ties.csv'),
                            colClasses=c('unit_id'='character'))
us_county_data <- read.csv(file.path(data_dir, 'release', 'us_county_long_ties.csv'),
                            colClasses=c('unit_id'='character'))

us_county_pop <- read.csv('./data/release/us_counties_pop.csv', colClasses=c('county'='character'))
us_county_data <- inner_join(us_county_pop, us_county_data, by=join_by(county == unit_id))
us_county_data <- us_county_data[us_county_data$population > 500,]

mx_county_pop <- read.csv('./data/release/mx_counties_pop.csv', colClasses=c('county'='character'))
mx_county_pop$population <- mx_county_pop$population_estimate
mx_county_data <- inner_join(mx_county_pop, mx_county_data, by=join_by(county == unit_id))
mx_county_data <- mx_county_data[mx_county_data$population_estimate > 500,]





###################################
country <- 'MX'
country <- 'US'
if(country == 'US') {
  df <- us_zipcode_data
} else if(country == 'MX') {
  df <- mx_zipcode_data
}

df <- process_release_geounit_data(df,
                                   num_cc_brackets=20,
                                   num_le_brackets=20,
                                   num_degrees_brackets=20,
                                   num_pop_brackets=15)

print(paste('There are', length(unique(df$clustering_coef_bracket)), 'clustering coef brackets'))
print(paste('There are', length(unique(df$weighted_clustering_coef_bracket)), 'weighted clustering coef brackets'))
print(paste('There are', length(unique(df$fraction_long_edges_bracket)), 'fraction_long_edges brackets'))
print(paste('There are', length(unique(df$weighted_fraction_long_edges_bracket)), 'weighted_fraction_long_edges brackets'))
print(paste('There are', length(unique(df$mean_degree_bracket)), 'mean_degree brackets'))
print(paste('There are', length(unique(df$population_bracket)), 'population brackets'))

outcome_vars <- c('income_median_household', 'log_income',
                  'unemployment_rate',
                  'kir_pooled_pooled_p25', 'kir_top20_pooled_pooled_p25',
                  'index_pca')

################################### FRACTION OF LONG TIES & ECONOMIC INDICATORS #####################################
indep_var <- 'fraction_long_edges'
dep_var <- outcome_vars[2]

if(country == 'US') {
  control_vars <- c('population_bracket', 'mean_degree_bracket', 'race_white_bracket')
} else if(country == 'MX') {
  control_vars <- c('population_bracket', 'mean_degree_bracket')
  # the only available outcome var for Mexico
  dep_var <- outcome_vars[6]
}

cat(paste0('Control vars: ', paste(control_vars, collapse=','), '\n',
           'Indep var: ', indep_var, '\n',
           'Dep var: ', dep_var))

df_comp <- df[, c(dep_var, indep_var, control_vars)]
df_comp <- df_comp[complete.cases(df_comp),]
controls_mat <- model.matrix(as.formula(paste('~ 0 + ', paste(control_vars, collapse=" + "))), df_comp)
dim(controls_mat)

#ggplot(df, aes_string(indep_var)) + geom_histogram(bins=60)
res <- binsreg(y=df_comp[[dep_var]], x=df_comp[[indep_var]],
               w=controls_mat,
               dots=c(0,0),
               #dotsgrid=0,
               #line=c(2,2),
               #polyreg=3,
               ci=c(1,0),
               #cb=c(3,3),
               #nbins=30,
               level=95)

################################### CLUSTERING COEFFICIENT & ECONOMIC INDICATORS #####################################
indep_var <- 'clustering_coef'
dep_var <- outcome_vars[2]

if(country == 'US') {
  control_vars <- c('population_bracket', 'mean_degree_bracket', 'race_white_bracket')
} else if(country == 'MX') {
  control_vars <- c('population_bracket', 'mean_degree_bracket')
  # the only available outcome var for Mexico
  dep_var <- outcome_vars[6]
}

cat(paste0('Control vars: ', paste(control_vars, collapse=','), '\n',
           'Indep var: ', indep_var, '\n',
           'Dep var: ', dep_var))

df_comp <- df[, c(dep_var, indep_var, control_vars)]
df_comp <- df_comp[complete.cases(df_comp),]
controls_mat <- model.matrix(as.formula(paste('~ 0 + ', paste(control_vars, collapse=" + "))), df_comp)
dim(controls_mat)

#ggplot(df, aes_string(indep_var)) + geom_histogram(bins=60)
res <- binsreg(y=df_comp[[dep_var]], x=df_comp[[indep_var]],
               w=controls_mat,
               dots=c(0,0),
               #dotsgrid=0,
               #line=c(2,2),
               #polyreg=3,
               ci=c(1,0),
               #cb=c(3,3),
               #nbins=30,
               level=95)

################################### WEIGHTED FRACTION OF LONG TIES & ECONOMIC INDICATORS #####################################
indep_var <- 'weighted_fraction_long_edges'
dep_var <- outcome_vars[2]

if(country == 'US') {
  control_vars <- c('population_bracket', 'mean_degree_bracket', 'race_white_bracket', 'fraction_long_edges_bracket')
} else if(country == 'MX') {
  control_vars <- c('population_bracket', 'mean_degree_bracket', 'fraction_long_edges_bracket')
  # the only available outcome var for Mexico
  dep_var <- outcome_vars[6]
}

cat(paste0('Control vars: ', paste(control_vars, collapse=','), '\n',
           'Indep var: ', indep_var, '\n',
           'Dep var: ', dep_var))

df_comp <- df[, c(dep_var, indep_var, control_vars)]
df_comp <- df_comp[complete.cases(df_comp),]
controls_mat <- model.matrix(as.formula(paste('~ 0 + ', paste(control_vars, collapse=" + "))), df_comp)
# controls with B-splines
#controls_mat <- model.matrix(as.formula(paste('~ 0 + ', paste0('bs(', control_vars, ', df=50, degree=1)', collapse=' + '))), df_comp)
dim(controls_mat)

#ggplot(df, aes_string(indep_var)) + geom_histogram(bins=60)
res <- binsreg(y=df_comp[[dep_var]], x=df_comp[[indep_var]],
               w=controls_mat,
               dots=c(0,0),
               #dotsgrid=0,
               #line=c(2,2),
               #polyreg=3,
               ci=c(0,0),
               #cb=c(3,3),
               nbins=30,
               level=95)

################################### WEIGHTED CLUSTEING COEF & ECONOMIC INDICATORS #####################################
indep_var <- 'weighted_clustering_coef'
dep_var <- outcome_vars[2]

if(country == 'US') {
  control_vars <- c('population_bracket', 'mean_degree_bracket', 'race_white_bracket', 'clustering_coef_bracket')
} else if(country == 'MX') {
  control_vars <- c('population_bracket', 'mean_degree_bracket', 'clustering_coef_bracket')
  # the only available outcome var for Mexico
  dep_var <- outcome_vars[6]
}

cat(paste0('Control vars: ', paste(control_vars, collapse=','), '\n',
           'Indep var: ', indep_var, '\n',
           'Dep var: ', dep_var))

df_comp <- df[, c(dep_var, indep_var, control_vars)]
df_comp <- df_comp[complete.cases(df_comp),]
controls_mat <- model.matrix(as.formula(paste('~ 0 + ', paste(control_vars, collapse=" + "))), df_comp)
# controls with B-splines
#controls_mat <- model.matrix(as.formula(paste('~ 0 + ', paste0('bs(', control_vars, ', df=50, degree=1)', collapse=' + '))), df_comp)
dim(controls_mat)

#ggplot(df, aes_string(indep_var)) + geom_histogram(bins=60)
res <- binsreg(y=df_comp[[dep_var]], x=df_comp[[indep_var]],
               w=controls_mat,
               dots=c(0,0),
               #dotsgrid=0,
               #line=c(2,2),
               #polyreg=3,
               ci=c(0,0),
               #cb=c(3,3),
               nbins=30,
               level=95)


################################### MAP OF US COUNTIES WITH FRAC LONG TIES #####################################
df <- us_county_data
df <- process_release_geounit_data(df,
                                   num_cc_brackets=20,
                                   num_le_brackets=20,
                                   num_degrees_brackets=20,
                                   num_pop_brackets=15)


fill_var <- 'fraction_long_edges'
control_vars <- c('mean_degree_bracket')#, 'population_bracket')
cat(paste0('Control vars: ', paste(control_vars, collapse=','), '\n',
           'fill var: ', fill_var))

lmfit <- lm(as.formula(paste(fill_var, "~", paste(control_vars, collapse=' + '))), df)
df$fill_var <- resid(lmfit)
# without residualizing
#df$fill_var <- df[[fill_var]]
num_brackets <- 10
cut_points <- unique(quantile(df$fill_var, seq(0,1,1.0/num_brackets), type=3))
df <- df %>% mutate(fill_var_bracket = cut(fill_var, breaks=cut_points, right=FALSE, include.lowest=TRUE, dig.lab=2, ordered_result = TRUE))
df$fill_var_bracket_decile <- as.factor(as.numeric(df$fill_var_bracket))

df$fips <- df$county
states_map <- plot_usmap("states")
counties_map <- plot_usmap("counties", data=df, values="fill_var_bracket")
counties_map_data <- counties_map[[1]]
counties_map_data <- counties_map_data[!is.na(counties_map_data$fill_var_bracket), ]

p <- ggplot() +  
  geom_polygon(data=counties_map_data, 
               aes(x=x,  y=y,  group=group,  fill = fill_var_bracket_decile), 
               color = alpha("white", 0.3),
               linewidth = 0.1) +  
  geom_polygon(data=states_map[[1]], 
               aes(x=x,  y=y,  group=group),
               color = "black",
               fill=alpha(0.01),
               linewidth = 0.9) +  
  scale_fill_brewer(palette = "PiYG", name = "Long Ties\nFraction Decile") +
  theme_nothing(legend = TRUE) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  guides(fill=guide_legend(label.position = "bottom", nrow=1,
                           title.hjust = 0.5, title.vjust = 1, 
                           label.hjust = 0.5, label.vjust = 0.5,
                           title.theme = element_text(size=14),
                           label.theme = element_text(size=12, angle = 0))) +
  theme(plot.margin=unit(c(0.1,0.1, 0, 0.1), "mm"),
        legend.position="bottom") +
  coord_equal()
p

pdf_width <- 9
pdf_height <- 7
ggsave(filename = paste0("~/research/long_range_ties/results/zipcode_paper/comments_180d/", fill_var, "_county_map_release_data.pdf"), plot = p,
       width = pdf_width, height = pdf_height, units= "in")

################################### MAP OF MX COUNTIES WITH FRAC LONG TIES #####################################
df <- mx_county_data
df <- process_release_geounit_data(df,
                                   num_cc_brackets=20,
                                   num_le_brackets=20,
                                   num_degrees_brackets=20,
                                   num_pop_brackets=15)


fill_var <- 'fraction_long_edges'
control_vars <- c('mean_degree_bracket')
cat(paste0('Control vars: ', paste(control_vars, collapse=','), '\n',
           'fill var: ', fill_var))

lmfit <- lm(as.formula(paste(fill_var, "~", paste(control_vars, collapse=' + '))), df)
df$fill_var <- resid(lmfit)
# without residualizing
#df$fill_var <- df[[fill_var]]
num_brackets <- 10
cut_points <- unique(quantile(df$fill_var, seq(0,1,1.0/num_brackets), type=3))
df <- df %>% mutate(fill_var_bracket = cut(fill_var, breaks=cut_points, right=FALSE, include.lowest=TRUE, dig.lab=2, ordered_result = TRUE))
df$fill_var_bracket_decile <- as.factor(as.numeric(df$fill_var_bracket))

data(mxstate.map)
data(mxmunicipio.map)
municipalities_map_data <- mxmunicipio.map %>% left_join(df, by=c("region"="county"))
municipalities_map_data <- municipalities_map_data[!is.na(municipalities_map_data$fill_var_bracket), ]

p <- ggplot() +  
  geom_polygon(data=municipalities_map_data, 
               aes(x=long,  y=lat,  group=group,  fill = fill_var_bracket_decile), 
               color = alpha("white", 0.3),
               linewidth = 0.1) +  
  geom_polygon(data=mxstate.map,
               aes(x=long,  y=lat,  group=group),
               color = "black",
               fill=alpha(0.01),
               linewidth = 0.9) +
  scale_fill_brewer(palette = "PiYG", name = "Long Ties\nFraction Decile") +
  theme_nothing(legend = TRUE) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  guides(fill=guide_legend(label.position = "bottom", nrow=1,
                           title.hjust = 0.5, title.vjust = 1, 
                           label.hjust = 0.5, label.vjust = 0.5,
                           title.theme = element_text(size=14),
                           label.theme = element_text(size=12, angle = 0))) +
  theme(plot.margin=unit(c(0.1,0.1, 0, 0.1), "mm"),
        legend.position="bottom") +
  coord_equal()
p

pdf_width <- 9
pdf_height <- 7
ggsave(filename = paste0("~/research/long_range_ties/results/zipcode_paper/comments_180d/", fill_var, "_municipality_map_release_data.pdf"), plot = p,
       width = pdf_width, height = pdf_height, units= "in")
