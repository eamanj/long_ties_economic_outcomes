library(dplyr)
library(stringr)
library(ggplot2)
library(reshape2)
library(binsreg)
library(ggmap)
library(usmap)
library(tidyverse)
library(viridis)

source("~/research/long_ties_economic_outcomes/analysis/utils.R")
setwd("~/research/long_ties_economic_outcomes")


################################### MAP OF COUNTIES WITH FRAC LONG TIES #####################################

###
# uncomment the lines below if working with original counties data rather than the minimal map data
#df <- read.csv(file='./data/us_counties.csv')
#df <- df %>% mutate(mean_degree_bracket=as.factor(ntile(mean_degree, 30)))
#
#lmfit <- lm(fraction_long_edges ~ mean_degree_bracket, df)
#df$fill_var <- resid(lmfit)
#num_brackets <- 10
#cut_points <- unique(quantile(df$fill_var, seq(0,1,1.0/num_brackets), type=3))
#df <- df %>% mutate(fill_var_bracket = cut(fill_var, breaks=cut_points, right=FALSE, include.lowest=TRUE, dig.lab=2, ordered_result = TRUE))
#df$fill_var_bracket_decile <- as.factor(as.numeric(df$fill_var_bracket))
#df$fips <- df$county_fips
#df <- df %>% select(-c('mean_degree', 'fraction_long_edges', 'fill_var'))
#write.csv(df, file="./data/us_counties_map_data.csv", row.names=FALSE)
# uncomment the lines above if working with original counties data rather than the minimal map data
###

df <- read.csv(file='./data/us_counties_map_data.csv')
df <- df %>% mutate(across(c(mean_degree_bracket, fill_var_bracket_decile), as.factor))

states_map <- plot_usmap("states")
counties_map <- plot_usmap("counties", data=df, values="fill_var_bracket")
counties_map_data <- counties_map[[1]]
counties_map_data <- counties_map_data[!is.na(counties_map_data$fill_var_bracket), ]

legend_text_size <- 16
legend_title_size <- 17
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
  #scale_fill_brewer(palette = "RdBu", name = "Long Ties Fraction") +
  scale_fill_brewer(palette = "PiYG", name = "Long Ties\nFraction Decile") +
  #scale_fill_viridis(discrete = TRUE, direction=-1, option="turbo", name="Long Ties Fraction") +
  #scale_fill_viridis(direction=-1, option="turbo", name="Long Ties Fraction") +
  theme_nothing(legend = TRUE) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  guides(fill=guide_legend(label.position = "bottom", nrow=1,
                           title.hjust = 0.5, title.vjust = 1, 
                           label.hjust = 0.5, label.vjust = 0.5,
                           title.theme = element_text(size=legend_title_size),
                           label.theme = element_text(size=legend_text_size, angle = 0))) +
  theme(plot.margin=unit(c(0.1,0.1, 0, 0.1), "mm"),
        #legend.position="none") +
        legend.position="bottom") +
  coord_equal()
p


################################### FRACTION/WEIGHTED LONG TIES & AGGRAGATE ECONOMIC INDICATORS #####################################

point_size <- 3.5
line_size <- 3.5
smooth_size <- 1
errorbar_size <- 1
axis_text_size <- 19
axis_title_size <- 23
legend_text_size <- 19
legend_title_size <- 23

##### fraction of long ties vs outcomes
## store results for all dep vars in this list to be saved
dep_var_results <- list()

dep_var <- 'log_median_income'
#dep_var <- 'kir_top20_pooled_pooled_p25'
#dep_var <- 'kir_pooled_pooled_p25'
#dep_var <- 'unemployment_rate'
#dep_var <- 'income_percentage_below_25K'
indep_var <- 'fraction_long_edges'
control_vars <- c('population_bracket', 'num_nodes_in_bracket_mean_degree_bracket', 'race_white_bracket')

###
## uncomment the lines below if working with original zipcode data rather than the minimal binreg data
#df <- read.csv(file='./data/zipcode_data.csv')
#num_pop_brackets <- 5
#num_le_brackets <- 8
#
#df$log_median_income <- log(df$income_median_household)
#df <- df %>% mutate(fraction_long_edges_bracket=as.factor(ntile(fraction_long_edges, num_le_brackets)),
#                    population_bracket=as.factor(ntile(population, num_pop_brackets)),
#                    race_white_bracket=as.factor(ntile(race_white, num_pop_brackets)))
#
#df_comp <- df[, c(dep_var, indep_var, control_vars, "county")]
#df_comp <- df_comp[complete.cases(df_comp),]
#controls_mat <- model.matrix(as.formula(paste('~ 0 + ', paste(control_vars, collapse=" + "))), df_comp)
## controls with B-splines
##controls_mat <- model.matrix(as.formula(paste('~ 0 + ', paste0('bs(', control_vars, ', df=50, degree=1)', collapse=' + '))), df_comp)
#dim(controls_mat)
#
## Cluster robust standard errors at the county level
#cluster_var <- df_comp$county
#num_bins <- 30
#res <- binsreg(y=df_comp[[dep_var]], x=df_comp[[indep_var]],
#               w=controls_mat,
#               dots=c(0,0),
#               cluster=cluster_var,
#               #dotsgrid=0,
#               #line=c(2,2),
#               #polyreg=3,
#               ci=c(0,0),
#               #cb=c(3,3),
#               nbins=num_bins,
#               level=95)
#dep_var_results[[dep_var]] <- res
#save(dep_var_results, file=paste0("./data/zipcode_", indep_var, "_binreg_results.RData"))
## uncomment the lines above if working with original zipcode data rather than the minimal binreg data
load(file=paste0("./data/zipcode_", indep_var, "_binreg_results.RData"))
res <- dep_var_results[[dep_var]]

max(res$data.plot[[1]]$data.dots$fit) - min(res$data.plot[[1]]$data.dots$fit)
mean(res$data.plot[[1]]$data.ci$ci.r - res$data.plot[[1]]$data.ci$ci.l)
min_x <- min(res$data.plot[[1]]$data.dots$x)
max_x <- max(res$data.plot[[1]]$data.dots$x)
min_y <- min(res$data.plot[[1]]$data.ci$ci.l)
max_y <- max(res$data.plot[[1]]$data.ci$ci.r)
paste('Range of y estimates is: ', round(exp(min(res$data.plot[[1]]$data.dots$fit)), 3), '-', round(exp(max(res$data.plot[[1]]$data.dots$fit)), 3))

plt_data <- res$data.plot[[1]]
color <- 'navy'
p <- ggplot() +
  geom_point(data=plt_data$data.dots, aes(x=x, y=fit), size=point_size, color=color) +
  geom_errorbar(data=plt_data$data.ci, aes(x=x, ymin=ci.l, ymax=ci.r), alpha=1, size=errorbar_size, color=color) +
  #geom_line(data=plt_data$data.line, aes(x=x, y=fit), size=line_size) +
  #geom_ribbon(data=plt_data$data.cb, aes(x=x, ymin=cb.l, ymax=cb.r), alpha=0.2) +
  geom_smooth(data=plt_data$data.dots, aes(x=x, y=fit),
              method='loess', span=0.5, se=FALSE, size=smooth_size, color=color) +
  #geom_smooth(data=plt_data$data.dots, aes(x=x, y=fit),
  #            method='loess', span=0.9, se=FALSE, size=smooth_size) +
  labs(x=agg_var_titles[[indep_var]],
       y=outcome_titles[[dep_var]]) +
  #coord_cartesian(x=c(min_x, 1.02*max_x), y=c(min_y,max_y)) +
  coord_cartesian(x=c(min_x, max_x), y=c(min_y,max_y)) +
  theme_bw() +
  theme(axis.text=element_text(colour="black", size=axis_text_size),
        axis.title=element_text(colour="black", size=axis_title_size),
        legend.title=element_text(size=legend_title_size),
        legend.text=element_text(size=legend_text_size))
p
if(dep_var == 'log_median_income') {
  #y_breaks <- c(75000, 100000, 125000, 150000, 175000, 200000, 225000)
  y_breaks <- c(30000, 40000, 55000, 70000, 90000)
  p <- p + scale_y_continuous(breaks=log(y_breaks), labels=thousands(y_breaks))
}
if(dep_var == 'kir_top20_pooled_pooled_p25') {
  y_breaks <- c(0.1, 0.12, 0.14, 0.16, 0.18, 0.2)
  p <- p + scale_y_continuous(breaks=y_breaks, labels=y_breaks)
}
if(dep_var == 'kir_pooled_pooled_p25') {
  y_breaks <- c(0.42, 0.44, 0.46, 0.48)
  p <- p + scale_y_continuous(breaks=y_breaks, labels=y_breaks)
}
p




##### weighted long ties vs outcomes
# store results for all dep vars in this list to be saved
dep_var_results <- list()

dep_var <- 'log_median_income'
#dep_var <- 'kir_top20_pooled_pooled_p25'
#dep_var <- 'kir_pooled_pooled_p25'
#dep_var <- 'unemployment_rate'
#dep_var <- 'income_percentage_below_25K'
indep_var <- 'fraction_weighted_long_edges'
control_vars <- c('population_bracket', 'num_nodes_in_bracket_mean_degree_bracket', 'race_white_bracket',
                  'fraction_long_edges_bracket')

###
## uncomment the lines below if working with original zipcode data rather than the minimal binreg data
#
#df_comp <- df[, c(dep_var, indep_var, control_vars, "county")]
#df_comp <- df_comp[complete.cases(df_comp),]
#controls_mat <- model.matrix(as.formula(paste('~ 0 + ', paste(control_vars, collapse=" + "))), df_comp)
## controls with B-splines
##controls_mat <- model.matrix(as.formula(paste('~ 0 + ', paste0('bs(', control_vars, ', df=50, degree=1)', collapse=' + '))), df_comp)
#dim(controls_mat)
#
## Cluster robust standard errors at the county level
#cluster_var <- df_comp$county
#num_bins <- 30
#res <- binsreg(y=df_comp[[dep_var]], x=df_comp[[indep_var]],
#               w=controls_mat,
#               dots=c(0,0),
#               cluster=cluster_var,
#               #dotsgrid=0,
#               #line=c(2,2),
#               #polyreg=3,
#               ci=c(0,0),
#               #cb=c(3,3),
#               nbins=num_bins,
#               level=95)
#dep_var_results[[dep_var]] <- res
#save(dep_var_results, file=paste0("./data/zipcode_", indep_var, "_binreg_results.RData"))
## uncomment the lines above if working with original zipcode data rather than the minimal binreg data
load(file=paste0("./data/zipcode_", indep_var, "_binreg_results.RData"))
res <- dep_var_results[[dep_var]]


max(res$data.plot[[1]]$data.dots$fit) - min(res$data.plot[[1]]$data.dots$fit)
mean(res$data.plot[[1]]$data.ci$ci.r - res$data.plot[[1]]$data.ci$ci.l)
min_x <- min(res$data.plot[[1]]$data.dots$x)
max_x <- max(res$data.plot[[1]]$data.dots$x)
min_y <- min(res$data.plot[[1]]$data.ci$ci.l)
max_y <- max(res$data.plot[[1]]$data.ci$ci.r)
paste('Range of y estimates is: ', round(exp(min(res$data.plot[[1]]$data.dots$fit)), 3), '-', round(exp(max(res$data.plot[[1]]$data.dots$fit)), 3))

plt_data <- res$data.plot[[1]]
color <- 'navy'
p <- ggplot() +
  geom_point(data=plt_data$data.dots, aes(x=x, y=fit), size=point_size, color=color) +
  geom_errorbar(data=plt_data$data.ci, aes(x=x, ymin=ci.l, ymax=ci.r), alpha=1, size=errorbar_size, color=color) +
  #geom_line(data=plt_data$data.line, aes(x=x, y=fit), size=line_size) +
  #geom_ribbon(data=plt_data$data.cb, aes(x=x, ymin=cb.l, ymax=cb.r), alpha=0.2) +
  geom_smooth(data=plt_data$data.dots, aes(x=x, y=fit),
              method='loess', span=0.5, se=FALSE, size=smooth_size, color=color) +
  #geom_smooth(data=plt_data$data.dots, aes(x=x, y=fit),
  #            method='loess', span=0.9, se=FALSE, size=smooth_size) +
  labs(x=agg_var_titles[[indep_var]],
       y=outcome_titles[[dep_var]]) +
  coord_cartesian(x=c(min_x, max_x), y=c(min_y,max_y)) +
  theme_bw() +
  theme(axis.text=element_text(colour="black", size=axis_text_size),
        axis.title=element_text(colour="black", size=axis_title_size),
        legend.title=element_text(size=legend_title_size),
        legend.text=element_text(size=legend_text_size))
p
if(dep_var == 'log_median_income') {
  y_breaks <- c(40000, 50000, 65000, 80000, 100000)
  p <- p + scale_y_continuous(breaks=log(y_breaks), labels=thousands(y_breaks))
}
if(dep_var == 'kir_top20_pooled_pooled_p25') {
  y_breaks <- c(0.10, 0.13, 0.16, 0.19, 0.22, 0.25)
  p <- p + scale_y_continuous(breaks=y_breaks, labels=y_breaks)
}
if(dep_var == 'kir_pooled_pooled_p25') {
  y_breaks <- c(0.42, 0.44, 0.46, 0.48, 0.5)
  p <- p + scale_y_continuous(breaks=y_breaks, labels=y_breaks)
}
p


################################### FRACTION/WEIGHTED LONG TIES & INDIVIDUAL ECONOMIC INDICATORS #####################################

#####################
# State-Level matching results
df <- read.csv(file='./data/individual_outcome_matching_fle_wfle.csv')

point_size <- 2
legend_point_size <- 0.7
line_size <- 1
smooth_size <- 1
errorbar_size <- 1
axis_text_size <- 19
axis_title_size <- 23
legend_text_size <- 22
legend_title_size <- 25

########
# Fraction Long Edges vs Outcomes using matching for selected states
indep_var <- 'fraction_long_edges_bracket_mean'
dep_var <- 'zip_log_income_median_family'

p1 <- ggplot(df[df$indep_var == indep_var & df$dep_var == dep_var,], aes(x,y, color=state)) +
  geom_pointrange(aes(ymin=y-2*se, ymax=y+2*se, color=state), fatten = point_size, size = line_size) +
  geom_line(size=line_size) +
  labs(x=indiv_var_titles[[indep_var]],
       y=paste(outcome_titles[[dep_var]], "Diff"),
       color='State') +
  theme_bw() +
  theme(axis.text=element_text(colour="black", size=axis_text_size),
        axis.title=element_text(colour="black", size=axis_title_size),
        plot.title = element_text(hjust = 0.5),
        #legend.position="none",
        legend.justification = c(1, 0),
        legend.position = c(0.98, 0.02),
        plot.margin = margin(5.5,20,5.5,20, "pt"),
        legend.title=element_text(size=legend_title_size),
        legend.text=element_text(size=legend_text_size)) +
  guides(color = guide_legend(override.aes = list(size=legend_point_size)))
p1

dep_var <- 'num_devices'
p2 <- ggplot(df[df$indep_var == indep_var & df$dep_var == dep_var,], aes(x,y, color=state)) +
  geom_pointrange(aes(ymin=y-2*se, ymax=y+2*se, color=state), fatten = point_size, size = line_size) +
  geom_line(size=line_size) +
  labs(x=indiv_var_titles[[indep_var]],
       y=paste(outcome_titles[[dep_var]], "Diff"),
       color='State') +
  theme_bw() +
  theme(axis.text=element_text(colour="black", size=axis_text_size),
        axis.title=element_text(colour="black", size=axis_title_size),
        plot.title = element_text(hjust = 0.5),
        #legend.position="none",
        legend.justification = c(1, 0),
        legend.position = c(0.98, 0.02),
        plot.margin = margin(5.5,20,5.5,20, "pt"),
        legend.title=element_text(size=legend_title_size),
        legend.text=element_text(size=legend_text_size)) +
  guides(color = guide_legend(override.aes = list(size=legend_point_size)))
p2

dep_var <- 'num_donations'
p3 <- ggplot(df[df$indep_var == indep_var & df$dep_var == dep_var,], aes(x,y, color=state)) +
  geom_pointrange(aes(ymin=y-2*se, ymax=y+2*se, color=state), fatten = point_size, size = line_size) +
  geom_line(size=line_size) +
  labs(x=indiv_var_titles[[indep_var]],
       y=paste(outcome_titles[[dep_var]], "Diff"),
       color='State') +
  theme_bw() +
  theme(axis.text=element_text(colour="black", size=axis_text_size),
        axis.title=element_text(colour="black", size=axis_title_size),
        plot.title = element_text(hjust = 0.5),
        #legend.position="none",
        legend.justification = c(1, 0),
        legend.position = c(0.98, 0.02),
        plot.margin = margin(5.5,20,5.5,20, "pt"),
        legend.title=element_text(size=legend_title_size),
        legend.text=element_text(size=legend_text_size)) +
  guides(color = guide_legend(override.aes = list(size=legend_point_size)))
p3


dep_var <- 'zip_log_income_mean_family'
p4 <- ggplot(df[df$indep_var == indep_var & df$dep_var == dep_var,], aes(x,y, color=state)) +
  geom_pointrange(aes(ymin=y-2*se, ymax=y+2*se, color=state), fatten = point_size, size = line_size) +
  geom_line(size=line_size) +
  labs(x=indiv_var_titles[[indep_var]],
       y=paste(outcome_titles[[dep_var]], "Diff"),
       color='State') +
  theme_bw() +
  theme(axis.text=element_text(colour="black", size=axis_text_size),
        axis.title=element_text(colour="black", size=axis_title_size),
        plot.title = element_text(hjust = 0.5),
        #legend.position="none",
        legend.justification = c(1, 0),
        legend.position = c(0.98, 0.02),
        plot.margin = margin(5.5,20,5.5,20, "pt"),
        legend.title=element_text(size=legend_title_size),
        legend.text=element_text(size=legend_text_size)) +
  guides(color = guide_legend(override.aes = list(size=legend_point_size)))
p4

dep_var <- 'num_countries_visited'
p5 <- ggplot(df[df$indep_var == indep_var & df$dep_var == dep_var,], aes(x,y, color=state)) +
  geom_pointrange(aes(ymin=y-2*se, ymax=y+2*se, color=state), fatten = point_size, size = line_size) +
  geom_line(size=line_size) +
  labs(x=indiv_var_titles[[indep_var]],
       y=paste(outcome_titles[[dep_var]], "Diff"),
       color='State') +
  theme_bw() +
  theme(axis.text=element_text(colour="black", size=axis_text_size),
        axis.title=element_text(colour="black", size=axis_title_size),
        plot.title = element_text(hjust = 0.5),
        #legend.position="none",
        legend.justification = c(1, 0),
        legend.position = c(0.98, 0.02),
        plot.margin = margin(5.5,20,5.5,20, "pt"),
        legend.title=element_text(size=legend_title_size),
        legend.text=element_text(size=legend_text_size)) +
  guides(color = guide_legend(override.aes = list(size=legend_point_size)))
p5

dep_var <- 'phone_price'
p6 <- ggplot(df[df$indep_var == indep_var & df$dep_var == dep_var,], aes(x,y, color=state)) +
  geom_pointrange(aes(ymin=y-2*se, ymax=y+2*se, color=state), fatten = point_size, size = line_size) +
  geom_line(size=line_size) +
  labs(x=indiv_var_titles[[indep_var]],
       y=paste(outcome_titles[[dep_var]], "Diff"),
       color='State') +
  #coord_cartesian(y=c(-0.3,0.3)) +
  #ggtitle(strength_var) +
  theme_bw() +
  theme(axis.text=element_text(colour="black", size=axis_text_size),
        axis.title=element_text(colour="black", size=axis_title_size),
        plot.title = element_text(hjust = 0.5),
        #legend.position="none",
        legend.justification = c(1, 0),
        legend.position = c(0.98, 0.02),
        plot.margin = margin(5.5,20,5.5,20, "pt"),
        legend.title=element_text(size=legend_title_size),
        legend.text=element_text(size=legend_text_size)) +
  guides(color = guide_legend(override.aes = list(size=legend_point_size)))
p6
                  
########
# Weighted Long Edges vs Outcomes using matching for selected states
indep_var <- 'weighted_long_edges_bracket_mean'
dep_var <- 'zip_log_income_median_family'

p1 <- ggplot(df[df$indep_var == indep_var & df$dep_var == dep_var,], aes(x,y, color=state)) +
  geom_pointrange(aes(ymin=y-2*se, ymax=y+2*se, color=state), fatten = point_size, size = line_size) +
  geom_line(size=line_size) +
  labs(x=indiv_var_titles[[indep_var]],
       y=paste(outcome_titles[[dep_var]], "Diff"),
       color='State') +
  theme_bw() +
  theme(axis.text=element_text(colour="black", size=axis_text_size),
        axis.title=element_text(colour="black", size=axis_title_size),
        plot.title = element_text(hjust = 0.5),
        #legend.position="none",
        legend.justification = c(1, 0),
        legend.position = c(0.98, 0.02),
        plot.margin = margin(5.5,20,5.5,20, "pt"),
        legend.title=element_text(size=legend_title_size),
        legend.text=element_text(size=legend_text_size)) +
  guides(color = guide_legend(override.aes = list(size=legend_point_size)))
p1

dep_var <- 'num_devices'
p2 <- ggplot(df[df$indep_var == indep_var & df$dep_var == dep_var,], aes(x,y, color=state)) +
  geom_pointrange(aes(ymin=y-2*se, ymax=y+2*se, color=state), fatten = point_size, size = line_size) +
  geom_line(size=line_size) +
  labs(x=indiv_var_titles[[indep_var]],
       y=paste(outcome_titles[[dep_var]], "Diff"),
       color='State') +
  theme_bw() +
  theme(axis.text=element_text(colour="black", size=axis_text_size),
        axis.title=element_text(colour="black", size=axis_title_size),
        plot.title = element_text(hjust = 0.5),
        #legend.position="none",
        legend.justification = c(1, 0),
        legend.position = c(0.98, 0.02),
        plot.margin = margin(5.5,20,5.5,20, "pt"),
        legend.title=element_text(size=legend_title_size),
        legend.text=element_text(size=legend_text_size)) +
  guides(color = guide_legend(override.aes = list(size=legend_point_size)))
p2

dep_var <- 'num_donations'
p3 <- ggplot(df[df$indep_var == indep_var & df$dep_var == dep_var,], aes(x,y, color=state)) +
  geom_pointrange(aes(ymin=y-2*se, ymax=y+2*se, color=state), fatten = point_size, size = line_size) +
  geom_line(size=line_size) +
  labs(x=indiv_var_titles[[indep_var]],
       y=paste(outcome_titles[[dep_var]], "Diff"),
       color='State') +
  theme_bw() +
  theme(axis.text=element_text(colour="black", size=axis_text_size),
        axis.title=element_text(colour="black", size=axis_title_size),
        plot.title = element_text(hjust = 0.5),
        #legend.position="none",
        legend.justification = c(1, 0),
        legend.position = c(0.98, 0.02),
        plot.margin = margin(5.5,20,5.5,20, "pt"),
        legend.title=element_text(size=legend_title_size),
        legend.text=element_text(size=legend_text_size)) +
  guides(color = guide_legend(override.aes = list(size=legend_point_size)))
p3


dep_var <- 'zip_log_income_mean_family'
p4 <- ggplot(df[df$indep_var == indep_var & df$dep_var == dep_var,], aes(x,y, color=state)) +
  geom_pointrange(aes(ymin=y-2*se, ymax=y+2*se, color=state), fatten = point_size, size = line_size) +
  geom_line(size=line_size) +
  labs(x=indiv_var_titles[[indep_var]],
       y=paste(outcome_titles[[dep_var]], "Diff"),
       color='State') +
  theme_bw() +
  theme(axis.text=element_text(colour="black", size=axis_text_size),
        axis.title=element_text(colour="black", size=axis_title_size),
        plot.title = element_text(hjust = 0.5),
        #legend.position="none",
        legend.justification = c(1, 0),
        legend.position = c(0.98, 0.02),
        plot.margin = margin(5.5,20,5.5,20, "pt"),
        legend.title=element_text(size=legend_title_size),
        legend.text=element_text(size=legend_text_size)) +
  guides(color = guide_legend(override.aes = list(size=legend_point_size)))
p4

dep_var <- 'num_countries_visited'
p5 <- ggplot(df[df$indep_var == indep_var & df$dep_var == dep_var,], aes(x,y, color=state)) +
  geom_pointrange(aes(ymin=y-2*se, ymax=y+2*se, color=state), fatten = point_size, size = line_size) +
  geom_line(size=line_size) +
  labs(x=indiv_var_titles[[indep_var]],
       y=paste(outcome_titles[[dep_var]], "Diff"),
       color='State') +
  theme_bw() +
  theme(axis.text=element_text(colour="black", size=axis_text_size),
        axis.title=element_text(colour="black", size=axis_title_size),
        plot.title = element_text(hjust = 0.5),
        #legend.position="none",
        legend.justification = c(1, 0),
        legend.position = c(0.98, 0.02),
        plot.margin = margin(5.5,20,5.5,20, "pt"),
        legend.title=element_text(size=legend_title_size),
        legend.text=element_text(size=legend_text_size)) +
  guides(color = guide_legend(override.aes = list(size=legend_point_size)))
p5

dep_var <- 'phone_price'
p6 <- ggplot(df[df$indep_var == indep_var & df$dep_var == dep_var,], aes(x,y, color=state)) +
  geom_pointrange(aes(ymin=y-2*se, ymax=y+2*se, color=state), fatten = point_size, size = line_size) +
  geom_line(size=line_size) +
  labs(x=indiv_var_titles[[indep_var]],
       y=paste(outcome_titles[[dep_var]], "Diff"),
       color='State') +
  #coord_cartesian(y=c(-0.3,0.3)) +
  #ggtitle(strength_var) +
  theme_bw() +
  theme(axis.text=element_text(colour="black", size=axis_text_size),
        axis.title=element_text(colour="black", size=axis_title_size),
        plot.title = element_text(hjust = 0.5),
        #legend.position="none",
        legend.justification = c(1, 0),
        legend.position = c(0.98, 0.02),
        plot.margin = margin(5.5,20,5.5,20, "pt"),
        legend.title=element_text(size=legend_title_size),
        legend.text=element_text(size=legend_text_size)) +
  guides(color = guide_legend(override.aes = list(size=legend_point_size)))
p6
                  

#####################
# State-Level ALE results
df <- read.csv(file='./data/individual_outcome_ale_fle_wfle.csv')
state_abbrs <- c(state.abb, 'DC', 'Overall')

point_size <- 3.4
line_size <- 2.5
smooth_size <- 1
axis_text_size <- 24
axis_title_size <- 27
legend_text_size <- 19
legend_title_size <- 23

########
# Fraction Long Edges vs Outcomes using ALE
indep_var <- 'fraction_long_edges'
dep_var <- 'zip_log_income_median_family'
dep_var <- 'num_devices'
dep_var <- 'num_donations'
dep_var <- 'zip_log_income_mean_family'
dep_var <- 'num_countries_visited'
dep_var <- 'phone_price'

plot_data <- df[df$indep_var == indep_var & df$dep_var == dep_var,]
plot_data$se <- NA
mean_me <- mean(plot_data$me)
boot_me <- boot(plot_data$me, function(x,i) mean(x[i]), 5000)
se_me <-  sd(boot_me$t)
plot_data <- plot_data %>% add_row(state="Overall", me=mean_me, se=se_me)
plot_data <- plot_data[order(plot_data$me),]
plot_data$state_abbr <- state_abbrs[match(plot_data$state, c(state.name, 'Washington, District of Columbia', 'Overall'))]
plot_data$state_abbr <- factor(plot_data$state_abbr, levels=plot_data$state_abbr)
plot_data$state <- factor(plot_data$state, levels=plot_data$state)
p <- ggplot(plot_data, aes(x=state_abbr, y=me, color = state_abbr == "Overall")) +
  scale_colour_manual(values = c("blue", "red")) + 
  geom_pointrange(aes(ymin=me-2*se, ymax=me+2*se), fatten = point_size, size = line_size) +
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=0 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  labs(y=outcome_titles[[dep_var]]) +
  theme_bw() +
  theme(axis.text=element_text(colour="black", size=axis_text_size),
        axis.title.x=element_text(colour="black", size=axis_title_size),
        axis.title.y = element_blank(),
        legend.position="none")
p

########
# Weighted Fraction Long Edges vs Outcomes using ALE
indep_var <- 'weighted_long_edges'
dep_var <- 'zip_log_income_median_family'
dep_var <- 'num_devices'
dep_var <- 'num_donations'
dep_var <- 'zip_log_income_mean_family'
dep_var <- 'num_countries_visited'
dep_var <- 'phone_price'

plot_data <- df[df$indep_var == indep_var & df$dep_var == dep_var,]
plot_data$se <- NA
mean_me <- mean(plot_data$me)
boot_me <- boot(plot_data$me, function(x,i) mean(x[i]), 5000)
se_me <-  sd(boot_me$t)
plot_data <- plot_data %>% add_row(state="Overall", me=mean_me, se=se_me)
plot_data <- plot_data[order(plot_data$me),]
plot_data$state_abbr <- state_abbrs[match(plot_data$state, c(state.name, 'Washington, District of Columbia', 'Overall'))]
plot_data$state_abbr <- factor(plot_data$state_abbr, levels=plot_data$state_abbr)
plot_data$state <- factor(plot_data$state, levels=plot_data$state)
p <- ggplot(plot_data, aes(x=state_abbr, y=me, color = state_abbr == "Overall")) +
  scale_colour_manual(values = c("blue", "red")) + 
  geom_pointrange(aes(ymin=me-2*se, ymax=me+2*se), fatten = point_size, size = line_size) +
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=0 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  labs(y=outcome_titles[[dep_var]]) +
  theme_bw() +
  theme(axis.text=element_text(colour="black", size=axis_text_size),
        axis.title.x=element_text(colour="black", size=axis_title_size),
        axis.title.y = element_blank(),
        legend.position="none")
p


########
# Both Fraction and Weighted Fraction Long Edges vs Outcomes using ALE in single plot
point_size <- 4.5
legend_point_size <- 5
line_size <- 1.7
axis_text_size <- 20
axis_title_size <- 23
legend_text_size <- 19
legend_title_size <- 23

dep_vars <- c('zip_log_income_median_family', 'num_devices',
             'num_donations')#, 'num_countries_visited', 'phone_price')

plot_data <- df[df$dep_var %in% dep_vars, ] %>%
  group_by(dep_var, indep_var) %>%
  summarise(mean_me=mean(me),
            se_me=sd(boot(me, function(x,i) mean(x[i]), 5000)$t))
plot_data[plot_data$indep_var == 'fraction_long_edges', 'indep_var'] <- 'Fraction of\nLong Ties'
plot_data[plot_data$indep_var == 'weighted_long_edges', 'indep_var'] <- 'Weighted\nLong Ties'
plot_data <- plot_data[order(plot_data$dep_var),]

p <- ggplot(plot_data, aes(x=dep_var, y=mean_me, color = indep_var)) +
  geom_point(size = point_size) +
  geom_errorbar(aes(ymin=mean_me-2*se_me, ymax=mean_me+2*se_me), width=0.2, size=line_size) +
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=0 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  labs(y="Marginal Effect") +
  scale_x_discrete(labels = setNames(bind_rows(outcome_titles[dep_vars]), dep_vars)) +
  theme_bw() +
  guides(color = guide_legend(keyheight = 4, override.aes = list(size=legend_point_size))) +
  theme(axis.text=element_text(colour="black", size=axis_text_size),
        axis.title.x=element_text(colour="black", size=axis_title_size),
        axis.title.y = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.98, 0.64),
        legend.text=element_text(size=legend_text_size),
        legend.title = element_blank())
        
p



################################### FRACTION OF LONG TIES & LIFE EVENTS #####################################
#####################
error_bar_size <- 1
point_size <- 5
smooth_size <- 1
axis_text_size <- 26
axis_title_size <- 28
legend_text_size <- 27

################### inter-state migrants
main_group_var <- 'diff_hometown_current_region'

# degree/fle over all ties
indep_deg_var1 <- "degree"
dep_var1 <- "fraction_long_edges"
df1 <- read.csv(file=paste0(file="./data/", dep_var1, "_", indep_deg_var1, "_by_", main_group_var, ".csv"))

# degree/fle over in-region ties
loc_qualifier <- 'in_region'
indep_deg_var2 <- paste0(indep_deg_var1, '_', loc_qualifier)
dep_var2 <- paste0(dep_var1, '_', loc_qualifier)
df2 <- read.csv(file=paste0(file="./data/", dep_var2, "_", indep_deg_var2, "_by_", main_group_var, ".csv"))


mean_indep_deg_var1 <- paste0(indep_deg_var1, "_bracket_mean")
mean_dep_var1 <- paste0(dep_var1, "_mean")
boot_se_dep_var1 <- paste0(dep_var1, "_boot_se")
p1 <- ggplot(df1, aes_string(mean_indep_deg_var1, mean_dep_var1, color=main_group_var)) +
  geom_point(size=point_size) +
  #geom_linerange(aes_string(ymin=paste0(mean_dep_var1, "- 1.96*", boot_se_dep_var1),
  #                          ymax=paste0(mean_dep_var1, "+ 1.96*", boot_se_dep_var1)),
  #               size=error_bar_size) +
  geom_smooth(method="loess", span = 0.3, size=smooth_size, se=FALSE) +
  scale_x_continuous(trans='log',
                     labels = scales::number_format(accuracy = 1.0)) +
  annotation_logticks() +
  labs(x=life_event_labels[[indep_deg_var1]], y=life_event_labels[[dep_var1]]) +
  theme_bw() +
  scale_color_manual(values = c("red", "blue")) +
  guides(color = guide_legend(reverse = TRUE)) +
  theme(axis.text=element_text(colour="black", size=axis_text_size),
        axis.title=element_text(colour="black", size=axis_title_size),
        legend.title = element_blank(),
        legend.position = c(0.29,0.16),
        legend.justification = c(1, 1),
        legend.text=element_text(size=legend_text_size))


mean_indep_deg_var2 <- paste0(indep_deg_var2, "_bracket_mean")
mean_dep_var2 <- paste0(dep_var2, "_mean")
boot_se_dep_var2 <- paste0(dep_var2, "_boot_se")

pt_scale_size <- 2.25
label_scale_size <- 1.6
p2 <- ggplot(df2, aes_string(mean_indep_deg_var2, mean_dep_var2, color=main_group_var)) +
  geom_point(size=point_size/2.0) +
  #geom_linerange(aes_string(ymin=paste0(mean_dep_var2, "- 1.96*", boot_se_dep_var2),
  #                          ymax=paste0(mean_dep_var2, "+ 1.96*", boot_se_dep_var2)),
  #               size=error_bar_size) +
  geom_smooth(method="loess", span = 0.3, size=smooth_size/pt_scale_size, se=FALSE) +
  scale_x_continuous(trans='log',
                     labels = scales::number_format(accuracy = 1.0)) +
  annotation_logticks() +
  ggtitle(life_event_labels[[loc_qualifier]]) +
  theme_bw() +
  scale_color_manual(values = c("red", "blue")) +
  theme(axis.text=element_text(colour="black", size=axis_text_size/label_scale_size),
        axis.title=element_blank(),
        plot.title = element_text(color="black", size=axis_title_size/label_scale_size, hjust=0.5),
        legend.position = "none")

p <- p1 + theme(legend.position = c(0.29,0.16)) +
  annotation_custom(ggplotGrob(p2), ymin=0.327, ymax=0.557, xmin=log(50), xmax=log(360))
p


################### out-of-state college
main_group_var <- 'out_of_region_college'

# degree/fle over all ties
indep_deg_var1 <- "degree"
dep_var1 <- "fraction_long_edges"
df1 <- read.csv(file=paste0(file="./data/", dep_var1, "_", indep_deg_var1, "_by_", main_group_var, ".csv"))

# degree/fle over out-college ties
loc_qualifier <- 'out_college'
indep_deg_var2 <- paste0(indep_deg_var1, '_', loc_qualifier)
dep_var2 <- paste0(dep_var1, '_', loc_qualifier)
df2 <- read.csv(file=paste0(file="./data/", dep_var2, "_", indep_deg_var2, "_by_", main_group_var, ".csv"))


mean_indep_deg_var1 <- paste0(indep_deg_var1, "_bracket_mean")
mean_dep_var1 <- paste0(dep_var1, "_mean")
boot_se_dep_var1 <- paste0(dep_var1, "_boot_se")
p1 <- ggplot(df1, aes_string(mean_indep_deg_var1, mean_dep_var1, color=main_group_var)) +
  geom_point(size=point_size) +
  #geom_linerange(aes_string(ymin=paste0(mean_dep_var1, "- 1.96*", boot_se_dep_var1),
  #                          ymax=paste0(mean_dep_var1, "+ 1.96*", boot_se_dep_var1)),
  #               size=error_bar_size) +
  geom_smooth(method="loess", span = 0.3, size=smooth_size, se=FALSE) +
  scale_x_continuous(trans='log',
                     labels = scales::number_format(accuracy = 1.0)) +
  annotation_logticks() +
  labs(x=life_event_labels[[indep_deg_var1]], y=life_event_labels[[dep_var1]]) +
  theme_bw() +
  scale_color_manual(values = c("red", "blue")) +
  guides(color = guide_legend(reverse = TRUE)) +
  theme(axis.text=element_text(colour="black", size=axis_text_size),
        axis.title=element_text(colour="black", size=axis_title_size),
        legend.title = element_blank(),
        legend.position = c(0.29,0.16),
        legend.justification = c(1, 1),
        legend.text=element_text(size=legend_text_size))


mean_indep_deg_var2 <- paste0(indep_deg_var2, "_bracket_mean")
mean_dep_var2 <- paste0(dep_var2, "_mean")
boot_se_dep_var2 <- paste0(dep_var2, "_boot_se")

pt_scale_size <- 2.25
label_scale_size <- 1.6
p2 <- ggplot(df2, aes_string(mean_indep_deg_var2, mean_dep_var2, color=main_group_var)) +
  geom_point(size=point_size/2.0) +
  #geom_linerange(aes_string(ymin=paste0(mean_dep_var2, "- 1.96*", boot_se_dep_var2),
  #                          ymax=paste0(mean_dep_var2, "+ 1.96*", boot_se_dep_var2)),
  #               size=error_bar_size) +
  geom_smooth(method="loess", span = 0.3, size=smooth_size/pt_scale_size, se=FALSE) +
  scale_x_continuous(trans='log',
                     labels = scales::number_format(accuracy = 1.0)) +
  annotation_logticks() +
  ggtitle(life_event_labels[[loc_qualifier]]) +
  theme_bw() +
  scale_color_manual(values = c("red", "blue")) +
  theme(axis.text=element_text(colour="black", size=axis_text_size/label_scale_size),
        axis.title=element_blank(),
        plot.title = element_text(color="black", size=axis_title_size/label_scale_size, hjust=0.5),
        legend.position = "none")

p <- p1 + theme(legend.position = c(0.37,0.16)) +
  annotation_custom(ggplotGrob(p2), ymin=0.341, ymax=0.562, xmin=log(48), xmax=log(359))
p


################### multiple high schools
main_group_var <- 'multiple_high_schools'

# degree/fle over all ties
indep_deg_var1 <- "degree"
dep_var1 <- "fraction_long_edges"
df1 <- read.csv(file=paste0(file="./data/", dep_var1, "_", indep_deg_var1, "_by_", main_group_var, ".csv"))

# degree/fle over out-high school ties
loc_qualifier <- 'out_high_school'
indep_deg_var2 <- paste0(indep_deg_var1, '_', loc_qualifier)
dep_var2 <- paste0(dep_var1, '_', loc_qualifier)
df2 <- read.csv(file=paste0(file="./data/", dep_var2, "_", indep_deg_var2, "_by_", main_group_var, ".csv"))


mean_indep_deg_var1 <- paste0(indep_deg_var1, "_bracket_mean")
mean_dep_var1 <- paste0(dep_var1, "_mean")
boot_se_dep_var1 <- paste0(dep_var1, "_boot_se")
p1 <- ggplot(df1, aes_string(mean_indep_deg_var1, mean_dep_var1, color=main_group_var)) +
  geom_point(size=point_size) +
  #geom_linerange(aes_string(ymin=paste0(mean_dep_var1, "- 1.96*", boot_se_dep_var1),
  #                          ymax=paste0(mean_dep_var1, "+ 1.96*", boot_se_dep_var1)),
  #               size=error_bar_size) +
  geom_smooth(method="loess", span = 0.3, size=smooth_size, se=FALSE) +
  scale_x_continuous(trans='log',
                     labels = scales::number_format(accuracy = 1.0)) +
  annotation_logticks() +
  labs(x=life_event_labels[[indep_deg_var1]], y=life_event_labels[[dep_var1]]) +
  theme_bw() +
  scale_color_manual(values = c("red", "blue")) +
  guides(color = guide_legend(reverse = TRUE)) +
  theme(axis.text=element_text(colour="black", size=axis_text_size),
        axis.title=element_text(colour="black", size=axis_title_size),
        legend.title = element_blank(),
        legend.position = c(0.29,0.16),
        legend.justification = c(1, 1),
        legend.text=element_text(size=legend_text_size))


mean_indep_deg_var2 <- paste0(indep_deg_var2, "_bracket_mean")
mean_dep_var2 <- paste0(dep_var2, "_mean")
boot_se_dep_var2 <- paste0(dep_var2, "_boot_se")

pt_scale_size <- 2.25
label_scale_size <- 1.6
p2 <- ggplot(df2, aes_string(mean_indep_deg_var2, mean_dep_var2, color=main_group_var)) +
  geom_point(size=point_size/2.0) +
  #geom_linerange(aes_string(ymin=paste0(mean_dep_var2, "- 1.96*", boot_se_dep_var2),
  #                          ymax=paste0(mean_dep_var2, "+ 1.96*", boot_se_dep_var2)),
  #               size=error_bar_size) +
  geom_smooth(method="loess", span = 0.3, size=smooth_size/pt_scale_size, se=FALSE) +
  scale_x_continuous(trans='log',
                     labels = scales::number_format(accuracy = 1.0)) +
  annotation_logticks() +
  ggtitle(life_event_labels[[loc_qualifier]]) +
  theme_bw() +
  scale_color_manual(values = c("red", "blue")) +
  theme(axis.text=element_text(colour="black", size=axis_text_size/label_scale_size),
        axis.title=element_blank(),
        plot.title = element_text(color="black", size=axis_title_size/label_scale_size, hjust=0.5),
        legend.position = "none")

p <- p1 + theme(legend.position = c(0.3,0.16)) +
  annotation_custom(ggplotGrob(p2), ymin=0.340, ymax=0.57, xmin=log(45), xmax=log(355))
p



################################### FRACTION OF LONG TIES & SCHOOL CLOSURES #####################################
data <- read.csv(file="./data/school_closure_fle.csv")
data$ncesid <- as.factor(data$ncesid)
data$age_factor <- as.factor(data$age_factor)
df <- data %>% group_by(ncesid, age_bracket, closure_status) %>%
  mutate(cnt=n()) %>%
  filter(cnt > 1) %>%
  group_by(ncesid, age_bracket) %>%
  mutate(num_conditions=n_distinct(closure_status, na.rm=T))
df <- df[df$num_conditions>1,]

# comparing before and during closure using stratification and cluster bootstrap for SE
main_group_var <- 'closure_status'
stratification_vars <- c('ncesid', 'age_bracket')
dep_var <- 'fraction_long_edges'


cluster_estimate <- function(df1, stratification_vars, main_group_var, dep_var) {
  estimate_df <- df1 %>% group_by(across(all_of(c(stratification_vars, main_group_var)))) %>%
    summarise(across(all_of(dep_var), list(mean=~mean(.x, na.rm=T), se=~sd(.x, na.rm=T)/sqrt(n())), .names="{.fn}_{.col}"), .groups='drop') %>%
    group_by(across(all_of(main_group_var))) %>%
    summarise(across(starts_with("mean_"), mean),
              across(starts_with("se"), ~ sqrt(sum(.x**2))/n()), .groups='drop')
  return(estimate_df)
}
bootstrap_replica <- function(df, stratification_vars, main_group_var, dep_var) {
  school_ids <- unique(df$ncesid)
  replica_school_ids <- sample(school_ids, length(school_ids), replace=TRUE) 
  replica_df <- rbindlist(lapply(replica_school_ids, function(x) df[df$ncesid %in% x,]))
  replica_result <- cluster_estimate(replica_df, stratification_vars, main_group_var, dep_var)
  return(as.data.frame(replica_result))
}
      
plot_df <- cluster_estimate(df, stratification_vars, main_group_var, dep_var)
replica_results <- replicate(400, bootstrap_replica(df, stratification_vars, main_group_var, dep_var), simplify = FALSE)
replica_results <- do.call(rbind, replica_results)
replica_ses <- replica_results %>% group_by(closure_status) %>%
  summarise(across(all_of(paste0("mean_",dep_var)), list(se=~sd(.x, na.rm=T)), .names="{.fn}_{.col}"))
plot_df <- plot_df %>% inner_join(replica_ses, by='closure_status')
plot_df


point_size <- 5
error_bar_size <- 1.5
axis_text_size <- 22
axis_title_size <- 24
pdf_width <- 8
pdf_height <- 7
p <- ggplot(plot_df, aes_string(x=main_group_var, y=paste0("mean_", dep_var), color=main_group_var)) +
  geom_point(size=point_size) +
  geom_errorbar(aes_string(ymin=paste0("mean_", dep_var, "- 1.96*se_", dep_var),
                           ymax=paste0("mean_", dep_var, "+ 1.96*se_", dep_var)),
                width=0.4,
                linewidth=error_bar_size) +
  scale_color_manual(values = c("red", "blue")) +
  labs(y="Fraction of Long Edges") +
  labs(x=life_event_labels[[main_group_var]], y=life_event_labels[[dep_var]]) +
  theme_bw() +
  theme(axis.text=element_text(colour="black", size=axis_text_size),
        axis.title=element_text(colour="black", size=axis_title_size),
        legend.position = "none")
p

####################################################
# comparing before and during closure using a matching style estimator at the high school
summary_df <- df %>% group_by(ncesid, closure_status) %>%
  summarise(fle = mean(fraction_long_edges),
            cnt=n())
summary_df[summary_df$closure_status=='Before Closure', 'closure_status'] <- 'before_closure'
summary_df[summary_df$closure_status=='During Closure', 'closure_status'] <- 'during_closure'
summary_df <- summary_df %>%
  pivot_wider(names_from = closure_status, values_from = c(cnt, fle))
summary_df
summary_df <- summary_df[!is.na(summary_df$fle_before_closure) & !is.na(summary_df$fle_during_closure),]
plot_df <- summary_df
plot_df$fle_during_before_diff <- plot_df$fle_during_closure - plot_df$fle_before_closure
plot_df <- plot_df %>% group_by() %>%
  summarise(mean_fle_during_before_diff=mean(fle_during_before_diff),
            se_fle_during_before_diff=sd(fle_during_before_diff)/sqrt(n()))
plot_df

p <- ggplot(plot_df, aes(x="", y=mean_fle_during_before_diff)) +
  geom_point(size=5) +
  geom_errorbar(aes(ymin=mean_fle_during_before_diff - 1.96*se_fle_during_before_diff,
                    ymax=mean_fle_during_before_diff + 1.96*se_fle_during_before_diff),
                width=0.4,
                linewidth=error_bar_size) +
  scale_color_manual(values = c("red", "blue")) +
  labs(y="Fraction of Long Edges Diff") +
  theme_bw() +
  theme(axis.text=element_text(colour="black", size=axis_text_size),
        axis.title=element_text(colour="black", size=axis_title_size),
        legend.position = "none")
p


####################################################
# comparing before and during closure using linear fixed effect model with cluster robust SE at school level
lmfit1 <- lm(fraction_long_edges ~ closure_status, data)
cluster_cov1 <- vcovCL(lmfit1, cluster = data$ncesid)
cluster_se1 <- sqrt(diag(cluster_cov1))
cluster_lmfit1 <- coeftest(lmfit1, vcov=cluster_cov1)

lmfit2<- lm(fraction_long_edges ~ closure_status + ncesid, data)
cluster_cov2 <- vcovCL(lmfit2, cluster = data$ncesid)
cluster_se2 <- sqrt(diag(cluster_cov2))
cluster_lmfit2 <- coeftest(lmfit2, vcov=cluster_cov2)

lmfit3<- lm(fraction_long_edges ~ closure_status + age_factor + ncesid, data)
cluster_cov3 <- vcovCL(lmfit3, cluster = data$ncesid)
cluster_se3 <- sqrt(diag(cluster_cov3))
cluster_lmfit3 <- coeftest(lmfit3, vcov=cluster_cov3)


cluster_lmfit1[1:2,]
cluster_lmfit2[1:2,]
cluster_lmfit3[1:2,]

stargazer(lmfit1, lmfit2, lmfit3,
          se=list(cluster_se1, cluster_se2, cluster_se3),
          type="html",
          out="~/Desktop/result.html",
          omit = c("*ncesid*", "*age*"),
          covariate.labels = c("During School Closure"),
          #dep.var.labels.include = F,
          dep.var.caption = "Fraction of Long Edges",
          add.lines = list(
            c("School Fixed Effects", "No", "Yes", "Yes"),
            c("Age Fixed Effects", "No", "No", "Yes")))