time_cuts <- c(-0.75,  -0.5, -0.25, 0)
#mer_data_source <- "lesotho/MER_Structured_Datasets_Site_IM_FY17-19_20190920_v2_1_Lesotho.txt"
mer_data_source <- "country_data/2022_q1/MER_Structured_Datasets_Site_IM_FY20-22_20220211_v1_1_Lesotho.txt"
spectrum_data_source <- "application/countries/lesotho/spectrum.xlsx"
world_pop_source <- "application/countries/lesotho/lso_ppp_2019.tif"
results_location <- "application/countries/lesotho/results/"
group_sizes <- c(50,30,15)
frm <- hiv_pos ~ (age + sitetype) +  sex + primepartner*age*sex +
  log(worldpop_50 + 1) +
  splines::bs(log_hts_tst) +
  splines::bs(time) +
  splines::bs(log_tx) +
  splines::bs(log_plhiv) +
  splines::bs(log_pop_est) +
  splines::bs(pmtct_lin_pred) +  I((modality == "Index") & (time >= 0)) : age +
  (1  | modality ) + ( log_hts_tst - 1 | modality) + (1 | cluster_1 / cluster_3) + 
  (1  | sitename / modality) 
site_re_formula <- ~(1 | sitename / modality) + (1 | cluster_1 / cluster_3)

index_ratio <- function(ratio, pediatric){
  ratio[is.na(ratio)] <- 0
  #ratio <- 1.5 * ratio
  #ratio[!pediatric & ratio < 1.5]  <- ratio[!pediatric & ratio < 1.5] * 1.25#pmin(1.5, ratio[!pediatric & ratio < 1.5] * 1.4)
  #ratio[pediatric & ratio < .4]  <- ratio[pediatric & ratio < .4] * 1.25#pmin(.4, ratio[pediatric & ratio < .4] * 1.4)
  #ratio[!pediatric & ratio < .5] <- .25
  #ratio[pediatric & ratio < .1] <- .05
  ratio[is.infinite(ratio)] <- 1.5
  ratio
}