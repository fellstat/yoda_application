library(tidyverse)
source("yoda_code/mer_extract.R")
source("yoda_code/mer_fit.R")
source("yoda_code/mer_results.R")
source("yoda_code/datim_geo.R")
source("yoda_code/worldpop.R")
source("yoda_code/pmtct.R")
run_id <- commandArgs(trailingOnly=TRUE)[1]
out_dir <- paste0("runs/", run_id,"/")
cat("Running...", file = paste0(out_dir,"status.txt"))
load(paste0("runs/",run_id,"/env.RData"))
fit_location <- paste0("application/countries/",country,"/results/")

filename <- paste0(fit_location, "results",".RData")
load(filename)
getwd()# 
load(paste0("runs/",run_id,"/env.RData"))
print(index_ratio)
results_location <- paste0(out_dir,"output/")
dir.create(results_location)

model_allocations <-  generate_allocations(dat_analysis %>% filter(time >= allocation_lookback), 
                                           predict_full_fit, trans_hts_tst, 
                                           glmm_index_fit=glmm_index_fit,
                                           max_diff=max_diff, 
                                           max_increase=max_increase,
                                           n_steps=n_steps,
                                           total_tests_target = total_tests_target,
                                           subgroup_fixed = data.frame(
                                             "modality",
                                             "PMTCT ANC",
                                             stringsAsFactors = FALSE),
                                           index_ratio_func = index_ratio
)
fname <- paste0(results_location, "allocations.csv")
write.csv(model_allocations$allocations, file = fname, row.names = FALSE)
fname2 <- paste0(results_location, "index_allocations.csv")
write.csv(model_allocations$index_allocations, file = fname2, row.names = FALSE)

tmp <- model_allocations$allocations %>%
  mutate(pediatric = ageasentered %in% c("<05","<10","01-04","01-09","05-09","10-14","01-10","<01","<15")) %>%
  group_by(psnu_t, ageasentered, pediatric, sex, modality) %>%
  summarise(
    proposed_hts_tst=sum(proposed_hts_tst),
    expected_hts_tst_pos = sum(expected_new_hiv_cases_at_proposed),
  ) %>%
  ungroup() %>%
  pivot_wider(
    names_from = modality,
    values_from = c(proposed_hts_tst, expected_hts_tst_pos),
    values_fill = list(proposed_hts_tst=0,expected_hts_tst_pos=0)
  )
tmpin <- model_allocations$index_allocations %>%
  group_by(psnu_t, pediatric) %>%
  summarise(
    proposed_hts_tst_IndexIndexMod=sum(proposed_hts_tst_index),
    expected_hts_tst_pos_IndexIndexMod = sum(expected_new_hiv_cases_at_proposed),
  )
ind_tst_dist <- dat_analysis %>%
  mutate(pediatric = age == "<15") %>%
  filter(time == 0, modality %in% c("Index","IndexMod")) %>%
  group_by(psnu_t, ageasentered, pediatric, sex) %>%
  summarise(nsub=sum(weight) + .0001,
            nsub_pos=sum(weight*hiv_pos) + .0001) %>%
  group_by(psnu_t, pediatric) %>%
  mutate(nsub = nsub / sum(nsub),
         nsub_pos = nsub_pos / sum(nsub_pos))
ind_tst_dist <- ind_tst_dist %>%
  merge(tmpin, all.x=TRUE) %>%
  mutate(
    proposed_hts_tst_IndexIndexMod = proposed_hts_tst_IndexIndexMod * nsub,
    expected_hts_tst_pos_IndexIndexMod = expected_hts_tst_pos_IndexIndexMod * nsub_pos
  )
ind_tst_dist$proposed_hts_tst_IndexIndexMod[is.na(ind_tst_dist$proposed_hts_tst_IndexIndexMod)] <- 0
ind_tst_dist$expected_hts_tst_pos_IndexIndexMod[is.na(ind_tst_dist$expected_hts_tst_pos_IndexIndexMod)] <- 0
allocations_by_psnu <- ind_tst_dist %>% select(-nsub,-nsub_pos) %>% merge(tmp,all=TRUE)
for(i in 5:ncol(allocations_by_psnu)){
  allocations_by_psnu[[i]][is.na(allocations_by_psnu[[i]])] <- 0
}

fname3 <- paste0(results_location, "allocations_by_psnu_modality_sex_age.csv")
write.csv(allocations_by_psnu, file = fname3, row.names = FALSE)

# requires Ckmeans.1d.dp and plotly
rmarkdown::render('mer_report.Rmd')
file.copy("mer_report.html",paste0(results_location,"mer_report",".html"), overwrite = TRUE)

print(paste0(out_dir,"output.zip"))
print(results_location)
zip(paste0(out_dir,"output.zip"), results_location)

cat("Done", file = paste0(out_dir,"status.txt"))

print("done")
