country <- "Nigeria"
filename <- "results/nigeria_MER_Structured_Datasets_Site_IM_FY17-19_20190920_v2_1_Nigeria.txt_results.RData"
load(filename)

source("R/mer_globals.R")
source("R/mer_extract.R")
source("R/mer_fit.R")
source("R/mer_results.R")
source("R/datim_geo.R")
source("R/worldpop.R")
source("R/pmtct.R")

dash <- read_excel("nigeria/surge_dashboard/dashboard_week_33.xlsx")
names(dash) <- str_replace_all(tolower(names(dash)), " ", "_")

dash$hts_tst_tot <- dash$pitc_tst_r + dash$other_tst_r + dash$index_tst_r
table(is.na(dash$hts_tst_tot))
dash <- dash %>% filter(hts_tst_tot != 0)
dash$pitc_tst_pos_r [is.na(dash$pitc_tst_pos_r)] <- 0
dash$other_tst_pos_r [is.na(dash$other_tst_pos_r)] <- 0
dash$index_tst_pos_r [is.na(dash$index_tst_pos_r)] <- 0
dash$hts_tst_pos_tot <- dash$pitc_tst_pos_r + dash$other_tst_pos_r + dash$index_tst_pos_r

dash$facility_unituid[is.na(dash$facility_unituid)] <- dash$facility_name[is.na(dash$facility_unituid)]
most_recent_periods <- rev(sort(unique(dash$reporting_period)))[1:4]

dash_sum <- dash %>%
  filter(
    reporting_period %in% most_recent_periods
  ) %>%
  group_by(
    state,
    implementing_partner,
    facility_name
  ) %>% 
  summarise(
    facility_unituid = facility_unituid[1],
    hts_tst_tot = mean(hts_tst_tot, na.rm=TRUE),
    pitc_tst_r = mean(pitc_tst_r, na.rm=TRUE),
    other_tst_r = mean(other_tst_r, na.rm=TRUE),
    index_tst_r = mean(index_tst_r, na.rm=TRUE),
    hts_tst_pos_tot = mean(hts_tst_pos_tot, na.rm=TRUE),
    pitc_tst_pos_r = mean(pitc_tst_pos_r, na.rm=TRUE),
    other_tst_pos_r = mean(other_tst_pos_r, na.rm=TRUE),
    index_tst_pos_r = mean(index_tst_pos_r, na.rm=TRUE),
    n_report = n()
  )
    
df <- dat_analysis %>% 
  filter(time == 0) %>%
  mutate_if(is.factor, as.character) %>%
  group_by(facilityuid) %>%
  mutate(
    has_index = any(modality %in% c("Index","IndexMod")),
    has_opitc = any(modality == "OtherPITC"),
    has_other = any(!(modality %in% c("Index", "IndexMod", "OtherPITC")))
  )

mfac <- unique(df$facilityuid)
dfac <- unique(dash_sum$facility_unituid)
table(dfac %in% mfac)

id_vars <- c("sitename", "psnu_t", "sitetype", "worldpop_50","worldpop_10","pmtct_lin_pred",
                  "snuprioritization", "cluster_1", "cluster_2", "cluster_3")



df_ave <- df %>%
  group_by(
    age,
    sex,
    ageasentered,
    modality
  ) %>%
  summarise(hts_tst = sum(weight))

#df_ave_unknown <- df  %>% 
#  filter(
#    !(modality %in% c("Index", "IndexMod", "OtherPITC"))
#  ) %>%
#  select(-sitename, -facilityuid, i
#  group_by_at(vars(one_of(setdiff(names(df),c("weight","obs_id","obs_id_factor"))))) %>%
#  summarise(weight = sum(weight))


i <- 1
normed_dfs <- list()
for(i in 1:length(dfac)){
  dash_i <- dash_sum %>% filter(facility_unituid == dfac[i])
  if(!(dfac[i] %in% mfac)){
    cat("- not in MER -\n")
    loc <- locations[locations$id == dash_i$facility_unituid,]
    if(nrow(loc) == 0){
      cat("- location by state -\n")
      loc <- locations[locations$displayName == dash_i$state,]
    }
    coord <- c(loc$longitude,loc$latitude)
    if(is.na(loc$latitude)){
      cat("- location by ancestor -\n")
      anc <- rev(unlist(loc$ancestors))
      for(a in anc){
        loc <- locations[locations$id == a,]
        if(!is.na(loc$latitude)){
          coord <- c(loc$longitude,loc$latitude)
          break
        }
      }
      if(is.na(coord[1])){
        print("No coordinates found")
        next
      }
    }
    dist <- (df$longitude - coord[1])^2 + (df$latitude - coord[2])^2
    dist[!df$has_index | !df$has_opitc | !df$has_other] <- Inf
    id <- which.min(dist)
    fid <- df$facilityuid[id]
    sn <- df$sitename[id]
    df_i <- df[df$facilityuid == fid & df$sitename == sn,]
  }else{
    df_i <- df %>% filter(facilityuid == dfac[i])
  }
  df_i_default <- df_i %>% 
    mutate(modality = "_") %>%
    group_by_at(vars(one_of(setdiff(names(df_i),c("weight","obs_id","obs_id_factor"))))) %>%
    summarise(weight = sum(weight))
  df_i_default$obs_id <- -1
  df_i_default$obs_id_factor <- "out_of_sample"
  
  conditions <- list(
    df_i$modality %in% c("Index", "IndexMod") & df_i$hiv_pos,
    df_i$modality %in% c("Index", "IndexMod") & !df_i$hiv_pos,
    df_i$modality %in% c("OtherPITC") & df_i$hiv_pos,
    df_i$modality %in% c("OtherPITC") & !df_i$hiv_pos,
    !(df_i$modality %in% c("Index", "IndexMod", "OtherPITC")) & df_i$hiv_pos,
    !(df_i$modality %in% c("Index", "IndexMod", "OtherPITC")) & !df_i$hiv_pos
  )
  modalities <- c("Index","Index",
                  "OtherPITC","OtherPITC",
                  "Unknown","Unknown")
  totals <- list(
    dash_i$index_tst_pos_r,
    dash_i$index_tst_r - dash_i$index_tst_pos_r,
    dash_i$pitc_tst_pos_r,
    dash_i$pitc_tst_r - dash_i$pitc_tst_pos_r,
    dash_i$other_tst_pos_r,
    dash_i$other_tst_r - dash_i$other_tst_pos_r
  )
  df_append <- list()
  for(j in 1:length(conditions)){
    condition <- conditions[[j]]
    total <- totals[[j]]
    val <- total * df_i$weight[condition] / sum(df_i$weight[condition], na.rm=TRUE)
    if(length(val) > 0){
      df_i$weight[condition] <- val
    }else if(total > 0){
      if(j %in% c(1,3,5) && sum(df_i$weight[conditions[[j+1]]], na.rm=TRUE) > 0){
        df_i_add <- df_i[conditions[[j+1]],]
        df_i_add$hiv_pos <- TRUE
        val <- total * df_i_add$weight / sum(df_i_add$weight, na.rm=TRUE) 
        df_i_add$weight <- val
      }else{
        if(modalities[[j]] == "Unknown"){
          cat("- unknown -\n")
          dfl <- list()
          tt <- table(df$modality[!(df$modality %in% c("Index","IndexMod","OtherPITC"))])
          for(k in 1:length(tt)){
            dfl[[k]] <- df_i_default
            dfl[[k]]$modality <- names(tt)[k]
            dfl[[k]]$weight <- dfl[[k]]$weight * tt[k]
            dfl[[k]]$obs_id <- -1
            dfl[[k]]$obs_id_factor <- "out_of_sample"
          }
          df_i_add <- bind_rows(dfl)
          
        }else{
          df_i_add <- df_i_default
          df_i_add$modality <- modalities[[j]]
        }
        val <- total* df_i_add$weight / sum(df_i_add$weight, na.rm=TRUE) 
        df_i_add$weight <- val
      }
      df_append[[length(df_append) + 1]] <- df_i_add
    }  
  }
  df_i <- bind_rows(df_i, df_append)
  normed_dfs[[i]] <- df_i
}

dash_df <- normed_dfs %>% bind_rows()
dash_df$weight <- 13 * dash_df$weight

dash_df_tots <- dash_df %>%
  group_by(facilityuid, psnuuid, ageasentered, sex, primepartner, modality, age) %>%
  summarise(hts_tst = sum(weight))
dash_df <- dash_df %>%
  select(-hts_tst) %>%
  merge(dash_df_tots, all.x=TRUE) %>%
  filter(weight > 0)

for(nm in names(dash_df)){
  if(is.factor(dat_analysis[[nm]])){
    dash_df[[nm]] <- factor(dash_df[[nm]], levels = levels(dat_analysis[[nm]]))
    if(any(is.na(dash_df[[nm]])))
      stop()
  }
}
total_tests_target <- 1.3 * sum(dash_df$weight)
max_diff <- 1.4

augmented_allocations <-  generate_allocations(dash_df, predict_full_fit, trans_hts_tst, 
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


fname <- paste("nigeria/augmented/allocations.csv")
write.csv(augmented_allocations$allocations, file = fname, row.names = FALSE)

fname2 <- paste("nigeria/augmented/index_allocations.csv")
write.csv(augmented_allocations$index_allocations, file = fname2, row.names = FALSE)



