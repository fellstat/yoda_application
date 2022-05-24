source("secret.R")

library(aws.s3)

write_bucket <- Sys.getenv("TEST_BUCKET_WRITE")
country_dirs <- list.dirs("application/countries", recursive = FALSE)
country_names <- list.dirs("application/countries", recursive = FALSE, full.names = FALSE)


if(dir.exists("tmp__"))
  unlink("tmp__", recursive = TRUE)
dir.create("tmp__")
setwd("application/countries")
for(i in 1:length(country_names)){
  print(country_names[i])
  print("zipping....")
  zfile <- paste0("../../tmp__/",country_names[i],".zip")
  zip(zfile, country_names[i])
  
  put_object(zfile,
             bucket = write_bucket,
             object = paste0("system_yoda/",country_names[i],".zip"),
             multipart = TRUE
  )
}
setwd("../..")
unlink("tmp__", recursive = TRUE)