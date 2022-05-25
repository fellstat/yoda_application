library(paws)
library(jsonlite)

s3Connect <- function() {
  # connect ---------------------------------------
  
  #Set the profile and region here
  Sys.setenv(SECRET_NAME = Sys.getenv("SECRET_NAME"),
             AWS_DEFAULT_REGION = "us-east-1",
             AWS_REGION = "us-east-1")
  
  svc <- secretsmanager()
  
  #Put the name of the secret which contains the aws key info
  see <- svc$get_secret_value(
    SecretId = Sys.getenv("SECRET_NAME")
  )
  
  see <- fromJSON(see$SecretString)
  
  #Fill in the strings
  Sys.setenv(AWS_ACCESS_KEY_ID = see$aws_access_key,
             AWS_SECRET_ACCESS_KEY = see$aws_secret_access_key,
             #AWS_PROFILE = Sys.getenv("AWS_PROFILE"),
             AWS_DEFAULT_REGION = "us-east-1",
             AWS_REGION = "us-east-1")
  
  #Delete the secret info as its now an env variable
  rm(see)
  rm(svc)
  
}

s3Connect()