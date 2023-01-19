
# some steps earliers I was able to install miniconda (reticulate::install_miniconda) and get a python interpreter path in the global option

# library(reticulate)
# library(rgee)
# ee_install(py_env = "rgee")
# restart r session

library(rgee)
ee_Initialize()
# told me gcloud is not installed. now try to install gcloud
# installed. registered an account. Logged in the account during g cloud SDK initializing in terminal.
# follow through intruction here https://developers.google.com/earth-engine/guides/python_install

ee_Initialize()

# restartred r and try again 
library(rgee)
ee_Initialize()

#still say gcloud is not found. trying restarting system.
# try setting sdk again.
Sys.setenv("EARTHENGINE_GCLOUD" = "C:/Program Files (x86)/Google/Cloud SDK/google-cloud-sdk/bin/")
ee_Authenticate()

# is it that the connection with python was wrong? 
reticulate::py_config() # looks fine

# uhg. 

#https://github.com/r-spatial/rgee/issues/290 
# according to this post, ran 
ee_check()
#Error in ee_check_gcloud() : 
#gcloud failed [os.system('gcloud --help')]. Please check
#for any errors above and install gcloud if needed (https://cloud.google.com/sdk/docs/install).

# then run 
system("gcloud --help") # did return a bunch of info. The connection seems fine. 
ee_check_gcloud()

# The default installation doesn't include the App Engine extensions required to deploy an application using gcloud commands. These components can be installed using the gcloud CLI component manager.
# https://cloud.google.com/sdk/docs/components
#C:/Users/wenjing.xu/AppData/Local/r-miniconda/envs/rgee/python.exe

# set path?

Sys.setenv(PATH= sprintf("%s:%s", Sys.getenv("PATH"),   "C:/Program Files (x86)/Google/Cloud SDK/google-cloud-sdk/bin/"))
Sys.setenv(PATH= sprintf("%s:%s", Sys.getenv("PATH"),  "C:/Users/wenjing.xu/AppData/Local/Google/Cloud SDK/google-cloud-sdK/bin/"))

Sys.setenv("EARTHENGINE_GCLOUD" = "C:/Users/wenjing.xu/AppData/Local/Google/Cloud SDK/google-cloud-sdK/bin/")
ee_check_gcloud()

# ok scrape all those, instead 
ee_Authenticate(auth_mode = 'notebook')   #using umich account  <- this step bypass the ee_check_gcloud error.
ee_Initialize()
# ee.ee_exception.EEException: Not signed up for Earth Engine or project is not registered.
#https://developers.google.com/earth-engine/guides/service_account
# created a service account on cloud. now register the service account https://signup.earthengine.google.com/#!/service_accounts
# using service account email address mara-gee-umich@gee-mara.iam.gserviceaccount.com

# did not need any code below at the end. still don't really know how to do it in a streamline
# ee_Initialize(user = "wenjing.xuuu@gmail.com", drive = TRUE, gcs = TRUE)
# 
# Sak_file <- ("./document/rgee-gcs/tidal-triumph-375223-5d4bbc1f844b.json")
# # Assign the SaK to a EE user.
# ee_utils_sak_copy(
#   sakfile =  SaK_file,
#   users = c("wenjing.xuuu") # Unlike GD, we can use the same SaK for multiple users.
# )
# 
# # Validate your SaK
# ee_utils_sak_validate()