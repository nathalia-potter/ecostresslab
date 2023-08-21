# Created by MRU on March 26, 2021
# To prepare the data for the online version of FFP

# Libraries: --------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(REddyProc)

# Test with one file of data: --------------------------------------------------------------

## read full output data:
testfile_header = read.csv("/Volumes/GoogleDrive/My Drive/Data/eddyflux_tests/control/2016/control_2016_dry/run3_adv_timelagopt_planarfit2/eddypro_2016dry_torre_control_as_full_output_2021-02-17T203028.csv", 
                             skip = 1, header = F, nrows = 1, as.is = T)
testfile = read.csv("/Volumes/GoogleDrive/My Drive/Data/eddyflux_tests/control/2016/control_2016_dry/run3_adv_timelagopt_planarfit2/eddypro_2016dry_torre_control_as_full_output_2021-02-17T203028.csv", 
                      skip = 3, header = FALSE)
colnames(testfile)= testfile_header
testfile[testfile == -9999] = NA
testfile = testfile %>%
  filter(filename != "not_enough_data")

timesinutc = as.POSIXct(paste(testfile$date, testfile$time), format="%Y-%m-%d %H:%M", tz = "America/Belem")
attr(timesinutc, "tzone") <- "UTC" 

testfile2 = testfile %>%
  mutate(yyyy = year(date), mm = month(date), day = day(date), HH_UTC = hour(timesinutc), MM = minute(timesinutc),
         zm = 36, d = 0.67 * 22, z0 = 0.15 * 22,
         u_mean = -999, L = L, sigma_v = 2 * `u*`, u_star = `u*`, wind_dir = wind_dir) %>%
  select(yyyy, mm, day, HH_UTC, HH_UTC, MM, zm, d, z0, u_mean, L, sigma_v, u_star, wind_dir)
  
  #   z = canopy height
  #   zm = measurement height
  #   d = displacement height ===> 0.67 * z (from https://www.licor.com/env/support/EddyPro/topics/displacement-height.html)
  #   zo = momentum roughness length [m] ===> zo= 0.15 * z 
  
write.csv(testfile2, "/Volumes/GoogleDrive/My Drive/Data/EddyFlux/FFP_files/Input/control_2016dry_ffp.csv", row.names = FALSE)


# Clean raw data and separate into daytime/nighttime: ---------------------
## Remove data when qc_LE > 5 & qc_co2_flux > 5
## u* filter: apply not only the QAQC criteria of eddy pro but also the u* filtering criteria
## Apply a filter on the NEE_fqc > 0 after the u* filtering 
## removing extreme values (e.g., LE > 900 | LE < -10, abs(co2_flux) > 50) -> would be good to see the distribution of the values. Those data are not necessarily bad in all the sites
## removing data from dates when it's known that there were issues with the instruments
## u* filtering, this should be at least applied. 
## including the correction for the storage of CO2? this is also very important 
## For the footprint climatology: just considering the turbulent signals is good enough, 
### you should only filter for the quality of tau. This the most important thing to consider.

hist(testfile$qc_Tau)
hist(testfile$'u*')

# Now do this for all files of all seasons and put them together: ----------

ffp_inout = function(eddypro_fulloutputfile, towername){  # filename with full path
  ## load and clean the dataset:
  eddypro_fulloutput_header = read.csv(eddypro_fulloutputfile, 
                             skip = 1, header = F, nrows = 1, as.is = T)
  eddypro_fulloutput = read.csv(eddypro_fulloutputfile, 
                      skip = 3, header = FALSE)
  colnames(eddypro_fulloutput)= eddypro_fulloutput_header
  eddypro_fulloutput[eddypro_fulloutput == -9999] = NA
  eddypro_fulloutput = eddypro_fulloutput %>%
    filter(filename != "not_enough_data")
  
  ## extract times and dates:
  timesinutc = as.POSIXct(paste(eddypro_fulloutput$date, eddypro_fulloutput$time), format="%Y-%m-%d %H:%M", tz = "America/Belem")
  attr(timesinutc, "tzone") <- "UTC" 
  
  ## organize all the columns:
  ffp_formatdf = eddypro_fulloutput %>%
    mutate(yyyy = year(date), mm = month(date), day = day(date), HH_UTC = hour(timesinutc), MM = minute(timesinutc),
           zm = 36, d = 0.67 * 22, z0 = 0.15 * 22,
           u_mean = -999, L = L, sigma_v = 2 * `u*`, u_star = `u*`, wind_dir = wind_dir) %>%
    select(yyyy, mm, day, HH_UTC, HH_UTC, MM, zm, d, z0, u_mean, L, sigma_v, u_star, wind_dir)
  
  ## export dataset:
  #ffp_filename = paste0("/Volumes/GoogleDrive/My Drive/Data/EddyFlux/FFP_files/", towername, , "_ffp.csv")
  #write.csv(ffp_formatdf, ffp_filename, row.names = FALSE)
  
  return(ffp_formatdf)
}

alledyproout = list.files()















