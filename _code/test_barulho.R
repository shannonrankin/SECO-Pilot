# test of barulho to match calls between files
# install remotes if not installed
if (!requireNamespace("remotes")) {
  install.packages("remotes")
}

# From github
remotes::install_github("ropensci/baRulho")

# load package
library(baRulho)

DD01 <- "C:/Users/Shannon/Documents/audiomoth/Rails/recordings/samples/ddTestwav/DD01_2453AC0263FA7F76_20250717_030700.wav"
DD02 <-"C:/Users/Shannon/Documents/audiomoth/Rails/recordings/samples/ddTestwav/DD02_249C600363FA8048_20250717_030700.wav"

plot_aligned_sounds()



# Test without it
dd <- birdnet_LFRR_DD %>%
  arrange(datetime_UTC) %>%



glimpse(dd)



