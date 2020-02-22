## ---- eval = FALSE------------------------------------------------------------
#  
#  # From CRAN would be
#  install.packages("baRulho")
#  
#  #load package
#  library(baRulho)
#  

## ---- eval = FALSE------------------------------------------------------------
#  
#  # From github
#  devtools::install_github("maRce10/baRulho")
#  
#  #load package
#  library(baRulho)
#  
#  # also set a working directory, for this example we will use a temporary directory
#  td <- tempdir()
#  

## ---- eval = TRUE, echo = FALSE, message=FALSE--------------------------------

# global option chunks
knitr::opts_chunk$set(dpi = 70, fig.width = 8, fig.height = 6)

#load package
library(baRulho)
library(kableExtra)

# also set a working directory, for this example we will use a temporary directory
td <- tempdir()

## -----------------------------------------------------------------------------

library(warbleR)
library(ggplot2)
library(viridis)


## ---- eval = FALSE------------------------------------------------------------
#  
#  # synthesize
#  synth.l <- sim_songs(n = 6, durs = 0.1, freqs = c(0.5, 1:5), harms = 1, gaps = 0.15,
#                       diff.fun = "pure.tone", selec.table = TRUE, path = td)
#  
#  # plot spectro
#  spectro(synth.l$wave, scale = FALSE, palette = reverse.topo.colors,
#          grid = FALSE, flim = c(0,6), collevels = seq(-20, 0, 1))
#  

## ---- eval = FALSE------------------------------------------------------------
#  
#  class(synth.l)
#  
#  

## ---- eval = TRUE, echo=FALSE-------------------------------------------------

c("list")


## ---- eval = FALSE------------------------------------------------------------
#  
#  names(synth.l)
#  

## ---- eval = TRUE, echo=FALSE-------------------------------------------------

c("selec.table", "wave")


## ---- eval = FALSE------------------------------------------------------------
#  
#  synth.l$selec.table
#  

## ---- eval = FALSE, echo = FALSE----------------------------------------------
#  
#  kbl <- kable(synth.l$selec_table, align = "c", row.names = F,  format = "html", escape = F)
#  
#  kbl <-  kable_styling(kbl, bootstrap_options = "striped", font_size = 14)
#  
#  kbl
#  

## -----------------------------------------------------------------------------

list.files(path = td, pattern = "\\.wav$")


## ---- eval = TRUE, echo=FALSE-------------------------------------------------

c("2020-01-09_17:00:50.wav")


## ---- eval = FALSE, echo = TRUE-----------------------------------------------
#  
#  # extract selection table
#  st <- synth.l$selec.table
#  
#  # add freq range (0.5 kHz)
#  st$bottom.freq <- st$bottom.freq - 0.25
#  st$top.freq <- st$top.freq + 0.25
#  
#  # make an extended selection table
#  synth.est <- selection_table(X = st, extended = TRUE, pb = FALSE,
#                               confirm.extended = FALSE, path = td)
#  
#  # create master sound file
#  synth.master.sf <- master_sound_file(X = synth.est, file.name = "synthetic_master",
#  dest.path = td, gap.duration = 0.15)
#  

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  
#  # plot spectro (saved in working directory)
#  spectrograms(synth.master.sf, path = td, by.song = "sound.files",
#             xl = 3, collevels = seq(-60, 0, 5))
#  

## ---- eval = FALSE, echo = TRUE-----------------------------------------------
#  
#  # load example data from warbleR
#  data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4",
#  "lbh_selec_table"))
#  
#  # save sound files to temporary folder
#  writeWave(Phae.long1, file.path(td, "Phae.long1.wav"))
#  writeWave(Phae.long2, file.path(td, "Phae.long2.wav"))
#  writeWave(Phae.long3, file.path(td, "Phae.long3.wav"))
#  writeWave(Phae.long4, file.path(td, "Phae.long4.wav"))
#  
#  # make an extended selection table
#  est <- selection_table(X = lbh_selec_table, extended = TRUE, confirm.extended = FALSE,
#  path = td)
#  
#  # create master sound file
#  master.sf <- master_sound_file(X = est, file.name = "example_master",
#  dest.path = td, gap.duration = 0.3)
#  

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  
#  spectrograms(master.sf, path = td, by.song = "sound.files",
#             xl = 3, collevels = seq(-60, 0, 5))
#  

## ---- eval = FALSE------------------------------------------------------------
#  
#  Rraven::exp_raven(master.sf, path = td, file.name = "example_master_selection_table")
#  

## ---- eval = FALSE------------------------------------------------------------
#  
#  # create a matrix that contains the selection/files to be cross-correlated
#  comp_mat <- matrix(c(paste(master.sf$sound.files[1], master.sf$selec[1], sep = "-"), "example_master.wav"),nrow = 1)
#  
#  # run cross correlation
#  xc <- xcorr(master.sf, compare.matrix = comp_mat, wl = 300, ovlp = 30, path = td, output = "list")
#  
#  # find peaks
#  pks <- find_peaks(xc.output = xc, max.peak = TRUE, path = td)
#  

## ---- eval = FALSE, echo=FALSE------------------------------------------------
#  
#  kbl <- kable(pks, align = "c", row.names = F,  format = "html", escape = F)
#  
#  kbl <-  kable_styling(kbl, bootstrap_options = "striped", font_size = 14)
#  
#  kbl
#  

## ---- eval = FALSE------------------------------------------------------------
#  
#  # start on new recording
#  new.start  <- master.sf$start[!master.sf$orig.sound.file %in%  c("start_marker", "end_marker")] - master.sf$start[1] + pks$start[1]
#  
#  new.start

## ---- echo = FALSE------------------------------------------------------------

new.start <- c(2.298655, 2.771722, 3.234833, 3.709811, 4.142433, 4.568611, 4.999855, 5.430077, 5.861322, 6.306788, 6.751011)

new.start 

## ---- eval = FALSE------------------------------------------------------------
#  
#  # get duration
#  durs  <- master.sf$end[!master.sf$orig.sound.file %in% c("start_marker", "end_marker")] - master.sf$start[!master.sf$orig.sound.file %in% c("start_marker", "end_marker")]
#  
#  new.end <- new.start + durs
#  
#  new.end

## ---- echo = FALSE------------------------------------------------------------

new.end <- c(2.471722, 2.934833, 3.409811, 3.842433, 4.268611, 4.699855, 5.130077, 5.561321, 6.006789, 6.451010, 6.896167)

new.end

## ---- eval = FALSE------------------------------------------------------------
#  
#  # get subset excluding markers
#  new.st  <- master.sf[!master.sf$orig.sound.file %in% c("start_marker", "end_marker"), ]
#  
#  # fix time columns
#  new.st$start <- new.start
#  new.st$end <- new.end
#  
#  # add distance column
#  new.st$distance <- 5
#  
#  # make it an extended selection table
#  new.est <- selection_table(new.st, extended = TRUE, confirm.extended = FALSE, by.song = "sound.files", path = td)
#  
#  new.est

## ---- eval = FALSE,  echo = FALSE---------------------------------------------
#  
#  kbl <- kable(new.est, align = "c", row.names = F,  format = "html", escape = F)
#  
#  kbl <-  kable_styling(kbl, bootstrap_options = "striped", font_size = 14)
#  
#  kbl <- scroll_box(kbl, width = "800px", height = "300px")
#  kbl
#  

## ---- eval = FALSE------------------------------------------------------------
#   data("playback_est_unaligned")
#  
#   # method 1
#  playback_est_aligned <- spcc_align(X = playback_est_unaligned)
#  

## ---- eval = FALSE------------------------------------------------------------
#  
#  # rename sound files so aligned and unaligned signals are intercalated
#  unalg <- rename_waves_est(playback_est_unaligned, playback_est_unaligned$sound.files, new.selec = seq(1, 200, by = 2)[1:nrow(playback_est_unaligned)])
#  alg <- rename_waves_est(playback_est_aligned, playback_est_aligned$sound.files, new.selec = seq(2, 200, by = 2)[1:nrow(playback_est_aligned)])
#  
#  # add label
#  unalg$type <- "Before aligning"
#  alg$type <- "After aligning"
#  
#  # put together in a single ext sel tab
#  unalg.alg <- rbind(unalg, alg)
#  
#  # create spectrograms
#  spectrograms(unalg.alg[unalg.alg$signal.type != "ambient", ], dest.path = tempdir(), res = 100, wl = 300, title.labels = "type", sel.labels = NULL)
#  

## ----eval=FALSE---------------------------------------------------------------
#  
#  data("playback_est")
#  
#  playback_est

## ---- eval = TRUE, echo = FALSE-----------------------------------------------

data("playback_est")

kbl <- kable(playback_est, align = "c", row.names = F,  format = "html", escape = F)

kbl <-  column_spec(kbl, 7:8, background = "#ccebff", bold = TRUE)

kbl <-  kable_styling(kbl, bootstrap_options = "striped", font_size = 12)

kbl <- scroll_box(kbl, height = "400px")

kbl


## ----eval=TRUE----------------------------------------------------------------

# count selection per recordings
unique(playback_est$sound.files)


## ---- eval = FALSE------------------------------------------------------------
#  
#  table(playback_est$signal.type, playback_est$distance)
#  

## ---- eval = TRUE, echo=FALSE-------------------------------------------------

tb <- table(playback_est$signal.type, playback_est$distance)

kbl <- kable(tb, align = "c", row.names = TRUE,  format = "html", escape = F)

kbl <-  kable_styling(kbl, bootstrap_options = "striped", font_size = 12)

kbl


## ---- eval = FALSE------------------------------------------------------------
#  
#  # run blur ratio
#  br <- blur_ratio(playback_est, method = 1, pb = FALSE)
#  
#  # check output class
#  is_extended_selection_table(br)

## ---- eval = TRUE, echo = FALSE-----------------------------------------------

# run blur ratio
br <- blur_ratio(playback_est, method = 1, pb = FALSE)

# check output class
is_extended_selection_table(br)


## ---- eval = FALSE------------------------------------------------------------
#  
#  # see output
#  br
#  

## ---- echo = FALSE------------------------------------------------------------

kbl <- kable(br, align = "c", row.names = F,  format = "html", escape = F)

kbl <-  column_spec(kbl, 9:10, background = "#ccebff", bold = TRUE)

kbl <-  kable_styling(kbl, bootstrap_options = "striped", font_size = 10)

kbl <- scroll_box(kbl, height = "400px")

kbl


## ---- eval = FALSE------------------------------------------------------------
#  
#  # run blur ratio
#  br <- blur_ratio(playback_est, method = 1, pb = FALSE, img = TRUE, ssmooth = 300, dest.path = td)
#  

## -----------------------------------------------------------------------------

envs <- blur_ratio(X = playback_est, output = "list", ssmooth = 300, pb = FALSE)$envelopes

envs$distance <- as.factor(envs$distance)

ggplot(envs, aes(x= time, y = amp, col = distance)) +
    geom_line() +
    facet_wrap(~ signal.type) + 
    scale_color_manual(values = viridis(4)) +
    labs(x = "Time (s)", y = "Amplitude (PMF)") +
    theme_classic()

## -----------------------------------------------------------------------------

envs <- blur_ratio(X = playback_est, output = "list", ssmooth = 1000, pb = FALSE)$envelopes

envs$distance <- as.factor(envs$distance)

ggplot(envs, aes(x= time, y = amp, col = distance)) +
    geom_line() +
    facet_wrap(~ signal.type) + 
    scale_color_manual(values = viridis(4)) +
    labs(x = "Time (s)", y = "Amplitude (PMF)") +
    theme_classic()


## ---- eval = FALSE------------------------------------------------------------
#  
#  # run Spectral blur ratio
#  sbr <- spectral_blur_ratio(playback_est, method = 1, pb = FALSE, img = TRUE, dest.path = td)
#  
#  # check output class
#  is_extended_selection_table(sbr)

## ---- eval = TRUE, echo = FALSE-----------------------------------------------

# run Spectral blur ratio
sbr <- spectral_blur_ratio(playback_est, method = 1, pb = FALSE)

# check output class
is_extended_selection_table(sbr)


## ---- eval = FALSE------------------------------------------------------------
#  
#  # see output
#  sbr
#  

## ---- echo = FALSE------------------------------------------------------------

kbl <- kable(sbr, align = "c", row.names = F,  format = "html", escape = F)

kbl <-  column_spec(kbl, 9:10, background = "#ccebff", bold = TRUE)

kbl <-  kable_styling(kbl, bootstrap_options = "striped", font_size = 10)

kbl <- scroll_box(kbl, height = "400px")

kbl


## -----------------------------------------------------------------------------

sbr <- spectral_blur_ratio(X = playback_est, output = "list", pb = FALSE)

spctr <- sbr$spectra

spctr$distance <- as.factor(spctr$distance)

ggplot(spctr, aes(y = amp, x = freq, col = distance)) +
geom_line() +
facet_wrap(~ signal.type) + 
scale_color_manual(values = viridis(4)) +
labs(x = "Frequency (kHz)", y = "Amplitude (PMF)") +
coord_flip() +
theme_classic()


## -----------------------------------------------------------------------------

# get the frequencies higher than lowest bottom but lower than highest top freq
spctr <- spctr[spctr$freq > min(playback_est$bottom.freq) & spctr$freq < max(playback_est$top.freq), ]

ggplot(spctr, aes(y = amp, x = freq, col = distance)) +
geom_line() +
facet_wrap(~ signal.type) + 
scale_color_manual(values = viridis(4)) +
labs(x = "Frequency (kHz)", y = "Amplitude (PMF)") +
coord_flip() +
theme_classic()


## ---- eval = TRUE-------------------------------------------------------------

# run  envelope correlation
ec <- envelope_correlation(playback_est, method = 1, pb = FALSE)

# check output class
is_extended_selection_table(ec)


## ---- eval = FALSE------------------------------------------------------------
#  # print output
#  ec

## ---- echo = FALSE------------------------------------------------------------

kbl <- kable(ec, align = "c", row.names = F,  format = "html", escape = F)

kbl <-  column_spec(kbl, 9:10, background = "#ccebff", bold = TRUE)

kbl <-  kable_styling(kbl, bootstrap_options = "striped", font_size = 10)

kbl <- scroll_box(kbl, height = "400px")

kbl


## ---- eval = TRUE-------------------------------------------------------------

# run spectral correlation
sc <- spectral_correlation(playback_est, method = 1, pb = FALSE)

# check output class
is_extended_selection_table(sc)


## ---- eval = FALSE------------------------------------------------------------
#  # print output
#  sc

## ---- echo = FALSE------------------------------------------------------------

kbl <- kable(sc, align = "c", row.names = F,  format = "html", escape = F)

kbl <-  column_spec(kbl, 9:10, background = "#ccebff", bold = TRUE)

kbl <-  kable_styling(kbl, bootstrap_options = "striped", font_size = 10)

kbl <- scroll_box(kbl, height = "400px")

kbl


## ---- eval = TRUE-------------------------------------------------------------

# run blur ratio
sa <- snr(playback_est,  pb = FALSE, noise.ref = "custom")

# check output class
is_extended_selection_table(sa)


## ---- eval = FALSE------------------------------------------------------------
#  # print output
#  sa

## ---- echo = FALSE------------------------------------------------------------

kbl <- kable(sa, align = "c", row.names = F,  format = "html", escape = F)

kbl <-  column_spec(kbl, 9, background = "#ccebff", bold = TRUE)

kbl <-  kable_styling(kbl, bootstrap_options = "striped", font_size = 10)

kbl <- scroll_box(kbl, height = "400px")

kbl


## ---- eval = TRUE-------------------------------------------------------------

# run spcc
spd <- spcc(X = playback_est, method = 1, pb = FALSE)

# check output class
is_extended_selection_table(spd)


## ---- eval = FALSE------------------------------------------------------------
#  # print output
#  spd

## ---- echo = FALSE------------------------------------------------------------

kbl <- kable(spd, align = "c", row.names = F,  format = "html", escape = F)

kbl <-  column_spec(kbl, 9:10, background = "#ccebff", bold = TRUE)

kbl <-  kable_styling(kbl, bootstrap_options = "striped", font_size = 10)

kbl <- scroll_box(kbl, height = "400px")

kbl


## ----session info, echo=F-----------------------------------------------------

sessionInfo()


