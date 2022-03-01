## ----eval= TRUE, echo=FALSE---------------------------------------------------

library(knitr)
opts_chunk$set(tidy = TRUE, fig.align = "center")


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


## ----load packages, eval=TRUE, echo=TRUE, warning=FALSE, message=FALSE--------

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
#  kbl <- kable_styling(kbl, bootstrap_options = "striped", font_size = 14)
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
#             xl = 3, collevels = seq(-60, 0, 5), osci = TRUE)
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
#             xl = 3, collevels = seq(-60, 0, 5), osci = TRUE)
#  

## ---- eval = FALSE------------------------------------------------------------
#  
#  Rraven::exp_raven(master.sf, path = td, file.name = "example_master_selection_table")
#  

## ---- eval = FALSE------------------------------------------------------------
#  
#  # read master
#  exmp.master <- readWave(file.path(td, "example_master.wav"))
#  
#  # add 1 s silence and create first copy
#  exmp.test1 <- addsilw(wave = exmp.master, at = "start", d = 1, output = "Wave", f = exmp.master@samp.rate)
#  
#  # add 2 s silence and create second copy
#  exmp.test2 <- addsilw(wave = exmp.master, at = "start", d = 2, output = "Wave", f = exmp.master@samp.rate)
#  
#  # make noise
#  ns <- noisew(f = exmp.master@samp.rate, d = duration(exmp.test2) + 1, output = "Wave")
#  
#  # make noise exactly the same length and add noise to 2 examples
#  exmp.test1@left <- exmp.test1@left + (ns@left[1:length(exmp.test1@left)] * 150)
#  exmp.test2@left <- exmp.test2@left + (ns@left[1:length(exmp.test2@left)] * 150)
#  
#  # normalize both  before saving
#  exmp.test1 <- normalize(exmp.test1, unit = "16")
#  exmp.test2 <- normalize(exmp.test2, unit = "16")
#  
#  # save simulated re-recorded sound files
#  writeWave(object = exmp.test1, filename = file.path(td, "example_test1.wav"), extensible = FALSE)
#  
#  writeWave(object = exmp.test2, filename = file.path(td, "example_test2.wav"), extensible = FALSE)
#  

## ---- eval = FALSE------------------------------------------------------------
#  
#  found.starts <- search_templates(X = master.sf, template.rows = which(master.sf$orig.sound.file == "start_marker"), test.files = c("example_test1.wav", "example_test2.wav"), path = td); pks
#  

## ---- eval = FALSE, echo=FALSE------------------------------------------------
#  
#  kbl <- kable(found.starts, align = "c", row.names = F,  format = "html", escape = F)
#  
#  kbl <-  kable_styling(kbl, bootstrap_options = "striped", font_size = 14)
#  
#  kbl
#  

## ---- eval = FALSE------------------------------------------------------------
#  
#  alg.tests <- align_test_files(X =  master.sf, Y = found.starts, path = td, by.song = TRUE)
#  

## ---- eval = FALSE------------------------------------------------------------
#  
#  is_extended_selection_table(alg.est)
#  
#  alg.est

## ---- eval = TRUE, echo = FALSE-----------------------------------------------

print(TRUE)


## ---- eval = FALSE,  echo = FALSE---------------------------------------------
#  
#  kbl <- kable(alg.tests, align = "c", row.names = F,  format = "html", escape = F)
#  
#  kbl <-  kable_styling(kbl, bootstrap_options = "striped", font_size = 14)
#  
#  kbl <- scroll_box(kbl, width = "800px", height = "300px")
#  kbl
#  

## ---- eval = FALSE------------------------------------------------------------
#  
#  spectrograms(alg.tests, by.song = "sound.files", xl = 3, collevels = seq(-60, 0, 5), dest.path = td, osci = TRUE)
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
    scale_color_viridis_d(alpha = 0.7) +  
    labs(x = "Time (s)", y = "Amplitude (PMF)") +
    theme_classic()

## -----------------------------------------------------------------------------

envs <- blur_ratio(X = playback_est, output = "list", ssmooth = 1000, pb = FALSE)$envelopes

envs$distance <- as.factor(envs$distance)

ggplot(envs, aes(x= time, y = amp, col = distance)) +
    geom_line() +
    facet_wrap(~ signal.type) + 
    scale_color_viridis_d(alpha = 0.7) +  
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
  scale_color_viridis_d(alpha = 0.7) +  
  labs(x = "Frequency (kHz)", y = "Amplitude (PMF)") +
  coord_flip() +
  theme_classic()


## -----------------------------------------------------------------------------

# get the frequencies higher than lowest bottom but lower than highest top freq
spctr <- spctr[spctr$freq > min(playback_est$bottom.freq) & spctr$freq < max(playback_est$top.freq), ]

ggplot(spctr, aes(y = amp, x = freq, col = distance)) +
  geom_line() +
  facet_wrap(~ signal.type) + 
  scale_color_viridis_d(alpha = 0.7) +  
  labs(x = "Frequency (kHz)", y = "Amplitude (PMF)") +
  coord_flip() +
  theme_classic()


## ---- eval = TRUE-------------------------------------------------------------

# run  envelope correlation
ea <- excess_attenuation(playback_est, method = 1, pb = FALSE)

# check output class
is_extended_selection_table(ea)


## ---- eval = FALSE------------------------------------------------------------
#  # print output
#  ea

## ---- echo = FALSE------------------------------------------------------------

kbl <- kable(ea, align = "c", row.names = F,  format = "html", escape = F)

kbl <-  column_spec(kbl, 9:10, background = "#ccebff", bold = TRUE)

kbl <-  kable_styling(kbl, bootstrap_options = "striped", font_size = 10)

kbl <- scroll_box(kbl, height = "400px")

kbl


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

# run signal to noise ratio
sa <- signal_to_noise_ratio(playback_est,  pb = FALSE, noise.ref = "custom")

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

# run tail to signal ratio
tsr <- tail_to_signal_ratio(playback_est,  pb = FALSE, 
                           type = 1, mar = 0.05)

# check output class
is_extended_selection_table(tsr)


## ---- eval = FALSE------------------------------------------------------------
#  # print output
#  
#  tsr
#  

## ---- echo = FALSE------------------------------------------------------------


kbl <- kable(tsr, align = "c", row.names = F,  format = "html", escape = F)

kbl <-  column_spec(kbl, 9, background = "#ccebff", bold = TRUE)

kbl <-  kable_styling(kbl, bootstrap_options = "striped", font_size = 10)

kbl <- scroll_box(kbl, height = "400px")

kbl


## ---- eval = TRUE-------------------------------------------------------------

# run spcc
spd <- spcc(X = playback_est, method = 1, pb = FALSE, wl = 512)

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


## ---- eval = TRUE-------------------------------------------------------------

# run noise profile
np <- noise_profile(X = playback_est[playback_est$distance > 5, ], mar = 0.05, pb = FALSE)

str(np)

## ---- eval = FALSE------------------------------------------------------------
#  # print output
#  head(np, 20)
#  

## ---- echo = FALSE------------------------------------------------------------

kbl <- kable(np[1:20, ], align = "c", row.names = F,  format = "html", escape = F)

kbl <-  column_spec(kbl, 2:3, background = "#ccebff", bold = TRUE)

kbl <-  kable_styling(kbl, bootstrap_options = "striped", font_size = 10)

kbl <- scroll_box(kbl, height = "400px")

kbl


## ---- eval = TRUE-------------------------------------------------------------

ggplot(np, aes(y = amp, x = freq, col = sound.files)) +
  geom_line(size = 1.4) +
   scale_color_viridis_d(begin = 0.2, end = 0.8, alpha = 0.5) +  
 labs(x = "Frequency (kHz)", y = "Amplitude (dBA)") +
  coord_flip() +
  theme_classic()


## ---- eval = TRUE, warning = FALSE--------------------------------------------

np <- noise_profile(X = playback_est[playback_est$distance > 5, ], 
      mar = 0.1, pb = FALSE, averaged = FALSE)

# make a column containing sound file and selection
np$sf.sl <- paste(np$sound.files, np$selec)

ggplot(np, aes(y = amp, x = freq, col = sound.files, group = sf.sl)) +
  geom_line(size = 1.4) +
    scale_color_viridis_d(begin = 0.2, end = 0.8, alpha = 0.5) +  
 labs(x = "Frequency (kHz)", y = "Amplitude (dBA)") +
  coord_flip() +
  theme_classic()


## ---- eval = TRUE-------------------------------------------------------------

np <- noise_profile(X = playback_est[playback_est$distance > 5, ], 
      mar = 0.05, pb = FALSE, bp = c(0, 10), 
      averaged = FALSE, hop.size = 3)

# make a column containing sound file and selection
np$sf.sl <- paste(np$sound.files, np$selec)

ggplot(np, aes(y = amp, x = freq, col = sound.files, group = sf.sl)) +
  geom_line(size = 1.4) +
  scale_color_viridis_d(begin = 0.2, end = 0.8, alpha = 0.5) +  
  labs(x = "Frequency (kHz)", y = "Amplitude (dBA)") +
  coord_flip() +
  theme_classic()


## ----session info, echo=F-----------------------------------------------------

sessionInfo()


