## REGRESI SPASIAL ##

library(sf)
library(ggplot2)
library(readxl)
library(sp)
library(lmtest)
library(ggspatial)
library(RColorBrewer)
library(psych)
library(car)
library(nortest)
library(GWmodel)
library(MASS) 
library(spdep) 
library(tidyr)
library(dplyr)
library(sf)
library(spatialreg)
library(tmap)


# Baca file shapefile dari GADM
gadm_data <- st_read("C:/Users/Tricia/Downloads/gadm41_IDN_2.shp")

# Filter data untuk Sulawesi Selatan
sulsel_data <- gadm_data[gadm_data$NAME_1 == "Sulawesi Selatan", ]

kab_kota <- unique(sulsel_data$NAME_2)
print(kab_kota)

#Mengubah nama Kabupaten/Kota yang tidak sesuai
sulsel_data$NAME_2 <- gsub("Pangkajene Dan Kepulauan", "Pangkep", sulsel_data$NAME_2)
sulsel_data$NAME_2 <- gsub("Parepare", "Pare Pare", sulsel_data$NAME_2)
sulsel_data$NAME_2 <- gsub("Sidenreng Rappang", "Sidrap", sulsel_data$NAME_2)

kab_kota <- unique(sulsel_data$NAME_2) 

kab_kota
centroid <- st_centroid(sulsel_data)
coords <- st_coordinates(centroid)

data <- read_excel("C:/Users/Tricia/Downloads/data tugas 4 spasial.xlsx")
data <- data.frame(data)

data <- data[order(data$`Kabupaten.Kota`), ]
data

colnames(data)[colnames(data) == "Kabupaten.Kota"] <- "kab_kota"
colnames(sulsel_data)[colnames(sulsel_data) == "NAME_2"] <- "kab_kota"

ggplot(sulsel_data) + geom_sf(aes(fill = data$kab_kota), show.legend = F) + 
  geom_sf (data = centroid, col= "black")

data_sp <- SpatialPointsDataFrame(coords, data)

Y <- data$TPT
X1 <- data$PMSKN
X2 <- data$DR
X3 <- data$GR
X4 <- data$RLS
X5 <- data$TPAK



#visualisasi peta
bbox <- st_bbox(sulsel_data) 

#TPT
ggplot(sulsel_data) +
  geom_sf(aes(fill = data$TPT), color = "black", size = 0.2) +  
  scale_fill_gradientn(
    colors = brewer.pal(9, "Purples"),  #Palet warna untuk peta
    name = "TPT"           # Label legenda
  ) +  
  labs(
    title = "Sebaran Tingkat Pengangguran Terbuka di Sulawesi Selatan", # Judul peta
    x = "Longitude", y = "Latitude"                        # Label sumbu
  ) +
  geom_sf_text(aes(label = kab_kota), size = 2.5, color = "black", check_overlap = TRUE) +  # Label nama wilayah
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]), 
    ylim = c(bbox["ymin"], bbox["ymax"]), 
    expand = FALSE
  ) +  
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 16, face = "bold")
  )

#PMSKN

ggplot(sulsel_data) +
  geom_sf(aes(fill = data$PMSKN), color = "black", size = 0.2) +  
  scale_fill_gradientn(
    colors = brewer.pal(9, "Blues"),  
    name = "PMSKN"          
  ) +  
  labs(
    title = "Sebaran Persentase Penduduk Miskin di Sulawesi Selatan", 
    x = "Longitude", y = "Latitude"                        
  ) +
  geom_sf_text(aes(label = kab_kota), size = 2.5, color = "black", check_overlap = TRUE) +  
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]), 
    ylim = c(bbox["ymin"], bbox["ymax"]), 
    expand = FALSE
  ) +  
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 16, face = "bold")
  )

#DR

ggplot(sulsel_data) +
  geom_sf(aes(fill = data$DR), color = "black", size = 0.2) +  
  scale_fill_gradientn(
    colors = brewer.pal(9, "Oranges"),  
    name = "DR"          
  ) +  
  labs(
    title = "Sebaran Angka Beban Ketergantungan di Sulawesi Selatan", 
    x = "Longitude", y = "Latitude"                       
  ) +
  geom_sf_text(aes(label = kab_kota), size = 2.5, color = "black", check_overlap = TRUE) + 
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]), 
    ylim = c(bbox["ymin"], bbox["ymax"]), 
    expand = FALSE
  ) +  
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 16, face = "bold")
  )


#GR
ggplot(sulsel_data) +
  geom_sf(aes(fill = data$GR), color = "black", size = 0.2) + 
  scale_fill_gradientn(
    colors = brewer.pal(9, "Greens"), 
    name = "GR"          
  ) +  
  labs(
    title = "Sebaran Gini Ratio di Sulawesi Selatan", 
    x = "Longitude", y = "Latitude"                        
  ) +
  geom_sf_text(aes(label = kab_kota), size = 2.5, color = "black", check_overlap = TRUE) +  
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]), 
    ylim = c(bbox["ymin"], bbox["ymax"]), 
    expand = FALSE
  ) +  
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 16, face = "bold")
  )

#RLS

ggplot(sulsel_data) +
  geom_sf(aes(fill = data$RLS), color = "black", size = 0.2) +  
  scale_fill_gradientn(
    colors = brewer.pal(9, "Reds"),  
    name = "RLS"           
  ) +  
  labs(
    title = "Sebaran Rata-rata Lama Sekolah di Sulawesi Selatan", 
    x = "Longitude", y = "Latitude"                        
  ) +
  geom_sf_text(aes(label = kab_kota), size = 2.5, color = "black", check_overlap = TRUE) +  
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]), 
    ylim = c(bbox["ymin"], bbox["ymax"]), 
    expand = FALSE
  ) +  
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 16, face = "bold")
  )

#TPAK
ggplot(sulsel_data) +
  geom_sf(aes(fill = data$TPAK), color = "black", size = 0.2) +  
  scale_fill_gradientn(
    colors = brewer.pal(9, "Greys"),  
    name = "TPAK"           
  ) +  
  labs(
    title = "Sebaran Tingkat Partisipasi Angkatan Kerja di Sulawesi Selatan",
    x = "Longitude", y = "Latitude"                       
  ) +
  geom_sf_text(aes(label = kab_kota), size = 2.5, color = "black", check_overlap = TRUE) +  
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]), 
    ylim = c(bbox["ymin"], bbox["ymax"]), 
    expand = FALSE
  ) +  
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 16, face = "bold")
  )


#statistik deskriptif
summary(data)


# Boxplot untuk TPT
boxplot(data$TPT, main = "Boxplot Tingkat Pengangguran Terbuka", ylab = "TPT")

# Boxplot untuk PMSKN
boxplot(data$PMSKN, main = "Boxplot Persentase Penduduk Miskin", ylab = "PMSKN")

# Boxplot untuk DR
boxplot(data$DR, main = "Boxplot Angka Beban Ketergantungan", ylab = "DR")

# Boxplot untuk GR
boxplot(data$GR, main = "Boxplot Gini Ratio", ylab = "GR")

# Boxplot untuk RLS
boxplot(data$RLS, main = "Boxplot Rata-rata Lama Sekolah", ylab = "RLS")

# Boxplot untuk TPAK
boxplot(data$TPAK, main = "Boxplot Tingkat Partisipasi Angkatan Kerja", ylab = "TPAK")


#regresi linier

mod <- lm(TPT ~ PMSKN + DR + GR + RLS + TPAK, data=data)
summary(mod)

AIC(mod)

#uji asumsi

#uji multikolinearitas
vif(mod)

#uji normalitas
qqnorm(residuals(mod), main = "Normal Q-Q Plot")
qqline(residuals(mod), col = "red")

shapiro.test(mod$residuals)
ks.test(mod$residuals, "pnorm")
ad.test(mod$residuals)

#uji autokorelasi
dwtest(TPT ~ PMSKN + DR + GR + RLS + TPAK, data=data)

#uji homoskedastisitas
data$residuals <- residuals(mod)
data$fitted <- fitted(mod)

bptest(mod)
spreadLevelPlot(mod)
par(mfrow = c(2,2))
plot(mod)
par(mfrow = c(1,1))
plot(mod,1)


plot(fitted(mod), residuals(mod), 
     main = "Residuals vs Fitted",
     xlab = "Fitted Values", 
     ylab = "Residuals")


reg <- lm(TPT ~ PMSKN + DR + GR + RLS + TPAK,data=data)
summary(reg)
#asumsi
ks.test(reg$residuals, "pnorm")
bptest(reg)
#log-likelihood
logLik(reg)
#AIC
AIC(reg)

# Mengonversi data_fix menjadi SpatialPointsDataFrame
coordinates(data_fix) <- c("long", "lat")  # Pastikan nama kolom sesuai
# Membuat buffer di sekitar titik dengan radius tertentu, misalnya 0.01 derajat
data_sf <- st_as_sf(data_fix)
points_buffer <- st_buffer(data_sf, dist = 0.1)

# Mengonversi buffer menjadi SpatialPolygonsDataFrame
points_buffer_spatial <- as(points_buffer, "Spatial")

#Dependensi spasial
w <- poly2nb(points_buffer_spatial, row.names = points_buffer$`Kabupaten/Kota`, snap=0.2)
summary(w)
wm <- nb2mat(w, style = 'B', zero.policy = TRUE)
rwm <- nb2listw(w, style = "W", zero.policy = TRUE)

# Uji indeks moran
#var. dependen
moran.test(points_buffer$TPT, rwm, alternative = "two.sided")
#var. independen
moran.test(points_buffer$PMSKN, rwm, alternative = "two.sided")
moran.test(points_buffer$DR, rwm, alternative = "two.sided")
moran.test(points_buffer$GR, rwm, alternative = "two.sided")
moran.test(points_buffer$RLS, rwm, alternative = "two.sided")
moran.test(points_buffer$TPAK, rwm, alternative = "two.sided")
#residual
lm.morantest(reg, rwm, alternative = "two.sided")

#langrange multipliers
lm.LMtests(reg, rwm, test = c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))

##Spatial regression
#spatial lag model (SAR)
model <- "TPT ~ PMSKN + DR + GR + RLS + TPAK"
sar <- lagsarlm(model, data=points_buffer, rwm, zero.policy = TRUE)
summary(sar)
#pengukuran
W <- as(rwm, "CsparseMatrix")
trmc <- trW(W, type = "MC")
im <- impacts(sar, tr=trmc, R=100)
sum <- summary(im, zstats=TRUE)
data.frame(sum$res)
data.frame(sum$pzmat)
#asumsi
ks.test(sar$residuals, "pnorm")
bptest.Sarlm(sar)
#likelihood
sar$LL
#AIC
AIC(sar)

#spatial error model (SEM)
sem <- errorsarlm(model, data=points_buffer, rwm, zero.policy = TRUE)
summary(sem)
#asumsi
ks.test(sem$residuals, "pnorm")
bptest.Sarlm(sem)
#log-likelihood
sem$LL
#AIC
AIC(sem)

#spatial lag exogeneous model (SLX)
slx <- lmSLX(model, data=points_buffer, rwm, Durbin = TRUE, zero.policy = TRUE)
summary(slx)
#asumsi
ks.test(slx$residuals, "pnorm")
bptest(slx)
#log-likelihood
logLik(slx)
#AIC
AIC(slx)

#spatial durbin model (SDM)
sdm <- lagsarlm(model, data = points_buffer, rwm, type = "mixed", zero.policy = TRUE)
summary(sdm)
#asumsi
ks.test(sdm$residuals, "pnorm")
bptest.Sarlm(sdm)
#log-likehood
sdm$LL
#AIC
AIC(sdm)

#spatial durbin error model (SDEM)
sedm <- errorsarlm(model, data = points_buffer, rwm, etype = "mixed", zero.policy = TRUE)
summary(sedm)
#asumsi
ks.test(sedm$residuals, "pnorm")
bptest.Sarlm(sedm)
#log-likehood
sedm$LL
#AIC
AIC(sedm)

#spatial autoregressive combined model (SAC)
sac <- sacsarlm(model, data = points_buffer, rwm, Durbin = FALSE, zero.policy = TRUE)
summary(sac)
#asumsi
ks.test(sac$residuals, "pnorm")
bptest.Sarlm(sac)
#log-likehood
sac$LL
#AIC
AIC(sac)

#general nested spatial model (GNSM)
gnsm <- sacsarlm(model, data = points_buffer, rwm, Durbin = TRUE, zero.policy = TRUE)
summary(gnsm)
#asumsi
ks.test(gnsm$residuals, "pnorm")
bptest.Sarlm(gnsm)
#log-likehood
gnsm$LL
#AIC
AIC(gnsm)

#regresi linier (OLS)
p1 <- ggplot(sulsel_data) + geom_sf(aes(fill=reg$residuals)) + 
  scale_fill_gradient2(name = "Residual", low = "red", mid = "white",
                       high = "blue", midpoint = 0) +
  ggtitle("Residual Model Regresi Linier (OLS)") + 
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location="bl", width_hint=0.4) +
  annotation_north_arrow(location="bl", which_north="true",
                         pad_x = unit(0.0,"in"), pad_y=unit(0.2, "in"),
                         style=north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(size = 10, face = "bold"))
p1

#SAR
p2 <- ggplot(sulsel_data) + geom_sf(aes(fill=sar$residuals)) + 
  scale_fill_gradient2(name = "Residual", low = "red", mid = "white",
                       high = "blue", midpoint = 0) +
  ggtitle("Residual Model Spatial Autoregressive (SAR)") + 
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location="bl", width_hint=0.4) +
  annotation_north_arrow(location="bl", which_north="true",
                         pad_x = unit(0.0,"in"), pad_y=unit(0.2, "in"),
                         style=north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(size = 10, face = "bold"), force=TRUE)
p2

#SEM
p3 <- ggplot(sulsel_data) + geom_sf(aes(fill=sem$residuals)) + 
  scale_fill_gradient2(name = "Residual", low = "red", mid = "white",
                       high = "blue", midpoint = 0) +
  ggtitle("Residual Spatial Error Model (SEM)") + 
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location="bl", width_hint=0.4) +
  annotation_north_arrow(location="bl", which_north="true",
                         pad_x = unit(0.0,"in"), pad_y=unit(0.2, "in"),
                         style=north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(size = 10, face = "bold"))
p3

#SLX
p4 <- ggplot(sulsel_data) + geom_sf(aes(fill=slx$residuals)) + 
  scale_fill_gradient2(name = "Residual", low = "red", mid = "white",
                       high = "blue", midpoint = 0) +
  ggtitle("Residual Model Spatial Lag Exogenous (SLX)") + 
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location="bl", width_hint=0.4) +
  annotation_north_arrow(location="bl", which_north="true",
                         pad_x = unit(0.0,"in"), pad_y=unit(0.2, "in"),
                         style=north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(size = 10, face = "bold"))
p4

#SDM
p5 <- ggplot(sulsel_data) + geom_sf(aes(fill=sdm$residuals)) + 
  scale_fill_gradient2(name = "Residual", low = "red", mid = "white",
                       high = "blue", midpoint = 0) +
  ggtitle("Residual Spatial Durbin Model (SDM)") + 
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location="bl", width_hint=0.4) +
  annotation_north_arrow(location="bl", which_north="true",
                         pad_x = unit(0.0,"in"), pad_y=unit(0.2, "in"),
                         style=north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(size = 10, face = "bold"))
p5

#SDEM
p6 <- ggplot(sulsel_data) + geom_sf(aes(fill=sedm$residuals)) + 
  scale_fill_gradient2(name = "Residual", low = "red", mid = "white",
                       high = "blue", midpoint = 0) +
  ggtitle("Residual Spatial Durbin Error Model (SDEM)") + 
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location="bl", width_hint=0.4) +
  annotation_north_arrow(location="bl", which_north="true",
                         pad_x = unit(0.0,"in"), pad_y=unit(0.2, "in"),
                         style=north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(size = 10, face = "bold"))
p6

#SAC
p7 <- ggplot(sulsel_data) + geom_sf(aes(fill=sac$residuals)) + 
  scale_fill_gradient2(name = "Residual", low = "red", mid = "white",
                       high = "blue", midpoint = 0) +
  ggtitle("Residual Model Spatial Autoregressive Combined (SAC)") + 
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location="bl", width_hint=0.4) +
  annotation_north_arrow(location="bl", which_north="true",
                         pad_x = unit(0.0,"in"), pad_y=unit(0.2, "in"),
                         style=north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(size = 10, face = "bold"))
p7

#GNSM
p8 <- ggplot(sulsel_data) + geom_sf(aes(fill=gnsm$residuals)) + 
  scale_fill_gradient2(name = "Residual", low = "red", mid = "white",
                       high = "blue", midpoint = 0) +
  ggtitle("Residual General Nested Spatial Model (GNSM)") + 
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location="bl", width_hint=0.4) +
  annotation_north_arrow(location="bl", which_north="true",
                         pad_x = unit(0.0,"in"), pad_y=unit(0.2, "in"),
                         style=north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(size = 10, face = "bold"))
p8
