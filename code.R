setwd("/Users/vdykes/Documents/Projects/MenvsWomen")

#### links

# https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/
# https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
# shapefile: https://public.opendatasoft.com/explore/dataset/landkreise-in-germany/information/

#### sex overall

geschlecht <- read.csv2("geschlecht_cleaned.csv", stringsAsFactors = F, colClasses = "character")

geschlecht$Maennlich <- as.numeric(geschlecht$Maennlich)
geschlecht$Weiblich <- as.numeric(geschlecht$Weiblich)
geschlecht$Insgesamt <- as.numeric(geschlecht$Insgesamt)

geschlecht$maennlich_percent <- (geschlecht$Maennlich / geschlecht$Insgesamt) * 100
geschlecht$weiblich_percent <- (geschlecht$Weiblich / geschlecht$Insgesamt) * 100

geschlecht$difference <- geschlecht$maennlich_percent - geschlecht$weiblich_percent

### visualization

## load from shapefile

library(rgdal)
landkreisen <- readOGR(dsn = "/Users/vdykes/Documents/Projects/MenvsWomen/shapefiles", layer = "vg2500_krs")
plot(landkreisen)

if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit() 
library(maptools)
library(broom)
landkreisen_fortified <- tidy(landkreisen, region = "RS")

saveRDS(landkreisen_fortified, "landkreisen_fortified")

library(dplyr)
landkreisen_fortified_geschlecht <- left_join(landkreisen_fortified, geschlecht, by = c("id" = "ID"))


#below: coord_fixed works; coord_map does not
library(ggplot2)
library(viridis)
ggplot(data = landkreisen_fortified_geschlecht,
       aes(long, lat, fill = difference, group = group)) +
  #scale_fill_distiller(type = "div",
  #                    palette = "RdYlBu",
  #                   name = "Percentage",
  #                  direction = -1) +
  scale_fill_viridis(option = "inferno", direction = -1) +
  coord_fixed() +
  geom_polygon() +
  geom_path(colour="grey", lwd = 0.05) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank())

#### by age

geschlecht_age <- read.csv2("geschlecht_alter_cleaned.csv", 
                            stringsAsFactors = F, colClasses = "character")

geschlecht_age_1835 <- geschlecht_age[,c(1,2,8:11,25:28)]

geschlecht_age_1835[,c(3:10)] <- as.numeric(as.character(unlist(geschlecht_age_1835[,c(3:10)])))

geschlecht_age_1835$gesamt <- rowSums(geschlecht_age_1835[,3:10])

geschlecht_age_1835$maennlich_gesamt <- rowSums(geschlecht_age_1835[,3:6])
geschlecht_age_1835$weiblich_gesamt <- rowSums(geschlecht_age_1835[,7:10])

geschlecht_age_1835$maennlich_percent <- 
  (geschlecht_age_1835$maennlich_gesamt / geschlecht_age_1835$gesamt) * 100

geschlecht_age_1835$weiblich_percent <- 
  (geschlecht_age_1835$weiblich_gesamt / geschlecht_age_1835$gesamt) * 100

geschlecht_age_1835$difference <- 
  geschlecht_age_1835$maennlich_percent - geschlecht_age_1835$weiblich_percent

landkreisen_fortified_age <- left_join(landkreisen_fortified, geschlecht_age_1835, by = c("id" = "ID"))

ggplot(data = landkreisen_fortified_age,
       aes(long, lat, fill = difference, group = group)) +
  scale_fill_distiller(type = "div",
                       palette = "RdYlBu",
                       name = "Percentage",
                       direction = -1) +
  #scale_fill_viridis(option = "inferno", direction = -1) +
  coord_fixed() +
  geom_polygon() +
  geom_path(colour="grey", lwd = 0.05) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank())

### with breaks

# https://gis.stackexchange.com/questions/178597/how-to-create-a-continuous-scale-with-distinct-custom-color-and-value-breaks-wit

landkreisen_fortified_age$brks <- cut(landkreisen_fortified_age$difference, 
                                      breaks = 
                                        c(-10, -6, -2, 2, 6, 10, 14))

ggplot(data = landkreisen_fortified_age,
       aes(long, lat, fill = brks, group = group)) +
  # scale_fill_distiller(type = "div",
  #                      palette = "RdYlBu",
  #                      name = "Percentage",
  #                      direction = -1) +
  scale_fill_viridis(option = "inferno", direction = -1, discrete = T) +
  coord_fixed() +
  geom_polygon() +
  geom_path(colour="grey", lwd = 0.05) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank())

#### correlation

cor(log(geschlecht_age_1835$gesamt), geschlecht_age_1835$difference, use = "complete.obs")

ggplot(geschlecht_age_1835, 
       aes(x = log(geschlecht_age_1835$gesamt), y = geschlecht_age_1835$difference)) +
  geom_point() +
  geom_smooth(method=lm)
