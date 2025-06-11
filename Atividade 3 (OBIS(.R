#OBIS
## OBIS
dori_obis <- robis::occurrence("Natator depressus")
# checar dados
names(dori_obis)
dori_obis1 <- dori_obis %>% 
  dplyr::select(scientificName, decimalLatitude, decimalLongitude,flags, 
               basisOfRecord, occurrenceStatus, rightsHolder, 
                datasetName, recordedBy, locality)%>% 
  distinct()

# check problemas reportados (flags)
dori_obis1 %>% 
  distinct(flags)
# check NA em datasetName
dori_obis1 %>% 
  filter(!flags %in% c("NO_DEPTH,ON_LAND", "ON_LAND", "DEPTH_EXCEEDS_BATH,ON_LAND"),
         is.na(datasetName))

# freq x tipo de observação
dori_obis1 %>% 
  filter(!flags %in% c("NO_DEPTH,ON_LAND", "ON_LAND", "DEPTH_EXCEEDS_BATH,ON_LAND"),
         !is.na(datasetName)) %>%
  group_by(datasetName, basisOfRecord) %>% 
  reframe(freq = n()) %>%
  ggplot(aes(x = freq, fill = basisOfRecord)) +
    geom_histogram()

# checar niveis
dori_obis1 %>% 
  filter(!flags %in% c("NO_DEPTH,ON_LAND", "ON_LAND", "DEPTH_EXCEEDS_BATH,ON_LAND"),
         !is.na(datasetName))%>%
  lapply(., unique)


# aplicar filtros
world <- map_data('world')
dori_obis <- dori_obis1 %>% 
  filter(!flags %in% c("NO_DEPTH,ON_LAND", "ON_LAND", "DEPTH_EXCEEDS_BATH,ON_LAND"),
         !is.na(datasetName))%>%
         

# plot
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = dori_obis1, aes(x = decimalLongitude, y = decimalLatitude, color = basisOfRecord)) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Natator depressus")))


dori_obis <- dori_obis1 %>% 
  dplyr::filter(decimalLatitude < 0 | decimalLongitude < 100)

# plot
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = dori_obis1, aes(x = decimalLongitude, y = decimalLatitude, color = basisOfRecord)) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Natator depressus")))

# unir GBIF e OBIS

# ver diferencas
setdiff(names(dori_gbif_ok), names(dori_obis))

setdiff(names(dori_obis), names(dori_obis))
library(tidyr)
library(dplyr)
all_data <- bind_rows(dori_gbif_ok %>% 
                        mutate(repo = paste0("gbif", row.names(.))), 
                      dori_obis %>% 
                        mutate(repo = paste0("obis", row.names(.)))) %>%
  tibble::column_to_rownames("repo") %>% 
  dplyr::select(decimalLongitude, decimalLatitude, basisOfRecord) %>% 
  distinct() %>% 
  tibble::rownames_to_column("occ") %>% 
  separate(col = "occ", into = c("datasetName", "rn"), sep = 4) %>%
  mutate(scientificName = "Natator depressus") %>% 
  dplyr::select(-rn)


# mapear ocorrencias
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = all_data, aes(x = decimalLongitude, y = decimalLatitude, color = datasetName)) +
  #theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Natator depressus")))

write.csv(all_data, "C:/Users/dudap/OneDrive/Disciplina César/Atividade 1.csv", row.names = FALSE)
