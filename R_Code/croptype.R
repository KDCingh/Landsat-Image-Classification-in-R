library(raster)
library(tidyverse)
library(sf)
library(rpart)
library(rpart.plot)
library(rasterVis)
library(mapedit)
library(mapview)
library(caret)
library(e1071)
library(forcats)

setwd("D:/KD/R_Project")
# Load the Landsat image and the training data
band1 <- raster("LC08_L2SP_141041_20211226_20211230_02_T1/LC08_L2SP_141041_20211226_20211230_02_T1_SR_B1.TIF")
band2 <- raster("LC08_L2SP_141041_20211226_20211230_02_T1/LC08_L2SP_141041_20211226_20211230_02_T1_SR_B2.TIF")
band3 <- raster("LC08_L2SP_141041_20211226_20211230_02_T1/LC08_L2SP_141041_20211226_20211230_02_T1_SR_B3.TIF")
band4 <- raster("LC08_L2SP_141041_20211226_20211230_02_T1/LC08_L2SP_141041_20211226_20211230_02_T1_SR_B4.TIF")
band5 <- raster("LC08_L2SP_141041_20211226_20211230_02_T1/LC08_L2SP_141041_20211226_20211230_02_T1_SR_B5.TIF")
band6 <- raster("LC08_L2SP_141041_20211226_20211230_02_T1/LC08_L2SP_141041_20211226_20211230_02_T1_SR_B6.TIF")
band7 <- raster("LC08_L2SP_141041_20211226_20211230_02_T1/LC08_L2SP_141041_20211226_20211230_02_T1_SR_B7.TIF")
# Read the individual bands and stack them
img <- stack(band1, band2, band3, band4, band5, band6, band7)
names(img) <- c(paste0("B", 1:7, coll = ""))
# Define the extent
aoi <- st_read("Sarlahi.shp")
aoi <-st_transform(aoi, crs(img))
# Clip the stacked raster to the custom extent
image <- crop(img, aoi)

# plot the clipped raster True Color Composite
par(col.axis="white",col.lab="white",tck=0)
plotRGB(image, r = 4, g = 3, b = 2, axes = TRUE, 
        stretch = "lin", main = "True Color Composite")
box(col="white")

# plot the clipped raster False Color Composite
par(col.axis="white",col.lab="white",tck=0)
plotRGB(image, r = 5, g = 4, b = 3, axes = TRUE, stretch = "lin", main = "False Color Composite")
box(col="white")

#We calculate NDVI using the following equation (NIR - Red)/(NIR + Red). The [[ ]] notation specifies the bands, in this case band 5 for NIR and band 4 for Red, within the multi-band raster.
ndvi <- (image[[5]] - image[[4]])/(image[[5]] + image[[4]])

as(ndvi, "SpatialPixelsDataFrame") %>% 
  as.data.frame() %>%
  ggplot(data = .) +
  geom_tile(aes(x = x, y = y, fill = layer)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "NDVI for Sarlahi", 
       x = " ", 
       y = " ") +
  scale_fill_gradient(high = "#CEE50E", 
                      low = "#087F28",
                      name = "NDVI")

# Read training points
training_shp <- st_read("LULC_Crop-Types_In-SituData2021_USP.shp")
training_shp <-st_transform(training_shp, crs(img))
LULC_aoi <- st_intersection(training_shp, aoi)

# Define replacement values
replacement_value_data_CropT <- 'OtherCrop'
# Replace NA values with specific values
LULC_aoi$data_CropT <- ifelse(is.na(LULC_aoi$data_CropT ), replacement_value_data_CropT  , LULC_aoi$data_CropT)


# Use mutate to create the "id" column based on the mapping
Class<- c("Sugarcane" = 1, "PaddyRice" = 2, "Orchid" = 3, "Bamboo" = 4,"OtherCrop" = 5)
LULC_aoi <- LULC_aoi %>%
  mutate(id = Class[data_CropT])
write_sf(LULC_aoi, "Sarlahi_TP2.shp")

training <- st_read("Sarlahi_TP2.shp", quiet = TRUE)

#Plot the training points
ggplot() +
  geom_sf(data = training, aes(color = data_CropT ), size = 0.5) +
  scale_color_manual(values = c('cyan', 'darkgreen', 'blue','red','yellow')) +
  labs(title = "Classification points by land use") +
  theme(panel.background = element_blank(), axis.ticks = element_blank(), axis.text = element_blank())

# Split the AOI into training and testing datasets (70% training, 30% testing)
set.seed(17)  # For reproducibility
splitIndex <- createDataPartition(training$data_CropT, p = 0.7, 
                                  list = FALSE, 
                                  groups = 1)
training_points <- training[splitIndex, ]
testing_points <- training[-splitIndex, ]
unique(testing_points$data_CropT)
#Extracting spectral values from the raster
training_points <- as(training_points, 'Spatial')
df <- raster::extract(image, training_points) %>%
  round()
head(df)
#Classifying Data
df <- data.frame(training_points$data_CropT, df)

model.class <- rpart(as.factor(training_points.data_CropT)~., data = df, method = 'class', minsplit = 5)
#plot the decision tree.
rpart.plot(model.class, box.palette = 0, main = "Classification Tree")

#Using the model, we predict on the entire image and set the classes to the four land cover categories.

pr <- predict(image, model.class, type ='class', progress = 'text') %>% 
  ratify()

levels(pr) <- levels(pr)[[1]] %>% 
  mutate(legend = c("Sugarcane", "PaddyRice", "Orchid", "Bamboo","Othercrop"))

levelplot(pr, maxpixels = 1e6,
          col.regions = c('burlywood', 'yellow','darkgreen','cyan','red','blue'),
          scales=list(draw=FALSE),
          main = " Classified Imagery")

predicted <- raster::extract(pr, testing_points) %>% 
  as.data.frame() %>% 
  rename(id = ".")

df1 <- data.frame(
  obs_id = testing_points$id,
  obs = testing_points$data_CropT,
  pred = predicted$id
)
# Replace numeric values with character labels
df1$pred <- ifelse(df1$pred == 1, "Sugarcane",
                 ifelse(df1$pred == 2, "PaddyRice",
                        ifelse(df1$pred == 3, "Orchid",
                               ifelse(df1$pred == 4, "Bamboo",
                                      ifelse(df1$pred == 5, "OtherCrop", NA)))))


unique(df1$obs)
unique(df1$pred)

# Assuming your dataframe is named 'data'
df1$obs <- as.factor(df1$obs)
df1$pred <- as.factor(df1$pred)

# Align the levels of 'pred' with the levels of 'obs'
#levels(df1$pred) <- levels(df1$obs)

# Calculate confusion matrix, precision, recall, F-score, accuracy, and Kappa
confusion_matrix <- confusionMatrix(df1$pred, df1$obs)
print("Confusion Matrix:")
print(confusion_matrix)

