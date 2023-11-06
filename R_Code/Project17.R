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

setwd("D:/R_Project")
# Load the Landsat image and the training data
band1 <- raster("Satellite_Image/LC08_L2SP_141041_20211226_20211230_02_T1_SR_B1.TIF")
band2 <- raster("Satellite_Image/LC08_L2SP_141041_20211226_20211230_02_T1_SR_B2.TIF")
band3 <- raster("Satellite_Image/LC08_L2SP_141041_20211226_20211230_02_T1_SR_B3.TIF")
band4 <- raster("Satellite_Image/LC08_L2SP_141041_20211226_20211230_02_T1_SR_B4.TIF")
band5 <- raster("Satellite_Image/LC08_L2SP_141041_20211226_20211230_02_T1_SR_B5.TIF")
band6 <- raster("Satellite_Image/LC08_L2SP_141041_20211226_20211230_02_T1_SR_B6.TIF")
band7 <- raster("Satellite_Image/LC08_L2SP_141041_20211226_20211230_02_T1_SR_B7.TIF")
# Read the individual bands and stack them
img <- stack(band1, band2, band3, band4, band5, band6, band7)
names(img) <- c(paste0("B", 1:7, coll = ""))
# Define the extent
aoi <- st_read("shp_files/Sarlahi.shp")
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
training_shp <- st_read("shp_files/LULC_Crop-Types_In-SituData2021_USP.shp")
training_shp <-st_transform(training_shp, crs(img))
LULC_aoi <- st_intersection(training_shp, aoi)

# Use mutate to create the "id" column based on the mapping
class_names <- c("Agriculture"= 1, "Commercial"= 2, "CULARCH"= 3, "Forest"= 4, "PublicUse"= 5, "Residential"= 6, "WaterBodies"= 7)
LULC_aoi <- LULC_aoi %>%
  mutate(id = class_names[data_LULC])
write_sf(LULC_aoi, "shp_files/Sarlahi_TP1.shp")
training <- st_read("shp_files/Sarlahi_TP1.shp", quiet = TRUE)

#Plot the training points
ggplot() +
  geom_sf(data = training, aes(color = data_LULC ), size = 0.5) +
  scale_color_manual(values = c('cyan', 'burlywood', 'darkgreen', 'blue','grey','red','yellow')) +
  labs(title = "Classification points by land use") +
  theme(panel.background = element_blank(), axis.ticks = element_blank(), axis.text = element_blank())

# Split the AOI into training and testing datasets (70% training, 30% testing)
set.seed(17)  # For reproducibility
splitIndex <- createDataPartition(training$data_LULC, p = 0.7, 
                                  list = FALSE, 
                                  groups = 1)
training_points <- training[splitIndex, ]
testing_points <- training[-splitIndex, ]
unique(testing_points$data_LULC)
#Extracting spectral values from the raster
training_points <- as(training_points, 'Spatial')
df <- raster::extract(image, training_points) %>%
  round()
head(df)

#Classifying the imagery
df <- data.frame(training_points$data_LULC, df)
model.class <- rpart(as.factor(training_points.data_LULC)~., data = df, method = 'class', minsplit = 7)
#plot the decision tree.
rpart.plot(model.class, box.palette = 0, main = "Classification Tree")

#Using the model, we predict on the entire image and set the classes to the four land cover categories.

pr <- predict(image, model.class, type ='class', progress = 'text') %>% 
  ratify()

levels(pr) <- levels(pr)[[1]] %>% 
  mutate(legend = c("Agriculture", "Commercial","Forest","Residential","Waterbodies"))

levelplot(pr, maxpixels = 1e6,
          col.regions = c('cyan', 'burlywood', 'grey', 'darkgreen','red','yellow','blue'),
          scales=list(draw=FALSE),
          main = " Classified Imagery")

predicted <- raster::extract(pr, testing_points) %>% 
  as.data.frame() %>% 
  rename(id = ".")

df1 <- data.frame(
  obs_id = testing_points$id,
  obs = testing_points$data_LULC,
  pred = predicted$id
)
# Replace numeric values with character labels
df1$pred <- ifelse(df1$pred == 1, "Agriculture",
                 ifelse(df1$pred == 2, "Commercial",
                        ifelse(df1$pred == 3, "CULARCH",
                               ifelse(df1$pred == 4, "Forest",
                                      ifelse(df1$pred == 5, "PublicUse",
                                             ifelse(df1$pred == 6, "Residential",
                                                    ifelse(df1$pred== 7, "WaterBodies", NA)))))))






actual_classes <- df1$obs
confusion_matrix <- table(Predicted = df1$pred, Actual = actual_classes)
print(confusion_matrix)


# Convert the confusion matrix to a data frame
confusion_matrix_df <- as.data.frame(as.table(confusion_matrix))

# Rename the columns for better labels
colnames(confusion_matrix_df) <- c("Predicted", "Actual", "Frequency")

# Create the heatmap using ggplot2
heatmap_plot <- ggplot(confusion_matrix_df, aes(Actual, Predicted, fill = Frequency)) +
  geom_tile() +
  geom_text(aes(label = Frequency), vjust = 1, size = 4) +
  scale_fill_gradient(low = "pink", high = "red") +  # Define color palette
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  labs(title = "Confusion Matrix Heatmap", x = "Actual", y = "Predicted")

# Display the heatmap
print(heatmap_plot)

# Assuming your dataframe is named 'data'
df1$obs <- as.factor(df1$obs)
df1$pred <- as.factor(df1$pred)

# Align the levels of 'pred' with the levels of 'obs'
levels(df1$pred) <- levels(df1$obs)

# Calculate confusion matrix, precision, recall, F-score, accuracy, and Kappa
confusion_matrix <- confusionMatrix(df1$pred, df1$obs)
print("Confusion Matrix:")
print(confusion_matrix)




