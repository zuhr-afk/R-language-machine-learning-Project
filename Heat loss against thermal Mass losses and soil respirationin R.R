install.packages("ggplot2")
install.packages("tidyverse")
install.packages("gridExtra")
install.packages("psych")
install.packages("ggcorrplot")
install.packages("ggpmisc")
install.packages("readr")
install.packages("corrplot")
install.packages("stringi")
install.packages("Matrix")


# Load required packages
library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(psych)
library(corrplot)
library(ggcorrplot)
library(ggpmisc)
library(stringi)
library(Matrix)

# Read the Excel file containing the data
data <- read_excel("Data_Masterstudents with missing data_IK (2).xlsx")
data1<-  read_excel("Excel furnace file.xlsx")
# Calculate the TML
initial_weight <- data1$Weight[1]  # Initial sample weight
tml <- diff(data1$Tr)  # Calculate mass loss per minute
tml_sum <- cumsum(tml)  # Cumulative sum of mass losses

# Sum mass losses per 10°C
tml_per_10C <- tml_sum[seq(1, length(tml_sum), by = 10)]
# Calculate TML
tml_final <- tml_per_10C[length(tml_per_10C)] / initial_weight

# Create a data frame with the selected data (e.g., data from index 10 to 20)
selected_data <- data1[10:20, ]

# Convert Ts and Tr to numeric if they are not already
selected_data$Ts <- as.numeric(selected_data$Ts)
selected_data$Tr <- as.numeric(selected_data$Tr)

# Calculate the heat flow based on the difference between Ts and Tr
heat_flow <- diff( selected_data$Tr-selected_data$Ts )
# Sum the temperature differences in 5°C steps (adjust step size as needed)
step_size <- 5
num_steps <- floor(length(heat_flow) / step_size)
heat_flow_sum <- sapply(1:num_steps, function(i) sum(heat_flow[(i - 1) * step_size + 1:i * step_size]))
# Print the heat flow for the data
cat("Heat Flow for selected data:", heat_flow, "\n")
# Print the heat flow for the selected data
cat("Heat Flow for selected data:", heat_flow_sum, "\n")
# Print the TML
cat("Total Mass Loss (TML):", tml_final, "\n")



# Convert data to numeric
data[, 5:ncol(data)] <- sapply(data[, 5:ncol(data)], as.numeric)

# Calculate thermal mass loss based on the columns used for mass_losses_diff
Thermal_mass_loss <- apply(data[, 6:ncol(data)], 1, function(row) diff(row) / 10)  # Assuming each column represents a 10°C step

# Calculate changes in thermal mass losses
#mass_losses_before <- as.numeric(data[[34]])  # Column corresponding to 300°C
#mass_losses_after <- as.numeric(data[[ncol(data) - 1]])  # Second last column (950°C)
#Heat_flow <- mass_losses_after - mass_losses_before
# Adjust the dimensions of heat_flow and mass_losses_diff
#num_data_points <- min(length(Thermal_mass_loss), length(Heat_flow))
#Thermal_mass_loss <- Thermal_mass_loss[1:num_data_points]
#Heat_flow <- Heat_flow[1:num_data_points]

# Remove outliers from Heat_flow and Thermal_mass_loss
# Calculate the interquartile range (IQR) for Heat_flow and Thermal_mass_loss
Q1_Heat_flow <- quantile(heat_flow, 0.25, na.rm = TRUE)
Q3_Heat_flow <- quantile(heat_flow, 0.75, na.rm = TRUE)
IQR_Heat_flow <- Q3_Heat_flow - Q1_Heat_flow

Q1_Thermal_mass_loss <- quantile(tml_final, 0.25, na.rm = TRUE)
Q3_Thermal_mass_loss <- quantile(tml_final, 0.75, na.rm = TRUE)
IQR_Thermal_mass_loss <- Q3_Thermal_mass_loss - Q1_Thermal_mass_loss

# Calculate the lower and upper bounds for outliers removal
lower_bound_Heat_flow <- Q1_Heat_flow - 1.5 * IQR_Heat_flow
upper_bound_Heat_flow <- Q3_Heat_flow + 1.5 * IQR_Heat_flow

lower_bound_Thermal_mass_loss <- Q1_Thermal_mass_loss - 1.5 * IQR_Thermal_mass_loss
upper_bound_Thermal_mass_loss <- Q3_Thermal_mass_loss + 1.5 * IQR_Thermal_mass_loss

# Update the soil_respiration and Clay_content variables with the filtered data
#Thermal_mass_loss<- filtered_data$
# Correlation between heat flow and changes in thermal mass losses
correlation_mass_losses <- cor(data.frame(heat_flow = heat_flow, tml_final = tml_final), use = "pairwise.complete.obs")
# Calculate mean soil respiration during incubation
soil_respiration <- rowMeans(data[, 10:ncol(data)], na.rm = TRUE)

# Calculate for clay content during incubation
Clay_content <- rowMeans(data[, 11:ncol(data)], na.rm = TRUE)


# Adjust the dimensions of heat_flow soil respiration
num_data_points_respiration <- min(length(soil_respiration), length(heat_flow))
soil_respiration <- soil_respiration[1:num_data_points_respiration]
Heat_flow <- heat_flow[1:num_data_points_respiration]

# Adjust the dimensions of heat_flow and clay content
num_data_points_clay <- min(length(Clay_content), length(Heat_flow))
Clay_content <- Clay_content[1:num_data_points_clay]
Heat_flow <- Heat_flow[1:num_data_points_clay]


# Remove outliers from soil_respiration and Clay_content
Q1_soil_respiration <- quantile(soil_respiration, 0.25, na.rm = TRUE)
Q3_soil_respiration <- quantile(soil_respiration, 0.75, na.rm = TRUE)
IQR_soil_respiration <- Q3_soil_respiration - Q1_soil_respiration

Q1_Clay_content <- quantile(Clay_content, 0.25, na.rm = TRUE)
Q3_Clay_content <- quantile(Clay_content, 0.75, na.rm = TRUE)
IQR_Clay_content <- Q3_Clay_content - Q1_Clay_content

lower_bound_soil_respiration <- Q1_soil_respiration - 1.5 * IQR_soil_respiration
upper_bound_soil_respiration <- Q3_soil_respiration + 1.5 * IQR_soil_respiration

lower_bound_Clay_content <- Q1_Clay_content - 1.5 * IQR_Clay_content
upper_bound_Clay_content <- Q3_Clay_content + 1.5 * IQR_Clay_content

# Correlation between heat flow and clay
correlation_clay<-  cor(data.frame(Heat_flow = Heat_flow, Clay_content = Clay_content), use = "pairwise.complete.obs")

# Correlation between heat flow and soil respiration
correlation_respiration <- cor(data.frame(Heat_flow = Heat_flow, soil_respiration = soil_respiration), use = "pairwise.complete.obs")


# Create a separate plot for each stage of thermal losses
plot1 <- ggplot(data.frame(x = Heat_flow, y = tml_final), aes(x, y)) +
  
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Heat Flow(mW/m2)", y = "Changes in Thermal Mass Losse(mg*g-1/10ºC)s") 


plot2 <- ggplot(data.frame(x = Heat_flow, y = soil_respiration), aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Heat Flow(mW/m2", y = "Soil Respiration(%respiration)") 

#corrplot for soil
p2<-ggcorrplot(correlation_respiration, method = "square",
           hc.order = TRUE,
           outline.color="black",
           type="upper",
          
)
plot3 <- ggplot(data.frame(x = Heat_flow, y = Clay_content), aes(x, y)) +
  
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Heat Flow(mW/m2)", y = "clay content(%clay)") 
#corrplot for clay
p3<-ggcorrplot(correlation_clay, method = "square",
           hc.order = TRUE,
           outline.color="black",
           type="upper",
          
)

# Display correlation results at temperature 300 and 950
cat("Correlation between heat flow and changes in thermal mass losses @ degree 300 and 950:", correlation_mass_losses, "\n")
cat("Correlation between heat flow and soil respiration @ degree 300 and 950:", correlation_respiration, "\n")
cat("Correlation between heat flow and clay content @ degree 300 and 950:", correlation_clay, "\n")




#At another temperature of 400 and 900
# Create a data frame with the selected data (e.g., data from index 20 to 300)
selected_data <- data1[20:30, ]

# Convert Ts and Tr to numeric if they are not already
selected_data$Ts <- as.numeric(selected_data$Ts)
selected_data$Tr <- as.numeric(selected_data$Tr)

# Calculate the heat flow based on the difference between Ts and Tr
heat_flow1 <- diff(selected_data$Tr-selected_data$Ts )
# Sum the temperature differences in 5°C steps (adjust step size as needed)
step_size <- 5
num_steps <- floor(length(heat_flow1) / step_size)
heat_flow_sum <- sapply(1:num_steps, function(i) sum(heat_flow[(i - 1) * step_size + 1:i * step_size]))
# Print the heat flow for the selected data
cat("Heat Flow for selected data:", heat_flow1, "\n")
# Print the TML
cat("Total Mass Loss (TML):", tml_final, "\n")


# Adjust the dimensions of heat_flow and thermal mass loss
num_data_points_heatflow <- min(length(Thermal_mass_loss), length(heat_flow1))
Thermal_mass_loss <- Thermal_mass_loss[1:num_data_points_heatflow]
Heat_flow_new <- heat_flow1[1:num_data_points_heatflow]


# Adjust the dimensions of heat_flow soil respiration
num_data_points_respiration <- min(length(soil_respiration), length(Heat_flow_new))
soil_respiration <- soil_respiration[1:num_data_points_respiration]
Heat_flow_new <- Heat_flow_new[1:num_data_points_respiration]

# Adjust the dimensions of heat_flow and clay content
num_data_points_clay <- min(length(Clay_content), length(Heat_flow_new))
Clay_content <- Clay_content[1:num_data_points_clay]
Heat_flow_new <- Heat_flow_new[1:num_data_points_clay]


# Create a separate plot for each stage of thermal losses
plot4 <- ggplot(data.frame(x = Heat_flow_new, y = tml_final), aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Heat Flow(mW/m2) ", y = "Changes in Thermal Mass Losses(mg*g-1/10ºC)") 

plot5 <- ggplot(data.frame(x = Heat_flow_new, y = soil_respiration), aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Heat Flow(mW/m2)", y = "Changes in soil respiration(%respiration)") 
#corrplot for soil
p5<-ggcorrplot(correlation_respiration_new, method = "square",
           hc.order = TRUE,
           outline.color="black",
           type="upper"
           
)

plot6 <- ggplot(data.frame(x = Heat_flow_new, y =Clay_content), aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Heat Flow(mW/m2)", y = "Changes in Clay_content(%clay)") 
#corrplot for clay
p6<-ggcorrplot(correlation_clay_new, method = "square",
           hc.order = TRUE,
           outline.color="black",
           type="upper"
           
)
# Correlation between heat flow and changes in thermal mass losses
correlation_mass_losses_new <- cor(data.frame(Heat_flow_new = Heat_flow_new, tml_final =tml_final), use = "pairwise.complete.obs")
# Correlation between heat flow and soil respiration
correlation_respiration_new <- cor(data.frame(Heat_flow_new = Heat_flow_new, soil_respiration = soil_respiration), use = "pairwise.complete.obs")
# Correlation between heat flow and clay
correlation_clay_new<-  cor(data.frame(Heat_flow_new = Heat_flow_new, Clay_content = Clay_content), use = "pairwise.complete.obs")


cat("Correlation between heat flow and changes in thermal mass losses @ degree 400 and 900:", correlation_mass_losses_new, "\n")
cat("Correlation between heat flow and changes in soil respiration @ degree 400 and 900:", correlation_respiration_new, "\n")
cat("Correlation between heat flow and changes in clay content @ degree 400 and 900:", correlation_clay_new, "\n")


# Combine the plots using grid.arrange
combined_plots <- grid.arrange(plot1, plot4,ncol = 2)
combined_plots_new1<- grid.arrange(plot2,plot5, ncol=2)
combined_plots_new2<- grid.arrange(plot3,plot6, ncol=2)

# Combine the corrplots using grid.arrange
combined_plots_corr2<- grid.arrange(p2,p5, ncol=1)
combined_plots_corr3<- grid.arrange(p3,p6, ncol=1)
