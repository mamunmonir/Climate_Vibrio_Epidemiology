setwd("/Users/monirulmemlab/Research_data/Hospital_data_icddrb")

data2<-read.csv("monthly_cases_2008_2022.csv", header=TRUE)

locations<-as.vector(colnames(data2)[-c(1,2)])

data <- data.frame(
	Location = rep(locations, each = 180),
	Year = rep(rep(2008:2022, each = 12), times = 91),
	Month = rep(1:12, times = 1365),
	Cases = cases_loc
)

data<-cbind(Location = rep(locations, each = 180),
						 Year = rep(rep(2008:2022, each = 12), times = 91),
						 Month = rep(1:12, times = 1365),
						 Cases = cases_loc)

data <- expand.grid(Location = locations, Year = years, Month = months)

cases_loc<-NULL

for (i in 3:93)
{
	cases_loc<-c(cases_loc,data2[,i])
}

# Create a sample dataset with multiple locations and years
set.seed(42)  # For reproducibility

locations <- c("Location A", "Location B", "Location C", "Location D", "Location E")
years <- 2018:2022
months <- 1:12

data <- expand.grid(Location = locations, Year = years, Month = months)
data$Cases <- sample(1:100, nrow(data), replace = TRUE)


##########
#
data<-read.csv("data.csv",header=TRUE)


# Load necessary libraries
library(ggplot2)

# Create a ggplot object for separate bubble plots based on locations with the y-axis representing years
p <- ggplot(data, aes(x = factor(Month, levels = 1:12, labels = month.abb), y = Location, size = Cases, color = Location))

# Add the geometry (bubble plot)

tiff("test.tiff",width=15, height=250, units="in", compression =c("lzw"), res=100)
p + geom_point() +
	
	# Create separate plots for each location
	facet_wrap(~ Year, scales = "free_y", ncol = 1) +
	
	# Customize the plot and remove grid lines
	labs(title = "Monthly Cases by Location",
			 x = "Month",
			 y = "Year",
			 size = "Cases") +
	scale_size_continuous(range = c(0, 12))

dev.off()

#-------------------------

data<-read.csv("data_dhaka_test.csv",header=TRUE)


# Load necessary libraries
library(ggplot2)

# Create a ggplot object for separate bubble plots based on locations with the y-axis representing years
p <- ggplot(data, aes(x = factor(Month, levels = 1:12, labels = month.abb), y = Location, size = Cases, color = Location))

# Add the geometry (bubble plot)

tiff("test.tiff",width=15, height=250, units="in", compression =c("lzw"), res=100)
p + geom_point() +
	
	# Create separate plots for each location
	facet_wrap(~ Year, scales = "free_y", ncol = 1) +
	
	# Customize the plot and remove grid lines
	labs(title = "Monthly Cases by Location",
			 x = "Month",
			 y = "Year",
			 size = "Cases") +
	scale_size_continuous(range = c(0, 12))

dev.off()



############
# Load necessary libraries
library(ggplot2)

# Read the CSV file
data <- read.csv("data_dhaka_test.csv", header = TRUE)

data$Location <- factor(data$Location, levels = unique(data$Location))

# Create a ggplot object for bubble plots
p <- ggplot(data, aes(x = factor(Month, levels = 1:12, labels = month.abb), y = Location, size = Cases, color = Location))

ggplot(data, aes(x = factor(Month, levels = 1:12, labels = month.abb), y = Location, size = Cases, color = Location))
# Create the bubble plot
p <- p + geom_point() +
	
	# Create separate plots for each year
	facet_wrap(~Year, scales = "free_y", ncol = 1) +
	
	# Customize the plot
	labs(title = "Monthly Cases by Location",
			 x = "Month",
			 y = "Location",
			 size = "Cases") +
	scale_size_continuous(range = c(-1, 12)) +  # Adjust the size range
	
	# Save the plot to a TIFF file
	tiff("test_dhaka_3.tiff", width=10, height=250, units = "in", compression = "lzw", res = 100)

print(p)

dev.off()

###############################
# Load necessary libraries
# Load necessary libraries
library(ggplot2)
library(dendextend)

# Read the CSV file
data <- read.csv("data_dhaka_test.csv", header = TRUE)

# Set "Location" as a factor with original levels
data$Location <- factor(data$Location, levels = unique(data$Location))

# Create a ggplot object for bubble plots
p <- ggplot(data, aes(x = factor(Month, levels = 1:12, labels = month.abb), y = Location, size = Cases, color = Location))

# Create the bubble plot
p <- p + geom_point() +
	
	# Customize the plot
	labs(title = "Monthly Cases by Location",
			 x = "Month",
			 y = "Location",
			 size = "Cases") +
	scale_size_continuous(range = c(2, 12))  # Adjust the size range

# Create a dendrogram for locations
dend <- as.dendrogram(hc)

# Get the order of leaves in the dendrogram
dend_order <- order.dendrogram(dend)

# Create separate plots for each level of the dendrogram
plots <- lapply(dend_order, function(i) {
	loc <- data$Location[dend_order[i]]
	p_filtered <- p + facet_grid(. ~ Year, scales = "free_y") +
		theme(strip.placement = "outside") +
		labs(title = paste("Monthly Cases by Location -", loc)) +
		geom_text(data = data[data$Location == loc,], aes(label = Location), hjust = 0.5, vjust = -0.5)
	
	return(p_filtered)
})

# Save the plot to a TIFF file
tiff("test_with_dendrogram.tiff", width = 15, height = 250, units = "in", compression = "lzw", res = 100)

# Print the plots using grid.arrange
library(gridExtra)
grid.arrange(grobs = plots, ncol = 1)

dev.off()

##################################
# Define the file path
setwd("/Users/monirulmemlab/Research_data/Hospital_data_icddrb")
file_path <- "Dhaka_Distance_Matrix2.txt"

# Read the file line by line in reverse order
lines <- rev(readLines(file_path))

# Initialize an empty list to store the matrix data
matrix_data <- list()

# Parse the lines and store the matrix data
for (i in 1:length(lines)) {
	# Split each line into individual elements using space or tab as the delimiter
	elements <- unlist(strsplit(lines[i], "[ \t]+"))
	
	# Convert the elements to numeric values
	elements <- as.numeric(elements)
	
	# Append the row to the matrix_data list
	matrix_data[[i]] <- elements
}

# Convert the list of rows into a matrix, transposing it
lower_triangular_matrix <- t(do.call(cbind, matrix_data))

# Print the resulting matrix
print(lower_triangular_matrix)


#############

dist_dhaka<-read.csv("Dhaka_Distance_Matrix2.csv", header=TRUE)

rownames(dist_dhaka) <- colnames(dist_dhaka)

clusters <- hclust(as.dist(dist_dhaka))
plot(clusters)

heatmap(clusters)

hc <- hclust(as.dist(dist_dhaka))
hc$order
dd <- as.dendrogram(hc)
order.dendrogram(dd) ## the same :
stopifnot(hc$order == order.dendrogram(dd))

dendro_plot <- ggdendrogram(data = dd, rotate = TRUE, size = 10)

# Preview the plot

tiff("dhaka_dendrogram.tiff", width = 15, height = 30, units = "in", compression = "lzw", res = 100)
dendro_plot$layers[[2]]$aes_params$size <- 2
# Print the plots using grid.arrange
dendro_plot <- dendro_plot + theme(axis.ticks = element_line(linewidth = 10)) + theme(axis.text.y = element_text(size = 25))
print(dendro_plot)
dev.off()


print(dendro_plot)

write.csv(labels(dd),file="dist_order_dhaka_locations.csv")



d2 <- as.dendrogram(hclust(dist(USArrests)))
labels(d2) ## in this case the same as
stopifnot(identical(labels(d2),
										rownames(USArrests)[order.dendrogram(d2)]))




