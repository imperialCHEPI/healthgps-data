



rm(list = ls())

CountryCode = 356 # India

## Import Country data
Year = 2022


setwd(file.path("C:", "healthgps-data", "data", "undb", "population"))

PopData = read.csv(file = paste0("P", CountryCode, ".csv"))
PopData = PopData[which(PopData$Time == Year), ]
ProportionBothSexes = PopData$PopTotal 	/ sum(PopData$PopTotal)
ProportionMale 		= PopData$PopMale	/ sum(PopData$PopMale)
ProportionFemale 	= PopData$PopFemale	/ sum(PopData$PopFemale)
PopData = cbind(PopData, ProportionBothSexes, ProportionMale, ProportionFemale)
str(PopData)
#unique(PopData$Time)
range(PopData$Age)
MaxAge = max(PopData$Age)

setwd(file.path("C:", "healthgps-data", "data", "diseases"))
getwd()


Diseases = c(
		"pulmonary"                  ,"diabetes"                    ,
		"ischemicheartdisease"       ,"larynxcancer"                ,
		"trachealbronchuslungcancer" ,"otherpharynxcancer"          ,
		"asthma"                     ,"lowerrespiratoryinfections"  ,
		"tuberculosis"               ,"stroke"                      ,
		"cervicalcancer"            , "alcoholusedisorders"        ,
		"cirrhosis"                 , "lipandoralcavitycancer"     ,
		"livercancer"               , "roadinjuries"               ,
		"selfharm"                  , "intracerebralhemorrhage"    ,
		"subarachnoidhemorrhage"    , "ischemicstroke"				)


#WeightedIncidence = data.frame(MaleIncidence = rep(NA, length(Diseases)), FemaleIncidence = rep(NA, length(Diseases)))
WeightedIncidence = matrix(NA, nrow = length(Diseases), ncol = 2)
colnames(WeightedIncidence) = c("MaleIncidence", "FemaleIncidence")
rownames(WeightedIncidence) = Diseases

signif(WeightedIncidence["lowerrespiratoryinfections",][1] / WeightedIncidence, 2)


IsRemissionAlwaysZero = data.frame(Diseases, AlswaysZero = rep(NA, length(Diseases)))

DiseaseIndex = 1

ColumnsICareAbout = c("age", "gender_id", "gender", "mean")

for (DiseaseIndex in 1:length(Diseases))
{
	Disease = Diseases[DiseaseIndex]
#	cat(paste0("\n\nDiseaseIndex = ", DiseaseIndex, "/", length(Diseases), ": ", Disease), "\n\n")
#	list.files(paste0(Disease))
	DiseaseData = read.csv(file = paste0(Disease, "/D", CountryCode, ".csv"))
	RemissionIndices = DiseaseData$measure == "remission"
	IsRemissionAlwaysZero$AlswaysZero[DiseaseIndex] = all(DiseaseData$mean[RemissionIndices] == 0)
#	DiseaseData[RemissionIndices,]
	
	
#	str(DiseaseData)
	DiseaseData = DiseaseData[which(DiseaseData$measure == "incidence"), ColumnsICareAbout]
	DiseaseData = DiseaseData[which(DiseaseData$age <= MaxAge), ]
	
	MaleData 	= DiseaseData[which(DiseaseData$gender_id == 1), ]
	FemaleData 	= DiseaseData[which(DiseaseData$gender_id == 2), ]
	
	OrderMale  		= order(MaleData$age)
	OrderFemale  	= order(FemaleData$age)
	
	MaleData 	= MaleData	[OrderMale	, ]
	FemaleData 	= FemaleData[OrderFemale, ]
	
	WeightedIncidence[DiseaseIndex, 1] = sum(MaleData$mean 		* PopData$ProportionMale)
	WeightedIncidence[DiseaseIndex, 2] = sum(FemaleData$mean 	* PopData$ProportionFemale)
}


IsRemissionAlwaysZero






barplot(t(WeightedIncidence), beside = TRUE,las=2, main = "Weighted Incidence by Disease")


barplot(t(WeightedIncidence), beside = TRUE,las=2, main = "Weighted Incidence by Disease", horiz=T)



# Convert the matrix to a data frame, keeping Disease as a column
WeightedIncidence_df <- as.data.frame(WeightedIncidence)
WeightedIncidence_df$Disease <- rownames(WeightedIncidence_df)

# Convert the data frame from wide to long (tidy) format
library(tidyr)
Incidence_long <- pivot_longer(
		WeightedIncidence_df,
		cols = c("MaleIncidence", "FemaleIncidence"), # Columns to gather
		names_to = "Gender",                        # Name for the new key column
		values_to = "Weighted_Incidence"            # Name for the new value column
)

# Clean up the Gender names for better display
Incidence_long$Gender <- gsub("Incidence", "", Incidence_long$Gender)

library(ggplot2)
library(grid) 

png(filename = file.path(getwd(), "WeightedIncidencePlot.png"), units = "in", height = 7, width = 11, res = 300)
ggplot(Incidence_long, aes(x = Disease, y = Weighted_Incidence, fill = Gender)) +
		# Create the bar plot
		geom_bar(stat = "identity", position = "dodge") +
		# Add labels and title
		labs(
				title = "Weighted Incidence by Disease",
				x = NULL,
				y = "Weighted Incidence"
		) +
		# Rotate x-axis labels by 45 degrees, make them 1.5x bigger, and add margin
		theme_minimal() +
		theme(
				# Rotate and size the x-axis text
				axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
				axis.text.y = element_text(size = 13),
				axis.title.y = element_text(size = 18),
				# Add a buffer zone to the plot's left margin
				plot.margin = unit(c(1, 1, 1, 2), "cm"), # T, R, B, L (increased left margin to 1.5cm)
				plot.title = element_text(hjust = 0.5) # Center the title
		) +
		# Use a distinct color palette
		scale_fill_manual(values = c("Female" = "maroon", "Male" = "darkblue"))
dev.off()








