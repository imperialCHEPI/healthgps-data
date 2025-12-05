


rm(list = ls())

CountryCode = 356 # India

setwd(file.path("C:", "healthgps-data", "data", "diseases"))
getwd()
#Diseases 	= list.files()
#Diseases 	= grep(Diseases, pattern ='.json', invert = TRUE, value = TRUE)
#Cancers 	= grep(Diseases, pattern = 'cancer', value = TRUE)
#NonCancers	= grep(Diseases, pattern = 'cancer', invert = TRUE, value = TRUE)
#Diseases 	= c(NonCancers, Cancers)
Diseases	= c(
		"ischemicheartdisease",
		"diabetes",
		"intracerebralhemorrhage",
		"ischemicstroke",
		"asthma",
		"subarachnoidhemorrhage",
		"chronickidneydisease"
		) ## from Jingmin Config.json file

RiskFactors	= c("sodium", "bmi") # shouldn't there be more?
Sexes		= c("male", "female")


DiseaseIndex = 1

COLLATE_DRs = TRUE
COLLATE_RRs = TRUE

RiskFactor = "bmi"
for (RiskFactor in RiskFactors)
{
	Sex = "male"
	for (Sex in Sexes)
	{
		DiseaseIndex = 1
		for (DiseaseIndex in 1:length(Diseases))
		{
			Disease = Diseases[DiseaseIndex]
			list.files(paste0(Disease, "/relative_risk/risk_factor"))
			RF_Filename = paste0(Disease, "/relative_risk/risk_factor/", Sex, "_", Disease, "_", RiskFactor, ".csv")
			if (file.exists(RF_Filename))
			{
				RiskFactorData = read.csv(file = RF_Filename)

				ColsNoAge = colnames(RiskFactorData)[-1]
				head(RiskFactorData)
				Dummy 			= unique(RiskFactorData[,ColsNoAge])
				AgesNumeric 	= as.numeric(rownames(Dummy)) - 1

				if (length(AgesNumeric) == 1) AgesString = "All" else
				{
					AgesString	= c()
					for (AgeIndex in 1:(length(AgesNumeric)-1))
						AgesString	= c(AgesString, paste0(AgesNumeric[AgeIndex], "-", AgesNumeric[AgeIndex + 1]))
					AgesString	= c(AgesString, paste0(AgesNumeric[length(AgesNumeric)], "-", max(SecondDiseaseData$Age)))
				}

			} else next

			DummyDFrame = data.frame(Disease = Disease, Age = AgesString, Dummy)
			if (DiseaseIndex == 1) BigDataFrame = DummyDFrame else BigDataFrame = rbind(BigDataFrame, DummyDFrame)


		}
	}
}



#DiseaseIndex = 1
#for (DiseaseIndex in 1:length(Diseases))
#{
#	Disease = Diseases[DiseaseIndex]
#	cat(paste0("DiseaseIndex = ", DiseaseIndex, "/", length(Diseases), ": ", Disease , "\n"))
#	print(list.files(paste0(Disease, "/relative_risk/risk_factor")))
#	cat("\n")
#}

IsRemissionAlwaysZero = data.frame(Diseases, AlswaysZero = rep(NA, length(Diseases)))


DiseaseIndex = 1
for (DiseaseIndex in 1:length(Diseases))
{
	Disease = Diseases[DiseaseIndex]
	cat(paste0("\n\nDiseaseIndex = ", DiseaseIndex, "/", length(Diseases), ": ", Disease), "\n\n")
	list.files(paste0(Disease))
	DiseaseData = read.csv(file = paste0(Disease, "/D", CountryCode, ".csv"))



	## Make table of the relative risks between diseases
	if (COLLATE_DRs)
	{
		list.files(paste0(Disease, "/relative_risk/disease/"))
		SecondDiseaseIndex = 3
		for (SecondDiseaseIndex in 1:length(Diseases))
		{
			SecondDisease = Diseases[SecondDiseaseIndex]
			cat(paste0("SecondDisease = ", SecondDisease, "/", length(Diseases), ": ", SecondDisease), "\n")

			Filename = paste0(Disease, "/relative_risk/disease/", Disease, "_", SecondDisease, ".csv")
			if (file.exists(Filename))
			{
				SecondDiseaseData = read.csv(file = Filename)

				Dummy 			= unique(SecondDiseaseData[, c("Male", "Female")])
				AgesNumeric 	= as.numeric(rownames(Dummy)) - 1

				if (length(AgesNumeric) == 1) AgesString = "All" else
				{
					AgesString	= c()
					for (AgeIndex in 1:(length(AgesNumeric)-1))
					{
						AgesString	= c(AgesString, paste0(AgesNumeric[AgeIndex], "-", AgesNumeric[AgeIndex + 1]))
					}
					AgesString	= c(AgesString, paste0(AgesNumeric[length(AgesNumeric)], "-", max(SecondDiseaseData$Age)))
				}

			} else ### RR assumed to be 1 if file not there.
			{
				Dummy = data.frame(Male = 1, Female = 1)
				AgesString = "All"
			}
			DummyDFrame = data.frame(DiseaseOne = Disease, DiseaseTwo = SecondDisease, Age = AgesString, Dummy)


			## grow data frame
			if (DiseaseIndex == 1 & SecondDiseaseIndex == 1) BigDataFrame = DummyDFrame else BigDataFrame = rbind(BigDataFrame, DummyDFrame)
		}

	}


#	str(DiseaseData)
	RemissionIndices = DiseaseData$measure == "remission"
	IsRemissionAlwaysZero$AlswaysZero[DiseaseIndex] = all(DiseaseData$mean[RemissionIndices] == 0)

}

rownames(BigDataFrame) = NULL
head(BigDataFrame, 20)


write.csv(BigDataFrame, file = "C:\\Users\\dlaydon\\OneDrive - Imperial College London\\Health-GPS_SHARED\\Health-GPS_INDIA\\DRs_Summarized.csv",
	row.names = F, col.names = T, quote = F, sep = "\t")



BigDataFrame
#names(IsRemissionAlwaysZero) = Diseases
IsRemissionAlwaysZero

DataSingleDisease
