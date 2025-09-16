#### ==== #### ==== #### ==== #### ==== #### ==== #### ==== #### ==== #### ==== #### ==== #### ==== #### ==== #### ==== #### ==== #### ==== #### ==== #### ====
#### ==== #### ==== #### ==== #### ==== #### ==== #### ==== #### ==== #### ==== #### ==== #### ==== #### ==== #### ==== #### ==== #### ==== #### ==== #### ====
#### ==== Script checks the disease data for completeness

#### ==== #### ==== #### ==== #### ==== #### ==== #### ==== #### ==== #### ====

rm(list = ls())

CountryCode = 356 # India
#CountryCode = 826 # UK

setwd(file.path("C:", "healthgps-data", "data", "diseases"))
getwd()

#Diseases 	= list.files()
#Diseases 	= grep(Diseases, pattern ='.json', invert = TRUE, value = TRUE)
#Cancers 	= grep(Diseases, pattern = 'cancer', value = TRUE)
#NonCancers	= grep(Diseases, pattern = 'cancer', invert = TRUE, value = TRUE)
#Diseases 	= c(NonCancers, Cancers)
#Diseases	= c(
#		"chronickidneydisease",
#		"ischemicheartdisease",
#		"stroke",
#		"ischemicstroke",
#		"intracerebralhemorrhage",
#		"subarachnoidhemorrhage"
#)
#Diseases	= c(
#		"chronickidneydisease",
#		"ischemicheartdisease",
#		"stroke",
#		"diabetes",
#		"gallbladder",
#		"asthma",
#		"lowbackpain",
#		"alzheimer",
#		"osteoarthritiship",
#		"osteoarthritisknee",
#		"colorectalcancer",
#		"esophaguscancer",
#		"kidneycancer",
#		"breastcancer",
#		"trachealbronchuslungcancer",
#		"ischemicstroke",
#		"intracerebralhemorrhage",
#		"subarachnoidhemorrhage"
#		)

#Diseases	= list.dirs(getwd(), full.names = FALSE, recursive = FALSE)

Dir = "C:\\Users\\dlaydon\\OneDrive - Imperial College London\\Health-GPS_SHARED\\healthgps_data_update\\disease\\Health-GPS_disease_India_2021"
Diseases	= list.dirs(Dir, full.names = FALSE, recursive = FALSE)
		


Sexes				= c("male", "female")
SentenceCaseSexes 	= c("Male", "Female")
Ages 				= 0:110

DiseaseIndex = 1

Measures = c("prevalence", "incidence", "mortality", "remission")
#DiseaseDataChecks = matrix(1, nrow = length(Diseases), ncol = length(Measures))
#DiseaseDataChecks

DiseaseDataChecks = expand.grid(Sex = Sexes, Measure = Measures, Disease = Diseases)
DiseaseDataChecks = DiseaseDataChecks[, c("Disease", "Measure", "Sex")]
DiseaseDataChecks$AllAgesPresent 	= 1
DiseaseDataChecks$AllAgesHaveValue 	= 1
#DiseaseDataChecks$AllAgesFinite 	= 1

#colnames(DiseaseDataChecks) = Measures
#rownames(DiseaseDataChecks) = Diseases

#### ==== #### ==== #### ==== #### ==== #### ==== #### ==== #### ==== #### ==== #### ==== #### ====
#### ==== First Check overall disease data "D{COUNTRY_CODE}.csv"
DiseaseIndex = 2
for (DiseaseIndex in 1:length(Diseases))
{
	Disease = Diseases[DiseaseIndex]

	## Load D{COUNTRY_CODE}.csv
	DiseaseData = read.csv(file = paste0(Disease, "/D", CountryCode,  ".csv"))
#	head(DiseaseData)
	# take relevant columns only
	DiseaseData = DiseaseData[, c("gender", "age", "measure", "mean", "lower", "upper")]
	DiseaseData$measure[which(DiseaseData$measure == "Prevalence")] = "prevalence"
	DiseaseData$measure[which(DiseaseData$measure == "Incidence" )] = "incidence"
	DiseaseData$measure[which(DiseaseData$measure == "Mortality" )] = "mortality"
	DiseaseData$measure[which(DiseaseData$measure == "Remission" )] = "remission"
	
	DiseaseData$gender[which(DiseaseData$gender == "Male" )] = "male"
	DiseaseData$gender[which(DiseaseData$gender == "Female" )] = "female"
	
	# checks
	Measure = Measures[1]
	Sex = Sexes[1]
	for (Measure in Measures)
		 for (Sex in Sexes)
		 {
			 # get index
			 RowIndex = which(DiseaseDataChecks$Disease == Disease &
							 DiseaseDataChecks$Sex == Sex &
							 DiseaseDataChecks$Measure == Measure)

			 SubDiseaseData = DiseaseData[DiseaseData$measure == Measure & DiseaseData$gender == Sex,]

			 ## all ages present?
			 if (!identical(sort(SubDiseaseData$age), Ages))
			 {
				 DiseaseDataChecks[RowIndex, "AllAgesPresent"] = NA
				 
				 
				 cat(paste0("\nDisease ", DiseaseIndex, " = ", Disease, ", ", Measure, ", ", Sex, ", Incomplete ages"))
			 }

			 ## all ages have a value?
			 if (any (is.na(SubDiseaseData[, c("mean", "lower", "upper")])))
			 {
				 DiseaseDataChecks[RowIndex, "AllAgesHaveValue"] = NA
				 cat(paste0("\nDisease ", DiseaseIndex, " = ", Disease, ", ", Measure, ", ", Sex, ", has NAs"))
			 }

#			 ## all ages have finite value?
#			 if (any (SubDiseaseData[, c("mean", "lower", "upper")] == Inf) || any (SubDiseaseData[, c("mean", "lower", "upper")] == Inf))
#			 {
#				 DiseaseDataChecks[RowIndex, "AllAgesFinite"] = NA
#				 cat(paste0("\nDisease ", DiseaseIndex, " = ", Disease, ", ", Measure, ", ", Sex, ", has Infs"))
#			 }
		 }
}

FilesWithIssues = DiseaseDataChecks[!complete.cases(DiseaseDataChecks),]
FilesWithIssues
write.table(FilesWithIssues, file = file.path(getwd(), paste0(CountryCode, "_FilesWithIssues.txt")), row.names = F, col.names = T, quote = F, sep = "\t")



#### ==== #### ==== #### ==== #### ==== #### ==== #### ==== #### ==== #### ==== #### ==== #### ====
#### ==== now check "risk_factor"


#RiskFactors	= c("bmi", "sodium", "fruit", "processedmeat", "redmeat", "vegetable", "fibre",
#		"fat", "alcohol", "legume", "saturatedfat")
RiskFactors	= c("bmi", "sodium", "fruit", "processedmeat", "redmeat", "vegetable", "fibre",
		"fat", "alcohol", "legume", "saturatedfat", "alcohol", "polyunsaturatedfattyacid",
		"monounsaturatedfat", "totalsugar", "addedsugar")
RiskFactorChecks 	= expand.grid(Sex = Sexes, RiskFactor = RiskFactors, Disease = Diseases)
RiskFactorChecks 	= RiskFactorChecks[, c("Disease", "RiskFactor", "Sex")]

RiskFactorChecks$AllAgesPresent 	= TRUE
RiskFactorChecks$AllAgesHaveValue 	= TRUE
RiskFactorChecks$AllAgesFinite 		= TRUE
#RiskFactorChecks$FileThere	 		= 1
head(RiskFactorChecks)


DiseaseIndex = 1
for (DiseaseIndex in 1:length(Diseases))
{
	Disease = Diseases[DiseaseIndex]

	RiskFactorIndex = 3
	for (RiskFactorIndex in 1:length(RiskFactors))
	{
		RiskFactor = RiskFactors[RiskFactorIndex]

		Sex = "male"
#		Sex = "female"
		for (Sex in Sexes)
		{
			Filename = paste0(Disease, "/relative_risk/risk_factor/", Sex, "_", Disease, "_", RiskFactor, ".csv")
			# get index
			RowIndex = which(RiskFactorChecks$Disease == Disease & RiskFactorChecks$Sex == Sex &
							RiskFactorChecks$RiskFactor == RiskFactor)

			if (file.exists(Filename))
			{
				RiskFactorData = read.csv(file = paste0(Disease, "/relative_risk/risk_factor/", Sex, "_", Disease, "_", RiskFactor, ".csv"))


				## all ages present?
				if (!identical(sort(RiskFactorData[,1]), Ages))
				{
					RiskFactorChecks[RowIndex, "AllAgesPresent"] = NA
					cat(paste0("\nDisease ", DiseaseIndex, " = ", Disease, ", ", RiskFactor, ", ", Sex, ", Incomplete ages"))

					print("")
					print(RiskFactorData[,1])
				}

				## all ages have a value?
				if (any(is.na(RiskFactorData)))
				{
					RiskFactorChecks[RowIndex, "AllAgesHaveValue"] = NA
					cat(paste0("\nDisease ", DiseaseIndex, " = ", Disease, ", ", RiskFactor, ", ", Sex, ", has NAs"))
				}

				## all ages have finite value?
				if (any(RiskFactorData == Inf))
				{
					RiskFactorChecks[RowIndex, "AllAgesFinite"] = NA
					cat(paste0("\nDisease ", DiseaseIndex, " = ", Disease, ", ", RiskFactor, ", ", Sex, ", has Infs"))
				}

			} else
			{
				cat(paste0("\n", Disease, ", ", RiskFactor, ", ", Sex, ", file doesn't exist"))
#				RiskFactorChecks[RowIndex, "FileThere"] = NA
			}
		}
	}
}

FilesWithIssues = RiskFactorChecks[!complete.cases(RiskFactorChecks),]
FilesWithIssues[is.na(FilesWithIssues)] = FALSE
FilesWithIssues

write.table(FilesWithIssues, file = file.path(getwd(), paste0("FilesWithIssues.txt")), row.names = F, col.names = T, quote = F, sep = "\t")



#### ==== #### ==== #### ==== #### ==== #### ==== #### ==== #### ==== #### ==== #### ==== #### ====
#### ==== now check "/relative_risk/disease/"


RR_DiseaseChecks = expand.grid(Disease2 = Diseases, Disease1 = Diseases)
RR_DiseaseChecks = RR_DiseaseChecks[,2:1]
RR_DiseaseChecks$AllAgesPresent 	= TRUE
RR_DiseaseChecks$AllAgesHaveValue 	= TRUE
RR_DiseaseChecks$AllAgesFinite 		= TRUE
#RR_DiseaseChecks$AllGood_Male 	= TRUE
#RR_DiseaseChecks$AllGood_Female = TRUE

Disease1 = Diseases[8]
Disease2 = Diseases[13]

for (Disease1 in Diseases)
	for (Disease2 in Diseases)
	{
		Filename = paste0(Disease1, "/relative_risk/disease/", Disease1, "_", Disease2, ".csv")

		# get index1
		RowIndex = which(RR_DiseaseChecks$Disease1 == Disease1 & RR_DiseaseChecks$Disease2 == Disease2)

		if (file.exists(Filename))
		{
			RR_DiseaseData = read.csv(file = Filename)

			## all ages present?
			if (!identical(sort(RR_DiseaseData$Age), Ages))
			{
				RR_DiseaseChecks[RowIndex, "AllAgesPresent"] = NA
				cat(paste0("\n", Disease1, ", ", Disease2, ", Incomplete ages"))

				print("")
				print(length(RR_DiseaseData$Age))
				print(range(RR_DiseaseData$Age))
				print(RR_DiseaseData$Age)
			}

			## all ages have a value?
			if (any(is.na(RR_DiseaseData)))
			{
				RR_DiseaseChecks[RowIndex, "AllAgesHaveValue"] = NA
				cat(paste0("\n", Disease1, ", ", Disease2, ", has NAs"))
			}

			## all ages have finite value?
			if (any(RR_DiseaseData == Inf))
			{
				RR_DiseaseChecks[RowIndex, "AllAgesFinite"] = NA
				cat(paste0("\n", Disease1, ", ", Disease2, ", has Infs"))
			}
		}
	}

FilesWithIssues = RR_DiseaseChecks[!complete.cases(RR_DiseaseChecks),]
FilesWithIssues[is.na(FilesWithIssues)] = FALSE

FilesWithIssues

write.table(FilesWithIssues, file = file.path(getwd(), paste0("RR_Disease_FilesWithIssues.txt")), row.names = F, col.names = T, quote = F, sep = "\t")
