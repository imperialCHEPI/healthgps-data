
rm(list = ls())

CountryCode = 356 # India
#CountryCode = 826 # UK

setwd(file.path("C:", "healthgps-data", "data", "diseases"))
getwd()

COUNTRY_CODE = 356

Sexes				= 0:1
SexNames			= c("Male", "Female")
Ages 				= 0:110
Years 				= 0:29
RiskFactors 		= c("Smoking", "Alcohol")
#RiskFactors 		= c("Alcohol", "Smoking")
ScenarioNumbers		= 1:3
ScenarioNames		= c("10p", "20p", "40p")

HGPS_diseases 		= list.dirs(getwd(), full.names = FALSE, recursive = FALSE)

ConvertYearStringToVector = function(YearString)
{
	if (YearString == "80+")
	{
		LowerBound = 80
		UpperBound = 110

	} else if (YearString == "0-4" | YearString == "5-9")
	{
		LowerBound = as.numeric(substr(YearString, 1,1))
		UpperBound = as.numeric(substr(YearString, 3,3))

	} else
	{
		LowerBound = as.numeric(substr(YearString, 1,2))
		UpperBound = as.numeric(substr(YearString, 4,5))
	}
	return (LowerBound:UpperBound)
}

RiskFactor 		= RiskFactors[1]
RiskFactor 		= RiskFactors[2]
#RiskFactor
ScenarioNumber 	= 1

for (RiskFactor in RiskFactors)
	for (ScenarioNumber in ScenarioNumbers)
{
	cat(paste(RiskFactor, ScenarioNumber, "\n"))

	# Create Filename
	Filename = paste0(RiskFactor, "_PIF")
	if (RiskFactor == "Smoking") Filename = paste0(Filename, "-MORBIDITY")
	Filename = paste0(Filename, "_", ScenarioNames[ScenarioNumber], "-increase-in-price_")

	Filename_Male 		= paste0(Filename, "Male.csv")
	Filename_Female 	= paste0(Filename, "Female.csv")

	# Load impact fraction raw data (for both males then females then combine
	IF_Data_Raw_Male 	= read.csv(file = Filename_Male)
	IF_Data_Raw_Female 	= read.csv(file = Filename_Female)
	IF_Data_Raw 		= rbind(IF_Data_Raw_Male, IF_Data_Raw_Female)
	
		

	# Change column names to be consistent with HGPS
	colnames(IF_Data_Raw) = gsub(pattern = "sex"		, replacement = "Gender"	, colnames(IF_Data_Raw))
	colnames(IF_Data_Raw) = gsub(pattern = "\\bmean\\b"	, replacement = "IF_Mean"	, colnames(IF_Data_Raw))
	colnames(IF_Data_Raw) = gsub(pattern = "\\bl\\b"	, replacement = "IF_Lower"	, colnames(IF_Data_Raw))
	colnames(IF_Data_Raw) = gsub(pattern = "\\bu\\b"	, replacement = "IF_Upper"	, colnames(IF_Data_Raw))
	colnames(IF_Data_Raw) = gsub(pattern = "outcome"	, replacement = "disease"	, colnames(IF_Data_Raw))
	colnames(IF_Data_Raw)

	head(IF_Data_Raw)

	## Expand age groups
	AgeGroups = unique(IF_Data_Raw$age_group)
	AgeGroups
	# Find minimum Age
	#MinAgeInRawData = Inf
	#for (AgeGroup in AgeGroups)
	#{
	#	Blah = ConvertYearStringToVector(AgeGroup)
	#	if (min(Blah) < MinAgeInRawData) MinAgeInRawData = min(Blah)
	#}
	#AgesWhereIfZero = 0:(MinAgeInRawData - 1)

	## Get Diseases
	DiseasesIFData = unique(IF_Data_Raw$disease)
	DiseasesIFData

	# Code Sexes as per HGPS
	IF_Data_Raw$Gender[which(IF_Data_Raw$Gender == "Female")] 	= "1"
	IF_Data_Raw$Gender[which(IF_Data_Raw$Gender == "Male")] 	= "0"
	IF_Data_Raw$Gender = as.numeric(IF_Data_Raw$Gender)

	DiseasesIFData = unique(IF_Data_Raw$disease)
	DiseasesIFData


	# Expand years pot intervention data
	IF_Data_Raw$years_post_int = gsub(pattern = " years", replacement = "", IF_Data_Raw$years_post_int)
	YearsPostInt = unique(IF_Data_Raw$years_post_int)
	YearsPostInt

	Disease_IF = DiseasesIFData[1]
	for (Disease_IF in DiseasesIFData)
	{
		## Convert Disease names in this data to their HGPS-data counterparts.
		if (RiskFactor == "Smoking")
		{
			# Diseases in smoking analysis
			if (Disease_IF  == "COPD") 					DiseasesHGPS_Data = "pulmonary"
			if (Disease_IF 	== "diabetes") 				DiseasesHGPS_Data = "diabetes"
			if (Disease_IF  == "IHD")					DiseasesHGPS_Data = "ischemicheartdisease"
			if (Disease_IF  == "lung_cancer") 			DiseasesHGPS_Data = "trachealbronchuslungcancer"
			if (Disease_IF  == "mouth_orophar_cancer") 	DiseasesHGPS_Data = "otherpharynxcancer"
			if (Disease_IF  == "other_resp_dis") 		DiseasesHGPS_Data = c("asthma", "lowerrespiratoryinfections", "tuberculosis")
			if (Disease_IF  == "stroke") 				DiseasesHGPS_Data = "stroke"
			if (Disease_IF  == "cervix_uter_cancer") 	DiseasesHGPS_Data = "cervicalcancer"
			if (Disease_IF  == "other_CVD") 			next

		} else if (RiskFactor == "Alcohol")
		{
			# Diseases in alcohol analysis
			if (Disease_IF  == "alcohol_use_disorders") 	DiseasesHGPS_Data = "alcoholusedisorders"
			if (Disease_IF 	== "liver_cirrhosis") 			DiseasesHGPS_Data = "cirrhosis"
			if (Disease_IF  == "lip_oral_cancer") 			DiseasesHGPS_Data = "lipandoralcavitycancer"
			if (Disease_IF  == "liver_cancer") 				DiseasesHGPS_Data = "livercancer"
			if (Disease_IF  == "other_pharyngeal_cancers") 	DiseasesHGPS_Data = "otherpharynxcancer"
			if (Disease_IF  == "road_injury") 				DiseasesHGPS_Data = "roadinjuries"
			if (Disease_IF  == "self_harm") 				DiseasesHGPS_Data = "selfharm"
			if (Disease_IF  == "intracerebral_haemorrhage") DiseasesHGPS_Data = c("intracerebralhemorrhage", "subarachnoidhemorrhage")
			if (Disease_IF  == "ischaemic_stroke") 			DiseasesHGPS_Data = "ischemicstroke"
			if (Disease_IF  == "tuberculosis") 				DiseasesHGPS_Data = "tuberculosis"
		
		} 

		## Subset raw data
		IF_Data_Raw_thisDisease = IF_Data_Raw[which(IF_Data_Raw$disease == Disease_IF), ]
		#head(IF_Data_Raw_thisDisease)

		## Create Empty Data Frame to file for this disease
		Processed_IFData_thisDisease 	= expand.grid(Age = Ages, Gender = Sexes, YearPostInt = Years)
		Processed_IFData_thisDisease 	= Processed_IFData_thisDisease[, c("Gender", "Age", "YearPostInt")]
		IF_Mean_ThisDisease 			= rep(0, dim(Processed_IFData_thisDisease)[1])
		Processed_IFData_thisDisease 	= cbind(Processed_IFData_thisDisease, IF_Mean = IF_Mean_ThisDisease)
		#head(Processed_IFData_thisDisease)

		Row = 1
		for (Row in 1:dim(IF_Data_Raw_thisDisease)[1])
		{
			# get sex, age group, years post intevention and IF_Mean for this row.
			Gender_ThisRow 			= IF_Data_Raw_thisDisease$Gender[Row]
			AgeGroup_ThisRow 		= IF_Data_Raw_thisDisease$age_group[Row]
			Ages_ThisRow 			= ConvertYearStringToVector(AgeGroup_ThisRow)
			years_post_int_ThisRow 	= IF_Data_Raw_thisDisease$years_post_int[Row]
			years_post_int_ThisRow	= ConvertYearStringToVector(years_post_int_ThisRow)
			IF_Mean_ThisRow			= IF_Data_Raw_thisDisease$IF_Mean[Row]

			Gender_ThisRow
			Ages_ThisRow
			years_post_int_ThisRow

			# find relevant indices
			IndicesOfProcessedData 	= which(
					Processed_IFData_thisDisease$Gender 			== 		Gender_ThisRow 			&
							Processed_IFData_thisDisease$Age 			%in% 	Ages_ThisRow 			&
							Processed_IFData_thisDisease$YearPostInt 	%in% 	years_post_int_ThisRow 	)

			# now populate processed data with IF in relevant indices
			Processed_IFData_thisDisease$IF_Mean[IndicesOfProcessedData] = IF_Mean_ThisRow
		}

		# create directory for this disease, and this scenario, for this risk factor.
		for (Disease_HGPS_Data in DiseasesHGPS_Data)
		{
			Dir = file.path(getwd(), Disease_HGPS_Data, "PIF", RiskFactor, paste0("Scenario", ScenarioNumber))
			dir.create(Dir, recursive = T)

			# write Processed data to relevant disease folder.
			write.table(Processed_IFData_thisDisease,
					file = file.path(Dir, paste0("IF", COUNTRY_CODE, ".csv")),
					row.names = F, col.names = T, quote = F, sep = ",")
		}
		
		rm(DiseasesHGPS_Data, Disease_HGPS_Data)
	}
}
