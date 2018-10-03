# load all R packages
library(reshape2) 
library(data.table)
library(tidyverse)

library(ggrepel)
library(scales)
library(migest)
library(RColorBrewer)
library(circlize)
library(choroplethr) # for state map
library(choroplethrMaps)
library(likert) # for likert plot
library(shiny)
library(shinydashboard)
library(plotly)
library(sunburstR)
library(gridExtra)

# read in data
dataGroup <- read.csv("sourceData_WSU.csv")
# count of alumni
alnCount = nrow(dataGroup)
# get intitute name
nameInstitute <- dataGroup$institute[1]


# convert all factor columns to character columns 
# https://stackoverflow.com/questions/47633241/convert-all-factor-columns-to-character-in-a-data-frame-without-affecting-non-fa
dataGroup <- dataGroup %>% mutate_if(is.factor, as.character)

### convert long entry names to short entry names for data visualization
# career_type
dataGroup$career_type[dataGroup$career_type == 'Further Training or Education'] <- "Further Training"
dataGroup$career_type[dataGroup$career_type == 'Not Related to Science'] <- "Non Scientific"
# job_function
dataGroup$job_function[dataGroup$job_function == 'Faculty Member - Tenure/Tenure Track'] <- "Faculty - Tenure"
dataGroup$job_function[dataGroup$job_function == 'Postdoctoral (Scientific Research)'] <- "Postdoctoral - Research"
dataGroup$job_function[dataGroup$job_function == 'Research Staff or Technical Director'] <- "Research Staff"
dataGroup$job_function[dataGroup$job_function == 'Business Development, Consulting, and Strategic Alliances'] <- "Business Dev."
dataGroup$job_function[dataGroup$job_function == 'Clinical Research Management'] <- "Clinical Res. Mgmt."
dataGroup$job_function[dataGroup$job_function == 'Technical Support and Product Development'] <- "Technical Support"
dataGroup$job_function[dataGroup$job_function == 'Data Science, Analytics, and Software Engineering'] <- "Data Science"
dataGroup$job_function[dataGroup$job_function == 'Science Writing and Communication'] <- "Science Writing"
dataGroup$job_function[dataGroup$job_function == 'Faculty Member - Nontenure Track'] <- "Faculty - Nontenure"
dataGroup$job_function[dataGroup$job_function == 'Intellectual Property and Law'] <- "Intellectual Property"
dataGroup$job_function[dataGroup$job_function == 'Function that is Not Directly Related to Science'] <- "Non-Scientific"
dataGroup$job_function[dataGroup$job_function == 'Science Policy and Government Affairs'] <- "Science Policy"
dataGroup$job_function[dataGroup$job_function == 'Science Education and Outreach'] <- "Science Education"
# school
dataGroup$school[dataGroup$school == 'Liberal Arts & Sciences'] <- "Arts & Sciences"
dataGroup$school[dataGroup$school == 'School of Medicine'] <- "Medicine"
dataGroup$school[dataGroup$school == 'College of Science'] <- "Science"
dataGroup$school[dataGroup$school == 'Pharmacy and Health Sciences'] <- "Pharmacy"
dataGroup$school[dataGroup$school == 'College of Engineering'] <- "Engineering"
dataGroup$school[dataGroup$school == 'Unknown'] <- "Unknown"
dataGroup$school[dataGroup$school == 'College of Education'] <- "Education"
dataGroup$school[dataGroup$school == 'School of Social Work'] <- "Social Work"
# department
dataGroup$department[dataGroup$department == 'Biological Sciences'] <- "Biol. Sci."
dataGroup$department[dataGroup$department == 'Biochemistry & Molecular Bio'] <- "Biochem. & Mol. Biol."
dataGroup$department[dataGroup$department == 'Immunology & Microbiology'] <- "Immunol. & Microbiol."
dataGroup$department[dataGroup$department == 'Nutrition & Food Science'] <- "Nutrition & Food Sci."
dataGroup$department[dataGroup$department == 'Biomedical Engineering'] <- "Biomed. Engineering"
dataGroup$department[dataGroup$department == 'Physics & Astronomy'] <- "Physics & Astronomy"
dataGroup$department[dataGroup$department == 'Mechanical Engineering'] <- "Mech. Engineering"
dataGroup$department[dataGroup$department == 'Theoretical & Behavioral Foundations'] <- "Behav. Sci."
dataGroup$department[dataGroup$department == 'Health Care Sciences'] <- "Health Care Sci."
# major
dataGroup$major[dataGroup$major == 'Biological Sciences'] <- "Biol. Sci."
dataGroup$major[dataGroup$major == 'Cancer Biology'] <- "Cancer Biol."
dataGroup$major[dataGroup$major == 'Immunology and Microbiology'] <- "Immunol. & Microbiol."
dataGroup$major[dataGroup$major == 'Pharmaceutical Sciences'] <- "Pharm Sci."
dataGroup$major[dataGroup$major == 'Nutrition and Food Science'] <- "Nutrition & Food Sci."
dataGroup$major[dataGroup$major == 'Biomedical Engineering'] <- "Biomed. Engineering"
dataGroup$major[dataGroup$major == 'Anatomy & Cell Biology'] <- "Anatomy & Cell Biol."
dataGroup$major[dataGroup$major == 'Biochemistry & Molecular Biology'] <- "Biochem. & Mol. Biol."
dataGroup$major[dataGroup$major == 'Medical Physics'] <- "Medical Physics"
dataGroup$major[dataGroup$major == 'Cellular and Clinical Neurobio'] <- "Cell and Clinic. Neurobio"
dataGroup$major[dataGroup$major == 'Mechanical Engineering'] <- "Mech. Engineering"
dataGroup$major[dataGroup$major == 'Educational Psychology'] <- "Educ. Psychology"
# race
dataGroup$race[dataGroup$race == 'Black or African American'] <- "Afr. American"
dataGroup$race[dataGroup$race == 'Race Not Reported'] <- "Race - Unknown"
dataGroup$race[dataGroup$race == 'American Indian or Alaskan Native'] <- "Am. Indian"
dataGroup$race[dataGroup$race == 'American Indian or Alaskan Native; White'] <- "Am. Indian; White"

# combine Permanent Resident with Citizen --> 'US'
dataGroup$citizenship[dataGroup$citizenship == 'Permanent Resident'] <- "US"
dataGroup$citizenship[dataGroup$citizenship == 'Citizen'] <- "US"
# convert Non-Citizen --> 'International'
dataGroup$citizenship[dataGroup$citizenship == 'Non-Citizen'] <- "International"

# Convert job functions which are not in top 10 to 'REST COMBINED' for better data visualization
tblJobFunc <- table(dataGroup[,"job_function"])
nameRestFunc <- names(tail(sort(tblJobFunc, decreasing = TRUE), -10))
dataGroup$job_function[dataGroup$job_function %in% nameRestFunc] <- "REST COMBINED"

# convert character columns back to factor columns 
dataGroup <- dataGroup %>% mutate_if(is.character, as.factor)

# color definition for Enployment Sector, Career Type, and Job Function
colorJSect = c("Academia"="#94363a",
               "For-Profit"="#f19493",
               "Government"="#b15426",
               "Nonprofit"="#f285a8",
               "Other"="#933761",
               "Unknown"="#da9a54")

colorJType = c("Further Training"="#24594d",
               "Non Scientific" ="#8acfbf",
               "Primarily Research"="#255a2d",
               "Primarily Teaching"="#23787a",
               "Science-Related"="#6cc06d",
               "Unknown"="#89c658")

colorJFunc = c("Administration"="#225f7b",
               "Business Dev."="#a792c5",
               "Clinical Res. Mgmt."="#8b9fd1",
               "Faculty - Tenure"="#243e7d",
               "Group Leader (Research)"="#747fbe",
               "Healthcare Provider"="#6b417d",
               "Lecturer/Instructor"="#23787a",
               "Postdoctoral - Research"="#47427e",
               "Research Staff"="#85357a",
               "Technical Support"="#c06aaa",
               "REST COMBINED"="#4bc4d2")



# Keep countries with large number of Alumnis:
#   United States, China, India, Sri Lanka, Canada,
#   France, Nepal, South Korea, Saudi Arabia, Taiwan 
# Group the rest countries to continents:
#   Africa, Asia, Europe, North America, Oceania, South America   
cnvtCountry <- fread("country_region.csv")

convertCountry <- function(dfin, countryTable) {
  df <- data.table(dfin)
  df$country_origin <- as.character(df$country_origin)
  df$job_country <- as.character(df$job_country)
  
  setkey(countryTable,Country)
  setkey(df,country_origin)
  df$country_origin <- df[countryTable, nomatch=0]$Region
  
  setkey(df,job_country)
  df$job_country <- df[countryTable, nomatch=0]$Region
  
  df$country_origin <- factor(df$country_origin)
  df$job_country <- factor(df$job_country)
  return(df)
}

mgrCountry <- convertCountry(dataGroup,cnvtCountry)
mgrCountryGroup <- mgrCountry %>% group_by(years,country_origin,job_country) %>% summarise (n = n())
mgrCountryAll <- mgrCountry %>% group_by(country_origin,job_country) %>% summarise (n = n())


# get the time period data for UI
timePeriods <- unique(sort(dataGroup$years))
timePeriods <- as.character(timePeriods)
timeYrs <- unlist(strsplit(timePeriods,'-'))
timeYrs <- as.numeric(timeYrs)
minYrs <- min(timeYrs)
maxYrs <- max(timeYrs)
totalYrs <- paste0(minYrs,'-',maxYrs)
trendYrs <- paste0(timePeriods, collapse = ",")

# demographical percentage count
tGender = table(dataGroup[,"gender"])
tMalePct = round(1 - as.numeric(tGender["Female"]) / alnCount, 3) * 100
tCitizen = table(dataGroup[,"citizenship"])
tCitizenPct = round(1- as.numeric(tCitizen["International"]) / alnCount, 3) * 100

# general career count
tGnrSector = table(dataGroup[,"job_sector"])
tGnrSectorPct = round(tGnrSector / alnCount, 3) * 100
tGnrType = table(dataGroup[,"career_type"])
tGnrTypePct = round(tGnrType / alnCount, 3) * 100
tGnrFunction = table(dataGroup[,"job_function"])
tGnrFuncPct = round(tGnrFunction / alnCount, 3) * 100


# training time
cntAvgTime = round(mean(dataGroup$train_time),1)
cntAcdTime = round(mean(dataGroup$train_time[dataGroup$job_sector == "Academia"]),1)
cntGvnTime = round(mean(dataGroup$train_time[dataGroup$job_sector == "Government"]),1)
cntPrfTime = round(mean(dataGroup$train_time[dataGroup$job_sector == "For-Profit"]),1)

# use dplyr to summarize gender information
# http://stackoverflow.com/questions/24576515/relative-frequencies-proportions-with-dplyr
gend4All <- dataGroup %>% group_by(gender) %>% summarise (cnt = n()) %>% spread(gender, cnt, fill=0)
gend4Sect <- dataGroup %>% group_by(job_sector, gender) %>% summarise (cnt = n()) %>% spread(gender, cnt, fill=0)
gend4Type <- dataGroup %>% group_by(career_type, gender) %>% summarise (cnt = n()) %>% spread(gender, cnt, fill=0)
gend4Func <- dataGroup %>% group_by(job_function, gender) %>% summarise (cnt = n()) %>% spread(gender, cnt, fill=0)
# gend4Schl <- dataGroup %>% group_by(school, gender) %>% summarise (cnt = n()) %>% spread(gender, cnt, fill=0)

gendYrAll <- dataGroup %>% group_by(years,gender) %>% summarise (cnt = n()) %>% spread(gender, cnt, fill=0)
gendYrSect <- dataGroup %>% group_by(years,job_sector, gender) %>% summarise (cnt = n()) %>% spread(gender, cnt, fill=0)
gendYrType <- dataGroup %>% group_by(years,career_type, gender) %>% summarise (cnt = n()) %>% spread(gender, cnt, fill=0)
gendYrFunc <- dataGroup %>% group_by(years,job_function, gender) %>% summarise (cnt = n()) %>% spread(gender, cnt, fill=0)
# gendYrSchl <- dataGroup %>% group_by(years,school, gender) %>% summarise (cnt = n()) %>% spread(gender, cnt, fill=0)

# use dplyr to summarize citizenship information
# http://stackoverflow.com/questions/24576515/relative-frequencies-proportions-with-dplyr
citi4All <- dataGroup %>% group_by(citizenship) %>% summarise (cnt = n()) %>% spread(citizenship, cnt, fill=0)
citi4Sect <- dataGroup %>% group_by(job_sector, citizenship) %>% summarise (cnt = n()) %>% spread(citizenship, cnt, fill=0)
citi4Type <- dataGroup %>% group_by(career_type, citizenship) %>% summarise (cnt = n()) %>% spread(citizenship, cnt, fill=0)
citi4Func <- dataGroup %>% group_by(job_function, citizenship) %>% summarise (cnt = n()) %>% spread(citizenship, cnt, fill=0)
# citi4Schl <- dataGroup %>% group_by(school, citizenship) %>% summarise (cnt = n()) %>% spread(citizenship, cnt, fill=0)

citiYrAll <- dataGroup %>% group_by(years,citizenship) %>% summarise (cnt = n()) %>% spread(citizenship, cnt, fill=0)
citiYrSect <- dataGroup %>% group_by(years,job_sector, citizenship) %>% summarise (cnt = n()) %>% spread(citizenship, cnt, fill=0)
citiYrType <- dataGroup %>% group_by(years,career_type, citizenship) %>% summarise (cnt = n()) %>% spread(citizenship, cnt, fill=0)
citiYrFunc <- dataGroup %>% group_by(years,job_function, citizenship) %>% summarise (cnt = n()) %>% spread(citizenship, cnt, fill=0)
# citiYrSchl <- dataGroup %>% group_by(years,school, citizenship) %>% summarise (cnt = n()) %>% spread(citizenship, cnt, fill=0)


# make the matrix for circular plotting of Alumni migration
makeCircMD <- function(idct) {
  # remove 
  dct <- idct
  dct <- dct[!(dct$job_country == 'Unknown'),]
  dct <- dct[!(dct$country_origin == 'Unknown'),]
  
  uniTop <- unique(c(as.character(dct$country_origin), as.character(dct$job_country)))
  mtrx <- matrix(0L,length(uniTop), length(uniTop))
  dimnames(mtrx) <- list(orig = uniTop, dest=uniTop)
  
  # map count to m matrix
  for(i in 1:nrow(dct)) {
    rw <- dct[i,]
    c <- as.character(rw$country_origin)
    j <- as.character(rw$job_country)
    mtrx[c, j] = rw$n
  }
  
  return(mtrx)
}


# job positions by state
dataUSA <- dataGroup[dataGroup$job_country == 'United States',]
dataUSA$job_state <- factor(tolower(dataUSA$job_state),exclude=NULL)
stateNM <- tolower(state.name)
misSTNM <- stateNM[!(stateNM %in% dataUSA$job_state)]
levels(dataUSA$job_state) <- c(levels(dataUSA$job_state),misSTNM)

dfStateAll <- dataUSA %>% group_by(job_state) %>% summarise (value = n()) %>% complete(job_state, fill = list(value = 0))
colnames(dfStateAll)[colnames(dfStateAll)=="job_state"] <- "region"
dfStateAll$value <- as.integer(dfStateAll$value)
dfStateYrs <- dataUSA %>% group_by(years,job_state) %>% summarise (value = n()) %>% complete(job_state, fill = list(value = 0))
colnames(dfStateYrs)[colnames(dfStateYrs)=="job_state"] <- "region"
dfStateYrs$value <- as.integer(dfStateYrs$value)

# summary job sector (where did they go) information
dfJobSectAll <- dataGroup %>% group_by(job_sector) %>% summarise (cnt = n()) %>% mutate(freq=cnt/sum(cnt))
colnames(dfJobSectAll) <- c('Sector','Count','Alumni')
dfJobSectYrs <- dataGroup %>% group_by(years, job_sector) %>% summarise (cnt = n()) %>% mutate(freq=cnt/sum(cnt))
colnames(dfJobSectYrs) <- c('years','Sector','Count','Alumni')

# summary job type (What is the level of their position) information
dfJobTypeAll <- dataGroup %>% group_by(career_type) %>% summarise (cnt = n()) %>% mutate(freq=cnt/sum(cnt))
colnames(dfJobTypeAll) <- c('Type','Count','Alumni')
dfJobTypeYrs <- dataGroup %>% group_by(years, career_type) %>% summarise (cnt = n()) %>% mutate(freq=cnt/sum(cnt))
colnames(dfJobTypeYrs) <- c('years','Type','Count','Alumni')

# summary job specificity (what are they doing) information
dfJobFuncAll <- dataGroup %>% group_by(job_function) %>% summarise (cnt = n()) %>% mutate(freq=cnt/sum(cnt))
colnames(dfJobFuncAll) <- c('Function','Count','Alumni')
dfJobFuncYrs <- dataGroup %>% group_by(years, job_function) %>% summarise (cnt = n()) %>% mutate(freq=cnt/sum(cnt))
colnames(dfJobFuncYrs) <- c('years','Function','Count','Alumni')

# average time in WSU by job sector
dfTimeSectAll <- dataGroup %>% group_by(job_sector) %>% summarise(avg_time=round(mean(train_time),digits = 1), min_time=min(train_time), max_time=max(train_time), num_data=n())
dfTimeSectAll$job_sector <- as.character(dfTimeSectAll$job_sector)
dfTimeSectAll <- bind_rows(dfTimeSectAll, dataGroup %>% summarise(avg_time=round(mean(train_time),digits = 1), min_time=min(train_time), max_time=max(train_time), num_data=n()) %>% mutate(job_sector="All sectors"))
dfTimeSectYrs <- dataGroup %>% group_by(years, job_sector) %>% summarise(avg_time=round(mean(train_time),digits = 1), min_time=min(train_time), max_time=max(train_time), num_data=n())
# (10/06/2016): change from rbind to bind_rows (dplyr) to fix the error of combine two data frames into list
#   http://stackoverflow.com/questions/3402371/combine-two-data-frames-by-rows-rbind-when-they-have-different-sets-of-columns
# dfTimeSectYrs <- rbind(dfTimeSectYrs, dataGroup %>% group_by(years) %>% summarise(avg_time=round(mean(train_time),digits = 1), min_time=min(train_time), max_time=max(train_time), num_data=n()) %>% mutate(job_sector="All sectors"))
dfTimeSectYrs$job_sector <- as.character(dfTimeSectYrs$job_sector)
dfTimeSectYrs <- bind_rows(dfTimeSectYrs, dataGroup %>% group_by(years) %>% summarise(avg_time=round(mean(train_time),digits = 1), min_time=min(train_time), max_time=max(train_time), num_data=n()) %>% mutate(job_sector="All sectors"))

# average time in WSU by career type
dfTimeTypeAll <- dataGroup %>% group_by(career_type) %>% summarise(avg_time=round(mean(train_time),digits = 1), min_time=min(train_time), max_time=max(train_time), num_data=n())
dfTimeTypeAll$career_type <- as.character(dfTimeTypeAll$career_type)
dfTimeTypeAll <- bind_rows(dfTimeTypeAll, dataGroup %>% summarise(avg_time=round(mean(train_time),digits = 1), min_time=min(train_time), max_time=max(train_time), num_data=n()) %>% mutate(career_type="All types"))
dfTimeTypeYrs <- dataGroup %>% group_by(years, career_type) %>% summarise(avg_time=round(mean(train_time),digits = 1), min_time=min(train_time), max_time=max(train_time), num_data=n())
dfTimeTypeYrs$career_type <- as.character(dfTimeTypeYrs$career_type)
dfTimeTypeYrs <- bind_rows(dfTimeTypeYrs, dataGroup %>% group_by(years) %>% summarise(avg_time=round(mean(train_time),digits = 1), min_time=min(train_time), max_time=max(train_time), num_data=n()) %>% mutate(career_type="All types"))

# average time in WSU by job job_function
dfTimeFuncAll <- dataGroup %>% group_by(job_function) %>% summarise(avg_time=round(mean(train_time),digits = 1), min_time=min(train_time), max_time=max(train_time), num_data=n())
dfTimeFuncAll$job_function <- as.character(dfTimeFuncAll$job_function)
dfTimeFuncAll <- bind_rows(dfTimeFuncAll, dataGroup %>% summarise(avg_time=round(mean(train_time),digits = 1), min_time=min(train_time), max_time=max(train_time), num_data=n()) %>% mutate(job_function="All job_function"))
dfTimeFuncYrs <- dataGroup %>% group_by(years, job_function) %>% summarise(avg_time=round(mean(train_time),digits = 1), min_time=min(train_time), max_time=max(train_time), num_data=n())
dfTimeFuncYrs$job_function <- as.character(dfTimeFuncYrs$job_function)
dfTimeFuncYrs <- bind_rows(dfTimeFuncYrs, dataGroup %>% group_by(years) %>% summarise(avg_time=round(mean(train_time),digits = 1), min_time=min(train_time), max_time=max(train_time), num_data=n()) %>% mutate(job_function="All job_function"))

# training time by gender and country
gendTimeAll <- dataGroup %>% group_by(gender) %>% summarise(avg_time=round(mean(train_time),digits = 1), min_time=min(train_time), max_time=max(train_time), num_data=n())
# gendTimeAll$years <- '2000-2014'
gendTimeGrp <- dataGroup %>% group_by(years, gender) %>% summarise(avg_time=round(mean(train_time),digits = 1), min_time=min(train_time), max_time=max(train_time), num_data=n())
gendTimeCtryAll <- mgrCountry %>% group_by(country_origin,gender) %>% summarise(avg_time=round(mean(train_time),digits = 1), min_time=min(train_time), max_time=max(train_time), num_data=n())

# training time by citizenship and country
citiTimeAll <- dataGroup %>% group_by(citizenship) %>% summarise(avg_time=round(mean(train_time),digits = 1), min_time=min(train_time), max_time=max(train_time), num_data=n())
citiTimeGrp <- dataGroup %>% group_by(years, citizenship) %>% summarise(avg_time=round(mean(train_time),digits = 1), min_time=min(train_time), max_time=max(train_time), num_data=n())
# citiTimeCtryAll <- mgrCountry %>% group_by(country_origin,citizenship) %>% summarise(avg_time=round(mean(train_time),digits = 1), min_time=min(train_time), max_time=max(train_time), num_data=n())

# training time by funding and country
fundTimeAll <- dataGroup %>% group_by(funding) %>% summarise(avg_time=round(mean(train_time),digits = 1), min_time=min(train_time), max_time=max(train_time), num_data=n())
fundTimeGrp <- dataGroup %>% group_by(years, funding) %>% summarise(avg_time=round(mean(train_time),digits = 1), min_time=min(train_time), max_time=max(train_time), num_data=n())
# fundTimeCtryAll <- mgrCountry %>% group_by(country_origin,funding) %>% summarise(avg_time=round(mean(train_time),digits = 1), min_time=min(train_time), max_time=max(train_time), num_data=n())

# top 5 countries
ctryCnt <- table(dataGroup[,"country_origin"])
top5Ctry <- names(head(sort(ctryCnt, decreasing = TRUE), 5))

# (7/10/18) top sector, top type, top job_function
sectCnt <- table(dataGroup[,"job_sector"])
setSect <- names(head(sort(sectCnt, decreasing = TRUE), 1))
typeCnt <- table(dataGroup[,"career_type"])
setType <- names(head(sort(typeCnt, decreasing = TRUE), 1))
funcCnt <- table(dataGroup[,"job_function"])
setFunc <- names(head(sort(funcCnt, decreasing = TRUE), 1))


# # average time in WSU by major field
# dfTimeMajrAll <- dataGroup %>% group_by(major) %>% summarise(avg_time=round(mean(train_time),digits = 1), min_time=min(train_time), max_time=max(train_time), num_data=n())
# dfTimeMajrAll$major <- as.character(dfTimeMajrAll$major)
# dfTimeMajrAll <- bind_rows(dfTimeMajrAll, dataGroup %>% summarise(avg_time=round(mean(train_time),digits = 1), min_time=min(train_time), max_time=max(train_time), num_data=n()) %>% mutate(major="All degree"))
# dfTimeMajrYrs <- dataGroup %>% group_by(years, major) %>% summarise(avg_time=round(mean(train_time),digits = 1), min_time=min(train_time), max_time=max(train_time), num_data=n())
# dfTimeMajrYrs$major <- as.character(dfTimeMajrYrs$major)
# dfTimeMajrYrs <- bind_rows(dfTimeMajrYrs, dataGroup %>% group_by(years) %>% summarise(avg_time=round(mean(train_time),digits = 1), min_time=min(train_time), max_time=max(train_time), num_data=n()) %>% mutate(major="All degree"))


# # summary country origin information
# # keep 0 count in summary:
# #   http://stackoverflow.com/questions/25956178
# #   http://stackoverflow.com/questions/16073918
# #   http://stackoverflow.com/questions/22523131
# #     complete(b, fill = list(count_a = 0))
# dcoAll <- dataGroup %>% group_by(country_origin) %>% summarise (Alumni = n()) %>% complete(country_origin, fill = list(Alumni = 0))
# colnames(dcoAll)[colnames(dcoAll)=="country_origin"] <- "Country"
# dcoYrs <- dataGroup %>% group_by(years,country_origin) %>% summarise (Alumni = n()) %>% complete(country_origin, fill = list(Alumni = 0))
# colnames(dcoYrs)[colnames(dcoYrs)=="country_origin"] <- "Country"
# 
# # sort country by alumni number
# ## https://trinkerrstuff.wordpress.com/2013/08/14/how-do-i-re-arrange-ordering-a-plot-revisited/
# srtCTRYori <- factor(dcoYrs$Country, levels=dcoYrs$Country[order(unique(dcoYrs$Alumni))])
# dcoYrs$Country <- srtCTRYori
# srtCTRYori4 <- factor(dcoAll$Country, levels=dcoAll$Country[order(unique(dcoAll$Alumni))])
# dcoAll$Country <- srtCTRYori4
# 
# # summary country job information
# dcjAll <- dataGroup %>% group_by(job_country) %>% summarise (Alumni = n()) %>% complete(job_country, fill = list(Alumni = 0))
# colnames(dcjAll)[colnames(dcjAll)=="job_country"] <- "Country"
# dcjYrs <- dataGroup %>% group_by(years,job_country) %>% summarise (Alumni = n()) %>% complete(job_country, fill = list(Alumni = 0))
# colnames(dcjYrs)[colnames(dcjYrs)=="job_country"] <- "Country"
# 
# # sort country by alumni number
# # http://rstudio-pubs-static.s3.amazonaws.com/7433_4537ea5073dc4162950abb715f513469.html
# #   x$name <- factor(x$name, levels = x$name[order(x$val)])
# srtCTRYjob <- factor(dcjYrs$Country, levels=dcjYrs$Country[order(unique(dcjYrs$Alumni))])
# dcjYrs$Country <- srtCTRYjob
# srtCTRYjob4 <- factor(dcjAll$Country, levels=dcjAll$Country[order(unique(dcjAll$Alumni))])
# dcjAll$Country <- srtCTRYjob4


# # count the major vs job_sector, career_type, job_function
# mjrSectAll <- dataGroup %>% group_by(major, job_sector) %>% summarise (cnt = n()) %>% mutate(freq=cnt/sum(cnt))
# mjrTypeAll <- dataGroup %>% group_by(major, career_type) %>% summarise (cnt = n()) %>% mutate(freq=cnt/sum(cnt))
# mjrFuncAll <- dataGroup %>% group_by(major, job_function) %>% summarise (cnt = n()) %>% mutate(freq=cnt/sum(cnt))
# 
# mjrSectYr <- dataGroup %>% group_by(years,major,job_sector) %>% summarise (cnt = n()) %>% mutate(freq=cnt/sum(cnt))
# mjrTypeYr <- dataGroup %>% group_by(years,major,career_type) %>% summarise (cnt = n()) %>% mutate(freq=cnt/sum(cnt))
# mjrFuncYr <- dataGroup %>% group_by(years,major,job_function) %>% summarise (cnt = n()) %>% mutate(freq=cnt/sum(cnt))
