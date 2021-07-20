# Load Data ----

# load("~/psychiatry/.RData")

# PsychArticles_Affiliation_trial <- PsychArticles_Affiliation[1:1000,]

# esearch -db pubmed -query "psychiatry[Affiliation]" -datetype PDAT -mindate 1999 -maxdate 2000 | efetch -db pubmed -format xml > ~/psychiatry/data/pubmed/trial_data2.xml

PsychArticles_Affiliation2 <- readRDS("~/psychiatry/data/pubmed/PsychArticles_Affiliation2.RDS")


PsychArticles_Affiliation2 <- PsychArticles_Affiliation2[1:1000,]

View(head(PsychArticles_Affiliation2))

# SunilVeeravalli/Extract_Pubmed_Affiliations ----

# https://github.com/SunilVeeravalli/Extract_Pubmed_Affiliations


# PubmedAffiliations_Packages.R ----

# packages to be installed and attached
# The following packages are to be installed
# tidyverse
# maps
# XML

# Checking and installing "tidyverse" package

if(length(grep("^tidyverse$", installed.packages(), ignore.case = T))==0) {
  install.packages("tidyverse")
  library(tidyverse)
} else {
  library(tidyverse)
}

# checking and installing "maps" package

if (length(grep("^maps$", installed.packages(), ignore.case = T)) == 0) {
  install.packages("maps")
  library(maps)
} else {
  library(maps)
}

# checking and installing "XML"package

if (length(grep("^XML$", installed.packages(), ignore.case = T)) == 0) {
  install.packages("XML")
  library(XML)
} else {
  library(XML)
}

# checking and installing "tcltk"package

if (length(grep("^tcltk$", installed.packages(), ignore.case = T)) == 0) {
  install.packages("tcltk")
  library(tcltk)
} else {
  library(tcltk)
}



# PubmedAffiliations_Functions.R ----


# Following are the functions used in Pubmed Affiliations project


# Split the text at ","

coma <- function(target){
  temp <- str_split(target, ",")
  str_trim(temp[[1]], side="both")
}


# Function to remove anything after ";"

semicolon <- function(target){
  temp <- str_locate(target, ";")[1,1]
  temp <- str_sub(target, 1, temp-1)
  str_trim(temp, side = "both")
}


# Function to remove email ids

email <- function(target){
  firstAt <- str_locate(target, "@")[1,1]
  allSpaces <- str_locate_all(target, " ")[[1]][,1]
  spacePrecedingFirstAt <- max(allSpaces[allSpaces<firstAt])
  temp <- str_sub(target, 1, spacePrecedingFirstAt)
  str_trim(temp, side = "both")
}


# Function to remove any numbers (0-9), symbols (-/\\-'\"&()), periods (.) in the text

numbers <- function(target){
  temp <- str_remove_all(target, "[0-9.-/\\-'\"&()]")
  str_trim(temp, side = "both")
}


# To view the summary of the xml file with info on nodes, attributes and much more.

# crossCheckOutput <- function(FileLocation) {
#   dataSummary <- xmlElementSummary(url = FileLocation)
#   noOfArticles <-
#     dataSummary$nodeCounts[names(dataSummary$nodeCounts) == "PubmedArticle"]
#   noOfAffiliations <-
#     dataSummary$nodeCounts[names(dataSummary$nodeCounts) == "Affiliation"]
#   data.frame(
#     "Original_File" = c(noOfArticles, noOfAffiliations, noOfAffiliations),
#     "After_Mining" = c(length(PMID), length(Affiliations), c(length(country))),
#     row.names = c("Articles", "Affiliations", "Countries")
#   )
# }


mining <- function(temp1) {

  name <- c()
  if (str_detect(temp1, "[,]")) {
    temp2 <- coma(temp1)
  } else {
    temp2 <- temp1
  }
  temp2

  for (c in length(temp2):1)
  {
    temp3 <- temp2[c]
    temp3
    if(is.null(temp3))
    {
      break
    }
    if (is.character(temp3) & !is.na(temp3)) {
      if (str_detect(temp3, "[;]")) {
        temp3 <- semicolon(temp3)
      } else {
        temp3 <- temp3
      }
    }
    temp3
    if (is.character(temp3) & !is.na(temp3)) {
      if (str_detect(temp3, "[@]")) {
        temp3 <- email(temp3)
      } else {
        temp3 <- temp3
      }
    }
    temp3
    if (is.character(temp3) & !is.na(temp3)) {
      if (str_detect(temp3, "[0-9.-/\\-'\"&()]")) {
        temp3 <- numbers(temp3)
      } else {
        temp3 <- temp3
      }
    }
    temp3
    if(is.character(temp3) & !is.na(temp3)){
      temp3 <- str_split(temp3, " ")[[1]]
    } else {
      temp3 <- temp3
    }
    temp3

    if(is.na(temp3) | is.null(temp3)){
      next
    }

    for(d in length(temp3):1){
      if (length(cityCountry$Country[cityCountry$Country == temp3[d]]) > 0)
      {
        name <-
          c(name, cityCountry$Country_Alias[cityCountry$Country == temp3[d]][1])
        temp2 <- NULL
        break
      } else if (length(cityCountry$City[cityCountry$City == temp3[d]]) > 0) {
        name <-
          c(name, na.omit(cityCountry$Country_Alias[cityCountry$City == temp3[d]]))

      } else {
        name <- c(name, NULL)
      }
    }
  }
  name
}



# Dataframe of country and city names can be found in package "maps". I added few more.

# city list ----

cityCountry <-
  data.frame(
    City = world.cities$name,
    Country = world.cities$country.etc,
    Country_Alias = world.cities$country.etc
  )
cityCountry$City <- as.character(cityCountry$City)
cityCountry$Country <- as.character(cityCountry$Country)
cityCountry$Country_Alias <-
  as.character(cityCountry$Country_Alias)
cityCountry <-
  rbind(
    cityCountry,
    c(NA, "United Kingdom", "UK"),
    c(NA, "Kingdom", "UK"),
    c(NA, "United States", "USA"),
    c(NA, "United States of America", "USA"),
    c(NA, "States", "USA"),
    c(NA, "America", "USA"),
    c(NA, "ROC", "China"),
    c(NA, "Republic of China", "China"),
    c(NA, "People's Republic of China", "China"),
    c(NA, "PR China", "China"),
    c(NA, "Wallis", "Wallis and Futuna"),
    c(NA, "Futuna", "Wallis and Futuna"),
    c(NA, "Republic of Korea", "South Korea"),
    c(NA, "North Korea", "North Korea"),
    c(NA, "Arabia", "Saudi Arabia"),
    c(NA, "UAE", "United Arab Emirates"),
    c(NA, "Arab", "United Arab Emirates"),
    c(NA, "Connecticut", "USA"),
    c(NA, "Sahara", "Western Sahara"),
    c(NA, "Vatican", "Vatican City"),
    c(NA, "Turks", "Turks and Caicos"),
    c(NA, "Caicos", "Turks and Caicos"),
    c(NA, "Trinidad", "Trinidad and Tobago"),
    c(NA, "Tobago", "Trinidad and Tobago"),
    c(NA, "Svalbard", "Svalbard and Jan Mayen"),
    c(NA, "Mayen", "Svalbard and Jan Mayen"),
    c(NA, "Lanka", "Sri Lanka"),
    c(NA, "Africa", "South Africa"),
    c(NA, "Solomon", "Solomon Islands"),
    c(NA, "Sierra", "Sierra Leone"),
    c(NA, "Leone", "Sierra Leone"),
    c(NA, "Serbia", "Serbia and Montenegro"),
    c(NA, "Montenegro", "Serbia and Montenegro"),
    c(NA, "Marino", "San Marino"),
    c(NA, "Vincent", "Saint Vincent and The Grenadines"),
    c(NA, "Grenadines", "Saint Vincent and The Grenadines"),
    c(NA, "Lucia", "Saint Lucia"),
    c(NA, "Pierre", "Saint Pierre and Miquelon"),
    c(NA, "Miquelon", "Saint Pierre and Miquelon"),
    c(NA, "Kitts", "Saint Kitts and Nevis"),
    c(NA, "Nevis", "Saint Kitts and Nevis"),
    c(NA, "Helena", "Saint Helena"),
    c(NA, "Rico", "Puerto Rico"),
    c(NA, "Puerto", "Puerto Rico"),
    c(NA, "Papua", "Papua New Guinea"),
    c(NA, "Guinea", "Papua New Guinea"),
    c(NA, "Mariana", "Northern Mariana Islands"),
    c(NA, "Zealand", "New Zealand"),
    c(NA, "Norfolk", "Norfolk Island"),
    c(NA, "Antilles", "Netherlands Antilles"),
    c(NA, "Marshall", "Marshall Islands"),
    c(NA, "Ivory", "Ivory Coast"),
    c(NA, "Man", "Isle of Man"),
    c(NA, "Guernsey", "Guernsey and Alderney"),
    c(NA, "Alderney", "Guernsey and Alderney"),
    c(NA, "Polynesia", "French Polynesia"),
    c(NA, "Guiana", "French Guiana"),
    c(NA, "Falkland", "Falkland Islands"),
    c(NA, "Faroe", "Faroe Islands"),
    c(NA, "Guinea", "Equatorial Guinea"),
    c(NA, "Salvador", "El Salvador"),
    c(NA, "Easter", "Easter Island"),
    c(NA, "Timor", "East Timor"),
    c(NA, "Dominican", "Dominican Republic"),
    c(NA, "Czech", "Czech Republic"),
    c(NA, "Rica", "Costa Rica"),
    c(NA, "Cook", "Cook Islands"),
    c(NA, "Congo", "Congo Democratic Republic"),
    c(NA, "African", "Central African Republic"),
    c(NA, "Cayman", "Cayman Islands"),
    c(NA, "Verde", "Cape Verde"),
    c(NA, "Canary", "Canary Islands"),
    c(NA, "Burkina", "Burkina Faso"),
    c(NA, "Bosnia", "Bosnia and Herzegovina"),
    c(NA, "Herzegovina", "Bosnia and Herzegovina"),
    c(NA, "Antigua", "Antigua and Barbuda"),
    c(NA, "Barbuda", "Antigua and Barbuda"),
    c(NA, "American", "American Samoa"),
    c(NA, "Samoa", "American Samoa")
    # British virgin island
    # us virgin island
    # korea north
    # korea south
  )

# To get more lists for city and country details, try the following:
# http://www.geonames.org/export/
# https://www.maxmind.com/en/free-world-cities-database


# Finding out number of Affiliations Per PMID which will be used in creating the FinalTable
# If we look at the pattern of the elements in xml file, to reach the Authors, we should pass PubmedArticle>MedlineCitation>Article>AuthorList. So, we need to find the number of authors in the AuthorList for each pubmedArticle.
no.ofAffiliationsPerPMID <- function() {
  temp <-
    getNodeSet(data,
               "//PubmedArticle/MedlineCitation/Article/AuthorList")
  lengthtemp <- length(temp)
  No.ofAffiliationsPerPMID <- c()
  for (i in 1:lengthtemp) {
    temp1 <- xmlSize(temp[[i]])
    temp2 <- c()
    for (j in 1:temp1) {
      temp2 <- c(temp2, names(getChildrenStrings(temp[[i]][[j]])))
    }
    No.ofAffiliationsPerPMID <-
      c(No.ofAffiliationsPerPMID, sum(as.integer(temp2 == "AffiliationInfo")))
  }
  No.ofAffiliationsPerPMID
}

















# PubmedAffiliations.R ----



# "XML" package is used to work with XML files.

# In .XML files, we see data enclosed between tags as shown below. For example:
# <dataset source="R Project" numRecords="32" name="mtcars">
#     <variables count="11">
#           <variable>cyl</variable>
#           <variable>disp</variable>
#           <variable>hp</variable>
#     </variables>
# </dataset>
#
# In the above example, dataset, variables, variable are called elements. Every element has a closing element with / in the beginning. The data between the opening and closing element is called value. For example, one opening element <variable> and its closing element </variable> has the value cyl in them. Also, elements will also have attributes. For dataset element, the attributes are source, numRecords and name. The attributes fro Variables is count.


# setting the working directory

# No need to set working directory because we created the project file (.RProj file) which will take care of setting the working directory.


# All the functions and packages used in this project are written in a separate file name "PubmedAffiliations_Functions.R" and "PubmedAffiliations_Packages.R" respectively. The source() command will load those functions for usage.
# source(file = "Functions/PubmedAffiliations_Packages.R")
# source(file = "Functions/PubmedAffiliations_Functions.R")


# choose the location of your target xml file when asked
# To show a popup message to user, use the package "tcktl".
# checking and installing "tcltk"package
# if (length(grep("^tcltk$", installed.packages(), ignore.case = T)) == 0) {
#   install.packages("tcltk")
#   library(tcltk)
# } else {
#   library(tcltk)
# }
# tk_messageBox(type = "ok", message = "In the next screen, please select the location of XML file that needs to be analyzed", caption = "Message")
# FileLocation <- file.choose()

# Pulling the xml file to R
# data <- xmlInternalTreeParse(file = FileLocation)

# We want to create a table with Pubmed ID (PMID) and all the affilations for each PMID
# PMID <- xpathSApply(data, "//PubmedArticle/MedlineCitation/PMID", xmlValue)

# Affiliations <- xpathSApply(data, "//PubmedArticle/MedlineCitation/Article/AuthorList/Author/AffiliationInfo/Affiliation", xmlValue)

Affiliations <- PsychArticles_Affiliation2$Affiliation



# To pull country name out of Affiliation
# We need to pull the Country out of Affiliations
# For this "stringr" is being used that has already been installed with "tidyverse" installation

country <- data.frame()

for (a in 1:length(Affiliations)) {
  x <- c()
  y <- c()
  if (str_detect(Affiliations[a], "[;]"))
  {
    temp <- str_split(Affiliations[a], "[;]")[[1]]
    temp

    for (b in 1:length(temp))
    {
      temp1 <- temp[b]
      temp1
      x <- mining(temp1)
      x
      if(length(x[duplicated(x)])>0)
      {
        x <- unique(x[duplicated(x)])
        x
      } else {
        if(length(x)>0){
          x <- x
        } else {
          x <- NA
        }
      }
      y <- c(y, x)
      y
    }
  } else {
    temp1 <- Affiliations[a]
    temp1
    x <- mining(temp1)
    x
    if(length(x[duplicated(x)])>0)
    {
      x <- unique(x[duplicated(x)])
      x
    } else {
      if(length(x)>0){
        x <- x
      } else {
        x <- NA
      }
    }
    y <- c(y, x)
    y

  }
  for(i in 1:length(y)){
    country[a,i] <- y[i]
  }
}

colnames(country) <- c(paste("Country", seq(1:ncol(country))))

View(country)

country$PMID <- PsychArticles_Affiliation2$PMID


PsychArticles_Affiliation2 <- PsychArticles_Affiliation2 %>%
  dplyr::left_join(country, by = "PMID")


# To find if extracting country name failed in some Affiliations
# which(is.na(country))
# Affiliations[which(is.na(country))]


# Check if there are errors while extracting the country name from Affiliations. If there are errors, then manually edit or write more functions if the error is seen with many Affiliations
# unique(country)


# Find out if the number of Pumbed Articles, Affiliations, Country you got while mining is equal to the actual number in the xml file. This is throwing an error if the xml file is huge.
# crossCheckTable <- crossCheckOutput(FileLocation)
# crossCheckTable


# Creating a final table with PMID, Affiliation, country Name
# The length of PMID will be different to the length of Affiliations because one PMID contains many Affiliations. So, find out the number of Affiliations each PMID has.

# affiliationsPerPMID <- no.ofAffiliationsPerPMID()
# affiliationsPerPMID

# Now we create a table with PMID column and respective Affiliations column
# FinalTable1 <- data.frame(PMID = rep(PMID, affiliationsPerPMID), Affiliations = Affiliations, Country = country)


# FinalTable1 <- unique(FinalTable1)
# View(FinalTable1)

# The following table gives the International collaboration information
# FinalTable2 <- unique(FinalTable1[,c(1,3:length(FinalTable1))])
# View(FinalTable2)

# sorting FinalTable2 to have one PMID per row
# lenPMID <- length(PMID)
# lenFinalTable2 <- length(FinalTable2)
# FinalTable3 <- data.frame()
# for (i in 1:lenPMID) {
#   temp <- FinalTable2[FinalTable2$PMID == PMID[i], 2:lenFinalTable2]
#   temp <- stack(temp)
#   temp <- unique(na.omit(temp$values))
#   lentemp <- length(temp)
#   if (lentemp > 0) {
#     for (j in 1:lentemp) {
#       FinalTable3[i, 1] <- PMID[i]
#       FinalTable3[i, j + 1] <- temp[j]
#     }
#   } else {
#     FinalTable3[i, 1] <- PMID[i]
#     FinalTable3[i, j + 1] <- NA
#   }
# }
# rm(i, j, lenPMID, lenFinalTable2, lentemp)
# names(FinalTable3) <- c("PMID", paste("Country_", 1:(length(FinalTable3)-1), sep = ""))
# View(FinalTable3)

# Save the FinalTable3 in csv format in the folder downloaded by the user
# tk_messageBox(type = "ok", message = "The output is saved to the downloaded folder as Output.csv", caption = "Message")
# write.csv(FinalTable3, file = "Output.csv", na = "", row.names = FALSE)


# things to do
# add a * in the output where two cities have same name but are in different countries
# sort south korea, north korea, us virgin island, british virgin island filtering,
# add date to the table so that we can find in a particular year, how the collaborations were














# Mycodes ----

# world_cities <- maps::world.cities %>%
#   dplyr::pull(name) %>%
#   stringr::str_c(., collapse = "|")


# PsychArticles_Affiliation2 <- PsychArticles_Affiliation

# PsychArticles_Affiliation2$world_cities <- sapply(
#   stringr::str_extract_all(
#     string = PsychArticles_Affiliation$Affiliation,
#     pattern = world_cities
#   ),
#   toString
# )

# saveRDS(
#   object = PsychArticles_Affiliation,
#   file = paste0("data/pubmed/PsychArticles_Affiliation3-", as.character(Sys.time()), ".RDS")
# )
