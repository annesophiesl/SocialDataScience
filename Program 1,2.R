
# Defining which libraries will be used in the program.
library("readr")
library("dplyr")
library("lubridate")
library("ggplot2")
library("stringr")
library("maps")
library("countrycode")

# Getting data

raw_data = read_csv("https://raw.githubusercontent.com/MuseumofModernArt/collection/master/Artworks.csv")
glimpse(raw_data)

# Generating year and month variable

raw_data$Dato_ = substr(raw_data$DateAcquired, start=1, stop=8) 

# Making it into a format that ggplot will read as a data variabel. By setting the date
# to the first in every month, we insure that the data will be group correctly and have 
# the right date format. 

raw_data$Dato_YM = as.Date(gsub(" ","",paste(raw_data$Dato_,"01")))

# We want make sure, that there is only one way painting is written in variable Classification.
# To know which kinds of values there a stored in the varibel Classification we do

View(distinct(select(raw_data, Classification)))

# Checking datatype af DateAcquired to see if it is the rigth datetype

class(raw_data$DateAcquired)

# We only want to have the paintings

only_paintings=filter(raw_data,Classification == "Painting")

# Aggregating stock of paintings

paints_by_DateAcq  = only_paintings %>%
  group_by(Dato_YM) %>%
  summarise(Stock_inc = n()) 

paints_by_DateAcq$cum = cumsum(paints_by_DateAcq$Stock_inc)

#Question 2 - Plotting data - Using geom_line to illustrate how the stock of paintings increases over time. 
p = ggplot(paints_by_DateAcq, aes(x = as.Date(Dato_YM), y = cum))
p + geom_line(name="Stock", colour = "#ee4d4d") + theme_minimal() +
  labs(list(title = "Stock of paintings from 1930 - 2015", x = "Year", y = "Stock")) +
  scale_fill_discrete(name="Experimental\nCondition")

#Question 3 - Aggregating stock of paintings by year, month and CuratorApproved
paints_by_DaAcq_CurApp  = only_paintings %>%
  group_by(Dato_YM, CuratorApproved) %>%
  summarise(Stock_inc = n()) 

paints_by_DaAcq_CurApp= paints_by_DaAcq_CurApp %>%
  group_by(CuratorApproved) %>%
  mutate(cum_cura = cumsum(Stock_inc)) %>% 
  mutate(Approved = ifelse(CuratorApproved=="Y","Yes","No"))   

#Plotting data 
p = ggplot(paints_by_DaAcq_CurApp, aes(x = as.Date(Dato_YM), y = cum_cura))
p + geom_line(aes(colour = Approved)) + theme_minimal() + 
  labs(list(title = "Stock of paintings from 1930 - 2015 by Curator Approved", x = "Year", y = "Stock"))

#Question 4 - Aggregating stock by department, year and month - 
#Assuming that the question applies to all classification, as there would otherwise only be one Department

paints_by_DaAcq_Dep  = raw_data %>%
  group_by(Department, Dato_YM) %>%
  summarise(Stock_inc = n()) 

paints_by_DaAcq_Dep= paints_by_DaAcq_Dep %>%
  group_by(Department) %>%
  mutate(cum_Depart = cumsum(Stock_inc)) 

# Question 5 - Plotting data 
p = ggplot(paints_by_DaAcq_Dep, aes(x = as.Date(Dato_YM), y = cum_Depart))
p + geom_line(aes(colour = Department)) + theme_minimal() + 
  labs(list(title = "Stock of art 1930 - 2015", x = "Year", y = "Stock of paintings"))

# Question 6 - Aggregating stock of paintings by Artist and only keeping 
# the 10 artists with the highest number af paintings.

paints_by_Artist  = only_paintings %>%
  group_by(Artist) %>%
  summarise( Stock_inc = n()) %>%
  arrange(desc(Stock_inc)) %>%
  top_n(n=10)
head(paints_by_Artist, 10)

# Question 7 - Finding which country the artist where born and aggregating stock by country.
# Removing double blanks and making lowercase

B_country = only_paintings %>% 
  select(ArtistBio) %>%  
  mutate(ArtistBio = gsub("  "," ", tolower(ArtistBio)))

# If the variable ArtistBio contains born, we want to have the characters after 
B_country$country = str_extract(B_country$ArtistBio, "(born)+[ ]+[A-z]+[ ][A-z]+|(born)+[ ]+[A-z]+")

# Cleaning missing born observation - Hardcoding
B_country$country = ifelse(B_country$ArtistBio=="(canadian, born u.s.a. 1928)", "USA",B_country$country)

# Removing born form the variable country 
B_country$country = gsub("born", "", B_country$country)

# If the word born is not in string, then we want to have the first countryname.
B_country = B_country %>%  select(ArtistBio, country) %>%  
  mutate(country  = ifelse(is.na(country), str_extract(ArtistBio, "[A-z]+"), country))

# Cleaning observation - Hardcoding
B_country$country = ifelse(B_country$ArtistBio=="(south african, born 1953)", "South Africa",B_country$country)


### Combining the mapdata and the data about where the artists comes form. 
## First the data in B_country need to have the same megering-key as in the mapdata. 

# We do not want to change anything to the observations equal to USA, South Africa
B_country = B_country %>% 
  mutate(country = ifelse(country=="USA"|country== "South Africa", country, str_to_title(country)))

B_country$country = gsub(" ","",B_country$country)
B_country$country = gsub("american","USA",B_country$country)
B_country$country = gsub("french","France",B_country$country)
B_country$country = gsub("spanish","Spain",B_country$country)
B_country$country = gsub("british","UK",B_country$country)
B_country$country = gsub("england","UK",B_country$country)
B_country$country = gsub("scotland","UK",B_country$country)
B_country$country = gsub("irish","Ireland",B_country$country)
B_country$country = gsub("danish","Denmark",B_country$country)
B_country$country = gsub("italian","Italy",B_country$country)
B_country$country = gsub("japanese","Japan",B_country$country)
B_country$country = gsub("belorussia","Belarus",B_country$country)
B_country$country = gsub("thephilipines","Philippines",B_country$country)
B_country$country = gsub("tunis","Tunisia",B_country$country)
B_country$country = gsub("belgian","Belgium",B_country$country)
B_country$country = gsub("norwegian","Norway",B_country$country)
B_country$country = gsub("canadian","Canada",B_country$country)
B_country$country = gsub("swedish","Sweden",B_country$country)
B_country$country = gsub("dutch","Netherlands",B_country$country)
B_country$country = gsub("polish","Poland",B_country$country)
B_country$country = gsub("turkish","Turkey",B_country$country)
B_country$country = gsub("german","Germany",B_country$country)
B_country$country = gsub("moroccan","Morocco",B_country$country)
B_country$country = gsub("hungarian","Hungary",B_country$country)
B_country$country = gsub("guyanese","Guyana",B_country$country)
B_country$country = gsub("czech","Czechoslovakia",B_country$country)
B_country$country = gsub("yugoslav","Yugoslavia",B_country$country)
B_country$country = gsub("inbeirut","Lebanon",B_country$country)
B_country$country = gsub("ukrainian","Ukraine",B_country$country)

# Using iso2c countrycodetype to group on and later as merging-key

B_country$iso2c = countrycode(B_country$country, origin = "country.name", destination = "iso2c")

born_by_country  = B_country %>%
  group_by( iso2c) %>%
  summarise( Stock_inc = n()) %>%
  arrange(desc(Stock_inc))

# Grouping the number of artists in intervals - This will make it easier to read the graf, but the downside will 
# be the lose of detailed information. 

born_by_country = born_by_country %>% 
  mutate(Number.Of.Artist = ifelse(born_by_country$Stock_in > 100,"Over 100","")) %>% 
  mutate(Number.Of.Artist = ifelse(100 > born_by_country$Stock_in & born_by_country$Stock_in >= 90,"90-100",Number.Of.Artist)) %>% 
  mutate(Number.Of.Artist = ifelse(90  > born_by_country$Stock_in & born_by_country$Stock_in >= 80,"80-89" ,Number.Of.Artist)) %>% 
  mutate(Number.Of.Artist = ifelse(80  > born_by_country$Stock_in & born_by_country$Stock_in >= 70,"70-79" ,Number.Of.Artist)) %>% 
  mutate(Number.Of.Artist = ifelse(60  > born_by_country$Stock_in & born_by_country$Stock_in >= 60,"60-69" ,Number.Of.Artist)) %>% 
  mutate(Number.Of.Artist = ifelse(60  > born_by_country$Stock_in & born_by_country$Stock_in >= 50,"50-59" ,Number.Of.Artist)) %>% 
  mutate(Number.Of.Artist = ifelse(50  > born_by_country$Stock_in & born_by_country$Stock_in >= 40,"40-59" ,Number.Of.Artist)) %>% 
  mutate(Number.Of.Artist = ifelse(40  > born_by_country$Stock_in & born_by_country$Stock_in >= 30,"30-49" ,Number.Of.Artist)) %>% 
  mutate(Number.Of.Artist = ifelse(30  > born_by_country$Stock_in & born_by_country$Stock_in >= 20,"20-29" ,Number.Of.Artist)) %>% 
  mutate(Number.Of.Artist = ifelse(20  > born_by_country$Stock_in & born_by_country$Stock_in >= 10,"10-19" ,Number.Of.Artist)) %>%  
  mutate(Number.Of.Artist = ifelse(10  > born_by_country$Stock_in & born_by_country$Stock_in >= 1 ,"1-9" ,Number.Of.Artist))

glimpse(born_by_country)

# Loading world map and getting countrycodetype iso2c
map = map_data("world")
map$iso2c = countrycode(map$region, origin = "country.name", destination = "iso2c")

# Controlling that nothing is missing. 
born_by_country$country[!unique(born_by_country$iso2c) %in% unique(map$iso2c)]

# Merging 
df.map = left_join( map,born_by_country, by = c("iso2c" = "iso2c")) # Joining data on iso2c

# Graph 1
p = ggplot(df.map, aes(x = long, y = lat, group = group, fill = Number.Of.Artist))
p + geom_polygon() + expand_limits(x = df.map$long, y = df.map$lat) +
  labs(title = "Number of artists in MOMA born in ")

#Question 8 
# Dimensions - We only want paintings, where there are minimal information about lenght and width dimensions
# We therefore filter out the observations without sufficient information about the size

Paint_area = select(only_paintings,Dimensions) %>% filter(Dimensions !="")
Paint_area = filter(Paint_area, Dimensions !="Overall dimensions variable"          & Dimensions !="Overall dimensions variable.")
Paint_area = filter(Paint_area, Dimensions !="Dimensions and installation variable" & Dimensions !="Various dimensions")
Paint_area = filter(Paint_area, Dimensions !="Dimensions variable"                  & Dimensions !="Installation variable")
Paint_area = filter(Paint_area, Dimensions !="Dimensions vary with installation"    & Dimensions !="Installation variable")
Paint_area = filter(Paint_area, Dimensions !='Approximately 2" (5.1 cm) high, length variable' & Dimensions != "Overall dimensions variable\r\n")   

# "cm" are written in the parentheses, therefore we only want what inside the parentheses.
Paint_area$Dim_Num = str_extract(Paint_area$Dimensions, "([\U0028][0-9].+[\U0029])")

#  Extracting "overall" Measurements
Paint_area$Dim_Num = ifelse(is.na(str_extract(Paint_area$Dimensions, "(overall)"))==FALSE, str_extract(str_extract(Paint_area$Dimensions, "([overall].+)+"), "([\U0028][0-9].+[\U0029])"), str_extract(Paint_area$Dimensions,"([\U0028][0-9].+[\U0029])" ))

?is.na
# Cleaning data
Paint_area$Dim_Num=gsub(".x"," x",Paint_area$Dim_Num)
Paint_area$Dim_Num=gsub("X","x",Paint_area$Dim_Num)
Paint_area$Dim_Num=gsub("×","x",Paint_area$Dim_Num) # Removing special character 
Paint_area$Dim_Num=gsub("  "," ",Paint_area$Dim_Num)


# Hardcoding NA's in the Dim_Num variabel - 
Paint_area = Paint_area %>%  
  mutate(Dim_Num = ifelse(is.na(Dim_Num),ifelse(Dimensions=="10 x 14 cm", "(10 x 14 cm)", Dim_Num),Dim_Num))

Paint_area = Paint_area %>%  
  mutate(Dim_Num = ifelse(is.na(Dim_Num),ifelse(Dimensions=="Framed 47.5 x 37.3 cm)", "(47.5 x 37.3 cm)",Dim_Num),Dim_Num))

Paint_area = Paint_area %>%  
  mutate(Dim_Num_1 = ifelse(is.na(Dim_Num), gsub("'","",Dimensions),Dim_Num))

Paint_area = Paint_area %>%  
  mutate(Dim_Num = ifelse(is.na(Dim_Num),ifelse(Dim_Num_1=='71" x 10.', "(215.9 x 304.8 cm)",Dim_Num),Dim_Num))

# Deviding the dimension into two variables - area will be calulate in CM^2 later. 
Paint_area$Dim_N1  = as.numeric(str_extract(Paint_area$Dim_Num, "([0-9].)+|[0-9]+"))
Paint_area$Dim_N2  = as.numeric(str_extract(str_extract(Paint_area$Dim_Num, "([x][ ][0-9].+)"), "([0-9]+[0-9][.].)|[0-9]+")) 


# Finding out what kind of shape the paintings are. Using to groups: squares and circles
# If the painting has a diameter or is oval then it will be grouped as a cirkel. Everything else 
# is grouped as a square. 
Paint_area$Dimensions  = gsub("oval","diameter",Paint_area$Dimensions)
Paint_area$Type  = str_extract(Paint_area$Dimensions, "\\b(d|D)?iameter\\b")
Paint_area$Type  = ifelse(is.na(Paint_area$Type),"Squares","Circle")

# Calculating area - If square then length times width and if Circle then pi times radius squared. 
Paint_area$Area  = ifelse(Paint_area$Type == "Circle",pi*(Paint_area$Dim_N1/2)**2, Paint_area$Dim_N1*Paint_area$Dim_N2)

# Finding 10 largest paintings 

paints_by_Size_Large2  = Paint_area %>%
  mutate(Area = ifelse(is.na(Area), mean(Paint_area$Area, na.rm=TRUE), Area)) %>% 
  arrange(desc(Area))
head(paints_by_Size_Large2$Area, 10)
tail(paints_by_Size_Large2$Area, 10)

Paint_area = Paint_area %>% 
  arrange(Area)
head(Paint_area$Area, 10)

# Alternatively: To find the smallest 10 paintings - we give all paintings a value of one in the varibel nr, 
# then we arrange them in descending order of area from largest paintings to smallest and 
# cumulate the values of nr in the varible NR. Then we take the 10 largest values of NR.  

Paint_area$nr = 1
paints_by_Size_Small = Paint_area %>% 
  arrange(desc(Area)) %>% 
  mutate(NR=cumsum(nr)) 
paints_by_Size_Small = paints_by_Size_Small %>%   
  top_n(n=10,NR)


