# Relationship between greenhouse gases and agriculture commodites

# The purpose today is to explore data and merge required data


setwd('~/Desktop/dataanalysis/whathodata/commodities')

library(dplyr)

# Let's start with CEREAL
### CEREAL ####
cereal <- read.csv(file='cereals.csv', header = TRUE, sep = ',')

typeof(cereal)

# Let us convert our list to dataframe for data cleansing
cereal_df <- as.data.frame(cereal)


# Let us see if there are any null values
# First list of columns

typeof(cereal)
class(cereal)
names(cereal)
str(cereal)
I(cereal)
dim(cereal)
head(cereal)
tail(cereal)

levels(as.factor(cereal$Year))
levels(cereal$Flow)

#Let's see if there are any null values in the data

which(is.na(cereal$Country.or.Area))
which(is.na(cereal$Year))
which(is.na(cereal$Comm..Code))
which(is.na(cereal$Commodity))
which(is.na(cereal$Flow))
which(is.na(cereal$Trade..USD.))
which(is.na(cereal$Weight..kg.))
which(is.na(cereal$Quantity.Name))
which(is.na(cereal$Quantity))

length(which(is.na(cereal$Weight..kg.)))
length(which(is.na(cereal$Quantity)))
#Let's examine the data that has null values to see if it is significant

cereal[which(is.na(cereal$Quantity)),]

#Lets see specific country and see if we can figure out a pattern - question remains...do we care?
filter(cereal, is.na(Quantity))
filter(cereal, is.na(Quantity) & Country.or.Area=='Bermuda')
# All the data is Flow=='Import'

#Let's see of everything is Flow
cereal$Flow[which(is.na(cereal$Quantity))]
#Most are Import with few Export and others...we are more interested in Exports
nrow(filter(cereal, is.na(Quantity) & Flow=='Export'))

filter(cereal, is.na(Quantity) & Flow=='Export')

# There is no pattern to the 50 records that have export == NA. 

#Let us determine if we should quantify our commodities using Trade..USD or weight of Quantity.

str(cereal)

# Since we will be determining various commodities that are measured in different ways..we 
# use Trade..USD

# Let us look at the columns and determine what we need and eliminate others

cereal_df <- filter(cereal, Flow=='Export')
cereal_df <- cereal_df[,c('Country.or.Area','Year','Trade..USD.')]
str(cereal_df)
colnames(cereal_df) <- c("Country","Year","Cereal")
colnames(cereal_df)


# We can add the USD for year and country

str(cereal_df)
head(cereal_df)

cereal_df <- cereal_df %>%
  select(Country, Year, Cereal) %>%
  group_by(Country, Year) %>%
  summarise(Cereal = sum(Cereal)) %>%
  ungroup()

head(cereal_df)

### General ####

check_nulls <- function (mylist) {
  if (which(is.na(mylist)) == 0) return(0) else return(1)
}
## DAIRY ####

dairy <- read.csv(file='dairy.csv', header=TRUE,sep =',')

# Let's examine the data again and see if it is similar to cereal
class(dairy)
names(dairy)
str(dairy)

# Now we know the columns we need, lets see if any of them have null values
# didn't work.. check_nulls(dairy$Country.or.Area)
which(is.na(dairy$Country.or.Area))
which(is.na(dairy$Year))
which(is.na(dairy$Trade..USD.))

dairy_df <- dairy[dairy$Flow=='Export',c('Country.or.Area','Year','Trade..USD.')] 
colnames(dairy_df) <- c('Country','Year','Dairy')


dairy_df <- dairy_df %>%
  select(Country,Year,Dairy) %>%
  group_by(Country, Year) %>%
  summarise(Dairy = sum(Dairy)) %>%
  ungroup()

head(dairy_df)

#### FISH ####

fish <- read.csv(file='fish.csv',header = TRUE, sep = ',')

str(fish)

which(is.na(fish$Country.or.Area))
which(is.na(fish$Year))
which(is.na(fish$Trade..USD.))

fish_df <- fish[fish$Flow == 'Export', c('Country.or.Area','Year','Trade..USD.')]
colnames(fish_df) <- c('Country', 'Year', 'Fish')

fish_df <- fish_df %>%
  select(Country, Year, Fish) %>%
  group_by(Country, Year) %>%
  summarise(Fish = sum(Fish)) %>%
  ungroup()

head(fish_df)

#### FRUITS ####

fruits <- read.csv(file='fruits.csv', header=TRUE, sep=',')
str(fruits)

which(is.na(fruits$Country.or.Area))
which(is.na(fruits$Year))
which(is.na(fruits$Trade..USD.))

fruits_df <- fruits[fruits$Flow=='Export',c('Country.or.Area','Year','Trade..USD.')]
colnames(fruits_df) <- c('Country','Year','Fruits')
head(fruits_df)
str(fruits_df)

fruits_df <- fruits_df %>%
  select(Country, Year, Fruits) %>%
  group_by(Country, Year) %>%
  summarise(Fruits=sum(Fruits)) %>%
  ungroup()

head(fruits_df)

#### LIVEANIMALS ####

liveanimals <- read.csv(file='liveanimals.csv', header = TRUE, sep=',')

class(liveanimals)
str(liveanimals)

which(is.na(liveanimals$Country.or.Area))
which(is.na(liveanimals$Year))
which(is.na(liveanimals$Trade..USD.))

liveanimals_df <- liveanimals[liveanimals$Flow=='Export', c('Country.or.Area','Year','Trade..USD.')]
colnames(liveanimals_df) <- c('Country', 'Year', 'LiveAnimals')

liveanimals_df <- liveanimals_df %>%
  select(Country, Year, LiveAnimals) %>%
  group_by(Country, Year) %>%
  summarise(LiveAnimals=sum(LiveAnimals)) %>%
  ungroup()

head(liveanimals_df)


#### Meat ####

meat <- read.csv(file='meat.csv', header = TRUE, sep=',')

str(meat)
class(meat)

which(is.na(meat$Country.or.Area))
which(is.na(meat$Year))
which(is.na(meat$Trade..USD.))

meat_df <- meat[meat$Flow=='Export',c('Country.or.Area','Year','Trade..USD.')]
colnames(meat_df) <- c('Country','Year','Meat')

meat_df <- meat_df %>%
  select(Country, Year, Meat) %>%
  group_by(Country, Year) %>%
  summarise(Meat=sum(Meat)) %>%
  ungroup()

head(meat_df)


#### Plants ####

plants <- read.csv(file='plants.csv',header=TRUE,sep=',')

class(plants)
str(plants)

which(is.na(plants$Country.or.Area))
which(is.na(plants$Year))
which(is.na(plants$Trade..USD.))

plants_df <- plants[plants$Flow=='Export',c('Country.or.Area','Year','Trade..USD.')]
colnames(plants_df) <- c('Country','Year','Plants')

plants_df <- plants_df %>%
  select(Country, Year, Plants) %>%
  group_by(Country, Year) %>%
  summarise(Plants=sum(Plants)) %>%
  ungroup()

head(plants_df)

#### Vegs ####

vegs <- read.csv(file='vegs.csv',header=TRUE, sep=',')

str(vegs)
class(vegs)

which(is.na(vegs$Country.or.Area))
which(is.na(vegs$Year))
which(is.na(vegs$Trade..USD.))

vegs_df <- vegs[vegs$Flow=='Export', c('Country.or.Area','Year','Trade..USD.')]
colnames(vegs_df) <- c('Country','Year','Vegs')

vegs_df <- vegs_df %>%
  select(Country,Year, Vegs) %>%
  group_by(Country,Year) %>%
  summarise(Vegs=sum(Vegs)) %>%
  ungroup()

head(vegs_df)

#### Greenhouse gases ####
ghgas <- read.csv(file='greenhousegases.csv',header=TRUE,sep=',')

str(ghgas)
which(is.na(ghgas$Country.or.Area))
which(is.na(ghgas$Year))
which(is.na(ghgas$Value))
head(ghgas)


all_list <- list(cereal_df, dairy_df, fish_df, fruits_df, liveanimals_df, 
                 meat_df, plants_df, vegs_df)

all_df <- Reduce(function(x,y) merge(x, y, by=c('Country','Year'), all='TRUE'), all_list)

head(all_df)

###########################
#Week 3
###########################

#install.packages("GGally")
install.packages("magrittr")
library(GGally)
library(magrittr)
library(dplyr)
library(reshape2)

# Let us first incorpporate most important character, Green house gar emissions.

ghgas <- read.csv(file='greenhousegases.csv',header=TRUE,sep=',')

str(ghgas)
which(is.na(ghgas$Country.or.Area))
which(is.na(ghgas$Year))
which(is.na(ghgas$Value))
head(ghgas)

# How is data related to each other
ggcorr(ghg_df, label="TRUE")
colnames(ghgas)[3] <- c("GHGas")
colnames(ghgas)[1] <- c("Country")
head(ghgas)

# Doesn't look like all the data is there in greenhouse gases dataframe. 

ghg_df <- merge(all_df, ghgas, by=c('Country','Year'), all='TRUE')

head(ghg_df)                

# Let us see if there are any null values in our new GHGas column

which(is.na(ghg_df$GHGas))

# Now our promised story. Let us see which of our columns has the most correlation

# Let us see how cereal correlates to GHGas

ggcorr(ghg_df)

# It is clear what is related to what now...but I understand better if I see numbers

ggcorr(ghg_df, label="TRUE")

# How many countries are there in our data

unique(ghg_df$Country)
unique(ghgas$Country)

# Let us see how the greenhouse gases changed over the years for each country

ggplot(data=ghg_df, aes(x=Year, y=log(GHGas), group=Country, shape=Country, color=Country)) + 
  geom_line() +
  geom_point()

# Just the two countries show up, let us look at the data why

ghg_df %>% 
  select(Country, Year, GHGas) %>%
  filter(Country=='Canada')

ghg_df
ghgas

ggplot(data=ghgas, aes(x=Year, y=log(GHGas), group=Country, shape=Country, color=Country)) + 
  geom_line() +
  geom_point()
head(ghgas)

ghg_df %>% 
  filter(Country=='Latvia')

ghg_latvia <- melt(ghg_df[which(ghg_df$Country=='Latvia'),], id.vars=c('Country','Year'), measure.id=c('Cereal','Dairy','Fish','Fruits','LiveAnimals','Meat','Plants','Vegs','GHGas'))

ghg_latvia

ggplot(ghg_latvia, aes(x=Year, y=variable,shape=variable,color=variable)) +
  geom_line() +
  geom_point()
