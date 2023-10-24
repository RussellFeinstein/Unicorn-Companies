#setwd('C:/Users/Lee/iCloudDrive/Document/Boston University/CS544 Foundation with R/Final Project')
#getwd()

##### read the file
df = read.csv("D:/CodingClasses/CS544/Project/Unicorn_Companies.csv")

##### clean the data
# check none for each columns
# we need to find NA. 
# in this dataset, it uses 'None' as empty data
numberOfNa = function(df){
  flag = 'None' # set 'None' as a checking flag
  for(i in 1:ncol(df)){
    temp = df[, i] # extract column one by one
    n = length(temp[temp == flag]) # count how many 'None'
    print(paste(colnames(df)[i], n)) # print column name and 'None' quantity
  }
}
dropNone = function(df, columnName){
  drop = which(df[, columnName] == 'None')
  df = df[-drop, ]
  return(df)
}
checkType = function(df){
  for(i in 1:ncol(df)){
    temp = df[, i]
    print(paste(colnames(df)[i], '--->', typeof(temp)))
  }
}

numberOfNa(df) # check na 
### Drop useless columns
# we have only 1037 data rows
# Financial.Stage and Portfolio.Exits have 988 NA 
df = df[, -which(colnames(df) %in% c('Financial.Stage', 'Portfolio.Exits'))] # Drop these two columns

# after check column Select.Inverstors
# it's not suitable to analysis
# and I think it's not important
df = df[, -which(colnames(df) %in% c('Select.Inverstors'))] # Drop 



### Drop NA for each columns
df = dropNone(df, 'Founded.Year') # drop NA in Founded.Year
numberOfNa(df)
nrow(df)

df = dropNone(df, 'Deal.Terms') # drop NA in Deal.Terms
numberOfNa(df)
nrow(df)

df = dropNone(df, 'Total.Raised') # drop NA in Total.Raised
numberOfNa(df)
nrow(df)





### change data type for Valuation...B. and Total.Raised
# change string '$140' into numerical data 140
# change string '$7.44B' into numerical data 7.44
# Valuation...B.
temp = c() # set an empty list
for(i in 1:nrow(df)){
  d = df[i, 'Valuation...B.'] # take string out, e.g. '$114
  d = substring(d, 2) # extract into '114'
  d = as.numeric(d) # change datatype into numeric, e.g. 114
  temp = append(temp, d) # save into a temporary list
}
df$Valuation...B. = temp # replace the old data with new data

# Total.Raised
# set 'million' as the column unit
# e.g. '$12B' into 12000
temp = c() # set an empty list
for(i in 1:nrow(df)){
  d = df[i, 'Total.Raised']
  n = nchar(d) # length of the character
  unit = substring(d,n) # extract 'B' or 'M'
  number = substring(d, 2, n-1) # extract number part, e.g. '114'
  number = as.numeric(number) # turn string into numeric, e.g. 114
  
  if(unit == "B"){
    number = number * 1000 
    # if unit is 'Billion', times 1000 to make the unit into million
    # e.g. 3B is 3000M
  }           
  
  temp = append(temp, number) # append the number into the temp list
}
df$Total.Raised = temp # replace the old data with new data, which is the temp list

### clean the time data column
# Date.Joined
# split date data into year, month, and day
# split into three columns
dayJoin = c() # create temp list
monthJoin = c()
yearJoin = c()

for(i in 1:nrow(df)){
  d = df[i, 'Date.Joined']
  d = strsplit(d, split = '/') # split timestamp
  names(d) = 'timestamp' # redundant but important thing, or we can't access the list
  
  # access the splited time stamp
  day = d$timestamp[2]
  month = d$timestamp[1]
  year = d$timestamp[3]
  
  # append them into the temp lists
  dayJoin = append(dayJoin, as.numeric(day)) 
  monthJoin = append(monthJoin, as.numeric(month))
  yearJoin = append(yearJoin, as.numeric(year))
}

df$dayJoin = dayJoin # create new columns 
df$monthJoin = monthJoin
df$yearJoin = yearJoin

# Founded.Year, turn string into numeric
# e.g. '2022' into 2022
df$Founded.Year = as.numeric(df$Founded.Year)

### turn the rest string data type into numeric data type
# Deal.Terms
df$Deal.Terms = as.numeric(df$Deal.Terms)

# Investors.Count
df$Investors.Count = as.numeric(df$Investors.Count)

# check data type for columns
checkType(df)

















####### Where I'm starting
par(mfrow=c(1,1))





# Only use countries that have more than 20 companies
tmp <- as.data.frame(table(df$Country))
tmp <- tmp[tmp$Freq > 20,]

df <- df[df$Country %in% tmp$Var1,]

df






# Sorts by country, alphabetically
df[order(df$Country),]


# Returns table of frequency of country occurrence
sort(table(df$Country), decreasing = TRUE)

barplot(sort(table(df[,'Country'])), col='cyan')
pie(sort(table(df[,'Country'])), col=rainbow(6))


# Look at the percentage of companies in each industry
barplot(sort(table(df[,'Industry'])), col='cyan')


# Boxplot of valuation. In theory useful, but the outliers make it almost useless
boxplot(df$Valuation...B., 
        col = "cyan",
        ylab = "Valuation")


# Create individual dataframes per country
for(i in unique(df$Country)) {
  nam <- paste("df", i, sep = ".")
  assign(nam, df[df$Country==i,])
}


# remove spaces from variable names with spaces

df.UnitedStates <- `df.United States`
df.UnitedKingdom <- `df.United Kingdom`

par(mfrow=c(6, 1))

boxplot(df.China$Valuation...B., 
        col = "red",
        xlab = "Valuation (Billions)",
        ylab = "China",
        ylim=c(0,10),
        horizontal = TRUE)

boxplot(df.France$Valuation...B., 
        col = "orange",
        xlab = "Valuation (Billions)",
        ylab = "France",
        ylim=c(0,10),
        horizontal = TRUE)

boxplot(df.Germany$Valuation...B., 
        col = "yellow",
        xlab = "Valuation (Billions)",
        ylab = "Germany",
        ylim=c(0,10),
        horizontal = TRUE)

boxplot(df.India$Valuation...B., 
        col = "green",
        xlab = "Valuation (Billions)",
        ylab = "India",
        ylim=c(0,10),
        horizontal = TRUE)

boxplot(df.UnitedKingdom$Valuation...B., 
        col = "cyan",
        xlab = "Valuation (Billions)",
        ylab = "United Kingdom",
        ylim=c(0,10),
        horizontal = TRUE)

boxplot(df.UnitedStates$Valuation...B., 
        col = "violet",
        xlab = "Valuation (Billions)",
        ylab = "United States",
        ylim=c(0,10),
        horizontal = TRUE)


par(mfrow=c(1, 1))



# Valuation vs Money Raised

# Overall

plot(df$Valuation...B., df$Total.Raised, ylim= c(0,8000), cex=.5)
abline(lm(Total.Raised~Valuation...B., data = df))


# By Country

points(df.UnitedStates$Valuation...B., df.UnitedStates$Total.Raised, pch=6, col = 'purple', cex=.5)
abline(lm(Total.Raised~Valuation...B., data = df.UnitedStates), col='purple')

points(df.China$Valuation...B., df.China$Total.Raised, ylim= c(0,8000), col='red', cex=.5)
abline(lm(Total.Raised~Valuation...B., data = df.China), col='red')

points(df.India$Valuation...B., df.India$Total.Raised, pch=4, col = 'green4', cex=.5)
abline(lm(Total.Raised~Valuation...B., data = df.India), col='green4')

points(df.UnitedKingdom$Valuation...B., df.UnitedKingdom$Total.Raised, pch=5, col = 'cyan3', cex=.5)
abline(lm(Total.Raised~Valuation...B., data = df.UnitedKingdom), col='cyan3')

points(df.France$Valuation...B., df.France$Total.Raised, pch=2, col = 'darkorange', cex=.5)
abline(lm(Total.Raised~Valuation...B., data = df.France), col='darkorange')

points(df.Germany$Valuation...B., df.Germany$Total.Raised, pch=3, col = 'gold2', cex=.5)
abline(lm(Total.Raised~Valuation...B., data = df.Germany), col='gold2')



# Central Limit Theorem

mu <- mean(df$Valuation...B.)
sigma <- sd(df$Valuation...B.)

samples <- 1000

par(mfrow = c(2,2))

xbar <- numeric(samples)

for (size in c(10, 20, 30, 40)) {
  for (i in 1:samples) {
    xbar[i] <- mean(rnorm(size, mean = mu, sd = sigma))
  }
  
  hist(xbar,
       breaks = 15, xlim=c(0, mu+sigma),
       main = paste("Sample Size =", size))
  
  cat("Sample Size = ", size, " Mean = ", mean(xbar),
      " SD = ", sd(xbar), "\n")
}


# Sampling
library(sampling)

sample.size <- 80
pop <- nrow(df)

barplot(table(df$Country))


# SRSWOR
set.seed(6396)
s <- srswor(sample.size, pop)
sample.random <- df[s != 0,]
sample.random

freq <- table(df$Country[s != 0])
freq
prop.table(freq)*100

barplot(freq)

# Systematic Sampling

k <- ceiling(pop / sample.size)

set.seed(6396)
r <- sample(k, 1)

s <- seq(r, by = k, length = sample.size)

sample.systematic <- df[s,]
sample.systematic

freq <- table(sample.systematic$Country)
freq
prop.table(freq)*100


barplot(freq)


# Stratified sampling by valuation

df.sorted <- df[order(df$Country),]

head(df.sorted)

freq <- table(df.sorted$Country)

freq

strata.sizes <- 80 * freq / sum(freq)

strata.sizes

set.seed(6396)
strata <- sampling::strata(df.sorted, stratanames = c("Country"),
                           size = strata.sizes, method = "srswor",
                           description = TRUE)

set.seed(6396)
sample.strata <- getdata(df.sorted, strata)

sample.strata

freq <- table(sample.strata$Country)
freq
prop.table(freq)*100

barplot(freq)

