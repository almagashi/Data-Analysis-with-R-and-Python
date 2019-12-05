
foo <- read.csv ("https://tinyurl.com/yb4phxx8")

# Column names 
names (foo)

# Dimensions of the dataset
dim (foo)

# A glimpse of the data 
head(foo)

# Columns representing dates
date.columns <- c(11,12,14,15,16,17,18,25)


# Iterate over the date data
for (i in date.columns)
    {
    # missing values
    which_values_are_missing <- which(as.character(foo[,i])=="")
    # replace to NA 
    foo[which_values_are_missing, i] <- NA
    # values -> date
    foo [, i] <- as.Date(as.character(foo[,i]))
}

foo [3,12]


# Remove NAs for a column
which.have.NAs <- which(is.na(foo$Rating))
new_foo <- foo [-which.have.NAs,]

# Not use NAs in Circulation.Date column
which.have.NAs <- which(is.na(foo$CirculationDate))
filtered_circulation <- new_foo[-which.have.NAs, ]

# Circulation.Date after 2009

mydata= subset(filtered_circulation, filtered_circulation$CirculationDate>=as.Date("2009-01-01"))
mydata



summary(mydata)

# Making sure the data is correct
which(is.na(mydata$OriginalCompletionDate))
which(is.na(mydata$ApprovalDate))


# The difference in days between completion and approval

diff_ocd_ad <- as.numeric (mydata$OriginalCompletionDate - mydata$ApprovalDate)
mean(diff_ocd_ad)
median(diff_ocd_ad)

# Since the difference is in days and the instructions talk about months 
# we divide this by amount by 30 days
mean(diff_ocd_ad)/30
median(diff_ocd_ad)/30


summary(diff_ocd_ad)
quantile (diff_ocd_ad)
IQR (diff_ocd_ad)

# Sort the dataset chronologically
sorted_mydata <- mydata[order(mydata$CirculationDate),]
# Difference between Revised Completion Date and Original Completion Date
diff_rcd_ocd <- as.numeric(sorted_mydata$RevisedCompletionDate-sorted_mydata$OriginalCompletionDate)
# Append the difference to the sorted data
sorted_mydata['Difference'] <- diff_rcd_ocd
nrow(sorted_mydata)



# Quartile ranges for the non-missing data dataset
dataset<-c(0:660)
summary(dataset)

# earlier projects
early <- sorted_mydata[0:330,]
summary (early$Difference)
quantile(early$Difference)
IQR (early$Difference)

# recent projects
recent <- sorted_mydata [331:660, ]
summary (recent$Difference)
quantile(recent$Difference)
IQR (recent$Difference)

diff_ocd_ad <- as.numeric(sorted_mydata$RevisedCompletionDate-sorted_mydata$ApprovalDate)
summary(diff_ocd_ad)
IQR(diff_ocd_ad)

# NAs check
which(is.na(mydata$RevisedCompletionDate))
# filter by date
rcd_subset= subset(mydata, mydata$RevisedCompletionDate>=as.Date("2010-01-01"))
rcd_subset


# Create a table that shows the rounded rating percentage
rating_percentage= round(prop.table(table (rcd_subset$Rating))*100, 2)
rating_percentage

# Display the data in a plot
mycols= c("brown", "orange1", "red", "sandybrown")
barplot (rating_percentage, border=0,main="Rating after 2010 (in percentage)", 
         ylim= c(0,100), xlab= 'Rating (Scale 0 to 3)', ylab= 'Percentage(%)', legend=TRUE,
         col=mycols)



# PPTA
pata <- rcd_subset[which(as.character(rcd_subset$Type)=="PATA"), ]
nrow(pata)


# Create a table that shows the rounded rating percentage
rating_percentage_pata= round(prop.table(table (pata$Rating))*100, 2)
rating_percentage_pata

# Display the data in a plot
mycols= c("brown", "orange1", "red", "sandybrown")
barplot (rating_percentage_pata, border=0,main="Rating after 2010 (in percentage)", 
         ylim= c(0,100), xlab= 'Rating (Scale 0 to 3)', ylab= 'Percentage(%)', legend=TRUE,
         col=mycols)

# Bottom 10% and top 10% of projects and remove NAs
bottom <- mydata[which(mydata$RevisedAmount < quantile (mydata$RevisedAmount, 0.10, na.rm=TRUE)),]
top <- mydata[which(mydata$RevisedAmount > quantile (mydata$RevisedAmount, 0.90, na.rm=TRUE)),]


# Table of rating for bottom and top 10%
bottom_table <- prop.table (table (bottom$Rating))
bottom_table
top_table <- prop.table (table (top$Rating))
top_table

# plot the differences
columnmerge <- rbind (bottom_table, top_table)
barplot (columnmerge, beside=T, main="Rating after 2010 (in percentage)", 
         ylim= c(0,1), xlab= 'Rating (Scale 0 to 3)', ylab= 'Percentage(%)', legend=c('Bottom 10%', 'Top 10%'))

summary(bottom)


summary(top)


