### Lori Kim

##### PART 1A
# variables for Taylor's monthly payment
x=465600
z=0.045
y=30
c=z/12
n=y*12
p=x*(c*(1+c)^n)/((1+c)^n-1)
p

# Alternative output
cat("The monthly payment is", p)
cat("The monthly payment is $", p)                # Not ideal, $ and amount is not together
cat(paste("The monthly payment is $", p,sep=""))  # Paste $ and amount together
p=round(p,2)                                      # Round the output to decimal two
cat(paste("The monthly payment is $", p,sep=""))

# Alternative Way: Function
mortgage=function(price,downpay,interest,year){
  x=price*(1-downpay/100)
  z=interest/100
  c=z/12
  n=year*12
  p=x*(c*(1+c)^n)/((1+c)^n-1)
  return(paste("The monthly payment is $", round(p,2),sep=""))
}
mortgage(582000,20,4.5,30)

##### PART 1B
# install.packages("readxl")  # You only need to install it for the first time
# library(readxl)             # You need to run this everytime to import the data in xlsx form

excel = read_excel("P01_LA zipcode payroll.xlsx", sheet = "2017") #putting the reading of the excel file into a variable

## need to display employment for total industry, information, professional, scientific, & technical skills
colnames(excel)

# variables to get columns in excel
zipCol = excel$`Zip Code`
empCol = excel$Employment
indCol = excel$Industry

# clean up the zip code "Total"
zipCode = gsub(" Total", "", zipCol)

# clean up the repetitive zip codes and display in a table
tZip = as.data.frame(table(zipCode))

## loop to get each zip code's total, put into a vector, add it as a col to laz17tech data frame
## " " to get information employment and professional employment
# reminder: excel file starts with row 2, but rstudio starts with row 2 as [1]

# variables for calculating laz17tech col 2:5
len = length(tZip[,2]) # length of col 2 of table tzip ('Freq')
total = c()
info = c()
prof = c()
perc = c()

for (i in 1:len) {
  n = sum(tZip[1:i,2]) # get the sum of each freq for the excel col / sum of the last zipcode
  p = sum(tZip[0:(i-1),2]) # first one we can't use bc it starts with 0
  
  # loop through to find the col num for Total in excel
  total[i] = empCol[n]
  
  # loop through each zip code to find "Information" and its col #
  for(a in p:n) {
    if("Information" %in% indCol[a]) {
      info[i] = empCol[a] # if "Information" then input into info vector
      break
    } else {
      info[i] = NA
    }
  } 
  
  # loop through each zip code to find ""Professional, Scientific, & Technical Skills" and its col #
  for(b in p:n) {
    if("Professional, Scientific, & Technical Skills" %in% indCol[b]) {
      prof[i] = empCol[b]
      break
    } else {
      prof[i] = NA
    }
  }
}

# cleaning the data
total = as.numeric(total)
info = as.numeric(gsub("\\*", 0, info))
prof = as.numeric(gsub("\\*", 0, prof))

# make a data frame to add the vectors and change the col into numeric
laz17t = data.frame(zipcode = tZip$zipCode, total = total, information = info, professional = prof)

# calculate the percentage: tech job percent = (information jobs + professional scientific technical jobs) / total jobs
# clean up the NA by changing it go 9999999
for(c in 1:len) {
  perc[c] = ( rowSums(laz17t[c,3:4] ) / laz17t[c,2] )
  
  if ( is.na(laz17t[c,2]) ) {
    laz17t[c,2] = 99999999
  }
  
  if ( is.na(laz17tech[c,3]) ) {
    laz17t[c,3] = 99999999
  }
  
  if ( is.na(laz17tech[c,4]) ) {
    laz17t[c,4] = 99999999
  }
  
  if( is.na(perc[c]) ) {
    perc[c] = 99999999
  }

}

# add col perc to laz17t
laz17t$per = perc

write.csv(laz17t,"laz17t.csv")

