#############################################################################################################################
##################### calculating net disposable income (B6N) = B6G - P51C following ESA 2010 ###############################
#############################################################################################################################

# https://ec.europa.eu/eurostat/databrowser/bookmark/f2567038-31c9-402f-b261-5d431990c599?lang=en # link to eurostat raw-data

rm(list=ls())                                                                                     # clean workspace

# load/install required package

# install.packages("readxl")

library(readxl)

# define helper function

to.num<-function(x){
  x=data.frame(x)
  h<-matrix(nrow = nrow(x),ncol = ncol(x))
  for(i in 1:nrow(x)){
    for(j in 1:ncol(x)){
      h[i,j]=as.numeric(x[i,j])
    }
  }
  return(h)
}

sum.4<-function(x){
  NC<-ncol(x)
  res=rowSums(x[,1:4])
  if(NC>4){
    for (i in seq(5,NC-3,4)) {
      res=cbind(res,rowSums(x[,i:(i+3)])) 
    }
  }
  return(res)
}
# variable s to customize

path_to_folder<-'C:/Users/BongBong/Downloads'                                                     # please adjust to location in your files

start_year<-2000                                                                                 # start of ts
end_year<-2020                                                                                   # end of ts

eurostat_raw<-read_xlsx(paste(path_to_folder,'NASQ_10_NF_TR__custom_10710451623917006023.xlsx',sep = '/'),sheet = 3)

# get structure in to raw data / extract relevant information

# select desired columns
cols<-c(1,2)
for(t in start_year:end_year){
  cols<-c(cols,grep(toString(t), eurostat_raw[9,]))
}

# test for errors / error if false
length(cols)-2==4*(end_year-start_year+1)

# proceed with eurostat_sel, the selected data form the raw data
eurostat_sel<-eurostat_raw[-c(1:8,10:11,94:100),cols]

# store points in time and countriy/aggregate names
Time<-c(t(eurostat_sel[1,-(1:2)]))
Name<-unique(c(t(eurostat_sel[-1,1])))

# calculate B6N = B6G - P51C
B6G<-to.num(eurostat_sel[eurostat_sel[,2]=='Disposable income, gross',-(1:2)]) # if there is missing data warnings will occur
P51C<-to.num(eurostat_sel[eurostat_sel[,2]=='Consumption of fixed capital',-(1:2)]) # if there is missing data warnings will occur
B6N=B6G-P51C

# get results into readable format / data.frame 
disp.income<-data.frame(B6N)
rownames(disp.income)=Name
colnames(disp.income)=Time

# aggregate to year level data
if(paste(start_year,'Q1',sep = '-') %in% colnames(disp.income) && paste(end_year,'Q4',sep = '-') %in% colnames(disp.income)){
  disp.income.yl<-sum.4(disp.income)
} else{print('remove years where not all quarters are available')}
colnames(disp.income.yl)<-start_year:end_year

# unit = mil Euros ; no seasonal nor calender adjustment ; current prices!!!
