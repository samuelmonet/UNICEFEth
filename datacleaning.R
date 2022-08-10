# For dummy classification
library(makedummies)
# Library for heatmap
library(ggplot2)
# Color Brewer palette
library(viridis)
install.packages(c(
  "colorBlindness", "directlabels", "dplyr", "gameofthrones", "ggforce", 
  "gghighlight", "ggnewscale", "ggplot2", "ggraph", "ggrepel", 
  "ggtext", "ggthemes", "hexbin", "Hmisc", "mapproj", "maps", 
  "munsell", "ozmaps", "paletteer", "patchwork", "rmapshaper", "scico", 
  "seriation", "sf", "stars", "tidygraph", "tidyr", "wesanderson" 
))


# Charging of data
data=read.csv(file = 'dataR.csv',sep=';',header=TRUE)
data2=read.csv(file = 'dataR.csv',sep=';',header=TRUE)
question=read.csv(file = 'questionsR.csv',sep=';',header=TRUE)
coding=read.csv(file = 'codingR.csv',sep=';',header=TRUE)

#removing the columns with no names (ie those whose name begins by Unnamed)
new_names = c()
for (i in names(data)) if (!any(grep("Unnamed",i))) new_names = c(new_names,i)
data=data[new_names]
data2=data2[new_names]

# Treatment of the question table to extract the type of columns
to_code <- question[1,][question[4,]=='to code']
to_code <- to_code[!(is.na(to_code))]

cont_feat <- question[1,][question[3,]=='continue']
cont_feat <- cont_feat[!(is.na(cont_feat))]

dummy <- question[1,][question[4,]=='dummy']
dummy <- dummy[!(is.na(dummy))]

categorical <- question[1,][question[4,]=='cat']
categorical <- c(categorical,question[1,][question[4,]=='catno'])
categorical <- categorical[!(is.na(categorical))]

text <- question[1,][question[4,]=='text']
text <- text[!(is.na(text))]

catno <- question[1,][question[4,]=='catno']
catno <- catno[!(is.na(catno))]

to_drop <- question[1,][question[4,]=='drop']
to_drop <- to_drop[!(is.na(to_drop))]

yesno <- question[1,][question[4,]=='yn']
yesno <- yesno[!(is.na(yesno))]

################## On en est la ###########################""

# Treatment of gender questions
# Creation of a new column: gender HHH which takes the value of gender resp if 
# the respondent is HHH or the one of the HHH instead
data$gender.HHH = ifelse(data$Head_Household=='Yes',data$gender.resp,data$gender_if_no)
#verification
data[c('gender.resp',"gender_if_no","gender.HHH")]

# hh_ppl
data$hh_ppl=data$hh_ppl1+data$hh_ppl2+data$hh_ppl3+data$hh_ppl4+data$hh_ppl5+data$hh_ppl6
data$incomepermember=data$income/data$hh_ppl

#howlong
data$how_long=data$how_long1+data$how_long2/12

#School attended
data$school_perc=data$school_attended/(data$hh_ppl3+data$hh_ppl4)

# Treatment of three main incomes to create one column per activity type and weight each
income_source  <-  function(x,income) {
  k <- 0
  if (x[["foodsource_rank1"]] == income) k <- 3 
  if (x[["foodsource_rank2"]] == income) k <- 2
  if (x[["foodsource_rank3"]] == income) k <- 1
  k
} 

for (income in unique(c(data$foodsource_rank1,data$foodsource_rank2,data$foodsource_rank3))) {
  #print(income)
  data[[income]]=apply(data,1,income_source,income)
  }

# aca3 and aca4 Only 2 values, I convert it to 0,1
for (feat in c("aca3","aca4")) {
  data[[feat]] <- as.factor(data[[feat]])
  data[[feat]] <- as.numeric(data[[feat]])-1
} 

#FCS score
data$FCS=ifelse(data$fcs1+data$fcs2<7,2*(data$fcs1+data$fcs2),7)+3*data$fcs3+
data$fcs4+data$fcs5+4*data$fcs6+4*data$fcs7+0.5*data$fcs8+0.5*data$fcs9
#check
fcs_names = c('FCS')
for (i in names(data)) if (any(grep("fcs",i))) fcs_names = c(fcs_names,i)
data[fcs_names]

#HHS use factors
data$hhs=ifelse(test = is.na(data$hh2)|data$hh2=="",0,ifelse(test = data$hh2=="Often (more than 10 times)",2,1))+
  ifelse(test = is.na(data$hh4)|data$hh4=="",0,ifelse(test = data$hh4=="Often (more than 10 times)",2,1))+
  ifelse(test = is.na(data$hh6)|data$hh6=="",0,ifelse(test = data$hh6=="Often (more than 10 times)",2,1))
data[c("hhs","hh2","hh4","hh6")]

#HDD also factor for converting Yes to 1 and No to O and then sum each
data$hdds <- ifelse(data$hhd1 == 'Yes',1,0)
for (i in 2:12) data$hdds <- data$hdds+ifelse(data[[paste("hhd",i,sep="")]]=='Yes',1,0)


#CSI Wait for Steve for the missing question
data$csi=data$fs_att1+2*data$fs_att2+2*data$fs_att3+4*data$fs_att4+3*data$fs_att5+
  2*data$fs_att6+4*data$fs_att7+data$fs_att8+2*data$fs_att9+2*data$fs_att10+2*data$fs_att11+4*data$fs_att12



#catno
catno <- gsub(pattern="[[:punct:^_]]",replacement=".",x=catno)
catno <- gsub(pattern="[ ]",replacement=".",x=catno)
for (feature in catno) {
  data[[feature]] <- ifelse(data[[feature]]=="",1,0) 
}

#Yes No Do not know
for (feature in yesno) {
  data[[feature]][is.na(data[[feature]])] <- 888 
  data[[feature]] <- as.factor(data[[feature]])
  levels(data[[feature]])[(levels(data[[feature]]) != 'Yes' & levels(data[[feature]]) != 'No')] <- 888
  levels(data[[feature]])[levels(data[[feature]]) == 'Yes'] <- 1
  levels(data[[feature]])[levels(data[[feature]]) == 'No'] <- 0
}

# To code
codes=read.csv(file = 'codingR.csv',sep=";",header=TRUE)

to_code <- gsub(pattern="[[:punct:^_]]",replacement=".",x=to_code)
to_code <- gsub(pattern="[ ]",replacement=".",x=to_code)
for (feature in to_code) {
  data[[feature]] <- ifelse(is.na(data[[feature]])|data[[feature]]=="",888,data[[feature]])
  subcodes=codes[codes$list.name==feature,]
  data[[feature]] <- as.factor(data[[feature]])
  for (n in 1:nrow(subcodes)) {
    levels(data[[feature]])[levels(data[[feature]]) == subcodes[n,"label.English.en."]] <- subcodes[n,"code"]
  }
}

# fillna
fillna=c()
for (i in colnames(data)) if (grepl('support2',i)) fillna=c(fillna,i)
for (i in colnames(data)) if (grepl('know',i)) fillna=c(fillna,i)
for (i in fillna) data[[i]] <- ifelse(is.na(data[[i]]),0,data[[i]])
rm(fillna)

# Now I replace all the NAs by 888
for (i in names(which(colSums(is.na(data))>0))) data[[i]] <- ifelse(is.na(data[[i]]),888,data[[i]])

#Treatment of 888 values (replacement by the mean of the column)
for (i in colnames(data)) {
  if (any(unique(data[[i]])==888)) {
    print(i)
    data[[i]] <- as.numeric(as.vector(data[[i]]))
    moyenne <- mean(data[[i]][data[[i]]!=888])
    print(moyenne)
    data[[i]] <- ifelse(data[[i]]==888,moyenne,data[[i]])
  }
}

################# DUMMIES ####################
# first replace values under a certain threshold by "Other_drop" or "Other" in data2 

threshold=0.26
for (i in dummy) {
  table <- table(data[[i]])
  others=names(table[table<=threshold*nrow(data)])
  data[[i]] <- as.factor(data[[i]])
  data2[[i]] <- as.factor(data2[[i]])
  levels(data[[i]])[levels(data[[i]]) %in% others] <- "Other_drop"
  levels(data2[[i]])[levels(data2[[i]]) %in% others] <- "Other"
}

#Dummy classifier
#Use of package makedummies
dummies=makedummies(dat=data,col=dummy,basal_level = T)


#drop les dummies
df <- data[, ! colnames(data) %in% dummy, drop = F]
#drop les to drop
to_drop <- gsub(pattern="[[:punct:^_]]",replacement=".",x=to_drop)
to_drop <- gsub(pattern="[-:(), ]",replacement=".",x=to_drop)
df <- df[, ! colnames(df) %in% to_drop, drop = F]
df <- cbind(df,dummies)

#Removal of columns with a unique value
uniques <- c("UniqueID")
for (i in colnames(data)) {
  if (length(unique(df[[i]]))==1) uniques <- c(uniques,i) 
}
df <- df[, ! colnames(df) %in% uniques, drop = F]

#Removal of columns _drop or _0
new_drops=c()
for (i in colnames(df)) {
  if (substr(i,start=nchar(i)-4,nchar(i))=='_drop'|substr(i,start=nchar(i)-1,nchar(i))=='_0') new_drops <- c(new_drops,i) 
}
df <- df[, ! colnames(df) %in% new_drops, drop = F]
for (i in colnames(df)) {
  df[[i]] <- as.numeric(as.vector(df[[i]]))
}

# Heatmap of features with high correlations
# Search of correlated variables
variables=c()
for (i in colnames(df)) {
  for (j in colnames(df)) {
    if (i != j){
      correl=cor(df[[i]], df[[j]], method="pearson")
      if (abs(correl)>0.95) 
        {
        variables <- c(variables,i)
        break
        }
    }
  }
}
  
corr2=round(cor(df[variables]),2)


library(reshape2)
melted_corr2 <- melt(corr2)

# Display heatmap to select which more feature to remove
library(ggplot2)

# Give extreme colors:
ggplot(melted_corr2, aes(Var1, Var2, fill= value)) + 
  geom_tile(color = "black") +
  scale_fill_gradient2(low = "#075AFF",
                     mid = "#FFFFCC",
                     high = "#FF0000") +
  geom_text(aes(label = value), color = "white", size = 4)

############ EXTRACTION OF CORRELATIONS USING ML MODEL ############

# Frist identify features for classification or regression 
# We extract the features which only have 2 values this will be the classifiers

classifier_features <- c()
for (i in colnames(df)) {
  if (length(unique(df[[i]]))==2) classifier_features <- c(classifier_features,i)
}

# We extract the list of features which have parent questions to extract only the relevant lines





