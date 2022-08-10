# I start by charging the table with only the questions
questions=read.csv('questions.csv',sep=';')
# I transpose it
questions <- t(questions)
#I change the col names with the first line and remove the first line
colnames(questions) <- questions["Question",]
questions <- as.data.frame(questions[-1,])
#What I did in python is not necessary as the mode here is already character

#I charge now the kobo file:
kobo=read.csv('kobo.csv',sep=';')

#I will look at data that I will treat as categories for which I do a special treatment
# I will also try to pre - fill the fact that a variable will be continued
# All this was my own recepi and I am sure it can be much more improved with a
# better knowledge of R and more specially of Kobo. Probably this could be pretty straight
# forward and also reduce the workload in the treatment of the data on the next file
# as long as there would be more consistency in the labelling of features from
# one survey to another (like for HH size, genders... but alos some scorings CSI, FCS...)

for (i in 1:nrow(kobo)) {
  if (kobo[i,"label.English.en."]!="") {
    for (quest in row.names(questions)) {
       if (gsub(pattern="[[:punct:] ]",replacement="",x=kobo[i,'label.English.en.'])==gsub(pattern="[[:punct:] ]",replacement="",x=quest)) {
         #print((gsub(pattern="[[:punct:] ]",replacement="",x=kobo[i,'label.English.en.'])==gsub(pattern="[[:punct:] ]",replacement="",x=quest)))
         #print(gsub(pattern="[[:punct:] ]",replacement="",x=kobo[i,'label.English.en.']))
         #print(kobo[i,'name'])
         #print(gsub(pattern="[. ]",replacement="",x=quest))
         #print(quest)
         if (kobo[i,'type']=='decimal'){
           questions.at[quest,'type']='continue'
         }
         questions[quest,'Idquest']=kobo[i,'name']
         
      } else {
          reference=gsub(pattern="[[:punct:] ]",replacement=".",x=kobo[i,'label.English.en.'])
          if (reference==substr(gsub(pattern="[[:punct:] ]",replacement=".",x=quest),start=1,stop=nchar(reference)) &&
               grepl("[.]{2}",quest)) { 
            split_quest=strsplit(substr(quest,start=nchar(reference),stop=1000),"[.]{2}")
            print(quest)
            questions[quest,'Idquest']=paste(c(kobo[i,'name'],split_quest[[1]]),collapse='_')
            questions[quest,'Treatment']='cat'
            }
        }
      }
    }  
  }

questions <- as.data.frame(t(questions))
write.csv(questions,"questions_autoR.csv")
