setwd("~/Desktop/2A/cyber_risk/data/csv");

# ETATS POLITIQUES
democrats=c("CA","CO","CT","DE","HI","IL","ME","MD","MA","MN","NV","NH","NJ","NM","NY","OR","RI","VT","WT","VA","WI");
republicans=c("AL","AK","AZ","FL","GA","ID","IN","IA","KS","KY","LA","MS","MO","MT","NE","NC","ND","OH","OK","PA","SC","SD","TX","TN","WV","UT","WY");

extractData<-function(filename){
  attacks<-read.csv(file=filename, header=TRUE, sep=",");
  date_attacks=attacks['Breach.Submission.Date'];
  state_attacks=attacks['State'];
  n=length(date_attacks[[1]]);
  date_attacks_converted=c();
  for (i in 1:n){
    if (is.element(state_attacks[[1]][i],republicans)){
      date_attacks_converted[i]=as.numeric(as.POSIXct(date_attacks[[1]][i], format="%m/%d/%Y"))
    }
  }
  return(date_attacks_converted);
}
DAC<-extractData("breach_report.csv");
hist(DAC,breaks=50,xlab="Date of attacks (timestamp in seconds)",main="Date of attacks from 02/2016 to 02/2018")

#ref=c();
#for (i in 1:n){ref[i]=1}
#plot(DAC,ref,xlab="Date of attack (timestamp)",cex=.3,col="green")
