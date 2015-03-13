#This is the code for the 2015 SR on pass-through businesses. It takes data from several sources and produces
#statistics on employment, net income, business size by business form and industry and top marginal tax rates
#NOTE: There is code and data here that the paper does not utilize. However, these data could be used in the
#future.

######################Setup#########################

  # Sets the working directory.
  
    setwd("U:/Documents backup 2-3/GitHub/passthroughs")
  
  #Clears all datasets and variables from memory
  
    rm(list=ls())
  
  #Library for smart bind. It allows you to bind rows while ignoring missing
  
    library(gtools)

###########################Load Data##########################

  #2014 Tax Foundation State Income Tax Data
  
    data<-read.csv("data.csv", header = TRUE, fill = TRUE, sep = ",")
  
  #2012 IRS Historic Table 2 Data: http://www.irs.gov/uac/SOI-Tax-Stats-Historic-Table-2

    table<-read.csv("historictabletwo.csv", header = TRUE, fill = TRUE, sep = ",")
  
  #2011 Census Statistics on U.S. Businesses (SUSB) Data on Employers, Size Breakdown, LFO: http://www.census.gov/econ/susb/data/susb2011.html
    
    lfo<-read.csv("lfo.csv", header = TRUE, fill = TRUE, sep = ",")
  
  #2011 Census Nonemployer Statistics, Size Breakdown, LFO: http://www.census.gov/econ/nonemployer/download.htm
  
    lfononemployers<-read.csv("nonemployers.csv", header = TRUE, fill = TRUE, sep = ",") 
  
  #2011 Census CBP Data on Employers, State breakdown, Payroll, and LFO: http://www.census.gov/econ/cbp/download/11_data/

    statenonemployers<-read.csv("statenonemployers.csv", header = TRUE, fill = TRUE, sep = ",") 
  
  #2011 Census CBP Data on Nonemployers, State breakdown: http://www.census.gov/econ/nonemployer/download.htm

    stateemployers<-read.csv("stateemployers.csv", header = TRUE, fill = TRUE, sep = ",")

  
################Top Marginal Income Tax Rates For Pass-Through Businesses#####
  
  #Set up needed federal parameters
    
    toprate<-.396
  
    medicare<-.029
    
    medicarededuction<-((medicare*.9235)/2)
    
    medicare2<-.009
    
    pease<-0

  #Calculates the top marginal tax rates of sole proprietorships and partnerships
    
    for (i in 1:length(data$state)){
    
      feddeduction<-(1-((toprate)*data$feddeduction[i]))
                     
        if(data$topmarginalrate[i]>0) {
          
          pease<-(.03*.396)
        
        } else {
        
          pease<-0
        
        }
      
      data$soleprop[i]<-(toprate*(1-((data$topmarginalrate[i]*feddeduction)+data$localincome[i])-medicarededuction))+((medicare+medicare2)*.9235)+pease+(data$topmarginalrate[i]*feddeduction)+data$localincome[i]
    
    }
    
  #S-Corps Top marginal tax rates
  
    #This assumes the last dollar is a distribution.
  
    for (i in 1:length(data$state)){
      
      feddeduction<-(1-((toprate)*data$feddeduction[i]))
      
      if(data$topmarginalrate[i]>0) {
      
        pease<-(.03*.396)
      
      } else {
      
        pease<-0
      
      }
      
      data$scorp[i]<-(toprate*(1-((data$topmarginalrate[i]*feddeduction)+data$localincome[i])-0))+((0)*.9235)+pease+(data$topmarginalrate[i]*feddeduction)+data$localincome[i]
      
    }
  
     #Tennessee applies their dividend income tax on S-corporation income

        t<-data$state=="Tennessee"

        feddeduction<-(1-((toprate)*data$feddeduction[t]))
        
        data$scorp[t]<-(toprate*(1-((0.06)+data$localincome[i])-0))+((0)*.9235)+pease+(0.06)+data$localincome[i]

  #S-Corp (Passive Shareholder) Marginal tax rates

    for (i in 1:length(data$state)){
      
      feddeduction<-(1-((toprate)*data$feddeduction[i]))
      
      if(data$topmarginalrate[i]>0) {
        
        pease<-(.03*.396)
      
      } else {
      
        pease<-0
      
      }
      
      data$scorppassive[i]<-(toprate*(1-((data$topmarginalrate[i]*feddeduction)+data$localincome[i])-0))+((0)*.9235)+pease+.038+(data$topmarginalrate[i]*feddeduction)+data$localincome[i]
      
    }

      #Tennessee. Tennessee's Hall tax hits distributions from s corporations because they do not recognize them.
    
        t<-data$state=="Tennessee"
    
        feddeduction<-(1-((toprate)*data$feddeduction[t]))
    
        data$scorppassive[t]<-(toprate*(1-((0.06)+data$localincome[i])-0))+((0)*.9235)+pease+.038+(0.06)+data$localincome[i]

  #National Average Pass Through Marginal Tax Rate
    
    #Sole Proprietors

      solepropnationalaverage<-sum((data$solepropincome/sum(data$solepropincome))*data$soleprop)
  
    #Active S Corporations

      scorpnationalaverage<-sum((data$scorpincome/sum(data$scorpincome))*data$scorp)
  
    #Passive S Corporations

      scorppassiveaverage<-sum((data$scorpincome/sum(data$scorpincome))*data$scorppassive)

######################Census Data Charts##########################

  #Set up the Census Data for use. This code allows the two datasets (employer and non-employer) to be merged
    
    #Need to clean the lfo data so it is readable by R. Create numeric codes for business size and type
        
      lfo$lfocode<-as.numeric(substring(lfo$lfo,1,1))
      
      lfo$sizecode<-as.numeric(substring(lfo$ENTERPRISESIZE,1,1))
      
    #Convert Factors to numeric
      
      for (i in 5:8){
       
        lfo[,i]<-as.numeric(lfo[,i])
      
      }

    #S corporations are included in "Corporation non employers." I need some sort of multiplier to separate them out

      share<-.75 #confirmed by Census as being 74.5 percent.

  #Total U.S. Employment, Firms, Establishments, Payroll by LFO For the entire country
  
    #Leaves only the industry totals in the dataset, also drops the size (by employment) factor
      
      emptotals<-lfo[lfo$NAICSdes == "Total" & lfo$sizecode == 1,c(1,5:8)]
      
    #The non-employers are firms, establishments, and employment. So add them to each variable by LFO
    #Rows are as follows 1:total 2:corporation 3:S-Corporation 4:Partnership 5:Sole Props
        
      emptotals[c(1,5,4),2:4]<-emptotals[c(1,5,4),2:4]+lfononemployers[c(1,14,15),6]
          
    #Separate calculation of S corps and C corps (need to make an assumption about their distribution)
        
      emptotals[c(2,3),2:4]<-emptotals[c(2,3),2:4]+c(lfononemployers[c(13),6]*(1-share),lfononemployers[c(13),6]*share)

  #Employment, Firms, Establishments, Payroll by LFO and Size of Firm

    #Extracts Employment, firms, establishments, payroll by LFO and size, dropping indusry factors
      
      businesssize<-lfo[lfo$NAICS=="--" & lfo$sizecode>4 & lfo$sizecode != 8,]
      
    #Need to add in non-employer firms (S-corp firms do not have this data)   #OLD CODE, Doesn't work correctly
       
      #Add a new rows for self-employed businesses (This code is lazy)
      #This code sets up a selfemployed dataset so it can be merged with the business size dataset.
      #I admit I do not know why I did it this way. It is so messy.
        
        selfemployed<-businesssize[1:8,]
        
        selfemployed[,1]<-levels(businesssize$lfo)
        
        selfemployed[4]<-factor("0: selfemployed")
        
        selfemployed[9]<-1:8
        
        selfemployed[10]<-1
        
        selfemployed[5:8]<-0
        
        selfemployed[c(1,2,5,4),5:7]<-lfononemployers[c(1,13:15),6]
    
    #bind the self employed dataset to the size dataset
    
      businesssize<-rbind(businesssize,selfemployed)
    
    #Need to divide the S corporations and C corporation by the multiplier
    
      businesssize[c(35),5:7]<-businesssize[c(34),c(5:7)]*(share)
      
      businesssize[c(34),5:7]<-businesssize[c(34),c(5:7)]*(1-share)

  #Employment, Firms, Establishments, Payroll by LFO and Industry
    
    #The challenge with this merge is that:
      #1) factors are a pain in the ass, so a lot of code is used to deal with that
      #2) the NAICS classification names are slightly different across the LFO and nonemployer datasets.
    
    #Industry Dataset : Employment, LFO, Payroll, Etc. (Total U.S. NOT STATE)
      
      #Dropping the size indicators, this time keeping the industry factors
      
        industrytotals<-lfo[lfo$sizecode == 1 & lfo$lfocode < 6,]
      
      #need to match the NAICS variable names in both datasets
        
        lfononemployers1<-lfononemployers
        
        colnames(lfononemployers1)[2] <- "NAICS"
      
      #Get the nonemployer dataset ready for a merge (creating LFO merge codes and matching them to lfo data)
        
        lfononemployers1$lfocode<-as.numeric(lfononemployers1$lfo)
        
        lfononemployers1$lfocode[which(lfononemployers1$lfocode == 4)]<-5
        
        lfononemployers1$lfocode[which(lfononemployers1$lfocode == 3)]<-4
          
          
      #fix the "All industries" in the nonemployer dataset
      
        levels(lfononemployers1$NAICS)[1]<- "--"
        
        sublfononemployers<-lfononemployers1[lfononemployers1$st == 0 & lfononemployers1$rcptot_size == 1,]
      
      #merging the employers and non-employers
      
        industrytotals<-merge(industrytotals, sublfononemployers, by = c("NAICS","lfocode"), all.x = TRUE)
      
      #Need to convert estab to num so i can add them together
      
        industrytotals$estab<-as.numeric(industrytotals$estab)
        
        industrytotals$estab[which(is.na(industrytotals$estab))]<-0
      
      #Before Adding Together the nonemployers and employers, I need to fix the S-C corp split
        
        industrytotals$estab[industrytotals$lfocode == 3]<-industrytotals$estab[industrytotals$lfocode == 2]*(share)
        
        industrytotals$estab[industrytotals$lfocode == 2]<-industrytotals$estab[industrytotals$lfocode == 2]*(1-share)
      
      #Adding together the nonemployers and employer data (remember, for nonemployers, firms, establishments and employment are identical)
      
        industrytotals$numberoffirms<-industrytotals$numberoffirms+industrytotals$estab
        
        industrytotals$numberofestablishments<-industrytotals$numberofestablishments+industrytotals$estab
        
        industrytotals$employment<-industrytotals$employment+industrytotals$estab
  
      #Subset the new dataset: only data I need
        
        industrytotals<-industrytotals[c(1,2,3,4,6,7,8,9)]
        
      #Reshape the Dataset too Make it Easier to Mess With
         
        industrytotals<- reshape(industrytotals,
                                  timevar = "lfocode",
                                  idvar = "NAICS",
                                  direction = "wide"
                                 )
      
      #With the data reshaped, It is now possible to recalculate the private sector total numbers (getting rid of non-profits and governments)
        
        industrytotals$numberoffirms.1<-industrytotals$numberoffirms.2+industrytotals$numberoffirms.3+industrytotals$numberoffirms.4+industrytotals$numberoffirms.5
        
        industrytotals$numberofestablishments.1<-industrytotals$numberofestablishments.2+industrytotals$numberofestablishments.3+industrytotals$numberofestablishments.4+industrytotals$numberofestablishments.5
        
        industrytotals$employment.1<-industrytotals$employment.2+industrytotals$employment.3+industrytotals$employment.4+industrytotals$employment.5
        
        industrytotals$payroll.1<-industrytotals$payroll.2+industrytotals$payroll.3+industrytotals$payroll.4+industrytotals$payroll.5

  #State LEVEL DATA, Employment, LFO, ESTABLISHMENTS
    
    #Uses County Business Patterns Data
    #CBP does not have firm data, only establishments 
    #Need to combine non-employer and employer businesses (2 datasets)
    
    #NONEMPLOYERS DATA
            
      #subset all industries
      
        statenonemployerssub<-subset(statenonemployers, statenonemployers$naics == 0) 
      
      #rename the state variable to match with employer dataset
      
        colnames(statenonemployerssub)[1]<-"fipstate"
      
      #rename so its easier to find in combined dataset
      
        colnames(statenonemployerssub)[5]<-"selfemployed"
      
      #Rename total from "-" to "T"
      
        levels(statenonemployerssub$lfo)[statenonemployerssub$lfo == "-"]<-"T"
  
    #EMPLOYERS DATA
      
      #Reduce datasets to all industries
    
        stateemployerssub<-subset(stateemployers, stateemployers$naics == "------")
      
      #Rename the total to T
      
        levels(stateemployerssub$lfo)[stateemployerssub$lfo == "-"]<-"T"
      
    #COMBINED DATASET
    
      #Merge the datasets together
      
        totalemployment<-merge(stateemployerssub,statenonemployerssub, by = c("fipstate","lfo"), all="TRUE")

      #Need to adjust all the state S corp and C corp data

        totalemployment$selfemployed[totalemployment$lfo == "Z"]<-round(totalemployment$selfemployed[totalemployment$lfo == "C"]*share)
        
        totalemployment$selfemployed[totalemployment$lfo == "C"]<-round(totalemployment$selfemployed[totalemployment$lfo == "C"]*(1-share))
    
      #Adjust employment data for selfemployed
      
        totalemployment$emp<-rowSums(totalemployment[c("emp","selfemployed")],na.rm=TRUE)

    #Share of Employment by business form (All industries) Among all employers
      
      for (i in totalemployment$fipstate){
        
        for (a in levels(totalemployment$lfo)){
          
          totalemployment$shareofemployment[
            
            totalemployment$fipstate == i & totalemployment$lfo == a]<-(
              
              totalemployment$emp[totalemployment$lfo == a & totalemployment$fipstate == i]/totalemployment$emp[totalemployment$lfo == "T" &totalemployment$fipstate == i])
        
        }
      
      }

    #Share of Employment by business form (All industries) Among only businesses
      
      for (i in totalemployment$fipstate){
        
        for (a in levels(totalemployment$lfo)[c(2,6,7,8)]){
          
          totalemployment$shareofemploymentbusiness[
            
            totalemployment$fipstate == i & totalemployment$lfo == a]<-(
              
              totalemployment$emp[totalemployment$lfo == a & totalemployment$fipstate == i]/sum(totalemployment$emp[totalemployment$lfo %in% c("C","Z","S","P") & totalemployment$fipstate == i])) 
        
        }
        
      }
  
    #Complete the final State employment dataset
      
      #Select variables
        
        vars<-c("fipstate","lfo","emp","ap","shareofemployment","shareofemploymentbusiness")
      
        stateemployment<-totalemployment[vars]
      
      #Reshape the data
  
        stateemployment<- reshape(stateemployment,
                                  timevar = "lfo",
                                  idvar = "fipstate",
                                  direction = "wide"
                                  )
  
      
      #Cleaning up the data for presentation
        
        vars<-c("fipstate", "emp.T","ap.T","emp.C","ap.C","shareofemploymentbusiness.C","emp.P","ap.P","shareofemploymentbusiness.P","emp.S","ap.S","shareofemploymentbusiness.S","emp.Z","ap.Z","shareofemploymentbusiness.Z")
        
        stateemployment<-stateemployment[vars]
      
      #Adding in private sector employment and payroll variable      
        
        stateemployment$P<-rowSums(stateemployment[c(4,7,10,13)])
        
        stateemployment$A<-rowSums(stateemployment[c(5,8,11,14)])
                                                          
      #Attach names to the States
        
        stateemployment$StateName<-c("Alabama",
                                     "Alaska",
                                     "Arizona",
                                     "Arkansas",
                                     "California",
                                     "Colorado",
                                     "Connecticut",
                                     "Delaware",
                                     "District of Columbia",
                                     "Florida",
                                     "Georgia",
                                     "Hawaii",
                                     "Idaho",
                                     "Illinois",
                                     "Indiana",
                                     "Iowa",
                                     "Kansas",
                                     "Kentucky",
                                     "Louisiana",
                                     "Maine",
                                     "Maryland",
                                     "Massachusetts",
                                     "Michigan",
                                     "Minnesota",
                                     "Mississippi",
                                     "Missouri",
                                     "Montana",
                                     "Nebraska",
                                     "Nevada",
                                     "New Hampshire",
                                     "New Jersey",
                                     "New Mexico",
                                     "New York",
                                     "North Carolina",
                                     "North Dakota",
                                     "Ohio",
                                     "Oklahoma",
                                     "Oregon",
                                     "Pennsylvania",
                                     "Rhode Island",
                                     "South Carolina",
                                     "South Dakota",
                                     "Tennessee",
                                     "Texas",
                                     "Utah",
                                     "Vermont",
                                     "Virginia",
                                     "Washington",
                                     "West Virginia",
                                     "Wisconsin",
                                     "Wyoming"
                                      )      
      
      #rename variables
          
          colnames(stateemployment)<-c("State",
                                       "Total Employment",
                                       "Total Payroll",
                                       "C Corporate Employment",
                                       "C Corporate Payroll",
                                       "C Corporate Share",
                                       "Partnership Employment",
                                       "Partnership Payroll",
                                       "Partnership Share", 
                                       "Sole Prop Employment",
                                       "Sole Prop Payroll",
                                       "Sole Prop Share",
                                       "S Corp Employment",
                                       "S Corp Payroll",
                                       "S Corp Share",
                                       "Private Sector Employment",
                                       "Private Sector Payroll",
                                       "State Name")
    
