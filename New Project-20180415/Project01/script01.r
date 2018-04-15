#Read the text file into a vecor
all<-scan(file="G:/My Drive/R/MNC/RScripts/chem5ch05.txt", what="character");

#initializing variables
wholeQ<-character();
question<-character();

#SECTION A: Finding All Questions and Separating Questions
############################################################
k=1;  #initializing question counter

#Scanning for the start of question number k
#If question k is found, then the inside of the while {} statement is performed
while (paste(k,".",sep="") %in% all) 
{
  #Detecting the start of question number k
  thisQ = paste(k,".",sep="");    #creating string to detect start of question number k
  startQ = match(thisQ,all);      #index in all for the start of question number k
  #Recognizing end of question
  nextQ = paste(k+1,".",sep="");  #creating string to detect start of question number k+1
  # determining the index for end of question number k in vector all
  #   by identifying the begining of question number k+1
  endQ = match(nextQ,all)-1;
  
  #Verifying that there is a next question (k+1) 
  # by checking that there is a start for the next question, and
  # by checking that the end of question k comes after the start of question k
  
  if (!is.na(endQ) & endQ > startQ ) 
  {
    #Extracting question number k
    wholeQ[k] = paste(all[startQ:endQ],collapse=" ");
  } else  {
    # If the last question has been reached 
    # the end of the last question is detected by its MSC label
    endQ = match("MSC:",all[startQ:length(all)]);
    endQ = endQ + startQ;
    wholeQ[k] = paste(all[startQ:endQ],collapse=" ");
    n=k;
    k=99999999
  }
  k=k+1;
}
#Reporting the number of questions detected
print(paste("##    ",n,"questions were detected"));
print("### Section A Completed");

#Section B: Separating the Information for Each Question
#########################################################
# Each piece of information for a question is placed 
# in a single row of the dataframe

#Declaring the dataframe where the information will be stored
#df <- data.frame(
#                  N=character(),
#                 TEXT=character(), 
#                  A=character(),
#                  B=character(),
#                  C=character(),
#                  D=character(),
#                  E=character(),
#                  ANS=character(),
#                  DIF=character(),
#                  REF=character(),
#                  OBJ=character(),
#                  MSC=character()
#               )
#Creating empty dataframe with the correct number of rows (n)
#The columns will be added as the data is generated 
df <- data.frame(matrix(0, ncol = 0, nrow = n))

# Creating a row for each detected question and assigning seqential numbers (n).
df$N=c(as.character(1:n));
#Assigning Blank characters to all other Columns
# and deciding the order of the columns in the dataframe
df$TEXT=" ";
df$REF=" ";
df$DIF=" ";
df$OBJ=" ";
df$MSC=" ";
df$ANS=" ";
df$A=" ";
df$B=" ";
df$C=" ";
df$D=" ";
df$E=" ";

# Sweeping all questions from 1 to n with counter k
# in order to extract the information from each of the fields.
k=1; #initializing reference counter
while (k <= n) 
  ### {print(paste("k=",k," n=",n));
{
  #Breaking down the question information into words
  #by using spaces as separatior between words
  q=strsplit(wholeQ[k]," ");
  q=unlist(q, use.names = FALSE);
  
  #Retrieving the question's text(TEXT)
  #Using MC a. marker to determine the end of the question
  start=2;
  end=match("a.",q)-1;  
  #extract question's tet
  df$TEXT[k]=paste(q[start:end],collapse=" "); 
  
  #Mapping start of answers a. b. c. d and e.
  apoint = match("a.",q);
  bpoint = match("b.",q);
  cpoint = match("c.",q);
  dpoint = match("d.",q);
  epoint = match("e.",q);
  ANSpoint = match("ANS:",q);
  DIFpoint = match("DIF:",q);
  REFpoint = match("REF:",q);
  OBJpoint = match("OBJ:",q);
  MSCpoint = match("MSC:",q);
  endpoint = length(q);
  pnts = c(apoint, bpoint,cpoint,dpoint,epoint,ANSpoint,DIFpoint,REFpoint,OBJpoint,MSCpoint,endpoint);
  pnts <- pnts[!is.na(pnts)];
  end=min(pnts[which(pnts>apoint)])
  
  
  #Retrieving Multiple Choice a. if it exists.
  if (!is.na(apoint)) 
  { start=apoint
  end=min(pnts[which(pnts>apoint)])-1;
  df$A[k]=paste(q[start:end],collapse=" ");
  }
  #Retrieving Multiple Choice b. if it exists.
  if (!is.na(bpoint))
  { start=bpoint;
  end=min(pnts[which(pnts>bpoint)])-1;
  df$B[k]=paste(q[start:end],collapse=" ");
  }
  
  #Retrieving Multiple Choice c.if it exists.
  if (!is.na(cpoint))
  { start=cpoint;
  end=min(pnts[which(pnts>cpoint)])-1;
  df$C[k]=paste(q[start:end],collapse=" ");
  }
  #Retrieving Multiple Choice d. if it exists.
  if (!is.na(dpoint))
  { start=dpoint;
  end=min(pnts[which(pnts>dpoint)])-1;
  df$D[k]=paste(q[start:end],collapse=" ");
  }
  #Retrieving Multiple Choice e.if it exists.
  if (!is.na(epoint)) 
  {start=epoint;
  end=min(pnts[which(pnts>epoint)])-1;
  df$E[k]=paste(q[start:end],collapse=" ");
  }
  #Retrieving the correct answer, ANS if it exists.
  if (!is.na(ANSpoint)) 
  { start=ANSpoint+1;
  end=min(pnts[which(pnts>ANSpoint)])-1;
  df$ANS[k]=paste(q[start:end],collapse=" ");
  }  
  
  #Retrieving the level of dificulty, DIF if it exists.
  if (!is.na(DIFpoint)) 
  { start=DIFpoint+1;
  end=min(pnts[which(pnts>DIFpoint)])-1;
  df$DIF[k]=paste(q[start:end],collapse=" ");
  }  
  
  #Retrieving the level of dificulty, REF if it exists.
  if (!is.na(REFpoint)) 
  { start=REFpoint+1;
  end=min(pnts[which(pnts>REFpoint)])-1;
  df$REF[k]=paste(q[start:end],collapse=" ");
  }  
  
  #Retrieving the level of dificulty, OBJ if it exists.
  if (!is.na(OBJpoint)) 
  { start=OBJpoint+1;
  end=min(pnts[which(pnts>OBJpoint)])-1;
  df$OBJ[k]=paste(q[start:end],collapse=" ");
  }  
  
  #Retrieving the level of dificulty, MSC if it exists.
  if (!is.na(MSCpoint)) 
  { start=MSCpoint+1;
  end=min(pnts[which(pnts>MSCpoint)]);
  df$MSC[k]=paste(q[start:end],collapse=" ");
  }  
  k=k+1;
}

#Writing the dataframe into a csv file
write.csv(df, file= "extracted question bank info.csv", row.names=FALSE);
