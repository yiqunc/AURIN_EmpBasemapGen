# required packages
library(maptools)   # for geospatial services; also loads foreign and sp
library(rgdal)      # for map projection work; also loads sp
library(rgeos)
library(gdata)
library(RJSONIO)
library(PBSmapping) # for GIS_like geospatial object manipulation / anslysis including poly

# set up the working directory
setwd("C:\\Users\\chen1\\Desktop\\datasets\\AURIN_EmploymentDataSets")


ANZSIC.buildAazsicHierarchyOnLevel4.2001 <- function (){
	
	plainAazsicTable = read.table("National_Industry_Tables\\2001 Industry-ANZSIC.txt", header=TRUE, na.strings="NA", sep="\t", blank.lines.skip=TRUE, fill=TRUE, colClasses="character")

  curDiv1="";
  curDiv2="";
  curDiv3="";
  curDiv4="";
  
  curLevel = -1;
  preLevel = -1;
  
  #   ignore the last 4 job cats which are not 
  # 	9700  Private Households Employing Staff
  # 	9900	Non-Classifiable Economic Units
  # 	&&&&	Not stated
  # 	Total	Total
  
	"Div1Code"  "Div2Code"	"Div3Code"	"Div4Code"	"Div4Name"	"ZoneCodes"
	div4DF <- data.frame(Div1Code=NULL, Div2Code=NULL, Div3Code=NULL, Div4Code=NULL, Div4Name=NULL, ZoneCodes=NULL)
  
  
  i = 0
	while(i < nrow(plainAazsicTable)-4){
    i=i+1
	  preLevel = curLevel
    
	  if (regexpr("[A-W]000",plainAazsicTable[i,"CODE"]) > 0){
      
	    #acceptable preLevel is -1, 1 or 4
	    if(preLevel == 2){
	      print("---------------------")
	      #missing lvl3 and lvl4, 
	      print(sprintf("    curDiv3: %s", plainAazsicTable[i-1,"CODE"]))
	      print(sprintf("      curDiv4: %s", plainAazsicTable[i-1,"CODE"]))
	      div4DF <- rbind(div4DF, data.frame(Div1Code=curDiv1, Div2Code=curDiv2, Div3Code=plainAazsicTable[i-1,"CODE"], Div4Code=plainAazsicTable[i-1,"CODE"], Div4Name=plainAazsicTable[i-1,"NAME"], ZoneCodes=""))
	      
	      curLevel = 4
	      i=i-1
	      
	    }else if(preLevel ==3){
	      print("---------------------")
	      #missing lvl4, 
	      print(sprintf("      curDiv4: %s", plainAazsicTable[i-1,"CODE"]))
	      div4DF <- rbind(div4DF, data.frame(Div1Code=curDiv1, Div2Code=curDiv2, Div3Code=plainAazsicTable[i-1,"CODE"], Div4Code=plainAazsicTable[i-1,"CODE"], Div4Name=plainAazsicTable[i-1,"NAME"], ZoneCodes=""))
	      
	      curLevel = 4
	      i=i-1
	      
	    }else{
	      curDiv1 = plainAazsicTable[i,"CODE"]
	      curLevel = 1
	      print(sprintf("curDiv1: %s", plainAazsicTable[i,"CODE"]))
	      
	    }
      
      
	  } else if (regexpr("[0-9][0-9]00",plainAazsicTable[i,"CODE"]) > 0){
      
      #acceptable preLevel is 1 or 4

        if(preLevel == 2){
          print("---------------------")
          #missing lvl3 and lvl4, 
          print(sprintf("    curDiv3: %s", plainAazsicTable[i-1,"CODE"]))
          print(sprintf("      curDiv4: %s", plainAazsicTable[i-1,"CODE"]))
          div4DF <- rbind(div4DF, data.frame(Div1Code=curDiv1, Div2Code=curDiv2, Div3Code=plainAazsicTable[i-1,"CODE"], Div4Code=plainAazsicTable[i-1,"CODE"], Div4Name=plainAazsicTable[i-1,"NAME"], ZoneCodes=""))
          
          curLevel = 4
          i=i-1
          
        }else if(preLevel ==3){
          print("---------------------")
          #missing lvl4, 
          print(sprintf("      curDiv4: %s", plainAazsicTable[i-1,"CODE"]))
          div4DF <- rbind(div4DF, data.frame(Div1Code=curDiv1, Div2Code=curDiv2, Div3Code=plainAazsicTable[i-1,"CODE"], Div4Code=plainAazsicTable[i-1,"CODE"], Div4Name=plainAazsicTable[i-1,"NAME"], ZoneCodes=""))
          
          curLevel = 4
          i=i-1
          
        }else{
          curDiv2 = plainAazsicTable[i,"CODE"]
          curLevel = 2
          print(sprintf("  curDiv2: %s", plainAazsicTable[i,"CODE"]))
          
        }
      
    } else if (regexpr("[0-9][0-9][0-9]0",plainAazsicTable[i,"CODE"]) > 0){
      
	    #acceptable preLevel is 2 or 4
	    if(preLevel ==3){
	      print("---------------------")
	      #missing lvl4, 
	      print(sprintf("      curDiv4: %s", plainAazsicTable[i-1,"CODE"]))
	      div4DF <- rbind(div4DF, data.frame(Div1Code=curDiv1, Div2Code=curDiv2, Div3Code=plainAazsicTable[i-1,"CODE"], Div4Code=plainAazsicTable[i-1,"CODE"], Div4Name=plainAazsicTable[i-1,"NAME"], ZoneCodes=""))
	      
	      curLevel = 4
        i=i-1
        
	    }else{
	      curDiv3 = plainAazsicTable[i,"CODE"]
	      curLevel = 3
	      print(sprintf("    curDiv3: %s", plainAazsicTable[i,"CODE"]))
	    }
    
	  } else if (regexpr("[0-9][0-9][0-9][0-9]",plainAazsicTable[i,"CODE"]) > 0){
	    
	    #acceptable preLevel is 3 or 4
	    if(preLevel ==2){
	      print("---------------------")
	      #missing lvl3, 
	      print(sprintf("    curDiv3: %s", plainAazsicTable[i-1,"CODE"]))
	      curDiv3 = plainAazsicTable[i-1,"CODE"]
	      curLevel = 3
	      i=i-1
	    }else{
	      curDiv4 = plainAazsicTable[i,"CODE"]
	      curLevel = 4
	      print(sprintf("      curDiv4: %s", plainAazsicTable[i,"CODE"]))
	      div4DF <- rbind(div4DF, data.frame(Div1Code=curDiv1, Div2Code=curDiv2, Div3Code=curDiv3, Div4Code=curDiv4, Div4Name=plainAazsicTable[i,"NAME"], ZoneCodes=""))
	      
	    }
      
	  }
	}
	
  # format Div1Code, Div2Code, Div3Code by removing extra 0
	div4DF[,1] = sub("000","",div4DF[,1])
	div4DF[,2] = sub("00","",div4DF[,2])
	div4DF[,3] = sub("00","",paste(div4DF[,3], "0", sep = ""))
	
	write.table(div4DF, file="National_Industry_Tables\\2001_div4_v1.csv", row.names = FALSE, sep="\t")
}

ANZSIC.buildAazsicHierarchyOnLevel4.1996 <- function (){
  
  plainAazsicTable = read.table("National_Industry_Tables\\1996 Industry-ANZSIC.txt", header=TRUE, na.strings="NA", sep="\t", blank.lines.skip=TRUE, fill=TRUE, colClasses="character")
  
  curDiv1="";
  curDiv2="";
  curDiv3="";
  curDiv4="";
  
  curLevel = -1;
  preLevel = -1;
  
  #   ignore the last 4 job cats which are not 
  # 	9700  Private Households Employing Staff
  # 	9900	Non-Classifiable Economic Units
  # 	&&&&	Not stated
  # 	Total	Total
  
  "Div1Code"  "Div2Code"	"Div3Code"	"Div4Code"	"Div4Name"	"ZoneCodes"
  div4DF <- data.frame(Div1Code=NULL, Div2Code=NULL, Div3Code=NULL, Div4Code=NULL, Div4Name=NULL, ZoneCodes=NULL)
  
  
  i = 0
  while(i < nrow(plainAazsicTable)-4){
    i=i+1
    preLevel = curLevel
    
    if (regexpr("[A-W]000",plainAazsicTable[i,"CODE"]) > 0){
      
      #acceptable preLevel is -1, 1 or 4
      if(preLevel == 2){
        print("---------------------")
        #missing lvl3 and lvl4, 
        print(sprintf("    curDiv3: %s", plainAazsicTable[i-1,"CODE"]))
        print(sprintf("      curDiv4: %s", plainAazsicTable[i-1,"CODE"]))
        div4DF <- rbind(div4DF, data.frame(Div1Code=curDiv1, Div2Code=curDiv2, Div3Code=plainAazsicTable[i-1,"CODE"], Div4Code=plainAazsicTable[i-1,"CODE"], Div4Name=plainAazsicTable[i-1,"NAME"], ZoneCodes=""))
        
        curLevel = 4
        i=i-1
        
      }else if(preLevel ==3){
        print("---------------------")
        #missing lvl4, 
        print(sprintf("      curDiv4: %s", plainAazsicTable[i-1,"CODE"]))
        div4DF <- rbind(div4DF, data.frame(Div1Code=curDiv1, Div2Code=curDiv2, Div3Code=plainAazsicTable[i-1,"CODE"], Div4Code=plainAazsicTable[i-1,"CODE"], Div4Name=plainAazsicTable[i-1,"NAME"], ZoneCodes=""))
        
        curLevel = 4
        i=i-1
        
      }else{
        curDiv1 = plainAazsicTable[i,"CODE"]
        curLevel = 1
        print(sprintf("curDiv1: %s", plainAazsicTable[i,"CODE"]))
        
      }
      
      
    } else if (regexpr("[0-9][0-9]00",plainAazsicTable[i,"CODE"]) > 0){
      
      #acceptable preLevel is 1 or 4
      
      if(preLevel == 2){
        print("---------------------")
        #missing lvl3 and lvl4, 
        print(sprintf("    curDiv3: %s", plainAazsicTable[i-1,"CODE"]))
        print(sprintf("      curDiv4: %s", plainAazsicTable[i-1,"CODE"]))
        div4DF <- rbind(div4DF, data.frame(Div1Code=curDiv1, Div2Code=curDiv2, Div3Code=plainAazsicTable[i-1,"CODE"], Div4Code=plainAazsicTable[i-1,"CODE"], Div4Name=plainAazsicTable[i-1,"NAME"], ZoneCodes=""))
        
        curLevel = 4
        i=i-1
        
      }else if(preLevel ==3){
        print("---------------------")
        #missing lvl4, 
        print(sprintf("      curDiv4: %s", plainAazsicTable[i-1,"CODE"]))
        div4DF <- rbind(div4DF, data.frame(Div1Code=curDiv1, Div2Code=curDiv2, Div3Code=plainAazsicTable[i-1,"CODE"], Div4Code=plainAazsicTable[i-1,"CODE"], Div4Name=plainAazsicTable[i-1,"NAME"], ZoneCodes=""))
        
        curLevel = 4
        i=i-1
        
      }else{
        curDiv2 = plainAazsicTable[i,"CODE"]
        curLevel = 2
        print(sprintf("  curDiv2: %s", plainAazsicTable[i,"CODE"]))
        
      }
      
    } else if (regexpr("[0-9][0-9][0-9]0",plainAazsicTable[i,"CODE"]) > 0){
      
      #acceptable preLevel is 2 or 4
      if(preLevel ==3){
        print("---------------------")
        #missing lvl4, 
        print(sprintf("      curDiv4: %s", plainAazsicTable[i-1,"CODE"]))
        div4DF <- rbind(div4DF, data.frame(Div1Code=curDiv1, Div2Code=curDiv2, Div3Code=plainAazsicTable[i-1,"CODE"], Div4Code=plainAazsicTable[i-1,"CODE"], Div4Name=plainAazsicTable[i-1,"NAME"], ZoneCodes=""))
        
        curLevel = 4
        i=i-1
        
      }else{
        curDiv3 = plainAazsicTable[i,"CODE"]
        curLevel = 3
        print(sprintf("    curDiv3: %s", plainAazsicTable[i,"CODE"]))
      }
      
    } else if (regexpr("[0-9][0-9][0-9][0-9]",plainAazsicTable[i,"CODE"]) > 0){
      
      #acceptable preLevel is 3 or 4
      if(preLevel ==2){
        print("---------------------")
        #missing lvl3, 
        print(sprintf("    curDiv3: %s", plainAazsicTable[i-1,"CODE"]))
        curDiv3 = plainAazsicTable[i-1,"CODE"]
        curLevel = 3
        i=i-1
      }else{
        curDiv4 = plainAazsicTable[i,"CODE"]
        curLevel = 4
        print(sprintf("      curDiv4: %s", plainAazsicTable[i,"CODE"]))
        div4DF <- rbind(div4DF, data.frame(Div1Code=curDiv1, Div2Code=curDiv2, Div3Code=curDiv3, Div4Code=curDiv4, Div4Name=plainAazsicTable[i,"NAME"], ZoneCodes=""))
        
      }
      
    }
  }
  
  # format Div1Code, Div2Code, Div3Code by removing extra 0
  div4DF[,1] = sub("000","",div4DF[,1])
  div4DF[,2] = sub("00","",div4DF[,2])
  div4DF[,3] = sub("00","",paste(div4DF[,3], "0", sep = ""))
  
  write.table(div4DF, file="National_Industry_Tables\\1996_div4_v1.csv", row.names = FALSE, sep="\t")
}

ANZSIC.buildAazsicHierarchyOnLevel4.1991 <- function (){
  
  plainAazsicTable = read.table("National_Industry_Tables\\1991 Industry-ANZSIC.txt", header=TRUE, na.strings="NA", sep="\t", blank.lines.skip=TRUE, fill=TRUE, colClasses="character")
  
  curDiv1="";
  curDiv2="";
  curDiv3="";
  curDiv4="";
  
  curLevel = -1;
  preLevel = -1;
  
  #   ignore the last 4 job cats which are not 
  #9400  Private households employing staff
  #M000	Non-classifiable economic units
  #9990	Industry not stated
  #Total	Total
  
  "Div1Code"  "Div2Code"	"Div3Code"	"Div4Code"	"Div4Name"	"ZoneCodes"
  div4DF <- data.frame(Div1Code=NULL, Div2Code=NULL, Div3Code=NULL, Div4Code=NULL, Div4Name=NULL, ZoneCodes=NULL)
  
  
  i = 0
  while(i < nrow(plainAazsicTable)-4){
    i=i+1
    preLevel = curLevel
    
    if (regexpr("[A-W]000",plainAazsicTable[i,"CODE"]) > 0){
      
      #acceptable preLevel is -1, 1 or 4
      if(preLevel == 2){
        print("---------------------")
        #missing lvl3 and lvl4, 
        print(sprintf("    curDiv3: %s", plainAazsicTable[i-1,"CODE"]))
        print(sprintf("      curDiv4: %s", plainAazsicTable[i-1,"CODE"]))
        div4DF <- rbind(div4DF, data.frame(Div1Code=curDiv1, Div2Code=curDiv2, Div3Code=plainAazsicTable[i-1,"CODE"], Div4Code=plainAazsicTable[i-1,"CODE"], Div4Name=plainAazsicTable[i-1,"NAME"], ZoneCodes=""))
        
        curLevel = 4
        i=i-1
        
      }else if(preLevel ==3){
        print("---------------------")
        #missing lvl4, 
        print(sprintf("      curDiv4: %s", plainAazsicTable[i-1,"CODE"]))
        div4DF <- rbind(div4DF, data.frame(Div1Code=curDiv1, Div2Code=curDiv2, Div3Code=plainAazsicTable[i-1,"CODE"], Div4Code=plainAazsicTable[i-1,"CODE"], Div4Name=plainAazsicTable[i-1,"NAME"], ZoneCodes=""))
        
        curLevel = 4
        i=i-1
        
      }else{
        curDiv1 = plainAazsicTable[i,"CODE"]
        curLevel = 1
        print(sprintf("curDiv1: %s", plainAazsicTable[i,"CODE"]))
        
      }
      
      
    } else if (regexpr("[0-9][0-9]00",plainAazsicTable[i,"CODE"]) > 0){
      
      #acceptable preLevel is 1 or 4
      
      if(preLevel == 2){
        print("---------------------")
        #missing lvl3 and lvl4, 
        print(sprintf("    curDiv3: %s", plainAazsicTable[i-1,"CODE"]))
        print(sprintf("      curDiv4: %s", plainAazsicTable[i-1,"CODE"]))
        div4DF <- rbind(div4DF, data.frame(Div1Code=curDiv1, Div2Code=curDiv2, Div3Code=plainAazsicTable[i-1,"CODE"], Div4Code=plainAazsicTable[i-1,"CODE"], Div4Name=plainAazsicTable[i-1,"NAME"], ZoneCodes=""))
        
        curLevel = 4
        i=i-1
        
      }else if(preLevel ==3){
        print("---------------------")
        #missing lvl4, 
        print(sprintf("      curDiv4: %s", plainAazsicTable[i-1,"CODE"]))
        div4DF <- rbind(div4DF, data.frame(Div1Code=curDiv1, Div2Code=curDiv2, Div3Code=plainAazsicTable[i-1,"CODE"], Div4Code=plainAazsicTable[i-1,"CODE"], Div4Name=plainAazsicTable[i-1,"NAME"], ZoneCodes=""))
        
        curLevel = 4
        i=i-1
        
      }else{
        curDiv2 = plainAazsicTable[i,"CODE"]
        curLevel = 2
        print(sprintf("  curDiv2: %s", plainAazsicTable[i,"CODE"]))
        
      }
      
    } else if (regexpr("[0-9][0-9][0-9]0",plainAazsicTable[i,"CODE"]) > 0){
      
      #acceptable preLevel is 2 or 4
      if(preLevel ==3){
        print("---------------------")
        #missing lvl4, 
        print(sprintf("      curDiv4: %s", plainAazsicTable[i-1,"CODE"]))
        div4DF <- rbind(div4DF, data.frame(Div1Code=curDiv1, Div2Code=curDiv2, Div3Code=plainAazsicTable[i-1,"CODE"], Div4Code=plainAazsicTable[i-1,"CODE"], Div4Name=plainAazsicTable[i-1,"NAME"], ZoneCodes=""))
        
        curLevel = 4
        i=i-1
        
      }else{
        curDiv3 = plainAazsicTable[i,"CODE"]
        curLevel = 3
        print(sprintf("    curDiv3: %s", plainAazsicTable[i,"CODE"]))
      }
      
    } else if (regexpr("[0-9][0-9][0-9][0-9]",plainAazsicTable[i,"CODE"]) > 0){
      
      #acceptable preLevel is 3 or 4
      if(preLevel ==2){
        print("---------------------")
        #missing lvl3, 
        print(sprintf("    curDiv3: %s", plainAazsicTable[i-1,"CODE"]))
        curDiv3 = plainAazsicTable[i-1,"CODE"]
        curLevel = 3
        i=i-1
      }else{
        curDiv4 = plainAazsicTable[i,"CODE"]
        curLevel = 4
        print(sprintf("      curDiv4: %s", plainAazsicTable[i,"CODE"]))
        div4DF <- rbind(div4DF, data.frame(Div1Code=curDiv1, Div2Code=curDiv2, Div3Code=curDiv3, Div4Code=curDiv4, Div4Name=plainAazsicTable[i,"NAME"], ZoneCodes=""))
        
      }
      
    }
  }
  
  # format Div1Code, Div2Code, Div3Code by removing extra 0
  div4DF[,1] = sub("000","",div4DF[,1])
  div4DF[,2] = sub("00","",div4DF[,2])
  div4DF[,3] = sub("00","",paste(div4DF[,3], "0", sep = ""))
  
  write.table(div4DF, file="National_Industry_Tables\\1991_div4_v1.csv", row.names = FALSE, sep="\t")
}

ANZSIC.buildAazsicHierarchyOnLevel4.1986 <- function (){
  
  plainAazsicTable = read.table("National_Industry_Tables\\1986 Industry-ANZSIC.txt", header=TRUE, na.strings="NA", sep="\t", blank.lines.skip=TRUE, fill=TRUE, colClasses="character")
  
  curDiv1="";
  curDiv2="";
  curDiv3="";
  curDiv4="";
  
  curLevel = -1;
  preLevel = -1;
  
  #   ignore the last 4 job cats which are not 
  #9400  Private households employing staff
  #M000  Non-classifiable economic units
  #9990	Industry not stated
  #Total	Total
  
  "Div1Code"  "Div2Code"	"Div3Code"	"Div4Code"	"Div4Name"	"ZoneCodes"
  div4DF <- data.frame(Div1Code=NULL, Div2Code=NULL, Div3Code=NULL, Div4Code=NULL, Div4Name=NULL, ZoneCodes=NULL)
  
  
  i = 0
  while(i < nrow(plainAazsicTable)-4){
    i=i+1
    preLevel = curLevel
    
    if (regexpr("[A-W]000",plainAazsicTable[i,"CODE"]) > 0){
      
      #acceptable preLevel is -1, 1 or 4
      if(preLevel == 2){
        print("---------------------")
        #missing lvl3 and lvl4, 
        print(sprintf("    curDiv3: %s", plainAazsicTable[i-1,"CODE"]))
        print(sprintf("      curDiv4: %s", plainAazsicTable[i-1,"CODE"]))
        div4DF <- rbind(div4DF, data.frame(Div1Code=curDiv1, Div2Code=curDiv2, Div3Code=plainAazsicTable[i-1,"CODE"], Div4Code=plainAazsicTable[i-1,"CODE"], Div4Name=plainAazsicTable[i-1,"NAME"], ZoneCodes=""))
        
        curLevel = 4
        i=i-1
        
      }else if(preLevel ==3){
        print("---------------------")
        #missing lvl4, 
        print(sprintf("      curDiv4: %s", plainAazsicTable[i-1,"CODE"]))
        div4DF <- rbind(div4DF, data.frame(Div1Code=curDiv1, Div2Code=curDiv2, Div3Code=plainAazsicTable[i-1,"CODE"], Div4Code=plainAazsicTable[i-1,"CODE"], Div4Name=plainAazsicTable[i-1,"NAME"], ZoneCodes=""))
        
        curLevel = 4
        i=i-1
        
      }else{
        curDiv1 = plainAazsicTable[i,"CODE"]
        curLevel = 1
        print(sprintf("curDiv1: %s", plainAazsicTable[i,"CODE"]))
        
      }
      
      
    } else if (regexpr("[0-9][0-9]00",plainAazsicTable[i,"CODE"]) > 0){
      
      #acceptable preLevel is 1 or 4
      
      if(preLevel == 2){
        print("---------------------")
        #missing lvl3 and lvl4, 
        print(sprintf("    curDiv3: %s", plainAazsicTable[i-1,"CODE"]))
        print(sprintf("      curDiv4: %s", plainAazsicTable[i-1,"CODE"]))
        div4DF <- rbind(div4DF, data.frame(Div1Code=curDiv1, Div2Code=curDiv2, Div3Code=plainAazsicTable[i-1,"CODE"], Div4Code=plainAazsicTable[i-1,"CODE"], Div4Name=plainAazsicTable[i-1,"NAME"], ZoneCodes=""))
        
        curLevel = 4
        i=i-1
        
      }else if(preLevel ==3){
        print("---------------------")
        #missing lvl4, 
        print(sprintf("      curDiv4: %s", plainAazsicTable[i-1,"CODE"]))
        div4DF <- rbind(div4DF, data.frame(Div1Code=curDiv1, Div2Code=curDiv2, Div3Code=plainAazsicTable[i-1,"CODE"], Div4Code=plainAazsicTable[i-1,"CODE"], Div4Name=plainAazsicTable[i-1,"NAME"], ZoneCodes=""))
        
        curLevel = 4
        i=i-1
        
      }else{
        curDiv2 = plainAazsicTable[i,"CODE"]
        curLevel = 2
        print(sprintf("  curDiv2: %s", plainAazsicTable[i,"CODE"]))
        
      }
      
    } else if (regexpr("[0-9][0-9][0-9]0",plainAazsicTable[i,"CODE"]) > 0){
      
      #acceptable preLevel is 2 or 4
      if(preLevel ==3){
        print("---------------------")
        #missing lvl4, 
        print(sprintf("      curDiv4: %s", plainAazsicTable[i-1,"CODE"]))
        div4DF <- rbind(div4DF, data.frame(Div1Code=curDiv1, Div2Code=curDiv2, Div3Code=plainAazsicTable[i-1,"CODE"], Div4Code=plainAazsicTable[i-1,"CODE"], Div4Name=plainAazsicTable[i-1,"NAME"], ZoneCodes=""))
        
        curLevel = 4
        i=i-1
        
      }else{
        curDiv3 = plainAazsicTable[i,"CODE"]
        curLevel = 3
        print(sprintf("    curDiv3: %s", plainAazsicTable[i,"CODE"]))
      }
      
    } else if (regexpr("[0-9][0-9][0-9][0-9]",plainAazsicTable[i,"CODE"]) > 0){
      
      #acceptable preLevel is 3 or 4
      if(preLevel ==2){
        print("---------------------")
        #missing lvl3, 
        print(sprintf("    curDiv3: %s", plainAazsicTable[i-1,"CODE"]))
        curDiv3 = plainAazsicTable[i-1,"CODE"]
        curLevel = 3
        i=i-1
      }else{
        curDiv4 = plainAazsicTable[i,"CODE"]
        curLevel = 4
        print(sprintf("      curDiv4: %s", plainAazsicTable[i,"CODE"]))
        div4DF <- rbind(div4DF, data.frame(Div1Code=curDiv1, Div2Code=curDiv2, Div3Code=curDiv3, Div4Code=curDiv4, Div4Name=plainAazsicTable[i,"NAME"], ZoneCodes=""))
        
      }
      
    }
  }
  
  # format Div1Code, Div2Code, Div3Code by removing extra 0
  div4DF[,1] = sub("000","",div4DF[,1])
  div4DF[,2] = sub("00","",div4DF[,2])
  div4DF[,3] = sub("00","",paste(div4DF[,3], "0", sep = ""))
  
  write.table(div4DF, file="National_Industry_Tables\\1986_div4_v1.csv", row.names = FALSE, sep="\t")
}

ANZSIC.buildAazsicHierarchyOnLevel4.1981 <- function (){
  
  plainAazsicTable = read.table("National_Industry_Tables\\1981 Industry-ANZSIC.txt", header=TRUE, na.strings="NA", sep="\t", blank.lines.skip=TRUE, fill=TRUE, colClasses="character")
  
  curDiv1="";
  curDiv2="";
  curDiv3="";
  curDiv4="";
  
  curLevel = -1;
  preLevel = -1;
  
  #   ignore the last 4 job cats which are not 
  #9400  Private households employing staff
  #M000  Non-classifiable economic units
  #9990  Industry not stated
  #Total	Total
  
  "Div1Code"  "Div2Code"	"Div3Code"	"Div4Code"	"Div4Name"	"ZoneCodes"
  div4DF <- data.frame(Div1Code=NULL, Div2Code=NULL, Div3Code=NULL, Div4Code=NULL, Div4Name=NULL, ZoneCodes=NULL)
  
  
  i = 0
  while(i < nrow(plainAazsicTable)-4){
    i=i+1
    preLevel = curLevel
    
    if (regexpr("[A-W]000",plainAazsicTable[i,"CODE"]) > 0){
      
      #acceptable preLevel is -1, 1 or 4
      if(preLevel == 2){
        print("---------------------")
        #missing lvl3 and lvl4, 
        print(sprintf("    curDiv3: %s", plainAazsicTable[i-1,"CODE"]))
        print(sprintf("      curDiv4: %s", plainAazsicTable[i-1,"CODE"]))
        div4DF <- rbind(div4DF, data.frame(Div1Code=curDiv1, Div2Code=curDiv2, Div3Code=plainAazsicTable[i-1,"CODE"], Div4Code=plainAazsicTable[i-1,"CODE"], Div4Name=plainAazsicTable[i-1,"NAME"], ZoneCodes=""))
        
        curLevel = 4
        i=i-1
        
      }else if(preLevel ==3){
        print("---------------------")
        #missing lvl4, 
        print(sprintf("      curDiv4: %s", plainAazsicTable[i-1,"CODE"]))
        div4DF <- rbind(div4DF, data.frame(Div1Code=curDiv1, Div2Code=curDiv2, Div3Code=plainAazsicTable[i-1,"CODE"], Div4Code=plainAazsicTable[i-1,"CODE"], Div4Name=plainAazsicTable[i-1,"NAME"], ZoneCodes=""))
        
        curLevel = 4
        i=i-1
        
      }else{
        curDiv1 = plainAazsicTable[i,"CODE"]
        curLevel = 1
        print(sprintf("curDiv1: %s", plainAazsicTable[i,"CODE"]))
        
      }
      
      
    } else if (regexpr("[0-9][0-9]00",plainAazsicTable[i,"CODE"]) > 0){
      
      #acceptable preLevel is 1 or 4
      
      if(preLevel == 2){
        print("---------------------")
        #missing lvl3 and lvl4, 
        print(sprintf("    curDiv3: %s", plainAazsicTable[i-1,"CODE"]))
        print(sprintf("      curDiv4: %s", plainAazsicTable[i-1,"CODE"]))
        div4DF <- rbind(div4DF, data.frame(Div1Code=curDiv1, Div2Code=curDiv2, Div3Code=plainAazsicTable[i-1,"CODE"], Div4Code=plainAazsicTable[i-1,"CODE"], Div4Name=plainAazsicTable[i-1,"NAME"], ZoneCodes=""))
        
        curLevel = 4
        i=i-1
        
      }else if(preLevel ==3){
        print("---------------------")
        #missing lvl4, 
        print(sprintf("      curDiv4: %s", plainAazsicTable[i-1,"CODE"]))
        div4DF <- rbind(div4DF, data.frame(Div1Code=curDiv1, Div2Code=curDiv2, Div3Code=plainAazsicTable[i-1,"CODE"], Div4Code=plainAazsicTable[i-1,"CODE"], Div4Name=plainAazsicTable[i-1,"NAME"], ZoneCodes=""))
        
        curLevel = 4
        i=i-1
        
      }else{
        curDiv2 = plainAazsicTable[i,"CODE"]
        curLevel = 2
        print(sprintf("  curDiv2: %s", plainAazsicTable[i,"CODE"]))
        
      }
      
    } else if (regexpr("[0-9][0-9][0-9]0",plainAazsicTable[i,"CODE"]) > 0){
      
      #acceptable preLevel is 2 or 4
      if(preLevel ==3){
        print("---------------------")
        #missing lvl4, 
        print(sprintf("      curDiv4: %s", plainAazsicTable[i-1,"CODE"]))
        div4DF <- rbind(div4DF, data.frame(Div1Code=curDiv1, Div2Code=curDiv2, Div3Code=plainAazsicTable[i-1,"CODE"], Div4Code=plainAazsicTable[i-1,"CODE"], Div4Name=plainAazsicTable[i-1,"NAME"], ZoneCodes=""))
        
        curLevel = 4
        i=i-1
        
      }else{
        curDiv3 = plainAazsicTable[i,"CODE"]
        curLevel = 3
        print(sprintf("    curDiv3: %s", plainAazsicTable[i,"CODE"]))
      }
      
    } else if (regexpr("[0-9][0-9][0-9][0-9]",plainAazsicTable[i,"CODE"]) > 0){
      
      #acceptable preLevel is 3 or 4
      if(preLevel ==2){
        print("---------------------")
        #missing lvl3, 
        print(sprintf("    curDiv3: %s", plainAazsicTable[i-1,"CODE"]))
        curDiv3 = plainAazsicTable[i-1,"CODE"]
        curLevel = 3
        i=i-1
      }else{
        curDiv4 = plainAazsicTable[i,"CODE"]
        curLevel = 4
        print(sprintf("      curDiv4: %s", plainAazsicTable[i,"CODE"]))
        div4DF <- rbind(div4DF, data.frame(Div1Code=curDiv1, Div2Code=curDiv2, Div3Code=curDiv3, Div4Code=curDiv4, Div4Name=plainAazsicTable[i,"NAME"], ZoneCodes=""))
        
      }
      
    }
  }
  
  # format Div1Code, Div2Code, Div3Code by removing extra 0
  div4DF[,1] = sub("000","",div4DF[,1])
  div4DF[,2] = sub("00","",div4DF[,2])
  div4DF[,3] = sub("00","",paste(div4DF[,3], "0", sep = ""))
  
  write.table(div4DF, file="National_Industry_Tables\\1981_div4_v1.csv", row.names = FALSE, sep="\t")
}

# ==== 1981
#NO TravelZone Geometry Data
ANZSIC.datamerge.1981.VIC <- function(){}

#SUCCESS
ANZSIC.datamerge.1981.NSW <- function(){
  tmpdf = read.table("National_Industry_Tables\\1981 Industry.txt", sep=",", header=TRUE, na.strings="-", blank.lines.skip = TRUE, fill = TRUE)
  
  
  #remove unwanted columns such as &&&& and @@@@, the column name will be automatically converted to X....
  tmpdf = tmpdf[,!(colnames(tmpdf) %in% c("X9400","X9900","M000","Total"))]
  
  # create a column to identify state
  tmpdf[,"STATECODE"] = substr(tmpdf[, "DZN"],1,1)
  tmpdf[,"TZCODE"] = substr(tmpdf[, "DZN"],nchar(tmpdf[, "DZN"])-2,nchar(tmpdf[, "DZN"]))
  
  filter = tmpdf[,"STATECODE"] == "1"
  targetStateDF = tmpdf[filter,]
  # duplicated TZCODE are : none
  
  tmpmx = as.matrix(targetStateDF)
  tmpmx[which(is.na(tmpmx))] = 0
  
  empDataFrame = as.data.frame(tmpmx)
  
  # join attribute data to polygon data on dzn code, all polygon data rows are kept
  # make sure the shp file is in WGS84 (EPSG:4326)
  
  empPolyRawDataFrame <- readOGR(dsn="NSW\\1981",layer="tz1981",encoding="utf8")
  
  empPolyRawDataFrame@data[,"TZCODE"] = paste("T00000",empPolyRawDataFrame@data[,"tz81"],sep="")
  empPolyRawDataFrame@data[,"TZCODE"] = substr(empPolyRawDataFrame@data[,"TZCODE"],nchar(empPolyRawDataFrame@data[,"TZCODE"])-2,nchar(empPolyRawDataFrame@data[,"TZCODE"]))
  
  empPolyRawDataFrame@data[,"orgSeqId"] = c(1:nrow(empPolyRawDataFrame@data))
  joinedDF = merge(empPolyRawDataFrame@data, empDataFrame, by.x = "TZCODE", by.y = "TZCODE", all.x=TRUE, sort=FALSE)
  
  orderedJoinedDF = joinedDF[order(joinedDF[,"orgSeqId"]), ]
  
  empPolyRawDataFrame@data = orderedJoinedDF
  
  proj4string(empPolyRawDataFrame)<-" +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  writeOGR(obj=empPolyRawDataFrame, dsn="NSW\\1981", layer="DZN_X_Employment_1981_NSW_Orginal", driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
}

#NO TravelZone Geometry Data
ANZSIC.datamerge.1981.SA <- function(){}

# ==== 1986
#NO TravelZone Geometry Data
ANZSIC.datamerge.1986.VIC <- function(){}

#NO TravelZone Geometry Data
ANZSIC.datamerge.1986.NSW <- function(){}

#SUCCESS
ANZSIC.datamerge.1986.SA <- function(){
  tmpdf = read.table("National_Industry_Tables\\1986 Industry.txt", sep=",", header=TRUE, na.strings="-", blank.lines.skip = TRUE, fill = TRUE)
  
  
  #remove unwanted columns such as &&&& and @@@@, the column name will be automatically converted to X....
  tmpdf = tmpdf[,!(colnames(tmpdf) %in% c("X9400","X9900","M000","Total"))]
  
  # create a column to identify state
  tmpdf[,"STATECODE"] = substr(tmpdf[, "DZN"],1,1)
  tmpdf[,"TZCODE"] = substr(tmpdf[, "DZN"],nchar(tmpdf[, "DZN"])-2,nchar(tmpdf[, "DZN"]))
  
  filter = tmpdf[,"STATECODE"] == "4"
  targetStateDF = tmpdf[filter,]
  # duplicated TZCODE are : none
  
  
  tmpmx = as.matrix(targetStateDF)
  tmpmx[which(is.na(tmpmx))] = 0
  
  empDataFrame = as.data.frame(tmpmx)
  
  # join attribute data to polygon data on dzn code, all polygon data rows are kept
  # make sure the shp file is in WGS84 (EPSG:4326)
  
  empPolyRawDataFrame <- readOGR(dsn="SA\\1986",layer="tz1986",encoding="utf8")
  
  empPolyRawDataFrame@data[,"TZCODE"] = paste("T00000",empPolyRawDataFrame@data[,"CODE"],sep="")
  empPolyRawDataFrame@data[,"TZCODE"] = substr(empPolyRawDataFrame@data[,"TZCODE"],nchar(empPolyRawDataFrame@data[,"TZCODE"])-2,nchar(empPolyRawDataFrame@data[,"TZCODE"]))
  
  empPolyRawDataFrame@data[,"orgSeqId"] = c(1:nrow(empPolyRawDataFrame@data))
  joinedDF = merge(empPolyRawDataFrame@data, empDataFrame, by.x = "TZCODE", by.y = "TZCODE", all.x=TRUE, sort=FALSE)
  
  orderedJoinedDF = joinedDF[order(joinedDF[,"orgSeqId"]), ]
  
  empPolyRawDataFrame@data = orderedJoinedDF
  
  proj4string(empPolyRawDataFrame)<-" +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  writeOGR(obj=empPolyRawDataFrame, dsn="SA\\1986", layer="DZN_X_Employment_1986_SA_Orginal", driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
}

# ==== 1991
#NO TravelZone Geometry Data
ANZSIC.datamerge.1991.VIC <- function(){}

#SUCCESS
ANZSIC.datamerge.1991.NSW <- function(){
  tmpdf = read.table("National_Industry_Tables\\1991 Industry.txt", sep=",", header=TRUE, na.strings="-", blank.lines.skip = TRUE, fill = TRUE)
  
  
  #remove unwanted columns such as &&&& and @@@@, the column name will be automatically converted to X....
  tmpdf = tmpdf[,!(colnames(tmpdf) %in% c("X9400","X9900","M000","Total"))]
  
  # create a column to identify state
  tmpdf[,"STATECODE"] = substr(tmpdf[, "DZN"],1,1)
  tmpdf[,"TZCODE"] = substr(tmpdf[, "DZN"],nchar(tmpdf[, "DZN"])-3,nchar(tmpdf[, "DZN"]))
  
  filter = tmpdf[,"STATECODE"] == "1"
  targetStateDF = tmpdf[filter,]
  # duplicated TZCODE are : none
  
  tmpmx = as.matrix(targetStateDF)
  tmpmx[which(is.na(tmpmx))] = 0
  
  empDataFrame = as.data.frame(tmpmx)
  
  # join attribute data to polygon data on dzn code, all polygon data rows are kept
  # make sure the shp file is in WGS84 (EPSG:4326)
  
  empPolyRawDataFrame <- readOGR(dsn="NSW\\1991",layer="tz1991",encoding="utf8")
  
  empPolyRawDataFrame@data[,"TZCODE"] = paste("T00000",empPolyRawDataFrame@data[,"tz91"],sep="")
  empPolyRawDataFrame@data[,"TZCODE"] = substr(empPolyRawDataFrame@data[,"TZCODE"],nchar(empPolyRawDataFrame@data[,"TZCODE"])-3,nchar(empPolyRawDataFrame@data[,"TZCODE"]))
  
  empPolyRawDataFrame@data[,"orgSeqId"] = c(1:nrow(empPolyRawDataFrame@data))
  joinedDF = merge(empPolyRawDataFrame@data, empDataFrame, by.x = "TZCODE", by.y = "TZCODE", all.x=TRUE, sort=FALSE)
  
  orderedJoinedDF = joinedDF[order(joinedDF[,"orgSeqId"]), ]
  
  empPolyRawDataFrame@data = orderedJoinedDF
  
  proj4string(empPolyRawDataFrame)<-" +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  writeOGR(obj=empPolyRawDataFrame, dsn="NSW\\1991", layer="DZN_X_Employment_1991_NSW_Orginal", driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
}

#SUCCESS
ANZSIC.datamerge.1991.SA <- function(){
  tmpdf = read.table("National_Industry_Tables\\1991 Industry.txt", sep=",", header=TRUE, na.strings="-", blank.lines.skip = TRUE, fill = TRUE)
  
  
  #remove unwanted columns such as &&&& and @@@@, the column name will be automatically converted to X....
  tmpdf = tmpdf[,!(colnames(tmpdf) %in% c("X9400","X9900","M000","Total"))]
  
  # create a column to identify state
  tmpdf[,"STATECODE"] = substr(tmpdf[, "DZN"],1,1)
  tmpdf[,"TZCODE"] = substr(tmpdf[, "DZN"],nchar(tmpdf[, "DZN"])-3,nchar(tmpdf[, "DZN"]))
  
  filter = tmpdf[,"STATECODE"] == "4"
  targetStateDF = tmpdf[filter,]
  # duplicated TZCODE are : none
  
  
  tmpmx = as.matrix(targetStateDF)
  tmpmx[which(is.na(tmpmx))] = 0
  
  empDataFrame = as.data.frame(tmpmx)
  
  # join attribute data to polygon data on dzn code, all polygon data rows are kept
  # make sure the shp file is in WGS84 (EPSG:4326)
  
  empPolyRawDataFrame <- readOGR(dsn="SA\\1991",layer="tz1991",encoding="utf8")
  
  empPolyRawDataFrame@data[,"TZCODE"] = paste("T00000",empPolyRawDataFrame@data[,"CODE"],sep="")
  empPolyRawDataFrame@data[,"TZCODE"] = substr(empPolyRawDataFrame@data[,"TZCODE"],nchar(empPolyRawDataFrame@data[,"TZCODE"])-3,nchar(empPolyRawDataFrame@data[,"TZCODE"]))
  
  empPolyRawDataFrame@data[,"orgSeqId"] = c(1:nrow(empPolyRawDataFrame@data))
  joinedDF = merge(empPolyRawDataFrame@data, empDataFrame, by.x = "TZCODE", by.y = "TZCODE", all.x=TRUE, sort=FALSE)
  
  orderedJoinedDF = joinedDF[order(joinedDF[,"orgSeqId"]), ]
  
  empPolyRawDataFrame@data = orderedJoinedDF
  
  proj4string(empPolyRawDataFrame)<-" +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  writeOGR(obj=empPolyRawDataFrame, dsn="SA\\1991", layer="DZN_X_Employment_1991_SA_Orginal", driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
}

# ==== 1996
#SUCCESS
ANZSIC.datamerge.1996.VIC <- function(){
  tmpdf = read.table("National_Industry_Tables\\1996 Industry.txt", sep=",", header=TRUE, na.strings="-", blank.lines.skip = TRUE, fill = TRUE)
  
  
  #remove unwanted columns such as &&&& and @@@@, the column name will be automatically converted to X....
  tmpdf = tmpdf[,!(colnames(tmpdf) %in% c("X9700","X9900","X....","Total"))]
  
  # create a column to identify state
  tmpdf[,"STATECODE"] = substr(tmpdf[, "DZN"],1,1)
  tmpdf[,"TZCODE"] = substr(tmpdf[, "DZN"],nchar(tmpdf[, "DZN"])-3,nchar(tmpdf[, "DZN"]))
  
  filter = tmpdf[,"STATECODE"] == "2"
  targetStateDF = tmpdf[filter,]
  # duplicated TZCODE are : none
  
  
  tmpmx = as.matrix(targetStateDF)
  tmpmx[which(is.na(tmpmx))] = 0
  
  empDataFrame = as.data.frame(tmpmx)
  
  # join attribute data to polygon data on dzn code, all polygon data rows are kept
  # make sure the shp file is in WGS84 (EPSG:4326)
  
  empPolyRawDataFrame <- readOGR(dsn="VIC\\1996",layer="tz1996",encoding="utf8")
  
  empPolyRawDataFrame@data[,"TZCODE"] = paste("T00000",empPolyRawDataFrame@data[,"TZNUM"],sep="")
  empPolyRawDataFrame@data[,"TZCODE"] = substr(empPolyRawDataFrame@data[,"TZCODE"],nchar(empPolyRawDataFrame@data[,"TZCODE"])-3,nchar(empPolyRawDataFrame@data[,"TZCODE"]))
  
  empPolyRawDataFrame@data[,"orgSeqId"] = c(1:nrow(empPolyRawDataFrame@data))
  joinedDF = merge(empPolyRawDataFrame@data, empDataFrame, by.x = "TZCODE", by.y = "TZCODE", all.x=TRUE, sort=FALSE)
  
  orderedJoinedDF = joinedDF[order(joinedDF[,"orgSeqId"]), ]
  
  empPolyRawDataFrame@data = orderedJoinedDF
  
  proj4string(empPolyRawDataFrame)<-" +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  writeOGR(obj=empPolyRawDataFrame, dsn="VIC\\1996", layer="DZN_X_Employment_1996_VIC_Orginal", driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
}

#SUCCESS
ANZSIC.datamerge.1996.NSW <- function(){
  tmpdf = read.table("National_Industry_Tables\\1996 Industry.txt", sep=",", header=TRUE, na.strings="-", blank.lines.skip = TRUE, fill = TRUE)
  
  
  #remove unwanted columns such as &&&& and @@@@, the column name will be automatically converted to X....
  tmpdf = tmpdf[,!(colnames(tmpdf) %in% c("X9700","X9900","X....","Total"))]
  
  # create a column to identify state
  tmpdf[,"STATECODE"] = substr(tmpdf[, "DZN"],1,1)
  tmpdf[,"TZCODE"] = substr(tmpdf[, "DZN"],nchar(tmpdf[, "DZN"])-3,nchar(tmpdf[, "DZN"]))
  
  filter = tmpdf[,"STATECODE"] == "1"
  targetStateDF = tmpdf[filter,]
  # duplicated TZCODE are : none
 
  tmpmx = as.matrix(targetStateDF)
  tmpmx[which(is.na(tmpmx))] = 0
  
  empDataFrame = as.data.frame(tmpmx)
  
  # join attribute data to polygon data on dzn code, all polygon data rows are kept
  # make sure the shp file is in WGS84 (EPSG:4326)
  
  empPolyRawDataFrame <- readOGR(dsn="NSW\\1996",layer="tz1996GMA",encoding="utf8")
  
  empPolyRawDataFrame@data[,"TZCODE"] = paste("T00000",empPolyRawDataFrame@data[,"tz96"],sep="")
  empPolyRawDataFrame@data[,"TZCODE"] = substr(empPolyRawDataFrame@data[,"TZCODE"],nchar(empPolyRawDataFrame@data[,"TZCODE"])-3,nchar(empPolyRawDataFrame@data[,"TZCODE"]))
  
  empPolyRawDataFrame@data[,"orgSeqId"] = c(1:nrow(empPolyRawDataFrame@data))
  joinedDF = merge(empPolyRawDataFrame@data, empDataFrame, by.x = "TZCODE", by.y = "TZCODE", all.x=TRUE, sort=FALSE)
  
  orderedJoinedDF = joinedDF[order(joinedDF[,"orgSeqId"]), ]
  
  empPolyRawDataFrame@data = orderedJoinedDF
  
  proj4string(empPolyRawDataFrame)<-" +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  writeOGR(obj=empPolyRawDataFrame, dsn="NSW\\1996", layer="DZN_X_Employment_1996_NSW_Orginal", driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
}

#SUCCESS
ANZSIC.datamerge.1996.SA <- function(){
  tmpdf = read.table("National_Industry_Tables\\1996 Industry.txt", sep=",", header=TRUE, na.strings="-", blank.lines.skip = TRUE, fill = TRUE)
  
  
  #remove unwanted columns such as &&&& and @@@@, the column name will be automatically converted to X....
  tmpdf = tmpdf[,!(colnames(tmpdf) %in% c("X9700","X9900","X....","Total"))]
  
  # create a column to identify state
  tmpdf[,"STATECODE"] = substr(tmpdf[, "DZN"],1,1)
  tmpdf[,"TZCODE"] = substr(tmpdf[, "DZN"],nchar(tmpdf[, "DZN"])-3,nchar(tmpdf[, "DZN"]))
  
  filter = tmpdf[,"STATECODE"] == "4"
  targetStateDF = tmpdf[filter,]
  # duplicated TZCODE are : none
  
  
  tmpmx = as.matrix(targetStateDF)
  tmpmx[which(is.na(tmpmx))] = 0
  
  empDataFrame = as.data.frame(tmpmx)
  
  # join attribute data to polygon data on dzn code, all polygon data rows are kept
  # make sure the shp file is in WGS84 (EPSG:4326)
  
  empPolyRawDataFrame <- readOGR(dsn="SA\\1996",layer="tz1996",encoding="utf8")
  
  empPolyRawDataFrame@data[,"TZCODE"] = paste("T00000",empPolyRawDataFrame@data[,"DZN96"],sep="")
  empPolyRawDataFrame@data[,"TZCODE"] = substr(empPolyRawDataFrame@data[,"TZCODE"],nchar(empPolyRawDataFrame@data[,"TZCODE"])-3,nchar(empPolyRawDataFrame@data[,"TZCODE"]))
  
  empPolyRawDataFrame@data[,"orgSeqId"] = c(1:nrow(empPolyRawDataFrame@data))
  joinedDF = merge(empPolyRawDataFrame@data, empDataFrame, by.x = "TZCODE", by.y = "TZCODE", all.x=TRUE, sort=FALSE)
  
  orderedJoinedDF = joinedDF[order(joinedDF[,"orgSeqId"]), ]
  
  empPolyRawDataFrame@data = orderedJoinedDF
  
  proj4string(empPolyRawDataFrame)<-" +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  writeOGR(obj=empPolyRawDataFrame, dsn="SA\\1996", layer="DZN_X_Employment_1996_SA_Orginal", driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
}

# ==== 2001
#SUCCESS
ANZSIC.datamerge.2001.VIC <- function(){
  tmpdf = read.table("National_Industry_Tables\\2001 Industry.txt", sep=",", header=TRUE, na.strings="-", blank.lines.skip = TRUE, fill = TRUE)
 
  
  #remove unwanted columns such as &&&& and @@@@, the column name will be automatically converted to X....
  tmpdf = tmpdf[,!(colnames(tmpdf) %in% c("X9700","X9900","X....","Total"))]
  
  # create a column to identify state
  tmpdf[,"STATECODE"] = substr(tmpdf[, "DZN"],1,1)
  tmpdf[,"TZCODE"] = substr(tmpdf[, "DZN"],nchar(tmpdf[, "DZN"])-3,nchar(tmpdf[, "DZN"]))
  
  filter = tmpdf[,"STATECODE"] == "2"
  targetStateDF = tmpdf[filter,]
  # duplicated TZCODE are : "3814" "3278" "3047" "2120" "9000", but they are not in the shp, so it's ok
  
  
  tmpmx = as.matrix(targetStateDF)
  tmpmx[which(is.na(tmpmx))] = 0
  
  empDataFrame = as.data.frame(tmpmx)
  
  # join attribute data to polygon data on dzn code, all polygon data rows are kept
  # make sure the shp file is in WGS84 (EPSG:4326)
  
  empPolyRawDataFrame <- readOGR(dsn="VIC\\2001",layer="tz2001",encoding="utf8")
  
  empPolyRawDataFrame@data[,"TZCODE"] = paste("T00000",empPolyRawDataFrame@data[,"TZNUM"],sep="")
  empPolyRawDataFrame@data[,"TZCODE"] = substr(empPolyRawDataFrame@data[,"TZCODE"],nchar(empPolyRawDataFrame@data[,"TZCODE"])-3,nchar(empPolyRawDataFrame@data[,"TZCODE"]))
  
  empPolyRawDataFrame@data[,"orgSeqId"] = c(1:nrow(empPolyRawDataFrame@data))
  joinedDF = merge(empPolyRawDataFrame@data, empDataFrame, by.x = "TZCODE", by.y = "TZCODE", all.x=TRUE, sort=FALSE)
  
  orderedJoinedDF = joinedDF[order(joinedDF[,"orgSeqId"]), ]

  empPolyRawDataFrame@data = orderedJoinedDF
  
  proj4string(empPolyRawDataFrame)<-" +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  writeOGR(obj=empPolyRawDataFrame, dsn="VIC\\2001", layer="DZN_X_Employment_2001_VIC_Orginal", driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
}

#ERROR: number of objects mismatch
ANZSIC.datamerge.2001.NSW <- function(){
  tmpdf = read.table("National_Industry_Tables\\2001 Industry.txt", sep=",", header=TRUE, na.strings="-", blank.lines.skip = TRUE, fill = TRUE)
  
  
  #remove unwanted columns such as &&&& and @@@@, the column name will be automatically converted to X....
  tmpdf = tmpdf[,!(colnames(tmpdf) %in% c("X9700","X9900","X....","Total"))]
  
  # create a column to identify state
  tmpdf[,"STATECODE"] = substr(tmpdf[, "DZN"],1,1)
  tmpdf[,"TZCODE"] = substr(tmpdf[, "DZN"],nchar(tmpdf[, "DZN"])-3,nchar(tmpdf[, "DZN"]))
  
  filter = tmpdf[,"STATECODE"] == "1"
  targetStateDF = tmpdf[filter,]
  # duplicated TZCODE are : "0018" "3854" "0503" "0601" "0602" "0603" "0604" "0605" "0607" "0608" "0704" "0705" "0708" "0703" "0707" "0701" "0706" "9000"
  # some duplicates cause problem for example: 3854 -> 1117203854, 1146503854, when merge is performed, 3854 will match two JTW records.
  # 
  tmpmx = as.matrix(targetStateDF)
  tmpmx[which(is.na(tmpmx))] = 0
  
  empDataFrame = as.data.frame(tmpmx)
  
  # join attribute data to polygon data on dzn code, all polygon data rows are kept
  # make sure the shp file is in WGS84 (EPSG:4326)
  
  empPolyRawDataFrame <- readOGR(dsn="NSW\\2001",layer="tz2001GMA",encoding="utf8")
  
  empPolyRawDataFrame@data[,"TZCODE"] = paste("T00000",empPolyRawDataFrame@data[,"tz01"],sep="")
  empPolyRawDataFrame@data[,"TZCODE"] = substr(empPolyRawDataFrame@data[,"TZCODE"],nchar(empPolyRawDataFrame@data[,"TZCODE"])-3,nchar(empPolyRawDataFrame@data[,"TZCODE"]))
  
  empPolyRawDataFrame@data[,"orgSeqId"] = c(1:nrow(empPolyRawDataFrame@data))
  joinedDF = merge(empPolyRawDataFrame@data, empDataFrame, by.x = "TZCODE", by.y = "TZCODE", all.x=TRUE, sort=FALSE)
  
  orderedJoinedDF = joinedDF[order(joinedDF[,"orgSeqId"]), ]
  
  empPolyRawDataFrame@data = orderedJoinedDF
  
  proj4string(empPolyRawDataFrame)<-" +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  writeOGR(obj=empPolyRawDataFrame, dsn="NSW\\2001", layer="DZN_X_Employment_2001_NSW_Orginal", driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
}

#SUCCESS
ANZSIC.datamerge.2001.SA <- function(){
  tmpdf = read.table("National_Industry_Tables\\2001 Industry.txt", sep=",", header=TRUE, na.strings="-", blank.lines.skip = TRUE, fill = TRUE)
  
  
  #remove unwanted columns such as &&&& and @@@@, the column name will be automatically converted to X....
  tmpdf = tmpdf[,!(colnames(tmpdf) %in% c("X9700","X9900","X....","Total"))]
  
  # create a column to identify state
  tmpdf[,"STATECODE"] = substr(tmpdf[, "DZN"],1,1)
  tmpdf[,"TZCODE"] = substr(tmpdf[, "DZN"],nchar(tmpdf[, "DZN"])-3,nchar(tmpdf[, "DZN"]))
  
  filter = tmpdf[,"STATECODE"] == "4"
  targetStateDF = tmpdf[filter,]
  # duplicated TZCODE are : "9000", but they are not in the shp, so it's ok
  
  
  tmpmx = as.matrix(targetStateDF)
  tmpmx[which(is.na(tmpmx))] = 0
  
  empDataFrame = as.data.frame(tmpmx)
  
  # join attribute data to polygon data on dzn code, all polygon data rows are kept
  # make sure the shp file is in WGS84 (EPSG:4326)
  
  empPolyRawDataFrame <- readOGR(dsn="SA\\2001",layer="tz2001",encoding="utf8")
  
  empPolyRawDataFrame@data[,"TZCODE"] = paste("T00000",empPolyRawDataFrame@data[,"DZN2001"],sep="")
  empPolyRawDataFrame@data[,"TZCODE"] = substr(empPolyRawDataFrame@data[,"TZCODE"],nchar(empPolyRawDataFrame@data[,"TZCODE"])-3,nchar(empPolyRawDataFrame@data[,"TZCODE"]))
  
  empPolyRawDataFrame@data[,"orgSeqId"] = c(1:nrow(empPolyRawDataFrame@data))
  joinedDF = merge(empPolyRawDataFrame@data, empDataFrame, by.x = "TZCODE", by.y = "TZCODE", all.x=TRUE, sort=FALSE)
  
  orderedJoinedDF = joinedDF[order(joinedDF[,"orgSeqId"]), ]
  
  empPolyRawDataFrame@data = orderedJoinedDF
  
  proj4string(empPolyRawDataFrame)<-" +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  writeOGR(obj=empPolyRawDataFrame, dsn="SA\\2001", layer="DZN_X_Employment_2001_SA_Orginal", driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
}

