# 資料特徵資料整理
# 程式撰寫: 中山財管所 104級 蘇彥庭
rm(list=ls());gc()

# 程式使用函數包
library("ggplot2")  
library("nnet")

# 讀取數據
data <- read.csv("C:/Users/user/Desktop/houseData.csv")

# 特徵資料
featureData <- NULL

############### 處理備註欄位(判斷此交易是否為關係人交易，如果是的話則刪除該樣本) ################ 
remarks <- as.character(data[,c("備註")])
viewData <-as.matrix(unique(remarks))           # 觀察樣本

# 關係人交易關鍵字詞
keyWord <- c("親友","親戚","親屬","兄","弟","姊","妹","員工","特殊關係",
             "等親","親等","二等親","朋友","母","父","子","女","夫妻",
             "爸","媽","姻親","祖孫","叔","姪")

# 判斷關鍵字詞
matchPattern <- lapply(keyWord, function(word){which(grepl(paste0("\\",word,"+"),remarks))})
matchSite <- unique(unlist(matchPattern))
viewData <-as.matrix(remarks[matchSite])        # 觀察樣本
data <- data[-matchSite,]                       # 刪除關係人交易樣本

############################## 預測變數:每平方公尺(含車位價格) ##############################
summary(data[,c("每平方公尺.車位.")])
featureData <- cbind(featureData, data[,c("每平方公尺.車位.")])
colnames(featureData)[ncol(featureData)] <- c("每平方公尺價格")

############################## 鄉鎮市區 ##############################
#dummy <- class.ind(factor(data[,c("鄉鎮市區")]))        # 轉為虛擬變數
#dummy <- dummy[,-ncol(dummy)]                           # 刪除最後一個欄位
#featureData <- cbind(featureData, dummy)                # 併入特徵資料
#summary(factor(data[,c("鄉鎮市區")]))                   # 敘述統計學
featureData <- cbind(featureData, as.factor(data[,c("鄉鎮市區")])) 
colnames(featureData)[ncol(featureData)] <- c("鄉鎮市區")

############################## 土地使用分區 ##############################
#delSite <- which(data[,c("土地使用分區")]==0)           # 刪除不合適資料
#data <- data[-delSite,]                                 # 刪除土地使用分區資料值為0
#featureData <- featureData[-delSite,]
#dummy <- class.ind(factor(data[,c("土地使用分區")]))    # 轉為虛擬變數
#dummy <- dummy[,-ncol(dummy)]                           # 刪除最後一個欄位
#featureData <- cbind(featureData, dummy)                # 併入特徵資料
#summary(factor(data[,c("土地使用分區")]))               # 敘述統計學
featureData <- cbind(featureData, data[,c("土地使用分區")])
colnames(featureData)[ncol(featureData)] <- c("土地使用分區")
############################## 建物現況格局 ##############################
summary(data[,c("建物現況格局.房")])
summary(data[,c("建物現況格局.廳")])
summary(data[,c("建物現況格局.衛")])
featureData <- cbind(featureData, data[,c("建物現況格局.房")]) 
colnames(featureData)[ncol(featureData)] <- c("建物現況格局.房")
featureData <- cbind(featureData, data[,c("建物現況格局.廳")]) 
colnames(featureData)[ncol(featureData)] <- c("建物現況格局.廳")
featureData <- cbind(featureData, data[,c("建物現況格局.衛")])
colnames(featureData)[ncol(featureData)] <- c("建物現況格局.衛")

############################## 有無管理組織 ##############################
summary(data[,c("有無管理組織")])
featureData <- cbind(featureData, as.numeric(data[,c("有無管理組織")] == "有")) 
colnames(featureData)[ncol(featureData)] <- c("有無管理組織")

############################## 建物型態 ##############################
delSite <-  which(data[,c("建物型態")] %in% c("工廠","其他","店面(店鋪)","倉庫","農舍","廠辦","辦公商業大樓"))
data <- data[-delSite,]                                 # 刪除建物型態非住宅資料
featureData <- featureData[-delSite,]                   
dummy <- class.ind(factor(data[,c("建物型態")]))        # 轉為虛擬變數
dummy <- dummy[,-ncol(dummy)]                           # 刪除最後一個欄位
featureData <- cbind(featureData, dummy)                # 併入特徵資料
summary(factor(data[,c("建物型態")]))                   # 敘述統計學

############################## 計算屋齡 ##############################
# 日期處理函數
DateConvert <- function(rawDate){

  # 判斷日期為幾位數，因為民國100年(含)以後的日期都會是7位數，所以需要分開處理
  if(!is.na(rawDate)){
    if(nchar(rawDate)==7){
      if(substr(rawDate,4,5)=="00"){
        convertDate <- NA
      }else{
        convertDate <- (as.numeric(substr(rawDate,1,3))+1911)*10000+
          as.numeric(substr(rawDate,4,5))*100+
          as.numeric(substr(rawDate,6,7))
      }
    }else if(nchar(rawDate)==6){
      convertDate <- (as.numeric(substr(rawDate,1,2))+1911)*10000+
        as.numeric(substr(rawDate,3,4))*100+
        as.numeric(substr(rawDate,5,6))
    }else{
      convertDate <- NA 
    }
  }else{
    convertDate <- NA 
  }
  convertDate <- as.Date(formatC(as.integer(convertDate), width=8, flag=0),"%Y%m%d") # 轉為日期格式
  return(convertDate)
}

# 取出交易年月日
tradeDate <- data[,c("交易年月日")]
tradeDateConvert <- sapply(tradeDate,DateConvert)
featureData <- cbind(tradeDate, featureData)
colnames(featureData)[1] <- c("交易日期")

# 取出建築完成年月
buildDate <- data[,c("建築完成年月")]
buildDateConvert <- sapply(buildDate,DateConvert)

# 計算屋齡(日數)=交易年月日-建築完成年月
houseAge <- tradeDateConvert-buildDateConvert
houseAge <- houseAge/365 # 轉為以年單位

# # 觀察特徵狀況
# # 有發現屋齡為負的狀況? 推測是預售屋? 或是資料登載錯誤?
# summary(houseAge)              # 屋齡敘述統計學
# length(which(houseAge<0))      # 屋齡為負的筆數
# length(which(houseAge>0))      # 屋齡為正的筆數
# length(which(is.na(houseAge))) # 屋齡為NA的筆數
# which(houseAge<0)
# viewSample <- 938              # 觀察樣本
# tradeDate[viewSample]
# tradeDateConvert[viewSample]
# buildDate[viewSample]
# buildDateConvert[viewSample]
# 
# # 繪製屋齡的直方圖
 plotData <- as.data.frame(houseAge[which(houseAge>0)]) # 只繪製屋齡為正的樣本
 colnames(plotData) <- c("houseAge")
 ggplot(plotData, aes(x=houseAge))+
   geom_histogram(color="darkblue", fill="lightblue")
# 
# # 與單價每平方公尺做相關係數
# pricePerM <- data[,c("單價每平方公尺")]
# cor(pricePerM,houseAge, use="complete.obs")  # 相關係數不高，可能是未考慮其他變因:例如地段差異

# 刪除屋齡為負或者是缺值的樣本
delSite <- which(is.na(houseAge)| houseAge<0)
houseAge <- houseAge[-delSite]
data <- data[-delSite,]                                 
featureData <- featureData[-delSite,] 
featureData <- cbind(featureData, houseAge)     # 併入特徵資料
colnames(featureData)[ncol(featureData)] <- c("屋齡")
summary(featureData[,c("屋齡")])                # 敘述統計學

############################## 移轉層次/總樓層數 ############################## 
sellFloor <- as.character(data[,c("移轉層次")])
totalFloor <- as.character(data[,c("總樓層數")])

# 觀看資料
viewData <-as.matrix(unique(sellFloor)) 
viewData <-as.matrix(unique(totalFloor)) 

# 中文數字轉換可運算數字
TransChNum <- function(ChNumber){
  if(ChNumber=="一"){
    number <- 1
  }else if(ChNumber=="二"){
    number <- 2
  }else if(ChNumber=="三"){
    number <- 3
  }else if(ChNumber=="四"){
    number <- 4
  }else if(ChNumber=="五"){
    number <- 5
  }else if(ChNumber=="六"){
    number <- 6
  }else if(ChNumber=="七"){
    number <- 7
  }else if(ChNumber=="八"){
    number <- 8
  }else if(ChNumber=="九"){
    number <- 9
  }
}

# 將中文表示數字轉為可運算數值
ChangeFloorNum <- function(floorNums){
  
  if(floorNums=="0" | floorNums=="99" | floorNums=="" | floorNums=="character(0)"){
    result <- NA
    
  }else{
    
    if(grepl("\\十",floorNums)){  # 判斷字串是否有"十"
      
      if(nchar(floorNums)==1){    # 若字串只有一個十，則代表十層樓
        
        result <- 10
        
      }else if(nchar(floorNums)>1){ # 字串內含十
        
        if(regexpr("\\十",floorNums)==nchar(floorNums)){  # 若最後一個字是"十"，做對應處理
          
          result <- TransChNum(substr(floorNums,1,1))*10
          
        }else if(nchar(floorNums)==3){  # 若字串為3位數，做對應處理
          
          result <- TransChNum(substr(floorNums,1,1))*10+TransChNum(substr(floorNums,3,3))
          
        }else if(nchar(floorNums)==2){  # 若字串為2位數，做對應處理
          
          result <- 10+TransChNum(substr(floorNums,2,2))
        }
      }
    }else{ # 若字串內不含十，直接套函數取得樓層數 
      
      result <- TransChNum(floorNums)
    }
  }
  
  return(result)
}

# 處理總樓層資料
totalFloor <- gsub("\\層+","",totalFloor)         # 刪掉"層"字
totalFloor <- sapply(totalFloor,ChangeFloorNum)   # 轉換為數值格式



