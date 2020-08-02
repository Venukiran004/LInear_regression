my_data=read.csv("C://Users//venuk//OneDrive//Desktop//linear project//Property_Price_Train.csv")
dim(my_data)
names(my_data)
#________________________
library(psych)
id_data=my_data$Id
describe(id_data)



#_________________________________________________________
#building class is categorical data
building_class_data=my_data$Building_Class
#building_class_data=as.numeric(building_class_data)   
#table(building_class_data)


   # 20  30  40  45  50  60  70  75  80  85  90 120 160 180 190 
  #535  69   4  12 144 299  60  16  58  20  52  87  63  10  30 
#______________________________________________________________________-

               
                  ####column 2#####
zoning_class_casted=my_data$Zoning_Class
summary(zoning_class_casted)

zoning_class_casted=as.factor(my_data$Zoning_Class)
table(zoning_class_casted)
zoning_class_casted1=as.numeric(zoning_class_casted)
zoning_class_casted1
table(zoning_class_casted1)   
                 ####commer==1,  FVR==2,  RHD==3,  RLD==4,  RMD==5
                                      ##10         ##65      ##16     #1150   #218
zoning_class_casted1

table(zoning_class_casted)
barplot(zoning_class_casted)

#_____________________________________________________________________________
                              ####column 3                                      

library(psych)
    
lot_extent_data=my_data$Lot_Extent
describe(lot_extent_data)
median_data=median(lot_extent_data,na.rm = T)
median_data
sum(is.na(lot_extent_data))
lot_extent_data[is.na(lot_extent_data)]=median_data
summary(lot_extent_data)
describe(lot_extent_data)
lot_extent_data                             #removed na and replaced by median
                                           #  bascially how much item is delivered
 
#_________________________________________________________________________________________________________
###lot_size is numerical data


lot_size_data=my_data$Lot_Size
summary(lot_size_data)                              #  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
                                                    #   1300    7549    9477   10517   11603  215245 
describe(lot_size_data)
lot_size_data


#_____________________________________________________________________________________________


Road_type_data=as.factor(my_data$Road_Type)              # Gravel  Paved 
                                                           #  6   1453 
table(Road_type_data)                                     #   1     2
road_type_data_numeric=as.numeric(Road_type_data)
table(road_type_data_numeric)   
road_type_data_numeric
names(my_data)
#_____________________________________________________________________________________________________
Lane_Type_data=my_data$Lane_Type
Lane_Type_data
Lane_Type_data[is.na(Lane_Type_data)]="NO ALLEY ACCESS"
#Lane_Type_data[Lane_Type_data=="NA"] 
#Lane_Type_data=replace(Lane_Type_data,ne,"no alley access")
Lane_Type_data=as.factor(Lane_Type_data)
Lane_Type_data_numeric=as.numeric(Lane_Type_data)
table(Lane_Type_data)
table(Lane_Type_data_numeric)
#ata.frame(Lane_Type_data_numeric)
#dim(d)
#Lane_Type_data_numeric=na.omit(Lane_Type_data_numeric)
#Lane_Type_data_numeric
#Grvl no alley access           Paved 
#50            1368              41 
#1              2                  3
#___________________________________________________________________________________


property_shape_data=as.factor(my_data$Property_Shape)
table(property_shape_data)
property_shape_data_numeric=as.numeric(property_shape_data)
                                                                    
table(property_shape_data_numeric)



                                                           #  IR1  IR2  IR3  Reg 
                                                            #  484  41   10   924 
                                                             #  1   2     3    4

#______________________________________________________________________________________-


land_outline_data=my_data$Land_Outline
land_outline_data=as.factor(land_outline_data)
table(land_outline_data)
land_outline_data_numeric=as.numeric(land_outline_data)
table(land_outline_data_numeric)
land_outline_data_numeric

#Bnk  HLS  Low  Lvl 
#63   50   36 1310 
#1    2    3    4

#_______________________________________________________________________________________
names(my_data)
Utility_Type_data=as.factor(my_data$Utility_Type)

Utility_Type_data_numeric=as.numeric(Utility_Type_data)
table(Utility_Type_data_numeric)
#AllPub NoSeWa 
#1458      1 
# 1         2

#_______________________________________________________________________________________--


Lot_Configuration_data=my_data$Lot_Configuration
Lot_Configuration_data=as.factor(Lot_Configuration_data)
table(Lot_Configuration_data)
Lot_Configuration_data_numeric=as.numeric(Lot_Configuration_data)
table(Lot_Configuration_data_numeric)

#  C    CulDSac    FR2P    FR3P       I 
# 263      94      47       4     1051 
#  1         2        3       4    5 

#__________________________________________________________________________________________

Neighbour_c1_c2=my_data[,c(13,14,15)]



col_names=names(Neighbour_c1_c2)                                              #GS   MS   SS 
Neighbour_c1_c2[,col_names]=lapply(Neighbour_c1_c2[,col_names],factor)        # 1    2    3 
                                                                              #1381   65   13 
table(Neighbour_c1_c2$Neighborhood)


Neighbour_c1_c2[,col_names]=lapply(Neighbour_c1_c2[,col_names],as.numeric)
head(Neighbour_c1_c2)


Neighborhood_data=Neighbour_c1_c2$Neighborhood
condition1_data=Neighbour_c1_c2$Condition1
condition2_data=Neighbour_c1_c2$Condition2
describe(condition2_data)
#Blmngtn Blueste  BrDale BrkSide ClearCr CollgCr Crawfor Edwards Gilbert  IDOTRR MeadowV Mitchel   NAmes NoRidge NPkVill 
 #   17       2      16      58      28     150      51      99      79      37      17      49     225      41       9 


#NridgHt  NWAmes OldTown  Sawyer SawyerW Somerst StoneBr   SWISU  Timber Veenker 
#77      73     113      74      59      86      25      25      38      11 





#con 1:
#Artery  Feedr   Norm   PosA   PosN   RRAe   RRAn   RRNe   RRNn 
#48     81   1259      8     19     11     26      2      5 


#cond 2:

#Artery  Feedr   Norm   PosA   PosN   RRAe   RRAn   RRNn 
#2      6   1444      1      2      1      1      2 





#________________________________________________________________________

House_Type_data=my_data$House_Type
House_Type_data=as.factor(House_Type_data)
table(House_Type_data)
House_Type_data_numeric=as.numeric(House_Type_data)
table(House_Type_data_numeric)
House_Type_data_numeric


#1Fam   2fmCon  Duplex  Twnhs  TwnhsE 
#1219     31     52     43      114 
#1         2     3        4      5 
#_____________________________________________________________________________
House_Design_data=my_data$House_Design
House_Design_data=as.factor(House_Design_data)
table(House_Design_data)
House_Design_data_numeric=as.numeric(House_Design_data)
table(House_Design_data_numeric)
House_Design_data_numeric


#1.5Fin 1.5Unf 1Story 2.5Fin 2.5Unf 2Story SFoyer   SLvl 
#154     14    725      8     11    445     37     65 
#1        2      3      4      5     6       7     8 


#_____________________________________________________________________

Overall_Material_data=my_data$Overall_Material
#Overall_Material_data=as.factor(Overall_Material_data)
#table(Overall_Material_data)


#1   2   3   4   5   6   7   8   9  10 
#2   3  20 116 396 374 319 168  43  18 

#10	Very Excellent
#9	Excellent
#8	Very Good
#7	Good
#6	Above Average
#5	Average
#4	Below Average
#3	Fair
#2	Poor
#1	Very Poor
#__________________________________________________________________________________________




#housecondition

House_Condition_data=my_data$House_Condition
#use_Condition=as.factor(House_Condition_data)
#table(House_Condition_data)


#1   2   3   4   5   6   7   8   9 
#1   5  25  57 821 251 205  72  22 



#________________________________________________________________


Construction_Year_data=my_data$Construction_Year
#Construction_Year_data=as.factor(Construction_Year_data)

#tabled_data_constyear=table(Construction_Year_data)
#View(tabled_data_constyear)
#max(tabled_data_constyear)

                                                       ####2006    67 houses
#______________________________________________________________________

Remodel_Year_data=my_data$Remodel_Year

#table(Remodel_Year_data)
#max(table(Remodel_Year_data))                         #1950 houses are remodeled 

#_____________________________________________________________________________

Roof_Design_data=my_data$Roof_Design
Roof_Design_data=as.factor(Roof_Design_data)
Roof_Design_data_numeric=as.numeric(Roof_Design_data) 
table(Roof_Design_data)
table(Roof_Design_data_numeric)
Roof_Design_data_numeric


#Flat   Gable Gambrel     Hip  Mansard    Shed 
#13    1140      11     286       7       2 
#1        2       3       4        5      6 


#__________________________________________________________________________________

Roof_Quality_data=my_data$Roof_Quality
Roof_Quality_data=as.factor(Roof_Quality_data)
table(Roof_Quality_data)
Roof_Quality_data_numeric=as.numeric(Roof_Quality_data)
table(Roof_Quality_data_numeric)

# CT    M   ME    R   SS   TG   WS  WSh 
#1    1    1    1 1433   11    5    6 
#1    2    3    4    5    6    7    8 

#_____________________________________________________________________________________

Exterior1st_data=my_data$Exterior1st
Exterior1st_data=as.factor(Exterior1st_data)
table(Exterior1st_data)
Exterior1st_data_numeric=as.numeric(Exterior1st_data)
table(Exterior1st_data_numeric)
###
#1        2        3        4       5       6       7      8       9       10       11      12    13   
#AsbShng AsphShn BrkComm BrkFace  CBlock CemntBd HdBoard ImStucc MetalSd Plywood   Stone  Stucco VinylSd 
#20       1       2      50       1      61     221       1     220     108       2      25     515 

#14         15
#Wd Sdng     WdShing 
#206         26 

#_____________________________________________________________________________________


####

Exterior2nd_data=my_data$Exterior2nd
Exterior2nd_data=as.factor(Exterior2nd_data)
Exterior2nd_data_numeric=as.numeric(Exterior2nd_data)
table(Exterior2nd_data)
table(Exterior2nd_data_numeric)

#AsbShng AsphShn Brk Cmn BrkFace  CBlock CmentBd HdBoard ImStucc MetalSd   Other Plywood   Stone  Stucco 
#20       3       7      25       1      60     206      10     214       1     142       5      26 
# 1         2      3      4       5       6      7        8       9       10     11       12      13 



#VinylSd Wd Sdng Wd Shng 
#504     197      38
#14      15       16

#______________________________________________________________________________________

Brick_Veneer_Type_data=my_data$Brick_Veneer_Type
Brick_Veneer_Type_data[is.na(Brick_Veneer_Type_data)]="None"
table(Brick_Veneer_Type_data)
#new540=Brick_Veneer_Type_data[Brick_Veneer_Type_data=="NA"]
#Brick_Veneer_Type_data=replace(Brick_Veneer_Type_data,new540,"None")
Brick_Veneer_Type_data=as.factor(Brick_Veneer_Type_data)
table(Brick_Veneer_Type_data)
Brick_Veneer_Type_data_numeric=as.numeric(Brick_Veneer_Type_data)
table(Brick_Veneer_Type_data_numeric)
#Brick_Veneer_Type_data_numeric=na.omit(Brick_Veneer_Type_data_numeric)
table(Brick_Veneer_Type_data_numeric)
#df=data.frame(Brick_Veneer_Type_data_numeric)
#dim(df)
Brick_Veneer_Type_data_numeric

#BrkCmn BrkFace    None  Stone 
#15     445     871     128 
#1       2        3        4       
#___________________________________________________________________________________________

summary(my_data$Brick_Veneer_Area)  ## 8 na values
Brick_Veneer_Area_data=my_data$Brick_Veneer_Area
Brick_Veneer_Area_data
median_data1=median(Brick_Veneer_Area_data,na.rm = T)
median_data1
sum(is.na(Brick_Veneer_Area_data))
Brick_Veneer_Area_data[is.na(Brick_Veneer_Area_data)]=median_data1
summary(Brick_Veneer_Area_data)
describe(Brick_Veneer_Area_data)
Brick_Veneer_Area_data
#______________________________________________________________________
summary(my_data$Exterior_Material)

Exterior_Material_data=my_data$Exterior_Material
Exterior_Material_data=as.factor(Exterior_Material_data)
table(Exterior_Material_data)
Exterior_Material_data_numeric=as.numeric(Exterior_Material_data)
table(Exterior_Material_data_numeric)


# Ex  Fa  Gd  TA 
#52  14 487 906
#1   2   3   4 

#____________________________________________________________________________________________
summary(my_data$Exterior_Condition)
Exterior_Condition_data=my_data$Exterior_Condition
Exterior_Condition_data=as.factor(Exterior_Condition_data)
table(Exterior_Condition_data)
Exterior_Condition_data_numeric=as.numeric(Exterior_Condition_data)
table(Exterior_Condition_data_numeric)


#Ex   Fa   Gd   Po   TA 
#3   28  146    1   1281
# 1   2    3     4    5

#__________________________________________________________________________________
Foundation_Type_data=my_data$Foundation_Type
Foundation_Type_data=as.factor(Foundation_Type_data)
table(Foundation_Type_data)
Foundation_Type_numeric=as.numeric(Foundation_Type_data)
table(Foundation_Type_numeric)

#BT  CB  PC   S  SL   W 
#146 633 647   6  24   3 
#1   2    3    4    5   6

#______________________________________________________________________________________

Basement_Height_data=my_data$Basement_Height
Basement_Height_data[Basement_Height_data=="NA"]="No Basement"

Basement_Height_data=as.factor(Basement_Height_data)
Basement_Height_data_numeric=as.numeric(Basement_Height_data)
table(Basement_Height_data)
#Basement_Height_data_numeric=na.omit(Basement_Height_data_numeric)
table(Basement_Height_data_numeric)


#Ex          Fa          Gd no Basement          TA 
#121          35         618          37         648 
# 1            2           3         4             5
#______________________________________________________________________________________\
Basement_Condition_data=my_data$Basement_Condition
is.na(Basement_Condition_data)
Basement_Condition_data[is.na(Basement_Condition_data)]="No Basement"
#new545=Basement_Condition_data[Basement_Condition_data=="NA"]
#Basement_Condition_data=replace(Basement_Condition_data,new545,"no Basement")
Basement_Condition_data=as.factor(Basement_Condition_data)
table(Basement_Condition_data)
Basement_Condition_data_numeric=as.numeric(Basement_Condition_data)
#Basement_Condition_data_numeric=na.omit(Basement_Condition_data_numeric)
table(Basement_Condition_data_numeric)

#Fa          Gd no Basement          Po          TA 
#45          65          37           2        1310 
#1             2          3            4          5
#_________________________________________________________________________________

Exposure_Level_data=my_data$Exposure_Level
Exposure_Level_data[is.na(Exposure_Level_data)]="No Basement"
#new789=Exposure_Level_data[Exposure_Level_data=="NA"]
#Exposure_Level_data=replace(Exposure_Level_data,new789,"no Basement")
Exposure_Level_data=as.factor(Exposure_Level_data)
table(Exposure_Level_data)
Exposure_Level_data_numeric=as.numeric(Exposure_Level_data)
#Exposure_Level_data_numeric=na.omit(Exposure_Level_data_numeric)
table(Exposure_Level_data_numeric)
#df=data.frame(Exposure_Level_data_numeric)
#dim(df)


#Av          Gd          Mn          No no Basement 
#221         134         114         952          38 
# 1            2           3          4          5
#___________________________________________________________________________________
BsmtFinType1_data=my_data$BsmtFinType1
BsmtFinType1_data[is.na(BsmtFinType1_data)]="No Basement"
#new800=BsmtFinType1_data[BsmtFinType1_data=="NA"]
#BsmtFinType1_data=replace(BsmtFinType1_data,new800,"no Basement")
BsmtFinType1_data=as.factor(BsmtFinType1_data)
table(BsmtFinType1_data)
BsmtFinType1_data_numeric=as.numeric(BsmtFinType1_data)
#BsmtFinType1_data_numeric=na.omit(BsmtFinType1_data_numeric)
table(BsmtFinType1_data_numeric)
#data_1=data.frame(BsmtFinType1_data_numeric)
#dim(data_1)
#ALQ         BLQ         GLQ         LwQ no Basement         Rec         Unf 
#220         147         418          74          37         133         430 
#1             2           3            4        5            6            7
#_________________________________________________________________________________________

BsmtFinSF1_data=my_data$BsmtFinSF1
summary(BsmtFinSF1_data)
describe(BsmtFinSF1_data)
BsmtFinSF1_data


#_______________________________________________________________________________________
BsmtFinType2_data=my_data$BsmtFinType2
BsmtFinType2_data[is.na(BsmtFinType2_data)]="No Basement"
#new801=BsmtFinType2_data[BsmtFinType2_data=="NA"]
#BsmtFinType2_data=replace(BsmtFinType2_data,new801,"no Basement")
BsmtFinType2_data=as.factor(BsmtFinType2_data)
table(BsmtFinType2_data) 
BsmtFinType2_data_numeric=as.numeric(BsmtFinType2_data)
table(BsmtFinType2_data_numeric)
#BsmtFinType2_data_numeric=na.omit(BsmtFinType2_data_numeric)
new_1=data.frame(BsmtFinType2_data_numeric)

dim(new_1)
 #ALQ         BLQ         GLQ         LwQ no Basement         Rec         Unf 
#19          33          14          45          38          54        1256 
#1             2          3            4          5           6            7

#____________________________________________________________________________________________


#_____________________________________________________________________________________

BsmtFinSF2_data=my_data$BsmtFinSF2
summary(BsmtFinSF2_data)


#______________________________________________________________________________________________

BsmtUnfSF_data=my_data$BsmtUnfSF
summary(BsmtUnfSF_data)

#_____________________________________________________________________________________________
Total_Basement_Area_data=my_data$Total_Basement_Area
Total_Basement_Area_data
summary(Total_Basement_Area_data)




#______________________________________________________________________________________________

Heating_Type_data=as.factor(my_data$Heating_Type)
table(Heating_Type_data)
Heating_Type_data_numeric=as.numeric(Heating_Type_data)
table(Heating_Type_data_numeric)

#Floor  GasA  GasW  Grav  OthW  Wall 
#1     1427    18     7     2     4
 #1      2       3     4    5     6

#_______________________________________________________________________________________
names(my_data)
cat_num_data=my_data[,c(41,42)]
head(cat_num_data)
table(cat_num_data$Air_Conditioning)

col_names=names(cat_num_data)
cat_num_data[,col_names]=lapply(cat_num_data[,col_names],factor)


cat_num_data[,col_names]=lapply(cat_num_data[,col_names],as.numeric)
head(cat_num_data)
heating_data=cat_num_data$Heating_Quality
airconditing_data=cat_num_data$Air_Conditioning


                                 #Air_conditing                #Electrical_system
#heating_quality                      N    Y              
 #                                    95 1364                  FuseA FuseF FuseP   Mix  SBrkr 
 #                                                             94    27     3     1     1333 
#Ex  Fa  Gd  Po  TA                    1   2                   
#741  49 240   1 428                                            1    2      3     4      5
#1    2    3   4  5
#___________________________________________________________________________________
electrical_system_data=my_data$Electrical_System
sum(is.na(electrical_system_data))
electrical_system_data[is.na(electrical_system_data)]="no type"
#new808=electrical_system_data[electrical_system_data=="NA"]
#electrical_system_data=replace(electrical_system_data,new808,"no type")
electrical_system_data=as.factor(electrical_system_data)
electrical_system_data_numeric=as.numeric(electrical_system_data)
#electrical_system_data_numeric=na.omit(electrical_system_data_numeric)
table(electrical_system_data)
##dim(dat)
# FuseA   FuseF   FuseP     Mix no type   SBrkr 
#94      27       3       1       1    1333 

#__________________________________________________________________________________________________

first_floor_area_data=my_data[,c(44)]
head(first_floor_area_data)
summary(first_floor_area_data)

#__________________________________________________________
second_floor_area_data=my_data[,c(45)]
summary(second_floor_area_data)

#_______________________________________________________________________________________-

LowQualFinSF_data=my_data$LowQualFinSF
summary(LowQualFinSF_data)
LowQualFinSF_data
#___________________________________________________________________________________________

Grade_Living_Area_data=my_data$Grade_Living_Area
summary(Grade_Living_Area_data)
Grade_Living_Area_data

#___________________________________________________________________

Underground_Full_Bathroom_data=my_data$Underground_Full_Bathroom
summary(Underground_Full_Bathroom_data)


#_____________________________________________________________________________

Underground_Half_Bathroom_data=my_data$Underground_Half_Bathroom
summary(Underground_Half_Bathroom_data)


#________________________________________________________________________________________
Full_Bathroom_Above_Grade_data=my_data$Full_Bathroom_Above_Grade
head(Full_Bathroom_Above_Grade_data)
summary(Full_Bathroom_Above_Grade_data)


#________________________________________________________________________________________
Half_Bathroom_Above_Grade=my_data$Half_Bathroom_Above_Grade
summary(Half_Bathroom_Above_Grade)


#_______________________________________________________________________

Bedroom_withoutground=my_data$Bedroom_Above_Grade
summary(Bedroom_withoutground)

#_____________________________________________________________________________________________

Kitchen_Above_Grade_data=my_data$Kitchen_Above_Grade
Kitchen_Above_Grade_data


#_______________________________________________________________________________

Kitchen_Quality_data=as.factor(my_data$Kitchen_Quality)
Kitchen_Quality_data_numeric=as.numeric(Kitchen_Quality_data)
table(Kitchen_Quality_data_numeric)
table(Kitchen_Quality_data)


# Ex  Fa  Gd  TA 
#100  39 586 734 
#1    2    3    4


#____________________________________________________________________________________________________________
Rooms_Above_Grade_data=my_data$Rooms_Above_Grade

Rooms_Above_Grade_data

#_________________________________________________________________________________________________________

Functional_Rate_data=as.factor(my_data$Functional_Rate)
Functional_Rate_data_numeric=as.numeric(Functional_Rate_data)
table(Functional_Rate_data_numeric)
table(Functional_Rate_data)

#MajD1 MajD2    MD   MD1   MD2    MS    SD    TF 
#14     5    14    31    34     1     1  1359 
#_________________________________________________________________________________________________________
Fireplaces_data=my_data$Fireplaces
summary(Fireplaces_data)


#_______________________________________________________________________________________________________
Fireplace_Quality_data=my_data$Fireplace_Quality
Fireplace_Quality_data[is.na(Fireplace_Quality_data)]="No Fireplace"

#summary(Fireplace_Quality_data)

#new3=Fireplace_Quality_data[Fireplace_Quality_data=="NA"

#Fireplace_Quality_data=replace(Fireplace_Quality_data,new3,"no Fireplace")
#Fireplace_Quality_data=na.omit(Fireplace_Quality_data)
Fireplace_Quality_data=as.factor(Fireplace_Quality_data)
summary(Fireplace_Quality_data)
Fireplace_Quality_data_numeric=as.numeric(Fireplace_Quality_data)
#Fireplace_Quality_data_numeric=na.omit(Fireplace_Quality_data_numeric)
table(Fireplace_Quality_data_numeric)

#dataa=data.frame(Fireplace_Quality_data_numeric)
#dim(dataa)
# Ex  Fa  Gd    no Fireplace Po  TA  
#24  33 380         689      20  313
# 1   2   3          4        5    6

#________________________________________________________________________________________________________

Garage_data=my_data$Garage
Garage_data[is.na(Garage_data)]="No Garage"
summary(Garage_data)
#new=Garage_data[Garage_data=="NA"]
#Garage_data=replace(Garage_data,new,"no Garage")
Garage_data=as.factor(Garage_data)
table(Garage_data)
Garage_data_numeric=as.numeric(Garage_data)
#Garage_data_numeric=na.omit(Garage_data_numeric)
table(Garage_data_numeric)



#2TFes  2Types  Attchd Basment BuiltIn CarPort  Detchd  no Garage
#5       1     869      19      88       9     387        81 
# 1      2      3       4       5         6       7       8


#______________________________________________________________________________
Garage_Built_Year_data=my_data$Garage_Built_Year
max(table(Garage_Built_Year_data)) 
summary(Garage_Built_Year_data)                                         # 65    2005  #mode
Garage_Built_Year_data[is.na(Garage_Built_Year_data)]=2005

#Garage_Built_Year_data=as.factor(Garage_Built_Year_data)
Garage_Built_Year_data


#__________________________________________________________________________________
Garage_Finish_Year_data=my_data$Garage_Finish_Year
Garage_Finish_Year_data[is.na(Garage_Finish_Year_data)]="No Garage"
#summary(Garage_Finish_Year_data)   

#new1=Garage_Finish_Year_data[Garage_Finish_Year_data=="NA"]
##Garage_Finish_Year_data=replace(Garage_Finish_Year_data,new1,"no garage")
#Garage_Finish_Year_data=na.omit(Garage_Finish_Year_data)

Garage_Finish_Year_data=as.factor(Garage_Finish_Year_data)
summary(Garage_Finish_Year_data)
                               
Garage_Finish_Year_data_numeric=as.numeric(Garage_Finish_Year_data)
#Garage_Finish_Year_data_numeric=na.omit(Garage_Finish_Year_data_numeric)
table(Garage_Finish_Year_data_numeric)
#dataaa=data.frame(Garage_Finish_Year_data_numeric)
#dim(dataa)
#Fin no garage       RFn       Unf 
 #351        81       422       605 
 # 1          2          3        4
#_______________________________________________________________________________

Garage_Size_data=my_data$Garage_Size
summary(Garage_Size_data)

#__________________________________________________________________________________________

Garage_Area_data=my_data$Garage_Area
Garage_Area_data=abs(Garage_Area_data)      #converting negative area to positive
summary(Garage_Area_data)

#______________________________________________________________________________________________

Garage_Quality_data=my_data$Garage_Quality
Garage_Quality_data[is.na(Garage_Quality_data)]="No Garage"
#new4=Garage_Quality_data[Garage_Quality_data=="NA"]
#Garage_Quality_data=replace(Garage_Quality_data,new4,"no garage")
Garage_Quality_data=as.factor(Garage_Quality_data)
table(Garage_Quality_data)
Garage_Quality_data_numeric=as.numeric(Garage_Quality_data)
#Garage_Quality_data_numeric=na.omit(Garage_Quality_data_numeric)
table(Garage_Quality_data_numeric)
#tab=data.frame(Garage_Quality_data_numeric)
#dim(tab)
#Ex        Fa        Gd no garage        Po        TA 
#3        48        14        81         3      1310 
#1          2       3        4           5        6

#______________________________________________________________________________________________________
Garage_Condition_data=my_data$Garage_Condition
Garage_Condition_data[is.na(Garage_Condition_data)]="No garage"
#new5=Garage_Condition_data[Garage_Condition_data=="NA"]
#Garage_Condition_data=replace(Garage_Condition_data,new5,"no garage")
Garage_Condition_data=as.factor(Garage_Condition_data)
table(Garage_Condition_data)
Garage_Condition_data_numeric=as.numeric(Garage_Condition_data)
#Garage_Condition_data_numeric=na.omit(Garage_Condition_data_numeric)
table(Garage_Condition_data_numeric)
#daa=data.frame(Garage_Condition_data_numeric)
#dim(daa)
#Ex        Fa        Gd no garage        Po        TA 
#2        35         9        81         7      1325 
#1         2         3      4            5         6

#____________________________________________________________________________________________________
Pavedd_Drive_data=as.factor(my_data$Pavedd_Drive)
Pavedd_Drive_data_numeric=as.numeric(Pavedd_Drive_data)
table(Pavedd_Drive_data_numeric)

Pavedd_Drive_data_numeric

#______________________________________________________________________________________

W_Deck_Area_data=my_data$W_Deck_Area
W_Deck_Area_data=abs(W_Deck_Area_data)
summary(W_Deck_Area_data)

W_Deck_Area_data

#______________________________________________________________________________________________________

Open_Lobby_Area_data=my_data$Open_Lobby_Area
Open_Lobby_Area_data=abs(Open_Lobby_Area_data)
summary(Open_Lobby_Area_data)

Open_Lobby_Area_data

#__________________________________________________________________________________________________

Enclosed_Lobby_Area_data=my_data$Enclosed_Lobby_Area
Enclosed_Lobby_Area_data=abs(Enclosed_Lobby_Area_data)
summary(Enclosed_Lobby_Area_data)


#__________________________________________________________________________________________________

Three_Season_Lobby_Area_data=my_data$Three_Season_Lobby_Area
Three_Season_Lobby_Area_data=abs(Three_Season_Lobby_Area_data)
summary(Three_Season_Lobby_Area_data)


#_________________________________________________________________________________________--
Screen_Lobby_Area_data=my_data$Screen_Lobby_Area
Screen_Lobby_Area_data=abs(Screen_Lobby_Area_data)
summary(Screen_Lobby_Area_data)

#______________________________________________________________________________________________
Pool_Area_data=my_data$Pool_Area
summary(Pool_Area_data)
table(Pool_Area_data)

#________________________________________________________________________________________________

Pool_Quality_data=my_data$Pool_Quality
Pool_Quality_data[is.na(Pool_Quality_data)]="No Pool"
#new5=Pool_Quality_data[Pool_Quality_data=="NA"]
#Pool_Quality_data=replace(Pool_Quality_data,new5,"no pool")
Pool_Quality_data=as.factor(Pool_Quality_data)
Pool_Quality_data_numeric=as.numeric(Pool_Quality_data)
#Pool_Quality_data_numeric=na.omit(Pool_Quality_data_numeric)
table(Pool_Quality_data_numeric)


Pool_Quality_data
#Ex      Fa      Gd no pool 
#2       2       3    1452 
#_____________________________________________________________________________________________

Fence_Quality_data=my_data$Fence_Quality
is.na(Fence_Quality_data)
Fence_Quality_data[is.na(Fence_Quality_data)]="No Fence"
#new6=Fence_Quality_data[Fence_Quality_data=="NA"]
#Fence_Quality_data=replace(Fence_Quality_data,new6,"no fence")
Fence_Quality_data=as.factor(Fence_Quality_data)
Fence_Quality_data_numeric=as.numeric(Fence_Quality_data)
#Fence_Quality_data_numeric=na.omit(Fence_Quality_data_numeric)
table(Fence_Quality_data_numeric)
table(Fence_Quality_data)


#GdPrv     GdWo    MnPrv     MnWw no fence 
#59       54      157       11     1178 
#1          2      3      4          5


#_______________________________________________________________________________________
Miscellaneous_Feature_data=my_data$Miscellaneous_Feature
Miscellaneous_Feature_data[is.na(Miscellaneous_Feature_data)]="No Special Features"
#new7=Miscellaneous_Feature_data[Miscellaneous_Feature_data=="NA"]
#Miscellaneous_Feature_data=replace(Miscellaneous_Feature_data,new7,"no special features")
Miscellaneous_Feature_data=as.factor(Miscellaneous_Feature_data)
Miscellaneous_Feature_data_numeric=as.numeric(Miscellaneous_Feature_data)
#Miscellaneous_Feature_data_numeric=na.omit(Miscellaneous_Feature_data_numeric)
table(Miscellaneous_Feature_data)

#Gar2 no special features                Othr                Shed                TenC 
#2                1405                     2                  49                   1 

#_____________________________________________________________________________________________
Miscellaneous_Value_data=my_data$Miscellaneous_Value
summary(Miscellaneous_Value_data)


#_______________________________________________________________________________________________
Month_Sold_data=my_data$Month_Sold
#Month_Sold_data=as.factor(Month_Sold_data)

#table(Month_Sold_data)

# 1   2   3   4   5   6   7   8   9  10  11  12 
#58  52 106 141 204 252 234 122  63  89  79  59 


#__________________________________________________________________________________________________


Year_Sold_data=my_data$Year_Sold
#Year_Sold_data=as.factor(Year_Sold_data)
#table(Year_Sold_data)

#2006 2007 2008 2009 2010 
#314  329  303  338  175 


#__________________________________________________________________________________________________

Sale_Type_data=my_data$Sale_Type
Sale_Type_data=as.factor(Sale_Type_data)
Sale_Type_data_numeric=as.numeric(Sale_Type_data)
table(Sale_Type_data)

Sale_Type_data_numeric

#COD   Con ConLD ConLI ConLw   CWD   New   Oth    WD 
#43     2     9     5     5     4   122     3  1266 
#1      2      3     4     5     6    7     8   9


#______________________________________________________________________________________

Sale_Condition_data=my_data$Sale_Condition
Sale_Condition_data=as.factor(Sale_Condition_data)
Sale_Condition_data_numeric=as.numeric(Sale_Condition_data)
table(Sale_Condition_data)

#Abnorml AdjLand  Alloca  Family  Normal Partial 
#101       4      12      20    1197     125 
#1         2        3      4       5        6


#____________________________________________________________________________________________________

Sale_Price_data=my_data$Sale_Price
summary(Sale_Price_data)
#________________________________________________________________________________________________
df=data.frame(id_data,building_class_data,zoning_class_casted1,lot_extent_data,lot_size_data,road_type_data_numeric,Lane_Type_data_numeric,
              property_shape_data_numeric,land_outline_data_numeric,Utility_Type_data_numeric,Lot_Configuration_data_numeric,Neighborhood_data,
              condition1_data,condition2_data,House_Type_data_numeric ,House_Design_data_numeric,Overall_Material_data,House_Condition_data,
              Construction_Year_data,Remodel_Year_data,Roof_Design_data_numeric,Roof_Quality_data_numeric,Exterior1st_data_numeric,
              Exterior2nd_data_numeric,Brick_Veneer_Type_data_numeric,Brick_Veneer_Area_data,Exterior_Material_data_numeric,
              Exterior_Condition_data_numeric,Foundation_Type_numeric,Basement_Height_data_numeric,Basement_Condition_data_numeric,
              Exposure_Level_data_numeric,BsmtFinType1_data_numeric,BsmtFinSF1_data,BsmtFinType2_data_numeric,
              BsmtFinSF2_data,BsmtUnfSF_data,Total_Basement_Area_data,Heating_Type_data_numeric,heating_data,airconditing_data,
              electrical_system_data_numeric
                        ,first_floor_area_data,second_floor_area_data,LowQualFinSF_data,Grade_Living_Area_data,
              Underground_Full_Bathroom_data,Underground_Half_Bathroom_data,Full_Bathroom_Above_Grade_data,Half_Bathroom_Above_Grade,
              Bedroom_withoutground,Kitchen_Above_Grade_data,Kitchen_Quality_data_numeric,Rooms_Above_Grade_data,Functional_Rate_data_numeric,
              Fireplaces_data,Fireplace_Quality_data_numeric,Garage_data_numeric,Garage_Built_Year_data,
              Garage_Finish_Year_data_numeric,Garage_Size_data,Garage_Area_data,Garage_Quality_data_numeric,Garage_Condition_data_numeric,
              Pavedd_Drive_data_numeric,W_Deck_Area_data,Open_Lobby_Area_data,Enclosed_Lobby_Area_data,Three_Season_Lobby_Area_data,
              Screen_Lobby_Area_data,Pool_Area_data,Pool_Quality_data_numeric,Fence_Quality_data_numeric,Miscellaneous_Feature_data_numeric,
              Miscellaneous_Value_data,Month_Sold_data,Year_Sold_data,Sale_Type_data_numeric,Sale_Condition_data_numeric,Sale_Price_data)
names(df)              
dim(df)
write.csv(df,"categoricaltonumericalfinal.csv")
getwd()



#_________________________________________________________________________________________________-
names(df)
 

#numerical dataset

#numerical_data=df[,c(4,5,#17,18,26,34,36,38,44:52,54,56,61,62,66:71,75,80)]
numerical_data=df[,c(4,5,18,26,34,36,38,44:52,54,56,61,62,66:71,75,80)]
describe(numerical_data)
View(numerical_data)
dim(numerical_data)
pairs.panels(numerical_data)

boxplot(numerical_data[,c(1:15)],col="blue")
boxplot(numerical_data[,c(16:28)])


cor_data=cor(numerical_data)
library(corrplot)
corrplot(cor_data,type="lower")


#________________________________________________

#spliting the data 


set.seed(100)
sample_data=sample(2,nrow(df),replace = T,prob = c(.7,.3))
sample_data


train_data=df[sample_data==1,]
dim(train_data)
test_data=df[sample_data==2,]
dim(test_data)

write.csv(train_data,"train.csv")
model=lm( Sale_Price_data ~.  ,data=train_data)
summary(model)
#________________________________________________---------
#Utility_Type_data_numeric has all AllPub	All public Utilities
#AllPub NoSeWa 
#1458      1 
# 1         2

#Total_Basement_Area_data=BsmtFinSF1_data+BsmtUnfSF_data

#Grade_Living_Area_data=first_floor_area_data+second_floor_area_data

#_________________________________________________________________________



train_data=train_data[,-c(10,38,46)]        #these 3 columns are not useful for building the model

model1=lm(Sale_Price_data~.,data=train_data)
summary(model1)
dim(train_data)






#________________________best model_________________
model_best=lm(Sale_Price_data~lot_size_data+Overall_Material_data+House_Condition_data+
                 Roof_Quality_data_numeric+Brick_Veneer_Area_data+Exterior_Material_data_numeric
              +first_floor_area_data+second_floor_area_data+Kitchen_Quality_data_numeric+Rooms_Above_Grade_data
              +Garage_Size_data,data=train_data)
summary(model_best)


verificatoion_data=lm(Sale_Price_data~lot_size_data+Overall_Material_data+House_Condition_data+
                 Roof_Quality_data_numeric+Brick_Veneer_Area_data+Exterior_Material_data_numeric
               +first_floor_area_data+second_floor_area_data+Kitchen_Quality_data_numeric
               ,data=train_data)
summary(verificatoion_data)
#_____________________________________________________________________

model_best1=lm(Sale_Price_data~lot_size_data+Overall_Material_data+House_Condition_data+
                  Roof_Quality_data_numeric+Brick_Veneer_Area_data+Exterior_Material_data_numeric
               +first_floor_area_data+second_floor_area_data+Kitchen_Quality_data_numeric
               +Garage_Size_data,data=train_data)



summary(model_best1)







predicted_data=predict(model_best1)
predicted_data
summary(predicted_data)


original_data=df$Sale_Price_data

differ_error=original_data-predicted_data  
                                   #(213527.99-208500
differ_sq=differ_error^2
differ_sq_rt=mean(differ_sq)
rmse_data=sqrt(differ_sq_rt)
rmse_data                               #105708


#________________________________________________________________________________

hist(differ_error)          ##normal


#__________________________

                         ##linearity

plot(lot_size_data,differ_error)
plot(Overall_Material_data,differ_error)
plot(House_Condition_data,differ_error)
plot(Roof_Quality_data_numeric,differ_error)
plot(Brick_Veneer_Area_data,differ_error)
plot(Exterior_Material_data_numeric ,differ_error)
plot(first_floor_area_data,differ_error)
plot(second_floor_area_data ,differ_error)
plot(Kitchen_Quality_data_numeric,differ_error)


#_____________________________________________________________

         #independence of errors

plot(df$id_data,differ_error)

#the errors are independent (all the variables are independent )




    #___________________________________________________________----
#Ho heteroscedasticity is not present
library(lmtest)
bptest(model_best1)                    
                                            #BP = 282.1, df = 10, p-value < 2.2e-16

#p vslue is less than 0.05, fail to reject hO
#variance of residuals is constant


#______________________________________________________

#auto correlation
library(lmtest)
dwtest(model_best1)


#ho no autocorealtion
#DW = 1.9096, p-value = 0.07659
#+VE autocorrealtion

#__________________________________________________________________________--
   #predicting the data

View(test_data)
dim(test_data)
write.csv(test_data,"test.csv")

#"lot_size_data","Overall_Material_data,House_Condition_data",
#"Roof_Quality_data_numeric","Brick_Veneer_Area_data","Exterior_Material_data_numeric",
#"first_floor_area_data", "second_floor_area_data","Kitchen_Quality_data_numeric",
#"Garage_Size_data"

test_predict=data.frame( lot_size_data=100824,Overall_Material_data=8,House_Condition_data=5,
                         Roof_Quality_data_numeric=5,Brick_Veneer_Area_data=186,Exterior_Material_data_numeric=3,
                         first_floor_area_data=1694,second_floor_area_data=0,Kitchen_Quality_data_numeric=3,
                         Garage_Size_data=2)


                        
                        #Flat   Gable Gambrel     Hip  Mansard    Shed 
                        #13    1140      11     286       7       2 
                        #1        2       3       4        5      6 
                        
                        
                        ## Ex  Fa  Gd  TA 
                        #52  14 487 906
                        #1   2   3   4 
                        
                        
                        # Ex  Fa  Gd  TA 
                        #100  39 586 734 
                        #1    2    3    4


predict(model_best1,newdata=test_predict)      #306335.2  ( original price is 307000)

predict(model_best1,newdata = test_predict,interval = "confidence")  #       fit      lwr      upr
                                                                     #  306335.2   287492.5   325178





#-----------------------------------------------------------------------------

test_predict_all=data.frame( lot_size_data=test_data$lot_size_data,Overall_Material_data=test_data$Overall_Material_data,
                             House_Condition_data=test_data$House_Condition_data,
                         Roof_Quality_data_numeric=test_data$Roof_Quality_data_numeric,Brick_Veneer_Area_data=test_data$Brick_Veneer_Area_data,
                         Exterior_Material_data_numeric=test_data$Exterior_Material_data_numeric,
                         first_floor_area_data=test_data$first_floor_area_data,second_floor_area_data=test_data$second_floor_area_data,
                         Kitchen_Quality_data_numeric=test_data$Kitchen_Quality_data_numeric,
                         
                         Garage_Size_data=test_data$Garage_Size_data)

predicted_output=predict(model_best1,newdata=test_predict_all)
predicted_output
#ass(predicted_data)
error=(test_data$Sale_Price_data)-(predicted_output)
summary(error)
error_round=round(error,2)
error_round=abs(error_round)
error_mean=mean(error_round)
sqrt(error_mean)
