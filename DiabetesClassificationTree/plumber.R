
#Load required packages
library(plumber)
library(tidyverse)
library(rpart)
library(httr)

#Read in csv file
Diabetes<-read.csv("diabetes_binary_health_indicators_BRFSS2015.csv")

#Transform data from numeric to factor
Diabetes<-Diabetes|>
  mutate(Diabetes_binary=factor(Diabetes_binary,levels=c("0","1"),
                                labels=c("No_Diabetes","Prediabetes_or_Diabetes")),
         HighBP=factor(HighBP,levels=c("0","1"),
                       labels=c("No_High_BP","High_BP")),
         HighChol=factor(HighChol,levels=c("0","1"),
                         labels=c("No_High_Cholesterol","High_Cholesterol")),
         CholCheck=factor(CholCheck,levels=c("0","1"),
                          labels=c("No_Cholesterol_Check_In_5_Years",
                                   "Yes_Cholesterol_Check_In_5_Years")),
         Smoker=factor(Smoker,levels=c("1","0"),
                       labels=c("Smoker",
                                "Not_A_Smoker")),
         Stroke=factor(Stroke,levels=c("1","0"),
                       labels=c("Diagnosed_With_Stroke","No_Stroke")),
         HeartDiseaseorAttack=factor(HeartDiseaseorAttack,levels=c("1","0"),
                                     labels=c("Coronary_Heart_Disease_or_Myocardial_Infarction",
                                              "No_Heart_Disease_Evident")),
         PhysActivity=factor(PhysActivity,levels=c("1","0"),
                             labels=c("Physical_Activity_in_Past_30_Days",
                                      "No_Physical_Activity_in_Past_30_Days")),
         Fruits=factor(Fruits,levels=c("1","0"),
                       labels=c("Consume_Fruit_1_Or_More_Times_Per_Day",
                                "Does_Not_Consume_Fruits_Regularly")),
         Veggies=factor(Veggies,levels=c("1","0"),
                        labels=c("Consume_Vegetables_1_Or_More_Times_Per_Day",
                                 "Does_Not_Consume_Vegetables_Regularly")),
         HvyAlcoholConsump=factor(HvyAlcoholConsump,levels=c("1","0"),
                                  labels=c("Heavy_Alcohol_Consumer",
                                           "Not_Heavy_Alcohol_Consumer")),
         AnyHealthcare=factor(AnyHealthcare,levels=c("1","0"),
                              labels=c("Has_Health_Insurance",
                                       "No_Health_Insurance")),
         NoDocbcCost=factor(NoDocbcCost,levels=c("1","0"),
                            labels=c("Missed_Doctors_Visit_Due_to_Cost",
                                     "No_Cost_Barrier_to_Healthcare")),
         GenHlth=factor(GenHlth,levels=c("1","2","3","4","5"),
                        labels=c("Excellent","Very_Good","Good","Fair","Poor")),
         DiffWalk=factor(DiffWalk,levels=c("1","0"),
                         labels=c("Difficulty_With_Walking_or_Stairs",
                                  "No_Difficulty_With_Walking_or_Stairs")),
         Sex=factor(Sex,levels=c("0","1"),
                    labels=c("Female","Male")),
         Age=factor(Age,levels=c("1","2","3","4","5","6","7","8","9","10",
                                 "11","12","13"),
                    labels=c("18-24","25-29","30-34","35-39","40-44",
                             "45-49","50-54","55-59","60-64","65-69",
                             "70-74","75-79","80_or_older")),
         Education=factor(Education,levels=c("1","2","3","4","5","6"),
                          labels=c("Never_Attended_School_Or_Only_Kindergarten",
                                   "Grades_1-8",
                                   "Grades_ 9-11",
                                   "Grades_12_or_GED",
                                   "Some_College_or_Technical_School",
                                   "Graduated_College_or_Technical_School")),
         Income=factor(Income,levels=c("1","2","3","4","5","6","7","8"),
                       labels=c("Less_than_$10,000",
                                "$10,000_to_less_than_$15,000",
                                "$15,000_to_less_than_$20,000",
                                "$20,000_to_less_than_$25,000",
                                "$25,000_to_less_than_$35,000",
                                "$35,000_to_less_than_$50,000",
                                "$50,000_to_less_than_$75,000",
                                "$75,000_or_more")),
  )

#Set a seed for reproducible results
set.seed(8675309)

#Refit classification tree to entire data set
finalfull_classificationtree <- rpart(Diabetes_binary~.,
                                  data=Diabetes,
                                  method="class",
                                  parms = list(split="information"),
                                  control=rpart.control(minbucket=5,
                                                        cp=0))

#Prune tree with optimal cp=0.0004113784
cp<-0.0004113784
finaltuned_classificationtree <- prune(finalfull_classificationtree, cp=cp)

#* @apiTitle Diabetes Classification Tree API
#* @apiDescription This API utilizes a classification tree model to determine whether a respondent is likely to have prediabetes/diabetes or not. Because it is a classification tree, all the parameters must be specified even though not all parameters were chosen as decision rules in the classification tree model.

#* Make a prediction
#* @param HighBP Class for whether the respondent has high blood pressure (No_High_BP,High_BP)
#* @param HighChol Class for whether the respondent has high cholesterol (No_High_Cholesterol,High_Cholesterol)
#* @param CholCheck Class for whether the respondent has had a cholesterol check in the last 5 years (No_Cholesterol_Check_In_5_Years,Yes_Cholesterol_Check_In_5_Years)
#* @param BMI Respondents numeric BMI
#* @param Smoker Class for whether the respondent has smoked at least 100 cigarettes in their life (Smoker, Not_A_Smoker)
#* @param Stroke Class for whether the respondent has ever had a stroke (Diagnosed_With_Stroke,No_Stroke)
#* @param HeartDiseaseorAttack Class for whether the respondent has coronary heart disease or had myocardial infarction (Coronary_Heart_Disease_or_Myocardial_Infarction,No_Heart_Disease_Evident)
#* @param PhysActivity Class for whether the respondent has had physical activity in the last 30 days not including job activity (Physical_Activity_in_Past_30_Days,No_Physical_Activity_in_Past_30_Days)
#* @param Fruits Class whether the respondent consumes fruit 1 or more times per day (Consume_Fruit_1_Or_More_Times_Per_Day,Does_Not_Consume_Fruits_Regularly)
#* @param Veggies Class whether the respondent consumes vegetables 1 or more times per day (Consume_Vegetables_1_Or_More_Times_Per_Day,Does_Not_Consume_Vegetables_Regularly)
#* @param HvyAlcoholConsump Class whether the respondent is a heavy alcohol consumer defined by >=14 drinks per week for adult men and >=7 adult drinks per week for adult women (Heavy_Alcohol_Consumer,Not_Heavy_Alcohol_Consumer)
#* @param AnyHealthcare Class on whether the respondent has any kind of health coverage including health insurance, prepaid plans such as HMO, etc.(Has_Health_Insurance,No_Health_Insurance)
#* @param NoDocbcCost Class for whether the respondent could not see a doctor in the last 12 months because of cost (Missed_Doctors_Visit_Due_to_Cost,No_Cost_Barrier_to_Healthcare)
#* @param GenHlth Class for the respondents general health (Excellent,Very_Good,Good,Fair,Poor)
#* @param MentHlth Numeric days the respondent experienced poor mental health (1-30)
#* @param PhysHlth Numeric days the respondent experienced physical illness or injury (1-30)
#* @param DiffWalk Class for whether the respondent has difficult walking or climbing stairs (Difficulty_With_Walking_or_Stairs,No_Difficulty_With_Walking_or_Stairs)
#* @param Sex Class for the respondents sex (Female,Male)
#* @param Age Class for the respondents age (18-24,25-29,30-34,35-39,40-44,45-49,50-54,55-59,60-64,65-69,70-74,75-79,80_or_older)
#* @param Education Class for the respondents highest completed education level (Never_Attended_School_Or_Only_Kindergarten,Grades_1-8,Grades_ 9-11,Grades_12_or_GED,Some_College_or_Technical_School,Graduated_College_or_Technical_School)
#* @param Income Class for the respondents income bracket (Less_than_$10,000,$10,000_to_less_than_$15,000,$15,000_to_less_than_$20,000,$20,000_to_less_than_$25,000,$25,000_to_less_than_$35,000,$35,000_to_less_than_$50,000,$50,000_to_less_than_$75,000,$75,000_or_more)
#* @get /pred
function(HighBP="No_High_BP",
         HighChol="No_High_Cholesterol",
         CholCheck="Yes_Cholesterol_Check_In_5_Years",
         BMI=28.38,
         Smoker="Not_A_Smoker",
         Stroke="No_Stroke",
         HeartDiseaseorAttack="No_Heart_Disease_Evident",
         PhysActivity="Physical_Activity_in_Past_30_Days",
         Fruits="Consume_Fruit_1_Or_More_Times_Per_Day",
         Veggies="Consume_Vegetables_1_Or_More_Times_Per_Day",
         HvyAlcoholConsump="Not_Heavy_Alcohol_Consumer",
         AnyHealthcare="Has_Health_Insurance",
         NoDocbcCost="No_Cost_Barrier_to_Healthcare",
         GenHlth="Very_Good",
         MentHlth=1,
         PhysHlth=1,
         DiffWalk="No_Difficulty_With_Walking_or_Stairs",
         Sex="Female",
         Age="60-64",
         Education="Graduated_College_or_Technical_School",
         Income="$75,000_or_more") {
    pred_data<-data.frame(HighBP=HighBP,
                          HighChol=HighChol,
                          CholCheck=CholCheck,
                          BMI=as.numeric(BMI),
                          Smoker=Smoker,
                          Stroke=Stroke,
                          HeartDiseaseorAttack=HeartDiseaseorAttack,
                          PhysActivity=PhysActivity,
                          Fruits=Fruits,
                          Veggies=Veggies,
                          HvyAlcoholConsump=HvyAlcoholConsump,
                          AnyHealthcare=AnyHealthcare,
                          NoDocbcCost=NoDocbcCost,
                          GenHlth=GenHlth,
                          MentHlth=as.numeric(MentHlth),
                          PhysHlth=as.numeric(PhysHlth),
                          DiffWalk=DiffWalk,
                          Sex=Sex,
                          Age=Age,
                          Education=Education,
                          Income=Income)
    
    prediction<-predict(finaltuned_classificationtree,newdata=pred_data,type="class")
    return(prediction)
}
#THREE EXAMPLE CALLS TO API
#http://127.0.0.1:24452/pred?HighBP=High_BP&HighChol=High_Cholesterol&CholCheck=No_Cholesterol_Check_In_5_Years&BMI=40&Smoker=Smoker&Stroke=Diagnosed_With_Stroke&HeartDiseaseorAttack=Coronary_Heart_Disease_or_Myocardial_Infarction&PhysActivity=No_Physical_Activity_in_Past_30_Days&Fruits=Consume_Fruit_1_Or_More_Times_Per_Day&Veggies=Consume_Vegetables_1_Or_More_Times_Per_Day&HvyAlcoholConsump=Heavy_Alcohol_Consumer&AnyHealthcare=No_Health_Insurance&NoDocbcCost=Missed_Doctors_Visit_Due_to_Cost&GenHlth=Poor&MentHlth=10&PhysHlth=4&DiffWalk=No_Difficulty_With_Walking_or_Stairs&Sex=Female&Age=60-64&Education=Grades_12_or_GED&Income=%2450%2C000_to_less_than_%2475%2C000

#http://127.0.0.1:24452/pred?HighBP=High_BP&HighChol=No_High_Cholesterol&CholCheck=Yes_Cholesterol_Check_In_5_Years&BMI=10&Smoker=Smoker&Stroke=Diagnosed_With_Stroke&HeartDiseaseorAttack=No_Heart_Disease_Evident&PhysActivity=Physical_Activity_in_Past_30_Days&Fruits=Consume_Fruit_1_Or_More_Times_Per_Day&Veggies=Consume_Vegetables_1_Or_More_Times_Per_Day&HvyAlcoholConsump=Not_Heavy_Alcohol_Consumer&AnyHealthcare=Has_Health_Insurance&NoDocbcCost=No_Cost_Barrier_to_Healthcare&GenHlth=Poor&MentHlth=10&PhysHlth=20&DiffWalk=No_Difficulty_With_Walking_or_Stairs&Sex=Male&Age=80_or_older&Education=Graduated_College_or_Technical_School&Income=Less_than_%2410%2C000

#http://127.0.0.1:24452/pred?HighBP=No_High_BP&HighChol=No_High_Cholesterol&CholCheck=Yes_Cholesterol_Check_In_5_Years&BMI=20&Smoker=Smoker&Stroke=No_Stroke&HeartDiseaseorAttack=Coronary_Heart_Disease_or_Myocardial_Infarction&PhysActivity=Physical_Activity_in_Past_30_Days&Fruits=Does_Not_Consume_Fruits_Regularly&Veggies=Does_Not_Consume_Vegetables_Regularly&HvyAlcoholConsump=Not_Heavy_Alcohol_Consumer&AnyHealthcare=No_Health_Insurance&NoDocbcCost=No_Cost_Barrier_to_Healthcare&GenHlth=Good&MentHlth=30&PhysHlth=30&DiffWalk=No_Difficulty_With_Walking_or_Stairs&Sex=Male&Age=25-29&Education=Never_Attended_School_Or_Only_Kindergarten&Income=%2410%2C000_to_less_than_%2415%2C000

#* Provide Github Pages URL
#* @get /info
function(){
  "Zach Ginder     https://zmginder.github.io/Project-3/EDA.html"
}

# Programmatically alter your API
#* @plumber
function(pr) {
    pr %>%
        # Overwrite the default serializer to return unboxed JSON
        pr_set_serializer(serializer_unboxed_json())
}
