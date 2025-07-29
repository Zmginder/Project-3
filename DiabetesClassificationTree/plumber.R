
#Load required packages
library(plumber)
library(tidyverse)
library(rpart)

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
#* @param HighBP Class for whether the respondent has high blood pressure
#* @param GenHlth Class for respondents general health
#* @param BMI The respondents numeric BMI
#* @param HighChol Class for whether the respondent has high cholersterol
#* @param Age Class of the respondents age
#* @param CholCheck Class for whether the respondent has had a cholesterol check in the last 5 years
#* @param HvyAlcoholConsump Class for whether the respondent is a heavy alcohol consumer
#* @param Income Class for what income bracket the respondent falls into
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

#* Plot a histogram
#* @serializer png
#* @get /plot
function() {
    rand <- rnorm(100)
    hist(rand)
}

#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum
function(a, b) {
    as.numeric(a) + as.numeric(b)
}

# Programmatically alter your API
#* @plumber
function(pr) {
    pr %>%
        # Overwrite the default serializer to return unboxed JSON
        pr_set_serializer(serializer_unboxed_json())
}
