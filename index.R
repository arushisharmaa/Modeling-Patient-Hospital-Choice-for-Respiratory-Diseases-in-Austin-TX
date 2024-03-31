#////////////////////////////////////////////////////////////
# Code to run models and create figures for Arushi UT Poster
# Emily Javan - 2024-03-18 - ATX
#////////////////////////////////////////////////////////////

# Source functions and load need libraries
source("CODE/0117_algorithm_hospfind.R")

#///////////////////////////////////
#### Get files for all quarters ####
#///////////////////////////////////
pudf_folder_path = "INPUT_DATA/Primary_Respiratory_Austin/"
pudf_file_list = list.files(pudf_folder_path)

# 2015Q4 missing a lot of the age group for patients so need to check later
# 2015Q3 and some of Q4 use ICD-9 codes so data may be incomplete
all_quarter_df = data.frame()
for(i in 1:length(pudf_file_list)){
  pudf_file = read_csv(paste0(pudf_folder_path, pudf_file_list[i])) # open file

  sub_pudf = pudf_file %>% # subset file to relevant features for model or counting summary
    drop_na() %>%
    mutate(DISEASE = case_when( PRINC_DIAG_CODE_SUB=="J09" ~ "FLU",
                                PRINC_DIAG_CODE_SUB=="J10" ~ "FLU",
                                PRINC_DIAG_CODE_SUB=="J11" ~ "FLU",
                                PRINC_DIAG_CODE_SUB=="J12" ~ "ILI",
                                PRINC_DIAG_CODE    =="U071"~ "COVID",
                                .default = "REMOVE" ) ) %>%
    filter(!(DISEASE=="REMOVE")) %>% # Extra check to ensure only getting patients admitted to hosp bc of COVID/FLU/ILI
    dplyr::select(PROVIDER_NAME, THCIC_ID, # the hospital IDs
           ZCTA_SVI, drive_time, # only continuous variables in the data
           DISCHARGE, TYPE_OF_ADMISSION, FIRST_PAYMENT_SRC, # Categorical or ordered features
           DISEASE, PRINC_DIAG_CODE, PRINC_DIAG_CODE_SUB, any_of(c("J09", "J10", "J11", "J12", "U071")),
           RACE, SPEC_UNIT_1, PAT_AGE_ORDINAL, ETHNICITY) %>% # J09-J11=FLU, J12=ILI, and U071=COVID ICD-10 codes
    mutate(across(DISCHARGE:ETHNICITY, as.factor))
    
  all_quarter_df = bind_rows(all_quarter_df, sub_pudf) # bind all the files into 1 big file
} # end for loop over the Austin respiratory pudf files


#### Count by Hosp ####
# Null model would always pick largest hosp - 26.5% correct
#  => our models have to signficantly outperform this
pat_per_hosp_df = as.data.frame(table(all_quarter_df$THCIC_ID, all_quarter_df$PROVIDER_NAME)) %>%
  rename(THCIC_ID = Var1, TOTAL_PAT = Freq) %>%
  filter(TOTAL_PAT > 100) %>% # leaves 6 hospitals with sufficient data for training
  mutate(TOTAL_PAT_SUM = sum(TOTAL_PAT),
         HOSP_FREQ = TOTAL_PAT/TOTAL_PAT_SUM)

reduced_hosp_df = all_quarter_df %>%             # 5,892 total hospitalizations
  filter(THCIC_ID %in% pat_per_hosp_df$THCIC_ID) %>% # 5,709
  separate(DISCHARGE, into = c("YEAR", "QUARTER"), sep = "Q", remove = F ) %>%
  mutate(YEAR = as.numeric(YEAR),
         QUARTER = as.numeric(QUARTER)) %>%
  filter(!(TYPE_OF_ADMISSION=="9")) %>% # remove invalid hospital admissions
  filter(SPEC_UNIT_1 %in% c("A", "I", "P")) %>% # A=acute, I=intensive, P=pediatric # 5,678
  droplevels() %>%
  mutate(THCIC_ID = as.factor(THCIC_ID)) # must be factor

#/////////////////////////////////////
#### Get count of hosp by quarter ####
#/////////////////////////////////////
# 2015Q4 - 2019Q3 is pre-pandemic only flu/ili codes
# 2019Q4 - 2022Q4 likely covid spread, however some patient may not have been diagnosed as covid early on just ili
# => we'll ignore the ICD-10 codes within the model for this reason
count_quarter = reduced_hosp_df %>%
  group_by(DISCHARGE) %>%
  summarise(total_hosp = n(),
            mean_drivetime     = mean(drive_time),
            median_drivetime   = median(drive_time),
            lower_ci_drivetime = confint(lm(drive_time ~ 1), level=0.95)[1],
            upper_ci_drivetime = confint(lm(drive_time ~ 1), level=0.95)[2]) %>%
  mutate(lower_ci_drivetime = ifelse(lower_ci_drivetime < 0, 0, lower_ci_drivetime)) %>%
  ungroup() %>%
  mutate(DISEASE = "TOTAL HOSP")
count_disease_quarter = reduced_hosp_df %>%
  group_by(DISCHARGE, DISEASE) %>%
  summarise(total_hosp = n(),
            mean_drivetime     = mean(drive_time),
            median_drivetime   = median(drive_time),
            lower_ci_drivetime = confint(lm(drive_time ~ 1), level=0.95)[1],
            upper_ci_drivetime = confint(lm(drive_time ~ 1), level=0.95)[2]) %>%
  mutate(lower_ci_drivetime = ifelse(lower_ci_drivetime < 0, 0, lower_ci_drivetime)) %>%
  ungroup() %>%
  bind_rows(count_quarter)

#//////////////////////////
#### Plot disease hosp ####
#//////////////////////////
# Plot by disease and total hospitalizations through time just as visual of pandemic impact on hospitals
cdq_plot = ggplot(count_disease_quarter, 
                  aes(x=DISCHARGE, y=total_hosp, group=DISEASE, color=DISEASE, shape=DISEASE))+
  geom_line(alpha=0.5, linewidth=2)+
  geom_point(size=5)+
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(y="Total Austin Respiratory Hospitalizations", x="Year-Quarter of Patient Discharge")+
  theme_bw(base_size = 18)
ggsave(filename="FIGURES/Arushi_Poster/count_disease_quarter_plot.png", plot=cdq_plot, bg="white", 
       width=11, height=7.5, units="in", dpi=1200)

#////////////////
#### Example ####
#////////////////
# Heat map of drive time as an example visualization to get a sense of a feature through time
browns = c("#ede5cf","#e0c2a2","#d39c83","#c1766f","#a65461","#813753","#541f3f")
dt_heatmap = ggplot(count_disease_quarter %>% filter((DISEASE=="TOTAL HOSP")), 
       aes(x=DISEASE, y=DISCHARGE, fill=mean_drivetime))+
  geom_tile()+
  scale_fill_gradientn(colours=browns) +
  labs(x="Mean Drive Time Austin Hospitalizations", y="Year-Quarter of Patient Discharge", fill="")+
  theme_bw(base_size = 20)+
  theme(legend.position="bottom",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.background=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())
ggsave(filename="FIGURES/Arushi_Poster/drive_time_heatmap.png", plot=dt_heatmap, bg="white", 
       width=7.5, height=9, units="in", dpi=1200)

#///////////////////
#### Run Models ####
#///////////////////
# Runs multi-nomial logistic regression and random forest

# Model formula run inside function
# THCIC_ID ~ RACE + ZCTA_SVI + drive_time + SPEC_UNIT_1 + ETHNICITY + PAT_AGE_ORDINAL
pre_pandemic_df = reduced_hosp_df %>%  
  filter(YEAR <= 2019) %>%
  filter(!(YEAR==2019 & QUARTER==4)) %>% # 1153 total hospitalizations
  select(THCIC_ID, ZCTA_SVI, drive_time, RACE, ETHNICITY,  
         PAT_AGE_ORDINAL, SPEC_UNIT_1) # TYPE_OF_ADMISSION made no improvement in model
pandemic_df = reduced_hosp_df %>%
  filter((YEAR==2019 & QUARTER==4) | (YEAR>=2020)) %>% # 4556 total hospitalizations
  select(THCIC_ID, ZCTA_SVI, drive_time, RACE, ETHNICITY,  
         PAT_AGE_ORDINAL, SPEC_UNIT_1)
all_data_df = bind_rows(pre_pandemic_df, pandemic_df)

# Did 5 folds => 5-fold cross validation
nfolds=5
pre_pand_result = perform_cross_validation(dataset=pre_pandemic_df, 
                                           sub_folder="PRE-PANDEMIC/", num_folds=nfolds) # seed=123, 
pandemic_result = perform_cross_validation(dataset=pandemic_df,     
                                           sub_folder="PANDEMIC/",     num_folds=nfolds) # seed=123, 
all_data_result = perform_cross_validation(dataset=all_data_df,     
                                           sub_folder="ALL_DATA/",     num_folds=nfolds) # seed=123, 

#/////////////////////////////////////
#### Aggregate Confusion Matrices ####
#/////////////////////////////////////
# Loop over all the confusion matrices and bind rows together
pre_confusion = pan_confusion = all_confusion = data.frame()
for(i in 1:nfolds){
  temp_pre_confusion = data.frame(pre_pand_result$RF_ConfusionMatrix[[i]]$table) %>% mutate(MODEL = "Random Forest") %>%
    bind_rows(data.frame(pre_pand_result$Multinom_ConfusionMatrix[[i]]$table)    %>% mutate(MODEL = "MultiNom Regression")) %>%
    mutate(THCIC_ID = Prediction,
           TIME_PERIOD = "PRE",
           FOLD=i)
  temp_pan_confusion = data.frame(pandemic_result$RF_ConfusionMatrix[[i]]$table) %>% mutate(MODEL = "Random Forest") %>%
    bind_rows(data.frame(pandemic_result$Multinom_ConfusionMatrix[[i]]$table)    %>% mutate(MODEL = "MultiNom Regression")) %>%
    mutate(THCIC_ID = Prediction,
           TIME_PERIOD = "PAND",
           FOLD=i)
  temp_all_confusion = data.frame(all_data_result$RF_ConfusionMatrix[[i]]$table) %>% mutate(MODEL = "Random Forest") %>%
    bind_rows(data.frame(all_data_result$Multinom_ConfusionMatrix[[i]]$table)    %>% mutate(MODEL = "MultiNom Regression")) %>%
    mutate(THCIC_ID = Prediction,
           TIME_PERIOD = "ALL",
           FOLD=i)
  
  pre_confusion = bind_rows(pre_confusion, temp_pre_confusion)
  pan_confusion = bind_rows(pan_confusion, temp_pan_confusion)
  all_confusion = bind_rows(all_confusion, temp_all_confusion)
} # end loop over confusion matrices

cm_df = bind_rows(pre_confusion, pan_confusion, all_confusion) %>%
  group_by(FOLD, MODEL, TIME_PERIOD, Reference) %>%
  mutate(TOTAL_PAT = sum(Freq)) %>%
  ungroup() %>%
  mutate(PERCENT_PREDICT = Freq/TOTAL_PAT) %>%
  mutate(DIAG = ifelse(Prediction==Reference, T, NA))

cm_df_mean = cm_df %>%
  group_by(MODEL, TIME_PERIOD, Reference, Prediction, DIAG) %>%
  summarise(PERCENT_PREDICT_mean = round(mean(PERCENT_PREDICT), 2) ) %>%
  ungroup() %>%
  mutate(THCIC_ID=Reference,
         Reference = case_when( Reference == "35000"  ~ "A",
                                Reference == "497000" ~ "B",
                                Reference == "602000" ~ "C",
                                Reference == "797600" ~ "D",
                                Reference == "829900" ~ "E",
                                Reference == "852000" ~ "F",
                                .default = "REMOVE" ),
         Prediction = case_when(Prediction == "35000"  ~ "StDavids - A",
                                Prediction == "497000" ~ "AscSeton - B",
                                Prediction == "602000" ~ "StDavidsS - C",
                                Prediction == "797600" ~ "AscSetonNW - D",
                                Prediction == "829900" ~ "NorthAustin - E",
                                Prediction == "852000" ~ "DellChildren - F",
                                .default = "REMOVE" ))

#//////////////////////////////
#### Plot Confusion Matrix ####
#//////////////////////////////
order=c("PRE", "PAND", "ALL") #  Flu/ILI  Flu/ILI/COVID-19 
time_label = c(`PRE`="Pre-Pandemic\n2015Q4-2019Q3", `PAND`="Pandemic\n2019Q4-2022Q4", `ALL`="All Admits\n2015Q4-2022Q4")
army_rose = rev(c("#798234","#a3ad62","#d0d3a2","#fdfbe4","#f0c6c3","#df91a3","#d46780"))
prediction_order = rev(c("StDavids - A",   "AscSeton - B",    "StDavidsS - C", 
                     "AscSetonNW - D", "NorthAustin - E", "DellChildren - F"))

mean_cm_plot = ggplot(cm_df_mean, aes(x=Reference, y=factor(Prediction, level=prediction_order),
                                      fill=PERCENT_PREDICT_mean)) +
  labs(x="Correct Hospital", y="Predicted Hospital")+
  geom_tile() + theme_bw(base_size=20) + coord_equal() +
    geom_tile(data = cm_df_mean[!is.na(cm_df_mean$DIAG), ], aes(color = DIAG), linewidth = 1) + # add black box around diagonal
  scale_color_manual(guide = "none", values = c(`TRUE` = "black"))+
  facet_grid( MODEL ~ factor(TIME_PERIOD, order),
              labeller = labeller( .cols = as_labeller(time_label) )) +
  #scale_fill_distiller(palette="PiYG", direction=1) +
  scale_fill_gradientn(colours = army_rose)+
  guides(fill="none") + # removing legend for `fill`
  geom_text(aes(label=PERCENT_PREDICT_mean), color="black") # + # printing values in tile
  #theme(axis.text.x = element_text(angle = 75, vjust = 0.6)) # , hjust=1
ggsave(filename="FIGURES/Arushi_Poster/mean_confusion_matrix.png", plot=mean_cm_plot, bg="white", 
       width=11, height=9, units="in", dpi=1200)

#///////////////////////////////////////////////
# RF Permute of best performing Test/Train Split
#///////////////////////////////////////////////

# best performing (highest accuracy of all folds/times)
# The output across folds was really similar and stable
plotTrace(all_data_result$RF_Model[[2]])

# ggsave wont't save it so doing screen shot for now
# will probably need to take data and make our own custom ggplot
# maybe look at function code to get each plot and do cowplot::plot_grid
plotImportance(all_data_result$RF_Model[[2]])

#///////////////////////////////////////
#### Multinomial regression summary ####
#///////////////////////////////////////
# Doesn't show p-values like we'd like need to find better summary
summary(all_data_result$Multinom_Model[[2]])



#/////////////////////////////
#### Train and Test Count ####
#/////////////////////////////
all_data_train = data.frame(table(all_data_result$Train_Data[[1]]$THCIC_ID)) %>% 
  rename(THCIC_ID = Var1, ALL_DATA_TRAIN = Freq)
all_data_test = data.frame(table(all_data_result$Test_Data[[1]]$THCIC_ID)) %>% 
  rename(THCIC_ID = Var1, ALL_DATA_TEST = Freq)
prepand_data_train = data.frame(table(pre_pand_result$Train_Data[[1]]$THCIC_ID)) %>% 
  rename(THCIC_ID = Var1, PRE_PAND_TRAIN = Freq)
prepand_data_test = data.frame(table(pre_pand_result$Test_Data[[1]]$THCIC_ID)) %>% 
  rename(THCIC_ID = Var1, PRE_PAND_TEST = Freq)
pand_data_train = data.frame(table(pandemic_result$Train_Data[[1]]$THCIC_ID)) %>% 
  rename(THCIC_ID = Var1, PANDEMIC_TRAIN = Freq)
pand_data_test = data.frame(table(pandemic_result$Test_Data[[1]]$THCIC_ID)) %>% 
  rename(THCIC_ID = Var1, PANDEMIC_TEST = Freq)

temp_hosp_df = pat_per_hosp_df %>%
  filter(!(Var2=="Seton Medical Center"))
hosp_name_df$TOTAL_PAT[which(hosp_name_df$THCIC_ID=="497000")] = (127+958)
hosp_name_df = temp_hosp_df %>%
  mutate(HOSP_FREQ = TOTAL_PAT/TOTAL_PAT_SUM) %>%
  rename(HOSP_NAME=Var2) %>%
  left_join(all_data_train, by="THCIC_ID") %>%
  left_join(all_data_test, by="THCIC_ID") %>%
  left_join(prepand_data_train, by="THCIC_ID") %>%
  left_join(prepand_data_test, by="THCIC_ID") %>%
  left_join(pand_data_train, by="THCIC_ID") %>%
  left_join(pand_data_test, by="THCIC_ID")
  




#///////////////////////////////////////////////////////////////////////
#### Test BRF performance without the specialty unit they reside in ####
#///////////////////////////////////////////////////////////////////////
# SPEC_UNIT_1 could be treated as risk group (high/low = icu/acute)
# All other features about the patient or ZIP code
#  could be estimated at census block group level from ACS
# Work towards proportion of ILI hospitalized in each CBG that will go to
#  which hospital

train_data = all_data_result$Train_Data[[2]]
test_data = all_data_result$Test_Data[[2]]



sampsize = balancedSampsize(train_data$THCIC_ID)
rfPermute_model = rfPermute(THCIC_ID ~ RACE + ZCTA_SVI + drive_time + ETHNICITY + PAT_AGE_ORDINAL, 
                            data = train_data, ntree = 500, num.rep = 1000, num.cores = 6,
                            replace = FALSE, sampsize = sampsize)

rfPermute_performance = predict(rfPermute_model, newdata = test_data)

rf_conf_mat <- caret::confusionMatrix(as.factor(rfPermute_performance), as.factor(test_data$THCIC_ID))


all_data_result$RF_ConfusionMatrix[[2]]





