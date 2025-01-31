# Rewritten project script without additional summarising of ndns data 

# Loading libraries

library("dplyr")
library("ggplot2")
options(scipen = 999) #Off the scientific notation


# 0 Loading & preparing the data

# Finding the files from NDNS for dietary data
files <- list.files(path = here::here("UK-NDNS-main/data/tab"),
                    pattern = "foodleveldietarydata") %>% 
  stringr::str_subset(., "9|10|11")

# Loading the 3 years of survey together

ndns <- paste0(here::here("UK-NDNS-main/data/tab//"), files) %>% 
  purrr::map_df(~readr::read_delim(., col_types = readr::cols(.default = "c"), 
                                   delim='\t',  locale = readr::locale(encoding = "Latin1"))) 
# Food consumption 
ndns$TotalGrams <- as.numeric(ndns$TotalGrams)

#Loading individual data
ind <- readr::read_delim(here::here("UK-NDNS-main/data", "tab", "ndns_rp_yr9-11a_indiv_20211020.tab"),
                         delim = "\t")


# 4) Getting average food consumption per person

ndns2 <- ndns %>% mutate(across(c(24:81), as.numeric)) %>% 
  group_by(seriali, SurveyYear, DayNo, AgeR, Sex, Country,
           # DiaryDate, DayofWeek, DayNo, 
           FoodName, FoodNumber, SubFoodGroupCode,
           SubFoodGroupDesc, MainFoodGroupCode, MainFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)))


# Checking main food groups supplying pro

ndns2 %>% 
  # ggplot(aes(forcats::fct_reorder(MainFoodGroupDesc, Proteing), Proteing)) + 
  ggplot(aes(forcats::fct_reorder(MainFoodGroupDesc, Proteing), Proteing)) + 
  geom_boxplot() + coord_flip()

# 5) Getting average per food subgroup (top 60)
ndns2 %>%
  group_by(seriali, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  #  group_by(MainFoodGroupCode, MainFoodGroupDesc) %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode, SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>% 
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Proteing) %>% 
  arrange(desc(Proteing)) %>% ungroup() %>% slice_head(n=60) %>% 
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Proteing), Proteing)) + 
  geom_boxplot() + coord_flip() 


#Checking veg/vegan Y/N
ind$Veg <- as.character(ind$Veg)


#Preparing ind for merging with ndns2:
ind2 <- ind %>% select(seriali,Veg, VegeChk, VeganChk, vegetarn)


# Converting variables into character
ind2$seriali <- as.character(ind2$seriali)
# is this necessary?

# 6) Merging ind2 with ndns2 to obtain vegan info for each individual
ndns3 <- ndns2 %>%    
  left_join(ind2, by = "seriali")

#Checking main vegetarian food groups supplying protein

ndns3 %>% filter(Veg==2) %>% 
  group_by(seriali, MainFoodGroupCode, MainFoodGroupDesc,
           SubFoodGroupCode, SubFoodGroupDesc,
           TotalGrams, Proteing) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>% 
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Proteing) %>% 
  arrange(desc(Proteing)) %>% ungroup() %>% slice_head(n=20) %>% View()

  
### OLD VERSION ###
  #  group_by(seriali, MainFoodGroupCode, MainFoodGroupDesc) %>% 
  group_by(seriali, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc, Veg) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  #  group_by(MainFoodGroupCode, MainFoodGroupDesc) %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode, SubFoodGroupDesc, Veg) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>% 
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Proteing, Veg) %>% 
  arrange(desc(Proteing)) %>% ungroup() %>% slice_head(n=20) %>% View()

####################################
#Create DietChk, checking self-reported dietary groups against what was actually consumed
DietChk <- ndns3 %>% 
  select(seriali, Veg, VegeChk, VeganChk, vegetarn, FoodName, FoodNumber, SubFoodGroupCode, SubFoodGroupDesc, MainFoodGroupCode, MainFoodGroupDesc)
#Vegans and vegetarians comsuming dairy and meat products respectively. Recategorise into 'true' consumption categories

# 7) Create new dietary groups

#Identify keywords that indicate non-vegetarian (meat consumption), pescatarian or vegetarian (dairy or eggs)
meat_keywords <- c("BACON AND HAM", 
                   "BEEF VEAL AND DISHES", 
                   "BURGERS AND KEBABS", 
                   "CHICKEN AND TURKEY DISHES", 
                   "COATED CHICKEN", 
                   "LAMB AND DISHES", 
                   "LIVER AND DISHES",
                   "MEAT PIES AND PASTRIES", 
                   "OTHER MEAT AND MEAT PRODUCTS",
                   "PORK AND DISHES",
                   "SAUSAGES")

meat_keywords_foodnames <- c("CHICKEN",
                             "LAMB",
                             "BEEF",
                             "PORK",
                             "QUICHE, MEAT BASED")

pesc_keywords <- c("OILY FISH",
                   "OTHER WHITE FISH SHELLFISH & FISH DISHES", 
                   "WHITE FISH COATED OR FRIED") 

pesc_keywords_foodnames <- c("COD LIVER OIL",
                             "FISH OIL",
                             "FISH",
                             "FOREVER ARTIC SEA",
                             "OMEGA 3")

vegetarian_keywords <- c("EGGS AND EGG DISHES",
                         "YOGURT")

vegetarian_keywords_foodnames <- c("MILK SEMI-SKIMMED PASTEURISED WINTER", 
                                   "MILK SKIMMED PASTEURISED WINTER", 
                                   "CHEESE CHEDDAR ENGLISH",
                                   "CHEESE CHEDDAR IRISH",
                                   "CHEESE CHEDDAR ANY OTHER OR FOR RECIPES", 
                                   "CHEESE PARMESAN",
                                   "CHEESE LEICESTERSHIRE",
                                   "CHEESE HALLOUMI",
                                   "CHEESE GRUYERE",
                                   "CHEESE MASCARPONE",
                                   "CHEESE FETA",
                                   "RICOTTA",
                                   "PANEER",
                                   "CHEESE PROCESSED SLICES OR BLOCKS",
                                   "CHEESE SPREADS, TRIANGLES, PLAIN, DAIRYLEA ONLY",
                                   "CHEESE DOUBLE GLOUCESTER",
                                   "MACARONI CHEESE",
                                   "CHEESECAKE",
                                   "CHEESE SOFT FULL FAT SOFT CHEESE - PHILADELPHIA TYPE", 
                                   "CHEESE SOFT MEDIUM FAT", 
                                   "CHEESE SOFT LOW FAT",
                                   "CHEESE OR CHEESE AND TOMATO PIZZA WITH VEGS AND/OR FRUIT. NO MEAT, NO FISH, WITH ANY BASE, RETAIL", 
                                   "CHEESE MOZZARELLA", 
                                   "CHEESE CREAM FULLFAT", 
                                   "GOATS CHEESE", 
                                   "YOGURT LOW FAT",
                                   "YOGURT , WHOLE MILK, NATURAL, UNSWEETENED",
                                   "YOGURT, FULL FAT",
                                   "CHEESE AND TOMATO PIZZA",
                                   "ICE CREAM", 
                                   "ICECREAM",
                                   "ICE-CREAM",
                                   "CREAM DOUBLE",
                                   "CREAM SINGLE PASTEURISED",
                                   "EGG",
                                   "WHEY PROTEIN",
                                   "WHEY POWDER",
                                   "YOGURT")

# Create a new variable to flag meat/fish/dairy consumption
ndns3 <- ndns3 %>% mutate(
  Consumed_Meat =
    grepl(paste(meat_keywords, collapse = "|"), MainFoodGroupDesc, ignore.case = TRUE)|
    grepl(paste(meat_keywords_foodnames, collapse = "|"), FoodName, ignore.case = TRUE))

ndns3 <- ndns3 %>% mutate(
  Consumed_Fish = 
    grepl(paste(pesc_keywords, collapse = "|"), MainFoodGroupDesc, ignore.case = TRUE)|
    grepl(paste(pesc_keywords_foodnames, collapse = "|"), FoodName, ignore.case = TRUE))

ndns3 <- ndns3 %>% mutate(
  Consumed_DairyEgg = 
    grepl(paste(vegetarian_keywords, collapse = "|"), MainFoodGroupDesc, ignore.case = TRUE)|
    grepl(paste(vegetarian_keywords_foodnames, collapse = "|"), FoodName, ignore.case = TRUE))


#Info on reported dietary group and true/false intakes 
DietGrp <- ndns3 %>% select(seriali, DayNo, Consumed_Meat, Consumed_Fish, Consumed_DairyEgg)

DietGrp <- DietGrp %>% 
  left_join(ind2, by = "seriali")


#Create 'Diet' variable 
DietGrp <- DietGrp %>% mutate(Diet = case_when(
  Consumed_Meat ~ "Neither", 
  Consumed_Fish ~ "Pescatarian",
  Consumed_DairyEgg ~ "Vegetarian",
  TRUE ~ "Vegan"))

#Detertmine true diet per individual
determine_diet <- function(diets){
  if ("Neither" %in% diets) {
    return("Neither")
  } else if ("Pescatarian" %in% diets) {
    return("Pescatarian")
  }
  else if ("Vegetarian" %in% diets) {
    return("Vegetarian")
  } else {return("Vegan")}}


#View diet group changes
DietGrp_summary <- DietGrp %>% 
  group_by(seriali, Veg) %>% 
  summarize(Overall_Diet = determine_diet(Diet)) %>% ungroup()

DietGrp2 <- DietGrp_summary %>% ungroup() %>% select(seriali, Overall_Diet)

#Check
DietGrp %>% filter(Diet=='Vegan') %>% View()

DietGrp2 %>% filter(Overall_Diet=='Vegan') %>% View()
# 3 vegans 


# 8) Individual dietary data of true dietary groups
ndns4 <- ndns3 %>% 
  left_join(DietGrp2, by = "seriali")

write.csv(here::here("data", 
                     "ndns_dietgroups.csv"))


# 9) Demographic data

#Summarise by gender, age, geo location and household income 
ind3 <- ind %>% select(seriali, Sex, AgeR, agegr1, Weight, region, GOR, eqv3)
ind3$seriali <- as.character(ind3$seriali)

#Inclusion of demographic data with individual data in ndns5
ndns5 <- ndns4 %>% left_join(ind3, by = "seriali")

demographics <- DietGrp_summary %>% 
  left_join(ind3, by = "seriali")
write.csv(here::here("data", 
                     "demographics.csv"))

# 10) 
#View top 60 food groups supplying protein to pop 
ndns4 %>% 
  group_by(seriali, FoodName, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc, Overall_Diet) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc,
           SubFoodGroupCode, SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Proteing) %>% 
  arrange(desc(Proteing)) %>% ungroup() %>% slice_head(n=60) %>%
  # ggplot FoodName, Proteing to obtain top 20 foods providing protein to Vegans
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Proteing), Proteing)) + 
  geom_boxplot() + coord_flip() + ylim(0, 45 ) +
  labs(title = 'Top 20 Food Groups Supplying Protein to UK Population',
       x = 'Food Groups',
       y = 'Average Protein (g)')

ggsave(filename= "Plots/New Plots/Pop_SFG_av_pro.png")


# 11)
# View top 20 food groups supplying protein to each dietary group
#Vegan
# OLD VERSION
ndns4 %>% filter(Overall_Diet=='Vegan') %>% 
  #  group_by(seriali, MainFoodGroupCode, MainFoodGroupDesc) %>% 
  group_by(seriali, FoodName, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc, Overall_Diet) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  #  group_by(MainFoodGroupCode, MainFoodGroupDesc) %>% 
  group_by(FoodName, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode, SubFoodGroupDesc, Overall_Diet) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%  
  select(FoodName, MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Proteing, Overall_Diet) %>% 
  arrange(desc(Proteing)) %>% ungroup() %>% slice_head(n=20) %>% 
# ggplot FoodName, Proteing to obtain top 20 foods providing protein to Vegans
ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Proteing), Proteing)) + 
  geom_boxplot() + coord_flip()

# NEW VERSION
ndns4 %>% filter(Overall_Diet=='Vegan') %>% 
  group_by(seriali, FoodName, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc, Overall_Diet) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc,
           SubFoodGroupCode, SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Proteing) %>% 
  arrange(desc(Proteing)) %>% ungroup() %>% slice_head(n=20) %>%
  # ggplot FoodName, Proteing to obtain top 20 foods providing protein to Vegans
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Proteing), Proteing)) + 
  geom_boxplot() + coord_flip() + ylim(0, 45 ) +
  labs(title = 'Top 20 Food Groups Supplying Protein to Vegans',
       x = 'Food Groups',
       y = 'Average Protein (g)')

ggsave(filename= "Plots/New Plots/Vegan_SFG_av_pro.png")



#Vegetarian
# OLD VERSION
ndns4 %>% filter(Overall_Diet=='Vegetarian') %>%
  #  group_by(seriali, MainFoodGroupCode, MainFoodGroupDesc) %>% 
  group_by(seriali, FoodName, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc, Overall_Diet) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  #  group_by(MainFoodGroupCode, MainFoodGroupDesc) %>% 
  group_by(FoodName, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode, SubFoodGroupDesc, Overall_Diet) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>% 
  select(FoodName, MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Proteing, Overall_Diet) %>% 
  arrange(desc(Proteing)) %>% ungroup() %>% slice_head(n=20) %>%
  # ggplot FoodName, Proteing to obtain top 20 foods providing protein to Vegans
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Proteing), Proteing)) + 
  geom_boxplot() + coord_flip()

#### Alternate version
ndns4 %>% filter(Overall_Diet=='Vegetarian') %>% 
  group_by(seriali, FoodName, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc, Overall_Diet) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc,
           SubFoodGroupCode, SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Proteing) %>% 
  arrange(desc(Proteing)) %>% ungroup() %>% slice_head(n=20) %>% 
  # ggplot FoodName, Proteing to obtain top 20 foods providing protein to Vegans
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Proteing), Proteing)) + 
  geom_boxplot() + coord_flip() +  ylim(0, 45) +
    labs(title = 'Top 20 Food Groups Supplying Protein to Vegetarians',
         x = 'Food Groups',
         y = 'Average Protein (g)')
  
ggsave(filename= "Plots/New Plots/Vegetarian_SFG_av_pro.png")

ndns4 %>% filter(Overall_Diet=='Vegetarian') %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc,
           SubFoodGroupCode, SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Proteing) %>% 
  arrange(desc(Proteing)) %>% ungroup() %>% slice_head(n=20) %>% 
  # ggplot FoodName, Proteing to obtain top 20 foods providing protein to Vegans
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Proteing), Proteing)) + 
  geom_boxplot() + coord_flip()

ndns4 %>% filter(Overall_Diet=='Vegetarian') %>% 
  group_by(seriali, FoodName, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc, Overall_Diet) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc,
           SubFoodGroupCode, SubFoodGroupDesc, FoodName) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc, FoodName,
         TotalGrams, Proteing) %>% 
  arrange(desc(Proteing)) %>% ungroup() %>% slice_head(n=20) %>%
  # ggplot FoodName, Proteing to obtain top 20 foods providing protein to Vegans
  ggplot(aes(forcats::fct_reorder(FoodName, Proteing), Proteing)) + 
  geom_boxplot() + coord_flip()



#Pescatarian
# OLD VERSION
ndns4 %>% filter(Overall_Diet=='Pescatarian') %>% 
  #  group_by(seriali, MainFoodGroupCode, MainFoodGroupDesc) %>% 
  group_by(seriali, FoodName, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc, Overall_Diet) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  #  group_by(MainFoodGroupCode, MainFoodGroupDesc) %>% 
  group_by(FoodName, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode, SubFoodGroupDesc, Overall_Diet) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>% 
  select(FoodName, MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Proteing, Overall_Diet) %>% 
  arrange(desc(Proteing)) %>% ungroup() %>% slice_head(n=20) %>%
  # ggplot FoodName, Proteing to obtain top 20 foods providing protein to Vegetarians
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Proteing), Proteing)) + 
  geom_boxplot() + coord_flip()


#### Alternate version
ndns4 %>% filter(Overall_Diet=='Pescatarian') %>% 
  group_by(seriali, FoodName, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc, Overall_Diet) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc,
           SubFoodGroupCode, SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Proteing) %>% 
  arrange(desc(Proteing)) %>% ungroup() %>% slice_head(n=20) %>% 
  # ggplot FoodName, Proteing to obtain top 20 foods providing protein to Vegans
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Proteing), Proteing)) + 
  geom_boxplot() + coord_flip() +  ylim(0, 45) +
  labs(title = 'Top 20 Food Groups Supplying Protein to Pescatarians',
       x = 'Food Groups',
       y = 'Average Protein (g)')

ggsave(filename= "Plots/New Plots/Pescatarian_SFG_av_pro.png")



#Omnivore/Neither
ndns4 %>% filter(Overall_Diet=='Neither') %>% 
  ###################################################
## POSSIBLE ADDITIONAL/UNNECESSARY SECTION
#  group_by(seriali, MainFoodGroupCode, MainFoodGroupDesc) %>% 
group_by(seriali, FoodName, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
         SubFoodGroupDesc, Overall_Diet) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  ######################################################
#  group_by(MainFoodGroupCode, MainFoodGroupDesc) %>% 
group_by(FoodName, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode, SubFoodGroupDesc, Overall_Diet) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>% 
  select(FoodName, MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Proteing, Overall_Diet) %>% 
  arrange(desc(Proteing)) %>% ungroup() %>% slice_head(n=20) %>%
  # ggplot FoodName, Proteing to obtain top 20 foods providing protein to Vegans
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Proteing), Proteing)) + 
  geom_boxplot() + coord_flip()

#### Alternate version
ndns4 %>% filter(Overall_Diet=='Neither') %>% 
  group_by(seriali, FoodName, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc, Overall_Diet) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc,
           SubFoodGroupCode, SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Proteing) %>% 
  arrange(desc(Proteing)) %>% ungroup() %>% slice_head(n=20) %>% 
  # ggplot FoodName, Proteing to obtain top 20 foods providing protein to Vegans
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Proteing), Proteing)) + 
  geom_boxplot() + coord_flip() +  ylim(0, 45) +
  labs(title = 'Top 20 Food Groups Supplying Protein to Omnivores',
       x = 'Food Groups',
       y = 'Average Protein (g)')

ggsave(filename= "Plots/New Plots/Omnivore_SFG_av_pro.png")


ndns4 %>% filter(Overall_Diet=='Neither') %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc,
           SubFoodGroupCode, SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Proteing) %>% 
  arrange(desc(Proteing)) %>% ungroup() %>% slice_head(n=20) %>%
  # ggplot FoodName, Proteing to obtain top 20 foods providing protein to Vegans
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Proteing), Proteing)) + 
  geom_boxplot() + coord_flip()


  
###########################################################################################
devtools::install_github("jmcphers/bookmarkr")

#Check nutrition powders and drinks 

NPD <- ndns5 %>% filter(SubFoodGroupCode=='50E') %>%
  select(seriali, Overall_Diet, FoodName, SubFoodGroupCode, SubFoodGroupDesc, Energykcal, EnergykJ, Proteing, Fatg, Carbohydrateg, TotalGrams, Waterg)

NPD <- NPD %>% 
  mutate(Proteing100g = (Proteing / TotalGrams) * 100)

NPD %>% 
  write.csv(here::here("data",
                       "NPD.csv"))

Total_AA_dietary_data %>% filter(SubFoodGroupCode=='50E') %>% View()


# Nutrition keywords
#What about fybogel? 

powder_keywords <- c("dry",
                     "PROTEIN POWDER",
                     "MEAL REPLACEMENT MILKSHAKE POWDER",
                     "MEAL REPLACEMENT POWDER",
                     "POWDER ONLY",
                     "COMPLAN",
                     "COMPLAN",
                     "whey",
                     "METAGENICS ULTRAINFLAMX",
                     "SHAKE POWDER",
                     "FYBOGEL",
                     "CAMBRIDGE WEIGHT PLAN CHOCOLATE SHAKE FORTIFIED",
                     "LIGHTER LIFE TOTAL BALANCE SOUP POWDER FORTIFIED")

drinks_keywords <- c("liquid",
                     "PROTEIN SHAKE",
                     "shakes",
                     "DUNN'S RIVER NURISHMENT",
                     "ENSURE",
                     "MEAL REPLACEMENT DRINK",
                     "ENERGY DRINK",
                     "INFATRINI")

bars_keywords <- c("BARS")

NPD <- NPD %>% mutate(
  nutrition_powders = 
    grepl(paste(powder_keywords, collapse = "|"), FoodName, ignore.case = TRUE))

NPD <- NPD %>% mutate(
  nutrition_drinks = 
    grepl(paste(drinks_keywords, collapse = "|"), FoodName, ignore.case = TRUE))

NPD <- NPD %>% mutate(
  nutrition_bars = 
    grepl(paste(bars_keywords, collapse = "|"), FoodName, ignore.case = TRUE))

NPDGrp <- NPD %>% select(seriali, DayNo, nutrition_powders, nutrition_drinks, nutrition_bars)

NPDGrp <- NPDGrp %>% mutate(NPD = case_when(
  nutrition_powders ~ "Powders", 
  nutrition_drinks ~ "Drinks",
  TRUE ~ "Bars"))

labels <- NPDGrp %>% ungroup() %>% 
  select(FoodNumber, NPD)



#Average consumption of NPDs

Total_AA_dietary_data %>% filter(SubFoodGroupCode=='50E') %>% 
  filter(Overall_Diet=='Vegetarian') %>% ungroup() %>% 
  summarise(mean_proteing=mean(Proteing, na.rm = TRUE))

  summarise(mean_LYS=mean(LYSg, na.rm = TRUE)) %>% print()
#== 9.93 protein 
#== 2.29 lysine 


Total_AA_dietary_data %>% filter(SubFoodGroupCode=='50E') %>% 
  filter(Overall_Diet=='Pescatarian') %>% ungroup() %>% 
  summarise(mean_Protein_g100g=mean(Protein_g100g, na.rm = TRUE)) %>% print()
#== 15.7 p CONSUMPTION 
#== 32.8 p composition
#== 2.29 L

Total_AA_dietary_data %>% filter(SubFoodGroupCode=='50E') %>% 
  filter(Overall_Diet=='Neither') %>% ungroup() %>% 
  summarise(mean_LYS=mean(LYSg, na.rm = TRUE)) %>% print()
#== 10.9 P
#== 2.29 L

Total_AA_dietary_data %>% filter(Overall_Diet=='Vegetarian') %>% 
  group_by(seriali, FoodName, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc, Overall_Diet) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc,
           SubFoodGroupCode, SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Proteing, Protein_g100g) %>% 
  arrange(desc(Protein_g100g)) %>% ungroup() %>% slice_head(n=20) %>% 
  # ggplot FoodName, Proteing to obtain top 20 foods providing protein to Vegans
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Protein_g100g), Protein_g100g)) + 
  geom_boxplot() + coord_flip() +  ylim(0, 45) +
  labs(title = 'Top 20 Food Groups Supplying Protein to Vegetarians',
       x = 'Food Groups',
       y = 'Average Protein (g)')

### Use g/100g rather than total protein g???

NPD %>% ungroup %>% 
  filter(Overall_Diet== 'Vegan') %>%
  summarise(mean_protein = mean(Proteing, na.rm = TRUE)) %>% print()
# === 17.1

NPD %>% ungroup %>% 
  filter(Overall_Diet== 'Vegetarian') %>% 
  summarise(mean_protein = mean(Proteing, na.rm = TRUE)) %>% print()
# === 4.55

NPD %>% ungroup %>% 
  filter(Overall_Diet== 'Pescatarian') %>%
  summarise(mean_protein = mean(Proteing, na.rm = TRUE)) %>% print()
# === 15.7

NPD %>% ungroup %>% 
  filter(Overall_Diet== 'Neither') %>% 
  summarise(mean_protein = mean(Proteing, na.rm = TRUE)) %>% print()
# === 10.9



###########################################################################################
# 12) Load AA Data 
library(readr)

#Load Matching data (excel file)
Food_match <- read_csv("NDNS_USDA_Food_Matching_data_V4.csv")
View(Food_match)

#Amino acid consumption
Food_match$Avg_protein <- as.numeric(Food_match$Avg_protein)
Food_match$Avg_water <- as.numeric(Food_match$Avg_water)
Food_match$Avg_Zn <- as.numeric(Food_match$Avg_Zn)
Food_match$Avg_Lys <- as.numeric(Food_match$Avg_Lys)
Food_match$Avg_Tryp <- as.numeric(Food_match$Avg_Tryp)

Food_match2 <- Food_match %>% 
  select(Sr_no,
         MainFoodGroupCode, 
         MainFoodGroupDesc, 
         SubFoodGroupCode, 
         SubFoodGroupDesc, 
         TotalGrams, 
         Proteing,
         Protein_g100g,
         Avg_protein,
         Avg_water,
         Avg_Zn,
         Avg_Lys,
         Avg_Tryp)

Food_match2 %>% group_by(MainFoodGroupCode, SubFoodGroupCode) %>% count() %>% 
  arrange(desc(n))

#Remove NA rows 
Food_match3 <- Food_match2 %>% drop_na()

Food_match4 <- Food_match3 %>% 
  select(SubFoodGroupCode,
         Protein_g100g,
         Avg_protein,
         Avg_water,
         Avg_Zn,
         Avg_Lys,
         Avg_Tryp)


#Select top 60 foods providing protein to diets
#Top 60 foods providing protein to survey participants
ndns6 <- ndns4 %>%
  group_by(seriali, DayNo, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  #  group_by(MainFoodGroupCode, MainFoodGroupDesc) %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode, SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>% 
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Proteing) %>% 
  arrange(desc(Proteing)) %>% ungroup() %>% slice_head(n=60)


#Merge Food_match data with ndns
AA_merged <- ndns6 %>% 
  left_join(Food_match4, by = 'SubFoodGroupCode')
View(AA_merged)


#Examining average AA intake by dietary group 
#Merge AA_merged with ndns5

AA_merged2 <- ndns5 %>% 
  left_join(Food_match4, by = "SubFoodGroupCode")
View(AA_merged2)

#exclude na rows so only have top 60 foods providing protein
#is this the right thing to do??? 
AA_merged2 <- AA_merged2 %>% drop_na(Protein_g100g, Avg_protein, Avg_water, Avg_Zn, Avg_Lys, Avg_Tryp)

#########
# 13) Examine AA intakes by dietary group   
#### THIS IS REPEATED LATER FOR PLOTS SENT TO EDWARD
#Top foods providing Lysine to Vegans 

AA_merged2 %>% filter(Overall_Diet=='Vegan') %>% 
  View()

AA_merged2 %>% filter(Overall_Diet=='Vegan') %>% 
  group_by(seriali, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc, FoodName) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  #  group_by(MainFoodGroupCode, MainFoodGroupDesc) %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode, SubFoodGroupDesc, FoodName) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>% 
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc, FoodName,
         TotalGrams, Proteing, Avg_Lys) %>% 
  arrange(desc(Proteing)) %>% ungroup() %>% slice_head(n=60) %>%
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Avg_Lys), Avg_Lys)) + 
  geom_boxplot() + coord_flip()

AA_merged2  %>% 
  group_by(seriali, FoodName, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc, Overall_Diet) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc,
           SubFoodGroupCode, SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Avg_Lys) %>% 
  arrange(desc(Avg_Lys)) %>% ungroup() %>% slice_head(n=60) %>% 
  # ggplot FoodName, Proteing to obtain top 20 foods providing protein to Vegans
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Avg_Lys), Avg_Lys)) + 
  geom_boxplot() + coord_flip() +  ylim(0, 5.5) +
  labs(title = 'Top 20 Food Groups Supplying Lysine to Population',
       x = 'Food Groups',
       y = 'Average Lysine (g)')

ggsave(filename= "Plots/New Plots/POP_SFG_av_LYS.png")

AA_merged2 %>% filter(Overall_Diet=='Vegan') %>% 
  group_by(seriali, FoodName, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc, Overall_Diet) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc,
           SubFoodGroupCode, SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Avg_Lys) %>% 
  arrange(desc(Avg_Lys)) %>% ungroup() %>% slice_head(n=20) %>% 
  # ggplot FoodName, Proteing to obtain top 20 foods providing protein to Vegans
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Avg_Lys), Avg_Lys)) + 
  geom_boxplot() + coord_flip() +  ylim(0, 6.5) +
  labs(title = 'Top 20 Food Groups Supplying Lysine to Vegans',
       x = 'Food Groups',
       y = 'Average Lysine (g)')

ggsave(filename= "Plots/New Plots/Vegan_SFG_av_LYS.png")

AA_merged2 %>% filter(Overall_Diet=='Vegetarian') %>% 
  group_by(seriali, FoodName, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc, Overall_Diet) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc,
           SubFoodGroupCode, SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Avg_Lys) %>% 
  arrange(desc(Avg_Lys)) %>% ungroup() %>% slice_head(n=20) %>% 
  # ggplot FoodName, Proteing to obtain top 20 foods providing protein to Vegans
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Avg_Lys), Avg_Lys)) + 
  geom_boxplot() + coord_flip() +  ylim(0, 6.5) +
  labs(title = 'Top 20 Food Groups Supplying Lysine to Vegetarians',
       x = 'Food Groups',
       y = 'Average Lysine (g)')

ggsave(filename= "Plots/New Plots/Vegetarian_SFG_av_LYS.png")

AA_merged2 %>% filter(Overall_Diet=='Pescatarian') %>% 
  group_by(seriali, FoodName, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc, Overall_Diet) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc,
           SubFoodGroupCode, SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Avg_Lys) %>% 
  arrange(desc(Avg_Lys)) %>% ungroup() %>% slice_head(n=20) %>% 
  # ggplot FoodName, Proteing to obtain top 20 foods providing protein to Vegans
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Avg_Lys), Avg_Lys)) + 
  geom_boxplot() + coord_flip() +  ylim(0, 6.5) +
  labs(title = 'Top 20 Food Groups Supplying Lysine to Pescatarians',
       x = 'Food Groups',
       y = 'Average Lysine (g)')

ggsave(filename= "Plots/New Plots/Pescatarian_SFG_av_LYS.png")


AA_merged2 %>% filter(Overall_Diet=='Neither') %>% 
  group_by(seriali, FoodName, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc, Overall_Diet) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc,
           SubFoodGroupCode, SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Avg_Lys) %>% 
  arrange(desc(Avg_Lys)) %>% ungroup() %>% slice_head(n=20) %>% 
  # ggplot FoodName, Proteing to obtain top 20 foods providing protein to Vegans
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Avg_Lys), Avg_Lys)) + 
  geom_boxplot() + coord_flip() +  ylim(0, 6.5) +
  labs(title = 'Top 20 Food Groups Supplying Lysine to Omnivores',
       x = 'Food Groups',
       y = 'Average Lysine (g)')

ggsave(filename= "Plots/New Plots/Omnivore_SFG_av_LYS.png")




#Top foods providing tryp to vegans
AA_merged2 %>% filter(Overall_Diet=='Vegan') %>% 
  group_by(seriali, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  #  group_by(MainFoodGroupCode, MainFoodGroupDesc) %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode, SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>% 
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Proteing, Avg_Tryp) %>% 
  arrange(desc(Proteing)) %>% ungroup() %>% slice_head(n=60) %>% 
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Avg_Tryp), Avg_Tryp)) + 
  geom_boxplot() + coord_flip()

#Top foods providing lys to vegetarians 
AA_merged2 %>% filter(Overall_Diet=='Vegetarian') %>% 
  group_by(seriali, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  #  group_by(MainFoodGroupCode, MainFoodGroupDesc) %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode, SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>% 
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Proteing, Avg_Lys) %>% 
  arrange(desc(Proteing)) %>% ungroup() %>% slice_head(n=60) %>%
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Avg_Lys), Avg_Lys)) + 
  geom_boxplot() + coord_flip()

#Tryp to vegetarians
AA_merged2 %>% filter(Overall_Diet=='Vegetarian') %>% 
  group_by(seriali, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  #  group_by(MainFoodGroupCode, MainFoodGroupDesc) %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode, SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>% 
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Proteing, Avg_Tryp) %>% 
  arrange(desc(Proteing)) %>% ungroup() %>% slice_head(n=60) %>% 
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Avg_Tryp), Avg_Tryp)) + 
  geom_boxplot() + coord_flip()


#Lys to Pesc
AA_merged2 %>% filter(Overall_Diet=='Pescatarian') %>% 
  group_by(seriali, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  #  group_by(MainFoodGroupCode, MainFoodGroupDesc) %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode, SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>% 
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Proteing, Avg_Lys) %>% 
  arrange(desc(Proteing)) %>% ungroup() %>% slice_head(n=60) %>%
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Avg_Lys), Avg_Lys)) + 
  geom_boxplot() + coord_flip()


#Tryp to Pesc
AA_merged2 %>% filter(Overall_Diet=='Pescatarian') %>% 
  group_by(seriali, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  #  group_by(MainFoodGroupCode, MainFoodGroupDesc) %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode, SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>% 
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Proteing, Avg_Tryp) %>% 
  arrange(desc(Proteing)) %>% ungroup() %>% slice_head(n=60) %>% 
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Avg_Tryp), Avg_Tryp)) + 
  geom_boxplot() + coord_flip()

#Lys to Omni
AA_merged2 %>% filter(Overall_Diet=='Neither') %>% 
  group_by(seriali, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  #  group_by(MainFoodGroupCode, MainFoodGroupDesc) %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode, SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>% 
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Proteing, Avg_Lys) %>% 
  arrange(desc(Proteing)) %>% ungroup() %>% slice_head(n=60) %>% 
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Avg_Lys), Avg_Lys)) + 
  geom_boxplot() + coord_flip()

#Tryp to Omni
AA_merged2 %>% filter(Overall_Diet=='Neither') %>% 
  group_by(seriali, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  #  group_by(MainFoodGroupCode, MainFoodGroupDesc) %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode, SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>% 
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Proteing, Avg_Tryp) %>% 
  arrange(desc(Proteing)) %>% ungroup() %>% slice_head(n=60) %>% 
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Avg_Tryp), Avg_Tryp)) + 
  geom_boxplot() + coord_flip()

#############################################################################################
## To provide SubFoodGroupCode to collect AA information from FoodData Central database
install.packages("zoo")

library(zoo)

Data <- Food_match %>%  select(SubFoodGroupCode, `NDB no`, `FDC Id`) %>% 
  filter(!is.na(`NDB no`)) %>% mutate(SubFoodGroupCode=zoo::na.locf(SubFoodGroupCode))

write.csv(Data, "Food_ID.csv", row.names = FALSE)

Data$SubFoodGroupCode <- na.locf(Data$SubFoodGroupCode)

tail(Data)
############################################################################################

# 14) Load complete AA composition data
library(readr)
AA_matches_full <- read_csv("AA_matches_full.csv")


# Obtain average amino acid information for each sub food group
Avg_AA_matches <- AA_matches_full %>% 
  select(SubFoodGroupCode, TRPg, THRg, ILEg, LEUg, LYSg, METg, CYSg, PHEg, TYRg, VALg, ARGg, HISTNg, ALAg, ASPg, GLUg, GLYg, PROg, SERg, HYPg) %>% 
  group_by(SubFoodGroupCode) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)))

# Add in proteing_100g information to new AA inforamtion
Food_match5 <- Food_match4 %>% 
  select(SubFoodGroupCode, Protein_g100g)

Avg_AA_matches <- Avg_AA_matches %>% 
  left_join(Food_match5, by = 'SubFoodGroupCode')


# Merge total Average AA information with ndns data
Full_food_information <- ndns6 %>% 
  left_join(Avg_AA_matches, by = 'SubFoodGroupCode')


#Merge with ndns5 for individual/dietary group intake analysis 
Food_information_condensed <- Full_food_information %>% 
  select(SubFoodGroupCode, Protein_g100g, TRPg, THRg, ILEg, LEUg, LYSg, METg, CYSg, PHEg, TYRg, VALg, ARGg, HISTNg, ALAg, ASPg, GLUg, GLYg, PROg, SERg, HYPg)


Full_AA_dietary_data <- ndns5 %>% 
  left_join(Food_information_condensed, by = "SubFoodGroupCode")

#Exclude NA rows so only have top 60 foods providing protein 
#Do not exclude NR from HYP as so many missing 
Full_AA_dietary_data <- Full_AA_dietary_data %>% 
  drop_na(TRPg, THRg, ILEg, LEUg, LYSg, METg, CYSg, PHEg, TYRg, VALg, ARGg, HISTNg, ALAg, ASPg, GLUg, GLYg, PROg, SERg) 


##############################################################################

# 15) Integration of digestibility dataset with sub food group matches
Digest_DIAAS_matches <- read_csv("NDNS_USDA_DIAAS_matching.csv")

#Remove unnecessary columns
Digest_DIAAS_matches <- Digest_DIAAS_matches %>% 
  select(-`USDA_item description`, -`NDB no`, -`FDC Id`, -`Avg_protein, g/100g_USDA`, 
         -`Water, g`, -Zn_mg, -Lys._g, -Tryp_g, -`Avg Protein Diff`, -`% Diff`,
         -Avg_protein, -Avg_water, -Avg_Zn, -Avg_Lys, -Avg_Tryp)

Digest_DIAAS_matches <- Digest_DIAAS_matches %>% select(-...38, -...39, -...40, -...41, -...42, -...43, -...44, -...45, -...46, -...47, -...48, -...49, -...50, -...51)

#Remove empty observations
Digest_DIAAS_matches <- Digest_DIAAS_matches %>% drop_na()

#Create new dataset with only digestiblity coefficients
Digest_DIAAS_matches2 <- Digest_DIAAS_matches %>% 
  select(-Sr_no, -MainFoodGroupCode, -MainFoodGroupDesc, -SubFoodGroupDesc, -TotalGrams, -Proteing, -Protein_g100g)

Digest_DIAAS_matches2 <- Digest_DIAAS_matches2 %>% 
  rename(Digest_food = Food_item_name)

#Merge with dietary data
#NDNS dietary information containing full AA contents and AA digestibility, and DIAAS scores and limiting AA 
Total_AA_dietary_data <- Full_AA_dietary_data %>% 
  left_join(Digest_DIAAS_matches2, by = 'SubFoodGroupCode')


#Digestibility coefficients
#Mean digestible total protein consumed 
Digest_DIAAS_matches3 <- Digest_DIAAS_matches2 %>% 
  select(-Digest_food)

Full_food_information <- Full_food_information %>% 
  left_join(Digest_DIAAS_matches3, by = 'SubFoodGroupCode')

#Multiply digestibility coefficients with composition data
Full_food_information <- Full_food_information %>% 
  mutate(
    Digest_protein = Protein_g100g * Protein_Dig,
    Digest_TRP = TRPg * TRP_COEF,
    Digest_THR = THRg * THR_COEF,
    Digest_ILE = ILEg * ILE_COEF,
    Digest_LEU = LEUg * LEU_COEF,
    Digest_LYS = LYSg * LYS_COEF,
    Digest_MET = METg * MET_COEF,
    Digest_CYS = CYSg * CYS_COEF,
    Digest_PHE = PHEg * PHE_COEF,
    Digest_TYR = TYRg * TYR_COEF,
    Digest_VAL = VALg * VAL_COEF,
    Digest_ARG = ARGg * ARG_COEF, 
    Digest_HIST = HISTNg * HIS_COEF
  )



Digest_data <- Full_food_information %>% select(SubFoodGroupCode, 
                                                Digest_protein, 
                                                Digest_TRP, 
                                                Digest_THR, 
                                                Digest_ILE, 
                                                Digest_LEU,
                                                Digest_LYS,
                                                Digest_MET,
                                                Digest_CYS,
                                                Digest_PHE,
                                                Digest_TYR,
                                                Digest_VAL,
                                                Digest_ARG,
                                                Digest_HIST,
)
#merge new digestibility data with consumption data
Total_AA_dietary_data <- Total_AA_dietary_data %>% 
  left_join(Digest_data, by = 'SubFoodGroupCode')


Total_AA_dietary_data <- Total_AA_dietary_data %>% 
  rename(Digest_protein_g100g = Digest_protein)

Total_AA_dietary_data <- Total_AA_dietary_data %>% 
  mutate(Digest_total_protein = Protein_Dig * Proteing)

Total_AA_dietary_data %>% 
  write.csv(here::here("data", 
                       "total_AA_dietary_data.csv"))
#############################################################################################
## Additional work for Edward: foods providing lysine to diets

#Food groups supplying protein, lysine and digestible lysine 
Total_AA_dietary_data %>% 
  group_by(seriali, FoodName, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc, Overall_Diet) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc,
           SubFoodGroupCode, SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Proteing) %>% 
  arrange(desc(Proteing)) %>% ungroup() %>% slice_head(n=60) %>% 
  # ggplot FoodName, Proteing to obtain top 20 foods providing protein to POP
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Proteing), Proteing)) + 
  geom_boxplot() + coord_flip() +  ylim(0, 45) +
  labs(title = 'Top 60 Food Groups Supplying Protein to Population',
       x = 'Food Groups',
       y = 'Average Protein (g)')

ggsave(filename= "Plots/New Plots/POP_proteing.png")

Total_AA_dietary_data %>% 
  group_by(seriali, FoodName, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc, Overall_Diet) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc,
           SubFoodGroupCode, SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, LYSg) %>% 
  arrange(desc(LYSg)) %>% ungroup() %>% slice_head(n=60) %>% 
  # ggplot FoodName, Proteing to obtain top 20 foods providing protein to POP
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, LYSg), LYSg)) + 
  geom_boxplot() + coord_flip() +  ylim(0, 6.5) +
  labs(title = 'Top 60 Food Groups Supplying Lysine to Population',
       x = 'Food Groups',
       y = 'Average Lysine (g)')

ggsave(filename= "Plots/New Plots/POP_SFG_LYSg.png")

Total_AA_dietary_data %>% 
  group_by(seriali, FoodName, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc, Overall_Diet) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc,
           SubFoodGroupCode, SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Digest_LYS) %>% 
  arrange(desc(Digest_LYS)) %>% ungroup() %>% slice_head(n=60) %>% 
  # ggplot FoodName, Proteing to obtain top 20 foods providing protein to Pop
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Digest_LYS), Digest_LYS)) + 
  geom_boxplot() + coord_flip() +  ylim(0, 6.5) +
  labs(title = 'Top 20 Food Groups Supplying Digestible Lysine to Population',
       x = 'Food Groups',
       y = 'Average Digestible Lysine (g)')

ggsave(filename= "Plots/New Plots/Pop_SFG_DIGLYS.png")

Total_AA_dietary_data %>% filter(Overall_Diet=='Vegan') %>% 
  group_by(seriali, FoodName, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc, Overall_Diet) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc,
           SubFoodGroupCode, SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Proteing) %>% 
  arrange(desc(Proteing)) %>% ungroup() %>% slice_head(n=20) %>% 
  # ggplot FoodName, Proteing to obtain top 20 foods providing protein to Vegans
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Proteing), Proteing)) + 
  geom_boxplot() + coord_flip() +  ylim(0, 45) +
  labs(title = 'Top 20 Food Groups Supplying Protein to Vegans',
       x = 'Food Groups',
       y = 'Average Protein (g)')

ggsave(filename="Plots/New Plots/Vegan_proteing.png")

Total_AA_dietary_data %>% filter(Overall_Diet=='Vegan') %>% 
  group_by(seriali, FoodName, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc, Overall_Diet) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc,
           SubFoodGroupCode, SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, LYSg) %>% 
  arrange(desc(LYSg)) %>% ungroup() %>% slice_head(n=20) %>% 
  # ggplot FoodName, Proteing to obtain top 20 foods providing protein to Vegans
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, LYSg), LYSg)) + 
  geom_boxplot() + coord_flip() +  ylim(0, 6.5) +
  labs(title = 'Top 20 Food Groups Supplying Lysine to Vegans',
       x = 'Food Groups',
       y = 'Average Lysine (g)')

ggsave(filename= "Plots/New Plots/Vegan_SFG_LYSg.png")

Total_AA_dietary_data %>% filter(Overall_Diet=='Vegan') %>% 
  group_by(seriali, FoodName, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc, Overall_Diet) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc,
           SubFoodGroupCode, SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Digest_LYS) %>% 
  arrange(desc(Digest_LYS)) %>% ungroup() %>% slice_head(n=20) %>% 
  # ggplot FoodName, Proteing to obtain top 20 foods providing protein to Vegans
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Digest_LYS), Digest_LYS)) + 
  geom_boxplot() + coord_flip() +  ylim(0, 6.5) +
  labs(title = 'Top 20 Food Groups Supplying Digestible Lysine to Vegans',
       x = 'Food Groups',
       y = 'Average Digestible Lysine (g)')

ggsave(filename= "Plots/New Plots/Vegan_SFG_av_DIGLYS.png")


Total_AA_dietary_data %>% filter(Overall_Diet=='Vegetarian') %>% 
  group_by(seriali, FoodName, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc, Overall_Diet) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc,
           SubFoodGroupCode, SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Proteing) %>% 
  arrange(desc(Proteing)) %>% ungroup() %>% slice_head(n=20) %>% 
  # ggplot FoodName, Proteing to obtain top 20 foods providing protein to Vegetarians
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Proteing), Proteing)) + 
  geom_boxplot() + coord_flip() +  ylim(0, 45) +
  labs(title = 'Top 20 Food Groups Supplying Protein to Vegetarians',
       x = 'Food Groups',
       y = 'Average Protein (g)')

ggsave(filename="Plots/New Plots/Vegetarian_proteing.png")

Total_AA_dietary_data %>% filter(Overall_Diet=='Vegetarian') %>% 
  group_by(seriali, FoodName, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc, Overall_Diet) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc,
           SubFoodGroupCode, SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, LYSg) %>% 
  arrange(desc(LYSg)) %>% ungroup() %>% slice_head(n=20) %>% 
  # ggplot FoodName, Proteing to obtain top 20 foods providing protein to vegetarians
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, LYSg), LYSg)) + 
  geom_boxplot() + coord_flip() +  ylim(0, 6.5) +
  labs(title = 'Top 20 Food Groups Supplying Lysine to Vegetarians',
       x = 'Food Groups',
       y = 'Average Lysine (g)')

ggsave(filename= "Plots/New Plots/Vegetarian_SFG_LYSg.png")

Total_AA_dietary_data %>% filter(Overall_Diet=='Vegetarian') %>% 
  group_by(seriali, FoodName, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc, Overall_Diet) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc,
           SubFoodGroupCode, SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Digest_LYS) %>% 
  arrange(desc(Digest_LYS)) %>% ungroup() %>% slice_head(n=20) %>% 
  # ggplot FoodName, Proteing to obtain top 20 foods providing protein to Vegetarians
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Digest_LYS), Digest_LYS)) + 
  geom_boxplot() + coord_flip() +  ylim(0, 6.5) +
  labs(title = 'Top 20 Food Groups Supplying Digestible Lysine to Vegetarians',
       x = 'Food Groups',
       y = 'Average Digestible Lysine (g)')

ggsave(filename= "Plots/New Plots/Vegetarian_SFG_av_DIGLYS.png")

Total_AA_dietary_data %>% filter(Overall_Diet=='Pescatarian') %>% 
  group_by(seriali, FoodName, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc, Overall_Diet) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc,
           SubFoodGroupCode, SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, LYSg) %>% 
  arrange(desc(LYSg)) %>% ungroup() %>% slice_head(n=20) %>% 
  # ggplot FoodName, Proteing to obtain top 20 foods providing protein to Pescatarians
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, LYSg), LYSg)) + 
  geom_boxplot() + coord_flip() +  ylim(0, 6.5) +
  labs(title = 'Top 20 Food Groups Supplying Lysine to Pescatarians',
       x = 'Food Groups',
       y = 'Average Lysine (g)')

ggsave(filename= "Plots/New Plots/Pescatarians_SFG_LYSg.png")

Total_AA_dietary_data %>% filter(Overall_Diet=='Pescatarian') %>% 
  group_by(seriali, FoodName, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc, Overall_Diet) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc,
           SubFoodGroupCode, SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Digest_LYS) %>% 
  arrange(desc(Digest_LYS)) %>% ungroup() %>% slice_head(n=20) %>% 
  # ggplot FoodName, Proteing to obtain top 20 foods providing protein to Pescs
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Digest_LYS), Digest_LYS)) + 
  geom_boxplot() + coord_flip() +  ylim(0, 6.5) +
  labs(title = 'Top 20 Food Groups Supplying Digestible Lysine to Pescatarians',
       x = 'Food Groups',
       y = 'Average Digestible Lysine (g)')

ggsave(filename= "Plots/New Plots/Pescatarians_SFG_av_DIGLYS.png")

Total_AA_dietary_data %>% filter(Overall_Diet=='Neither') %>% 
  group_by(seriali, FoodName, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc, Overall_Diet) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc,
           SubFoodGroupCode, SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, LYSg) %>% 
  arrange(desc(LYSg)) %>% ungroup() %>% slice_head(n=20) %>% 
  # ggplot FoodName, Proteing to obtain top 20 foods providing protein to Omni
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, LYSg), LYSg)) + 
  geom_boxplot() + coord_flip() +  ylim(0, 6.5) +
  labs(title = 'Top 20 Food Groups Supplying Lysine to Omnivores',
       x = 'Food Groups',
       y = 'Average Lysine (g)')

ggsave(filename= "Plots/New Plots/Omni_SFG_LYSg.png")

Total_AA_dietary_data %>% filter(Overall_Diet=='Neither') %>% 
  group_by(seriali, FoodName, MainFoodGroupCode, MainFoodGroupDesc, SubFoodGroupCode,
           SubFoodGroupDesc, Overall_Diet) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  group_by(MainFoodGroupCode, MainFoodGroupDesc,
           SubFoodGroupCode, SubFoodGroupDesc) %>% 
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
  select(MainFoodGroupCode, MainFoodGroupDesc,
         SubFoodGroupCode, SubFoodGroupDesc,
         TotalGrams, Digest_LYS) %>% 
  arrange(desc(Digest_LYS)) %>% ungroup() %>% slice_head(n=20) %>% 
  # ggplot FoodName, Proteing to obtain top 20 foods providing protein to Omni
  ggplot(aes(forcats::fct_reorder(SubFoodGroupDesc, Digest_LYS), Digest_LYS)) + 
  geom_boxplot() + coord_flip() +  ylim(0, 6.5) +
  labs(title = 'Top 20 Food Groups Supplying Digestible Lysine to Omnivores',
       x = 'Food Groups',
       y = 'Average Digestible Lysine (g)')

ggsave(filename= "Plots/New Plots/Omni_SFG_av_DIGLYS.png")

######################################
# 16) Determine if people are consuming adequate protein: 

# Obtain daily data 
daily_data <- Total_AA_dietary_data %>% ungroup() %>% 
  select(seriali, 
         AgeR.x, 
         Sex.x, 
         DayNo, 
         Overall_Diet, 
         Energykcal,
         Proteing, 
         Digest_total_protein,
         TRPg,
         THRg,
         ILEg,
         LEUg,
         LYSg,
         METg,
         CYSg,
         PHEg,
         TYRg,
         VALg,
         ARGg,
         HISTNg,
         ALAg,
         ASPg,
         GLUg,
         GLYg,
         PROg,
         SERg,
         HYPg,
         Digest_TRP, 
         Digest_THR, 
         Digest_ILE, 
         Digest_LEU, 
         Digest_LYS, 
         Digest_MET, 
         Digest_CYS, 
         Digest_PHE, 
         Digest_TYR, 
         Digest_VAL, 
         Digest_ARG, 
         Digest_HIST)


#Daily protein per individual (all foods)
# Using averaged NDNS data
all_protein <- ndns2 %>% 
  select(seriali,
         AgeR,
         Sex,
         DayNo,
         Energykcal,
         Proteing) %>% 
  group_by(seriali, DayNo, AgeR, Sex) %>% 
  summarise(sum_Proteing = sum(Proteing),
            sum_Energykcal = sum(Energykcal))
### Using original NDNS data: 
all_protein <- ndns %>%
  mutate(across(c(24:81), as.numeric)) %>% 
  select(seriali,
         AgeR,
         Sex,
         DayNo,
         Energykcal,
         Proteing) %>% 
  group_by(seriali, DayNo, AgeR, Sex) %>% 
  summarise(sum_Proteing = sum(Proteing),
            sum_Energykcal = sum(Energykcal))


all_protein$AgeR <- as.numeric(as.character(all_protein$AgeR))

#male adult = 85.4 (ndns original form) excluding kcal < 800
# = 78.5 ndns2 (averaged consumption)
all_protein %>% ungroup() %>% 
  filter(Sex==1 & AgeR >=19 & AgeR <= 64 & sum_Energykcal >= 800) %>% 
  summarise(mean_daily_proteing = mean(sum_Proteing, na.rm = TRUE)) %>% 
  print()

#female adult = 67.4 excluding kcal < 500
# = 62.5 ndns2
all_protein %>% ungroup() %>% 
  filter(Sex==2 & AgeR >=19 & AgeR <= 64 & sum_Energykcal >= 500) %>% 
  summarise(mean_daily_proteing = mean(sum_Proteing, na.rm = TRUE)) %>% 
  print()





#Daily protein & energy per individual top 60
daily_proteing <- daily_data %>% 
  group_by(seriali, DayNo, AgeR.x, Sex.x, Overall_Diet) %>% 
  summarise(sum_Proteing = sum(Proteing, na.rm = TRUE),
            sum_digest_Proteing = sum(Digest_total_protein, na.rm = TRUE),
            sum_Energykcal = sum(Energykcal, na.rm = TRUE))


# Mean and SD : ADULT FEMALE ENERGY
female_adult_energy <- daily_proteing %>% ungroup() %>% 
  filter(Sex.x==2 & AgeR.x >= 19 & AgeR.x <= 64 & sum_Energykcal>=500) %>% 
  summarise(mean_female_energy = mean(sum_Energykcal, na.rm = TRUE),
            sd_female_energy = sd(sum_Energykcal, na.rm = TRUE)) %>% print()

##### Evidence of serious underreporting of intakes. Energy intakes mean = 879, sd = 303



# Determine mean protein intake (g/day) for each population sub-group 
daily_proteing$AgeR.x <- as.numeric(as.character(daily_proteing$AgeR.x))
daily_proteing$Sex.x <- as.numeric(as.character(daily_proteing$Sex.x))
daily_proteing$sum_Proteing <- as.numeric(as.character(daily_proteing$sum_Proteing))


######!!!!!!!!!!!!!!! 
#ALL NEED TO BE CHANGED TO .X (SEX AND AGER)
######!!!!!!!!!!!!!!!
# Adult male = 76.1kg
daily_proteing %>% ungroup() %>% 
  filter(Sex.x==1 & AgeR.x >=19 & AgeR.x <= 64 & sum_Energykcal >= 800) %>% 
  summarise(mean_daily_proteing = mean(sum_Proteing, na.rm = TRUE)) %>% 
  print()
# Adult female = 56.5kg
daily_proteing %>% ungroup() %>% 
  filter(Sex==2 & AgeR >=19 & AgeR <= 64 & sum_Energykcal >= 500) %>% 
  summarise(mean_daily_proteing = mean(sum_Proteing, na.rm = TRUE)) %>% 
  print()

#Elderly male = 66.3kg
daily_proteing %>% 
  filter(Sex==1 & AgeR >=65 & sum_Energykcal >= 800) %>% ungroup() %>% 
  summarise(mean_daily_proteing = mean(sum_Proteing, na.rm = TRUE)) %>% 
  print()


#Elderly female = 48.8kg
daily_proteing %>% 
  filter(Sex==2 & AgeR >=65 & sum_Energykcal >= 500) %>% ungroup() %>% 
  summarise(mean_daily_proteing = mean(sum_Proteing, na.rm = TRUE)) %>% 
  print()



############################################################################################################
#Determine average weights for adult men and women, elderly men and women, infants, children and adolescents
# Filter to remove -1.0? N/A scores 
#male adults
demographics %>% filter(AgeR > 18 & AgeR < 65 & Sex==1) %>% 
  summarise(avg_weight = mean(Weight, na.rm = TRUE)) %>% print()
#== 81.3 = ~80

#female adults
demographics %>% filter(AgeR > 18 & AgeR < 65 & Sex==2) %>% 
  summarise(avg_weight = mean(Weight, na.rm = TRUE)) %>% print()
#== 68.9 = ~70

#elderly male 
demographics %>% filter(AgeR > 64 & Sex==1) %>% 
  summarise(avg_weight = mean(Weight, na.rm = TRUE)) %>% print()
#== 75.7 = ~75

#elderly female 
demographics %>% filter(AgeR > 64 & Sex==2) %>% 
  summarise(avg_weight = mean(Weight, na.rm = TRUE)) %>% print()
#== 59.4 = ~60

#infants == 12.7
demographics %>% filter(AgeR < 4) %>% 
  summarise(avg_weight = mean(Weight, na.rm = TRUE)) %>% print()

#children == 25.3
demographics %>% filter(AgeR > 3 & AgeR < 11) %>% 
  summarise(avg_weight = mean(Weight, na.rm = TRUE)) %>% print()

#adolescent male = 56
demographics %>% filter(Sex==1 & AgeR > 10 & AgeR < 19) %>% 
  summarise(avg_weight = mean(Weight, na.rm = TRUE)) %>% print()

#adolescent female = 54.6
demographics %>% filter(Sex==2 & AgeR > 10 & AgeR < 19) %>% 
  summarise(avg_weight = mean(Weight, na.rm = TRUE)) %>% print()
#############################################################################################################
# 17)
### Mean daily intakes of protein and IAAs for adults 

## Whole population: 
whole_pop <- daily_data %>% 
  group_by(seriali, DayNo, AgeR.x, Sex.x, Overall_Diet) %>% 
  summarise(sum_energy = sum(Energykcal, na.rm = TRUE),
            sum_Proteing = sum(Proteing, na.rm = TRUE),
            sum_dig_protein = sum(Digest_total_protein, na.rm = TRUE),
            sum_TRPg = sum(TRPg, na.rm = TRUE),
            sum_dig_TRP = sum(Digest_TRP, na.rm = TRUE),
            sum_TRPg = sum(TRPg, na.rm = TRUE),
            sum_dig_TRP = sum(Digest_TRP, na.rm = TRUE),
            sum_THRg = sum(THRg, na.rm = TRUE),
            sum_dig_THR = sum(Digest_THR, na.rm = TRUE),
            sum_ILEg = sum(ILEg, na.rm = TRUE),
            sum_dig_ILE = sum(Digest_ILE, na.rm = TRUE),
            sum_LEUg = sum(LEUg, na.rm = TRUE),
            sum_dig_LEU = sum(Digest_LEU, na.rm = TRUE),
            sum_LYSg = sum(LYSg, na.rm = TRUE),
            sum_dig_LYS = sum(Digest_LYS, na.rm = TRUE),
            sum_METg = sum(METg, na.rm = TRUE),
            sum_dig_MET = sum(Digest_MET, na.rm = TRUE),
            sum_CYSg = sum(CYSg, na.rm = TRUE),
            sum_dig_CYS = sum(Digest_CYS, na.rm = TRUE),
            sum_PHEg = sum(PHEg, na.rm = TRUE),
            sum_dig_PHE = sum(Digest_PHE, na.rm = TRUE),
            sum_TYRg = sum(TYRg, na.rm = TRUE),
            sum_dig_TYR = sum(Digest_TYR, na.rm = TRUE),
            sum_VALg = sum(VALg, na.rm = TRUE),
            sum_dig_VAL = sum(Digest_VAL, na.rm = TRUE),
            sum_ARGg = sum(ARGg, na.rm = TRUE),
            sum_dig_ARG = sum(Digest_ARG, na.rm = TRUE),
            sum_HISTg = sum(HISTNg, na.rm = TRUE),
            sum_dig_HIST = sum(Digest_HIST, na.rm = TRUE)) %>% ungroup()

whole_pop %>% 
  filter(Sex.x==1) %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_protein = mean(sum_dig_protein, na.rm = TRUE)) %>% 
  print()

whole_pop$AgeR.x <- as.numeric(as.character(whole_pop$AgeR.x))
whole_pop$Sex.x <- as.numeric(as.character(whole_pop$Sex.x))
whole_pop$sum_Proteing <- as.numeric(as.character(whole_pop$sum_Proteing))

whole_pop2 <- Total_AA_dietary_data %>% 
  group_by(seriali, Sex.x, DayNo, Overall_Diet) %>% 
  summarise(sum_energy = sum(Energykcal, na.rm = TRUE),
            sum_pro = sum(Proteing, na.rm=TRUE),
            sum_dig_pro = sum(Digest_total_protein, na.rm = TRUE),
            sum_TRPg = sum(TRPg, na.rm = TRUE),
            sum_dig_TRP = sum(Digest_TRP, na.rm = TRUE),
            sum_TRPg = sum(TRPg, na.rm = TRUE),
            sum_dig_TRP = sum(Digest_TRP, na.rm = TRUE),
            sum_THRg = sum(THRg, na.rm = TRUE),
            sum_dig_THR = sum(Digest_THR, na.rm = TRUE),
            sum_ILEg = sum(ILEg, na.rm = TRUE),
            sum_dig_ILE = sum(Digest_ILE, na.rm = TRUE),
            sum_LEUg = sum(LEUg, na.rm = TRUE),
            sum_dig_LEU = sum(Digest_LEU, na.rm = TRUE),
            sum_LYSg = sum(LYSg, na.rm = TRUE),
            sum_dig_LYS = sum(Digest_LYS, na.rm = TRUE),
            sum_METg = sum(METg, na.rm = TRUE),
            sum_dig_MET = sum(Digest_MET, na.rm = TRUE),
            sum_CYSg = sum(CYSg, na.rm = TRUE),
            sum_dig_CYS = sum(Digest_CYS, na.rm = TRUE),
            sum_PHEg = sum(PHEg, na.rm = TRUE),
            sum_dig_PHE = sum(Digest_PHE, na.rm = TRUE),
            sum_TYRg = sum(TYRg, na.rm = TRUE),
            sum_dig_TYR = sum(Digest_TYR, na.rm = TRUE),
            sum_VALg = sum(VALg, na.rm = TRUE),
            sum_dig_VAL = sum(Digest_VAL, na.rm = TRUE),
            sum_ARGg = sum(ARGg, na.rm = TRUE),
            sum_dig_ARG = sum(Digest_ARG, na.rm = TRUE),
            sum_HISTg = sum(HISTNg, na.rm = TRUE),
            sum_dig_HIST = sum(Digest_HIST, na.rm = TRUE))


whole_pop2$Sex.x <- as.numeric(as.character(whole_pop2$Sex.x))
whole_pop2$sum_pro <- as.numeric(as.character(whole_pop2$sum_pro))
whole_pop2$sum_dig_pro <- as.numeric(as.character(whole_pop2$sum_dig_pro))

whole_pop2 %>% 
  filter(Sex.x==1) %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_pro = mean(sum_pro, na.rm=TRUE),
            mean_dig_pro = mean(sum_dig_pro, na.rm=TRUE)) %>% 
  print()

whole_pop2 %>% 
  filter(Sex.x==1) %>% 
  group_by(Overall_Diet) %>% 
  summarise(
    mean_TRPg = mean(sum_TRPg, na.rm = TRUE),
    mean_dig_TRP = mean(sum_dig_TRP, na.rm = TRUE),
    mean_THRg = mean(sum_THRg, na.rm = TRUE),
    mean_dig_THR = mean(sum_dig_THR, na.rm = TRUE),
    mean_ILEg = mean(sum_ILEg, na.rm = TRUE),
    mean_dig_ILE = mean(sum_dig_ILE, na.rm = TRUE),
    mean_LEUg = mean(sum_LEUg, na.rm = TRUE),
    mean_dig_LEU = mean(sum_dig_LEU, na.rm = TRUE),
    mean_LYSg = mean(sum_LYSg, na.rm = TRUE),
    mean_dig_LYS = mean(sum_dig_LYS, na.rm = TRUE),
    mean_METg = mean(sum_METg, na.rm = TRUE),
    mean_dig_MET = mean(sum_dig_MET, na.rm = TRUE),
    mean_CYSg = mean(sum_CYSg, na.rm = TRUE),
    mean_dig_CYS = mean(sum_dig_CYS, na.rm = TRUE),
    mean_PHEg = mean(sum_PHEg, na.rm = TRUE),
    mean_dig_PHE = mean(sum_dig_PHE, na.rm = TRUE),
    mean_TYRg = mean(sum_TYRg, na.rm = TRUE),
    mean_dig_TYR = mean(sum_dig_TYR, na.rm = TRUE),
    mean_VALg = mean(sum_VALg, na.rm = TRUE),
    mean_dig_VAL = mean(sum_dig_VAL, na.rm = TRUE),
    mean_ARGg = mean(sum_ARGg, na.rm = TRUE),
    mean_dig_ARG = mean(sum_dig_ARG, na.rm = TRUE),
    mean_HISTg = mean(sum_HISTg, na.rm = TRUE),
    mean_dig_HIST = mean(sum_dig_HIST, na.rm = TRUE)) %>% View()


#Change exclusions to those who consume inadequate total energy??
#Men: Intake below 800 kcal/day or above 4000 kcal/day
#Women: Intake below 500 kcal/day or above 3500 kcal/day

# Create new data set for adult males and females, excluding low energy intakes 

exclude_male_adult <- daily_data %>%
  group_by(seriali, DayNo, AgeR.x, Sex.x, Overall_Diet) %>% 
  summarise(sum_energy = sum(Energykcal, na.rm = TRUE),
            sum_Proteing = sum(Proteing, na.rm = TRUE),
            sum_dig_protein = sum(Digest_total_protein, na.rm = TRUE),
            sum_TRPg = sum(TRPg, na.rm = TRUE),
            sum_dig_TRP = sum(Digest_TRP, na.rm = TRUE),
            sum_TRPg = sum(TRPg, na.rm = TRUE),
            sum_dig_TRP = sum(Digest_TRP, na.rm = TRUE),
            sum_THRg = sum(THRg, na.rm = TRUE),
            sum_dig_THR = sum(Digest_THR, na.rm = TRUE),
            sum_ILEg = sum(ILEg, na.rm = TRUE),
            sum_dig_ILE = sum(Digest_ILE, na.rm = TRUE),
            sum_LEUg = sum(LEUg, na.rm = TRUE),
            sum_dig_LEU = sum(Digest_LEU, na.rm = TRUE),
            sum_LYSg = sum(LYSg, na.rm = TRUE),
            sum_dig_LYS = sum(Digest_LYS, na.rm = TRUE),
            sum_METg = sum(METg, na.rm = TRUE),
            sum_dig_MET = sum(Digest_MET, na.rm = TRUE),
            sum_CYSg = sum(CYSg, na.rm = TRUE),
            sum_dig_CYS = sum(Digest_CYS, na.rm = TRUE),
            sum_PHEg = sum(PHEg, na.rm = TRUE),
            sum_dig_PHE = sum(Digest_PHE, na.rm = TRUE),
            sum_TYRg = sum(TYRg, na.rm = TRUE),
            sum_dig_TYR = sum(Digest_TYR, na.rm = TRUE),
            sum_VALg = sum(VALg, na.rm = TRUE),
            sum_dig_VAL = sum(Digest_VAL, na.rm = TRUE),
            sum_ARGg = sum(ARGg, na.rm = TRUE),
            sum_dig_ARG = sum(Digest_ARG, na.rm = TRUE),
            sum_HISTg = sum(HISTNg, na.rm = TRUE),
            sum_dig_HIST = sum(Digest_HIST, na.rm = TRUE),
  ) %>% ungroup() %>% 
  filter(Sex.x==1 & AgeR.x > 18 & AgeR.x < 65 & sum_energy >=800)

exclude_female_adult <- daily_data %>% 
  group_by(seriali, DayNo, AgeR.x, Sex.x, Overall_Diet) %>% 
  summarise(sum_energy = sum(Energykcal, na.rm = TRUE),
            sum_Proteing = sum(Proteing, na.rm = TRUE),
            sum_dig_protein = sum(Digest_total_protein, na.rm = TRUE),
            sum_TRPg = sum(TRPg, na.rm = TRUE),
            sum_dig_TRP = sum(Digest_TRP, na.rm = TRUE),
            sum_TRPg = sum(TRPg, na.rm = TRUE),
            sum_dig_TRP = sum(Digest_TRP, na.rm = TRUE),
            sum_THRg = sum(THRg, na.rm = TRUE),
            sum_dig_THR = sum(Digest_THR, na.rm = TRUE),
            sum_ILEg = sum(ILEg, na.rm = TRUE),
            sum_dig_ILE = sum(Digest_ILE, na.rm = TRUE),
            sum_LEUg = sum(LEUg, na.rm = TRUE),
            sum_dig_LEU = sum(Digest_LEU, na.rm = TRUE),
            sum_LYSg = sum(LYSg, na.rm = TRUE),
            sum_dig_LYS = sum(Digest_LYS, na.rm = TRUE),
            sum_METg = sum(METg, na.rm = TRUE),
            sum_dig_MET = sum(Digest_MET, na.rm = TRUE),
            sum_CYSg = sum(CYSg, na.rm = TRUE),
            sum_dig_CYS = sum(Digest_CYS, na.rm = TRUE),
            sum_PHEg = sum(PHEg, na.rm = TRUE),
            sum_dig_PHE = sum(Digest_PHE, na.rm = TRUE),
            sum_TYRg = sum(TYRg, na.rm = TRUE),
            sum_dig_TYR = sum(Digest_TYR, na.rm = TRUE),
            sum_VALg = sum(VALg, na.rm = TRUE),
            sum_dig_VAL = sum(Digest_VAL, na.rm = TRUE),
            sum_ARGg = sum(ARGg, na.rm = TRUE),
            sum_dig_ARG = sum(Digest_ARG, na.rm = TRUE),
            sum_HISTg = sum(HISTNg, na.rm = TRUE),
            sum_dig_HIST = sum(Digest_HIST, na.rm = TRUE),
  ) %>% ungroup() %>%
  filter(Sex.x==2 & AgeR.x > 18 & AgeR.x < 65 & sum_energy >= 500)
## 3283 female obvs remaining compared to 1812 male 

#####################################################################################
#Male
#Female

exclude_male_adult %>% ungroup() %>% 
  summarise(mean_Proteing = mean(sum_Proteing, na.rm = TRUE)) %>% 
  print()

group_by(Overall_Diet) %>% 
  exclude_female_adult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_Proteing = mean(sum_Proteing, na.rm = TRUE)) %>% 
  print()

exclude_adult %>% filter(Overall_Diet=='Vegan' & Sex.x==2) %>% View()


summarise(sum_energy = sum(Energykcal, na.rm = TRUE),
          sum_Proteing = sum(Proteing, na.rm = TRUE),
          sum_dig_protein = sum(Digest_total_protein, na.rm = TRUE),
          sum_TRPg = sum(TRPg, na.rm = TRUE),
          sum_dig_TRP = sum(Digest_TRP, na.rm = TRUE),
          sum_TRPg = sum(TRPg, na.rm = TRUE),
          sum_dig_TRP = sum(Digest_TRP, na.rm = TRUE),
          sum_THRg = sum(THRg, na.rm = TRUE),
          sum_dig_THR = sum(Digest_THR, na.rm = TRUE),
          sum_ILEg = sum(ILEg, na.rm = TRUE),
          sum_dig_ILE = sum(Digest_ILE, na.rm = TRUE),
          sum_LEUg = sum(LEUg, na.rm = TRUE),
          sum_dig_LEU = sum(Digest_LEU, na.rm = TRUE),
          sum_LYSg = sum(LYSg, na.rm = TRUE),
          sum_dig_LYS = sum(Digest_LYS, na.rm = TRUE),
          sum_METg = sum(METg, na.rm = TRUE),
          sum_dig_MET = sum(Digest_MET, na.rm = TRUE),
          sum_CYSg = sum(CYSg, na.rm = TRUE),
          sum_dig_CYS = sum(Digest_CYS, na.rm = TRUE),
          sum_PHEg = sum(PHEg, na.rm = TRUE),
          sum_dig_PHE = sum(Digest_PHE, na.rm = TRUE),
          sum_TYRg = sum(TYRg, na.rm = TRUE),
          sum_dig_TYR = sum(Digest_TYR, na.rm = TRUE),
          sum_VALg = sum(VALg, na.rm = TRUE),
          sum_dig_VAL = sum(Digest_VAL, na.rm = TRUE),
          sum_ARGg = sum(ARGg, na.rm = TRUE),
          sum_dig_ARG = sum(Digest_ARG, na.rm = TRUE),
          sum_HISTg = sum(HISTNg, na.rm = TRUE),
          sum_dig_HIST = sum(Digest_HIST, na.rm = TRUE),)


exclude_female_adult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_protein = mean(sum_dig_protein, na.rm = TRUE)) %>% 
  print()

#trp
exclude_male_adult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_TRP = mean(sum_dig_TRP, na.rm = TRUE)) %>% 
  print()
exclude_female_adult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_TRP = mean(sum_dig_TRP, na.rm = TRUE)) %>% 
  print()


#thr
exclude_male_adult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_THR = mean(sum_dig_THR, na.rm = TRUE)) %>% 
  print()
exclude_female_adult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_THR = mean(sum_dig_THR, na.rm = TRUE)) %>% 
  print()

#ile
exclude_male_adult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_ILE = mean(sum_dig_ILE, na.rm = TRUE)) %>% 
  print()
exclude_female_adult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_ILE = mean(sum_dig_ILE, na.rm = TRUE)) %>% 
  print()

#leu
exclude_male_adult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_LEU = mean(sum_dig_LEU, na.rm = TRUE)) %>% 
  print()
exclude_female_adult %>% ungroup() %>%
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_LEU = mean(sum_dig_LEU, na.rm = TRUE)) %>% 
  print()

#LYS
exclude_male_adult %>% ungroup() %>%
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_LYS = mean(sum_dig_LYS, na.rm = TRUE)) %>% 
  print()
exclude_female_adult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_LYS = mean(sum_dig_LYS, na.rm = TRUE)) %>% 
  print()

#MET
exclude_male_adult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_MET = mean(sum_dig_MET, na.rm = TRUE)) %>% 
  print()
exclude_female_adult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_MET = mean(sum_dig_MET, na.rm = TRUE)) %>% 
  print()

#cys
exclude_male_adult %>% ungroup() %>%
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_CYS = mean(sum_dig_CYS, na.rm = TRUE)) %>% 
  print()
exclude_female_adult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_CYS = mean(sum_dig_CYS, na.rm = TRUE)) %>% 
  print()

#PHE
exclude_male_adult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_PHE = mean(sum_dig_PHE, na.rm = TRUE)) %>% 
  print()
exclude_female_adult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_PHE = mean(sum_dig_PHE, na.rm = TRUE)) %>% 
  print()

#TYR
exclude_male_adult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_TYR = mean(sum_dig_TYR, na.rm = TRUE)) %>% 
  print()
exclude_female_adult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_TYR = mean(sum_dig_TYR, na.rm = TRUE)) %>% 
  print()

#VAL
exclude_male_adult %>% ungroup() %>%
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_VAL = mean(sum_dig_VAL, na.rm = TRUE)) %>% 
  print()
exclude_female_adult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_VAL = mean(sum_dig_VAL, na.rm = TRUE)) %>% 
  print()


#ARG
exclude_male_adult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_ARG = mean(sum_dig_ARG, na.rm = TRUE)) %>% 
  print()
exclude_female_adult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_ARG = mean(sum_dig_ARG, na.rm = TRUE)) %>% 
  print()

#HIST
exclude_male_adult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_HIST = mean(sum_dig_HIST, na.rm = TRUE)) %>% 
  print()
exclude_female_adult%>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_HIST = mean(sum_dig_HIST, na.rm = TRUE)) %>% 
  print()

####################
# 18)
### Mean daily intakes of protein and IAAs for older adults 

exclude_female_olderadult <- daily_data %>% 
  group_by(seriali, DayNo, AgeR, Sex, Overall_Diet) %>% 
  summarise(sum_energy = sum(Energykcal, na.rm = TRUE),
            sum_Proteing = sum(Proteing, na.rm = TRUE),
            sum_dig_protein = sum(Digest_total_protein, na.rm = TRUE),
            sum_TRPg = sum(TRPg, na.rm = TRUE),
            sum_dig_TRP = sum(Digest_TRP, na.rm = TRUE),
            sum_TRPg = sum(TRPg, na.rm = TRUE),
            sum_dig_TRP = sum(Digest_TRP, na.rm = TRUE),
            sum_THRg = sum(THRg, na.rm = TRUE),
            sum_dig_THR = sum(Digest_THR, na.rm = TRUE),
            sum_ILEg = sum(ILEg, na.rm = TRUE),
            sum_dig_ILE = sum(Digest_ILE, na.rm = TRUE),
            sum_LEUg = sum(LEUg, na.rm = TRUE),
            sum_dig_LEU = sum(Digest_LEU, na.rm = TRUE),
            sum_LYSg = sum(LYSg, na.rm = TRUE),
            sum_dig_LYS = sum(Digest_LYS, na.rm = TRUE),
            sum_METg = sum(METg, na.rm = TRUE),
            sum_dig_MET = sum(Digest_MET, na.rm = TRUE),
            sum_CYSg = sum(CYSg, na.rm = TRUE),
            sum_dig_CYS = sum(Digest_CYS, na.rm = TRUE),
            sum_PHEg = sum(PHEg, na.rm = TRUE),
            sum_dig_PHE = sum(Digest_PHE, na.rm = TRUE),
            sum_TYRg = sum(TYRg, na.rm = TRUE),
            sum_dig_TYR = sum(Digest_TYR, na.rm = TRUE),
            sum_VALg = sum(VALg, na.rm = TRUE),
            sum_dig_VAL = sum(Digest_VAL, na.rm = TRUE),
            sum_ARGg = sum(ARGg, na.rm = TRUE),
            sum_dig_ARG = sum(Digest_ARG, na.rm = TRUE),
            sum_HISTg = sum(HISTNg, na.rm = TRUE),
            sum_dig_HIST = sum(Digest_HIST, na.rm = TRUE),
  ) %>% ungroup() %>%
  filter(Sex==2 & AgeR > 64 & sum_energy > 500 & sum_energy < 3500)

exclude_male_olderadult <- daily_data %>% 
  group_by(seriali, DayNo, AgeR, Sex, Overall_Diet) %>% 
  summarise(sum_energy = sum(Energykcal, na.rm = TRUE),
            sum_Proteing = sum(Proteing, na.rm = TRUE),
            sum_dig_protein = sum(Digest_total_protein, na.rm = TRUE),
            sum_TRPg = sum(TRPg, na.rm = TRUE),
            sum_dig_TRP = sum(Digest_TRP, na.rm = TRUE),
            sum_TRPg = sum(TRPg, na.rm = TRUE),
            sum_dig_TRP = sum(Digest_TRP, na.rm = TRUE),
            sum_THRg = sum(THRg, na.rm = TRUE),
            sum_dig_THR = sum(Digest_THR, na.rm = TRUE),
            sum_ILEg = sum(ILEg, na.rm = TRUE),
            sum_dig_ILE = sum(Digest_ILE, na.rm = TRUE),
            sum_LEUg = sum(LEUg, na.rm = TRUE),
            sum_dig_LEU = sum(Digest_LEU, na.rm = TRUE),
            sum_LYSg = sum(LYSg, na.rm = TRUE),
            sum_dig_LYS = sum(Digest_LYS, na.rm = TRUE),
            sum_METg = sum(METg, na.rm = TRUE),
            sum_dig_MET = sum(Digest_MET, na.rm = TRUE),
            sum_CYSg = sum(CYSg, na.rm = TRUE),
            sum_dig_CYS = sum(Digest_CYS, na.rm = TRUE),
            sum_PHEg = sum(PHEg, na.rm = TRUE),
            sum_dig_PHE = sum(Digest_PHE, na.rm = TRUE),
            sum_TYRg = sum(TYRg, na.rm = TRUE),
            sum_dig_TYR = sum(Digest_TYR, na.rm = TRUE),
            sum_VALg = sum(VALg, na.rm = TRUE),
            sum_dig_VAL = sum(Digest_VAL, na.rm = TRUE),
            sum_ARGg = sum(ARGg, na.rm = TRUE),
            sum_dig_ARG = sum(Digest_ARG, na.rm = TRUE),
            sum_HISTg = sum(HISTNg, na.rm = TRUE),
            sum_dig_HIST = sum(Digest_HIST, na.rm = TRUE),
  ) %>% ungroup() %>%
  filter(Sex==1 & AgeR > 64 & sum_energy > 800 & sum_energy < 4000)

#Male
#Female

exclude_male_olderadult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_Proteing = mean(sum_Proteing, na.rm = TRUE)) %>% 
  print()
exclude_female_olderadult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_Proteing = mean(sum_Proteing, na.rm = TRUE)) %>% 
  print()

exclude_male_olderadult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_protein = mean(sum_dig_protein, na.rm = TRUE)) %>% 
  print()
exclude_female_olderadult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_protein = mean(sum_dig_protein, na.rm = TRUE)) %>% 
  print()

#trp
exclude_male_olderadult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_TRP = mean(sum_dig_TRP, na.rm = TRUE)) %>% 
  print()
exclude_female_olderadult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_TRP = mean(sum_dig_TRP, na.rm = TRUE)) %>% 
  print()


#thr
exclude_male_olderadult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_THR = mean(sum_dig_THR, na.rm = TRUE)) %>% 
  print()
exclude_female_olderadult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_THR = mean(sum_dig_THR, na.rm = TRUE)) %>% 
  print()

#ile
exclude_male_olderadult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_ILE = mean(sum_dig_ILE, na.rm = TRUE)) %>% 
  print()
exclude_female_olderadult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_ILE = mean(sum_dig_ILE, na.rm = TRUE)) %>% 
  print()

#leu
exclude_male_olderadult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_LEU = mean(sum_dig_LEU, na.rm = TRUE)) %>% 
  print()
exclude_female_olderadult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_LEU = mean(sum_dig_LEU, na.rm = TRUE)) %>% 
  print()

#LYS
exclude_male_olderadult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_LYS = mean(sum_dig_LYS, na.rm = TRUE)) %>% 
  print()
exclude_female_olderadult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_LYS = mean(sum_dig_LYS, na.rm = TRUE)) %>% 
  print()

#MET
exclude_male_olderadult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_MET = mean(sum_dig_MET, na.rm = TRUE)) %>% 
  print()
exclude_female_olderadult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_MET = mean(sum_dig_MET, na.rm = TRUE)) %>% 
  print()

#cys
exclude_male_olderadult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_CYS = mean(sum_dig_CYS, na.rm = TRUE)) %>% 
  print()
exclude_female_olderadult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_CYS = mean(sum_dig_CYS, na.rm = TRUE)) %>% 
  print()

#PHE
exclude_male_olderadult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_PHE = mean(sum_dig_PHE, na.rm = TRUE)) %>% 
  print()
exclude_female_olderadult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_PHE = mean(sum_dig_PHE, na.rm = TRUE)) %>% 
  print()

#TYR
exclude_male_olderadult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_TYR = mean(sum_dig_TYR, na.rm = TRUE)) %>% 
  print()
exclude_female_olderadult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_TYR = mean(sum_dig_TYR, na.rm = TRUE)) %>% 
  print()

#VAL
exclude_male_olderadult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_VAL = mean(sum_dig_VAL, na.rm = TRUE)) %>% 
  print()
exclude_female_olderadult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_VAL = mean(sum_dig_VAL, na.rm = TRUE)) %>% 
  print()


#ARG
exclude_male_olderadult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_ARG = mean(sum_dig_ARG, na.rm = TRUE)) %>% 
  print()
exclude_female_olderadult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_ARG = mean(sum_dig_ARG, na.rm = TRUE)) %>% 
  print()

#HIST
exclude_male_olderadult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_HIST = mean(sum_dig_HIST, na.rm = TRUE)) %>% 
  print()
exclude_female_olderadult %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_HIST = mean(sum_dig_HIST, na.rm = TRUE)) %>% 
  print()

###################################################################################
# 19)
#Infants, Children and adolescents

#Infants
#Age < 4 and no energy restrictions/exclusions
exclude_infants <- daily_data %>% 
  group_by(seriali, DayNo, AgeR.x, Sex.x, Overall_Diet) %>% 
  summarise(sum_energy = sum(Energykcal, na.rm = TRUE),
            sum_Proteing = sum(Proteing, na.rm = TRUE),
            sum_dig_protein = sum(Digest_total_protein, na.rm = TRUE),
            sum_TRPg = sum(TRPg, na.rm = TRUE),
            sum_dig_TRP = sum(Digest_TRP, na.rm = TRUE),
            sum_TRPg = sum(TRPg, na.rm = TRUE),
            sum_dig_TRP = sum(Digest_TRP, na.rm = TRUE),
            sum_THRg = sum(THRg, na.rm = TRUE),
            sum_dig_THR = sum(Digest_THR, na.rm = TRUE),
            sum_ILEg = sum(ILEg, na.rm = TRUE),
            sum_dig_ILE = sum(Digest_ILE, na.rm = TRUE),
            sum_LEUg = sum(LEUg, na.rm = TRUE),
            sum_dig_LEU = sum(Digest_LEU, na.rm = TRUE),
            sum_LYSg = sum(LYSg, na.rm = TRUE),
            sum_dig_LYS = sum(Digest_LYS, na.rm = TRUE),
            sum_METg = sum(METg, na.rm = TRUE),
            sum_dig_MET = sum(Digest_MET, na.rm = TRUE),
            sum_CYSg = sum(CYSg, na.rm = TRUE),
            sum_dig_CYS = sum(Digest_CYS, na.rm = TRUE),
            sum_PHEg = sum(PHEg, na.rm = TRUE),
            sum_dig_PHE = sum(Digest_PHE, na.rm = TRUE),
            sum_TYRg = sum(TYRg, na.rm = TRUE),
            sum_dig_TYR = sum(Digest_TYR, na.rm = TRUE),
            sum_VALg = sum(VALg, na.rm = TRUE),
            sum_dig_VAL = sum(Digest_VAL, na.rm = TRUE),
            sum_ARGg = sum(ARGg, na.rm = TRUE),
            sum_dig_ARG = sum(Digest_ARG, na.rm = TRUE),
            sum_HISTg = sum(HISTNg, na.rm = TRUE),
            sum_dig_HIST = sum(Digest_HIST, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(AgeR.x < 4)

#Children
exclude_children <- daily_data %>% 
  group_by(seriali, DayNo, AgeR.x, Sex.x, Overall_Diet) %>% 
  summarise(sum_energy = sum(Energykcal, na.rm = TRUE),
            sum_Proteing = sum(Proteing, na.rm = TRUE),
            sum_dig_protein = sum(Digest_total_protein, na.rm = TRUE),
            sum_TRPg = sum(TRPg, na.rm = TRUE),
            sum_dig_TRP = sum(Digest_TRP, na.rm = TRUE),
            sum_THRg = sum(THRg, na.rm = TRUE),
            sum_dig_THR = sum(Digest_THR, na.rm = TRUE),
            sum_ILEg = sum(ILEg, na.rm = TRUE),
            sum_dig_ILE = sum(Digest_ILE, na.rm = TRUE),
            sum_LEUg = sum(LEUg, na.rm = TRUE),
            sum_dig_LEU = sum(Digest_LEU, na.rm = TRUE),
            sum_LYSg = sum(LYSg, na.rm = TRUE),
            sum_dig_LYS = sum(Digest_LYS, na.rm = TRUE),
            sum_METg = sum(METg, na.rm = TRUE),
            sum_dig_MET = sum(Digest_MET, na.rm = TRUE),
            sum_CYSg = sum(CYSg, na.rm = TRUE),
            sum_dig_CYS = sum(Digest_CYS, na.rm = TRUE),
            sum_PHEg = sum(PHEg, na.rm = TRUE),
            sum_dig_PHE = sum(Digest_PHE, na.rm = TRUE),
            sum_TYRg = sum(TYRg, na.rm = TRUE),
            sum_dig_TYR = sum(Digest_TYR, na.rm = TRUE),
            sum_VALg = sum(VALg, na.rm = TRUE),
            sum_dig_VAL = sum(Digest_VAL, na.rm = TRUE),
            sum_ARGg = sum(ARGg, na.rm = TRUE),
            sum_dig_ARG = sum(Digest_ARG, na.rm = TRUE),
            sum_HISTg = sum(HISTNg, na.rm = TRUE),
            sum_dig_HIST = sum(Digest_HIST, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(sum_energy > 250 & sum_energy < 2500 & AgeR.x >=4 & AgeR.x <= 10)

unique(daily_data$AgeR.x)
sum(is.na(daily_data$AgeR.x))
daily_data$AgeR.x <- as.numeric(daily_data$AgeR.x)

#Adolescents
exclude_adol_male <- daily_data %>% 
  group_by(seriali, DayNo, AgeR.x, Sex.x, Overall_Diet) %>% 
  summarise(sum_energy = sum(Energykcal, na.rm = TRUE),
            sum_Proteing = sum(Proteing, na.rm = TRUE),
            sum_dig_protein = sum(Digest_total_protein, na.rm = TRUE),
            sum_TRPg = sum(TRPg, na.rm = TRUE),
            sum_dig_TRP = sum(Digest_TRP, na.rm = TRUE),
            sum_TRPg = sum(TRPg, na.rm = TRUE),
            sum_dig_TRP = sum(Digest_TRP, na.rm = TRUE),
            sum_THRg = sum(THRg, na.rm = TRUE),
            sum_dig_THR = sum(Digest_THR, na.rm = TRUE),
            sum_ILEg = sum(ILEg, na.rm = TRUE),
            sum_dig_ILE = sum(Digest_ILE, na.rm = TRUE),
            sum_LEUg = sum(LEUg, na.rm = TRUE),
            sum_dig_LEU = sum(Digest_LEU, na.rm = TRUE),
            sum_LYSg = sum(LYSg, na.rm = TRUE),
            sum_dig_LYS = sum(Digest_LYS, na.rm = TRUE),
            sum_METg = sum(METg, na.rm = TRUE),
            sum_dig_MET = sum(Digest_MET, na.rm = TRUE),
            sum_CYSg = sum(CYSg, na.rm = TRUE),
            sum_dig_CYS = sum(Digest_CYS, na.rm = TRUE),
            sum_PHEg = sum(PHEg, na.rm = TRUE),
            sum_dig_PHE = sum(Digest_PHE, na.rm = TRUE),
            sum_TYRg = sum(TYRg, na.rm = TRUE),
            sum_dig_TYR = sum(Digest_TYR, na.rm = TRUE),
            sum_VALg = sum(VALg, na.rm = TRUE),
            sum_dig_VAL = sum(Digest_VAL, na.rm = TRUE),
            sum_ARGg = sum(ARGg, na.rm = TRUE),
            sum_dig_ARG = sum(Digest_ARG, na.rm = TRUE),
            sum_HISTg = sum(HISTNg, na.rm = TRUE),
            sum_dig_HIST = sum(Digest_HIST, na.rm = TRUE),
  ) %>% ungroup() %>% 
  filter(Sex.x==1 & AgeR.x > 10 & AgeR.x < 19 & sum_energy >= 500 & sum_energy <=3500)

exclude_adol_female <- daily_data %>% 
  group_by(seriali, DayNo, AgeR.x, Sex.x, Overall_Diet) %>% 
  summarise(sum_energy = sum(Energykcal, na.rm = TRUE),
            sum_Proteing = sum(Proteing, na.rm = TRUE),
            sum_dig_protein = sum(Digest_total_protein, na.rm = TRUE),
            sum_TRPg = sum(TRPg, na.rm = TRUE),
            sum_dig_TRP = sum(Digest_TRP, na.rm = TRUE),
            sum_TRPg = sum(TRPg, na.rm = TRUE),
            sum_dig_TRP = sum(Digest_TRP, na.rm = TRUE),
            sum_THRg = sum(THRg, na.rm = TRUE),
            sum_dig_THR = sum(Digest_THR, na.rm = TRUE),
            sum_ILEg = sum(ILEg, na.rm = TRUE),
            sum_dig_ILE = sum(Digest_ILE, na.rm = TRUE),
            sum_LEUg = sum(LEUg, na.rm = TRUE),
            sum_dig_LEU = sum(Digest_LEU, na.rm = TRUE),
            sum_LYSg = sum(LYSg, na.rm = TRUE),
            sum_dig_LYS = sum(Digest_LYS, na.rm = TRUE),
            sum_METg = sum(METg, na.rm = TRUE),
            sum_dig_MET = sum(Digest_MET, na.rm = TRUE),
            sum_CYSg = sum(CYSg, na.rm = TRUE),
            sum_dig_CYS = sum(Digest_CYS, na.rm = TRUE),
            sum_PHEg = sum(PHEg, na.rm = TRUE),
            sum_dig_PHE = sum(Digest_PHE, na.rm = TRUE),
            sum_TYRg = sum(TYRg, na.rm = TRUE),
            sum_dig_TYR = sum(Digest_TYR, na.rm = TRUE),
            sum_VALg = sum(VALg, na.rm = TRUE),
            sum_dig_VAL = sum(Digest_VAL, na.rm = TRUE),
            sum_ARGg = sum(ARGg, na.rm = TRUE),
            sum_dig_ARG = sum(Digest_ARG, na.rm = TRUE),
            sum_HISTg = sum(HISTNg, na.rm = TRUE),
            sum_dig_HIST = sum(Digest_HIST, na.rm = TRUE),
  ) %>% ungroup() %>% 
  filter(Sex.x==2 & AgeR.x >= 11 & AgeR.x <= 18 & sum_energy >= 500 & sum_energy <=3500)


#Protein

exclude_infants %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_Proteing = mean(sum_Proteing, na.rm = TRUE)) %>% 
  print()

exclude_infants %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_protein = mean(sum_dig_protein, na.rm = TRUE)) %>% 
  print()

exclude_children %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_Proteing = mean(sum_Proteing, na.rm = TRUE)) %>% 
  print()

exclude_children %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_protein = mean(sum_dig_protein, na.rm = TRUE)) %>% 
  print()

exclude_adol_male %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_Proteing = mean(sum_Proteing, na.rm = TRUE)) %>% 
  print()

exclude_adol_male %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_protein = mean(sum_dig_protein, na.rm = TRUE)) %>% 
  print()

exclude_adol_female %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_Proteing = mean(sum_Proteing, na.rm = TRUE)) %>% 
  print()

exclude_adol_female %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_protein = mean(sum_dig_protein, na.rm = TRUE)) %>% 
  print()

#Lysine 
exclude_infants %>% ungroup() %>% 
  group_by(Overall_Diet) %>%
  summarise(mean_dig_LYS = mean(sum_dig_LYS, na.rm = TRUE)) %>% 
  print()

exclude_children %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_LYS = mean(sum_dig_LYS, na.rm = TRUE)) %>% 
  print()

exclude_adol_male %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_LYS = mean(sum_dig_LYS, na.rm = TRUE)) %>% 
  print()

exclude_adol_female %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_LYS = mean(sum_dig_LYS, na.rm = TRUE)) %>% 
  print()

#SAA 
#Met
exclude_infants %>% ungroup() %>% 
  group_by(Overall_Diet) %>%
  summarise(mean_dig_MET = mean(sum_dig_MET, na.rm = TRUE)) %>% 
  print
exclude_children %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_MET = mean(sum_dig_MET, na.rm = TRUE)) %>% 
  print()

exclude_adol_male %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_MET = mean(sum_dig_MET, na.rm = TRUE)) %>% 
  print()

exclude_adol_female %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_MET = mean(sum_dig_MET, na.rm = TRUE)) %>% 
  print()

#Cys
exclude_infants %>% ungroup() %>% 
  group_by(Overall_Diet) %>%
  summarise(mean_dig_CYS = mean(sum_dig_CYS, na.rm = TRUE)) %>% 
  print
exclude_children %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_CYS = mean(sum_dig_CYS, na.rm = TRUE)) %>% 
  print()

exclude_adol_male %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_CYS = mean(sum_dig_CYS, na.rm = TRUE)) %>% 
  print()

exclude_adol_female %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_CYS = mean(sum_dig_CYS, na.rm = TRUE)) %>% 
  print()

#THR
exclude_infants %>% ungroup() %>% 
  group_by(Overall_Diet) %>%
  summarise(mean_dig_THR = mean(sum_dig_THR, na.rm = TRUE)) %>% 
  print

exclude_children %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_THR = mean(sum_dig_THR, na.rm = TRUE)) %>% 
  print()

exclude_adol_male %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_THR = mean(sum_dig_THR, na.rm = TRUE)) %>% 
  print()

exclude_adol_female %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_THR = mean(sum_dig_THR, na.rm = TRUE)) %>% 
  print()

#TRP
exclude_infants %>% ungroup() %>% 
  group_by(Overall_Diet) %>%
  summarise(mean_dig_TRP = mean(sum_dig_TRP, na.rm = TRUE)) %>% 
  print
exclude_children %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_TRP = mean(sum_dig_TRP, na.rm = TRUE)) %>% 
  print()

exclude_adol_male %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_TRP = mean(sum_dig_TRP, na.rm = TRUE)) %>% 
  print()

exclude_adol_female %>% ungroup() %>% 
  group_by(Overall_Diet) %>% 
  summarise(mean_dig_TRP = mean(sum_dig_TRP, na.rm = TRUE)) %>% 
  print()

