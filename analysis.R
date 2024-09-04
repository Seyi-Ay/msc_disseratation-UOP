###################
# import required library
###################
# 
library(tidyverse) # for easy data manipulation and visualization
library(openxlsx) # for reading, writing and working with excel files
library(tidyr) # for data manipulation
library(scales) # for adjusting axis of plots made with ggplot2
library(gtsummary) # for formatting regression table results 
library(car) # for checking VIF (variance inflation factor) in model variables
library(pscl) # for computing pseudo-R2 
library(caret) # for computing variable importance
library(ggcorrplot) # for correlation matrix plot
library(ResourceSelection) # for Hosmer and Lemeshow goodness of fit (GOF) test
library(MASS) # for stepwise regression model building
library(dominanceanalysis) # for ranking predictors in logit model
library(ggpubr) # for arranging and generating publication ready plots
##
##
############
# read in hotel data
############
Hotel_Reviews <- read_csv("Hotel_Reviews.csv",
                          col_types = cols(Review_Date = col_date(format = "%m/%d/%Y")))
#
# format the dataframe column names
names(Hotel_Reviews) <- tolower(names(Hotel_Reviews))
##
##
###########
# create hotel_country variable
###########
# create an index for sub-setting the hotel_address
len <- nrow(Hotel_Reviews)
index <- numeric(length = len)
address_index <- vector("list",length = len)
country <- character(length = len)
# 
for (i in seq(len)){
  address_index[[i]] <- gregexpr(" ",Hotel_Reviews$hotel_address[i])[[1]]
  # get the index of the second space in the hotel address
  index[i] <- address_index[[i]][length(address_index[[i]])-1]
  # Subset hotel country
  country[i] <- substring(Hotel_Reviews$hotel_address[i],index[i]+1,nchar(Hotel_Reviews$hotel_address[i]))
}

# add country to hotel dataframe
Hotel_Reviews <- mutate(Hotel_Reviews, hotel_country = country, hotel_country = case_match(
  country,
  "Amsterdam Netherlands" ~ "Netherland",
  "Paris France" ~  "France",
  "Barcelona Spain" ~ "Spain",
  "Milan Italy" ~ "Italy",
  "Vienna Austria" ~ "Austria",
  .default = country)
  )
##
##
############
# create review_month variable
###########
Hotel_Reviews <- mutate(Hotel_Reviews, review_month = strftime(review_date,"%B"))
##
##
############
# create review_season variable
###########
Hotel_Reviews <- mutate(Hotel_Reviews, review_season = case_match(
  review_month,
  c("December", "January", "February") ~ "Winter",
  c("March", "April", "May") ~ "Spring",
  c("June", "July", "August") ~ "Summer",
  c("September", "October", "November") ~ "Autumn")
)
##
##
############
# create trip_type variable
###########
hotel_tags <- vector("list", length = len)
for (i in seq(len)){
  hotel_tags[i] <- str_split(Hotel_Reviews$tags[i], ",")
  for (j in seq(length(hotel_tags[[i]]))){
    hotel_tags[[i]][j] <- str_replace_all(hotel_tags[[i]][j],"\\[|\\]|\\'","") %>%
      str_trim()}
}
#
trip_type <- character(length = len)
for (i in seq(len)){
  trip_val <- str_subset(hotel_tags[[i]], "Business trip|Leisure trip")
  trip_type[i] <- ifelse(length(trip_val) > 0, trip_val, NA)
}
#
# Add trip_type to hotel_review dataset
Hotel_Reviews <- mutate(Hotel_Reviews, trip_type = trip_type)
##
##
# ##################
# # DO NOT TOUCH
# #################
# tag_string <- character()
# for (i in seq(len)){
#   for (j in seq(length(hotel_tags[[i]]))){
#     tag_string <- append(tag_string,hotel_tags[[i]][j])
#   }
# }
# ##################
# # DO NOT TOUCH
# #################
##
############
# create solo_group_traveler variable
###########
solo_group_traveler <- character(length = len)
for (i in seq(len)){
  solo_group_traveler[i] <- str_subset(hotel_tags[[i]],"Couple|Solo traveler|
  |Group|Family with young children|Family with older children|Travelers with friends")
}
#
# Add solo_group_traveler to hotel_review dataset
Hotel_Reviews <- mutate(Hotel_Reviews, traveler_type = case_match(
  solo_group_traveler,
  c("Group","Travelers with friends") ~ "Group",
  c("Family with young children","Family with older children") ~ "Traveler with Family",
  .default = solo_group_traveler)
)
##
##
############
# create length_of_stay variable
###########
stay <- vector(length = 31)
for(i in seq(31)){
  stay[i] <- paste("Stayed",i,"nights")
}
stay[1] <- "Stayed 1 night"
##
length_of_stay <- character(length = len)
for (i in seq(len)){
  stay_val <- str_subset(hotel_tags[[i]],paste(stay, collapse = "|"))
  length_of_stay[i] <- ifelse(length(stay_val) > 0, stay_val,NA)
}
length_of_stay_recode <- str_replace_all(length_of_stay,"[:alpha:]|[:blank:]","") %>%
  as.numeric()
##
# Add length_of_stay variable to hotel_review dataset
Hotel_Reviews <- mutate(Hotel_Reviews, length_of_stay = length_of_stay_recode)
##
##
# ##################
# # Code Section to extract negative and positive review values
# #################
# a_1<-table(tolower(Hotel_Reviews$negative_review))
# b_1<-as.data.frame(a_1) %>% filter(Freq>1)
# View(b_1)
# write.xlsx(b_1, 'negative_review.xlsx')
# 
# 
# a_2<-table(tolower(Hotel_Reviews$positive_review))
# b_2<-as.data.frame(a_2) %>% filter(Freq>1)
# View(b_2)
# write.xlsx(b_2, 'positive_review.xlsx')
##
##
############
# create negative_review_given variable
###########
#create variable to hold non negative values in negative review
negative_val <- read.xlsx("code_4_recode_negative.xlsx")
# Add negative_review_given variable to the hotel_review dataset
Hotel_Reviews <- mutate(Hotel_Reviews, negative_review = tolower(negative_review),
                        negative_review_given = ifelse(negative_review %in% 
                                                         negative_val$recode_val, "No", "Yes"))
##
##
############
# create positive_review_given variable
###########
#create variable to hold non positive values in positive review
positive_val <- read.xlsx("code_4_recode_negative.xlsx")
# Add positive_review_given variable to the hotel_review dataset
Hotel_Reviews <- mutate(Hotel_Reviews, positive_review = tolower(positive_review),
                        positive_review_given = ifelse(positive_review %in% 
                                                         positive_val$recode_val, "No", "Yes"))
##
##
###################
## Split each reviews into positive and negative
##################
#
hotel_review_d2 <- Hotel_Reviews %>%
  pivot_longer(c(negative_review_given,positive_review_given), 
               names_to = "review_type", values_to = "review_type_value")
#
# rename the review_type variable &
# remove rows with false positive and negative reviews &
hotel_review_d3 <- hotel_review_d2 %>%
  filter(review_type_value == "Yes") %>%
  mutate(review_type = case_match(review_type, 
             "negative_review_given" ~ "negative",
             "positive_review_given" ~ "positive"))
#
# create new variable - review_word_count &
# remove rows with zero word count
hotel_review_d4 <- hotel_review_d3 %>% mutate(review_word_count = ifelse(
  review_type == "negative", review_total_negative_word_counts, review_total_positive_word_counts)) %>%
  filter(review_word_count > 0)
##
##
####################
# Create continent variable in the hotel review dataset
####################
#
# Please do not touch to avoid touching stories
# original country & continent data from https://worldpopulationreview.com/country-rankings/list-of-countries-by-continent
# write_csv(data.frame(reviewer_nationality = unique(Hotel_Reviews$reviewer_nationality)),
          # "hotel_continent.csv")
#
# read in continent dataset
country_by_continent <- read_csv("hotel_continent.csv")
#
# Join review dataset with continent dataset to match reviewer nationality with continent 
hotel_review_d5 <- hotel_review_d4 %>% 
  left_join(country_by_continent, by = join_by(reviewer_nationality))
##
##
################
# Prepare dataset for EDA
################
# most frequent reviewers continent in the hotel dataset
table_continent <- table(hotel_review_d5$continent)
mod_continent <- names(table_continent[which.max(table_continent)])
#
# most frequent length of stay in the hotel dataset
table_length_of_stay <- table(hotel_review_d5$length_of_stay)
mod_length_of_stay <- names(table_length_of_stay[which.max(table_length_of_stay)])
#
# data imputation to handle missing values
# handling missing values for trip_type, continent and length_of_stay
hotel_review_d6 <- hotel_review_d5 %>% 
  mutate(trip_type = ifelse(is.na(trip_type), sample(hotel_review_d5$trip_type,1), trip_type),
         continent = ifelse(is.na(continent), mod_continent, continent),
         length_of_stay = ifelse(is.na(length_of_stay), is.numeric(mod_length_of_stay), length_of_stay))
#
# Change character variables to factors
hotel_review_d7 <- hotel_review_d6 %>% mutate_if(is.character,as.factor)
##
############################
# Exploratory data analysis
###########################
# total reviews by country
view(count(hotel_review_d7,hotel_country,review_type) %>%
  pivot_wider(names_from = review_type, values_from = n) %>%
  mutate(prop = positive/(negative+positive)) %>% # prop => proportion of positive review for each country 
  arrange(desc(prop)))
# plot "Total Reviews by Hotel Country",
count(hotel_review_d7,hotel_country,review_type) %>%
  ggplot(aes(reorder(hotel_country,-n), n, fill=review_type))+
  geom_bar(position = "dodge", stat = "identity")+
  labs(y="Count of Reviews", x="Hotel Country", 
       fill = "Review Type")+
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "K"))+
  theme(plot.title = element_text(hjust=0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12))
#
#
# total reviews by reviewer continent
view(count(hotel_review_d7,continent,review_type) %>%
       pivot_wider(names_from = review_type, values_from = n) %>%
       mutate(prop = positive/(negative+positive)) %>% # prop => proportion of positive review for each country 
       arrange(desc(prop)))
# plot "total reviews by reviewer continent"
count(hotel_review_d7,continent,review_type) %>%
  ggplot(aes(reorder(continent,-n), n, fill=review_type))+
  geom_bar(position = "dodge", stat = "identity")+
  labs(y="Count of Reviews", x="Reviewer Continent", 
       fill = "Review Type")+
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "K"))+
  theme(plot.title = element_text(hjust=0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12))
#
# total no of hotels by country
group_by(hotel_review_d7,hotel_country) %>% 
  summarize(n=length(unique(hotel_name))) %>% arrange(desc(n))
# plot
group_by(hotel_review_d7,hotel_country) %>% 
  summarize(n=length(unique(hotel_name))) %>%
  ggplot(aes(reorder(hotel_country,-n), n, fill=hotel_country))+
  geom_bar(stat = "identity", show.legend = FALSE)+
  labs(y="Count of Hotels", x="Country", 
       title = "Hotels count by Country")+
  scale_fill_brewer(palette = "Dark2")+
  theme(plot.title = element_text(hjust=0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12))
#
# library(RColorBrewer)
# display.brewer.all()
#
# date range for reviews
range(Hotel_Reviews$review_date)
#
#
# total number of reviews by month
count(hotel_review_d7,review_month, review_type, sort = T) %>%
  pivot_wider(names_from = review_type, values_from = n)
# plot
fig1 <- count(hotel_review_d7,review_month, review_type) %>% 
  mutate(review_month = fct_relevel(review_month, 
                                c("January","February","March","April","May","June","July","August","September","October","November","December"))) %>%
  ggplot(aes(review_month ,n, fill=review_month, colour = review_type))+
  geom_line(aes(group = review_type))+
  labs(y="Count of Reviews", x="Month", 
       title = "Total Reviews by Month",
       colour = "Review Type")+
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "K"))+
  theme(plot.title = element_text(hjust=0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(angle=30, hjust=1))
#
#
# total number of reviews by season
view(count(hotel_review_d7,review_season, review_type, sort = T) %>%
  pivot_wider(names_from = review_type, values_from = n) %>%
  mutate(prop = positive/(negative+positive)) %>% # prop => proportion of positive review for each country 
  arrange(desc(prop)))
# plot
fig2 <- count(hotel_review_d7,review_season, review_type) %>% 
  arrange(desc(n)) %>%
  ggplot(aes(review_season, n, fill = review_type)) +
  geom_bar(position = "dodge", stat = "identity")+
  labs(y="Count of Reviews", x="Season", 
       title = "Total Reviews by Season",
       fill = "Review Type")+
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "K"))+
  theme(plot.title = element_text(hjust=0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12))
#
# generate grouped plot for analysis with time
ggarrange(fig1, fig2, nrow = 2, labels = c("(A)","(B)"), hjust = -2)
#
# Top 10 nationality by reviews
count(hotel_review_d7,reviewer_nationality,sort = T) %>%
  slice_head(n=10)
# plot "Top 10 nationality by reviews"
count(hotel_review_d7,reviewer_nationality,sort = T) %>%
  slice_head(n=10) %>%
  ggplot(aes(reorder(reviewer_nationality,-n), n, fill = reviewer_nationality)) +
  geom_bar(stat = "identity", show.legend = FALSE)+
  labs(y="Count of Reviews", x="Reviewer Nationality",
       title = "Top 10 Nationality by Review Frequency",
       fill = "Review Type")+
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "K"))+
  theme(plot.title = element_text(hjust=0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(angle=30, hjust=1))
#
# Hotels count by country
group_by(hotel_review_d7,hotel_country) %>% 
  summarize(n=length(unique(hotel_name)))
# plot "hotel count by country"
group_by(hotel_review_d7,hotel_country) %>% 
  summarize(n=length(unique(hotel_name))) %>%
ggplot(aes(reorder(hotel_country,-n), n, fill = hotel_country)) +
  geom_bar(stat = "identity", show.legend = FALSE)+
  labs(y="Unique hotel count", x="Hotel Country",
       fill = "Review Type")+
  theme(plot.title = element_text(hjust=0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 11))
#
#
# Total Reviews by Reviewers continent
count(hotel_review_d7,continent,sort = T) %>% filter(!is.na(continent))
#
count(hotel_review_d7,continent,sort = T) %>% filter(!is.na(continent)) %>%
  ggplot(aes(reorder(continent,-n), n, fill = continent)) +
  geom_bar(stat = "identity", show.legend = FALSE)+
  labs(y="Count of Reviews", x="Reviewers Continent",
       title = "Total Reviews by Reviewers Continent",
       fill = "Review Type")+
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "K"))+
  theme(plot.title = element_text(hjust=0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12))
#
#
# reviewer score by review type
view(group_by(hotel_review_d7, review_type) %>%
       summarize(avg_reviewer_score = mean(reviewer_score),
                 med_reviewer_score = median(reviewer_score),
                 sd_reviewer_score = sd(reviewer_score)))
# plot
f1 <- ggplot(hotel_review_d7, aes(review_type, reviewer_score))+
  geom_boxplot()+
  labs(x="Review Type", y="Reviewer Score")+
  theme_classic()
#
#
# Average score of hotel by review type
view(group_by(hotel_review_d7, review_type) %>%
       summarize(avg_hotel_score = mean(average_score),
                 med_hotel_score = median(average_score),
                 sd_hotel_score = sd(average_score)))
# plot Average score of hotel by review type
f2 <- ggplot(hotel_review_d7, aes(review_type, average_score))+
  geom_boxplot()+
  labs(x="Review Type", y="Hotel Average Score")+
  theme_classic()
#
# total_number_of_reviews_reviewer_has_given by review type
view(group_by(hotel_review_d7, review_type) %>%
       summarize(med_number_of_reviews_by_reviewer = median(total_number_of_reviews_reviewer_has_given),
                 avg_number_of_reviews_by_reviewer = mean(total_number_of_reviews_reviewer_has_given),
                 sd_number_of_reviews_by_reviewer = sd(total_number_of_reviews_reviewer_has_given)))
# plot total_number_of_reviews_reviewer_has_given by review type
f3 <- ggplot(hotel_review_d7, aes(review_type, total_number_of_reviews_reviewer_has_given))+
  geom_boxplot()+
  labs(x="Review Type", y="Number of Reviews by Reviewer \n (log10 scale)")+
  theme_classic()+
  scale_y_log10()
#
#
# total_number_of_reviews (Hotels) by review type
view(group_by(hotel_review_d7, review_type) %>%
       summarize(avg_total_number_of_reviews = mean(total_number_of_reviews),
                 med_total_number_of_reviews = median(total_number_of_reviews),
                 sd_total_number_of_reviews = sd(total_number_of_reviews)))
# plot total_number_of_reviews (Hotels) by review type
f4 <- ggplot(hotel_review_d7, aes(review_type, total_number_of_reviews))+
  geom_boxplot()+
  labs(x="Review Type", y="Total Number of Reviews")+
  theme_classic()+
  scale_y_log10()
#
#
# Average reviewer score by trip type
view(count(hotel_review_d7, review_type, trip_type, sort = T) %>%
  pivot_wider(names_from = review_type, values_from = n) %>%
  mutate(prop = positive/(negative+positive)) %>% # prop => proportion of positive review for each country 
  arrange(desc(prop)))
# plot
count(hotel_review_d7,review_type,trip_type,sort = T) %>%
  ggplot(aes(trip_type, n, fill=review_type))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(y="Count of Reviews", x="Trip Type",
       title = "Total Reviews by Trip Type",
       fill = "Review Type")+
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "K"))+
  theme(plot.title = element_text(hjust=0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12))
#
# Reviews Count by traveler type
view(count(hotel_review_d7,review_type,traveler_type,sort = T) %>%
  pivot_wider(names_from = review_type, values_from = n) %>%
  mutate(prop = positive/(negative+positive)) %>% # prop => proportion of positive review for each country 
  arrange(desc(prop)))
# plot Reviews Count by traveler type
count(hotel_review_d7,review_type,traveler_type,sort = T) %>%
  ggplot(aes(traveler_type, n, fill=review_type))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(y="Count of Reviews", x="Traveler Type",
       fill = "Review Type")+
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "K"))+
  theme(plot.title = element_text(hjust=0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12))
#
# review word count by review type
view(group_by(hotel_review_d7, review_type) %>% 
       summarize(avg_word_count = mean(review_word_count),
                 median_word_count = median(review_word_count),
                 sd_word_count = sd(review_word_count)))
# plot review word count by review type
f5 <- ggplot(hotel_review_d7, aes(review_type, review_word_count+0.001))+ # added 0.001 to handle error in log transformation
  geom_boxplot()+
  labs(x="Review Type", y="Review Word Count \n (log10 scale)")+
  theme_classic()+
  scale_y_log10()
# 
#
# length_of_stay by review type
view(group_by(hotel_review_d7, review_type) %>% 
  summarize(avg_length_stay = mean(length_of_stay),
            median_length_stay = median(length_of_stay),
            sd_length_stay = sd(length_of_stay)))
# plot length_of_stay by review type
f6 <- ggplot(hotel_review_d7, aes(review_type, length_of_stay))+
  geom_boxplot()+
  labs(x="Review Type", y="Length of stay")+
  theme_classic()
#
#
# generate plot using fig 1 - fig 6
ggarrange(f1, f2, f3, f4, f5, f6, labels = "AUTO", hjust = -1)
ggsave("groupedFig.png",device = "png")
##
##
#####################
# STATISTICAL ANALYSIS
####################
# prepare dataset for Inferential Analysis
# remove variable that have no practical importance
# 
hotel_review_d8 <- dplyr::select(hotel_review_d7, !c(hotel_name, 
                                              hotel_address,
                                              additional_number_of_scoring,
                                              review_month, 
                                              reviewer_nationality,
                                              review_type_value,
                                              lat,
                                              lng,
                                              days_since_review,
                                              tags,
                                              review_total_positive_word_counts,
                                              review_total_negative_word_counts,
                                              review_date,
                                              positive_review,
                                              negative_review)) %>%
  mutate(hotel_country = relevel(hotel_country, "United Kingdom"),
         trip_type = relevel(trip_type, "Leisure trip"),
         continent = relevel(continent, "Europe")) %>%
  rename("Average score" = average_score,
          "Total number of reviews" = total_number_of_reviews,                  
          "Total number of reviews reviewer has given" = total_number_of_reviews_reviewer_has_given,
          "Reviewer score" = reviewer_score,
          "Hotel country" = hotel_country,
          "Review season" = review_season,
          "Trip type" = trip_type,
          "Traveler type" = traveler_type,                             
          "Length of stay" = length_of_stay,
          "Review type" = review_type,                               
          "Review word count" = review_word_count,
          "continent" = continent
          )
#
# correlation matrix for numeric predictors
M <- cor(hotel_review_d9)
ggcorrplot(M, hc.order = TRUE, type = "lower", lab = TRUE)
#
# Build model with positive review as outcome
logit_mod <- glm(formula = `Review type`~.,family = binomial,data = dplyr::select(hotel_review_d8, !`Reviewer score`))
#
summary(logit_mod)
t1 <- tbl_regression(logit_mod)
gt::gtsave(as_gt(t1), file = file.path(getwd(), "logit.png"))
#
logit_mod_pVal <- drop1(logit_mod, test = "Chisq")
t2 <- tbl_regression(logit_mod_pVal)
gt::gtsave(as_gt(t2), file = file.path(getwd(), "logit_2.png"))
#
# Logistic regression diagnostics
# Predict the probability (p) of review type
probabilities <- predict(logit_mod, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "positive", "negative")
# 
# Linearity assumption
hotel_review_d9 <- select_if(hotel_review_d8, is.numeric)  %>% # select numeric predictors
  rename("Average score" = average_score,
         "Total number of reviews" = total_number_of_reviews, 
         "Total number of reviews reviewer has given" = total_number_of_reviews_reviewer_has_given,
         "Reviewer score" = reviewer_score,
         "Length of stay" = length_of_stay,
         "Review word count" = review_word_count)
hotel_review_d10 <- mutate(hotel_review_d9, logit = log(probabilities/(1-probabilities))) %>%
  pivot_longer(!logit, names_to = "predictors", values_to = "predictor.value")
# create scatter plot to examine predictors relationship with the logit of outcome
ggplot(slice_sample(hotel_review_d10, n=100000), aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")
#
# Assessing multicollinearity
view(vif(logit_mod))
#
# Assessing Model Fit
# using McFadden’s pseudo R2 
pR2(logit_mod)["McFadden"]
# using HosmerLemeshow test
hoslem.test(logit_mod$y, fitted(logit_mod))
#
# rank variable importance from the model
var_importance <- varImp(logit_mod) 
view(var_importance %>% rownames_to_column(var = "predictors") %>%
  arrange(desc(Overall)))
# 
# Performing stepwise regression for the model
backward_mod <- stepAIC(logit_mod, direction="backward",trace = FALSE)
#
backward_mod$anova
#
# performing Dom analysis
da.glm.fit()("names")
# domAnalysis_result<-dominanceAnalysis(logit_mod)
#
getFits(domAnalysis_result,"r2.m")
#
plot(domAnalysis_result, which.graph ="general",fit.function = "r2.m")
#
domData %>%
  ggplot(aes(reorder(variable,-r2.m),r2.m, fill=variable))+
           geom_col()+
           labs(x="variable", y=expression("McFadden's Index ("~R[2]*")"))+
  coord_flip()
#
averageContribution(domAnalysis_result, fit.functions = "r2.m")
#
# plot Pearson’s residual against predictors (access linearity)
logit_mod_2 <- glm(formula = review_type~.,family = binomial,
                 data = dplyr::select(hotel_review_d8, !`Reviewer score`) %>%
                   rename_with(~ tolower(gsub(" ", "_", .x, fixed = TRUE))))
#
# ?plot.lm
residualPlots(logit_mod_2)
##################
##################