install.packages("readr")
install.packages("MASS")
install.packages("tidyverse")

library("readr")
library("survival")
library(tidyverse)

viz_data <- readr::read_csv("data/viz_data.csv")
mean(viz_data$weight) #can check ?help and specify, if NAs are included or not
median(viz_data$weight)
min(viz_data$bmi)
max(viz_data$bmi)
quantile(viz_data$bmi)
sd(viz_data$bmi)
cov(viz_data$height,viz_data$weight) #co-vary?
cor(viz_data$height,viz_data$weight) #correlate

#tests of association and extracting their output
group_outcome_table <- table(viz_data$group,viz_data$outcome)
chisq.test(group_outcome_table)
fisher.test(group_outcome_table)
t.test(response~group,data=viz_data,subset=group %in% c("High dose","Low dose")) #~ separates the axes, so y~x
wilcox.test(response~group,data=viz_data,subset=group %in% c("High dose","Low dose")) #%in% is equivalent to == here, so shorter form for OR here, instead of colors == "red"
response_site_aov=aov(response~site,data=viz_data)
summary(response_site_aov) #seems like ANOVA having a summary is better than the raw output?
response_ttest=t.test(response~group,data=viz_data,subset=group %in% c("High dose","Low dose"))
str(response_ttest) #best to always check than guess the variable,because R will silently return NULL instead of telling you what's wrong
response_ttest$p.value
response_ttest$conf.int
weight_summary=summary(viz_data$weight)
str(weight_summary)
weight_summary["Median"]
weight_summary[[3]]

#changing default behaviour in functions
summary(viz_data$bmi)
quantile(viz_data$bmi,probs = seq(0,1,0.1))
viz_data[,c("height")]
t.test(response~group,data=viz_data,subset=group %in% c("High dose","Low dose"),alternative ="greater")

viz_data=readr::read_csv("data/viz_data.csv")
followup_data=readr::read_csv("data/followup_data.csv")
head(viz_data)
head(followup_data)
followup_data=merge(viz_data,followup_data,by='id')

#regressions - lm usually used for continuous outcomes and no need for non-normal family, glm for logistic or negative binomial regression
lm_fit=lm(response~bmi+group,data=viz_data)
summary(lm_fit)
viz_data$improved=as.integer(viz_data$outcome == "Improved")
logistic_fit=glm(improved~bmi+group,data=viz_data,family=binomial)
summary(logistic_fit)

negbin_fit=MASS::glm.nb(clinic_visits~group,data=followup_data)
followup_surv=Surv(followup_data$survival_months,followup_data$event) #Surv combines a time variable and event indicator (1 for event observed, 0 for censored)
head(followup_surv)
cox_fit=coxph(
  Surv(survival_months,event)~group,
  data=followup_data
) #coxph is from survival package, fits a cox proportional hazards model
summary(cox_fit)
coef(lm_fit) #coefficient
confint(lm_fit) #confidence intervals
summary(lm_fit)$coefficients #returns a table of estimates, standard errors, test statistics, p-values
plot(lm_fit,which=1) #which=1 selects just the first of four default diagnostic plots, residuals vs fitted values
plot(logistic_fit,which=1)
plot(negbin_fit,which=1)
cox_survfit=survfit(cox_fit,newdata=data.frame(group=unique(followup_data$group)))
plot(cox_survfit,
     xlab="Time (months)",
     ylab="Estimated survival probability")
ggplot(viz_data,aes(x=bmi,y=response))+
  geom_point()+
  geom_smooth(method="lm",se=TRUE)
ggplot(viz_data,aes(x=bmi,y=as.numeric(improved)))+
  geom_point()+
  geom_smooth(method="glm",method.args=list(family="binomial"),se=TRUE)
ggplot(followup_data,aes(x=group,y=clinic_visits))+
  geom_jitter(width=0.1,height=0)+
  stat_summary(fun=mean,geom="point",size=3,color="red") #specify how much jitter, big red dots are mean of the groups
cox_survfit_df=broom::tidy(cox_survfit)
ggplot(cox_survfit_df,aes(x=time))+
  geom_step(aes(y=estimate.1),color="black")+
  geom_step(aes(y=estimate.2),color="red")+
  geom_step(aes(y=estimate.3),color="blue")+
  labs(x="Time(months)",
       y="Estimated survival probability",
       color="Group")

#data wrangling
clinic_raw=read_csv("data/clinic_raw.csv",skip=2) #skip=2 drops the title and blank line sitting above the real header; merged cells are pain lmao
str(clinic_raw) #see that coercion happened, where a single non-numeric value forces the whole column to be stored as text; maybe can insert self notes which to be cleaned
sum(is.na(clinic_raw$age)) #counts how many missing values a column has
range(clinic_raw$age,na.rm=TRUE) #drops NAs before computing min and max
summary(clinic_raw)
ggplot(clinic_raw,aes(x=age))+
  geom_histogram(binwidth=5,na.rm=TRUE)
clinic_clean=
  clinic_raw %>% #%>% is read "and then", i.e. to sequence steps
  rename(id=patient_id) #changes column's name without touching its values
clinic_clean=
  clinic_clean %>%
  select(id,site,sex,age,visit_date,blood_pressure,weight_kg,notes) #keeps (or drops when - is used) columns
clinic_clean=
  clinic_clean %>%
  filter(age>=18|is.na(age)) #keeps adults, but don't drop missing ages
clinic_clean %>% arrange(age) %>% head() #youngest patients first
clinic_clean %>% arrange(desc(age)) %>% head() #desc is for descending, therefore oldest patients first

clinic_clean %>%
  group_by(site) %>% #splits data into groups based on one or more columns, without changing how it looks like yet
  summarise( #collapses each group down to a single summary row, using whatever calculation specified
    mean_age=mean(age,na.rm=TRUE)
    ,n=n() #count number of rows in each group
  )

clinic_clean=
  clinic_clean%>%
  mutate( #creates a new column or overwrites an existing one, based on some calculations
    site=tolower(trimws(site)) #make lowercase & trim (strip away) whitespace
    , site=gsub("_"," ",site) #replaces all matches of a pattern in a string; sub() replaces only the first match
  ) #even though 2 sites here, it's done stepwise, one after another
table(clinic_clean$site)
substring("PT-0001",4,7) #extracts a portion of a string based on start and end character positions
clinic_clean=
  clinic_clean %>%
  mutate(
    bp_upper=as.numeric(str_extract(blood_pressure,"^[0-9]+")) #extract digits, which will stop after hitting the / in 120/80 mmHg
    , bp_lower=as.numeric(str_extract(blood_pressure,"(?<=/)[0-9]+")) #digits after the / in 120/80 mmHg, which will stop after hitting the space before mmHG
  )
str_extract("123/23","[0-9]+$") #will return 23, experimenting with regular expression (regex), whole book on it! but simply put, ^ means start from beginning, $ means start from the back; [] is looking for the numerals, and the + allows for multiple extracting it out until it hits a non-number
head(clinic_clean[,c("blood_pressure","bp_upper","bp_lower")]) #woah! checks if the transformation was correct
gsub("[^0-9.]","","72.5 kg") #strips everything except the digits and .
grep("follow",clinic_clean$notes,ignore.case=TRUE) #grep returns positions of the elements in a vector that matches a pattern (defined in first argument of the grep function); ignore.case catches "FOLLOW UP" and "follow up"
clinic_clean %>% filter(grepl("follow",notes,ignore.case=TRUE)) #grepl returns TRUE/FALSE for every element, on whether it matches the pattern

#still data wrangling - converting between data types
#earlier worked on bp, now we'll try to do weight and visit_date
clinic_clean=
  clinic_clean %>%
  mutate(
    weight_kg=na_if(weight_kg,"-") #turns - into NA
    , weight_kg=as.numeric(weight_kg) #now safe to convert to numeric
    , visit_date=as.Date(visit_date,format="%d/%m/%Y")
  )
head(clinic_clean)
clinic_clean=
  clinic_clean %>%
  mutate(
    sex=ifelse( #ifelse() checks a condition, returns one value if true, and another if false
      toupper(sex) == "M","Male"
      , ifelse(toupper(sex)=="F","Female",NA) #ifelse for the third outcome
    )
  )
clinic_clean=
  clinic_clean %>%
  mutate(
    bp_category=case_when( #case_when() extends same idea as ifelse to any number of conditions, easier than nesting ifelse()
      is.na(bp_upper) ~NA_character_ #keep missing as missing
      , bp_upper<120 ~"Normal"
      , bp_upper<140 ~"Elevated"
      , TRUE ~"High" #catch anything left
    )
  )
table(clinic_clean$bp_category,useNA="always")

#writing own function
clean_text=function(x) { #funny problem, if cursor is on the following lines and I press ctrl-enter, it will run only those lines; if I put the cursor on this line (before the curly brackets, it recognises the whole chunk to be run when ctrl+enter if run)
  x=trimws(x) #strip whitespace
  x=tolower(x) #standardise case
  x
}

clean_text("   FOLLOW UP") #test if new function works, should return "follow up"
clinic_clean=
  clinic_clean %>%
  mutate(notes=clean_text(notes))
table(clinic_clean$notes,useNA="always")

2+2
