#causes of death in France 2001-2008
# Melissa Tran
# created: 1/04/2020
-------------------------------------------------------------------------------------------
#load packages 
library(tidyverse)

# working with data ------------------------------------------------------------
data <- read_csv("CausesOfDeath_France_2001-2008_.csv") %>% 
  select(-GEO, -UNIT, -AGE, -Value)%>%    # keep variables: "cause of death", "sex", "year"
  rename(YEAR = TIME,                     # rename variables: "CAUSE", "SEX", "YEAR"
         CAUSE = ICD10) %>%
  filter(DEATHS > 1)%>%                   # filter out causes of death below threshold count
  mutate(total = sum(DEATHS),
         coverage = DEATHS*100/total) %>%
  arrange(desc(coverage))

# Analysing data for females -------------- ------------------------------------
female <- data%>%                           # data for females only
  filter(SEX == "Females") %>%
  select(YEAR, SEX, CAUSE, DEATHS) 

#summary table for females_ grouped cause of death across years
f_summary <- female %>%                     
  group_by(CAUSE)%>%
  summarise(t_f = sum(DEATHS),             #total no. of female deaths across 2001-2008
            m_f = mean(DEATHS),            #mean no. deaths per year
            s_f = sd(DEATHS),
            n_f = n()) %>%
  arrange(desc(m_f)) %>%                   #arrange from most to least deaths
  mutate(total_f = sum(t_f),               #add col. for total number of deaths
         coverage_f = t_f*100/total_f,     #add col, for percentage of deaths covered by cause
         rank = rank(-coverage_f),         #add col. for ranking from most to least deaths
         SEX = "Female")
ungroup()

### final table: Leading cause of death for females ###
top10_f <- f_summary %>%                    
  filter(rank < 11) %>%
  select(CAUSE, rank, coverage_f, SEX)%>%
  rename(percentage = coverage_f)%>%
  arrange(rank)
view(top10_f)

# creating the plot--------------------------------------------------
#basic plot
fp <- ggplot(top10_f, aes(x = percentage)) + 
  geom_col(aes(y = reorder(CAUSE, percentage)), fill = "pink") +
  geom_rug()

#changing axis tick mark labels
fp1 <- fp + 
  theme(axis.text.x = element_text(size = 12),                          #edit aesthetics, label orientation, colour
        axis.text.y = element_text(size = 10),
        axis.line = element_line(color = "grey", size = 0.5))+
  scale_x_continuous(name = "Percentage of total female deaths (%)")+
  scale_y_discrete(name = "Cause of death")+
  ggtitle("Leading causes of death in Females")

print(fp1)

#### obtain top 10 leading causes of death for males ------------------------------------
male <- data%>%                           # data for males only
  filter(SEX == "Males") %>%
  select(YEAR, SEX, CAUSE, DEATHS) 

#summary table for males_ grouped cause of death across years
m_summary <- male %>%                     
  group_by(CAUSE)%>%
  summarise(t_m = sum(DEATHS),
            m_m = mean(DEATHS),
            s_m = sd(DEATHS),
            n_m = n()) %>%
  arrange(desc(m_m)) %>%
  mutate(total_m = sum(t_m),
         coverage_m = t_m*100/total_m,
         rank = rank(-coverage_m),
         SEX = "Male")%>%
  ungroup()

### final: Leading cause of death for males ###
top10_m <- m_summary %>%                    
  filter(rank < 11) %>%
  select(CAUSE, rank, coverage_m, SEX)%>%
  arrange(rank)
view(top10_m)

# creating the plot--------------------------------------------------
#basic plot
mp <- ggplot(top10_m, aes(x = coverage_m)) + 
  geom_col(aes(y = reorder(CAUSE, coverage_m)), fill = "sky blue") +
  geom_rug()

#changing axis tick mark labels
mp1 <- mp + 
  theme(axis.text.x = element_text(size = 12), #edit aesthetics, label orientation, colour
        axis.text.y = element_text(size = 10),
        axis.line = element_line(color = "grey", size = 0.5))+
  scale_x_continuous(name = "Percentage of total male deaths (%)")+
  scale_y_discrete(name = "Cause of death")+
  ggtitle("Leading causes of death in Males")

print(mp1)