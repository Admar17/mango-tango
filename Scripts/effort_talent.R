library(tidyverse)

skill <- data.frame(talent = seq(0,10,.1), effort = seq(0,10,.1))
skill$skill <- skill$talent * skill$effort
skill$accomplishment <- skill$skill * skill$effort

ggplot(data = skill)+
  geom_line(aes(color = talent, x = effort, y = accomplishment))




talent_seq <- seq(0,10,.1)

i <- 1
effort_ <- i

talent_seq*effort_

grit_df <- data.frame(effort = NA, talent = NA,skill = NA)

for (i in 1:10){
  effort = i
  talent = talent_seq
  skill = effort*talent
  
  grit_df_ <-data.frame(effort = effort, talent = talent, skill = skill)
  
  grit_df <- rbind(grit_df_, grit_df)
}

combinde_df <- grit_df %>%
  select(talent, effort, skill) %>% 
  drop_na() %>% 
  arrange(talent, effort) %>% 
  mutate(accomplishment= skill * effort)


combinde_df %>% 
  filter(talent <= 1) %>% 
  # head(10) %>% 
  ggplot() +
  geom_line(aes(x = effort, y = (skill), color = as.factor(talent), group = talent))+
  labs(x = 'Effort', y = 'Skill', color = 'Talent', title = 'Effort Leads to Skill')+
  # scale_color_discrete(colors = rainbow(10))
  guides(colour = guide_legend(reverse=T))

combinde_df %>% 
  # head(10) %>% 
  ggplot() +
  geom_line(aes(x = effort, y = (accomplishment), color = as.factor(talent), group = talent))+
  guides(colour = guide_legend(reverse=T))+
  labs(x = 'Effort', title = 'Effort Counts Twice',y = 'Accomplishment', color = 'Talent')

grit_lm <- lm(accomplishment ~ effort + effort:as.factor(talent) , data = combinde_df)

plot(accomplishment ~ effort, col = as.factor(talent), data = combinde_df)
abline(grit_lm)

plot(grit_lm)

plot(grit_lm$residuals, col = as.factor(combinde_df$talent))

grit_df__ <- data.frame(effort = NA, talent = NA,skill = NA, effort_2 = NA, accomplishment = NA)

for (i in 1:10){
  effort_2 = i
  skill = combinde_df$skill
  accomplishment <- effort_2*skill
  
  grit_df_ <-data.frame(effort = effort, talent = talent, skill = skill, effort_2 = i, accomplishment = accomplishment)
  
  
  
  
  grit_df__ <- rbind(grit_df_, grit_df)
  
}


grit_df__

for (effort in 2:2){
  
  skill <- effort * seq(1,10)
  print(paste('Skill:', skill))
  
  for (effort_two in 1:1){
    
    achievement <- effort_two * seq(1,10)
    
    print(paste('Achievement: ', achievement))
    
  }
  
  
  
  
}
