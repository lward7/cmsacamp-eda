library(tidyverse)

# determining whether those with early season (first three races) wins have better overall seasons
# do early season results predict overall results better than late season results, or random race results? (no)
# hypothesis unjustified??

indycar <- indycar %>% 
  group_by(drive_id) %>% 
  mutate(seasonpoints = sum(points)) %>% 
  ungroup()

  #first three races vs overall scores
indycar_firstthree <- indycar %>% 
  filter(race_id %in% c("2019_Firestone_Grand_Prix_of_St_Petersburg", 
                        "2019_IndyCar_Classic", 
                        "2019_Honda_Indy_Grand_Prix_of_Alabama")) %>% 
  select(date, 
         race_id, 
         start, 
         finish, 
         points, 
         drive_id,
         seasonpoints) %>% 
  group_by(drive_id) %>% 
  mutate(firstthree_sum = sum(points)) 
  
indycar_firstthree %>% 
  ggplot(aes(x = firstthree_sum, 
             y = seasonpoints)) +
  geom_point()

#last three races vs overall score
indycar_lastthree <- indycar %>% 
  arrange(date) %>% 
  filter(race_id %in% c("2019_Firestone_Grand_Prix_of_Monterey", 
                        "2019_Grand_Prix_of_Portland", 
                        "2019_Bommarito_Automotive_Group_500")) %>% 
  select(date, 
         race_id, 
         start, 
         finish, 
         points, 
         drive_id,
         seasonpoints) %>% 
  group_by(drive_id) %>% 
  mutate(lastthree_sum = sum(points))
  
indycar_lastthree %>% 
  ggplot(aes(x = lastthree_sum, 
             y = seasonpoints)) +
  geom_point()

  #selecting three random races vs overall scores
indycar_randthree <- indycar %>% 
  filter(race_id %in% c(sample(indycar$race_id,3))) %>% #trying to randomly select three races
  select(date, 
         race_id, 
         start, 
         finish, 
         points, 
         drive_id,
         seasonpoints) %>% 
  group_by(drive_id) %>% 
  mutate(randthree_sum = sum(points))

indycar_randthree %>% 
  ggplot(aes(x = randthree_sum, 
             y = seasonpoints)) +
  geom_point() 



#looking at distribution of season points
indycar_seasonpoints <- indycar %>%
  group_by(drive_id) %>% 
  summarize(seasonpoints = sum(points))

indycar_seasonpoints %>% 
  ggplot(aes(x = seasonpoints)) +
  geom_histogram(binwidth = 20)
  
indycar_seasonpoints %>% 
  ggplot(aes(x = seasonpoints)) +
  stat_ecdf()

# comparing starting position and finishing position

indycar %>% 
  ggplot(aes(x = start,
             y = finish,
             color = track_type)) +
  geom_jitter() +
  facet_grid(cols = vars(track_type)) +
  geom_smooth()

#looking at teams
install.packages("ggthemes")
library(ggthemes)

indycar %>% 
  select(race_id,
         points,
         drive_id,
         team_id) %>% 
  group_by(drive_id) %>% 
  mutate(drivepoints = sum(points)) %>% 
  filter(race_id == "2019_Indianapolis_500") %>% 
  group_by(team_id) %>% 
  mutate(teamsize = n(),
         teampoints = sum(drivepoints)) %>% 
  ggplot(aes(x = teamsize,
         y = teampoints / teamsize)) +
  geom_point(size = 2) +
  theme_bw() +
  labs(x = "Team Size",
       y = "Average Points per Team Member",
       title = "Comparing Team Size and Driver Success")
  
  

# road type and frame brand
indycar %>% 
  filter(finish == 1) %>% 
  ggplot(aes(x= track_type, 
             color = chassis_engine_tires)) +
  geom_bar() +
  theme_bw()

  # top 10
indycar %>% 
  filter(finish %in% c(1:10)) %>% 
  ggplot(aes(x = track_type,
             fill = chassis_engine_tires)) +
  geom_bar()

seasonracers <- indycar %>% 
  select(race_id, drive_id, finish, points, seasonpoints) %>% 
  group_by(drive_id) %>% 
  mutate(races = n()) %>% 
  filter(races == 17) %>% 
  mutate(driverfinish = sum(finish)) %>% 
  filter(race_id %in% c("2019_Indianapolis_500",
                        "2019_Firestone_Grand_Prix_of_Monterey")) %>% 
  mutate(indymonterey = sum(finish))

seasonracers %>% 
  ggplot(aes(x = indymonterey,
             y = driverfinish,
             color = seasonpoints)) +
  geom_point()          #really difficult interpretation, because lower placings are better, maybe color helps?


indycar_seasonracers <- indycar %>% 
  select(race_id, finish, points, drive_id, track_type) %>% 
  group_by(track_type,drive_id) %>% 
  mutate(track_typepoints = sum(points)) %>% 
  ungroup() %>%
  group_by(drive_id) %>% 
  mutate(races = n()) %>% 
  filter(races == 17) %>% 
  mutate(seasonpoints = sum(points)) %>% 
  filter(seasonpoints > 388) %>% 
  mutate(drive_idcleaned = gsub("_"," ",drive_id))

  


 #stacked bar chart
indycar_seasonracers %>%  ggplot(aes(x = reorder(drive_idcleaned, -points),
                                     y = points,
                                     fill = track_type)) +
  geom_col() + #default position = "stack"
  coord_flip() +
  theme_bw() +
  labs(x = "Total Season Points",
       y = "Driver Name",
       fill = "Track Type",
       title = "Points Earned by Track Type") +
  scale_fill_discrete(labels = c("Oval","Road","Street")) +
  scale_fill_colorblind()

#side-by-side bars (this is not correct - why???)
indycar_seasonracers %>%  ggplot(aes(x = drive_id,
                                     y = points,
                                     fill = track_type)) +
  geom_col(position = "dodge") + #not correct when dodge is added, so weird
  coord_flip() 

#testing geom_col() with Titanic dataset, seems to work fine with the dodge function here
Titanic <- as.data.frame(Titanic)

Titanic %>% ggplot(aes(x = Class, y = Freq, fill = Survived)) +
  geom_col()

Titanic %>% ggplot(aes(x = Class, y = Freq, fill = Survived)) +
  geom_col(position = "dodge")



  

#clustering (in progress)
install.packages("protoclust")
library(protoclust)

indycar %>% ggplot(aes(x = start,y = finish)) +
  geom_point()

minimax_data <- indycar %>% 
  filter(race_id == "2019_Indianapolis_500")
  
indycar_minimax <- protoclust(dist(dplyr::select(minimax_data,
                                                 start,
                                                 finish)))
plot(indycar_minimax)

indycar_clusters <- 
  protocut(indycar_minimax, k=4)

indycar_clustering <- minimax_data %>% 
  mutate(clusters = as.factor(indycar_clusters$cl))

indycar_clustering %>% 
  ggplot(aes(x = start,
             y = finish,
             color = clusters)) +
  geom_point(size = 2) +
  scale_color_colorblind() +
  theme_bw() +
  labs(x = "Starting Position",
       y = "Finishing Position",
       title = "Starting and Finishing Position in the 2019 Indianapolis 500",
       color = "Clusters") 


