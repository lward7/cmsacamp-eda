library(tidyverse)

# determining whether those with early season (first three races) wins have better overall seasons
# hypothesis unjustified??

indycar <- indycar %>% 
  group_by(drive_id) %>% 
  mutate(seasonpoints = sum(points)) %>% 
  ungroup()

  #first three races vs overall scores
indycar_firstthree <- indycar %>% 
  arrange(date) %>% 
  group_by(drive_id) %>% 
  mutate(seasonpoints = sum(points)) %>% 
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
  group_by(drive_id) %>% 
  mutate(seasonpoints = sum(points)) %>% 
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
n_distinct(indycar$race_id)

sample(1:17,3)

indycar_randthree <- indycar %>% 
  arrange(date) %>% 
  group_by(drive_id) %>% 
  mutate(seasonpoints = sum(points)) %>% 
  filter(race_id %in% c("2019_REV_Group_Grand_Prix_at_Road_America", 
                        "2019_Chevrolet_Detroit_Grand_Prix_Race_1", 
                        "2019_Firestone_Grand_Prix_of_Monterey")) %>% 
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



#histogram of 
indycar_seasonpoints <- indycar %>% 
  group_by(drive_id) %>% 
  summarize(seasonpoints = sum(point))
  

indycar_randthree %>% 
  ggplot(aes(x = seasonpoints)) +
  geom_histogram()
  
indycar_randthree %>% 
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
  geom_point() +
  geom_smooth()

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
  select(race_id, drive_id, finish, points) %>% 
  group_by(drive_id) %>% 
  mutate(races = n()) %>% 
  filter(races == 17) %>% 
  mutate(driverfinish = sum(finish)) %>% 
  filter(race_id %in% c("2019_Indianapolis_500",
                        "2019_Firestone_Grand_Prix_of_Monterey")) %>% 
  mutate(indymonterey = sum(finish))

seasonracers %>% 
  ggplot(aes(x = indymonterey,
             y = driverfinish)) +
  geom_point()

indycar_plot <- indycar %>% 
  select(race_id, finish, points, drive_id, track_type) %>% 
  group_by(track_type,drive_id) %>% 
  mutate(track_typepoints = sum(points)) %>% 
  ungroup() %>%
  group_by(drive_id) %>% 
  mutate(races = n()) %>% 
  filter(races == 17) %>% 
  mutate(seasonpoints = sum(points))
  



indycar_plot %>%  ggplot() +
  geom_col(aes(x = drive_id,
               y = points,
               fill = track_type),
           position = "") +
  coord_flip() 



Titanic <- as.data.frame(Titanic)
Titanic %>% ggplot(aes(x = Class, y = Freq, fill = Survived)) +
  geom_col()

Titanic %>% ggplot(aes(x = Class, y = Freq, fill = Survived)) +
  geom_col(position = "dodge")


indycar_scores<-indycar %>% 
  group_by(drive_id) %>% 
  summarize(seasonpoints = sum(points))
indycar_scores %>% 
  ggplot(aes(x = seasonpoints)) +
  stat_ecdf()
indycar_scores %>% 
  ggplot(aes(x=seasonpoints)) +
  geom_histogram() +
  stat_bin(binwidth = 10)+
  theme_bw()
  

#clustering
install.packages("protoclust")
library(protoclust)


minimax_data <- indycar %>% 
  dplyr::select(race_id,start,points,drive_id,team_id,track_type,chassis_engine_tires) %>%
  group_by(drive_id) %>% 
  mutate(seasonpoints = sum(points)) %>% 
  mutate(average_start = mean(start)) %>% 
  filter(race_id == "2019_Indianapolis_500")

minimax_data %>% ggplot(aes(x = average_start,y = seasonpoints)) +
  geom_point()
  
indycar_minimax <- protoclust(dist(dplyr::select(indycar,
                                                 points,
                                                 start)))
plot(indycar_minimax)

indycar_clusters <- 
  protocut(indycar_minimax, k=4)

indycar_filtered %>% indycar %>% 


nba_filtered_stats <- nba_filtered_stats %>%
  mutate(minimax_clusters = 
           as.factor(minimax_player_clusters$cl))
nba_filtered_stats %>%
  ggplot(aes(x = x3pa, y = trb,
             color = minimax_clusters
