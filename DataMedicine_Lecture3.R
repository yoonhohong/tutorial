
# project: predictALS 

temp <- gapminder %>%
  select(continent, country)

temp %>%
  distinct() -> temp2

gapminder %>%
  filter(continent == "Asia")

gapminder %>%
  arrange(desc(gdpPercap))

gapminder %>%
  mutate(gdp = gdpPercap * pop)

gapminder %>% 
  group_by(continent, country) %>%
  filter(year == 2007) %>%
  summarize(meanLifeExp = mean(lifeExp))
  
x1 = c("a", "b", "c")
y1 = c("b", "c", "d")

intersect(x1, y1)
setdiff(x1, y1)
union(x1, y1)  
  
x2 = c(1, 2, 3)
y2 = c(2, 3, 4)

x12 = data.frame(x1, x2)
y12 = data.frame(y1, y2)  

names(y12)[1] = "x1"
  
inner_join(x12, y12, by="x1")
left_join(x12, y12, by="x1")
right_join(x12, y12, by="x1") 
full_join(x12, y12, by="x1") 

alsfrs = read.csv("ALSFRS_long.csv")

levels(alsfrs$feature)

alsfrs_wide = spread(alsfrs, key = "feature", value = "value")

alsfrs_long = 
  gather(alsfrs_wide, key = "feature", 
         value = "val", ALSFRS_R_Total:trunk)

gapminder %>%
  filter(year == 2007) -> gapminder_2007

p = ggplot(gapminder_2007, aes(x=gdpPercap, y=lifeExp)) 
p + geom_point(aes(color = continent, size=pop)) + 
  facet_grid(.~continent)

gapminder %>%
  group_by(continent, year) %>%
  summarize(meanLifeExp = mean(lifeExp)) -> temp

p = ggplot(temp, aes(x=year, y=meanLifeExp, col=continent))
p + geom_line() + geom_point()

p = ggplot(gapminder_2007, aes(gdpPercap, lifeExp))
p + geom_point() + scale_x_log10() + 
  stat_smooth(method = "lm", se = F, col = "red")

gapminder %>%
  select(continent, country) %>%
  distinct() -> temp 

temp %>%
  group_by(continent) %>%
  summarize(n = n()) -> temp2

ggplot(temp2, aes(x=continent, y=n)) + 
  geom_col()  

ggplot(temp, aes(x=continent)) + geom_bar(aes(fill = continent)) + 
  coord_flip()

ggplot(gapminder, aes(x=lifeExp)) + geom_histogram(binwidth = 5)

ggplot(gapminder, aes(x=lifeExp)) + stat_bin()

ggplot(gapminder, aes(x=continent, y=lifeExp)) + 
  geom_boxplot() + geom_jitter(col = "orange", alpha = 0.5)

ggplot(gapminder, aes(x=continent, y=lifeExp)) +
  geom_violin()

ggplot(gapminder, aes(x=continent, y=lifeExp)) + 
  geom_boxplot() + theme_bw() + 
  labs(title = "mean life expectancy across continents", 
       x = "", 
       y = "Mean life expectancy") + 
  theme(text = element_text(size = 16, family="Comic Sans MS"))

proact = read.delim("proact_sample.txt", sep = "|")
str(proact)
levels(proact$form_name)

proact %>%
  filter(form_name == "ALSFRS") -> alsfrs

alsfrs = droplevels(alsfrs) 
levels(alsfrs$feature_name)

alsfrs <- alsfrs %>%
  filter(feature_name %in% c("ALSFRS_R_Total", 
                             "hands", "leg", 
                             "mouth", "respiratory_R", 
                             "trunk")) 

alsfrs = alsfrs %>%
  select(-c(form_name, feature_unit))

alsfrs = rename(alsfrs, feature = feature_name, 
                value = feature_value, 
                delta = feature_delta)

alsfrs = droplevels(alsfrs)

alsfrs$value = as.integer(as.character(alsfrs$value))

alsfrs = distinct(alsfrs)

alsfrs_wide = spread(alsfrs, key = "feature", value = "value")

alsfrs %>%
  group_by(SubjectID, feature, delta) %>%
  filter(n() == 1) -> alsfrs

alsfrs_wide = spread(alsfrs, key = "feature", value = "value")
alsfrs_wide_complete = alsfrs_wide[complete.cases(alsfrs_wide),]

alsfrs_long = gather(alsfrs_wide_complete, 
                     key = "feature", 
                     value = "value", 
                     ALSFRS_R_Total:trunk)

write.csv(alsfrs_long, "ALSFRS_long.csv", 
          quote = F, row.names = F)

alsfrs_long$SubjectID = factor(alsfrs_long$SubjectID)
alsfrs_wide_complete$SubjectID = factor(alsfrs_wide_complete$SubjectID)

alsfrs = alsfrs_wide_complete

sample_subj = sample(alsfrs$SubjectID, 10)
alsfrs %>%
  filter(SubjectID %in% sample_subj) -> temp

ggplot(temp, 
       aes(x=delta, y=ALSFRS_R_Total, 
           col=SubjectID)) + geom_line() + 
  theme(legend.position = "none")








