source("0_libraries.R", local = TRUE)

# data()
star_df = data.table(starwars)
class(star_df)

# Re-coding the data
star_df = star_df %>%
  # Giving the datatype as factor/categorical variable
  mutate(sex = as.factor(sex)) %>%
  # re-coding
  mutate(sex = recode(sex,
                      "male" = "man",
                      "female" = "woman")) %>%
  # Giving levels to the data (best for categorical variables with order (Ordinal data))
  mutate(sex = factor(sex, levels = c('man', 'woman', 'hermaphroditic', 'none'))) %>%
  View()
  #glimpse()

# Checking the Levels of the data
levels(star_df$sex)

# Small summary of the distince count (can use summarize)
star_df[,n_distinct(name), by=sex]

# Making a new column that would be a list
star_df[, unique_h_color := .(list(unique(hair_color))), by = sex]

data = gapminder %>%
  select('country', 'year', 'lifeExp')

# Going from long data format to wide format (Quite useful for plotting)
wide_data = data %>%
  pivot_wider(names_from = year, values_from = lifeExp)

# Going from wide format to long format
long_data = wide_data %>%
  pivot_longer(2:13, names_to = 'year', values_to = 'lifeExp')

# Range/spread in R
# min(), max(), range(), IQR()

# Centrality
# mean(), median()

# Variance
# var()

# Some descriptive summary
msleep %>%
  select('awake', 'sleep_total') %>%
  summary()

# Summarizing the data
msleep %>%
  # Dropping NAs
  drop_na(vore) %>%
  # Grouping
  group_by(vore) %>%
  # Summarizing
  summarise(
    Lower = min(sleep_total),
    Average = mean(sleep_total),
    Upper = max(sleep_total),
    Diff = max(sleep_total) - min(sleep_total),
    Genus_distinct = n_distinct(genus)
  ) %>%
  # Arranging
  arrange(desc(Average)) %>%
  View()

# Visualization

plot(pressure)


olympic = matrix(
  c(46,37,38,121,27,23,17,67,26,18,26,70,19,18,19,56,17,10,15,42,12,8,21,41),
  ncol = 4,
  byrow = TRUEs
)
colnames(olympic) = c('Gold', 'Silver', 'Bronze', 'Total')
rownames(olympic) = c('USA', 'GBR', 'CHN', 'RUS', 'GER', 'JPN')

# Introduction to Data Science

medals <- data.frame(Country=c("USA","GBR","CHN"),
                               Gold=c(46,27,26),
                               Silver=c(37,23,18),
                               Bronze=c(38,17,26))

rio2016Medals<-read.csv("Rio2016(2).csv"
              , header=TRUE)


# dt[,{tmp1=mean(mpg); tmp2=mean(abs(mpg-tmp1)); tmp3=round(tmp2, 2); list(tmp2=tmp2, tmp3=tmp3)}, by=
