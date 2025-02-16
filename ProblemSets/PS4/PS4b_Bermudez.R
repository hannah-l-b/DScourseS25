library(tidyverse)
library(sparklyr)

# Install Spark and Connect to R
spark_install(version = "3.0.0")
sc <- spark_connect(master = "local")

# Load 'Iris' data frame into Spark
df1 <- as_tibble(iris)

# Copy tibble into SparK
df  <- copy_to(sc, df1)

# Verify data frames
print(class(df1))
print(class(df))

# List 6 rows of Sepal_Length and Species columns of df
df  %>% select(Sepal_Length,Species) %>% head(6) %>% print
df1 %>% select(Sepal.Length,Species) %>% head(6) %>% print

# List 6 rows of all columns where Sepal_Length > 5.5
df  %>% filter(Sepal_Length>5.5) %>% head(6) %>% print
df1 %>% filter(Sepal.Length>5.5) %>% head(6) %>% print

# List columns Sepal_Length and Species, and only include rows where Sepal_Length > 5.5
df  %>% filter(Sepal_Length>5.5) %>% select(Sepal_Length,Species) %>% head %>% print

# Group by Species and calculate average of Sepal_Length
df2 <- df %>% group_by(Species) %>% summarize(mean  = mean(Sepal_Length), count = n() %>% head %>% print

# Order results ascending by Species
df2 %>% arrange(Species) %>% head %>% print
