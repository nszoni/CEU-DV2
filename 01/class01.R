library(dplyr)
library(ggplot2)
library(GGally)

source('http://bit.ly/CEU-R-shoes')

plot(students$shoe, students$math)

str(students)
plot(students)

ggpairs(students)

summary(lm(math ~ x, students))

library(psych)

partial.r(students, 1:2, 3)


# 2 -----------------------------------------------------------------------

download.file('http://bit.ly/hun-cities-distance', 'cities.xls')

library(readxl)

cities <- read_excel('cities.xls')

cities <- cities[, -1]
cities <- cities[-nrow(cities), ]

mds <- cmdscale(as.dist(cities))

plot(mds)
text(mds[, 1], mds[, 2], names(cities))

library(ggplot2)

mds <- as.data.frame(mds)

mds$city <- rownames(mds)

ggplot(mds, aes(V1, -V2, label = city)) +
  geom_text() + 
  theme_void()

mds2 <- as.data.frame(cmdscale(eurodist))
mds2$city <- rownames(mds2)
ggplot(mds2, aes(V1, -V2, label = city)) +
  geom_text() + 
  theme_void


# 3 -----------------------------------------------------------------------

mds3 <- cmdscale(dist(scale(mtcars)))

plot(mds3)
text(mds[, 1], mds[ ,2], rownames(mtcars))

mds3 <- as.data.frame(mds3)
mds3$car <- rownames(mds3)

library(ggrepel)
ggplot(mds3, aes(V1, V2, label = car)) +
  geom_text_repel()

subset(mtcars, hp >= 245)


# 4 -----------------------------------------------------------------------

plot(UCBAdmissions)

data <- as.data.frame(UCBAdmissions)

ggplot(data) +
  geom_col(aes(Gender, Freq, fill = Admit), position = 'fill') +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~Dept) +
  scale_fill_manual(values = c('Admitted' = 'darkgreen', 'Rejected' = 'red')) +
  theme('legend.position' = 'None')


# 5 -----------------------------------------------------------------------

str(iris)

ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
  geom_point(aes(color = Species)) +
  geom_smooth(color = 'black', method = 'lm', se=F) +
  geom_smooth(aes(color = Species), method = 'lm', se=F) +
  theme_bw()

summary(lm(Sepal.Width ~ Sepal.Length + Species, iris))


# data.table --------------------------------------------------------------

library(data.table)

bookings <- fread('http://bit.ly/CEU-R-hotels-2018-prices')

bookings[weekend == 1, mean(price)]


# features ----------------------------------------------------------------

features <- fread('http://bit.ly/CEU-R-hotels-2018-features')

hotels <- merge(bookings, features, all.x = T)

hotels <- merge(
            bookings[, .(price = mean(price/nnights), bookings = .N), by = hotel_id],
            features,
            all.y = T
            )

hotels[, .(avg_price = weighted.mean(price, bookings)), by=stars]

countries <- hotels[, .(rating = mean(rating, na.rm=T)), by = 'country']

mean(countries$rating, na.rm = TRUE)

countries[rating > mean(rating, na.rm = T)]

df <- rbindlist(lapply(1:4, function(i) {
  data.frame(
    x = anscombe[, i],
    y = anscombe[, i + 4],
    set = i
  )
}))

ggplot(df, aes(x,y)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  facet_wrap( ~ set)

          