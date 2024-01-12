setwd("C:/Users/mattb/OneDrive/GitHub/oas-exercise-ratings")

library(dplyr)
library(tidyr)
library(ggplot2)
library(wesanderson)

df1 <- read.csv("data/FRED_OAS_data_raw.csv")


frame6 <- read.csv("data/cleaned_fred_oas_data.csv")
frame6 <- gather(frame6, "rating", "spread", -date)
frame6$spread <- as.numeric(frame6$spread)
frame6 <- frame6[complete.cases(frame6), ]
frame6$rating <- factor(frame6$rating)
levels(frame6$rating)[levels(frame6$rating) == "aaa"] <- "Aaa"
levels(frame6$rating)[levels(frame6$rating) == "aa"] <- "Aa2"
levels(frame6$rating)[levels(frame6$rating) == "a"] <- "A2"
levels(frame6$rating)[levels(frame6$rating) == "bbb"] <- "Baa2"
levels(frame6$rating)[levels(frame6$rating) == "bb"] <- "Ba2"
levels(frame6$rating)[levels(frame6$rating) == "b"] <- "B2"
levels(frame6$rating)[levels(frame6$rating) == "ccc"] <- "Caa2"
colors <- wes_palette("Zissou1", length(unique(frame6$rating)) + 1, type = "continuous")
fill_palette <- scale_fill_manual(values = colors)
color_palette <- scale_color_manual(values = colors)
ggplot(frame6) +
geom_line(aes(x = as.Date(date), y = spread, color = rating)) +
theme_minimal() +
color_palette +
  labs(y = "Spread (%)", x="") + 
    theme(legend.position="bottom") 
ggsave("plots/FIG1.jpeg",dpi=300, bg="white")




rc <- read.csv("data/rating_conversion_table.csv", header = FALSE)

colnames(df1) <- c(
    "DATE", 
    "Baa2",
    "Ca",
    "Aaa",
    "Aa2",
    "Ba2")

df1 <- as.data.frame(df1 %>% gather(
    rating, oas, Baa2:Ba2
    ) %>% dplyr::group_by(
        rating
    ) %>% dplyr::mutate(
        oas = as.numeric(oas),
        oas = median(oas, na.rm = TRUE)
    ) %>% dplyr::select(
        rating,
        oas
    ) %>% distinct(rating, oas, .keep_all = TRUE))

get_number_rating <- function(letter_rating){
    nr <- rc[rc$V1==letter_rating, ]$V2
    return (nr)
}

nr <- c()
for (i in df1$rating){
    nr <- append(nr, get_number_rating(i))
}

df1$nr <- nr

oas_poly <- lm(formula = oas ~ poly(nr, 3), data = df1)


ggplot(df1, aes(x = nr, y = oas)) +
stat_smooth(method="lm", se=FALSE,formula=y ~ poly(x, 3),colour="black") +
theme_minimal() + 
  labs(y = "Spread (%)", x="Numerical rating (Aaa = 58)")
ggsave("plots/FIG2.jpeg",dpi=300, bg="white")



# 'newdata' had 1 row but variables found have 5 rows 


get_estimate_poly <- function(rating){
    y <- predict(oas_poly, newdata = data.frame(nr = rating))
    return (y)}

get_rating <- function(a, b){
    return (29.749 + (9.458 * a) + (2.344 * a * b))
}


delta_PQ <- c(1, 2, -1, -2)
delta_ENV_1_2 <- c(1,2,1,2)

approx_rating <- get_rating(delta_PQ, delta_ENV_1_2)

oas_spread <- get_estimate_poly(approx_rating)

table1 <- data.frame(
    "change in PQI" = delta_PQ,
    "change in ENV 1.2" = delta_ENV_1_2,
    "Approx rating" = approx_rating,
    "oas_spread" = oas_spread
)
table1




