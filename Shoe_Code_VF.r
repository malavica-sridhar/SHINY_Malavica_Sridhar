library("dplyr")
library("stringr")
library("jsonlite")
library("purrr")
library("tidyr")
library("gridExtra")
library("ggplot2")

# Download and assign to women and men's datasets to appropriate dataframes 

mens_shoes <- read.csv(file = "7004_1.csv", header = TRUE, stringsAsFactors = FALSE)
womens_shoes <- read.csv(file = "WomenShoes.csv", header = TRUE, stringsAsFactors = FALSE)

# Filter the files based on interesting traits/colnames  

filtered_mens_shoes <- mens_shoes %>% select(., c("id", "brand", "categories", "colors", "features", "name", 
                                                  "prices_amountmin", "prices_amountmax", "prices_currency", 
                                                  "prices_merchant", "prices_size"))

filtered_womens_shoes <- womens_shoes %>% select(., c("id", "brand", "categories", "colors", "features", "name", 
                                                      "prices.amountMin", "prices.amountMax", "prices.currency", 
                                                      "prices.merchant", "prices.size"))

# Data Cleaning PART 1

features <- filtered_womens_shoes$features

#Pattern Feature

pattern_feature <- purrr::map(features, function(string) {
  tryCatch({
  df <- jsonlite::fromJSON(string, simplifyDataFrame = TRUE) %>% tidyr::spread(., key = key, value = value)
  if("pattern" %in% colnames(df)) colnames(df)[which(colnames(df)) == "pattern"] <- "Pattern"
  res <- df$Pattern[[1]]
  if (is.null(res)) res <- "No Pattern Available"
  res
  },
  error=function(cond) {
    return(NA)})
}
) %>% purrr::reduce(`rbind`)

pattern_feature <- as.data.frame(pattern_feature, row.names = 1:length(pattern_feature))
names(pattern_feature) <- "Pattern"
filtered_womens_shoes <- cbind(pattern_feature, filtered_womens_shoes)

#Material Feature

material_feature <- purrr::map(features, function(string) {
  tryCatch({
  df <- jsonlite::fromJSON(string, simplifyDataFrame = TRUE) %>% tidyr::spread(., key = key, value = value)
  colnames(df)[which(names(df) == "material")] <- "Material"
  res <- df$Material[[1]]
  if (is.null(res)) res <- "No Material Available"
  res
  },
  error=function(cond) {
    return(NA)})
}
) %>% purrr::reduce(`rbind`)

material_feature <- as.data.frame(material_feature, row.names = 1:length(material_feature))
names(material_feature) <- "Material"
filtered_womens_shoes <- cbind(material_feature, filtered_womens_shoes)

#Heel Height

heel_height <- purrr::map(features, function(string) {
  tryCatch({
  df <- jsonlite::fromJSON(string, simplifyDataFrame = TRUE) %>% tidyr::spread(., key = key, value = value)
  colnames(df)[which(names(df) == "Heel Height" | names(df) == "heel Height" | names(df) == "heel height")] <- "Heel"
  res <- df$Heel[[1]]
  if (is.null(res)) res <- "No Heel Available"
  res
  },
  error=function(cond) {
    return(NA)})
}
) %>% purrr::reduce(`rbind`)

heel_height <- as.data.frame(heel_height, row.names = 1:length(heel_height))
names(heel_height) <- "Heel"
filtered_womens_shoes <- cbind(heel_height, filtered_womens_shoes)

#Boot Height

boot_height <- purrr::map(features, function(string) {
  tryCatch({
    df <- jsonlite::fromJSON(string, simplifyDataFrame = TRUE) %>% tidyr::spread(., key = key, value = value)
    colnames(df)[which(names(df) == "Boot Height" | names(df) == "boot Height" | names(df) == "boot height")] <- "Boot"
    res <- df$Boot[[1]]
    if (is.null(res)) res <- "No Boot Available"
    res
  },
  error=function(cond) {
    return(NA)})
}
) %>% purrr::reduce(`rbind`)

boot_height <- as.data.frame(boot_height, row.names = 1:length(boot_height))
names(boot_height) <- "Boot"
filtered_womens_shoes <- cbind(boot_height, filtered_womens_shoes)

#Country

country_mat <- purrr::map(features, function(string) {
  tryCatch({
    df <- jsonlite::fromJSON(string, simplifyDataFrame = TRUE) %>% tidyr::spread(., key = key, value = value)
    colnames(df)[which(names(df) == "Country of Origin" | names(df) == "Worldstock Country" | names(df) == "country" | names(df) == "Origin of Components")] <- "Country"
    res <- df$Country[[1]]
    if (is.null(res)) res <- "No Country Available"
    res
  },
  error=function(cond) {
    return(NA)})
}
) %>% purrr::reduce(`rbind`)

origin_country <- as.data.frame(country_mat, row.names = 1:length(country_mat))
names(origin_country) <- "Country"
filtered_womens_shoes <- cbind(origin_country, filtered_womens_shoes)

#Style

style_matrix <- purrr::map(features, function(string) {
  tryCatch({
    df <- jsonlite::fromJSON(string, simplifyDataFrame = TRUE) %>% tidyr::spread(., key = key, value = value)
    colnames(df)[which(names(df) == "style")] <- "Style"
    res <- df$Style[[1]]
    if (is.null(res)) res <- "No Style Available"
    res
  },
  error=function(cond) {
    return(NA)})
}
) %>% purrr::reduce(`rbind`)

style_type <- as.data.frame(style_matrix, row.names = 1:length(style_matrix))
names(style_type) <- "Style"
filtered_womens_shoes <- cbind(style_type, filtered_womens_shoes)

## Men's Data Cleaning

mens_features <- filtered_mens_shoes$features

#Pattern Feature

pattern_feature_mens <- purrr::map(mens_features, function(string) {
  tryCatch({
    df <- jsonlite::fromJSON(string, simplifyDataFrame = TRUE) %>% tidyr::spread(., key = key, value = value)
    if("pattern" %in% colnames(df)) colnames(df)[which(colnames(df)) == "pattern"] <- "Pattern"
    res <- df$Pattern[[1]]
    if (is.null(res)) res <- "No Pattern Available"
    res
  },
  error=function(cond) {
    return(NA)})
}
) %>% purrr::reduce(`rbind`)

pattern_feature_mens <- as.data.frame(pattern_feature_mens, row.names = 1:length(pattern_feature_mens))
names(pattern_feature_mens) <- "Pattern"
filtered_mens_shoes <- cbind(pattern_feature_mens, filtered_mens_shoes)

#Material Feature

material_feature_mens <- purrr::map(mens_features, function(string) {
  tryCatch({
    df <- jsonlite::fromJSON(string, simplifyDataFrame = TRUE) %>% tidyr::spread(., key = key, value = value)
    colnames(df)[which(names(df) == "material")] <- "Material"
    res <- df$Material[[1]]
    if (is.null(res)) res <- "No Material Available"
    res
  },
  error=function(cond) {
    return(NA)})
}
) %>% purrr::reduce(`rbind`)

material_feature_mens <- as.data.frame(material_feature_mens, row.names = 1:length(material_feature_mens))
names(material_feature_mens) <- "Material"
filtered_mens_shoes <- cbind(material_feature_mens, filtered_mens_shoes)

#Heel Height

heel_height_mens <- purrr::map(mens_features, function(string) {
  tryCatch({
    df <- jsonlite::fromJSON(string, simplifyDataFrame = TRUE) %>% tidyr::spread(., key = key, value = value)
    colnames(df)[which(names(df) == "Heel Height" | names(df) == "heel Height" | names(df) == "heel height")] <- "Heel"
    res <- df$Heel[[1]]
    if (is.null(res)) res <- "No Heel Available"
    res
  },
  error=function(cond) {
    return(NA)})
}
) %>% purrr::reduce(`rbind`)

heel_height_mens <- as.data.frame(heel_height_mens, row.names = 1:length(heel_height_mens))
names(heel_height_mens) <- "Heel"
filtered_mens_shoes <- cbind(heel_height_mens, filtered_mens_shoes)

## Boot Height

boot_height_mens <- purrr::map(mens_features, function(string) {
  tryCatch({
    df <- jsonlite::fromJSON(string, simplifyDataFrame = TRUE) %>% tidyr::spread(., key = key, value = value)
    colnames(df)[which(names(df) == "Boot Height" | names(df) == "boot Height" | names(df) == "boot height")] <- "Boot"
    res <- df$Boot[[1]]
    if (is.null(res)) res <- "No Boot Available"
    res
  },
  error=function(cond) {
    return(NA)})
}
) %>% purrr::reduce(`rbind`)

boot_height_mens <- as.data.frame(boot_height_mens, row.names = 1:length(boot_height_mens))
names(boot_height_mens) <- "Boot"
filtered_mens_shoes <- cbind(boot_height_mens, filtered_mens_shoes)

#Country

country_mat_mens <- purrr::map(mens_features, function(string) {
  tryCatch({
    df <- jsonlite::fromJSON(string, simplifyDataFrame = TRUE) %>% tidyr::spread(., key = key, value = value)
    colnames(df)[which(names(df) == "Country of Origin" | names(df) == "Worldstock Country" | names(df) == "country" | names(df) == "Origin of Components")] <- "Country"
    res <- df$Country[[1]]
    if (is.null(res)) res <- "No Country Available"
    res
  },
  error=function(cond) {
    return(NA)})
}
) %>% purrr::reduce(`rbind`)

country_mat_mens <- as.data.frame(country_mat_mens, row.names = 1:length(country_mat_mens))
names(country_mat_mens) <- "Country"
filtered_mens_shoes <- cbind(country_mat_mens, filtered_mens_shoes)

#Style

style_matrix_mens <- purrr::map(mens_features, function(string) {
  tryCatch({
    df <- jsonlite::fromJSON(string, simplifyDataFrame = TRUE) %>% tidyr::spread(., key = key, value = value)
    colnames(df)[which(names(df) == "style")] <- "Style"
    res <- df$Style[[1]]
    if (is.null(res)) res <- "No Style Available"
    res
  },
  error=function(cond) {
    return(NA)})
}
) %>% purrr::reduce(`rbind`)

style_matrix_mens <- as.data.frame(style_matrix_mens, row.names = 1:length(style_matrix_mens))
names(style_matrix_mens) <- "Style"
filtered_mens_shoes <- cbind(style_matrix_mens, filtered_mens_shoes)

# Data Cleaning PART 2

women <- filtered_womens_shoes %>% mutate(., Sex = "Women")
men <- filtered_mens_shoes %>% mutate(., Sex = "Men")

women <- women[ ,c(7,8,1,2,3,4,5,6,10,13,14,15,16,17,18)]
men <- men[ , c(7,8,1,2,3,4,5,6,10,13,14,15,16,17,18)]

# First Exploratory Graph

two_dim <- women %>% select(., c("brand", "prices.amountMax"))
two_dim <- two_dim[two_dim$brand != "", ]

two_dim_men <- men %>% select(., c("brand", "prices_amountmax"))
two_dim_men <- two_dim_men[two_dim_men$brand != "", ]

# Adding Tiers: Non-Luxury, Everyday Luxury, and Luxury Shoes

two_dim <- two_dim %>% mutate(., Tier = ifelse(prices.amountMax <= 100, "Non-Luxury", 
                                               ifelse(prices.amountMax >= 200, "Luxury", "Everyday Luxury")))

two_dim_men <- two_dim_men %>% mutate(., Tier = ifelse(prices_amountmax <= 100, "Non-Luxury", 
                                                   ifelse(prices_amountmax >= 200, "Luxury", "Everyday Luxury")))

two_dim_men$prices_amountmax <- as.numeric(two_dim_men$prices_amountmax)

# -- Average price per brand

summary_prices <- two_dim %>% group_by(., brand) %>% summarise(., Average_by_Brand = mean(prices.amountMax))
summary_prices <- summary_prices %>% mutate(., Tier = ifelse(Average_by_Brand <= 100, "Non-Luxury", 
                                                      ifelse(Average_by_Brand >= 200, "Luxury", "Everyday Luxury")))

summary_prices_men <- two_dim_men %>% group_by(., brand) %>% summarise(., Average_by_Brand = mean(prices_amountmax))
summary_prices_men <- summary_prices_men %>% mutate(., Tier = ifelse(Average_by_Brand <= 100, "Non-Luxury", 
                                                             ifelse(Average_by_Brand >= 200, "Luxury", "Everyday Luxury")))

# GRAPH 1: Graphing by Tier

g <- ggplot(data = two_dim, aes(x = prices.amountMax))
g + geom_histogram(aes(fill = Tier), binwidth = 10) + xlim(c(0,800))

# -- GRAPH 1.B Average price per brand

# ---Women

g <- ggplot(data = summary_prices, aes(x = Average_by_Brand))
g + geom_histogram(aes(fill = Tier), binwidth = 10) + xlim(c(0,800)) + xlab("Average Price by Brand") + ylab("Count")

# ---Men 

g_men <- ggplot(data = summary_prices_men, aes(x = Average_by_Brand))
g_men + geom_histogram(aes(fill = Tier), binwidth = 10) + xlim(c(0,800)) + xlab("Average Price by Brand") + ylab("Count")

# GRAPH 2: Graphing by Tier

# Non-Luxury Shoes - WOMEN

Non_Luxury_Shoes <- ggplot(subset(summary_prices, Tier == "Non-Luxury"), aes(x = Average_by_Brand)) + 
  geom_freqpoly(aes(color = Tier), binwidth = 10) + xlab("Average Price") + ylab("Count") + 
  theme(legend.position="none") + ggtitle("Non-Luxury Shoes")

Everyday_Shoes <- ggplot(subset(summary_prices, Tier == "Everyday Luxury"), aes(x = Average_by_Brand)) + 
  geom_freqpoly(aes(color = Tier), binwidth = 10) + xlab("Average Price") + ylab("Count") + 
  theme(legend.position="none") + ggtitle("Everyday Luxury Shoes")

Luxury_Shoes <- ggplot(subset(summary_prices, Tier == "Luxury"), aes(x = Average_by_Brand)) + 
  geom_freqpoly(aes(color = Tier), binwidth = 100) + xlab("Average Price") + ylab("Count") + 
  theme(legend.position="none") + ggtitle("Luxury Shoes") + xlim(c(100,4500))

grid.arrange(Non_Luxury_Shoes, Everyday_Shoes, Luxury_Shoes)

# Non-Luxury Shoes - MEN

Non_Luxury_Shoes_Men <- ggplot(subset(summary_prices_men, Tier == "Non-Luxury"), aes(x = Average_by_Brand)) + 
  geom_freqpoly(aes(color = Tier), binwidth = 10) + xlab("Average Price") + ylab("Count") + 
  theme(legend.position="none") + ggtitle("Non-Luxury Shoes")

Everyday_Shoes_Men <- ggplot(subset(summary_prices_men, Tier == "Everyday Luxury"), aes(x = Average_by_Brand)) + 
  geom_freqpoly(aes(color = Tier), binwidth = 10) + xlab("Average Price") + ylab("Count") + 
  theme(legend.position="none") + ggtitle("Everyday Luxury Shoes")

Luxury_Shoes_Men <- ggplot(subset(summary_prices_men, Tier == "Luxury"), aes(x = Average_by_Brand)) + 
  geom_freqpoly(aes(color = Tier), binwidth = 100) + xlab("Average Price") + ylab("Count") + 
  theme(legend.position="none") + ggtitle("Luxury Shoes") + xlim(c(100,1500))

grid.arrange(Non_Luxury_Shoes_Men, Everyday_Shoes_Men, Luxury_Shoes_Men)

# A Look into Individual Shoe Brands and their Pricing Strategies 

# WOMEN -- Highest Count

summary_individual_prices <- two_dim %>% group_by(., Tier, brand) %>% summarise(., Total = n()) %>% arrange(., desc(Total))
non_luxury_summary <- summary_individual_prices %>% filter(., Tier == "Non-Luxury")
everydaylux_summary <- summary_individual_prices %>% filter(., Tier == "Everyday Luxury")
luxury_summary <- summary_individual_prices %>% filter(., Tier == "Luxury")

# MEN -- Highest Count

summary_individual_prices_men <- two_dim_men %>% group_by(., Tier, brand) %>% summarise(., Total = n()) %>% arrange(., desc(Total))
non_luxury_summary_men <- summary_individual_prices_men %>% filter(., Tier == "Non-Luxury")
everydaylux_summary_men <- summary_individual_prices_men %>% filter(., Tier == "Everyday Luxury")
luxury_summary_men <- summary_individual_prices_men %>% filter(., Tier == "Luxury")

# Non-Luxury Shoes 

nl1 <- two_dim %>% filter(., brand == "In-Sattva") %>% arrange(., prices.amountMax) %>%  filter (., prices.amountMax < 100)
nl2 <- two_dim %>% filter(., brand == "TOMS") %>% arrange(., prices.amountMax) %>%  filter (., prices.amountMax < 100)
nl3 <- two_dim %>% filter(., brand == "Skechers") %>% arrange(., prices.amountMax) %>%  filter (., prices.amountMax < 100)
nl4 <- two_dim %>% filter(., brand == "Hadari") %>% arrange(., prices.amountMax) %>%  filter (., prices.amountMax < 100)
nl5 <- two_dim %>% filter(., brand == "Easy Spirit") %>% arrange(., prices.amountMax) %>%  filter (., prices.amountMax < 100)
nl6 <- two_dim %>% filter(., brand == "Nina") %>% arrange(., prices.amountMax) %>%  filter (., prices.amountMax < 100)
nl7 <- two_dim %>% filter(., brand == "Nature Breeze") %>% arrange(., prices.amountMax) %>%  filter (., prices.amountMax < 100)
nl8 <- two_dim %>% filter(., brand == "New Balance") %>% arrange(., prices.amountMax) %>%  filter (., prices.amountMax < 100)
nl9 <- two_dim %>% filter(., brand == "G BY GUESS") %>% arrange(., prices.amountMax) %>%  filter (., prices.amountMax < 100)

nl1_graph <- ggplot(data = nl1, aes(x = prices.amountMax)) + geom_density(aes(color = brand)) + 
  xlab(" ") + ylab(" ") + theme(legend.position="none",axis.text.y=element_blank()) + 
  ggtitle("In-Sattva")

nl2_graph <- ggplot(data = nl2, aes(x = prices.amountMax)) + geom_density(aes(color = brand)) + 
  xlab(" ") + ylab(" ") + theme(legend.position="none",axis.text.y=element_blank()) + 
  ggtitle("TOMS")

nl3_graph <- ggplot(data = nl3, aes(x = prices.amountMax)) + geom_density(aes(color = brand)) + 
  xlab(" ") + ylab(" ") + theme(legend.position="none",axis.text.y=element_blank()) + 
  ggtitle("Skechers")

nl4_graph <- ggplot(data = nl4, aes(x = prices.amountMax)) + geom_density(aes(color = brand)) + 
  xlab(" ") + ylab(" ") + theme(legend.position="none",axis.text.y=element_blank()) + 
  ggtitle("Hadari")

nl5_graph <- ggplot(data = nl5, aes(x = prices.amountMax)) + geom_density(aes(color = brand)) + 
  xlab(" ") + ylab(" ") + theme(legend.position="none",axis.text.y=element_blank()) + 
  ggtitle("Easy Spirit")

nl6_graph <- ggplot(data = nl6, aes(x = prices.amountMax)) + geom_density(aes(color = brand)) + 
  xlab(" ") + ylab(" ") + theme(legend.position="none",axis.text.y=element_blank()) + 
  ggtitle("Nina")

nl7_graph <- ggplot(data = nl7, aes(x = prices.amountMax)) + geom_density(aes(color = brand)) + 
  xlab("Price Distribution") + ylab(" ") + theme(legend.position="none",axis.text.y=element_blank()) + 
  ggtitle("Nature Breeze")

nl8_graph <- ggplot(data = nl8, aes(x = prices.amountMax)) + geom_density(aes(color = brand)) + 
  xlab("Price Distribution") + ylab(" ") + theme(legend.position="none",axis.text.y=element_blank()) + 
  ggtitle("New Balance")

nl9_graph <- ggplot(data = nl9, aes(x = prices.amountMax)) + geom_density(aes(color = brand)) + 
  xlab("Price Distribution") + ylab(" ") + theme(legend.position="none",axis.text.y=element_blank()) + 
  ggtitle("G BY GUESS")

non_luxury_grid <- grid.arrange(nl1_graph, nl2_graph, nl3_graph, 
                                nl4_graph, nl5_graph, nl6_graph, 
                                nl7_graph, nl8_graph, nl9_graph)

# Everyday Luxury Shoes 

el1 <- two_dim %>% filter(., brand == "Gabor") %>% arrange(., prices.amountMax) %>%  filter (., prices.amountMax < 201 & prices.amountMax > 100)
el2 <- two_dim %>% filter(., brand == "Kenneth Cole New York") %>% arrange(., prices.amountMax) %>%  filter (., prices.amountMax < 201 & prices.amountMax > 100)
el3 <- two_dim %>% filter(., brand == "Nine West") %>% arrange(., prices.amountMax) %>%  filter (., prices.amountMax < 201 & prices.amountMax > 100)
el4 <- two_dim %>% filter(., brand == "Altra Footwear") %>% arrange(., prices.amountMax) %>%  filter (., prices.amountMax < 201 & prices.amountMax > 100)
el5 <- two_dim %>% filter(., brand == "UGG") %>% arrange(., prices.amountMax) %>%  filter (., prices.amountMax < 201 & prices.amountMax > 100)
el6 <- two_dim %>% filter(., brand == "Drew") %>% arrange(., prices.amountMax) %>%  filter (., prices.amountMax < 201 & prices.amountMax > 100)
el7 <- two_dim %>% filter(., brand == "Vaneli") %>% arrange(., prices.amountMax) %>%  filter (., prices.amountMax < 201 & prices.amountMax > 100)
el8 <- two_dim %>% filter(., brand == "Ros Hommerson") %>% arrange(., prices.amountMax) %>%  filter (., prices.amountMax < 201 & prices.amountMax > 100)
el9 <- two_dim %>% filter(., brand == "David Tate") %>% arrange(., prices.amountMax) %>%  filter (., prices.amountMax < 201 & prices.amountMax > 100)

el1_graph <- ggplot(data = el1, aes(x = prices.amountMax)) + geom_density(aes(color = brand)) + 
  xlab(" ") + ylab(" ") + theme(legend.position="none",axis.text.y=element_blank()) + 
  ggtitle("Gabor")

el2_graph <- ggplot(data = el2, aes(x = prices.amountMax)) + geom_density(aes(color = brand)) + 
  xlab(" ") + ylab(" ") + theme(legend.position="none",axis.text.y=element_blank()) + 
  ggtitle("Kenneth Cole New York")

el3_graph <- ggplot(data = el3, aes(x = prices.amountMax)) + geom_density(aes(color = brand)) + 
  xlab(" ") + ylab(" ") + theme(legend.position="none",axis.text.y=element_blank()) + 
  ggtitle("Nine West")

el4_graph <- ggplot(data = el4, aes(x = prices.amountMax)) + geom_density(aes(color = brand)) + 
  xlab(" ") + ylab(" ") + theme(legend.position="none",axis.text.y=element_blank()) + 
  ggtitle("Altra Footwear")

el5_graph <- ggplot(data = el5, aes(x = prices.amountMax)) + geom_density(aes(color = brand)) + 
  xlab(" ") + ylab(" ") + theme(legend.position="none",axis.text.y=element_blank()) + 
  ggtitle("UGG")

el6_graph <- ggplot(data = el6, aes(x = prices.amountMax)) + geom_density(aes(color = brand)) + 
  xlab(" ") + ylab(" ") + theme(legend.position="none",axis.text.y=element_blank()) + 
  ggtitle("Drew")

el7_graph <- ggplot(data = el7, aes(x = prices.amountMax)) + geom_density(aes(color = brand)) + 
  xlab("Price Distribution") + ylab(" ") + theme(legend.position="none",axis.text.y=element_blank()) + 
  ggtitle("Vaneli")

el8_graph <- ggplot(data = el8, aes(x = prices.amountMax)) + geom_density(aes(color = brand)) + 
  xlab("Price Distribution") + ylab(" ") + theme(legend.position="none",axis.text.y=element_blank()) + 
  ggtitle("Ros Hommerson")

el9_graph <- ggplot(data = el9, aes(x = prices.amountMax)) + geom_density(aes(color = brand)) + 
  xlab("Price Distribution") + ylab(" ") + theme(legend.position="none",axis.text.y=element_blank()) + 
  ggtitle("David Tate")

everyday_luxury_grid <- grid.arrange(el1_graph, el2_graph, el3_graph, 
                                    el4_graph, el5_graph, el6_graph, 
                                    el7_graph, el8_graph, el9_graph)

# Luxury Shoes 

l1 <- two_dim %>% filter(., brand == "Ralph Lauren") %>% arrange(., prices.amountMax) %>%  filter (., prices.amountMax > 200)
l2 <- two_dim %>% filter(., brand == "Diane von Furstenberg") %>% arrange(., prices.amountMax) %>%  filter (., prices.amountMax > 200)
l3 <- two_dim %>% filter(., brand == "Gucci") %>% arrange(., prices.amountMax) %>%  filter (., prices.amountMax > 200)
l4 <- two_dim %>% filter(., brand == "Valentino") %>% arrange(., prices.amountMax) %>%  filter (., prices.amountMax > 200)
l5 <- two_dim %>% filter(., brand == "Ara") %>% arrange(., prices.amountMax) %>%  filter (., prices.amountMax > 200)
l6 <- two_dim %>% filter(., brand == "Boulet") %>% arrange(., prices.amountMax) %>%  filter (., prices.amountMax > 200)
l7 <- two_dim %>% filter(., brand == "Jimmy Choo") %>% arrange(., prices.amountMax) %>%  filter (., prices.amountMax > 200)
l8 <- two_dim %>% filter(., brand == "Prada") %>% arrange(., prices.amountMax) %>%  filter (., prices.amountMax > 200)
l9 <- two_dim %>% filter(., brand == "Christian Louboutin") %>% arrange(., prices.amountMax) %>%  filter (., prices.amountMax > 200)

l1_graph <- ggplot(data = l1, aes(x = prices.amountMax)) + geom_density(aes(color = brand)) + 
  xlab(" ") + ylab(" ") + theme(legend.position="none",axis.text.y=element_blank()) + 
  ggtitle("Ralph Lauren")

l2_graph <- ggplot(data = l2, aes(x = prices.amountMax)) + geom_density(aes(color = brand)) + 
  xlab(" ") + ylab(" ") + theme(legend.position="none",axis.text.y=element_blank()) + 
  ggtitle("Diane von Furstenberg")

l3_graph <- ggplot(data = l3, aes(x = prices.amountMax)) + geom_density(aes(color = brand)) + 
  xlab(" ") + ylab(" ") + theme(legend.position="none",axis.text.y=element_blank()) + 
  ggtitle("Gucci")

l4_graph <- ggplot(data = l4, aes(x = prices.amountMax)) + geom_density(aes(color = brand)) + 
  xlab(" ") + ylab(" ") + theme(legend.position="none",axis.text.y=element_blank()) + 
  ggtitle("Valentino")

l5_graph <- ggplot(data = l5, aes(x = prices.amountMax)) + geom_density(aes(color = brand)) + 
  xlab(" ") + ylab(" ") + theme(legend.position="none",axis.text.y=element_blank()) + 
  ggtitle("Ara")

l6_graph <- ggplot(data = l6, aes(x = prices.amountMax)) + geom_density(aes(color = brand)) + 
  xlab(" ") + ylab(" ") + theme(legend.position="none",axis.text.y=element_blank()) + 
  ggtitle("Boulet")

l7_graph <- ggplot(data = l7, aes(x = prices.amountMax)) + geom_density(aes(color = brand)) + 
  xlab("Price Distribution") + ylab(" ") + theme(legend.position="none",axis.text.y=element_blank()) + 
  ggtitle("Jimmy Choo")

l8_graph <- ggplot(data = l8, aes(x = prices.amountMax)) + geom_density(aes(color = brand)) + 
  xlab("Price Distribution") + ylab(" ") + theme(legend.position="none",axis.text.y=element_blank()) + 
  ggtitle("Prada")

l9_graph <- ggplot(data = l9, aes(x = prices.amountMax)) + geom_density(aes(color = brand)) + 
  xlab("Price Distribution") + ylab(" ") + theme(legend.position="none",axis.text.y=element_blank()) + 
  ggtitle("Christian Louboutin")

luxury_grid <- grid.arrange(l1_graph, l2_graph, l3_graph, 
                            l4_graph, l5_graph, l6_graph, 
                            l7_graph, l8_graph, l9_graph)

# MEN: Grids on Pricing Strategies

mel1 <- two_dim_men %>% filter(., brand == "adidas") %>% arrange(., prices_amountmax) %>%  filter (., prices_amountmax <= 200 & prices_amountmax >= 100)
mel2 <- two_dim_men %>% filter(., brand == "Oakley") %>% arrange(., prices_amountmax) %>%  filter (., prices_amountmax <= 200 & prices_amountmax >= 100)
mel3 <- two_dim_men %>% filter(., brand == "Georgia Boot") %>% arrange(., prices_amountmax) %>%  filter (., prices_amountmax <= 200 & prices_amountmax >= 100)
mel4 <- two_dim_men %>% filter(., brand == "Reebok") %>% arrange(., prices_amountmax) %>%  filter (., prices_amountmax <= 200 & prices_amountmax >= 100)
mel5 <- two_dim_men %>% filter(., brand == "Carrera") %>% arrange(., prices_amountmax) %>%  filter (., prices_amountmax <= 200 & prices_amountmax >= 100)
mel6 <- two_dim_men %>% filter(., brand == "ASICS") %>% arrange(., prices_amountmax) %>%  filter (., prices_amountmax <= 200 & prices_amountmax >= 100)
mel7 <- two_dim_men %>% filter(., brand == "Timberland") %>% arrange(., prices_amountmax) %>%  filter (., prices_amountmax <= 200 & prices_amountmax >= 100)
mel8 <- two_dim_men %>% filter(., brand == "Cole Haan") %>% arrange(., prices_amountmax) %>%  filter (., prices_amountmax <= 200 & prices_amountmax >= 100)
mel9 <- two_dim_men %>% filter(., brand == "Justin") %>% arrange(., prices_amountmax) %>%  filter (., prices_amountmax <= 200 & prices_amountmax >= 100)

mel1_graph <- ggplot(data = mel1, aes(x = prices_amountmax)) + geom_density(aes(color = brand)) + 
  xlab(" ") + ylab(" ") + theme(legend.position="none", axis.text.y=element_blank()) + ggtitle("Adidas")

mel2_graph <- ggplot(data = mel2, aes(x = prices_amountmax)) + geom_density(aes(color = brand)) + 
  xlab(" ") + ylab(" ") + theme(legend.position="none", axis.text.y=element_blank()) + ggtitle("Oakley")

mel3_graph <- ggplot(data = mel3, aes(x = prices_amountmax)) + geom_density(aes(color = brand)) + 
  xlab(" ") + ylab(" ") + theme(legend.position="none", axis.text.y=element_blank()) + ggtitle("Georgia Boot")

mel4_graph <- ggplot(data = mel4, aes(x = prices_amountmax)) + geom_density(aes(color = brand)) + 
  xlab(" ") + ylab(" ") + theme(legend.position="none", axis.text.y=element_blank()) + ggtitle("Reebok")

mel5_graph <- ggplot(data = mel5, aes(x = prices_amountmax)) + geom_density(aes(color = brand)) + 
  xlab(" ") + ylab(" ") + theme(legend.position="none", axis.text.y=element_blank()) + ggtitle("Carrera")

mel6_graph <- ggplot(data = mel6, aes(x = prices_amountmax)) + geom_density(aes(color = brand)) + 
  xlab(" ") + ylab(" ") + theme(legend.position="none", axis.text.y=element_blank()) + ggtitle("ASICS")

mel7_graph <- ggplot(data = mel7, aes(x = prices_amountmax)) + geom_density(aes(color = brand)) + 
  xlab("Price Distribution") + ylab(" ") + theme(legend.position="none", axis.text.y=element_blank()) + ggtitle("Timberland")

mel8_graph <- ggplot(data = mel8, aes(x = prices_amountmax)) + geom_density(aes(color = brand)) + 
  xlab("Price Distribution") + ylab(" ") + theme(legend.position="none", axis.text.y=element_blank()) + ggtitle("Cole Haan")

mel9_graph <- ggplot(data = mel9, aes(x = prices_amountmax)) + geom_density(aes(color = brand)) + 
  xlab("Price Distribution") + ylab(" ") + theme(legend.position="none", axis.text.y=element_blank()) + ggtitle("Justin")

everydaylux_grid_men <- grid.arrange(mel1_graph, mel2_graph, mel3_graph, 
                                     mel4_graph, mel5_graph, mel6_graph,
                                     mel7_graph, mel8_graph, mel9_graph)

# Luxury Men Shoes: Pricing Strategies

ml1 <- two_dim_men %>% filter(., brand == "Ralph Lauren") %>% arrange(., prices_amountmax) %>%  filter (., prices_amountmax >= 200)
ml2 <- two_dim_men %>% filter(., brand == "Boss Hugo Boss") %>% arrange(., prices_amountmax) %>%  filter (., prices_amountmax >= 200)
ml3 <- two_dim_men %>% filter(., brand == "BURBERRY") %>% arrange(., prices_amountmax) %>%  filter (., prices_amountmax >= 200)
ml4 <- two_dim_men %>% filter(., brand == "Tag Heuer") %>% arrange(., prices_amountmax) %>%  filter (., prices_amountmax >= 200)
ml5 <- two_dim_men %>% filter(., brand == "Salvatore Ferragamo") %>% arrange(., prices_amountmax) %>%  filter (., prices_amountmax >= 200)
ml6 <- two_dim_men %>% filter(., brand == "Gucci") %>% arrange(., prices_amountmax) %>%  filter (., prices_amountmax >= 200)
ml7 <- two_dim_men %>% filter(., brand == "Lucchese") %>% arrange(., prices_amountmax) %>%  filter (., prices_amountmax >= 200)
ml8 <- two_dim_men %>% filter(., brand == "Ferrari") %>% arrange(., prices_amountmax) %>%  filter (., prices_amountmax >= 200)
ml9 <- two_dim_men %>% filter(., brand == "Prada Sport") %>% arrange(., prices_amountmax) %>%  filter (., prices_amountmax >= 200)

ml1_graph <- ggplot(data = ml1, aes(x = prices_amountmax)) + geom_density(aes(color = brand)) + 
  xlab(" ") + ylab(" ") + theme(legend.position="none", axis.text.y=element_blank()) + ggtitle("Ralph Lauren")

ml2_graph <- ggplot(data = ml2, aes(x = prices_amountmax)) + geom_density(aes(color = brand)) + 
  xlab(" ") + ylab(" ") + theme(legend.position="none", axis.text.y=element_blank()) + ggtitle("HUGO Boss")

ml3_graph <- ggplot(data = ml3, aes(x = prices_amountmax)) + geom_density(aes(color = brand)) + 
  xlab(" ") + ylab(" ") + theme(legend.position="none", axis.text.y=element_blank()) + ggtitle("Burberry")

ml4_graph <- ggplot(data = ml4, aes(x = prices_amountmax)) + geom_density(aes(color = brand)) + 
  xlab(" ") + ylab(" ") + theme(legend.position="none", axis.text.y=element_blank()) + ggtitle("Tag Heuer")

ml5_graph <- ggplot(data = ml5, aes(x = prices_amountmax)) + geom_density(aes(color = brand)) + 
  xlab(" ") + ylab(" ") + theme(legend.position="none", axis.text.y=element_blank()) + ggtitle("Salvatore Ferragamo")

ml6_graph <- ggplot(data = ml6, aes(x = prices_amountmax)) + geom_density(aes(color = brand)) + 
  xlab(" ") + ylab(" ") + theme(legend.position="none", axis.text.y=element_blank()) + ggtitle("Gucci")

ml7_graph <- ggplot(data = ml7, aes(x = prices_amountmax)) + geom_density(aes(color = brand)) + 
  xlab("Price Distribution") + ylab(" ") + theme(legend.position="none", axis.text.y=element_blank()) + ggtitle("Lucchese")

ml8_graph <- ggplot(data = ml8, aes(x = prices_amountmax)) + geom_density(aes(color = brand)) + 
  xlab("Price Distribution") + ylab(" ") + theme(legend.position="none", axis.text.y=element_blank()) + ggtitle("Ferrari")

ml9_graph <- ggplot(data = ml9, aes(x = prices_amountmax)) + geom_density(aes(color = brand)) + 
  xlab("Price Distribution") + ylab(" ") + theme(legend.position="none", axis.text.y=element_blank()) + ggtitle("Prada Sport")

luxury_grid_men <- grid.arrange(ml1_graph, ml2_graph, ml3_graph, 
                                     ml4_graph, ml5_graph, ml6_graph,
                                     ml7_graph, ml8_graph, ml9_graph)

# COMBINED: Matching Column Names for Top 5 Patterns

combined <- rbind(women, men)
top_patterns <- combined %>% filter(., Pattern == "Solid" | Pattern == "Floral" | Pattern == "Multi-Colored"
                                    | Pattern == "Stripe" | Pattern == "Snake/Reptile")

top_patterns$prices.amountMax <- as.numeric(top_patterns$prices.amountMax)

# Pattern Graph

pattern_box <- ggplot(data = top_patterns, aes(x = reorder(Pattern, prices.amountMax), y = prices.amountMax)) + 
  geom_boxplot() + ylim(c(0,200)) + xlab("Pattern") + ylab("Price")

# Material Graph - SPLIT BETWEEN MEN AND WOMEN 

men_top_material <- men %>% filter(., Material == "Leather" | Material == "Synthetic" | Material == "Canvas" |
                                     Material == "Suede" | Material == "Rubber" | Material == "Polyester" |
                                     Material == "100% Cotton" | Material == "Mesh" | Material == "Multi" |
                                     Material == "PVC" | Material == "Cotton blend" | Material == "Fabric" |
                                     Material == "Synthetics" | Material == "Synethic-And-Mesh" | Material == "Denim")
                                    

men_top_material$prices.amountMax <- as.numeric(men_top_material$prices.amountMax)

men_material_graph <- ggplot(data = men_top_material, aes(x = reorder(Material, prices.amountMax), y = prices.amountMax)) + 
  geom_boxplot() + xlab("Material") + ylab("Price") + ylim(c(0,500))

women_top_material <- women %>% filter(., Material == "Leather" | Material == "Synthetic" | Material == "Faux Leather" |
                                         Material == "Canvas" | Material == "Suede" | Material == "Faux Suede" |
                                         Material == "Fabric" | Material == "EVA" | Material == "Rubber" |
                                         Material == "Lace" | Material == "Satin" | Material == "Polyurethane" |
                                         Material == "Cotton" | Material == "Mesh" | Material == "Polyester")

women_top_material$prices.amountMax <- as.numeric(women_top_material$prices.amountMax)

women_material_graph <- ggplot(data = women_top_material, aes(x = reorder(Material, prices.amountMax), y = prices.amountMax)) +
  geom_boxplot() + xlab("Material") + ylab("Price") + ylim(c(0,500))

# Color Analysis: Men

men_top_color <- men %>% filter(., colors == "Black" | colors == "Brown" | colors == "Multicolor" | colors == "Blue" | colors == "Gray" |
                                  colors == "White" | colors == "Red" | colors == "Tan" | colors == "Yellow" | colors == "Green" |
                                  colors == "Beige" | colors == "Navy" | colors == "Orange" | colors == "Dark Brown" | colors == "Silver")

men_top_color$colors <- as.factor(men_top_color$colors)
men_top_color$prices.amountMax <- as.numeric(men_top_color$prices.amountMax)

men_color_graph <- ggplot(data = men_top_color, aes(x = reorder(colors, prices.amountMax), y = prices.amountMax)) + 
  geom_boxplot() + xlab("Colors") + ylab("Price") + ylim(c(0,400))

# Color Analysis: Women 

women_top_color <- women %>% filter(., colors == "Black" | colors == "Brown" | colors == "White" | colors == "Blue" | 
                                      colors == "Gray" | colors == "Silver" | colors == "Beige" | colors == "Red" |
                                      colors == "Pink" | colors == "Green" | colors == "Tan" | colors == "Orange" |
                                      colors == "Gold" | colors == "Yellow" | colors == "Purple")

women_top_color$colors <- as.factor(women_top_color$colors)
women_top_color$prices.amountMax <- as.numeric(women_top_color$prices.amountMax)

women_color_graph <- ggplot(data = women_top_color, aes(x = reorder(colors, prices.amountMax), y = prices.amountMax)) + 
  geom_boxplot() + xlab("Colors") + ylab("Price") + ylim(c(0,400))


# Boot Height: Just Women

women_boot <- women %>% filter(., Boot == "Knee-High Boots" | Boot == "Ankle Boots" | Boot == "Mid-Calf Boots" | Boot == "Standard Boots" |
                                 Boot == "Over-the-Knee Boots")

women_boot$Boot <- as.factor(women_boot$Boot)
women_boot$prices.amountMax <- as.numeric(women_boot$prices.amountMax)

women_boot_graph <- ggplot(data = women_boot, aes(x = reorder(Boot, prices.amountMax), y = prices.amountMax)) + 
  geom_boxplot() + xlab("Boot") + ylab("Price") + ylim(c(0,250))


# Heat Map

combined_color <- rbind(women_top_color, men_top_color)
combined_color_revised <- combined_color %>% filter(., colors == "Black" | colors == "Brown" | colors == "White" | colors == "Blue" | colors == "Gray")

combined_color_material <- combined_color_revised %>% filter(., Material == "Leather" | Material == "Synthetic" | Material == "Canvas" | Material == "Suede" |
                                                       Material == "Rubber" | Material == "Faux Leather")

combined_color_material$prices.amountMax <- as.numeric(combined_color_material$prices.amountMax)
combined_color_material <- combined_color_material %>% filter(., prices.amountMax < 400 & prices.amountMax > 10)

heatmap_graph <- ggplot(data = combined_color_material, aes(colors, Material)) + geom_raster(aes(fill = prices.amountMax)) + 
  scale_fill_gradient(low = "lightblue", high = "purple")


# Density Plots -- Overall Distribution Analysis

summary_prices_revised <- summary_prices %>% mutate(., Sex = "Women")
summary_prices_men_revised <- summary_prices_men %>% mutate(., Sex = "Men")
combined_overall <- rbind(summary_prices_revised, summary_prices_men_revised)

combined_overall_NL <- combined_overall %>% filter(., Tier == "Non-Luxury")
combined_overall_EL <- combined_overall %>% filter(., Tier == "Everyday Luxury")
combined_overall_L <- combined_overall %>% filter(., Tier == "Luxury")

NL <- ggplot(data = combined_overall_NL, aes(x = Average_by_Brand, group = Sex)) + geom_density(aes(fill = Sex), alpha = 0.3) + 
  xlim(0,100) + xlab("Average by Brand (in Dollars)") + ylab("") + theme(axis.text.y=element_blank(), legend.position="none") + ggtitle("Non-Luxury Shoes")

EL <- ggplot(data = combined_overall_EL, aes(x = Average_by_Brand, group = Sex)) + geom_density(aes(fill = Sex), alpha = 0.3) + 
  xlim(100,200) + xlab("Average by Brand (in Dollars)") + ylab("") + theme(axis.text.y=element_blank(), legend.position="none") + ggtitle("Everyday Luxury")

L <- ggplot(data = combined_overall_L, aes(x = Average_by_Brand, group = Sex)) + geom_density(aes(fill = Sex), alpha = 0.3) + 
  xlim(200, 600) + xlab("Average by Brand (in Dollars)") + ylab("") + theme(axis.text.y=element_blank()) + ggtitle("Luxury Shoes")

grid.arrange(NL, EL, L, ncol = 3)













