library(readxl)
library(dplyr)
library(ggplot2)

f.tail = 1.1
df = read_excel("R Shiny Assignment.xlsx", range = "Assignment!B3:D9")
number_loss_year <- n_distinct(df$`Loss Year`)
number_dev_year <- max(df$`Development Year`)

triangle <- matrix(0, nrow = number_loss_year, ncol = number_dev_year, byrow = TRUE)
loss_years <- sort(unique(df$`Loss Year`))
dev_years <- c(1:number_dev_year)
rownames(triangle) <- loss_years
colnames(triangle) <- dev_years

for (row in 1:nrow(df)){
  LYear <- as.character(df[row, 1])
  DYear <- as.character(df[row, 2])
  triangle[LYear, DYear] <- as.double(df[row, 3])
}

cum.triangle <- t(apply(triangle, 1, cumsum))

n <- number_dev_year
dev_factors <- sapply(1:(n-1),
                      function(i){
                        sum(cum.triangle[c(1:(n-i)),i+1])/sum(cum.triangle[c(1:(n-i)),i])
                      })

dev_factors <- c(dev_factors, f.tail)
full.triangle <- cbind(cum.triangle, "4" = rep(0, number_loss_year))

for(k in 1:n){
  full.triangle[(n-k+1):n, k+1] <- full.triangle[(n-k+1):n,k] * dev_factors[k]
}
round(full.triangle)

# colnames(full.triangle) <- paste("Dev year", colnames(full.triangle), sep = " ")
# rownames(full.triangle) <- paste("Loss year", rownames(full.triangle), sep = " ")

Development = rep(c(1:(number_dev_year+1)), times = number_loss_year)
Loss = as.character(rep(loss_years, each = number_dev_year + 1))
Amount = round(c(t(full.triangle)))

df_plot = data.frame(Development, Loss, Amount)
ggplot(df_plot, aes(x=Development, y=Amount, group=Loss, color=Loss)) +
  geom_point(size = 1.5) +
  geom_text(label=Amount, hjust=0.5, vjust=-1, 
            check_overlap = TRUE, size = 3, color = "black") +
  geom_line(size = 0.5) +
  xlab("Development Year") +
  ylab("Amount ($)") +
  labs(title = "Cumulative Paid Claims", color = "Loss Year") +
  theme(plot.title = element_text(hjust = 0.5))