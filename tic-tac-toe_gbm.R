library(readr)
library(gbm)

# get UCI data set
tic_tac_toe <- readr::read_csv(
  "https://archive.ics.uci.edu/ml/machine-learning-databases/tic-tac-toe/tic-tac-toe.data"
)

# descriptive column names
colnames(tic_tac_toe) <-
  c(
    'top-left',
    'top-middle',
    'top-right',
    'middle-left',
    'middle-middle',
    'middle-right',
    'bottom-left',
    'bottom-middle',
    'bottom-right',
    'result'
  )

# make all columns factors
tic_tac_toe <- data.frame(lapply(tic_tac_toe, factor))

# make result a numerical factor
tic_tac_toe$result <- ifelse(tic_tac_toe$result == "negative", 0, 1)

# split training from test data (80/20)
idx <- sample(1:nrow(tic_tac_toe), 0.8 * nrow(tic_tac_toe))
trn_df <- tic_tac_toe[idx, ]
tst_df <- tic_tac_toe[-idx, ]

# train the model
gbm1 <-
  gbm(
    result ~ .,
    data = trn_df,
    distribution = "bernoulli",
    n.trees = 2000,
    interaction.depth = 4
  )

# test data predictions
preds <-
  predict(gbm1,
          newdata = tst_df,
          type = "response",
          n.trees = 2000)

# encode predictions
preds_enc <- ifelse(preds < 0.5, 0, 1)

# percentage of correct predictions
sum(preds_enc == tst_df$result) / nrow(tst_df)

# summary
summary(gbm1)
