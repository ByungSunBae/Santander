library("data.table")
library("Information")
library("h2o")

ths <- parallel::detectCores() - 2

h2o.init(nthreads = ths, max_mem_size = "50g")
h2o.removeAll()

train_h2o <- h2o.importFile("data/fnl_train.csv", destination_frame = "train_h2o")
test_h2o <- h2o.importFile("data/fnl_test.csv", destination_frame = "test_h2o")

train_h2o$target <- as.factor(train_h2o$target)

target <- "target"
all_vars <- names(train_h2o)
features <- all_vars[! all_vars %in% c("ID_code", target)]

<<<<<<< HEAD
aml <-
  h2o.automl(
    x = features,
    y = target,
    training_frame = train_h2o,
    max_runtime_secs = 3 * 3600,
    exclude_algos = c("DRF")
  )

=======
aml <- h2o.automl(x = features, y = target, training_frame = train_h2o, max_runtime_secs = 5 * 3600)
>>>>>>> dcf0d721983620adea03dc9f115ea8a3dde09265
h2o.saveModel(aml@leader, "./model", force = TRUE)

pred <- as.data.table(h2o.predict(aml@leader, newdata = test_h2o))

pred$ID_code <- as.vector(test_h2o$ID_code)

sub_dt <- fread("data/sample_submission.csv")
sub_dt <- merge(sub_dt, pred[, c("ID_code", "p1")], by = "ID_code")
sub_dt[, target := NULL]
setnames(sub_dt, "p1", "target")

<<<<<<< HEAD
fwrite(sub_dt, "result/submission_v9.csv")
=======
fwrite(sub_dt, "result/submission_v10.csv")
>>>>>>> dcf0d721983620adea03dc9f115ea8a3dde09265
