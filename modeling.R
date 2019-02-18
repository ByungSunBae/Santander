library("data.table")
library("h2o")

ths <- parallel::detectCores() - 2

h2o.init(nthreads = ths, max_mem_size = "40g")
h2o.removeAll()

train_h2o <- h2o.importFile("data/fnl_train.csv", destination_frame = "train_h2o")
test_h2o <- h2o.importFile("data/fnl_test.csv", destination_frame = "test_h2o")

train_h2o$target <- as.factor(train_h2o$target)

target <- "target"
all_vars <- names(train_h2o)
features <- all_vars[! all_vars %in% c("ID_code", target)]

aml <- h2o.automl(x = features, y = target, training_frame = train_h2o, max_runtime_secs = 8 * 3600)
h2o.saveModel(aml@leader, "./model", force = TRUE)

pred <- as.data.table(h2o.predict(aml@leader, newdata = test_h2o))

pred$ID_code <- as.vector(test_h2o$ID_code)

sub_dt <- fread("data/sample_submission.csv")
sub_dt <- merge(sub_dt, pred[, c("ID_code", "p1")], by = "ID_code")
sub_dt[, target := NULL]
setnames(sub_dt, "p1", "target")

fwrite(sub_dt, "result/submission_v7.csv")
