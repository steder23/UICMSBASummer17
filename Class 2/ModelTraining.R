# Clean environment
rm(list=ls())

# DB connection string ####
if (Sys.info()[1] == "Windows"){
	db.connection = paste0("Driver={SQL Server};",
												 "Server=CHRPF36Z726\\SQLEXPRESS;",
												 "Database=UIC;",
												 "Trusted_connection=Yes")
	
} else if (Sys.info()[1] == "Darwin"){
	db.connection = paste0("Driver=/usr/local/lib/libtdsodbc.so;",
												 "PORT=1433;",
												 "Server=CHRPF36Z726\\SQLEXPRESS;",
												 "Database=UIC;",
												 "UID=myuser;",
												 "PWD=mypwd")
}  else if (Sys.info()[1] == "Linux"){
	db.connection = paste0(
		"Driver=/usr/lib/x86_64-linux-gnu/odbc/libtdsodbc.so;",
		"PORT=1433;",
		"Server=.\\SQLEXPRESS;",
		"Database=UIC;",
		"UID=myuser;",
		"PWD=mypwd;")
} else {
	stop("System not set up for db connection yet")
}

# Get data ####
library(RODBC)

# Create ODBC connection
dbConn = odbcDriverConnect(connection=db.connection)

# Pull data from DB
reportData = sqlQuery(dbConn, "SELECT * FROM dbo.vw_DataFrame")

dimReportCode = sqlQuery(dbConn, "SELECT * FROM dbo.vw_ReportCode")

dimUser = sqlQuery(dbConn, "SELECT * FROM dbo.vw_User")

close(dbConn)

# Non-exhaustive Cross-Validation: Holdout ####
trainPerc = .75
nTrain = ceiling(dim(reportData)[1] * trainPerc)
nValid = reportData[1] - nTrain

# Split data into training / validation
indTrain = sample(x = 1:dim(reportData)[1], size = nTrain)
rownames(reportData) = reportData$ReportID
data.train = reportData[indTrain, -1]
data.valid = reportData[-indTrain, -1]

# Model training & Validation####
targetVar = "NextReportCodeID"
reportFormula = formula("NextReportCodeID ~ .")

# Linear Model
mod.lm = lm(formula = reportFormula, data = data.train)
pred.lm = predict(object = mod.lm, newdata = data.valid)

# Linear Discriminant Analysis
library(MASS)
mod.lda = lda(formula = reportFormula, data = data.train)
pred.lda = predict(object = mod.lda, newdata = data.valid)

# Remove ReportYear feature
reportData = reportData[, -4]

# Go back to line 50

# Get highest prob
pred.lda$prob = apply(X = pred.lda$posterior, MARGIN = 1, FUN = max)
# pred.lda$pred = colnames(pred.lda$posterior)[max.col(m = pred.lda$posterior)]

# Calculate error rate
err = length(which(pred.lda$class!=data.valid$NextReportCodeID)) /
	dim(data.valid)[1]



# Random Forest ####
library(randomForest)
mod.rf = randomForest(formula = reportFormula, data = data.train)













