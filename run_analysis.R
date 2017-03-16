run_analysis<-function() {
        ## Course Project
        
        ## Libraries
        library(reshape2)
        
        ##---------------------------------STEP 1------------------------------------------------------------------------
        
        ## Load relevant files into R
        X_col_names <- read.table("features.txt")
        X_train<-read.table("X_train.txt")
        subject_train<-read.table("subject_train.txt")
        y_train<-read.table("y_train.txt")
        X_test<-read.table("X_test.txt")
        subject_test<-read.table("subject_test.txt")
        y_test<-read.table("y_test.txt") 
        
        ## Change Column Names
        names(X_test)<-X_col_names[,2]
        names(X_train)<-X_col_names[,2]
        names(y_test)<-"Activities"
        names(y_train)<-"Activities"
        names(subject_test)<-"Subject_ID"
        names(subject_train)<-"Subject_ID"
        
        ## Combine files into one dataset
        train <- cbind(subject_train, y_train, X_train)
        test <- cbind(subject_test, y_test, X_test)
        Final_Data <- rbind(train, test)
        
        ##---------------------------------STEP 2------------------------------------------------------------------------
        
        ## Remove unwanted Columns
        columns_keep<-grepl("mean\\(|std()|Activities|Subject_ID",names(Final_Data))
        Final_Data_Short<-Final_Data[,columns_keep]
        
        ##---------------------------------STEP 3------------------------------------------------------------------------
        
        ## Changing names of Activities Column
        Final_Data_Short$Activities<-factor(Final_Data_Short$Activities,labels=c("WALKING",
                                                                                    "WALKING_UPSTAIRS",
                                                                                    "WALKING_DOWNSTAIRS",
                                                                                    "SITTING",
                                                                                    "STANDING",
                                                                                    "LAYING"))
        
        ##---------------------------------STEP 4------------------------------------------------------------------------
        
        ## Melt and Re-Cast data to create Tidy data set with mean and export to CSV file
        Final_Data_Melted <- melt(Final_Data_Short, id=c("Subject_ID","Activities"))
        Final_Data_Tidy <- dcast(Final_Data_Melted, Subject_ID+Activities ~ variable, mean)
        
        ## Changing Column Names
        names(Final_Data_Tidy) <- gsub("^t", "Time-", names(Final_Data_Tidy))
        names(Final_Data_Tidy) <- gsub("^f", "Freq-", names(Final_Data_Tidy))
        names(Final_Data_Tidy) <- gsub("mean\\(\\)", "MEAN", names(Final_Data_Tidy))
        names(Final_Data_Tidy) <- gsub("std\\(\\)", "STD", names(Final_Data_Tidy))
        
        ## Writting CSV File
        write.csv(Final_Data_Tidy, "Final_Data_Tidy.csv", row.names=FALSE)
        
}