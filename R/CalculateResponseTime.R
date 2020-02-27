###############################################################################
# L0_I1_Q1_RT Per vraag de response tijd
#
###############################################################################

calculate_response_time <- function(mydata) {
    rowNewData = 0
    participants<-unique(mydata$participant_id)
    tasks<-unique(mydata$task_id)

    newData <- data.frame(matrix(ncol = 0, nrow = 0))

    for (participant in participants) {
        for (task in tasks) {
            data = NULL
            # select participant and task
            data = subset(mydata, task_id == task)
            data = subset(data, participant_id == participant)

            if(nrow(data) == 0 ) {
                # when length is zero, no results found for this participant and task
                next
            }

            rowNewData = rowNewData+1
            for (row in 1:nrow(data)) {
                columnName = paste('L', data[row, 'level'], '_I', data[row, 'item'], "_Q", data[row, 'question_number'], "_RT", sep = "")
                newData[rowNewData, columnName] = data[row, 'response_time']
            }

            newData[rowNewData, 'task_id'] = task
            newData[rowNewData, 'participant_id'] = participant
        }
    }

    return(newData[order(
        newData$task_id,
        newData$participant_id),])
}
