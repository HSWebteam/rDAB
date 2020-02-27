###############################################################################
# project_name
# oganisation_name
# group_name
# participant_case_number
# id
# task_id
# taak
# #levels
# #items
# language
# theme
# matrix_size
# date
# L*_I*_Q*
# L*_I*_prop
# L*_I*_Q*_RT
# meanprop
###############################################################################

transpose <- function(mydata) {
    participants<-unique(mydata$participant_id)
    tasks<-unique(mydata$task_id)

    rowNewData = 0
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
                if(row == 1) {
                    newData[rowNewData, 'organisation_name'] = toString(data[row, 'organisation_name'])
                    newData[rowNewData, 'group_name'] = toString(data[row, 'group_name'])
                    newData[rowNewData, 'participant_case_number'] = toString(data[row, "participant_case_number"])
                    newData[rowNewData, 'participant_id'] = data[row, 'participant_id']
                    newData[rowNewData, 'id'] = data[row, 'participant_id']
                    newData[rowNewData, 'task_id'] = data[row, 'task_id']
                    newData[rowNewData, 'taak'] = toString(data[row, 'taak'])
                    newData[rowNewData, '#levels'] = data[row, 'X.levels']
                    newData[rowNewData, '#items'] = data[row, 'X.items']
                    newData[rowNewData, 'language'] = toString(data[row, 'language'])
                    newData[rowNewData, 'theme'] = toString(data[row, 'theme'])
                    newData[rowNewData, 'matrix_size'] = data[row, 'matrix_size']
                    newData[rowNewData, 'date'] = toString(data[row, 'updated_at'])
                }
                columnName = paste('L', data[row, 'level'], '_I', data[row, 'item'], "_Q", data[row, 'question_number'], sep = "")
                newData[rowNewData, columnName] = data[row, 'score']
            }
        }
    }

    return(newData[order(
        newData$task_id,
        newData$participant_id),])
}
