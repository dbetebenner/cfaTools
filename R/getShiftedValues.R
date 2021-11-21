`getShiftedValues` <-
function(
         data_table,
         shift_group=c("ID", "CONTENT_AREA"),
         shift_period="YEAR",
         shift_variable="SCALE_SCORE",
         shift_amount=1L,
         shift_type="lag") {

    ### Utility functions
    completeDT <- function(DT, cols, defs = NULL) {
        mDT = do.call(CJ, c(DT[, ..cols], list(unique=TRUE)))
        res = DT[mDT, on=names(mDT)]
        if (length(defs))
            res[, names(defs) := Map(replace, .SD, lapply(.SD, is.na), defs), .SDcols=names(defs)]
            res[]
    }

    if ("VALID_CASE" %in% names(data_table)) valid_case <- "VALID_CASE" else valid_case <- NULL
    data_table[,FILTER_VARIABLE:=seq.int(dim(data_table)[1])]
    data_table_complete <- completeDT(data_table[,c("FILTER_VARIABLE", shift_group, shift_period, valid_case, shift_variable), with=FALSE], cols=c(shift_period, shift_group))
    if (!is.null(valid_case)) data_table_complete[is.na(VALID_CASE), VALID_CASE := "VALID_CASE"]
    setkeyv(data_table_complete, c(shift_group, shift_period, valid_case))
    shift_var_names <- paste(rep(shift_variable, each=length(shift_amount)), toupper(shift_type), shift_amount, sep="_")
    data_table_complete[, (shift_var_names) := shift(.SD, n=shift_amount, type=shift_type), .SDcols=shift_variable, by=shift_group]
    # data_table_complete[, paste(shift_variable, toupper(shift_type), shift_amount, sep="_") := shift(get(shift_variable), n=shift_amount, type=shift_type), by=shift_group]
    data_table_complete <- data_table_complete[!is.na(FILTER_VARIABLE)]
    setkeyv(data_table_complete, "FILTER_VARIABLE")
    data_table[, (shift_var_names) := data_table_complete[, (shift_var_names), with=FALSE]]
    # data_table[,paste(shift_variable, toupper(shift_type), shift_amount, sep="_") := data_table_complete[, paste(shift_variable, toupper(shift_type), shift_amount, sep="_"), with=FALSE]]
    return(data_table[,FILTER_VARIABLE:=NULL])
} ### END getShiftedValues function
