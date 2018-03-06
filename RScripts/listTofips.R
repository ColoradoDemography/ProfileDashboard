#'  listTofips : Produces a vector of FIPS codes from an inpout list of Census County and Plance Name Codes.
#'
#' @param  df The census place look up file, produced by popPlace.
#' @param level identifies the level to be used (State, Plannign Regions, Counties, Municipalities/Places)
#'    taken from the input$level parameter from the dashboard
#' @param inList1 The list of place names,  Comes from input$unit or input$comp2.
#' @return the fipscode(s) for a selected data level
#' @export

listTofips <- function(df, level, inList1){
  # Function to produce a vector of FIPS codes from an input list of names and codes

  fipsl <- vector()
  switch(level,
         "State" = {fipsl = "300"},
         "Planning Regions" = {
           switch(inlist1,
                  "1"	= c("08075","08087","08095","08115","08121","08125"),
                  "2"	= c("08069","08123"),
                  "3"	= c("08001","08005","08013","08014","08019","08031","08035","08047","08059"),
                  "4"	= c("08041","08093","08119"),
                  "5"	= c("08017","08039","08063","08073"),
                  "6"	= c("08009","08011","08025","08061","08089","08099"),
                  "7"	= c("08101"),
                  "8"	= c("08003","08021","08023","08079","08105","08109"),
                  "9"	= c("08007","08033","08067","08083","08111"),
                  "10" = c("08029","08051","08053","08085","08091","08113"),
                  "11" = c("08045","08077","08081","08103","08107"),
                  "12" = c("08037","08049","08057","08097","08117"),
                  "13" = c("08015","08027","08043","08065"),
                  "14" = c("08055","08071")
           )
         }, #Planning Regions
         "Counties" = {
           if(length(inList1) == 1) {  #Only one entry
             fipsl <- paste0("08",formatC(df[which(df$municipalityname == inList1),1],digits=0, width=3, format="f",flag= "0"))
           } else {
             fipsl <- paste0("08",formatC(df[which(df$municipalityname == inList1),1],digits=0, width=3, format="f",flag= "0"))
             for(i in 2:length(inList1)){
               fipsl <- rbind(fipsl, paste0("08",formatC(df[which(df$municipalityname == inList1[i]),1],digits=0, width=3, format="f",flag= "0")))
             }
           } #if
         }, #County
         "Municipalities/Places" = {
           if(length(inList1) == 1) {  #only one entry
             fipsl <- paste0("08",formatC(df[which(df$municipalityname == inList1),2],digits=0, width=5, format="f",flag= "0"))
           } else {
             fipsl <- paste0("08",formatC(df[which(df$municipalityname == inList1),2],digits=0, width=5, format="f",flag= "0"))
             for(i in 1:length(inList1)){
               fipsl <- rbind(fipsl, paste0("08",formatC(df[which(df$municipalityname == inList1),2],digits=0, width=5, format="f",flag= "0")))
             }
           } #if
         } #Municipalities/Places

  ) #switch

  return(fipsl)
} #end listTofips
