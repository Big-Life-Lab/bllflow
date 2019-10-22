TestingONLY <- function(){
  source('C:/Users/DarkShadeKnight/Documents/Work/R/pblTransformations/PBLTransformations/Derived Variables RecWTable functions/FunctionIndex.R')
}
# PackYearRecode <-
#   function(SMKDSTY,
#            DHHGAGE_cont,
#            SMKG203,
#            SMK_204,
#            SMK_09A_B,
#            SMKG09C,
#            SMK_208,
#            SMK_05B,
#            SMK_05C,
#            SMKG01C_B,
#            SMK_01A,
#            SMK_207) {
#     TypeOfSmoker2 <- SMKDSTY
#     stpo <- ifelse((SMK_09A_B == 6), 0,
#                    ifelse((SMK_09A_B == 7), NA,
#                           ifelse((SMK_09A_B == 8), NA,
#                                  ifelse((SMK_09A_B == 9), NA, SMK_09A_B)
#                           )))
#     stpoy <- SMKG09C
#     tsq_ds <- ifelse((stpo = 1), 0.5,
#                      ifelse((stpo = 2), 1.5,
#                             ifelse((stpo = 3), 2.5,
#                                    ifelse((stpo = 4), stpoy, NA
#                                    ))))
#     agecigd <- SMKG203
#     agec1 <- SMKG01C_B
#     s100 <-
#       ifelse((SMK_01A == 7), NA,
#              ifelse((SMK_01A == 8), NA,
#                     ifelse((SMK_01A == 9), NA, SMK_01A)))
#     
#     packYears <-
#       PackYears1.fun(
#         TypeOfSmoker2,
#         DHHGAGE_cont,
#         agecigd,
#         SMK_207,
#         SMK_204,
#         tsq_ds,
#         SMK_208,
#         SMK_05B,
#         SMK_05C,
#         agec1,
#         s100
#       )
#     # print(paste("typeOfSmoker2 =",TypeOfSmoker2, "Age_cont =", DHHGAGE_cont, "agecigd =", agecigd, "agecigo =", SMK_207, "cigdayd =", SMK_204,
#     #             "tsq_ds =", tsq_ds, "cigdayf =", SMK_208, "cigdayo =", SMK_05B, "dayocc =", SMK_05C, "agec1 =", agec1, "s100 =", s100))
#     # print(paste("PACKYEARS =", packYears))
#     
#     return(packYears)
#   }
# PackYears1.fun <-
#   function(TypeOfSmoker2,
#            Age_cont,
#            agecigd,
#            agecigo,
#            cigdayd,
#            tsq_ds,
#            cigdayf,
#            cigdayo,
#            dayocc,
#            agec1,
#            s100) {
#     ifelse2(TypeOfSmoker2 == 1,
#             pmax(((Age_cont - agecigd) * (cigdayd / 20)), 0.0137),
#             ifelse2(
#               TypeOfSmoker2 == 2,
#               pmax(((Age_cont - agecigo - tsq_ds) * (cigdayf / 20)
#               ), 0.0137) + (pmax((
#                 cigdayo * dayocc / 30
#               ), 1) * tsq_ds),
#               ifelse2(
#                 TypeOfSmoker2 == 3,
#                 (pmax((
#                   cigdayo * dayocc / 30
#                 ), 1) / 20) * (Age_cont - agec1),
#                 ifelse2(
#                   TypeOfSmoker2 == 4,
#                   pmax(((Age_cont - agecigo - tsq_ds) * (cigdayf / 20)
#                   ), 0.0137),
#                   ifelse2(
#                     TypeOfSmoker2 == 5 & s100 == 's1001',
#                     0.0137,
#                     ifelse2(
#                       TypeOfSmoker2 == 5 & s100 == 's1002',
#                       0.007,
#                       ifelse2(TypeOfSmoker2 ==
#                                 6, 0, NA)
#                     )
#                   )
#                 )
#               )
#             ))
#   }
# ifelse2 <- function(x, a, b) {
#   falseifNA <- function(x) {
#     ifelse(is.na(x), FALSE, x)
#   }
#   ifelse(falseifNA(x), a, b)
# }