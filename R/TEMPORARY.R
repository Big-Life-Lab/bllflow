#(
# TypeOfSmoker2[SMKDSTYb],
# DHHAGE_cont,
# SMKG203,
# SMK_204b,
# tsq_ds[stpo[SMK_09A], stpoy[SMKG09C]],
# SMK_208b,
# SMK_05Bb,
# agec1[SMK_05Cb],
# s100[SMKG01C]
# )
PackYearRecode <-
  function(SMKDSTY,
           DHHGAGE_cont,
           SMKG203,
           SMK_204,
           SMK_09A_B,
           SMKG09C,
           SMK_208,
           SMK_05B,
           SMK_05C,
           SMKG01C_B,
           SMK_01A) {
    TypeOfSmoker2 <- SMKDSTY
    stpo <- ifelse((SMK_09A_B == 6), 0,
                   ifelse((SMK_09A_B == 7), NA,
                          ifelse((SMK_09A_B == 8), NA,
                                 ifelse((SMK_09A_B == 9), NA, SMK_09A_B)
                          )))
    stpoy <-
      ifelse((SMKG09C == 1), PUMF[PUMF$variable_start = "SMKG09C" &
                                    PUMF$variable_start_value = "1", ]$value,
             ifelse((SMKG09C == 2), PUMF[PUMF$variable_start = "SMKG09C" &
                                           PUMF$variable_start_value = "2", ]$value,
                    ifelse((SMKG09C == 3), PUMF[PUMF$variable_start =
                                                  "SMKG09C" &
                                                  PUMF$variable_start_value = "3", ]$value,
                           ifelse((SMKG09C == 6), 0, NA)
                    )))
    tsq_ds <- ifelse((stpo = 1), 0.5,
                     ifelse((stpo = 2), 1.5,
                            ifelse((stpo = 3), 2.5,
                                   ifelse((stpo = 4), stpoy, NA
                                   ))))
    agecigd <-
      ifelse((SMKG203 == 1), PUMF[PUMF$variable_start = "SMKG203" &
                                    PUMF$variable_start_value = "1", ]$value,
             ifelse((SMKG203 == 2), PUMF[PUMF$variable_start =
                                           "SMKG203" &
                                           PUMF$variable_start_value = "2", ]$value,
                    ifelse((SMKG203 == 3), PUMF[PUMF$variable_start =
                                                  "SMKG203" &
                                                  PUMF$variable_start_value = "3", ]$value,
                           ifelse((SMKG203 == 4), PUMF[PUMF$variable_start =
                                                         "SMKG203" &
                                                         PUMF$variable_start_value = "4", ]$value,
                                  ifelse((SMKG203 == 5), PUMF[PUMF$variable_start =
                                                                "SMKG203" &
                                                                PUMF$variable_start_value = "5", ]$value,
                                         ifelse((SMKG203 == 6), PUMF[PUMF$variable_start =
                                                                       "SMKG203" &
                                                                       PUMF$variable_start_value = "6", ]$value,
                                                ifelse((SMKG203 ==
                                                          7), PUMF[PUMF$variable_start = "SMKG203" &
                                                                     PUMF$variable_start_value = "7", ]$value,
                                                       ifelse((SMKG203 ==
                                                                 8), PUMF[PUMF$variable_start = "SMKG203" &
                                                                            PUMF$variable_start_value = "8", ]$value,
                                                              ifelse((SMKG203 ==
                                                                        9), PUMF[PUMF$variable_start = "SMKG203" &
                                                                                   PUMF$variable_start_value = "9", ]$value,
                                                                     ifelse((SMKG203 ==
                                                                               10), PUMF[PUMF$variable_start = "SMKG203" &
                                                                                           PUMF$variable_start_value = "10", ]$value,
                                                                            ifelse((SMKG203 ==
                                                                                      11), PUMF[PUMF$variable_start = "SMKG203" &
                                                                                                  PUMF$variable_start_value = "11", ]$value,
                                                                                   ifelse((SMKG203 ==
                                                                                             96), PUMF[PUMF$variable_start = "SMKG203" &
                                                                                                         PUMF$variable_start_value = "12", ]$value, NA)
                                                                            ))
                                                              ))
                                                ))
                                  ))
                    )))
    agec1 <-
      ifelse((SMKG01C_B == 1), PUMF[PUMF$variable_start = "SMKG01C" &
                                      PUMF$variable_start_value = "1",]$value,
             ifelse((SMKG01C_B == 2), PUMF[PUMF$variable_start = "SMKG01C" &
                                             PUMF$variable_start_value = "2",]$value,
                    ifelse((SMKG01C_B == 3), PUMF[PUMF$variable_start = "SMKG01C" &
                                                    PUMF$variable_start_value = "3",]$value,
                           ifelse((SMKG01C_B == 4), PUMF[PUMF$variable_start =
                                                           "SMKG01C" &
                                                           PUMF$variable_start_value = "4",]$value,
                                  ifelse((SMKG01C_B == 5), PUMF[PUMF$variable_start =
                                                                  "SMKG01C" &
                                                                  PUMF$variable_start_value = "5",]$value,
                                         ifelse((SMKG01C_B == 6), PUMF[PUMF$variable_start =
                                                                         "SMKG01C" &
                                                                         PUMF$variable_start_value = "6",]$value,
                                                ifelse((SMKG01C_B == 7), PUMF[PUMF$variable_start =
                                                                                "SMKG01C" &
                                                                                PUMF$variable_start_value = "7",]$value,
                                                       ifelse((SMKG01C_B ==
                                                                 8), PUMF[PUMF$variable_start = "SMKG01C" &
                                                                            PUMF$variable_start_value = "8",]$value,
                                                              ifelse((SMKG01C_B ==
                                                                        9), PUMF[PUMF$variable_start = "SMKG01C" &
                                                                                   PUMF$variable_start_value = "9",]$value,
                                                                     ifelse((SMKG01C_B ==
                                                                               10),
                                                                            PUMF[PUMF$variable_start = "SMKG01C" &
                                                                                   PUMF$variable_start_value = "10",]$value,
                                                                            ifelse((SMKG01C_B ==
                                                                                      11), PUMF[PUMF$variable_start = "SMKG01C" &
                                                                                                  PUMF$variable_start_value = "11",]$value,
                                                                                   ifelse((SMKG01C_B ==
                                                                                             96), PUMF[PUMF$variable_start = "SMKG01C" &
                                                                                                         PUMF$variable_start_value = "12",]$value, NA))
                                                                     )
                                                              ))
                                                ))
                                  ))
                    )))
    s100 <-
      ifelse((SMK_01A == 7), NA,
             ifelse((SMK_01A == 8), NA,
                    ifelse((SMK_01A == 9), NA, SMK_01A)))

    packYears <-
      PackYears1.fun(
        TypeOfSmoker2,
        DHHGAGE_cont,
        agecigd,
        SMK_204,
        tsq_ds,
        SMK_208,
        SMK_05B,
        SMK_05C,
        agec1,
        s100
      )
    
  }