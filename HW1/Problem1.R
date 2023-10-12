admissionsProbs <- function() {
  adm <- UCBAdmissions

  num_admitted <- sum(adm[1, , ])

  total_num_applicants <- sum(adm[ , , ])

  num_admitted_and_female <- sum(adm[1, 2, ])

  num_females <- sum(adm[ , 2, ])

  num_admitted_female_deptB <- sum(adm[1, 2, 2])

  num_female_deptB <- sum(adm[ , 2, 2])

  num_admitted_female_deptC <- sum(adm[1, 2, 3])

  num_female_deptC <- sum(adm[ ,2, 3])

  num_admitted_and_female_and_deptB_or_deptC <- sum(adm[1 , 2, c(2, 3)])

  num_female_and_deptB_or_deptC <- sum(adm[ , 2 ,c(2, 3)])

  num_deptC_or_deptD <- sum(adm[ , , c(3, 4)])

  admit <- num_admitted / total_num_applicants

  admit_female <- num_admitted_and_female / num_females

  admit_femaleNdepB <- num_admitted_female_deptB / num_female_deptB

  admit_femaleNdepC <- num_admitted_female_deptC / num_female_deptC

  deptC_female <- num_female_deptC / num_females

  admit_femalN_depBOdepC <- num_admitted_and_female_and_deptB_or_deptC / num_female_and_deptB_or_deptC

  female_admit <- num_admitted_and_female / num_admitted

  femaleNadmit <- num_admitted_and_female / total_num_applicants

  depCOdepB <- num_deptC_or_deptD / total_num_applicants

  return(c(admit, admit_female, admit_femaleNdepB, admit_femaleNdepC, deptC_female,
           admit_femalN_depBOdepC, female_admit, femaleNadmit, depCOdepB))
}