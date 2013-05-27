

(defstruct exp-result id var-num cl-num history time sat)




(defun experiment ()
  (let ((dataset (load-sat-all (list 
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-1/3sat-10-10_1.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-1/3sat-10-10_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-1/3sat-10-10_3.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-1/3sat-10-10_4.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-1/3sat-10-10_5.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-1/3sat-15-15_1.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-1/3sat-15-15_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-1/3sat-15-15_3.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-1/3sat-15-15_4.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-1/3sat-15-15_5.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-1/3sat-20-20_1.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-1/3sat-20-20_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-1/3sat-20-20_3.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-1/3sat-20-20_4.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-1/3sat-20-20_5.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-1/3sat-25-25_1.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-1/3sat-25-25_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-1/3sat-25-25_3.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-1/3sat-25-25_4.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-1/3sat-25-25_5.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-1/3sat-5-5_1.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-1/3sat-5-5_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-1/3sat-5-5_3.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-1/3sat-5-5_4.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-1/3sat-5-5_5.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-2/3sat-10-20_1.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-2/3sat-10-20_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-2/3sat-10-20_3.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-2/3sat-10-20_4.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-2/3sat-10-20_5.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-2/3sat-15-30_1.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-2/3sat-15-30_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-2/3sat-15-30_3.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-2/3sat-15-30_4.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-2/3sat-15-30_5.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-2/3sat-20-40_1.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-2/3sat-20-40_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-2/3sat-20-40_3.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-2/3sat-20-40_4.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-2/3sat-20-40_5.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-2/3sat-25-50_1.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-2/3sat-25-50_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-2/3sat-25-50_3.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-2/3sat-25-50_4.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-2/3sat-25-50_5.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-2/3sat-5-10_1.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-2/3sat-5-10_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-2/3sat-5-10_3.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-2/3sat-5-10_4.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-2/3sat-5-10_5.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-3/3sat-10-30_1.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-3/3sat-10-30_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-3/3sat-10-30_3.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-3/3sat-10-30_4.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-3/3sat-10-30_5.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-3/3sat-15-45_1.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-3/3sat-15-45_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-3/3sat-15-45_3.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-3/3sat-15-45_4.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-3/3sat-15-45_5.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-3/3sat-20-60_1.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-3/3sat-20-60_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-3/3sat-20-60_3.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-3/3sat-20-60_4.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-3/3sat-20-60_5.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-3/3sat-25-75_1.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-3/3sat-25-75_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-3/3sat-25-75_3.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-3/3sat-25-75_4.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-3/3sat-25-75_5.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-3/3sat-5-15_1.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-3/3sat-5-15_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-3/3sat-5-15_3.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-3/3sat-5-15_4.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-3/3sat-5-15_5.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4/3sat-10-40_1.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4/3sat-10-40_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4/3sat-10-40_3.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4/3sat-10-40_4.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4/3sat-10-40_5.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4/3sat-15-60_1.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4/3sat-15-60_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4/3sat-15-60_3.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4/3sat-15-60_4.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4/3sat-15-60_5.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4/3sat-20-80_1.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4/3sat-20-80_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4/3sat-20-80_3.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4/3sat-20-80_4.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4/3sat-20-80_5.cnf"
;"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4/3sat-25-100_1.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4/3sat-25-100_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4/3sat-25-100_3.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4/3sat-25-100_4.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4/3sat-25-100_5.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4/3sat-5-20_1.cnf"
;"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4/3sat-5-20_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4/3sat-5-20_3.cnf"
;"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4/3sat-5-20_4.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4/3sat-5-20_5.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4-5/3sat-10-45_1.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4-5/3sat-10-45_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4-5/3sat-10-45_3.cnf"
;"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4-5/3sat-10-45_4.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4-5/3sat-10-45_5.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4-5/3sat-15-67_1.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4-5/3sat-15-67_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4-5/3sat-15-67_3.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4-5/3sat-15-67_4.cnf"
;"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4-5/3sat-15-67_5.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4-5/3sat-20-90_1.cnf"
;"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4-5/3sat-20-90_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4-5/3sat-20-90_3.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4-5/3sat-20-90_4.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4-5/3sat-20-90_5.cnf"
;"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4-5/3sat-25-112_1.cnf"
;"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4-5/3sat-25-112_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4-5/3sat-25-112_3.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4-5/3sat-25-112_4.cnf"
;"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4-5/3sat-25-112_5.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4-5/3sat-5-22_1.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4-5/3sat-5-22_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4-5/3sat-5-22_3.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4-5/3sat-5-22_4.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-4-5/3sat-5-22_5.cnf"
;"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-5/3sat-10-50_1.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-5/3sat-10-50_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-5/3sat-10-50_3.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-5/3sat-10-50_4.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-5/3sat-10-50_5.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-5/3sat-15-75_1.cnf"
;"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-5/3sat-15-75_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-5/3sat-15-75_3.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-5/3sat-15-75_4.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-5/3sat-15-75_5.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-5/3sat-20-100_1.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-5/3sat-20-100_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-5/3sat-20-100_3.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-5/3sat-20-100_4.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-5/3sat-20-100_5.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-5/3sat-25-125_1.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-5/3sat-25-125_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-5/3sat-25-125_3.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-5/3sat-25-125_4.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-5/3sat-25-125_5.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-5/3sat-5-25_1.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-5/3sat-5-25_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-5/3sat-5-25_3.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-5/3sat-5-25_4.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-5/3sat-5-25_5.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-6/3sat-10-60_1.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-6/3sat-10-60_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-6/3sat-10-60_3.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-6/3sat-10-60_4.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-6/3sat-10-60_5.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-6/3sat-15-90_1.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-6/3sat-15-90_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-6/3sat-15-90_3.cnf"
;"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-6/3sat-15-90_4.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-6/3sat-15-90_5.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-6/3sat-20-120_1.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-6/3sat-20-120_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-6/3sat-20-120_3.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-6/3sat-20-120_4.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-6/3sat-20-120_5.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-6/3sat-25-150_1.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-6/3sat-25-150_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-6/3sat-25-150_3.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-6/3sat-25-150_4.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-6/3sat-25-150_5.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-6/3sat-5-30_1.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-6/3sat-5-30_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-6/3sat-5-30_3.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-6/3sat-5-30_4.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-6/3sat-5-30_5.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-7/3sat-10-70_1.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-7/3sat-10-70_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-7/3sat-10-70_3.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-7/3sat-10-70_4.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-7/3sat-10-70_5.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-7/3sat-15-105_1.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-7/3sat-15-105_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-7/3sat-15-105_3.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-7/3sat-15-105_4.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-7/3sat-15-105_5.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-7/3sat-20-140_1.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-7/3sat-20-140_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-7/3sat-20-140_3.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-7/3sat-20-140_4.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-7/3sat-20-140_5.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-7/3sat-25-175_1.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-7/3sat-25-175_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-7/3sat-5-35_1.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-7/3sat-5-35_2.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-7/3sat-5-35_3.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-7/3sat-5-35_4.cnf"
"/mnt/diskData/Sources/LispProjects/mi-paa/paa6-sat/datasat/soft-7/3sat-5-35_5.cnf"
)))
	(results nil)
	(config (make-ga-config :population-size 100
				:stopping-criterion-fn (make-generation-age-condition-fn 5000)
				:repopulation-fn (make-simple-repopulation 3 1)
				:mutation-fn (make-bit-flip-mutation 0.99) )))
    (with-open-file (str "datasat/results/generations/random-data-gen.csv"
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
      (dolist (instance dataset)

	(let ((result nil)
	      (before nil)
	      (after nil))
	      (setf before (get-internal-real-time))
	      (print (sat-problem-id instance))
	      (setf result (genetic-algorithm instance config))
	      (setf after (get-internal-real-time))
	      (setf results  (cons 
			      (make-exp-result :id (sat-problem-id instance) :var-num (sat-problem-dimension instance) :cl-num (length (sat-problem-clauses instance)) :history (result-history result) :time (- after before) :sat (result-satisfied result))
			      results))))
      (mapcar 
       (lambda (exp-res)
	 (format str "~5,3F;" (exp-result-id exp-res))
	 (format str "~5,3F;" (exp-result-var-num exp-res))
	 (format str "~5,3F;" (exp-result-cl-num exp-res))
	 (format str "~5,3F;" (exp-result-sat exp-res))

	 (mapcar (lambda (res)
		   (format str "~5,3F;" res))
		 (exp-result-history exp-res))

	 (format str "~%"))
       results))))


(defstruct bf-res var-num clauses-num solution)

(defun bf-experiment ()
  (let ((iter 0)
	(dataset (load-sat-all 
		  (reverse (list 

"/home/watanabe/mi-paa/paa6-sat/datasat/soft-5/3sat-20-100_1.cnf"
"/home/watanabe/mi-paa/paa6-sat/datasat/soft-5/3sat-20-100_2.cnf"
"/home/watanabe/mi-paa/paa6-sat/datasat/soft-5/3sat-20-100_3.cnf"
"/home/watanabe/mi-paa/paa6-sat/datasat/soft-5/3sat-20-100_4.cnf"
"/home/watanabe/mi-paa/paa6-sat/datasat/soft-5/3sat-20-100_5.cnf"
"/home/watanabe/mi-paa/paa6-sat/datasat/soft-5/3sat-25-125_1.cnf"
"/home/watanabe/mi-paa/paa6-sat/datasat/soft-5/3sat-25-125_2.cnf"
"/home/watanabe/mi-paa/paa6-sat/datasat/soft-5/3sat-25-125_3.cnf"
"/home/watanabe/mi-paa/paa6-sat/datasat/soft-5/3sat-25-125_4.cnf"
"/home/watanabe/mi-paa/paa6-sat/datasat/soft-5/3sat-25-125_5.cnf"



)))))
    (with-open-file (str "/home/watanabe/mi-paa/paa6-sat/datasat/solutions-5-2.csv"
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
      (dolist (instance dataset)
	(let ((result (brute-force instance)))
	      (print result)
	      (format str "~5,3F;" (1+ iter))
	      (incf iter)
	      (setf iter (mod iter 5))
	      (format str "~5,3F;" (sat-problem-dimension instance))
	      (format str "~5,3F;" (length (sat-problem-clauses instance)))
	      (format str "~5,3F;" result)
	      (format str "~%"))
	))))
