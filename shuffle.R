
# 读取 CSV 文件
file_path <- 'D:/data_analyse/data/20241115/data.csv'
data<- read.csv(file_path)

# datav1 （初版）
datav1 <- data %>% select(SEQN,       # 序号 99-23 
                          RIAGENDR,   # 性别 99-23
                          RIDAGEYR,   # 年龄 99-23
                          BMXWT,      # 身高 99-23
                          BMXWAIST,   # 腰围 99-23
                          RIDRETH1,   # 人种 99-23
                          RIDEXPRG,   # 怀孕 99-23
                          LBDSUASI,   # 尿酸 99-20
                          DMDEDUC2,   # 教育 99-23
                          DMDMARTL, DMDMARTZ,  # 婚姻 99-16 17-23
                          INDFMPIR,   # 收入 99-23
                          BMXBMI,     # BMI  99-23
                          DSDCOUNT,   # 补充食物 1 （如果只问有没有的话到23年）
                          PHQ030,     # 酒精 99-23
                          DIQ010,     # 糖尿病 99-23
                          MCQ160C,    # 冠心病 99-23
                          BPQ020,     # 高血压 99-23
                          LBDTCSI,    # 胆固醇 mmol/L 99-23
                          LBDSCRSI,   # 血肌酐 99-20
                          # 新增列　
                          DRXTKCAL, DR1TKCAL,  # 卡路里 kcal 99-02 03-23
                          DRXTPROT, DR1TPROT,  # 蛋白质 gm 99-02 03-23
                          DRXTCARB, DR1TCARB,  # 糖类 gm 99-02 03-23
                          DRXTTFAT, DR1TTFAT,  # 总脂肪 gm 99-02 03-23
                          DRXTSFAT, DR1TSFAT,  # 总饱和脂肪酸 gm 99-02 03-23
                          DRXTMFAT, DR1TMFAT,  # 总单不饱和脂肪酸 gm 99-02 03-23
                          DRXTPFAT, DR1TPFAT,  # 总多不饱和脂肪酸 gm 99-02 03-23
                          DRXTCHOL, DR1TCHOL,  # 胆固醇 gm 99-02 03-23
                          DRXTFIBE, DR1TFIBE,  # 膳食纤维 gm 99-02 03-23
                          DRXTVARE, DRXTVARA, DR1TVARA,  # 维他命A mcg 99-00 01-02 03-23
                          
                          DRXTCARO, 
                          DRXTACAR, DRXTBCAR, 
                          DR1TACAR, DR1TBCAR,  # 胡萝卜素、alpha胡萝卜素、beta胡萝卜素 re mcg 99-00 01-02a 01-02b 03-23a 03-23b
                          
                          DRXTVB1, DR1TVB1,    # 维他命B1 gm 99-02 03-23
                          DRXTVB2, DR1TVB2,    # 维他命B2 gm 99-02 03-23
                          DRXTNIAC, DR1TNIAC,  # 烟酸 gm 99-02 03-23
                          DRXTVB6, DR1TVB6,    # 维他命B6 gm 99-02 03-23
                          DRXTFOLA, DR1TFOLA,  # 总叶酸 mcg 99-02 03-23
                          DRXTVB12, DR1TVB12,  # 维他命B12 gm 99-02 03-23 
                          DRXTVC, DR1TVC,      # 维他命C gm 99-02 03-23
                          DRXTVE, DRXTATOC, DR1TATOA,    # 维他命E gm 99-00 01-02 03-23
                          DRXTMAGN, DR1TMAGN,  # 镁 mg 99-02 03-23
                          DRXTIRON, DR1TIRON,  # 铁 mg 99-02 03-23
                          DRXTZINC, DR1TZINC,  # 锌 mg 99-02 03-23
                          DRXTSELE, DR1TSELE,  # 晒 mg 99-02 03-23
                          DRXTCAFF, DR1TCAFF,  # 咖啡因 mg 99-02 03-23
                          DRXTALCO, DR1TALCO,  # 酒精 gm 99-02 03-23
                          LBXWBCSI,  # 白细胞计数 1000 cells/uL 99-23
                          LBDLYMNO,  # 淋巴细胞计数 1000 cells/uL 99-23
                          LBDMONO,   # 单核细胞计数 1000 cells/uL 99-23
                          LBXPLTSI,  # 血小板计数 %SI 99-23
                          LBDNENO,   # 中性粒细胞 1000 cells/uL 99-23
                          # 酒精
                          # ALQ100, ALD100, ALQ101,       # 12酒精/年 99-00 01-02 03-20
                          # ALQ110,    # 12酒精/一生 99-16
                          ALQ120Q, ALQ121,    # 去年喝酒频率 99-16 17-23
                          # ALQ120U,   # days drink alcohol per wk, mo, yr 99-16
                          ALQ130,    # 最近一年喝酒天数 99-23
                          ALQ140Q, ALQ141Q, ALQ142,     # 最近一年喝5杯以及上的天数 99-10 11-16 17-23
                          # ALQ140U, ALQ141U, # days per week, month, year? 99-10 11-16
                          ALQ150, ALQ151,   # 是否曾每天喝五杯以上酒？ 99-10 11-23
                          # 吸烟
                          # ECQ020, # mother smoke when pregnant 99-20
                          # ECQ040, # mother quit smoke when pregnant mo 99-20
                          SMD030, # 开始有习惯性吸烟的年龄 99-20
                          # SMD055, # Age last smoked cigarettes regularly 99-16
                          SMD057, # 戒烟时每天吸烟数量 99-20
                          # SMD070, # cigarettes smoked per day now 99-06
                          # SMD075, # How many years smoked this amount 99-06
                          SMD080, SMD641, # 30天内吸烟的天数 99-02 03-23
                          # SMD090, # Avg # cigarettes/day during past 30 days 99-02
                          # SMD410, # does anyone smoke in the home 99-12
                          SMD415, SMD460, # 家中吸烟者数量 99-12 13-23
                          SMD630, # 第一次吸完整根烟的年龄 99-23
                          SMQ020, # 一生中至少吸过100支香烟 99-23
                          SMQ040, # 你现在吸烟吗 99-23
                          SMQ050Q, # 戒烟多久了 99-20
                          SMQ050U, # 上一问的单位（天/周/月/年） 99-20
                          # SMQ077, SMQ078, # How soon after waking do you smoke 01-12 13-20
                          SMQ620, SMQ621, # 曾经尝试过吸烟 99-10 11-23
                          SMQ670, # 尝试过戒烟 99-20
                          SMQ725, # 你吸的上一支烟是什么时候 99-23
                          # SMQ740, # days smoked pipe over last 5 days 01-20
                          # 运动
                          # PAQ180, # avg level of physical activity each day 99-06
                          PAD200, PAQ605, # 30天内高强度运动  99-06 07-20
                          PAD320, PAQ620, # 30天内普通强度运动 99-06 07-20
                          # PAD440, # Muscle strengthening activities 99-06
                          # PAD460, # Number of times past 30 days 99-06
                         )

# datav1 （筛选版）
datav1 <- data %>% select(SEQN,       # 序号 99-23 
                          RIAGENDR,   # 性别 99-23
                          RIDAGEYR,   # 年龄 99-23
                          BMXWT,      # 身高 99-23
                          BMXWAIST,   # 腰围 99-23
                          RIDRETH1,   # 人种 99-23
                          RIDEXPRG,   # 怀孕 99-23
                          LBDSUASI,   # 尿酸 99-20
                          DMDEDUC2,   # 教育 99-23
                          DMDMARTL, DMDMARTZ,  # 婚姻 99-16 17-23
                          INDFMPIR,   # 收入 99-23
                          BMXBMI,     # BMI  99-23
                          DSDCOUNT,   # 补充食物 1 （如果只问有没有的话到23年）
                          PHQ030,     # 酒精 99-23
                          DIQ010,     # 糖尿病 99-23
                          MCQ160C,    # 冠心病 99-23
                          BPQ020,     # 高血压 99-23
                          LBDTCSI,    # 胆固醇 mmol/L 99-23
                          LBDSCRSI,   # 血肌酐 99-20
                          # 第二篇文章新增列　
                          DRXTKCAL, DR1TKCAL,  # 卡路里 kcal 99-02 03-23
                          DRXTPROT, DR1TPROT,  # 蛋白质 gm 99-02 03-23
                          DRXTCARB, DR1TCARB,  # 糖类 gm 99-02 03-23
                          DRXTTFAT, DR1TTFAT,  # 总脂肪 gm 99-02 03-23
                          DRXTSFAT, DR1TSFAT,  # 总饱和脂肪酸 gm 99-02 03-23
                          DRXTMFAT, DR1TMFAT,  # 总单不饱和脂肪酸 gm 99-02 03-23
                          DRXTPFAT, DR1TPFAT,  # 总多不饱和脂肪酸 gm 99-02 03-23
                          DRXTCHOL, DR1TCHOL,  # 胆固醇 gm 99-02 03-23
                          DRXTFIBE, DR1TFIBE,  # 膳食纤维 gm 99-02 03-23
                          DRXTVARE, DRXTVARA, DR1TVARA,  # 维他命A mcg 99-00 01-02 03-23
                          
                          DRXTCARO, 
                          DRXTACAR, DRXTBCAR, 
                          DR1TACAR, DR1TBCAR,  # 胡萝卜素、alpha胡萝卜素、beta胡萝卜素 re mcg 99-00 01-02a 01-02b 03-23a 03-23b 8
                          
                          DRXTVB1, DR1TVB1,    # 维他命B1 gm 99-02 03-23
                          DRXTVB2, DR1TVB2,    # 维他命B2 gm 99-02 03-23
                          DRXTNIAC, DR1TNIAC,  # 烟酸 gm 99-02 03-23
                          DRXTVB6, DR1TVB6,    # 维他命B6 gm 99-02 03-23
                          DRXTFOLA, DR1TFOLA,  # 总叶酸 mcg 99-02 03-23
                          DRXTVB12, DR1TVB12,  # 维他命B12 gm 99-02 03-23 
                          DRXTVC, DR1TVC,      # 维他命C gm 99-02 03-23
                          DRXTVE, DRXTATOC, DR1TATOA,    # 维他命E gm 99-00 01-02 03-23
                          DRXTMAGN, DR1TMAGN,  # 镁 mg 99-02 03-23
                          DRXTIRON, DR1TIRON,  # 铁 mg 99-02 03-23
                          DRXTZINC, DR1TZINC,  # 锌 mg 99-02 03-23
                          DRXTSELE, DR1TSELE,  # 晒 mg 99-02 03-23
                          DRXTCAFF, DR1TCAFF,  # 咖啡因 mg 99-02 03-23
                          DRXTALCO, DR1TALCO,  # 酒精 gm 99-02 03-23
                          LBXWBCSI,  # 白细胞计数 1000 cells/uL 99-23
                          LBDLYMNO,  # 淋巴细胞计数 1000 cells/uL 99-23
                          LBDMONO,   # 单核细胞计数 1000 cells/uL 99-23
                          LBXPLTSI,  # 血小板计数 %SI 99-23
                          LBDNENO,   # 中性粒细胞 1000 cells/uL 99-23
                          # 酒精
                          ALQ120Q, ALQ121,    # 去年喝酒频率 99-16 17-23
                          ALQ120U,   # 上一问频率 per wk, mo, yr 99-16
                          ALQ130,    # 最近一年喝酒天数 99-23
                          ALQ140Q, ALQ141Q, ALQ142,     # 最近一年喝5杯以及上的天数 99-10 11-16 17-23
                          ALQ150, ALQ151,   # 是否曾每天喝五杯以上酒？ 99-10 11-23
                          # 吸烟
                          SMD030, # 开始有习惯性吸烟的年龄 99-20
                          SMD057, # 戒烟时每天吸烟数量 99-20
                          SMD080, SMD641, # 30天内吸烟的天数 99-02 03-23
                          SMD415, SMD460, # 家中吸烟者数量 99-12 13-23
                          SMD630, # 第一次吸完整根烟的年龄 99-23
                          SMQ020, # 一生中至少吸过100支香烟 99-23
                          SMQ040, # 你现在吸烟吗 99-23
                          SMQ050Q, # 戒烟多久了 99-20
                          SMQ050U, # 上一问的单位（天/周/月/年） 99-20
                          SMQ620, SMQ621, # 曾经尝试过吸烟 99-10 11-23
                          SMQ670, # 尝试过戒烟 99-20
                          SMQ725, # 你吸的上一支烟是什么时候 99-23
                          # 运动
                          PAD200, PAQ605, # 30天内高强度运动  99-06 07-20
                          PAD320, PAQ620, # 30天内普通强度运动 99-06 07-20
)

# datav1 （最终版）
datav1 <- data %>% select(SEQN,       # 序号 99-23 
                          RIAGENDR,   # 性别 99-23
                          RIDAGEYR,   # 年龄 99-23
                          BMXWT,      # 身高 99-23
                          BMXWAIST,   # 腰围 99-23
                          RIDRETH1,   # 人种 99-23
                          RIDEXPRG,   # 怀孕 99-23
                          LBDSUASI,   # 尿酸 99-20
                          DMDEDUC2,   # 教育 99-23
                          DMDMARTL, DMDMARTZ,  # 婚姻 99-16 17-23
                          INDFMPIR,   # 收入 99-23
                          BMXBMI,     # BMI  99-23
                          DSDCOUNT,   # 补充食物 1 （如果只问有没有的话到23年）
                          PHQ030,     # 酒精 99-23
                          DIQ010,     # 糖尿病 99-23
                          MCQ160C,    # 冠心病 99-23
                          BPQ020,     # 高血压 99-23
                          LBDTCSI,    # 胆固醇 mmol/L 99-23 7
                          LBDSCRSI,   # 血肌酐 99-20
                          # 第二篇文章新增列　
                          DRXTKCAL, DR1TKCAL,  # 卡路里 kcal 99-02 03-23 1
                          DRXTPROT, DR1TPROT,  # 蛋白质 gm 99-02 03-23 2
                          DRXTCARB, DR1TCARB,  # 糖类 gm 99-02 03-23 5
                          DRXTTFAT, DR1TTFAT,  # 总脂肪 gm 99-02 03-23 3
                          DRXTSFAT, DR1TSFAT,  # 总饱和脂肪酸 gm 99-02 03-23 4
                          DRXTMFAT, DR1TMFAT,  # 总单不饱和脂肪酸 gm 99-02 03-23
                          DRXTPFAT, DR1TPFAT,  # 总多不饱和脂肪酸 gm 99-02 03-23
                          DRXTCHOL, DR1TCHOL,  # 胆固醇 mg 99-02 03-23
                          DRXTFIBE, DR1TFIBE,  # 膳食纤维 gm 99-02 03-23 6
                          DRXTVARE, DRXTVARA, DR1TVARA,  # 维他命A RAE 99-00 01-02 03-23 8
                          
                          DRXTCARO, 
                          DRXTACAR, DRXTBCAR, 
                          DR1TACAR, DR1TBCAR,  # 胡萝卜素、alpha胡萝卜素、beta胡萝卜素 re mcg 99-00 01-02a 01-02b 03-23a 03-23b
                          
                          DRXTVB1, DR1TVB1,    # 维他命B1 mg 99-02 03-23
                          DRXTVB2, DR1TVB2,    # 维他命B2 mg 99-02 03-23
                          DRXTNIAC, DR1TNIAC,  # 烟酸 mg 99-02 03-23
                          DRXTVB6, DR1TVB6,    # 维他命B6 mg 99-02 03-23
                          DRXTFOLA, DR1TFOLA,  # 总叶酸 mcg 99-02 03-23
                          DRXTVB12, DR1TVB12,  # 维他命B12 mg 99-02 03-23 
                          DRXTVC, DR1TVC,      # 维他命C mg 99-02 03-23
                          DRXTVE, DRXTATOC, DR1TATOC,    # 维他命E mg 99-00 01-02 03-23
                          DRXTMAGN, DR1TMAGN,  # 镁 mg 99-02 03-23
                          DRXTIRON, DR1TIRON,  # 铁 mg 99-02 03-23
                          DRXTZINC, DR1TZINC,  # 锌 mg 99-02 03-23
                          DRXTSELE, DR1TSELE,  # 晒 mg 99-02 03-23
                          DRXTCAFF, DR1TCAFF,  # 咖啡因 mg 99-02 03-23
                          DRXTALCO, DR1TALCO,  # 酒精 gm 99-02 03-23
                          LBXWBCSI,  # 白细胞计数 1000 cells/uL 99-23
                          LBDLYMNO,  # 淋巴细胞计数 1000 cells/uL 99-23
                          LBDMONO,   # 单核细胞计数 1000 cells/uL 99-23
                          LBXPLTSI,  # 血小板计数 %SI 99-23
                          LBDNENO,   # 中性粒细胞 1000 cells/uL 99-23
                          # 酒精
                          ALQ100, ALD100, ALQ101, ALQ121,   # 去年喝酒是否到12次(alq121有所不同，<=5为是，>5、0为否) 99-00 01-02 03-16 17-23
                          # 吸烟
                          SMQ020, # 一生中至少吸过100支香烟 99-23
                          # 运动
                          PAD200, PAQ605, # 30天内高强度运动  99-06 07-20
                          PAD320, PAQ620, # 30天内普通强度运动 99-06 07-20
)
write.csv(datav1, "D:/data_analyse/data/20241115/datav1.csv", row.names = FALSE, na = "")

# datav2

# 每个周期的序号范围
seqn_1999_2000 <- 1:9965
seqn_2001_2002 <- 9966:21004
seqn_2003_2004 <- 21005:31126
seqn_2005_2006 <- 31127:41474
seqn_2007_2008 <- 41475:51623
seqn_2009_2010 <- 51624:62160
seqn_2011_2012 <- 62161:71916
seqn_2013_2014 <- 73557:83731
seqn_2015_2016 <- 83732:93702
seqn_2017_2018 <- 93703:102956
seqn_2019_2020 <- 109263:124822
seqn_2021_2023 <- 130378:142310

# 不同变量涉及的周期范围
seqn_2001_2023 <- c(seqn_2001_2002, seqn_2003_2004, seqn_2005_2006, seqn_2007_2008, seqn_2009_2010, seqn_2011_2012, seqn_2013_2014, seqn_2015_2016, seqn_2017_2018, seqn_2019_2020, seqn_2021_2023)
seqn_2003_2023 <- c(seqn_2003_2004, seqn_2005_2006, seqn_2007_2008, seqn_2009_2010, seqn_2011_2012, seqn_2013_2014, seqn_2015_2016, seqn_2017_2018, seqn_2019_2020, seqn_2021_2023)
seqn_2003_2016 <- c(seqn_2003_2004, seqn_2005_2006, seqn_2007_2008, seqn_2009_2010, seqn_2011_2012, seqn_2013_2014, seqn_2015_2016)
seqn_2007_2020 <- c(seqn_2007_2008, seqn_2009_2010, seqn_2011_2012, seqn_2013_2014, seqn_2015_2016, seqn_2017_2018, seqn_2019_2020)
seqn_2011_2016 <- c(seqn_2011_2012, seqn_2013_2014, seqn_2015_2016)
seqn_2011_2023 <- c(seqn_2011_2012, seqn_2013_2014, seqn_2015_2016, seqn_2017_2018, seqn_2019_2020, seqn_2021_2023)
seqn_2013_2023 <- c(seqn_2013_2014, seqn_2015_2016, seqn_2017_2018, seqn_2019_2020, seqn_2021_2023)
seqn_2017_2023 <- c(seqn_2017_2018, seqn_2019_2020, seqn_2021_2023)

datav2 <- datav1

# 合并婚姻状况列 99-16 17-23
datav2$DMDMARTL[data$SEQN %in% seqn_2017_2023] <- datav2$DMDMARTZ[data$SEQN %in% seqn_2017_2023]
datav2 <- datav2[ , !names(datav2) %in% "DMDMARTZ"]

# 合并卡路里列 99-02 03-23
datav2$DRXTKCAL[data$SEQN %in% seqn_2003_2023] <- datav2$DR1TKCAL[data$SEQN %in% seqn_2003_2023]
datav2 <- datav2[ , !names(datav2) %in% "DR1TKCAL"]

# 合并蛋白质列 99-02 03-23
datav2$DRXTPROT[data$SEQN %in% seqn_2003_2023] <- datav2$DR1TPROT[data$SEQN %in% seqn_2003_2023]
datav2 <- datav2[ , !names(datav2) %in% "DR1TPROT"]

# 合并糖类列 99-02 03-23
datav2$DRXTCARB[data$SEQN %in% seqn_2003_2023] <- datav2$DR1TCARB[data$SEQN %in% seqn_2003_2023]
datav2 <- datav2[ , !names(datav2) %in% "DR1TCARB"]

# 合并总脂肪列 99-02 03-23
datav2$DRXTTFAT[data$SEQN %in% seqn_2003_2023] <- datav2$DR1TTFAT[data$SEQN %in% seqn_2003_2023]
datav2 <- datav2[ , !names(datav2) %in% "DR1TTFAT"]

# 合并总饱和脂肪酸列 99-02 03-23
datav2$DRXTSFAT[data$SEQN %in% seqn_2003_2023] <- datav2$DR1TSFAT[data$SEQN %in% seqn_2003_2023]
datav2 <- datav2[ , !names(datav2) %in% "DR1TSFAT"]

# 合并总单不饱和脂肪酸列 99-02 03-23
datav2$DRXTMFAT[data$SEQN %in% seqn_2003_2023] <- datav2$DR1TMFAT[data$SEQN %in% seqn_2003_2023]
datav2 <- datav2[ , !names(datav2) %in% "DR1TMFAT"]

# 合并总多不饱和脂肪酸列 99-02 03-23
datav2$DRXTPFAT[data$SEQN %in% seqn_2003_2023] <- datav2$DR1TPFAT[data$SEQN %in% seqn_2003_2023]
datav2 <- datav2[ , !names(datav2) %in% "DR1TPFAT"]

# 合并胆固醇列 99-02 03-23
datav2$DRXTCHOL[data$SEQN %in% seqn_2003_2023] <- datav2$DR1TCHOL[data$SEQN %in% seqn_2003_2023]
datav2 <- datav2[ , !names(datav2) %in% "DR1TCHOL"]

# 合并膳食纤维列 99-02 03-23
datav2$DRXTFIBE[data$SEQN %in% seqn_2003_2023] <- datav2$DR1TFIBE[data$SEQN %in% seqn_2003_2023]
datav2 <- datav2[ , !names(datav2) %in% "DR1TFIBE"]

# 合并维他命A列 99-00 01-23
datav2$DRXTVARE[data$SEQN %in% seqn_2001_2002] <- datav2$DRXTVARA[data$SEQN %in% seqn_2001_2002]
datav2$DRXTVARE[data$SEQN %in% seqn_2003_2023] <- datav2$DR1TVARA[data$SEQN %in% seqn_2003_2023]
datav2 <- datav2[ , !names(datav2) %in% c("DR1TVARA", "DRXTVARA")]

# 合并胡萝卜素列 99-00 01-02a 01-02b 03-23a 03-23b
datav2$DRXTCARO[data$SEQN %in% seqn_2001_2002] <- (datav2$DRXTACAR[data$SEQN %in% seqn_2001_2002] * 0.025) + 
                                                  (datav2$DRXTBCAR[data$SEQN %in% seqn_2001_2002] * 0.5)
datav2$DRXTCARO[data$SEQN %in% seqn_2003_2023] <- (datav2$DR1TACAR[data$SEQN %in% seqn_2003_2023] * 0.025) + 
                                                  (datav2$DR1TBCAR[data$SEQN %in% seqn_2003_2023] * 0.5)
datav2 <- datav2[ , !names(datav2) %in% c("DRXTACAR", "DRXTBCAR", "DR1TACAR", "DR1TBCAR")]

# 合并维他命B1列 99-02 03-23
datav2$DRXTVB1[data$SEQN %in% seqn_2003_2023] <- datav2$DR1TVB1[data$SEQN %in% seqn_2003_2023]
datav2 <- datav2[ , !names(datav2) %in% "DR1TVB1"]

# 合并维他命B2列 99-02 03-23
datav2$DRXTVB2[data$SEQN %in% seqn_2003_2023] <- datav2$DR1TVB2[data$SEQN %in% seqn_2003_2023]
datav2 <- datav2[ , !names(datav2) %in% "DR1TVB2"]

# 合并烟酸列 99-02 03-23
datav2$DRXTNIAC[data$SEQN %in% seqn_2003_2023] <- datav2$DR1TNIAC[data$SEQN %in% seqn_2003_2023]
datav2 <- datav2[ , !names(datav2) %in% "DR1TNIAC"]

# 合并维他命B6列 99-02 03-23
datav2$DRXTVB6[data$SEQN %in% seqn_2003_2023] <- datav2$DR1TVB6[data$SEQN %in% seqn_2003_2023]
datav2 <- datav2[ , !names(datav2) %in% "DR1TVB6"]

# 合并总叶酸列 99-02 03-23
datav2$DRXTFOLA[data$SEQN %in% seqn_2003_2023] <- datav2$DR1TFOLA[data$SEQN %in% seqn_2003_2023]
datav2 <- datav2[ , !names(datav2) %in% "DR1TFOLA"]

# 合并维他命B12列 99-02 03-23
datav2$DRXTVB12[data$SEQN %in% seqn_2003_2023] <- datav2$DR1TVB12[data$SEQN %in% seqn_2003_2023]
datav2 <- datav2[ , !names(datav2) %in% "DR1TVB12"]

# 合并维他命C列 99-02 03-23
datav2$DRXTVC[data$SEQN %in% seqn_2003_2023] <- datav2$DR1TVC[data$SEQN %in% seqn_2003_2023]
datav2 <- datav2[ , !names(datav2) %in% "DR1TVC"]

# 合并维他命E列 99-00 01-02 03-23
datav2$DRXTVE[data$SEQN %in% seqn_2001_2002] <- datav2$DRXTATOC[data$SEQN %in% seqn_2001_2002]
datav2$DRXTVE[data$SEQN %in% seqn_2003_2023] <- datav2$DR1TATOC[data$SEQN %in% seqn_2003_2023]
datav2 <- datav2[ , !names(datav2) %in% c("DRXTATOC", "DR1TATOC")]

# 合并镁列 99-02 03-23
datav2$DRXTMAGN[data$SEQN %in% seqn_2003_2023] <- datav2$DR1TMAGN[data$SEQN %in% seqn_2003_2023]
datav2 <- datav2[ , !names(datav2) %in% "DR1TMAGN"]

# 合并铁列 99-02 03-23
datav2$DRXTIRON[data$SEQN %in% seqn_2003_2023] <- datav2$DR1TIRON[data$SEQN %in% seqn_2003_2023]
datav2 <- datav2[ , !names(datav2) %in% "DR1TIRON"]

# 合并锌列 99-02 03-23
datav2$DRXTZINC[data$SEQN %in% seqn_2003_2023] <- datav2$DR1TZINC[data$SEQN %in% seqn_2003_2023]
datav2 <- datav2[ , !names(datav2) %in% "DR1TZINC"]

# 合并晒列 99-02 03-23
datav2$DRXTSELE[data$SEQN %in% seqn_2003_2023] <- datav2$DR1TSELE[data$SEQN %in% seqn_2003_2023]
datav2 <- datav2[ , !names(datav2) %in% "DR1TSELE"]

# 合并咖啡因列 99-02 03-23
datav2$DRXTCAFF[data$SEQN %in% seqn_2003_2023] <- datav2$DR1TCAFF[data$SEQN %in% seqn_2003_2023]
datav2 <- datav2[ , !names(datav2) %in% "DR1TCAFF"]

# 合并酒精列 99-02 03-23
datav2$DRXTALCO[data$SEQN %in% seqn_2003_2023] <- datav2$DR1TALCO[data$SEQN %in% seqn_2003_2023]
datav2 <- datav2[ , !names(datav2) %in% "DR1TALCO"]

# 饮酒
# 合并去年喝酒是否达到12次列 99-00 01-02 03-16 17-23
datav2$ALQ100[data$SEQN %in% seqn_2001_2002] <- datav2$ALD100[data$SEQN %in% seqn_2001_2002]
datav2$ALQ100[data$SEQN %in% seqn_2003_2016] <- datav2$ALQ101[data$SEQN %in% seqn_2003_2016]
datav2$ALQ100[data$SEQN %in% seqn_2017_2023] <- datav2$ALQ121[data$SEQN %in% seqn_2017_2023]
datav2 <- datav2[ , !names(datav2) %in% c("ALD100", "ALQ101", "ALQ121")]


# 吸烟
# 合并上个月吸烟的天数列 99-02 03-23
datav2$SMD080[data$SEQN %in% seqn_2003_2023] <- datav2$SMD641[data$SEQN %in% seqn_2003_2023]
datav2 <- datav2[ , !names(datav2) %in% "SMD641"]

# 合并家中吸烟者数量列 99-12 13-23
datav2$SMD415[data$SEQN %in% seqn_2013_2023] <- datav2$SMD460[data$SEQN %in% seqn_2013_2023]
datav2 <- datav2[ , !names(datav2) %in% "SMD460"]

# 合并是否尝试过吸烟列 99-10 11-23
datav2$SMQ620[data$SEQN %in% seqn_2011_2023] <- datav2$SMQ621[data$SEQN %in% seqn_2011_2023]
datav2 <- datav2[ , !names(datav2) %in% "SMQ621"]

# 运动
# 合并30天内高强度运动列 99-06 07-20
datav2$PAD200[data$SEQN %in% seqn_2007_2020] <- datav2$PAQ605[data$SEQN %in% seqn_2007_2020]
datav2 <- datav2[ , !names(datav2) %in% "PAQ605"]

# 合并30天内普通强度运动列 99-06 07-20
datav2$PAD320[data$SEQN %in% seqn_2007_2020] <- datav2$PAQ620[data$SEQN %in% seqn_2007_2020]
datav2 <- datav2[ , !names(datav2) %in% "PAQ620"]

write.csv(datav2, "D:/data_analyse/data/20241115/datav2.csv", row.names = FALSE, na = "")

# datav3

datav3 <- datav2
colSums(is.na(datav3))

# 删除所有列都为空的行
# datav3 <- datav3 %>% filter(rowSums(!is.na(.) & . != "") > 0)
# colSums(is.na(datav3))

# 将范围框定到2020年前
datav3 <- datav3 %>% filter(SEQN <= 124822)
colSums(is.na(datav3))

# 替换所有的"5.4E-79"为NA
datav3[abs(datav3) < 1e-20] <- 0

# 选出所有 年龄 大于等于20岁的 
datav3 <- datav3 %>% filter(RIDAGEYR >= 20)  # 124822 -> 64313
colSums(is.na(datav3))

# 删除所有 体重 列为空的行  64313 -> 59930
datav3 <- datav3 %>% filter(!is.na(BMXWT) & BMXWT != "")
colSums(is.na(datav3))

# 删除所有 腰围 列为空的行  59930 -> 57333
datav3 <- datav3 %>% filter(!is.na(BMXWAIST) & BMXWAIST != "")
colSums(is.na(datav3))

# 删除所有 怀孕 为1的行，而后删除该列  57333 -> 55869
datav3 <- datav3 %>% filter(!RIDEXPRG %in% 1)
datav3 <- datav3[ , !names(datav3) %in% "RIDEXPRG"]
colSums(is.na(datav3))

# 删除所有 尿酸 列为空的行  55869 -> 52825
datav3 <- datav3 %>% filter(!is.na(LBDSUASI) & LBDSUASI != "")
colSums(is.na(datav3))

# 删除所有受教育程度为7、9的行 并清除空行  52825 -> 52764
datav3 <- datav3 %>% filter(!DMDEDUC2 %in% c(7, 9))
datav3 <- datav3 %>% filter(!is.na(DMDEDUC2) & DMDEDUC2 != "")
colSums(is.na(datav3))

# 删除所有 婚姻状况 为77、99的行 并清除空行  52764 -> 47744
datav3 <- datav3 %>% filter(!DMDMARTL %in% c(77, 99))
datav3 <- datav3 %>% filter(!is.na(DMDMARTL) & DMDMARTL != "")
colSums(is.na(datav3))

# 删除所有 收入 列为空的行  47744 -> 43558
datav3 <- datav3 %>% filter(!is.na(INDFMPIR) & INDFMPIR != "")
colSums(is.na(datav3))

# 删除所有 BMI 列为空的行  43558->43446
datav3 <- datav3 %>% filter(!is.na(BMXBMI) & BMXBMI != "")
colSums(is.na(datav3))

# 删除所有 补充膳食 为77、99的行 并清除空行  43446 -> 43429
datav3 <- datav3 %>% filter(!DSDCOUNT %in% c(77, 99))
datav3 <- datav3 %>% filter(!is.na(DSDCOUNT) & DSDCOUNT != "")
colSums(is.na(datav3))

# 删除 PHQ030 列，因其并非饮酒相关列 
datav3 <- datav3[ , !names(datav3) %in% "PHQ030"]
colSums(is.na(datav3))

# 删除所有 糖尿病 为9的行 清除空行 并将边界值定义为不患有糖尿病  43429 -> 43410
datav3 <- datav3 %>% filter(!DIQ010 %in% 9)
datav3 <- datav3 %>% filter(!is.na(DIQ010) & DIQ010 != "")
datav3 <- datav3 %>% mutate(DIQ010 = if_else(DIQ010 == 3, 2, DIQ010))
colSums(is.na(datav3))

# 删除所有 冠心病 为7、9的行 并清除空行  43410 -> 43258
datav3 <- datav3 %>% filter(!MCQ160C %in% c(7, 9))
datav3 <- datav3 %>% filter(!is.na(MCQ160C) & MCQ160C != "")
colSums(is.na(datav3))

# 删除所有 高血压 为7、9的行 并清除空行  43258 -> 43106
datav3 <- datav3 %>% filter(!BPQ020 %in% c(7, 9))
datav3 <- datav3 %>% filter(!is.na(BPQ020) & BPQ020 != "")
colSums(is.na(datav3))

# 删除所有 胆固醇 列为空的行  43106 -> 43085
datav3 <- datav3 %>% filter(!is.na(LBDTCSI) & LBDTCSI != "")
colSums(is.na(datav3))

# 删除所有 血肌酐 列为空的行  43085 -> 43084
datav3 <- datav3 %>% filter(!is.na(LBDSCRSI) & LBDSCRSI != "")
colSums(is.na(datav3))

# 删除所有 卡路里 到 酒精 列为空的行（这些列缺失值位置一样，故只清理一列） 43084->41202
datav3 <- datav3 %>% filter(!is.na(DRXTKCAL) & DRXTKCAL != "")
colSums(is.na(datav3))

# 删除所有 白细胞 列为空的行  41202 -> 41137
datav3 <- datav3 %>% filter(!is.na(LBXWBCSI) & LBXWBCSI != "")
colSums(is.na(datav3))

# 删除所有 淋巴细胞 列为空的行 （单核细胞、血小板、中性粒细胞同时清除）  41137 -> 41058
datav3 <- datav3 %>% filter(!is.na(LBDLYMNO) & LBDLYMNO != "")
colSums(is.na(datav3))

# 删除所有 去年喝酒频率 列为9、7、99、77的行 并删除空行（该变量datav4以12界定）  41058 -> 38890
datav3 <- datav3 %>% filter(!(SEQN < 93703 & ALQ100 %in% c(7, 9)))
datav3 <- datav3 %>% filter(!(SEQN >= 93703 & ALQ100 %in% c(77, 99)))
datav3 <- datav3 %>% filter(!is.na(ALQ100) & ALQ100 != "")
# 对17-23数据进行整理
datav3$ALQ100[datav3$SEQN >= 93703] <- 
  ifelse(datav3$ALQ100[datav3$SEQN >= 93703 & !is.na(datav3$ALQ100)] >= 8, 0,  # 如果 ALQ >= 6，变为 0
         ifelse(datav3$ALQ100[datav3$SEQN >= 93703 & !is.na(datav3$ALQ100)] %in% c(2, 3, 4, 5, 6, 7), 1,  # 如果 ALQ 是 2、3、4、5，变为 1
                datav3$ALQ100[datav3$SEQN >= 93703 & !is.na(datav3$ALQ100)]))  # 否则不做改变
datav3$ALQ100[datav3$SEQN >= 93703] <- 
  ifelse(datav3$ALQ100[datav3$SEQN >= 93703 & !is.na(datav3$ALQ100)] == 0, 2, # 如果 ALQ 为0，变为 2
                datav3$ALQ100[datav3$SEQN >= 93703 & !is.na(datav3$ALQ100)])  # 否则不做改变
colSums(is.na(datav3))

# 删除所有 一生至少吸过100支烟 为7、9的行 并清除空行  38890 -> 38871
datav3 <- datav3 %>% filter(!SMQ020 %in% c(7, 9))
datav3 <- datav3 %>% filter(!is.na(SMQ020) & SMQ020 != "")
colSums(is.na(datav3))

# 删除所有 30天内高强度运动 为7、9的行 并清除空行  38871 -> 38867
datav3 <- datav3 %>% filter(!PAD200 %in% c(7, 9))
datav3 <- datav3 %>% filter(!is.na(PAD200) & PAD200 != "")
colSums(is.na(datav3))

# 删除所有 30天内普通强度运动 为7、9的行 并清除空行  38867 -> 38858
datav3 <- datav3 %>% filter(!PAD320 %in% c(7, 9))
datav3 <- datav3 %>% filter(!is.na(PAD320) & PAD320 != "")
colSums(is.na(datav3))

# 为表一计算的单位换算
# datav3 <- datav3 %>% mutate(DRXTVB12 = DRXTVB12 / 1000)
# datav3 <- datav3 %>% mutate(DRXTFOLA = DRXTFOLA / 1000)
# datav3 <- datav3 %>% mutate(DRXTSELE = DRXTSELE / 1000)
# datav3 <- datav3 %>% mutate(DRXTCAFF = DRXTCAFF / 1000)

write.csv(datav3, "D:/data_analyse/data/20241115/datav3.csv", row.names = FALSE, na = "")

# datav4 38858

datav4 <- datav3

datav4 <- datav4 %>%
  mutate(
    # 性别
    Sex = case_when(
      RIAGENDR == 1 ~ "Male",
      RIAGENDR == 2 ~ "Female"
    ),
    
    # 年龄
    AgeGroup = case_when(
      RIDAGEYR >= 20 & RIDAGEYR < 30 ~ '20-29',
      RIDAGEYR >= 30 & RIDAGEYR < 40 ~ '30-39',
      RIDAGEYR >= 40 & RIDAGEYR < 50 ~ '40-49',
      RIDAGEYR >= 50 & RIDAGEYR < 60 ~ '50-59',
      RIDAGEYR >= 60 & RIDAGEYR < 70 ~ '60-69',
      RIDAGEYR >= 70 & RIDAGEYR < 80 ~ '70-79',
      RIDAGEYR >= 80 ~ '80+'
    ),
    
    # 种族
    Race = case_when(
      RIDRETH1 == 3 ~ "Non-Hispanic white",
      RIDRETH1 == 4 ~ "Non-Hispanic black",
      RIDRETH1 == 1 ~ "Mexican American",
      RIDRETH1 == 2 ~ "Others",
      RIDRETH1 == 5 ~ "Others"
    ),
    
    # 教育水平
    Education = case_when(
      DMDEDUC2 == 1 ~ "Some high school",
      DMDEDUC2 == 2 ~ "Some high school",
      DMDEDUC2 == 3 ~ "High school or GED",
      DMDEDUC2 == 4 ~ "Some college",
      DMDEDUC2 == 5 ~ "College graduate"
    ),
    
    # 婚姻状况
    Marital = case_when(
      DMDMARTL == 1 ~ "Married or living with a partner",
      DMDMARTL == 6 ~ "Married or living with a partner",
      DMDMARTL == 2 ~ "Living alone",
      DMDMARTL == 3 ~ "Living alone",
      DMDMARTL == 4 ~ "Living alone",
      DMDMARTL == 5 ~ "Living alone"
    ),
    
    # 家庭收入
    Income = case_when(
      INDFMPIR <= 1 ~ "≤ 1.0",
      INDFMPIR > 1 & INDFMPIR < 2 ~ "1.0 to 2.0",
      INDFMPIR >= 2 ~ "＞ 2.0"
    ),
    
    # BMI
    BMI = case_when(
      BMXBMI < 18.5 ~ "Underweight",
      BMXBMI >= 18.5 & BMXBMI < 24 ~ "Normal weight",
      BMXBMI >= 24 & BMXBMI < 28 ~ "Overweight",
      BMXBMI >= 28 ~ "Obesity"
    ),
    
    # BMI median Q1 Q3
    
    
    # 肥胖
    Obesity = ifelse(
      (Sex == "Male" & BMXWAIST >= 90) |
        (Sex == "Female" & BMXWAIST > 85),
      "Yes", "No"
    ),
    
    # 补充膳食
    DSD = ifelse(
      DSDCOUNT == 0, "NO", "YES"
    ),
    
    # 卡路里消耗 median Q1 Q3
    
    
    # 酒精
    Alchohol = case_when(
      ALQ100 == 2 ~ "NO",
      ALQ100 == 1 ~ "YES"
    ),
    
    # 高尿酸血症
    LBDSUASImg = LBDSUASI / 60,
    Hyperuricemia = ifelse(
      (Sex == "Male" & LBDSUASImg > 7.0) |
        (Sex == "Female" & LBDSUASImg > 6.0),
      "Yes", "No"
    ),
    
    # 高尿酸血症
    hyperuricemia_count = ifelse(
      (Sex == "Male" & LBDSUASImg > 7.0) |
        (Sex == "Female" & LBDSUASImg > 6.0),
      1, 0
    ),
    
    # 糖尿病
    Diabetes = case_when(
      DIQ010 == 2 ~ "No",
      DIQ010 == 1 ~ "Yes"
    ),
    
    # 高胆固醇血症
    Hypercholesterolemia = case_when(
      LBDTCSI < 5.2 ~ "No",
      LBDTCSI >= 5.2 ~ "Yes"
    ),
    
    # 高血压
    Hypertension = case_when(
      BPQ020 == 2 ~ "No",
      BPQ020 == 1 ~ "Yes"
    ),
    
    # 冠心病
    HeartDisease = case_when(
      MCQ160C == 2 ~ "No",
      MCQ160C == 1 ~ "Yes"
    ),
    
    # CKD-EPI公式计算eGFR 
    eGFR = case_when(
      Sex == "Female" & (LBDSCRSI / 88.4) <= 0.7 ~ 141 * ((LBDSCRSI / 88.4) / 0.7) ^ -0.329 * (0.993 ^ RIDAGEYR) * 1.018 * ifelse(Race == "Non-Hispanic black", 1.159, 1),
      Sex == "Female" & (LBDSCRSI / 88.4) > 0.7  ~ 141 * ((LBDSCRSI / 88.4) / 0.7) ^ -1.209 * (0.993 ^ RIDAGEYR) * 1.018 * ifelse(Race == "Non-Hispanic black", 1.159, 1),
      Sex == "Male" & (LBDSCRSI / 88.4) <= 0.9    ~ 141 * ((LBDSCRSI / 88.4) / 0.9) ^ -0.411 * (0.993 ^ RIDAGEYR) * ifelse(Race == "Non-Hispanic black", 1.159, 1),
      Sex == "Male" & (LBDSCRSI / 88.4) > 0.9     ~ 141 * ((LBDSCRSI / 88.4) / 0.9) ^ -1.209 * (0.993 ^ RIDAGEYR) * ifelse(Race == "Non-Hispanic black", 1.159, 1)
    ),
    
    # GFR
    GFR = case_when(
      eGFR >= 90 ~ "GFR ≥ 90mL/min",
      eGFR >= 60 & eGFR < 90 ~ "GFR 60 to 89mL/min",
      eGFR >= 30 & eGFR < 60 ~ "GFR 30 to 59mL/min",
      eGFR < 30 ~ "GFR < 30mL/min"
    ),
    
    # SII 血小板x中性粒细胞÷淋巴细胞
    SII = LBXPLTSI * LBDNENO / LBDLYMNO,
    
    # SIRI 单核细胞x中性粒细胞÷淋巴细胞
    SIRI = LBDMONO * LBDNENO / LBDLYMNO,
    
    # 是否吸烟
    smoke_count = case_when(
      SMQ020 == 2 ~ 0,
      SMQ020 == 1 ~ 1
    ),
    
    # 糖尿病计数
    diabetes_count = case_when(
      DIQ010 == 2 ~ 0,
      DIQ010 == 1 ~ 1
    ),
    
    # 高胆固醇血症计数
    hypercholesterolemia_count = case_when(
      LBDTCSI < 5.2 ~ 0,
      LBDTCSI >= 5.2 ~ 1
    ),
    
    # 高血压计数
    hypertension_count = case_when(
      BPQ020 == 2 ~ 0,
      BPQ020 == 1 ~ 1
    ),
    
    # 冠心病计数
    heartdisease_count = case_when(
      MCQ160C == 2 ~ 0,
      MCQ160C == 1 ~ 1
    ),
  )
write.csv(datav4, "D:/data_analyse/data/20241115/datav4.csv", row.names = FALSE, na = "")





# datav4_gout  20281

datav4_gout <- datav4[datav4$SEQN >= 41474 & datav4$SEQN <= 102956, ]

# 从data中筛选出07-18年的痛风数据
gout <- data[data$SEQN >= 41474 & data$SEQN <= 102956, c("SEQN", "MCQ160N")]

# 将该数据合并至datav3_gout中，按SEQN列对齐
datav4_gout <- merge(datav4_gout, gout[, c("SEQN", "MCQ160N")], by = "SEQN", all.x = TRUE)
colSums(is.na(datav4_gout))

# 删除痛风列为7、9的行 
datav4_gout <- datav4_gout %>% filter(!MCQ160N %in% c(7, 9))

# 筛选痛风
datav4_gout <- datav4_gout %>%
  mutate(
    Gout = case_when(
      MCQ160N == 2 ~ "No",
      MCQ160N == 1 ~ "Yes"
    ),
    gout_count = case_when(
      MCQ160N == 2 ~ 0,
      MCQ160N == 1 ~ 1
    )
  )
write.csv(datav4_gout, "D:/data_analyse/data/20241115/datav4_gout.csv", row.names = FALSE, na = "")































# 为病例选择流程图提取痛风患病数量
gout <- data[data$SEQN >= 41474 & data$SEQN <= 93702, c("SEQN", "MCQ160N", "RIDAGEYR")]
write.csv(gout, "D:/data_analyse/data/20241115/data_only_gout.csv", row.names = FALSE, na = "")


# 为病例选择流程图在痛风基础之上找高尿酸数量
hyperuricemia <- data_dii_07_18[, c("Sex", "hyperuricemia_count")]
write.csv(hyperuricemia, "D:/data_analyse/data/20241115/data_only_hyperuricemia.csv", row.names = FALSE, na = "")

