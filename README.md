# estate

library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

#건물통합정보_마스터 파일 불러오기
FAC <- read.table("C:/Users/ojh38/OneDrive/바탕 화면/도시빅데이터응용/0602_팀플2/FAC.txt",
                header = T,
                sep = ",",
                colClasses = c("UFID"="character", "GRND_FLR"="character", "UGRND_FLR"="character",
                               "PNU"="character", "ARCHAREA"="character", "TOTALAREA"="character",
                               "PLATAREA"="character", "HEIGHT"="character", "STRCT_CD"="character",
                               "USABILITY"="character", "BC_RAT"="character", "VL_RAT"="character",
                               "BLDRGST_PK"="character", "USEAPR_DAY"="character", "REGIST_DAY"="character",
                               "GB_CD"="character", "VIOL_BD_YN" = "character", "GEOIDN"="character",
                               "BD_MGT_SN"="character", "SGG_OID"="character", "COL_ADM_SE"="character"),
                fileEncoding = "UTF-8")


#도로명주소_건물 파일 불러오기(csv파일로 gis에서 저장해야 행 개수 똑같이 불러와짐)
TL <- read.csv("C:/Users/ojh38/OneDrive/바탕 화면/도시빅데이터응용/0602_팀플2/0603_TL.txt",
               header = TRUE,
               sep = ",",
               fill = TRUE,
               colClasses = c("BDTYP_CD"="character", "BD_MGT_SN"="character", "BUL_MAN_NO"="character",
                              "EMD_CD"="character", "LI_CD"="character", "LNBR_MNNM"="character", "LNBR_SLNO"="character",
                              "SIG_CD"="character", "GRO_FLO_CO"="character", "UND_FLO_CO"="character",
                              "Shape_Length"="character", "Shape_Area"="character"),
               fileEncoding = "UTF-8",
               stringsAsFactors = FALSE)


#연속지적도 파일 불러오기
LSMD1 <- read.csv("C:/Users/ojh38/OneDrive/바탕 화면/도시빅데이터응용/0602_팀플2/LSMD_CONT_LDREG_11_202505.csv",
                   header = T,
                   sep = ",",
                   colClasses = c("SGG_OID"="character", "JIBUN"="character",
                                  "PNU"="character", "COL_ADM_SE"="character",
                                  "BCHK"="character"),
                   fileEncoding = "UTF-8")

LSMD <- read.table("C:/Users/ojh38/OneDrive/바탕 화면/도시빅데이터응용/0602_팀플2/LSMD_SHAPE.txt",
                  header = T,
                  sep = ",",
                  colClasses = c("SGG_OID"="character", "JIBUN"="character",
                                 "PNU"="character", "COL_ADM_SE"="character",
                                 "BCHK"="character", "Shape_Length"="character", "Shape_Area"="character"),
                  fileEncoding = "UTF-8")

#지목이 누락된 LSMD의 지번열을 LSMD1의 데이터로 바꾸기
LSMD[, 2] <- LSMD1[, 2]



#토지이용 파일 불러오기
LU <- read.table("C:/Users/ojh38/OneDrive/바탕 화면/도시빅데이터응용/0602_팀플2/ABPD_LAND_USE_STS_서울/ABPD_LAND_USE_STS_11_202505.txt",
                      header = T,
                      sep = "|",
                      colClasses = c("ADM_SECT_CD"="character", "LAND_LOC_CD"="character",
                                     "LEDG_GBN"="character", "BOBN"="character", "BUBN"="character",
                                     "MNG_NO"="character", "REM"="character", "COL_ADM_SECT_CD"="character",
                                     "USE_ZONE_ZONE_CD"="character"),
                      fileEncoding = "CP949")


#주소 파일 불러오기
SEOUL <- read.table("C:/Users/ojh38/OneDrive/문서/카카오톡 받은 파일/rnaddrkor_seoul.txt",
                   sep = "|",
                   header = TRUE,
                   fileEncoding = "CP949",
                   fill = TRUE,
                   colClasses = c(
                     "BULD_MGMT_NO" = "character",
                     "BJDONG_CD" = "character",
                     "SIDO_NM" = "character",
                     "SIGUNGU_NM" = "character",
                     "EMD_NM" = "character",
                     "RI_NM" = "character",
                     "BOBN" = "character",
                     "BUBN" = "character",
                     "RN_CD" = "character",
                     "ROAD_NM" = "character",
                     "UNDER_YN" = "character",
                     "BULD_MNNM" = "character",
                     "BULD_SLNO" = "character",
                     "ADSTRD_CD" = "character",
                     "ADMDONG_NM" = "character",
                     "ZIP" = "character",
                     "ZIP_SEQ_NO" = "character",
                     "ROAD_ADDR_DT" = "character",
                     "BLD_SE_CD" = "character",
                     "APT_SE_CD" = "character",
                     "BULD_NM" = "character",
                     "DETAIL_BLD_NM" = "character",
                     "CHG_RSN_CD" = "character"
                   ))
                  

#A.
#TL 파일 편집

#TL_1로 TL 재지정
TL_1 <- TL

#TL_1에서 산지번 +1
TL_1$MNTN_YN <- TL_1$MNTN_YN + 1

#LNBR_MNNM, LNBR_SLNO 열을 4자리로 맞추되, 왼쪽에 0을 채워줌
library(stringr)
TL_1$LNBR_MNNM <- str_pad(TL_1$LNBR_MNNM, width = 4, side = "left", pad = "0")
TL_1$LNBR_SLNO <- str_pad(TL_1$LNBR_SLNO, width = 4, side = "left", pad = "0")

#TL_1에서 도로명주소건물관리, 지상층수, 지하층수, 건물 둘레길이, 건물용도만 추출
TL_1 <- TL_1[, c(1, 2, 15, 30, 31)]

#칼럼명 중복을 막기 위해 TL_1의 건물 둘레길이 Shape_Length 컬럼명을 BLD_Shape_Length으로 바꾸기
names(TL_1)[names(TL_1) == "Shape_Length"] <- "BLD_Shape_Length"




#FAC 파일 편집

#FAC_1로 FAC 재지정
FAC_1 <- FAC

#FAC_1에서 건축물대장PK, UFID, GEOIDN, 건축면적, 연면적, 건폐율, 용적률, 높이, 구조, 준공일자만 추출
FAC_1 <- FAC_1[, c(1, 6, 7, 8, 10, 11, 13, 14, 15, 16, 20, 24)]



#TL_1과 FAC_1를 BD_MGT_SN 기준으로 조인
install.packages("dplyr")
library(dplyr)
BLD <- inner_join(TL_1, FAC_1, by = "BD_MGT_SN")

#중복되는 행 제거
BLD <- BLD[!duplicated(BLD), ]



#결측치 제거
#건축면적, 연면적, 건폐율, 용적률 값이 모두 0이 아닌 행만 추출

library(dplyr)

BLD_cleaned <- BLD %>%
  filter(ARCHAREA != 0,
         TOTALAREA != 0,
         BC_RAT != 0,
         VL_RAT != 0)


# 빈 문자열을 NA로 변환
BLD_cleaned[BLD_cleaned == ""] <- NA

#건축물이 없는 행 삭제
library(dplyr)

BLD_cleaned <- BLD_cleaned %>%
  filter(
    !is.na(STRCT_CD),
    !is.na(USEAPR_DAY),
    !(GRO_FLO_CO == 0 & UND_FLO_CO == 0),
    !(HEIGHT != 0 & GRO_FLO_CO == 0),
    !(HEIGHT == 0 & GRO_FLO_CO != 0)
  ) %>%
  distinct()


#중복되는 행 제거
BLD_cleaned <- BLD_cleaned[!duplicated(BLD_cleaned), ]




#B.
#LU 편집

LU_1 <- LU

#grepl()로 하위 용도지역에 해당하는 행 추출 -> LU 데이터프레임으로 지정
LU_1 <- LU_1[grepl("UQA11[1-2]|UQA12[1-3]|UQA130|UQA2[1-4]0|UQA3[1-3]0|UQA4[1-3]0|UQB[1-3]00|UQ[C-D]001", LU_1$USE_ZONE_ZONE_CD),]

#용도지역 파일에서 PNU 합치기(5열 합치기, tidyr라이브러리 켜기)
library(tidyr)
LU_1 <- unite(LU_1, 1:5, col = "PNU", sep = "")

#중복되는 행 제거
LU_1 <- LU_1[!duplicated(LU_1), ]

#LU에서 PNU, SPA(USE_ZONE_ZONE_CD)만 추출
LU_1 <- LU_1[, c(1, 4)]



#LSMD 파일 편집

#LSMD_1로 LSMD 재지정
LSMD_1 <- LSMD

#LSMD_1에서 지목 문자열 추출
library(stringr)
LSMD_1$Land_Category <- str_sub(LSMD_1$JIBUN, -1)

#LSMD_1에서 PNU, Shape_Length, Shpae_Area, Land_Category만 추출
LSMD_1 <- LSMD_1[, c(4, 6, 7, 8)]



#LSMD와 LU합치기
LAND <- inner_join(LU_1, LSMD_1, by = "PNU")

#1. 지목에서 숫자인 행 삭제1
LAND <- LAND[!grepl("^[0-9]+$", LAND$Land_Category), ]

#2. (선택)지목열에서 숫자로 된 것 빼기2(무슨 숫자인지 알 때)
unique(LAND$Land_Category)
LAND <- LAND %>%
  filter(!(Land_Category %in% c("2", "5", "6")))

#중복되는 행 제거
LAND <- LAND[!duplicated(LAND), ]

#NA값 제거
LAND <- na.omit(LAND)

#(선택)만약 특정 열에서 NA값만 지우고 싶다?-> PNU열에서 NA일때 행 삭제 코드
LAND <- LAND[!is.na(LAND$PNU), ]




#주소 파일 편집
#도로주소명 합치기
SEOUL <- unite(SEOUL, "SIDO_NM", "SIGUNGU_NM", "ROAD_NM", "BULD_MNNM", col = "도로주소명", sep = " ", remove = FALSE)

#필지주소명 합치기
SEOUL$"필지주소명" <- paste0(
  SEOUL$SIDO_NM, " ",
  SEOUL$SIGUNGU_NM, " ",
  SEOUL$EMD_NM, " ",
  ifelse(SEOUL$MNTN_YN == 1, "산", ""),  # 조건부 '산'
  SEOUL$BOBN, "-",
  SEOUL$BUBN
)

#BOBN, BUBN 열을 4자리로 맞추되, 왼쪽에 0을 채워줌
library(stringr)
SEOUL$BOBN <- str_pad(SEOUL$BOBN, width = 4, side = "left", pad = "0")
SEOUL$BUBN <- str_pad(SEOUL$BUBN, width = 4, side = "left", pad = "0")


#산지번 +1
SEOUL$MNTN_YN <- SEOUL$MNTN_YN + 1

#PNU 합치기
library(tidyr)
SEOUL <- unite(SEOUL, BJDONG_CD, MNTN_YN, BOBN, BUBN, col = "PNU", sep = "")


#SEOUL에서 PNU, 두 개의 주소만 추출
SEOUL <- SEOUL[, c(2, 3, 23)]



#LAND와 SEOUL합치기
LAND_final <- merge(LAND, SEOUL, by = "PNU")

#중복되는 행 제거
LAND_final <- LAND_final[!duplicated(LAND_final), ]

#BLD_cleaned와 LAND합치기
FINAL <- merge(BLD_cleaned, LAND_final, by = "PNU")

#중복되는 행 제거
FINAL <- FINAL[!duplicated(FINAL), ]

#결측치가 존재하는 행 제거
FINAL <- na.omit(FINAL)

#오름차순 정렬
library(data.table)
FINAL <- setorder(FINAL, "BD_MGT_SN")

#건폐율>=용적률인 행만 남기기
library(dplyr)
FINAL %>% filter(ARCHAREA > TOTALAREA)
invalid_area <- FINAL %>%
  filter(as.numeric(ARCHAREA) > as.numeric(TOTALAREA))
FINAL <- FINAL[as.numeric(FINAL$ARCHAREA) <= as.numeric(FINAL$TOTALAREA), ]




#other data: 실거래가 파일 불러오기
real_estate <- read.csv("C:/Users/ojh38/OneDrive/문서/카카오톡 받은 파일/서울시 부동산 실거래가 정보 (1).csv", fileEncoding = "euc-kr")


# 계약일을 날짜 형식으로 변환
real_estate$계약일 <- as.Date(as.character(real_estate$계약일), format = "%Y%m%d")

#본번, 부번 자리를 4자리로 만들어주기기
library(stringr)
real_estate$본번 <- str_pad(real_estate$본번, width = 4, side = "left", pad = "0")
real_estate$부번 <- str_pad(real_estate$부번, width = 4, side = "left", pad = "0")

#real_estate 파일에 pnu 항목 만들어주기기
real_estate <- unite(real_estate, c(2, 4, 6, 8, 9), col = "PNU", sep = "")

#pnu로 merged_data와 real_estate 병합
real_final <- merge(FINAL, real_estate, by = "PNU")


#실거래 금액 박스플롯
library(ggplot2)
ggplot(real_final, aes(x = 자치구명, y = `물건금액.만원.`)) +
  geom_boxplot() +
  labs(title = "자치구별 실거래금액 분포", x = "자치구", y = "실거래금액 (만원)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#각 자치구별 Q1, Q3, IQR 계산
iqr_summary <- real_final %>%
  group_by(자치구명) %>%
  summarise(
    Q1 = quantile(`물건금액.만원.`, 0.25, na.rm = TRUE),
    Q3 = quantile(`물건금액.만원.`, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1
  ) %>%
  arrange(desc(IQR))

#IQR 꺾은선그래프
ggplot(iqr_summary, aes(x = reorder(자치구명, -IQR), y = IQR, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "자치구별 IQR (Q3 - Q1)", x = "자치구", y = "IQR (만원)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#IQR이 가장 큰 행정구 찾기
top_gu <- iqr_summary$자치구명[1]

#해당 행정구만 남긴 데이터 필터링
top_data <- real_final %>%
  filter(자치구명 == top_gu)


#CSV로 저장
write.table(top_data,
            file = "C:/Users/ojh38/OneDrive/바탕 화면/도시빅데이터응용/top_data.csv",
            sep = ",",
            quote = FALSE,
            row.names = FALSE,
            col.names = TRUE,
            append = FALSE,
            na = "NA",
            fileEncoding = "UTF-8")

#txt로 저장
write.table(top_data,
            file = "C:/Users/ojh38/OneDrive/바탕 화면/도시빅데이터응용/top_data.txt",
            sep = "|",
            quote = FALSE,
            row.names = FALSE,
            col.names = TRUE,
            append = FALSE,
            na = "NA",
            fileEncoding = "UTF-8")

