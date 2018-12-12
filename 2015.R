# 09. 데이터 분석 프로젝트
# 09-1. '한국복지패널데이터' 분석 준비하기

install.packages("foreign") # foreign 패키지 설치 
library(foreign) # SPSS 파일 로드 
library(dplyr) # 전처리 
library(ggplot2) # 시각화 
library(readxl) # 엑셀 파일 불러오기

raw_welfare <- read.spss(file = "Koweps_hpc10_2015_beta3.sav", to.data.frame = T)
welfare <- raw_welfare

# 데이터 검토하기 
head(welfare) 
tail(welfare) 
View(welfare) 
dim(welfare) 
str(welfare) 
summary(welfare)

# 변수명 바꾸기 
welfare <- rename(welfare, sex = h10_g3, # 성별 
                  birth = h10_g4, # 태어난 연도 
                  marriage = h10_g10, # 혼인 상태 
                  religion = h10_g11, # 종교 
                  income = p1002_8aq1, # 월급 
                  code_job = h10_eco9, # 직종 코드 
                  code_region = h10_reg7) # 지역 코드

str(welfare)
View(welfare$sex)


# 09-2. 성별에 따른 월급 차이
- "성별에 따라 월급이 다를까?"

class(welfare$sex)
table(welfare$sex)

# 이상치 결측 처리 
welfare$sex <- ifelse(welfare$sex == 9, NA, welfare$sex)

# 결측치 확인 
table(is.na(welfare$sex))

welfare$sex <- ifelse(welfare$sex == 1, "male", "female")  # 1대신 남자, 2대신 여자

table(welfare$sex)
qplot(welfare$sex)

# 월급 변수 검토 및 전처리
# 1. 변수 검토하기

class(welfare$income)
summary(welfare$income)

qplot(welfare$income)
qplot(welfare$income) + xlim(0, 1000)  # 좀 더 자세히 보자

# 2. 전처리
# 이상치 확인  : 수입은 공개하지 않은 사람도 많다. 
summary(welfare$income)

# 이상치 결측 처리
welfare$income <- ifelse(welfare$income %in% c(0, 9999), NA, welfare$income)

# 결측치 확인
table(is.na(welfare$income))  # NA가 12044개

# 성별에 따른 월급 차이 분석하기
# 1. 성별 월급 평균표 만들기
sex_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(sex) %>%
  summarise(mean_income = mean(income))

sex_income
ggplot(data = sex_income, aes(x = sex, y = mean_income)) + geom_col()

# 09-3. 나이와 월급의 관계
# - "몇 살 때 월급을 가장 많이 받을까?"   pdf 17쪽


class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)


summary(welfare$birth)
table(is.na(welfare$birth))

welfare$birth <- ifelse(welfare$birth == 9999, NA, welfare$birth)
table(is.na(welfare$birth))

welfare$age <- 2015 - welfare$birth + 1
summary(welfare$age)

qplot(welfare$age)

age_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(age) %>%
  summarise(mean_income = mean(income))
head(age_income)

ggplot(data = age_income, aes(x = age, y = mean_income)) + geom_line()

#09-4. 연령대에 따른 월급 차이
#- "어떤 연령대의 월급이 가장 많을까?"
welfare <- welfare %>%
  mutate(ageg = ifelse(age < 30, "young",
                       ifelse(age <= 59, "middle", "old")))

table(welfare$ageg)
qplot(welfare$ageg)

ageg_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(ageg) %>%
  summarise(mean_income = mean(income))
ageg_income

ggplot(data = ageg_income, aes(x = ageg, y = mean_income)) + geom_col()

ggplot(data = ageg_income, aes(x = ageg, y = mean_income)) +
  geom_col() +
  scale_x_discrete(limits = c("young", "middle", "old"))

#09-5. 연령대 및 성별 월급 차이
#- "성별 월급 차이는 연령대별로 다를까?"

sex_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg, sex) %>% 
  summarise(mean_income = mean(income)) 
sex_income 

ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) + 
  geom_col() + 
  scale_x_discrete(limits = c("young", "middle", "old"))

ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) + 
  geom_col(position = "dodge") + 
  scale_x_discrete(limits = c("young", "middle", "old"))

sex_age <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age, sex) %>% 
  summarise(mean_income = mean(income)) 
head(sex_age) 

ggplot(data = sex_age, aes(x = age, y = mean_income, col = sex)) + geom_line()





















