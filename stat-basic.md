통계학개론 출석수업과제 2022
================

## 1번 문제

``` r
score <- c(88, 83, 83, 85, 94, 88, 91, 96, 89, 83, 81, 80, 84, 89, 83, 79) 
hist(score, freq = NULL, main="점수 히스토그램")
```

![](stat-basic_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
boxplot(score, ylab="Score", main="상자그림")
```

![](stat-basic_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

``` r
fivenum(score)
```

    ## [1] 79.0 83.0 84.5 89.0 96.0

## 2번 문제

1.  통계적 가설 H0 : m = 32.3 H1 : m != 32.3
2.  유의수준 𝛼 = 0.05
3.  검정통계량 계산
4.  검정통계량에 따른 유의확률계산(P 값)계산
5.  유의확률이 유의수준보다 낮으면 (p값＜𝛼) H0 를 기각하고 H1 을
    채택한다 결과: 대립가설 기각 못함

``` r
num <- c(25, 41, 35, 8, 52, 23, 32, 37, 42, 28)
t.test(num, m=32.3, alternative="two.sided")
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  num
    ## t = 0, df = 9, p-value = 1
    ## alternative hypothesis: true mean is not equal to 32.3
    ## 95 percent confidence interval:
    ##  23.58392 41.01608
    ## sample estimates:
    ## mean of x 
    ##      32.3

## 3번 문제

대응표본 t 검정 운동전후 체중감량 확인 결과 : 대립가설 기각 불가

``` r
before <- c(80, 56, 49, 82, 70)
after  <- c(76, 55, 52, 79, 72)
t.test(before, after, alternative="less", paired=TRUE)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  before and after
    ## t = 0.43994, df = 4, p-value = 0.6586
    ## alternative hypothesis: true difference in means is less than 0
    ## 95 percent confidence interval:
    ##      -Inf 3.507451
    ## sample estimates:
    ## mean of the differences 
    ##                     0.6

## 4번 문제

일원배치법 aov 적용 귀무가설 : 직업군에 따라서 월급의 평균이 다르지 않다
대립가설 : 직업군에 따라서 월급의 평균이 다르다 결과 : 귀무가설 기각

``` r
x <- c(269, 196, 254, 226, 215, 228, 251, 217, 260, 240,
       320, 281, 336, 303, 294, 354, 315, 259,
       283, 268, 357, 325, 288, 295, 272, 245, 275, 246, 341)
A <- c(rep(1,10), rep(2,8), rep(3,11))
aovdata <- data.frame(x, A)
aovmodel <- aov(x ~ A, data=aovdata)
summary(aovmodel)
```

    ##             Df Sum Sq Mean Sq F value  Pr(>F)   
    ## A            1  15102   15102   11.26 0.00236 **
    ## Residuals   27  36202    1341                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## 5번 문제

독립성 검정 귀무가설: 백신의 종류와 부작용에 관계가 없다 대립가설:
백신의 종류와 부작용의 관계가 있다 결과 : 귀무가설을 기각하지 못함

``` r
kind <- c(rep("A", 30), rep("B",30))
effect <- c(rep("Y",18),rep("N",12), rep("Y",15), rep("N",15))
chisq.test(x=kind, y=effect, correct=F)
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  kind and effect
    ## X-squared = 0.60606, df = 1, p-value = 0.4363

## 6번 문제

회귀분석

``` r
before <- c(72, 80, 83, 63, 66, 76, 82)
after  <- c(78, 82, 82, 68, 70, 75, 88)
dat    <- cbind(before, after)
dat
```

    ##      before after
    ## [1,]     72    78
    ## [2,]     80    82
    ## [3,]     83    82
    ## [4,]     63    68
    ## [5,]     66    70
    ## [6,]     76    75
    ## [7,]     82    88

``` r
plot(formula=after~before, data=dat, main="Scatter plot")
```

![](stat-basic_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
cor(before, after)
```

    ## [1] 0.921773

``` r
obj <- lm(after~before)
summary(obj)
```

    ## 
    ## Call:
    ## lm(formula = after ~ before)
    ## 
    ## Residuals:
    ##        1        2        3        4        5        6        7 
    ##  2.57110 -0.09454 -2.59416  0.06995 -0.42967 -3.76172  4.23905 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)  15.4381    11.7438   1.315  0.24572   
    ## before        0.8332     0.1567   5.316  0.00315 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.022 on 5 degrees of freedom
    ## Multiple R-squared:  0.8497, Adjusted R-squared:  0.8196 
    ## F-statistic: 28.26 on 1 and 5 DF,  p-value: 0.003151
