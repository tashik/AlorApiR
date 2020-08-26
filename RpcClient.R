library(httr)
library(lubridate)
library (plyr)
library(RJSONIO)

# NB! Production URLs!
alor.apiURL       <<- "https://api.alor.ru"
alor.refreshToken <- "your-refresh-token-here"
alor.authURL      <<- paste("https://oauth.alor.ru/refresh?token=", alor.refreshToken, sep="")

# ------------------
# Получение токена доступа к API (OAuth 2 access token)
# ------------------
GetToken<-function() {
  r <- POST(alor.authURL)
  
  authResp<-content(r, "parsed")  
  return (authResp$AccessToken)
}


# ------------------
# Построение заголовков запроса
# ------------------
BuildReqHeaders<-function(accessToken) {
  headers<-add_headers(.headers = c('Authorization'= paste('Bearer', accessToken, sep=' ')
                                    , 'Content-Type' = 'application/x-www-form-urlencoded')
                       )
  return (headers)
}

# -------------------
# Получение свечей
# EXAMPLE method call:
#
#  candlesQuery<-list(
#    from = toString(ToTimestamp(CalcStartDate)),
#    to = toString(ToTimestamp(CalcEndDate)),
#    tf = 60,
#    exchange = 'MOEX',
#    code = ticker
#  )
#  candlesData <- ObtainCandles(candlesQuery, T)
# -------------------
ObtainCandles <-function(candlesQuery, full=F) {
  accessToken <- GetToken()
  candlesResp<-GET(paste(alor.apiURL, "/md/history", sep=""), query=candlesQuery, BuildReqHeaders(accessToken))
  candles<-content(candlesResp, "parsed")
  df <- ldply (candles$history, data.frame)
  if (full) {
    return (df)
  }
  p <- df$close
  p <- p[!is.na(p)]
  return (p)
}

# --------------------
# Получение стакана по инструменту
# EXAMPLE method call: ObtainOrderBook("RTS-9.20", "your access token from GetToken() method here")
# --------------------
ObtainOrderBook <- function(ticker, accessToken) {
  orderBookResp <-GET(paste(alor.apiURL, "/md/orderbooks/MOEX/", ticker, sep=""), BuildReqHeaders(accessToken))
  orderBook<-content(orderBookResp, "parsed")
  return (orderBook)
}

# --------------------
# Получение параметров инструмента
# EXAMPLE method call: ObtainSecurities("RTS-9.20")
# --------------------
ObtainSecurities <- function(ticker) {
  accessToken <- GetToken()
  securitiesResp <- GET(paste(alor.apiURL, "/md/Securities/MOEX/", ticker, sep=""), BuildReqHeaders(accessToken))
  security<-content(securitiesResp, "parsed")
  
  # лучший бид и аск отдается только в стакане - поэтому получаем стакан
  orderBook <- ObtainOrderBook(ticker, accessToken)
  
  securityData <- list(
    ticker=security$symbol,
    tickerShort=security$shortname,
    lotSize=security$lotsize,
    expiry=security$cancellation,
    marginBuy=security$marginbuy,
    marginSell=security$marginsell,
    priceStep=security$minstep,
    priceStepCost=security$pricestep,
    theorPrice=security$theorPrice,
    iv=security$volatility
  )
  
  # Вытаскиваем лучший бид, лучший аск и центр стакана
  if (length(orderBook$bids)) {
    securityData$bestBid <- orderBook$bids[1][[1]]$price
  } else {
    securityData$bestBid <- 0
  }
  
  if (length(orderBook$asks)) {
    securityData$bestAsk <- orderBook$asks[length(orderBook$asks)][[1]]$price
  } else {
    securityData$bestAsk <- 0
  }
  numDecimals <- 0
  if (securityData$priceStep < 1) {
    numDecimals <- 2
  }  
  securityData$realPrice <- round(mean(securityData$bestBid, securityData$bestAsk), numDecimals)
  return (securityData)
}
