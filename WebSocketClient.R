library(websocket)
library(RJSONIO)
library(uuid)

source("RpcClient.R")

alor.WsURL <<- "wss://api.alor.ru/ws"
ws <<- WebSocket$new(alor.WsURL)

# ----------------
# Клиент для Websocket API
#
# @param vector tickers, i.e c("RTS-12.20", "Si-12.20")
# ----------------
StartSocket <- function(tickers) {
  token <- GetToken() # получаем токен доступа
  
  tickerGuids <<- list()
  
  # Формируем подписки на инструменты
  for (i in 1:length(tickers)) {
    GuidQuotes <<- uuid::UUIDgenerate() # генерим идентификатор для будущей подписки на ленту сделок по инструменту
    ticker <- tickers[i]
    tickerGuids[GuidQuotes] <<- ticker
    
    CurrentQuotes <- list(
      opcode="AllTradesGetAndSubscribe",
      code=ticker,
      delayed="false",
      exchange="MOEX",
      format="Simple",
      guid=GuidQuotes,
      token=token
    )
    
    CurrentQuotesJson<-toJSON(CurrentQuotes)
    ws$send(CurrentQuotesJson)
  }
  
  GuidQuotes <<- uuid::UUIDgenerate() 
  
  # Обработчик всех подписок (можно вынести в отдельный метод)
  ws$onMessage(function(event) {
    parsedMessage <- fromJSON(event$data)
    if ("requestGuid" %in% names(parsedMessage)) {
      ticker<- tickerGuids[[parsedMessage$requestGuid]]
      
      if (!is.na(ticker) && parsedMessage$message == "Handled successfully")
      {
        print(paste("ws handled quotes subscription successfully for ticker", ticker, sep=" "))
      }
    }
    
    if ("guid" %in% names(parsedMessage) && parsedMessage$guid == GuidQuotes) {
      ticker<- tickerGuids[[parsedMessage$guid]] # так можно вытащить тикер из нашего списка, не разбирая сообщение
      # тут нужно вызвать метод обработки и сохранения (если нужно) полученных данных
    }
  })
}

Stop() <- function() {
  ws$close()
}  

# Запуск
tickers <- c("RTS-12.20", "Si-12.20", "BR-10.20")
StartSocket(tickers)
