library(websocket)
library(RJSONIO)
library(uuid)

source("RpcClient.R")

alor.WsURL <<- "wss://api.alor.ru/ws"

# ----------------
# Клиент для Websocket API
# ----------------
StartSocket <- function() {
  ws <- WebSocket$new(alor.WsURL)
  token <- GetToken() # получаем токен доступа
  GuidQuotes <<- uuid::UUIDgenerate() # генерим идентификатор для будущей подписки на ленту сделок по инструменту
  
  # Обработчик всех подписок (можно вынести в отдельный метод)
  ws$onMessage(function(event) {
    parsedMessage <- fromJSON(event$data)
    if ("requestGuid" %in% names(parsedMessage)) {
      if (parsedMessage$requestGuid == GuidQuotes && parsedMessage$message == "Handled successfully")
      {
        print("ws handled quotes subscription successfully")
      }
    }
    
    if ("guid" %in% names(parsedMessage) && parsedMessage$guid == GuidQuotes) {
      # тут нужно вызвать метод обработки и сохранения (если нужно) полученных данных
    }
  })
  
  # Инициализация подписок (тут только подписка на ленту сделок по RTS, но подписок может быть сколько угодно и на все, что в документации есть)
  CurrentQuotes <- list(
    opcode="AllTradesGetAndSubscribe",
    code="RTS-9.20",
    delayed="false",
    exchange="MOEX",
    format="Simple",
    guid=GuidQuotes,
    token=token
  )
  
  CurrentQuotesJson<-toJSON(CurrentQuotes)
  ws$send(CurrentQuotesJson)
}

# Запуск
StartSocket()
