(ns trabalho.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [cheshire.core :as json]
            [ring.middleware.defaults :refer [wrap-defaults
                                              api-defaults]]
            [ring.middleware.json :refer [wrap-json-body]]
            [clj-http.client	:as	http-client]))

;UTILS;;;;;;
(defn remover-uma-vez [valor lista]
  (let [idx (first (keep-indexed #(when (= %2 valor) %1) lista))]
    (if (nil? idx)
      lista
      (concat (take idx lista) (next (drop idx lista))))))

(defn como-json [conteudo & [status]]
  {:status (or status 200)
   :headers {"Content-Type" "application/json; charset=utf-8"}
   :body (json/generate-string conteudo)})

;pode jogar exception
(defn infoAcao [codigoAcao]
  (let
   [url (str "https://brapi.dev/api/quote/" codigoAcao "?modules=summaryProfile&token=oZ89LuuEfB7zjyS7BaeB6s")]
    (-> (:body (http-client/get url)) (json/parse-string true) (get-in [:results]) (first))))

(defn infoAcaoMapper [brutoInfoAcao]
  (hash-map :nome (:shortName brutoInfoAcao) :codigo (:symbol brutoInfoAcao) :variacaoDia (:regularMarketChange brutoInfoAcao) :variacaoDiaPercentual (:regularMarketChange brutoInfoAcao) :precoAtual (:regularMarketPrice brutoInfoAcao) :precoMaximo (:regularMarketDayHigh brutoInfoAcao) :precoMinimo (:regularMarketDayLow brutoInfoAcao) :precoAbertura (:regularMarketOpen brutoInfoAcao) :precoFechamento (:regularMarketPreviousClose brutoInfoAcao) :horaFechamento (:regularMarketTime brutoInfoAcao) :descricao (:longBusinessSummary (:summaryProfile brutoInfoAcao))))


;BANCO;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def banco (atom {:saldo 1000000 :historico [] :acoes []}))

(defn exibeHistoricoDepedendoDoTipo [tipo]
  (filter #(= tipo (:tipo %)) (:historico @banco)))

(defn exibeFama []
  (hash-map :saldo (:saldo @banco) :acoes (:acoes @banco) :historico (:historico @banco)))

(defn exibeHistoricoPorData []
  (:historico @banco))

(defn atualizaSaldoCompra [valor]
  (swap! banco assoc :saldo (- (:saldo @banco) valor)))

(defn atualizaHistoricoCompra [codigoAcao,valor]
  (swap! banco assoc :historico (conj (:historico @banco) (hash-map :acao codigoAcao :valor valor :tipo "compra" :data 2023))))

(defn atualizaAcoesCompra [codigoAcao]
  (swap! banco assoc :acoes (conj (:acoes @banco) codigoAcao)))

(defn bancoValidaCompra [valor]
  (>= (:saldo banco) valor))

(defn adicionaCompra [valor,codigoAcao]
  (atualizaSaldoCompra valor)
  (atualizaHistoricoCompra codigoAcao valor)
  (atualizaAcoesCompra codigoAcao))

(defn atualizaSaldoVenda [valor]
  (swap! banco assoc :saldo (+ (:saldo @banco) valor)))

(defn atualizaHistoricoVenda [codigoAcao,valor]
  (swap! banco assoc :historico (conj (:historico @banco) (hash-map :acao codigoAcao :valor valor :tipo "venda" :data 2023))))

(defn atualizaAcoesVenda [codigoAcao]
  (let [acoesAtualizadas (remover-uma-vez codigoAcao (:acoes @banco))]
    (swap! banco assoc :acoes acoesAtualizadas)))

(defn bancoValidaVenda [codigoAcao]
  (contains? (:acoes @banco) codigoAcao))

(defn adicionaVenda [valor,codigoAcao]
  (atualizaSaldoVenda valor)
  (atualizaHistoricoVenda codigoAcao valor)
  (atualizaAcoesVenda codigoAcao))

(defroutes app-routes
  (GET "/" [] "Olá, mundo!")
  (GET "/companhias" [] (let [body (:body (http-client/get "https://brapi.dev/api/quote/list?token=58iNus1bh51Ymw9gXmzuop")) stocks (get-in (json/parse-string body true) [:stocks])]
                          (como-json (map #(hash-map :nome (:name %) :codigo (:stock %)) stocks))))
  (GET "/info/:codigoAcao" [codigoAcao]
    (try
      (como-json (infoAcaoMapper (infoAcao codigoAcao)))
      (catch Exception e (como-json {:mensagem "Caiu numa exception opssss"} 422))))

  (GET "/fama" [] (como-json (exibeFama)))
  (GET "/vendas" []
    (como-json (exibeHistoricoDepedendoDoTipo "venda")))
  (GET "/compras" []
    (como-json (exibeHistoricoDepedendoDoTipo "compra")))

  (POST "/compra/:codigoAcao" [codigoAcao]
    (try
      (->
       (infoAcaoMapper (infoAcao codigoAcao))
       (get-in ,,, [:precoAtual])
       (adicionaCompra ,,, codigoAcao)
       (como-json ,,, 201))
      (catch Exception e (como-json {:mensagem (str "Exceptiooon" (.getMessage e))} 422))))

  (POST "/venda/:codigoAcao" [codigoAcao]
    (try
      (->
       (infoAcaoMapper (infoAcao codigoAcao))
       (get-in ,,, [:precoAtual])
       (adicionaVenda ,,, codigoAcao)
       (como-json ,,, 201))
      (catch Exception e (como-json {:mensagem (str "Exceptiooon" (.getMessage e))} 422))))


  (route/not-found "Recurso não encontrado"))

(def app
  (-> (wrap-defaults app-routes api-defaults)
      (wrap-json-body {:keywords? true :bigdecimals? true})))