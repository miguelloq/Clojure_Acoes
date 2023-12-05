(ns trabalho.db)

(def registros
  (atom []))

(defn transacoes []
  @registros)

(defn- compra? [transacao]
  (= (:tipo transacao) "compra"))

(defn- calcular [acumulado transacao]
  (let [valor (:valor transacao)]
    (if (compra? transacao)
      (- acumulado valor)
      (+ acumulado valor))))

(defn registrar [transacao]
  (let [colecao-atualizada (swap! registros conj transacao)]
    (merge transacao {:id (count colecao-atualizada)})))

(defn saldo []
  (reduce calcular 0 @registros))

(defn transacoes-do-tipo [tipo]
  (filter #(= tipo (:tipo %)) (transacoes)))




