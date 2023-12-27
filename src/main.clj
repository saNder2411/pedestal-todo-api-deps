(ns main
  (:require [io.pedestal.http :as http]
            [io.pedestal.http.route :as route]
            [io.pedestal.test :as test]))

(defn response [status body & {:as headers}]
  {:status status :body body :headers headers})

(def ok (partial response 200))
(def created (partial response 201))
(def accepted (partial response 202))

;;;
;;; "Database" functions
;;;
(defonce database (atom {}))

(defn find-list-by-id [db-val id] (get db-val id))

(defn find-list-item-by-ids [db-val list-id item-id] (get-in db-val [list-id :items item-id] nil))

(defn delete-list [db-val list-id]
  (if (contains? db-val list-id)
    (dissoc db-val list-id)
    db-val))

(defn update-list [db-val list-id upd-item]
  (if (contains? db-val list-id)
    (let [item (get db-val list-id)
          new-item (into item upd-item)]
      (assoc db-val list-id new-item))
    db-val))

(defn create-list-item [db-val list-id item-id new-item]
  (if (contains? db-val list-id)
    (assoc-in db-val [list-id :items item-id] new-item)
    db-val))

(defn delete-list-item [db-val list-id item-id]
  (if (contains? db-val list-id)
    (update-in db-val [list-id :items] dissoc item-id)
    db-val))

(defn update-list-item [db-val list-id item-id upd-item]
  (if (contains? db-val list-id)
    (let [item (get-in db-val [list-id :items item-id])
          new-item (into item upd-item)]

      (assoc-in db-val [list-id :items item-id] new-item))
    db-val))

(def db-interceptor
  {:name :db-interceptor
   :enter #(update % :request assoc :database @database)
   :leave (fn [ctx]
            (if-let [[operation & args] (:tx-data ctx)]
              (do
                (apply swap! database operation args)
                (assoc-in ctx [:request :database] @database))
              ctx))})

;;;
;;; Domain functions
;;;
(defn make-list [nm] {:name nm :items {}})

(defn make-list-item [nm] {:name nm :done? false})

;;;
;;; API Interceptors
;;;
;; (def echo {:name :echo
;;            :enter #(assoc % :response (ok (:request %)))})

(def entity-render
  {:name :entity-render
   :leave (fn [ctx]
            (if-let [item (:result ctx)]
              (assoc ctx :response (ok item))
              ctx))})

(def list-create
  {:name :list-create
   :enter (fn [ctx]
            (let [nm (get-in ctx [:request :query-params :name] "Unnamed List")
                  new-list (make-list nm)
                  db-id (str (gensym "l-"))
                  url (route/url-for :list-view :params {:list-id db-id})]
              (assoc ctx
                :response (created new-list "Location" url)
                :tx-data [assoc db-id new-list])))})

(def list-query-form
  {:name :list-query-form
   :leave (fn [ctx]
            (assoc ctx :result (get-in ctx [:request :database])))})


(def list-view
  {:name :list-view
   :leave (fn [ctx]
            (let [id (get-in ctx [:request :path-params :list-id])
                  the-list (when id (find-list-by-id (get-in ctx [:request :database]) id))]
              (cond-> ctx
                      the-list (assoc :result the-list))))})

(def list-delete
  {:name :list-delete
   :enter (fn [ctx]
            (let [list-id (get-in ctx [:request :path-params :list-id])]
              (cond-> ctx
                      list-id (assoc :tx-data [delete-list list-id]))))})

(def list-update
  {:name :list-update
   :enter (fn [ctx]
            (let [list-id (get-in ctx [:request :path-params :list-id])
                  upd-list (get-in ctx [:request :query-params] {})]
              (cond-> ctx
                      list-id (assoc :tx-data [update-list list-id upd-list]))))})


(def list-item-view
  {:name :list-item-view
   :leave (fn [ctx]
            (let [list-id (get-in ctx [:request :path-params :list-id])
                  item-id (and list-id (get-in ctx [:request :path-params :item-id]))
                  item (and item-id
                            (find-list-item-by-ids (get-in ctx [:request :database]) list-id item-id))]
              (cond-> ctx
                      item (assoc :result item))))})

(def list-item-create
  {:name :list-item-create
   :enter (fn [ctx]
            (if-let [list-id (get-in ctx [:request :path-params :list-id])]
              (let [nm (get-in ctx [:request :query-params :name] "Unnamed Item")
                    new-item (make-list-item nm)
                    item-id (str (gensym (str list-id "-i-")))]
                (-> ctx
                    (assoc :tx-data [create-list-item list-id item-id new-item])
                    (assoc-in [:request :path-params :item-id] item-id)))
              ctx))})

(def list-item-delete
  {:name :list-item-delete
   :enter (fn [ctx]
            (let [{:keys [list-id item-id]} (get-in ctx [:request :path-params])]
              (cond-> ctx
                      (and list-id item-id) (assoc :tx-data [delete-list-item list-id item-id]))))})

(def list-item-update
  {:name :list-item-update
   :enter (fn [ctx]
            (let [{:keys [list-id item-id]} (get-in ctx [:request :path-params])
                  upd-item (get-in ctx [:request :query-params] {})]
              (cond-> ctx
                      (and list-id item-id) (assoc :tx-data [update-list-item list-id item-id upd-item]))))})



(def routes
  (route/expand-routes
    #{["/todo" :post [db-interceptor list-create]]
      ["/todo" :get [entity-render db-interceptor list-query-form]]
      ["/todo/:list-id" :get [entity-render db-interceptor list-view]]
      ["/todo/:list-id" :put [entity-render list-view db-interceptor list-update]]
      ["/todo/:list-id" :delete [entity-render list-query-form db-interceptor list-delete]]

      ["/todo/:list-id" :post [entity-render list-item-view db-interceptor list-item-create]]
      ["/todo/:list-id/:item-id" :get [entity-render list-item-view db-interceptor]]
      ["/todo/:list-id/:item-id" :put [entity-render list-item-view db-interceptor list-item-update]]
      ["/todo/:list-id/:item-id" :delete [entity-render list-view db-interceptor list-item-delete]]}))

(def service-map
  {::http/routes routes
   ::http/type :jetty
   ::http/port 8890})

(defn start []
  (-> service-map http/create-server http/start))

;; For interactive development
(defonce server (atom nil))

(defn start-dev []
  (reset! server (-> (assoc service-map ::http/join? false)
                     http/create-server
                     http/start)))

(defn stop-dev []
  (when @server (http/stop @server)))

(defn restart []
  (stop-dev)
  (start-dev))

(defn test-request [verb url]
  (test/response-for (::http/service-fn @server) verb url))

(comment
  @database
  (restart)
  (test-request :post "/todo/l-25613?name=C-Item")
  )
