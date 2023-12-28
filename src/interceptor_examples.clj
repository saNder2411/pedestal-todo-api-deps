(ns interceptor-examples)

(def user-id-validator-
  {:name  ::user-id-validator
   :enter (fn [context]
            (if (re-matches #"[0-9]+" (get-in context [:request :path-params :user-id]))
              context
              (assoc context :response {:status 400 :body "user-id must be numeric"})))})