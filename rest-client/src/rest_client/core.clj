(ns rest-client.core
  (:gen-class)
  (:require [cheshire.core :as cheshire]
            [clj-http.client :as client]
            [clojure.string :as str])
  (:import (java.io BufferedReader)))

(defn read-json-from-stdin []
  (cheshire/parse-stream (BufferedReader. *in*)))

(defmulti http (fn [_ method] (str/upper-case method)))
(defmethod http "GET" [url _]
  (client/get url {:accept :json
                   :throw-exceptions false}))
(defmethod http "POST" [url _]
  (client/post url {:form-params (read-json-from-stdin)
                    :content-type :json
                    :accept :json
                    :throw-exceptions false}))
(defmethod http :default [_ method]
  (throw (UnsupportedOperationException. (str "Unsupported HTTP method: " method))))

(defn -main [& args]
  (when (not= (count args) 2)
    (throw (IllegalArgumentException. "Exactly 2 arguments must be specified")))
  (let [{:keys [status body]} (apply http args)]
    (when (not= status 200)
      (print (str status \tab)))
    (println body)))
