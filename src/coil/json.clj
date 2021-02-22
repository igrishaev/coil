(ns coil.json
  (:require
   [coil.mults :as m]
   [coil.util :as u]
   [coil.publishers :as pub]

   [clojure.java.io :as io]
   [clojure.data.json :as json]))


(def read-defaults
  {:key-fn keyword})


(def write-params
  nil)


(defmethod m/make-custom-body-publisher :json
  [{:keys [body
           json-write-params]}]

  (pub/of-string
   (apply json/write-str
          body
          (u/map->seq
           (merge
            write-params
            json-write-params)))))


(defmethod m/handle-content-type :json
  [opt]
  (assoc-in opt
            [:headers :content-type]
            "application/json"))


(defn resp-json? [response]
  (u/content-type-matches?
   response
   #"(?i)application/json"))


(defmethod m/handle-as :json
  [response
   {:keys [json-read-params]}]

  (if-not (resp-json? response)
    response

    (update response :body
            (fn [stream]
              (apply json/read
                     (io/reader stream)
                     (u/map->seq
                      (merge
                       read-defaults
                       json-read-params)))))))
