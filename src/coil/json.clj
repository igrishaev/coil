(ns coil.json
  (:import
   java.net.http.HttpRequest$BodyPublishers)
  (:require
   [coil.mults :as m]
   [coil.util :as u]

   [clojure.java.io :as io]
   [clojure.data.json :as json]))


(def read-defaults
  {:key-fn keyword})


(def write-params
  nil)


(defmethod m/make-custom-body-publisher :json
  [{:keys [body
           json-write-params]}]

  (HttpRequest$BodyPublishers/ofString
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
  (some->> response
           :headers
           :content-type
           (re-find #"(?i)/json")
           some?))


(defmethod m/handle-as :json
  [response
   {:keys [json-read-params]}]

  (if (resp-json? response)
    (update response :body
            (fn [stream]
              (apply json/read
                     (io/reader stream)
                     (u/map->seq
                      (merge
                       read-defaults
                       json-read-params)))))

    response))
