(ns coil.edn
  (:import
   java.io.PushbackReader
   java.net.http.HttpRequest$BodyPublishers)
  (:require
   [coil.mults :as m]
   [clojure.java.io :as io]
   [coil.util :as u]
   [coil.publishers :as pub]

   [clojure.edn :as edn]))


(defmethod m/make-custom-body-publisher :edn
  [{:keys [body]}]

  (pub/of-string
   (pr-str body)))


(defmethod m/handle-content-type :edn
  [opt]
  (assoc-in opt
            [:headers :content-type]
            "application/edn"))


(defn resp-edn? [response]
  (u/content-type-matches?
   response
   #"(?i)(application|text)/edn"))


(defmethod m/handle-as :edn
  [response
   {:keys [edn-read-params]}]

  (if-not (resp-edn? response)
    response

    (update response :body
            (fn [stream]
              (->> stream
                   io/reader
                   PushbackReader.
                   (edn/read edn-read-params))))))
