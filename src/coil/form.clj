(ns coil.form
  (:import
   java.net.http.HttpRequest$BodyPublishers)
  (:require
   [coil.mults :as m]
   [coil.query :as q]))


(defmethod m/make-custom-body-publisher :form
  [{:keys [body]}]

  (HttpRequest$BodyPublishers/ofString
   (q/make-query-string body)))


(defmethod m/handle-content-type :form
  [opt]
  (assoc-in opt
            [:headers :content-type]
            "application/x-www-form-urlencoded"))
