(ns coil.auth
  (:import
   java.util.Base64
   java.util.Base64$Encoder))


(def ^Base64$Encoder
  encoder (Base64/getEncoder))


(defn handle-basic-auth
  [{:as opt
    :keys [basic-auth]}]

  (if-not basic-auth
    opt

    (let [[user pass] basic-auth
          pair (str user ":" pass)

          encoded
          (.encodeToString encoder (.getBytes pair "UTF-8"))]

      (assoc-in opt
                [:headers :authorization]
                (str "Basic " encoded)))))
