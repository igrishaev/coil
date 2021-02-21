(ns coil.mults)


(defmulti make-custom-body-publisher
  :content-type)


(defmethod make-custom-body-publisher :default
  [{:keys [content-type]}]
  (throw (ex-info "wrong content-type"
                  {:content-type content-type})))


(defmulti handle-content-type :content-type)


(defmethod handle-content-type :default
  [opt] opt)


(defmulti handle-as
  (fn [response {:keys [as]}]
    as))


(defmethod handle-as :default
  [response _]
  response)
