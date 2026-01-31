(ns provisdom.test.assertions.cljs
  (:require
    #_{:clj-kondo/ignore [:unused-namespace]}
    [cljs.test :as t]
    #?(:clj [cljs.analyzer.api :as ana])))

#?(:clj
   (defn fully-qualify
     [env sym]
     (let [{ns-name :ns
            sym     :name} (ana/resolve env sym)]
       (symbol (name ns-name) (name sym)))))

#?(:clj
   (defmethod t/assert-expr 'spec-check
     [menv _msg form]
     (let [[_ sym-form opts] form
           syms (if (coll? sym-form) sym-form [sym-form])
           ;; fully qualify syms here since it requires the cljs analyzer api
           syms (map #(fully-qualify menv %) syms)]
       `(provisdom.test.core/do-spec-check-report ~syms ~opts))))
