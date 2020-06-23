(ns provisdom.test.assertions.cljs
  (:require
    [cljs.test :as t]))

#?(:clj (defmethod t/assert-expr 'spec-check
          [menv msg form]
          (let [[_ sym-form opts] form]
            `(provisdom.test.core/do-spec-check-report ~sym-form ~opts))))