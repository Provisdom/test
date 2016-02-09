(ns provisdom.test.core
  (:require [midje.sweet :refer :all]
            [criterium.core :refer :all]
            [clojure.walk :as wa]
            [clojure.test.check.generators :as g]
            [incanter [core :as ic]
             [charts :as ich]]))

(defn- wrap-leaves-with-checker [call tree]
  (wa/postwalk (fn [node]
                 (if (coll? node)
                   `(just ~node)
                   (macroexpand `(-> ~node ~call))))
               tree))

(defmacro check-with [call-form tree]
  "https://gist.github.com/marick/1117354
examples:
   (check-with test-roughly [m/nan m/nan])
   (check-with (test-roughly 1e-5) [1.2564312086572633 0.0])
   (check-with (test-roughly) [1.2564312086572633 0.0])"
  (wrap-leaves-with-checker call-form tree))

(defchecker test-roughly
            "With three arguments, accepts a value within abs of the expected value
             or within rel * expected of the expected value
          With one argument, the defaults are abs = 1e-6 and rel = 1e-3."
            ([expected abs rel]
              (checker [actual]
                       (or (and (nil? expected) (nil? actual))
                           (and (>= expected (- actual abs))
                                (<= expected (+ actual abs)))
                           (and (>= expected (- actual (* rel expected)))
                                (<= expected (+ actual (* rel expected))))
                           (and (Double/isNaN expected) (Double/isNaN actual)))))
            ([expected abs]
              (checker [actual]
                       (or
                         (and (nil? expected) (nil? actual))
                         (and (>= expected (- actual abs))
                             (<= expected (+ actual abs)))
                         (and (Double/isNaN expected) (Double/isNaN actual)))))
            ([expected]
              (test-roughly expected 1e-6 1e-3)))

;;;also mess with Tableau/cars.com-like interface with sliders and multiple
;;;   charts on same page showing time
;;;also mess with dynamic charts that simulate and perhaps change color
;;;and leave blocks of color behind showing strategy changes
;;; (multiple lines across bottom could represent multiple strategies at once)

(defn xy-plot
  "Returns a JFreeChart object representing a xy-plot of the given data.
Options:
   :title (default nil)
   :x-label (default 'x')
   :y-label (default 'y')
   :legend? (default false)
   :series-label (default 'x')
   :points? (default false)
   :auto-sort-x-data? (default true)

References:
;; see INCANTER_HOME/examples/probability_plots.clj for more examples of plots
http://www.jfree.org/jfreechart/api/javadoc/
http://www.jfree.org/jfreechart/api/javadoc/org/jfree/chart/JFreeChart.html"
  [x y &
   {:keys [title x-label y-label legend? series-label points?
           auto-sort-x-data?]
    :or   {x-label           "x", y-label "y", series-label "x",
           auto-sort-x-data? true}}]
  (ich/xy-plot x y :title title :x-label x-label :y-label y-label
               :legend legend? :series-label series-label :points points?
               :auto-sort auto-sort-x-data?))

(defn scatter-plot
  "Returns a JFreeChart object representing a scatter-plot of the given data.
Options:
   :title (default nil)
   :x-label (default 'x')
   :y-label (default 'y')
   :legend? (default false)
   :series-label (default 'x')
   :density? (default false) -- chart will represent density instead of
      frequency.
   :nbins (default 10) -- number of bins (i.e. bars)
   :gradient? (default false) -- use gradient on bars"
  [x y &
   {:keys [title x-label y-label legend? series-label density? nbins gradient?]
    :or   {x-label "x", y-label "y", series-label "x", nbins 10}}]
  (ich/scatter-plot x y :title title :x-label x-label :y-label y-label
                    :legend legend? :series-label series-label
                    :density? density? :nbins nbins :gradient? gradient?))

(defn bar-chart
  "Returns a JFreeChart object representing a bar-chart of the given data.
Options:
   :title (default nil)
   :x-label (default 'x')
   :y-label (default 'y')
   :legend? (default false)
   :series-label (default 'x')
   :vertical? (default true)"
  [x y &
   {:keys [title x-label y-label legend? series-label vertical?]
    :or   {x-label "x", y-label "y", series-label "x", vertical? true}}]
  (ich/bar-chart
    x y :title title :x-label x-label :y-label y-label :legend legend?
    :series-label series-label :vertical? vertical?))

(defn parametric-plot
  "Returns a JFreeChart object representing a parametric plot of the given
      function.
   Function should take a value t and return a point [x y]
   Options:
      :title (default nil)
      :x-label (default 'x')
      :y-label (default 'y')
      :legend? (default false)
      :series-label (default 'x')
      :step-size (default (/ (- max-range min-range) 500))"
  [f min-range max-range &
   {:keys [title x-label y-label legend? series-label step-size]
    :or   {x-label "x", y-label "y", series-label "x", vertical? true}}]
  (let [step-size (if step-size step-size (/ (- max-range min-range) 500))]
    (ich/parametric-plot
      f min-range max-range :title title :x-label x-label :y-label y-label
      :legend legend? :series-label series-label :step-size step-size)))

(defn heat-map
  "Function should take two scalar arguments and return a scalar.
Options:
   :title (default nil)
   :x-label (default 'x')
   :y-label (default 'y')
   :z-label (default 'z')
   :color? (default true) -- should the plot be in color or not?
   :include-zero? (default true) -- should the plot include the origin if
      it if not in the ranges specified?"
  [f x-min x-max y-min y-max
   & {:keys [title x-label y-label z-label color? include-zero?]
      :or   {x-label       "x", y-label "y", z-label "z", color? true,
             include-zero? true}}]
  (ich/heat-map f x-min x-max y-min y-max :title title :x-label x-label
                :y-label y-label :z-label z-label :color? color?
                :include-zero? include-zero?))

(defn histogram
  "Returns a JFreeChart object representing the histogram of the given data.
Options:
    :nbins (default 10) number of bins
    :density? (default false) if false, plots frequency, otherwise density
    :title (default 'Histogram') main title
    :x-label (default x expression)
    :y-label (default 'Frequency')
    :legend (default false) prints legend
    :series-label (default x expression)"
  [x & {:keys [title x-label y-label legend? series-label nbins density?]
        :or   {x-label "x", y-label "y", series-label "x", nbins 10}}]
  (ich/histogram x :title title :x-label x-label :y-label y-label
                 :legend legend? :series-label series-label :nbins nbins
                 :density density?))

(defn view-plot
  "plot can also be a saved file: (view 'plot.png')"
  [plot] (ic/view plot))

(defn save-plot
  "filename: 'plot.png' or (FileOutputStream. '/tmp/plot.png')"
  [plot filename] (ic/save plot filename))

(defn add-points
  "Plots points on the given scatter or line plot (xy-plot) of the (x,y)
   points.
Options:
   :series-label (default 'x')
   :auto-sort-x-data? (default true)"
  [chart x y & {:keys [series-label] :or {series-label "x"}}]
  (ich/add-points chart x y :series-label series-label))

(defn add-line
  "Plots a line on the given scatter or line plot (xy-plot) of the (x,y)
   points.
Options:
   :series-label (default 'x')
   :points? (default false)
   :auto-sort-x-data? (default true)

   ;; Clojure's doto macro can be used to build a chart
   (doto (histogram (sample-normal 1000) :density true)
         (add-lines (range -3 3 0.05) (pdf-normal (range -3 3 0.05)))
         view)"
  [chart x y & {:keys [series-label points? auto-sort?]
                :or   {series-label "x", auto-sort? true}}]
  (ich/add-lines chart x y :series-label series-label :points points?
                 :auto-sort auto-sort?))

(defn add-parametric
  "Adds a xy-plot of the given parametric function to the given chart,
   returning a modified version of the chart.
Function takes 1 argument t and returns point [x y].
Options:
   :series-label (default 'x')
   :step-size (default (/ (- max-range min-range) 500))"
  [chart f min-range max-range & {:keys [series-label step-size]
                                  :or   {series-label "x"}}]
  (let [step-size (if step-size step-size (/ (- max-range min-range) 500))]
    (ich/add-parametric chart f min-range max-range :series-label series-label
                        :step-size step-size)))

;(defn add-const-line
;  "Plots constant line"
;  [chart v & {:keys [series-label vertical?]} :or {vertical? true}]
;  (if vertical? ...
;  (add-line chart [star

;(defn dynamic-xy-plot
;  "Examples:
;  (let [x (range -3 3 0.1)]
;    (view (dynamic-xy-plot [mean (range -3 3 0.1)
;                            sd (range 0.1 10 0.1)]
;            [x (pdf-normal x :mean mean :sd sd)]
;            :title 'Normal PDF Plot')))
;
;   (let [x (range -3 3 0.1)]
;     (view (dynamic-xy-plot [mean (range -3 3 0.1)
;                             sd (range 0.1 10 0.1)]
;            (for [xi x] [xi (pdf-normal xi :mean mean :sd sd)])
;            :title 'Normal PDF Plot')))"
; [[slider-bindings] expression & options]
; (ic/view (ich/dynamic-xy-plot slider-bindings expression)))
