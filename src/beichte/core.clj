(ns beichte.core
  (:require [clojure.tools.analyzer :as ana]
            [clojure.tools.analyzer.env :as env]
            [clojure.tools.analyzer.jvm :as jvm]
            [clojure.java.io :as io])
  (:import (java.io LineNumberReader InputStreamReader PushbackReader)
           (clojure.lang RT Reflector)))


;; taken from clojure.repl
(defn source-var
  "Returns a string of the source code for the given symbol, if it can
  find it.  This requires that the symbol resolve to a Var defined in
  a namespace for which the .clj is in the classpath.  Returns nil if
  it can't find the source.  For most REPL usage, 'source' is more
  convenient.
  Example: (source-fn 'filter)"
  [v]
  (if-let [filepath (:file (meta v))]
    (if-let [strm (or (.getResourceAsStream (RT/baseLoader) filepath)
                        ;; fall back to filesystem for dev
                        (io/input-stream filepath))]
      (with-open [rdr (LineNumberReader. (InputStreamReader. strm))]
        (dotimes [_ (dec (:line (meta v)))] (.readLine rdr))
        (let [text (StringBuilder.)
              pbr (proxy [PushbackReader] [rdr]
                    (read [] (let [i (proxy-super read)]
                               (.append text (char i))
                               i)))
              read-opts (if (.endsWith ^String filepath "cljc") {:read-cond :allow} {})]
          (if (= :unknown *read-eval*)
            (throw (IllegalStateException. "Unable to read source while *read-eval* is :unknown."))
            (read read-opts (PushbackReader. pbr))) (str text)))
      {:problem :cannot-open-stream
       :var v :file filepath})
    {:problem :file-not-found
     :var v :meta (meta v)}))


(comment
  (source-var (:var (jvm/analyze 'clojure.core/*print-readably* (ana/empty-env)))))

(def pure-ground (atom {#'clojure.core/*print-readably* true
                        #'clojure.core/*print-dup* true
                        #'clojure.core/*out* false
                        #'clojure.core/pr true

                        #'clojure.core/cons true ;; breaks recursion
                        #'clojure.core/first true
                        #'clojure.core/rest true
                        #'clojure.core/spread true

                        ;; classes
                        "Exception" true
                        "clojure.lang.LazySeq" true
                        "clojure.lang.ChunkBuffer" true
                        "clojure.lang.ChunkedCons" true

                        ;; instance calls
                        ["clojure.lang.IFn" 'applyTo] true
                        ["clojure.lang.Var" 'pushThreadBindings] true
                        ["clojure.lang.Var" 'popThreadBindings] true
                        ["clojure.lang.PersistentHashMap" 'create] true
                        ["clojure.lang.IChunkedSeq" 'chunkedFirst] true
                        ["clojure.lang.IChunkedSeq" 'chunkedMore] true
                        ["clojure.lang.Numbers" 'add] true
                        ["clojure.lang.Numbers" 'inc] true
                        ["clojure.lang.Numbers" 'lt] true
                        ["clojure.lang.Numbers" 'unchecked_inc] true
                        ["clojure.lang.Numbers" 'isZero] true
                        ["clojure.lang.Util" 'identical] true
                        ["clojure.lang.RT" 'seq] true
                        ["clojure.lang.RT" 'intCast] true
                        ["clojure.lang.RT" 'longCast] true
                        ["clojure.lang.RT" 'count] true
                        ["clojure.lang.RT" 'next] true
                        ["clojure.lang.RT" 'conj] true
                        ["clojure.lang.ChunkBuffer" 'add] true
                        ["clojure.lang.ChunkBuffer" 'chunk] true
                        ["clojure.lang.Indexed" 'nth] true
                        }))


(declare impure?)

(defn impure-var? [visited ast]
  (let [v (or (:var ast) (:the-var ast))
        g (@pure-ground v)
        res (cond (true? g) false
                  (false? g)
                  {:problem :impure-var-accessed
                   :file (:file (meta v))
                   :line (:line (meta v))
                   :var v}
                  :else
                  (let [src (source-var v)]
                    (if (map? src) ;; error
                      src
                      (-> src
                          read-string
                          (jvm/analyze (assoc (jvm/empty-env)
                                              :ns (symbol (str (:ns (meta v))))))))))
        res (impure? visited res)]
    (swap! pure-ground assoc v (not res))
    res))



(defn pure-class? [class]
  (@pure-ground (pr-str class)))

(defn pure-method? [class method]
  (@pure-ground [(pr-str class) method]))

(defn some-impure? [visited args]
  (some identity (map (partial impure? visited) args)))


(defn impure?
  ([ast]
   (impure? #{} ast))
  ([visited ast]
   (let [{:keys [args op env form children] :as ast} ast
         res (case op
               nil nil ;; catch missing forms
               :static-call (if (pure-method? (:class ast) (:method ast))
                              (some-impure? visited args)
                              {:problem :unsafe-static-instance-call
                               :class (:class ast)
                               :method (:method ast)
                               :form (:form ast)})
               :const (case (:type ast)
                        :class (let [res (not (pure-class? (:form ast)))]
                                 (if res
                                   {:problem :unsafe-class-instantiated
                                    :form (:form ast)}
                                   res))
                        :vector false
                        :map false
                        :set false
                        :nil false
                        :number false
                        :keyword false
                        :string false
                        :bool false
                        :constant false)
               :def (impure? (conj visited (:var ast))
                             ;; TODO why inconsistent?
                             (or (-> ast :init :expr) (-> ast :init)))
               :fn (some-impure? visited (:methods ast))
               :fn-method (impure? visited (:body ast))
               :do (or (some-impure? visited (:statements ast))
                       (impure? visited (:ret ast)))
               :local (when (:assignable? ast)
                        {:problem :assignable-local
                         :form (:form ast)})
               :invoke (or (impure? visited (:fn ast))
                           (some-impure? visited args))
               :try (or (impure? visited (:body ast))
                        (some-impure? visited (:catches ast))
                        (impure? visited (:finally ast)))
               :catch (or (impure? visited (:class ast))
                          (impure? visited (:body ast)))
               :instance? false
               :let (or (some-impure? visited (:bindings ast))
                        (impure? visited (:body ast))) 
               :binding (impure? visited (:init ast))
               :instance-call
               (if (pure-method? (:class ast) (:method ast))
                 (some-impure? visited args) 
                 {:problem :unsafe-instance-call
                  :class (:class ast)
                  :method (:method ast)
                  :form (:form ast)})
               :loop (or (some-impure? visited (:bindings ast))
                         (impure? visited (:body ast)))
               :recur (some-impure? visited (:exprs ast))
               :new (or (impure? visited (:class ast))
                        (some-impure? visited args))
               :if (or (impure? visited (:test ast))
                       (impure? visited (:then ast))
                       (impure? visited (:else ast)))
               :vector (some-impure? visited (:items ast))
               :set (some-impure? visited (:items ast))
               :map (or (some-impure? visited (:keys ast))
                        (some-impure? visited (:vals ast)))
               :the-var (if (visited (:var ast))
                          nil
                          (impure-var? visited ast))
               :var (if (visited (:var ast))
                      nil
                      (impure-var? visited ast)))]
     (if res
       {:form (:form ast)
        :subform res}
       res))))


(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))





(comment
  (impure? (jvm/analyze '{:a (inc 1)} (jvm/empty-env)))

  (impure? (jvm/analyze '(map (fn [x] (+ 1 x)) [1 2 (inc 3)]) (jvm/empty-env)))

  (impure? (jvm/analyze '(foo 42) (jvm/empty-env)))

  (env/with-env (jvm/global-env)
    (jvm/analyze 'println (jvm/empty-env) #_(env/deref-env)))

  (impure? (jvm/analyze '(try 1 (catch Exception e)) (jvm/empty-env)))

  (jvm/analyze '(try 1 (catch Exception e)) (jvm/empty-env))


  (impure? (jvm/analyze '(let [a 1] a) (jvm/empty-env)))

  (impure? (jvm/analyze '(java.util.UUID/randomUUID) (jvm/empty-env)))

  (->
   (source-var #'clojure.core/println)
   read-string
   (jvm/analyze (jvm/empty-env))
   prn))





