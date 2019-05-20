(ns cardboard.model.src-dir-manager
  (:require [clojure.java.io :as io]
            [clojure.core :refer :all]
            [datoteka.core :as fs]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.spec.alpha :as s]))

;; IMPROVEMENT NEEDED:
;; this does not maintain ids across sessions. i.e. if you change code block ordering, you will change the ids. this should be updated such that ids are more stable.

;; currently safe: changing contents
;; unsafe: reordering, adding, deleting

(s/def :code-block/start int?)
(s/def :code-block/end int?)
(s/def :code-block/id int?)
(s/def :code-block/meta map?)
(s/def :code-block/name string?)
(s/def :code-block/full-name (s/and string? #(str/includes? % "/")))
(s/def :code-block/contents delay?)                         ;; map of contents with :code :header :footer
(s/def :code-block/parsed-type keyword?)                    ;; most likely are :ns :def :reg (regexpr match) :lt-code-block-comment
(s/def :code-block/ns string?)
(s/def :code-block/file fs/path?)

(s/def :code-block/code-block (s/keys :req-un [:code-block/start
                                               :code-block/end
                                               :code-block/name
                                               :code-block/full-name
                                               :code-block/contents
                                               :code-block/file
                                               :code-block/id]
                                      :req [:code-block/parsed-type
                                            :code-block/ns]))



; rules: can see all and only top level forms. Can read its own output. 
; currently  it is cheating, using the fact that "\((def|ns) ..." is the shape
; of nearly every top level form. With the manual code marking it can do, should
; be fine. 


; format for our nice comments
; | ;; LTCodeBlockStart {:name ...} 
; | ;; LTCodeBlockEnd

;; these functions need to be extensible. Right now the blockify fn works ok with these as inputs.
(defn is-block-start? [line]
  (let [res (or (when (str/starts-with? line "(def") :def)
                (when (str/starts-with? line "(ns") :ns)
                (when (re-matches #"\([\w-\/*^]*reg.*" line) :reg)
                (when (str/starts-with? line ";; LTCodeBlockStart") :lt-code-block-comment)
                )]
    (if res res false)))

(defn try-extract-meta
  "given the first line of a code block and the type of matcher that extracted it,
  try to give some meta data on it. Most often it finds the name."
  [first-line type]
  (let [name (case type
               :def (second (first (re-seq #"def[\w-\.]*[ ,]+([^ ,\(\)]+)" first-line)))
               :ns (second (first (re-seq #"ns[\w-\.]*[ ,]+([^ ,\(\)]+)" first-line)))
               :reg (second (first (re-seq #"reg[\w-\.]*[ ,]+([^ ,\(\)]+)" first-line)))
               nil)]
    (if (= :lt-code-block-comment type)
      (let [first (str/index-of first-line "{")
            meta-str (apply str (drop (dec first) first-line))]
        (edn/read-string meta-str))
      (when name {:name name}))))

(defn comment-block-end? [line]
  (when (re-matches #";;[ ]+LTCodeBlockEnd[ ]*" line) true))


(defn vswap! "Renames a volatile fn to work on local vars" [x f] (var-set x (f @x)))


;; has two ending modes, if not in a comment code block, tries to end when it finds a new one.
;; if it is in a code block, it ignores new blocks until it hits a ;; lt-cblock-end comment

;; black hole of time. rewrite before editing in most cases I think
(defn- blockify-file
  "Extract raw code blocks from a file. Never use directly, instead prefer extract-blocks-from-file"
  [file]
  (with-open [in (io/reader file)]
    (let [lines (line-seq in)]
      (with-local-vars
        [remaining-lines lines
         code-blocks []
         block-start nil
         guessed-block-end nil
         type-of-block nil
         block-meta nil
         line-num 0]
        (while (not-empty @remaining-lines)
          (let [line (first @remaining-lines)
                end-of-file? (empty? (rest @remaining-lines))
                close-code-block
                (fn []
                  (when-let [last-blk-start @block-start]   ;; had a block prev
                    (vswap! code-blocks #(conj % {:start                  last-blk-start
                                                  :end                    @guessed-block-end
                                                  :meta                   @block-meta
                                                  :code-block/parsed-type @type-of-block
                                                  :file                   file})))
                  (var-set type-of-block nil)
                  (var-set block-start nil))                ;; erase prev vals
                begin-new-block
                (fn [type extracted-meta]
                  (var-set block-start @line-num)           ;; reset block start and give meta
                  (var-set type-of-block type)
                  (var-set block-meta extracted-meta))]

            ;; line inc, dump curr line off seq, if eof, jump  g-blk-end to line num
            (vswap! remaining-lines rest)
            (vswap! line-num inc)
            (when end-of-file?                              ;; don't set guessed block
              ;; end to line when blank here. must wait for the code to check that
              ;; this isn't a new code block currently. 
              (var-set guessed-block-end @line-num))

            (when (not= :lt-code-block-comment @type-of-block) ;; not in comment block
              (when-let [type (is-block-start? line)]       ;; new block start
                (close-code-block)
                (begin-new-block type (try-extract-meta line type))))

            (when (not (str/blank? line))                   ;; not line blank set as guessed block end
              (var-set guessed-block-end @line-num))

            (when (or end-of-file?                          ;; eof or end of code block comment
                      (and (= :lt-code-block-comment @type-of-block)
                           (comment-block-end? line)))
              (close-code-block))))
        @code-blocks
        ))))

(defn get-code-block-contents
  "get all of the lines in the block with keys {:start :end :file}. returns a map of the shape 
  {:code str
   :footer str
   :header str}" [block]
  (let [commented-code-block? (= :lt-code-block-comment (:code-block/parsed-type block))
        {:keys [start end]} block
        raw-block (with-open [in (io/reader (:file block))]
                    (loop [lines (line-seq in)
                           line-num 1
                           ret []]
                      (let [ret (if (<= start line-num end) (conj ret (first lines)) ret)]
                        (if (<= line-num end)
                          (recur (rest lines) (inc line-num) ret)
                          ret))))
        header (if commented-code-block? (first raw-block) "")
        footer (if commented-code-block? (last raw-block) "")]
    {:code   (str/join "\n" raw-block)
     :footer footer
     :header header}))


(defn rewrite-code-block
  "take a code block description and a string, and replace all the original lines
  with the exact string"
  [block ^String new-code]
  (let [old (:file block)
        {:keys [^String header ^String footer]} @(:contents block)
        new (fs/path (str (fs/normalize old) ".swp"))]
    
    (with-open [in (io/reader old)
                out (io/writer new :append false)]
      (loop [line-num 0
             lines (line-seq in)]
        (let [line-num (inc line-num)
              ^String line (first lines)
              lines (rest lines)]
          (when (not (<= (:start block) line-num (:end block)))
            (.write out line)
            (.newLine out))
          (when (= (:start block) line-num)
            (when (not (str/blank? header))
              (.write out header)
              (.newLine out))
            (.write out new-code)
            (.newLine out)
            (when (not (str/blank? footer))
              (.write out footer)
              (.newLine out)))
          (if (not-empty lines)
            (recur line-num lines)
            nil))))
    (fs/move new old)
    ;(fs/delete new)
    ))

(defn extract-blocks-from-file
  "primary function of the ns. returns a speced :code-block"
  [file]
  (let [blocks (try (blockify-file file)
                    (catch Exception e
                      (println e)))
        namespace (-> (filter #(= :ns (:code-block/parsed-type %)) blocks)
                      first :meta :name)
        attached-ns (map #(assoc % :code-block/ns namespace
                                   :name (-> % :meta :name)
                                   :contents (delay (get-code-block-contents %))
                                   :full-name (str namespace "/" (-> % :meta :name)))
                         blocks)]
    attached-ns))

(def extensions #{"clj" "cljs" "cljc"})

(defn- files-in-dir [dir]
  (loop [to-visit [dir]
         completed []]
    (cond (empty? to-visit) completed
          (fs/directory? (first to-visit))                  ; cant explain it, sometimes has a nil from fs/list-dir
          (recur (concat (rest to-visit) (filter identity (fs/list-dir (first to-visit))))
                 (conj completed (first to-visit)))
          :else
          (recur (rest to-visit) (conj completed (first to-visit))))))


(defn blocks-in-dir [dir]
  (->> dir files-in-dir
       (filter #(some (partial = (fs/ext %)) extensions))
       (mapcat extract-blocks-from-file)
       (map-indexed (fn [idx e] (assoc e :id idx)))))

(defn by-unique-key
  "key must be either :full-name or :id"
  [code-blocks key]
  (->> code-blocks
       (group-by key)
       (map (fn [[k v]] [k (first v)]))
       (into {})))


(defn print-code-block "purely for debugging" [cb]
  (println "----")
  (println (->> cb :contents deref :code))
  (println "----"))


(comment
  (def dir (fs/path "/media/jarrett/Windows/code-projects/electron-apps/reframe-transform/src"))
  (def nss (blocks-in-dir dir))

  (mapcat extract-blocks-from-file files)
  (->> files first extract-blocks-from-file first :contents deref) ;contents are (delay)

  (def nss-by-name (->> nss by-full-name))
  (print-code-block (get nss-by-name "noted.events.preview-note-events/preview-state->"))) 