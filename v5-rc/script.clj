(ns script
  (:require
   [clojure.java.io :as io]
   [clj-yaml.core :as yaml]
   [clojure.walk :as walk]
   [clojure.string :as str]
   [babashka.fs :as fs]))

(def spec-file (io/file "spec.yaml"))

(defn slurp-yaml
  [path]
  (yaml/parse-string (slurp path)
                     :keywords false))

(def top-level-spec (slurp-yaml spec-file))

(defn ref?
  [form]
  (and (map? form)
       (contains? form "$ref")))

(defn attach-path-if-ref
  [path form]
  (if (and path (ref? form))
    (assoc form :path path)
    form))

(defn relative-to-file
  [file-path path]
  (if file-path
    (let [current-folder (fs/parent file-path)]
      (loop [result (vec (fs/components current-folder))
             segments (vec (fs/components path))]
        (let [[segment & segments] segments]
          (if segment
            (recur
             (case (str segment)
               "." result
               ".." (vec (butlast result))
               (conj result segment))
             segments)
            (str (apply fs/path result))))))
    path))

(defn attach-path-to-refs
  [path form]
  (walk/postwalk (partial attach-path-if-ref path) form))

;; get properties
(defn properties
  [schema-file-path]
  (let [first-schema (attach-path-to-refs schema-file-path (slurp-yaml schema-file-path))]
    (loop [schemas [first-schema]
           props {}]
      (if (empty? schemas)
        props
        (let [schema (first schemas)
              current-path (:path schema)]
          (cond
            (contains? schema "properties")
            (recur (rest schemas) (into props (get schema "properties")))

            (contains? schema "allOf")
            (recur (into (rest schemas) (map (partial attach-path-if-ref current-path) (get schema "allOf"))) props)

            (ref? schema)
            (let [new-path (relative-to-file current-path (get schema "$ref"))
                  new-schema (assoc (slurp-yaml new-path) :path new-path)]
              (recur (conj (rest schemas) new-schema) props))))))))

(def entities
  [{:name "AcademicSession"
    :file "schemas/AcademicSession.yaml"}
   {:name "Building"
    :file "schemas/Building.yaml"}
   {:name "Component"
    :file "schemas/Component.yaml"}
   {:name "ComponentOffering"
    :file "schemas/ComponentOffering.yaml"}
   {:name "ComponentOfferingAssociation"
    :file "schemas/ComponentOfferingAssociation.yaml"}
   {:name "Course"
    :file "schemas/Course.yaml"}
   {:name "CourseOffering"
    :file "schemas/CourseOffering.yaml"}
   {:name "CourseOfferingAssociation"
    :file "schemas/CourseOfferingAssociation.yaml"}
   {:name "EducationSpecification"
    :file "schemas/EducationSpecification.yaml"}
   {:name "Group"
    :file "schemas/Group.yaml"}
   {:name "NewsFeed"
    :file "schemas/NewsFeed.yaml"}
   {:name "NewsItem"
    :file "schemas/NewsItem.yaml"}
   {:name "Organization"
    :file "schemas/Organization.yaml"}
   {:name "Person"
    :file "schemas/Person.yaml"}
   {:name "Program"
    :file "schemas/Program.yaml"}
   {:name "ProgramOffering"
    :file "schemas/ProgramOffering.yaml"}
   {:name "ProgramOfferingAssociation"
    :file "schemas/ProgramOfferingAssociation.yaml"}
   {:name "Room"
    :file "schemas/Room.yaml"}
   {:name "Service"
    :file "schemas/Service.yaml"}])

(def relationship-types
  {:? ["|o" "o|"] ;; Zero or one
   :1 ["||" "||"] ;; Exactly one
   :* ["}o" "o{"] ;; Zero or more (no upper limit)
   :+ ["}|" "|{"]}) ;; One or more (no upper limit)

:- ; normal
:ext ; extends
:comp ; composition

; https://kroki.io/

(def relations
  [["EducationSpecification" [:? :?] "EducationSpecification" "hierarchic"]
   ["EducationSpecification" [:? :?] "Program" "has"]
   ["EducationSpecification" [:? :?] "Course" "has"]
   ["Program" [:? :?] "Program" "hierarchic"]
   ["Program" [:* :*] "Person" "coordinator"]
   ["Program" [:1 :*] "ProgramOffering" "has"]
   ["ProgramOffering" [:1 :*] "ProgramOfferingAssociation" "has"]
   ["ProgramOffering" [:* :?] "AcademicSession" "has"]
   ["ProgramOfferingAssociation" [:* :1] "Person" "has"]
   ["Program" [:* :*] "Course" "has"]
   ["Course" [:1 :*] "CourseOffering" "has"]
   ["Course" [:* :*] "Person" "coordinator"]
   ["CourseOffering" [:1 :*] "CourseOfferingAssociation" "has"]
   ["CourseOffering" [:* :?] "AcademicSession" "has"]
   ["CourseOfferingAssociation" [:* :1] "Person" "has"]
   ["Course" [:* :*] "Component" "has"]
   ["Component" [:1 :*] "ComponentOffering" "has"]
   ["ComponentOffering" [:1 :*] "ComponentOfferingAssociation" "has"]
   ["ComponentOffering" [:* :?] "AcademicSession" "has"]
   ["ComponentOffering" [:* :?] "Room" "in"]
   ["ComponentOfferingAssociation" [:* :1] "Person" "has"]
   ["AcademicSession" [:? :?] "AcademicSession" "hierarchic"]
   ["Organization" [:? :*] "EducationSpecification" "has"]
   ["Organization" [:? :*] "Program" "has"]
   ["Organization" [:? :*] "Course" "has"]
   ["Organization" [:? :*] "Component" "has"]
   ["Organization" [:? :*] "ProgramOffering" "has"]
   ["Organization" [:? :*] "CourseOffering" "has"]
   ["Organization" [:? :*] "ComponentOffering" "has"]
   ["Organization" [:? :*] "Group" "has"]
   ["Building" [:? :*] "Room" "contains"]
   ["Group" [:* :*] "Person" "membership"]
   ["Group" [:* :*] "ProgramOffering" "has"]
   ["Group" [:* :*] "CourseOffering" "has"]
   ["Group" [:* :*] "ComponentOffering" "has"]
   ["NewsFeed" [:+ :*] "NewsItem" "has"]
   ["Organization" [:? :?] "Organization" "hierarchic"]])

(def always-remove
  #{"academicSession" "building" "children" "component" "consumers" "coordinators" "course" "courseOffering" "educationSpecification" "ext" "newsFeeds" "organization" "parent" "program" "programOffering" "programs" "room" "year"})

(def preferred-ordering
  ["academicSessionId" "associationId" "buildingId" "componentId" "courseId" "educationSpecificationId" "groupId" "newsFeedId" "newsItemId" "offeringId" "organizationId" "personId" "programId" "roomId"
   "primaryCode" "otherCodes"
   "academicSessionType" "associationType" "componentType" "educationSpecificationType" "groupType" "newsFeedType" "newsItemType" "offeringType" "organizationType" "programType" "roomType"
   "name" "surname" "surnamePrefix" "givenName" "displayName" "titlePrefix" "titleSuffix"
   "shortName" "abbreviation"
   "description"
   "activeEnrollment"   "address"   "addresses"   "admissionRequirements"   "affiliations"   "assessment"   "authors"   "availableSeats"   "cityOfBirth"   "contactEmail"   "content"
   "countryOfBirth"   "dateOfBirth"   "dateOfNationality"   "documentation"   "duration"   "endDate"   "endDateTime"   "enrolledNumberStudents"
   "enrollEndDate"   "enrollment"   "enrollStartDate"   "fieldsOfStudy"   "firstStartDate"   "flexibleEntryPeriodEnd"   "flexibleEntryPeriodStart"   "floor"   "formalDocument"
   "gender"   "geolocation"   "ICEName"   "ICEPhoneNumber"   "ICERelation"   "image"   "initials"   "languageOfChoice"   "lastModified"   "learningOutcomes"   "level"   "levelOfQualification"
   "link"   "logo"   "mail"   "maxNumberStudents"   "minNumberStudents"   "mobileNumber"   "modeOfDelivery"   "modeOfStudy"   "nationality"   "office"   "pendingNumberStudents"
   "personCount"   "photoOfficial"   "photoSocial"   "priceInformation"   "qualificationAwarded"   "qualificationRequirements"   "remoteState"   "resources"   "result"   "resultExpected"   "resultValueType"   "resultWeight"
   "role"   "secondaryMail"   "sector"   "specification"   "startDate"   "startDateTime"   "state"   "studyLoad"   "teachingLanguage"   "telephoneNumber"
   "totalSeats"   "validFrom"   "validTo"   "validUntil"   "wing"])

(def ind "    ")

(defn str-prop
  [[name attrs]]
  (str ind ind (get attrs "type" "unknown") " " name))

(def preferred-order
  (apply hash-map (interleave preferred-ordering (range))))

(->> (properties "./schemas/Program.yaml")
     (remove (comp always-remove key))
     (sort-by (fn [[k _]] (get preferred-order k))))

(defn er-entitity
  [{:keys [name file]}]
  (let [props (->> (properties file)
                   (remove (comp always-remove key))
                   (sort-by (fn [[k _]] (get preferred-order k))))
        str-props (str/join "\n" (map str-prop props))]
    (str ind name " {\n" str-props  "\n" ind "}")))

(defn er-relation
  [[name1 rel name2 label]]
  (str ind
       name1
       " "
       (first (get relationship-types (first rel)))
       ".."
       (second (get relationship-types (second rel)))
       " "
       name2
       " : "
       label))

(defn er-diagram
  []
  (str "erDiagram\n"
       (str/join "\n" (map er-relation relations))
       "\n"
       (str/join "\n" (map er-entitity entities))))

(spit "er-diagram.txt" (er-diagram))

(def erd-relationship-types
  {:? "?" ;; Zero or one
   :1 "1" ;; Exactly one
   :* "*" ;; Zero or more (no upper limit)
   :+ "+"}) ;; One or more (no upper limit)

(defn erd-str-props
  [[name _]]
  (str name))

(defn erd-entity
  [{:keys [name file]}]
  (let [props (->> (properties file)
                   (remove (comp always-remove key))
                   (sort-by (fn [[k _]] (get preferred-order k))))
        str-props (str/join "\n" (map erd-str-props props))]
    (str "[" name "]\n" str-props "\n")))

(defn erd-relation
  [[name1 rel name2 _label]]
  (str name1
       " "
       (get erd-relationship-types (first rel))
       "--"
       (get erd-relationship-types (second rel))
       " "
       name2))

(defn erd-diagram
  []
  (str (str/join "\n" (map erd-entity entities))
       "\n"
       (str/join "\n" (map erd-relation relations))))

(spit "erd-diagram.txt" (erd-diagram))