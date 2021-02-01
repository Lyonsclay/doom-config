;;; postgres-keywords.el -*- lexical-binding: t; -*-

(defvar sql-mode-postgres-font-lock-keywords
  (eval-when-compile
    (list
     ;; Postgres psql commands
     '("^\\s-*\\\\.*$" . font-lock-doc-face)

     ;; Postgres unreserved words but may have meaning
     (sql-font-lock-keywords-builder 'font-lock-builtin-face nil "a"
"abs" "absent" "according" "ada" "alias" "allocate" "are" "array_agg"
"asensitive" "atomic" "attribute" "attributes" "avg" "base64"
"bernoulli" "bit_length" "bitvar" "blob" "blocked" "bom" "breadth" "c"
"call" "cardinality" "catalog_name" "ceil" "ceiling" "char_length"
"character_length" "character_set_catalog" "character_set_name"
"character_set_schema" "characters" "checked" "class_origin" "clob"
"cobol" "collation" "collation_catalog" "collation_name"
"collation_schema" "collect" "column_name" "columns"
"command_function" "command_function_code" "completion" "condition"
"condition_number" "connect" "connection_name" "constraint_catalog"
"constraint_name" "constraint_schema" "constructor" "contains"
"control" "convert" "corr" "corresponding" "count" "covar_pop"
"covar_samp" "cube" "cume_dist" "current_default_transform_group"
"current_path" "current_transform_group_for_type" "cursor_name"
"datalink" "datetime_interval_code" "datetime_interval_precision" "db"
"defined" "degree" "dense_rank" "depth" "deref" "derived" "describe"
"descriptor" "destroy" "destructor" "deterministic" "diagnostics"
"disconnect" "dispatch" "dlnewcopy" "dlpreviouscopy" "dlurlcomplete"
"dlurlcompleteonly" "dlurlcompletewrite" "dlurlpath" "dlurlpathonly"
"dlurlpathwrite" "dlurlscheme" "dlurlserver" "dlvalue" "dynamic"
"dynamic_function" "dynamic_function_code" "element" "empty"
"end-exec" "equals" "every" "exception" "exec" "existing" "exp" "file"
"filter" "final" "first_value" "flag" "floor" "fortran" "found" "free"
"fs" "fusion" "g" "general" "generated" "get" "go" "goto" "grouping"
"hex" "hierarchy" "host" "id" "ignore" "implementation" "import"
"indent" "indicator" "infix" "initialize" "instance" "instantiable"
"integrity" "intersection" "iterate" "k" "key_member" "key_type" "lag"
"last_value" "lateral" "lead" "length" "less" "library" "like_regex"
"link" "ln" "locator" "lower" "m" "map" "matched" "max"
"max_cardinality" "member" "merge" "message_length"
"message_octet_length" "message_text" "method" "min" "mod" "modifies"
"modify" "module" "more" "multiset" "mumps" "namespace" "nclob"
"nesting" "new" "nfc" "nfd" "nfkc" "nfkd" "nil" "normalize"
"normalized" "nth_value" "ntile" "nullable" "number"
"occurrences_regex" "octet_length" "octets" "old" "open" "operation"
"ordering" "ordinality" "others" "output" "overriding" "p" "pad"
"parameter" "parameter_mode" "parameter_name"
"parameter_ordinal_position" "parameter_specific_catalog"
"parameter_specific_name" "parameter_specific_schema" "parameters"
"pascal" "passing" "passthrough" "percent_rank" "percentile_cont"
"percentile_disc" "permission" "pli" "position_regex" "postfix"
"power" "prefix" "preorder" "public" "rank" "reads" "recovery" "ref"
"referencing" "regr_avgx" "regr_avgy" "regr_count" "regr_intercept"
"regr_r2" "regr_slope" "regr_sxx" "regr_sxy" "regr_syy" "requiring"
"respect" "restore" "result" "return" "returned_cardinality"
"returned_length" "returned_octet_length" "returned_sqlstate" "rollup"
"routine" "routine_catalog" "routine_name" "routine_schema"
"row_count" "row_number" "scale" "schema_name" "scope" "scope_catalog"
"scope_name" "scope_schema" "section" "selective" "self" "sensitive"
"server_name" "sets" "size" "source" "space" "specific"
"specific_name" "specifictype" "sql" "sqlcode" "sqlerror"
"sqlexception" "sqlstate" "sqlwarning" "sqrt" "state" "static"
"stddev_pop" "stddev_samp" "structure" "style" "subclass_origin"
"sublist" "submultiset" "substring_regex" "sum" "system_user" "t"
"table_name" "tablesample" "terminate" "than" "ties" "timezone_hour"
"timezone_minute" "token" "top_level_count" "transaction_active"
"transactions_committed" "transactions_rolled_back" "transform"
"transforms" "translate" "translate_regex" "translation"
"trigger_catalog" "trigger_name" "trigger_schema" "trim_array"
"uescape" "under" "unlink" "unnamed" "unnest" "untyped" "upper" "uri"
"usage" "user_defined_type_catalog" "user_defined_type_code"
"user_defined_type_name" "user_defined_type_schema" "var_pop"
"var_samp" "varbinary" "variable" "whenever" "width_bucket" "within"
"xmlagg" "xmlbinary" "xmlcast" "xmlcomment" "xmldeclaration"
"xmldocument" "xmlexists" "xmliterate" "xmlnamespaces" "xmlquery"
"xmlschema" "xmltable" "xmltext" "xmlvalidate"
)

     ;; Postgres non-reserved words
     (sql-font-lock-keywords-builder 'font-lock-builtin-face nil
"abort" "absolute" "access" "action" "add" "admin" "after" "aggregate"
"also" "alter" "always" "assertion" "assignment" "at" "attribute" "backward"
"before" "begin" "between" "by" "cache" "called" "cascade" "cascaded"
"catalog" "chain" "characteristics" "checkpoint" "class" "close"
"cluster" "coalesce" "comment" "comments" "commit" "committed"
"configuration" "connection" "constraints" "content" "continue"
"conversion" "copy" "cost" "createdb" "createrole" "createuser" "csv"
"current" "cursor" "cycle" "data" "database" "day" "deallocate" "dec"
"declare" "defaults" "deferred" "definer" "delete" "delimiter"
"delimiters" "dictionary" "disable" "discard" "document" "domain"
"drop" "each" "enable" "encoding" "encrypted" "enum" "escape"
"exclude" "excluding" "exclusive" "execute" "exists" "explain"
"extension" "external" "extract" "family" "first" "float" "following" "force"
"forward" "function" "functions" "global" "granted" "greatest"
"handler" "header" "hold" "hour" "identity" "if" "immediate"
"immutable" "implicit" "including" "increment" "index" "indexes"
"inherit" "inherits" "inline" "inout" "input" "insensitive" "insert"
"instead" "invoker" "isolation" "key" "label" "language" "large" "last"
"lc_collate" "lc_ctype" "leakproof" "least" "level" "listen" "load" "local"
"location" "lock" "login" "mapping" "match" "maxvalue" "minute"
"minvalue" "mode" "month" "move" "names" "national" "nchar"
"next" "no" "nocreatedb" "nocreaterole" "nocreateuser" "noinherit"
"nologin" "none"  "noreplication" "nosuperuser" "nothing" "notify" "nowait" "nullif"
"nulls" "object" "of" "off" "oids" "operator" "option" "options" "out"
"overlay" "owned" "owner" "parser" "partial" "partition" "passing" "password"
"plans" "position" "preceding" "precision" "prepare" "prepared" "preserve" "prior"
"privileges" "procedural" "procedure" "quote" "range" "read"
"reassign" "recheck" "recursive" "ref" "reindex" "relative" "release"
"rename" "repeatable" "replace" "replica" "replication" "reset" "restart" "restrict"
"returns" "revoke" "role" "rollback" "row" "rows" "rule" "savepoint"
"schema" "scroll" "search" "second" "security" "sequence"
"serializable" "server" "session" "set" "setof" "share" "show"
"simple" "snapshot" "stable" "standalone" "start" "statement" "statistics"
"stdin" "stdout" "storage" "strict" "strip" "substring" "superuser"
"sysid" "system" "tables" "tablespace" "temp" "template" "temporary"
"transaction" "treat" "trim" "truncate" "trusted" "type" "types"
"unbounded" "uncommitted" "unencrypted" "unlisten" "unlogged" "until"
"update" "vacuum" "valid" "validate" "validator" "value" "values" "varying" "version"
"view" "volatile" "whitespace" "without" "work" "wrapper" "write"
"xmlattributes" "xmlconcat" "xmlelement" "xmlexists" "xmlforest" "xmlparse"
"xmlpi" "xmlroot" "xmlserialize" "year" "yes" "zone"
)

     ;; Postgres Reserved
     (sql-font-lock-keywords-builder 'font-lock-keyword-face nil
"all" "analyse" "analyze" "and" "array" "asc" "as" "asymmetric"
"authorization" "binary" "both" "case" "cast" "check" "collate"
"column" "concurrently" "constraint" "create" "cross"
"current_catalog" "current_date" "current_role" "current_schema"
"current_time" "current_timestamp" "current_user" "default"
"deferrable" "desc" "distinct" "do" "else" "end" "except" "false"
"fetch" "foreign" "for" "freeze" "from" "full" "grant" "group"
"having" "ilike" "initially" "inner" "in" "intersect" "into" "isnull"
"is" "join" "leading" "left" "like" "limit" "localtime"
"localtimestamp" "natural" "notnull" "not" "null" "offset"
"only" "on" "order" "or" "outer" "overlaps" "over" "placing" "primary"
"references" "returning" "right" "select" "session_user" "similar"
"some" "symmetric" "table" "then" "to" "trailing" "true" "union"
"unique" "user" "using" "variadic" "verbose" "when" "where" "window"
"with"
)

     ;; Postgres PL/pgSQL
     (sql-font-lock-keywords-builder 'font-lock-keyword-face nil
"assign" "if" "case" "loop" "while" "for" "foreach" "exit" "elsif" "return"
"raise" "execsql" "dynexecute" "perform" "getdiag" "open" "fetch" "move" "close"
)

     ;; Postgres Data Types
     (sql-font-lock-keywords-builder 'font-lock-type-face nil
"bigint" "bigserial" "bit" "bool" "boolean" "box" "bytea" "char"
"character" "cidr" "circle" "date" "decimal" "double" "float4"
"float8" "inet" "int" "int2" "int4" "int8" "integer" "interval" "line"
"lseg" "macaddr" "money" "name" "numeric" "path" "point" "polygon"
"precision" "real" "serial" "serial4" "serial8" "sequences" "smallint" "text"
"time" "timestamp" "timestamptz" "timetz" "tsquery" "tsvector"
"txid_snapshot" "unknown" "uuid" "varbit" "varchar" "varying" "without"
"xml" "zone"
)))

  "Postgres SQL keywords used by font-lock.

This variable is used by `sql-mode' and `sql-interactive-mode'.  The
regular expressions are created during compilation by calling the
function `regexp-opt'.  Therefore, take a look at the source before
you define your own `sql-mode-postgres-font-lock-keywords'.")
