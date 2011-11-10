;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; GNOME GLib bindings
;;
;; This is here as a placeholder to be added to over time.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define gliblib (sys:open-dylib (if (string=? "Linux" (sys:platform))
				    "libglib-2.0.so"
				    (println "Where can I find glib on your platform?"))))

;; some GLib type aliases
;; note that some of these differ from GLib's actual implementation!
(bind-alias GHashFunc i8*)
(bind-alias GEqualFunc i8*)
(bind-alias GHashTable i8*) ;; GHashTable in GLib is actually an opaque struct (not pointer to struct)
(bind-alias GRegex i8*) ;; GRegex in GLib is actually an opaque struct (not pointer to struct)
(bind-alias GMatchInfo i8*) ;; GMatchInfo in GLib is actually an opaque struct (not pointer to struct)
(bind-alias gconstpointer i8*)
(bind-alias gpointer i8*)
(bind-alias gchar i8)
(bind-alias guint i32)
(bind-alias gssize i64)
(bind-alias GQuark i32)
(bind-alias GRegexMatchFlags i32)
(bind-alias gint i32)
(bind-alias gboolean i1) ;; GLib defines this as i32

;; some types
(bind-type GError <GQuark,gint,gchar*>)

;; hash table stuff
(bind-lib gliblib g_hash_table_new [GHashTable,GHashFunc,GEqualFunc]*)
(bind-lib gliblib g_hash_table_destroy [void,GHashTable]*)
(bind-lib gliblib g_hash_table_insert [void,GHashTable,gpointer,gpointer]*)
(bind-lib gliblib g_hash_table_lookup [gpointer,GHashTable,gpointer]*)
(bind-lib gliblib g_str_hash [guint,gconstpointer]*)
(bind-lib gliblib g_str_equal [gboolean,gconstpointer,gconstpointer]*)

;; misc stuff
(bind-lib gliblib g_get_user_name [gchar*]*)
(bind-lib gliblib g_get_real_name [gchar*]*)
(bind-lib gliblib g_get_host_name [gchar*]*)
(bind-lib gliblib g_get_home_dir [gchar*]*)
(bind-lib gliblib g_get_environ [gchar**]*)
(bind-lib gliblib g_getenv [gchar*,gchar*]*)
(bind-lib gliblib g_setenv [gboolean,gchar*]*)
(bind-lib gliblib g_unsetenv [void,gchar*]*)
(bind-lib gliblib g_spaced_primes_closest [guint,guint]*)

;; regex stuff
(bind-lib gliblib g_match_info_matches [gboolean,GMatchInfo]*)
(bind-lib gliblib g_match_info_next [gboolean,GMatchInfo,GError**]*)
(bind-lib gliblib g_match_info_get_match_count [gint,GMatchInfo]*)
(bind-lib gliblib g_match_info_is_partial_match [gboolean,GMatchInfo]*)
(bind-lib gliblib g_match_info_fetch [gchar*,GMatchInfo,gint]*)
(bind-lib gliblib g_match_info_fetch_named [gchar*,GMatchInfo,gchar*]*)
(bind-lib gliblib g_match_info_fetch_all [gchar**,GMatchInfo]*)
(bind-lib gliblib g_match_info_free [void,GMatchInfo]*)

(bind-lib gliblib g_regex_match_simple [i1,gchar*,gchar*,i32,i32]*)
(bind-lib gliblib g_regex_split_simple [gchar**,gchar*,gchar*,i32,i32]*)
(bind-lib gliblib g_regex_new [GRegex,gchar*,i32,i32,GError**]*)
(bind-lib gliblib g_regex_unref [void,GRegex]*)
(bind-lib gliblib g_regex_match [gboolean,GRegex,gchar*,i32,i8**]*)
(bind-lib gliblib g_regex_match_all [gboolean,GRegex,gchar*,i32,i8**]*)
(bind-lib gliblib g_regex_match_full [gboolean,GRegex,gchar*,gssize,gint,i32,i8**,GError**]*)
(bind-lib gliblib g_regex_match_all_full [gboolean,GRegex,gchar*,gssize,gint,i32,i8**,GError**]*)
(bind-lib gliblib g_regex_split [gchar**,GRegex,gchar*,GRegexMatchFlags]*)
(bind-lib gliblib g_regex_split_full [gchar**,GRegex,gchar*,gssize,gint,GRegexMatchFlags,gint,GError**]*)
(bind-lib gliblib g_regex_replace [gchar*,GRegex,gchar*,gssize,gint,gchar*,GRegexMatchFlags,GError**]*)
(bind-lib gliblib g_regex_replace_literal [gchar*,GRegex,gchar*,gssize,gint,gchar*,GRegexMatchFlags,GError**]*)