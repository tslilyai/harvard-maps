module type QUERY_ARG = 
sig
  module S : Myset.SET with type elt = Util.CrawlerServices.link
  module D : Dict.DICT with type key = string
                       with type value = S.set
end

module Query(A : QUERY_ARG) = 
struct
                       
  let query_re = Str.regexp "\\?q=\\(.*\\)"
  let term_sep_re = Str.regexp "\\+"
    
  (* now returns a list rather than a query *)
  let parse_query s = 
    if Str.string_match query_re s 0 then 
      let qs = Str.matched_group 1 s in 
      let words = Str.split term_sep_re qs 
      in 
        (*parse_words *) words
    else raise (Failure "query not understood")

end
