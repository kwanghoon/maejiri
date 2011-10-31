
module PCCAgent where

---------------------------------------------------------------------------------
-- LF signature for the syntax of first-order predicates logic
-- with equality and subscripted variables, showing expression
-- and predicate constructors.
---------------------------------------------------------------------------------

fol_kind_decls =
  [
    ("iota", "Type"),
    ("o",    "Type"),
    ("w",    "Type"),
    ("s",    "Type")
  ]
  
fol_type_decls =  
  [
    ("zero", "iota"),
    ("sel",  "iota -> iota -> iota"),
    ("upd",  "iota -> iota -> iota"),
    
    ("int",  "w"),
    ("list", "w -> w"),
    ("list", "w -> w"),
    ("seq1", "w -> s"),
    ("seq2", "w -> s -> s"),
    ("ptr", "s -> w"),
    ("settype",  "(iota -> o) -> w"),
    
    ("true", "o"),
    ("and",  "o -> o -> o"),
    ("imp", "o -> o -> o"),
    ("all",  "(iota -> o) -> o"),
    ("eq",   "iota -> iota -> o"),
    ("neq",  "iota -> iota -> o"),
    ("addr", "iota -> o"),
    ("hastype",  "iota -> w -> o"),
    ("ge",  "iota -> iota -> o")
  ]
  
---------------------------------------------------------------------------------
-- LF signature for safety policy proof rules
---------------------------------------------------------------------------------

policy_kind_decls =
  [
    ("pf", "o -> Type")
  ]
  
policy_type_decls =
  [
    ("truei", "pf Type"),
    ("andi",  "/\\p:o./\\r:o.pf p -> pf r -> pf (and p r)"),
    ("andel", "/\\p:o./\\r:o.pf (and p r) -> pf p"),
    ("ander", "/\\p:o./\\r:o.pf (and p r) -> pf r"),
    ("impi",  "/\\p:o./\\r:o.(pf p -> pf r) -> pf (imp p r)"),
    ("impe",  "/\\p:o./\\r:o.pf (imp p r) -> pf p -> pf r"),
    ("alli",  "/\\p:iota->o.(/\\nu:iota.pf (p nu)) -> pf (all p)"),
    ("alle",  "/\\p:iota->o./\\e:iota.pf (all p) -> pf (p e)"),
    ("mem0",  "/\\m:iota./\\a:iota./\\nu:iota./\\a':iota.pf (eq a a') -> pf (eq (sel (upd m a nu) a') nu)"),
    ("mem1",  "/\\m:iota./\\a:iota./\\nu:iota./\\a':iota.pf (neq a a') -> pf (eq (sel (upd m a nu) a') (sel m a'))"),
    ("cons",  "/\\E:iota./\\W:w.pf (hastype E (list W)) -> pf (neq E zero) -> pf (hastype E (ptr (seq2 W (seq1 (list W)))))"),
    ("set",   "/\\E:iota./\\F:iota->o.pf (hastype E (settype F)) -> pf (F E)")
  ]

---------------------------------------------------------------------------------
-- Example: The proof of the predicate \/a.a : ptr {int} => addr a.
---------------------------------------------------------------------------------

extra_decls =
  [
    ("ptraddr",
     "/\\A:iota./\\S:s.pf (hastype A (ptr S)) -> pf (addr A)"
    )
  ]

pf_mm  = pf_mm0 ++ pf_mm1
pf_mm0 = "alli ( \\a:iota. (imp (hastype a (ptr (seq1 int))) (addr a)) )"
pf_mm1 = "( \\a:iota. \ 
               \ (impi \
                 \ (hastype a (ptr (seq1 int))) \ 
                 \ (addr a) \
                 \ (\\u:pf (hastype a (ptr (seq1 int))). \
                      \(ptraddr a (seq1 int) u))))"





