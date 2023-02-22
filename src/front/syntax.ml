(** {1 The shallow abstract syntax} *)

(** A location represents a range of characters from the input file.
    TODO: this is inherited from ocamllex and not really used anymore, cleanup.
*)
type location = {
  loc_file : string;
  start_pos : int;
  end_pos : int;
  start_line : int;
  start_col : int;
}

type quantifier_kind =
  | Longest  (* aka "greedy" *)
  | Shortest (* aka "lazy" *)

(** This represents a piece of OCaml code, as appearing in semantic
    actions, as well as in the header and trailer. *)
type ocaml_code = location * string

(** A position in the input file. *)
type position = {line: int; col: int}

(** A grammar symbol (a terminal or a non-terminal) *)
type symbol =
  | Name of string
  (** Symbols are usually simples names, like a or X *)
  | Apply of string * symbol list
  (** Menhir supports higher-order non-terminals. In this case, a symbol is
      the application of the higher-order non-terminal to a some arguments.
      E.g separated_list(sep, X) is represented as:
        [Apply ("separated_list", [Name "sep"; Name "X"])] *)

type wild_symbol = symbol option

(** [regular_desc] describes the different cases of the regular expression
    syntax. *)

type regular_desc =
  | Atom of string option * wild_symbol
  | Alternative of regular_expr list
  (** A disjunction of multiple expressions.
      [e1 | e2 | e3] is represented as [Alternative [e1; e2; e3]] *)
  | Repetition of {
      kind: quantifier_kind;
      expr: regular_expr;
    }
  (** [Repetition e] represents [e*] *)
  | Reduce of {
      capture: string option;
      kind: quantifier_kind;
      expr: regular_expr;
    }
  (** [Reduce] represents the [!] operator *)
  | Concat of regular_expr list
  (** [Concat [e1; e2; ..]] is [e1; e2; ...] *)
  | Filter of {
      lhs: wild_symbol;
      pre_anchored: bool;
      prefix: wild_symbol list;
      suffix: wild_symbol list;
      post_anchored: bool;
    }

(** [regular_expr] adds position information to [regular_desc] for error
    reporting purposes. *)
and regular_expr = {
  desc: regular_desc;
  position: position; (** the position where this term ends *)
}

(** The semantic action associated to a pattern *)
type clause_action =
  | Total of ocaml_code   (** ... { code }, normal semantic action **)
  | Partial of ocaml_code (** ... partial { ... }, a semantic action that can
                              return [None] to continue matching *)
  | Unreachable           (** [... { . }] the pattern should never match *)

(** A clause is a pair of a pattern and an action, representing one rule *)
type clause = {
  pattern: regular_expr; (** the pattern *)
  lookaheads: string list; (** restrict matching to these lookahead terminals, or [] for all terminals *)
  action: clause_action; (** the semantic action *)
}

(** An .mlyl file can contain multiple entrypoints, each is represented as an
    instance of [entry] *)
type entry = {
  startsymbols: string list; (** TODO: unused for now, the list of startsymbols
                                 of the grammar that this rule applies on. *)
  error   : bool; (** true if the entry is of the form
                        rule x ... = parse error
                        | ...
                      This entrypoint only matches stack that ended up in an
                      error. *)
  name    : string; (** the name of this entry point *)
  args    : string list; (** the list of OCaml arguments to abstract over,
                             e.g the [x y] in [rule foo x y = ...] *)
  clauses : clause list; (** the list of clause that are matched *)
}

(** An .mlyl file is an header containing some OCaml code, one or more entries,
    and a trailer with some other OCaml code. *)
type lexer_definition = {
  header      : ocaml_code;
  entrypoints : entry list;
  trailer     : ocaml_code;
}

(** This definition is used by the interpreter, not the analyser.
    The interpreter reads a list of [prompt_sentence] commands from
    standard input. *)
type prompt_sentence =
  | Prompt_entrypoint of symbol
  (** Starts interpretation from this grammar entrypoint *)
  | Prompt_interpret of symbol list
  (** Interpret this list of symbol *)

(** {1 Helper and cmoning functions} *)

let make_position {Lexing. pos_lnum; pos_cnum; pos_bol; _} =
  {line = pos_lnum; col = pos_cnum - pos_bol + 1}

let make_location startpos endpos = {
  loc_file = startpos.Lexing.pos_fname;
  start_line = startpos.Lexing.pos_lnum;
  start_pos = startpos.Lexing.pos_cnum;
  start_col =  startpos.Lexing.pos_cnum - startpos.Lexing.pos_bol + 1;
  end_pos = endpos.Lexing.pos_cnum;
}

let cmon_location {
    loc_file;
    start_pos;
    end_pos;
    start_line;
    start_col;
  } = Cmon.(record [
    "loc_file"  , string loc_file;
    "start_pos" , int start_pos;
    "end_pos"   , int end_pos;
    "start_line", int start_line;
    "start_col" , int start_col;
  ])

let cmon_ocamlcode (location, code) =
  Cmon.tuple [
    cmon_location location;
    Cmon.string code;
  ]

let cmon_position {line; col} =
  Cmon.(record ["line", int line; "col", int col])

let cmon_option f = function
  | None -> Cmon.constant "None"
  | Some x -> Cmon.constructor "Some" (f x)

let rec cmon_symbol = function
  | Name sym -> Cmon.constructor "Name" (Cmon.string sym)
  | Apply (sym, args) -> Cmon.construct "Apply" [
      Cmon.string sym;
      Cmon.list (List.map cmon_symbol args);
    ]

let cmon_capture cap =
  cmon_option Cmon.string cap

let cmon_wild_symbol sym =
  cmon_option cmon_symbol sym

let cmon_quantifier_kind = function
  | Longest -> Cmon.constant "Longest"
  | Shortest -> Cmon.constant "Shortest"

let rec cmon_regular_term = function
  | Atom (cap, sym) ->
    Cmon.construct "Atom" [cmon_capture cap; cmon_wild_symbol sym]
  | Alternative res ->
    Cmon.constructor "Alternative" (Cmon.list_map cmon_regular_expression res)
  | Concat res ->
    Cmon.constructor "Concat" (Cmon.list_map cmon_regular_expression res)
  | Repetition {kind; expr} ->
    Cmon.crecord "Repetition" [
      "kind", cmon_quantifier_kind kind;
      "expr", cmon_regular_expression expr;
    ]
  | Reduce {capture; kind; expr} ->
    Cmon.crecord "Reduce" [
      "capture", cmon_capture capture;
      "kind", cmon_quantifier_kind kind;
      "expr", cmon_regular_expression expr;
    ]
  | Filter {lhs; pre_anchored; prefix; suffix; post_anchored} ->
    Cmon.crecord "Filter" [
      "lhs"           , cmon_wild_symbol lhs;
      "pre_anchored"  , Cmon.bool pre_anchored;
      "prefix"        , Cmon.list_map cmon_wild_symbol prefix;
      "suffix"        , Cmon.list_map cmon_wild_symbol suffix;
      "post_anchored" , Cmon.bool post_anchored;
    ]

and cmon_regular_expression re =
  Cmon.record [
    "desc", cmon_regular_term re.desc;
    "position", cmon_position re.position;
  ]

let cmon_clause_action = function
  | Unreachable -> Cmon.constant "Unreachable"
  | Total code -> Cmon.constructor "Total" (cmon_ocamlcode code)
  | Partial code -> Cmon.constructor "Partial" (cmon_ocamlcode code)

let cmon_clause {pattern; lookaheads; action} =
  Cmon.record [
    "pattern", cmon_regular_expression pattern;
    "lookaheads", Cmon.list_map Cmon.string lookaheads;
    "action", cmon_clause_action action;
  ]

let cmon_entrypoints {error; startsymbols; name; args; clauses} =
  Cmon.record [
    "startsymbols", Cmon.list_map Cmon.string startsymbols;
    "error", Cmon.bool error;
    "name", Cmon.string name;
    "args", Cmon.list_map Cmon.string args;
    "clauses", Cmon.list_map cmon_clause clauses;
  ]

let cmon_definition {header; entrypoints; trailer} : Cmon.t =
  Cmon.record [
    "header", cmon_ocamlcode header;
    "entrypoints", Cmon.list (List.map cmon_entrypoints entrypoints);
    "trailer", cmon_ocamlcode trailer;
  ]

