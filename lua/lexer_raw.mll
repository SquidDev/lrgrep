{
  [@@@coverage exclude_file]
  let buffer_with len char =
    let b = Buffer.create len in
    Buffer.add_char b char;
    b
}

let white = [' ' '\t']


let digit = ['0'-'9']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']
let number = digit | ['E' 'e'] ['+' '-']? | '.'

let ident_head = ['a'-'z' 'A'-'Z' '_']
let ident_tail = ident_head | '_' | digit

rule token l = parse
| white+ as x           
| '\n'                  
| '\r' '\n'             
| "--[" ('='* as x) '[' 
(* We split line comments into two parts. Otherwise "--[^\n]*" would match "--[[foo]]". *)
| "--"       { token l lexbuf }

| "and"      { Parser_raw.AND }
| "break"    { Parser_raw.BREAK    }
| "do"       { Parser_raw.DO       }
| "else"     { Parser_raw.ELSE     }
| "elseif"   { Parser_raw.ELSEIF   }
| "end"      { Parser_raw.END      }
| "false"    { Parser_raw.FALSE    }
| "for"      { Parser_raw.FOR      }
| "function" { Parser_raw.FUNCTION }
| "if"       { Parser_raw.IF       }
| "in"       { Parser_raw.IN       }
| "local"    { Parser_raw.LOCAL    }
| "nil"      { Parser_raw.NIL      }
| "not"      { Parser_raw.NOT      }
| "or"       { Parser_raw.OR       }
| "repeat"   { Parser_raw.REPEAT   }
| "return"   { Parser_raw.RETURN   }
| "then"     { Parser_raw.THEN     }
| "true"     { Parser_raw.TRUE     }
| "until"    { Parser_raw.UNTIL    }
| "while"    { Parser_raw.WHILE    }

| ":"        { Parser_raw.COLON }
| ","        { Parser_raw.COMMA }
| "."        { Parser_raw.DOT }
| "..."      { Parser_raw.DOTS }
| "="        { Parser_raw.EQUALS }
| ";"        { Parser_raw.SEMICOLON }

| '(' { Parser_raw.OPAREN }  | ')' { Parser_raw.CPAREN }
| '{' { Parser_raw.OBRACE }  | '}' { Parser_raw.CBRACE }
| '[' { Parser_raw.OSQUARE } | ']' { Parser_raw.CSQUARE }

| '+'  { Parser_raw.ADD }
| '-'  { Parser_raw.SUB }
| '*'  { Parser_raw.MUL }
| '/'  { Parser_raw.DIV }
| '^'  { Parser_raw.POW }
| '%'  { Parser_raw.MOD }
| ".." { Parser_raw.CONCAT }
| "==" { Parser_raw.EQ }
| "~=" { Parser_raw.NE }
| "<"  { Parser_raw.LT }
| "<=" { Parser_raw.LE }
| ">"  { Parser_raw.GT }
| ">=" { Parser_raw.GE }
| '#'  { Parser_raw.LEN }

(* Numbers *)
| "0x" hex+ as i         { Parser_raw.INT }
| digit+ as i            { Parser_raw.INT }
| digit number* as i     { Parser_raw.NUMBER }
| '.' digit number* as i { Parser_raw.NUMBER }

(* Identifiers *)
| ident_head ident_tail* as i { Parser_raw.IDENT }

| '\"'          { string (buffer_with 17 '\"') (Buffer.create 17) '\"' lexbuf }
| '\''          { string (buffer_with 17 '\'') (Buffer.create 17) '\'' lexbuf }
| '[' ('='* as x) '[' { long_string (Buffer.create 16) (String.length x) (fun _ _ -> Parser_raw.STRING) l lexbuf }

| eof { Parser_raw.EOF }

| _ { failwith "Unexpected character" }

and string contents value c = parse
| '\"'              { Buffer.add_char contents '\"';
                      if c = '\"' then Parser_raw.STRING
                      else (Buffer.add_char value '\"'; string contents value c lexbuf) }
| '\''              { Buffer.add_char contents '\'';
                      if c = '\'' then Parser_raw.STRING
                      else (Buffer.add_char value '\''; string contents value c lexbuf) }

| "\\a"             { Buffer.add_string contents "\\a";  Buffer.add_char value '\007'; string contents value c lexbuf }
| "\\b"             { Buffer.add_string contents "\\b";  Buffer.add_char value '\b';   string contents value c lexbuf }
| "\\f"             { Buffer.add_string contents "\\f";  Buffer.add_char value '\012'; string contents value c lexbuf }
| "\\n"             { Buffer.add_string contents "\\n";  Buffer.add_char value '\n';   string contents value c lexbuf }
| "\\r"             { Buffer.add_string contents "\\r";  Buffer.add_char value '\r';   string contents value c lexbuf }
| "\\v"             { Buffer.add_string contents "\\v";  Buffer.add_char value '\011'; string contents value c lexbuf }
| "\\t"             { Buffer.add_string contents "\\t";  Buffer.add_char value '\t';   string contents value c lexbuf }

| "\\x" ((hex hex?) as x)
                    { Buffer.add_string contents "\\x"; Buffer.add_string contents x;
                      Buffer.add_char value ("0x" ^ x |> int_of_string |> char_of_int);
                      string contents value c lexbuf }
| "\\" ((digit digit? digit?) as x)
                    { Buffer.add_char contents '\\'; Buffer.add_string contents x;
                      Buffer.add_char value (int_of_string x |> char_of_int);
                      string contents value c lexbuf }

| "\\" ([^ '\r' '\n'] as x)
                    { Buffer.add_char contents '\\'; Buffer.add_char contents x;
                      Buffer.add_char value x;
                      string contents value c lexbuf }

| [^'\\' '\"' '\'' '\n']+ as x
                    { Buffer.add_string contents x;
                      Buffer.add_string value x;
                      string contents value c lexbuf }

| eof { failwith "Unterminated string" }
| '\r' { failwith "Unterminated string" }
| '\n' { failwith "Unterminated string" }
| _ { failwith "Unexpected character" }

and long_string buf eqs term l = parse
| [^']' '\r' '\n']+ as x { Buffer.add_string buf x;              long_string buf eqs term l lexbuf }
| ']' '='* ']' as x      { if String.length x == eqs + 2
                           then term eqs (Buffer.contents buf)
                           else (Buffer.add_string buf x;        long_string buf eqs term l lexbuf) }
| ']'                    { Buffer.add_char buf ']';              long_string buf eqs term l lexbuf }
| '\n'                   { Buffer.add_char buf '\n'; long_string buf eqs term l lexbuf }
| '\r' '\n'              { Buffer.add_string buf "\r\n"; long_string buf eqs term l lexbuf }
| eof                    { failwith "Unterminated string" }

and line_comment = parse
| [^'\r' '\n']* as x     { failwith "Comment" }
