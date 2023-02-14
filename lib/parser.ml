type lex_token = 
  | Ident of string
  | Set
  | Append 

type parse_token = 
  | SetProp of string * string 
  | AppendProp of string * string 
  | Invalid of string

let lex s =
  match s with
   | ":=" -> Set
   | "+=" -> Append
   | x -> Ident x
 

let convert a b c =
  match (a, b, c) with 
    | (Ident f, Set, Ident s) -> SetProp (f, s)
    | (Ident f, Append, Ident s) -> AppendProp (f, s)
    | _ -> Invalid "Syntax Error"
  
let rec parse_lexem tks lexems = 
  match lexems with
  | [] -> tks
  | a :: b :: c :: rest -> parse_lexem (convert a b c :: tks) rest
  | _ -> tks 