fun only_capitals(l) =
  List.filter(fn x => Char.isUpper(String.sub(x, 0)))(l);

fun longest_string1(l) =
  List.foldl(
    fn (x, acc) => 
      if String.size(x) > String.size(acc) 
      then x 
      else acc
  )("")(l);

fun longest_string2(l) =
  List.foldl(
    fn (x, acc) => 
      if String.size(x) >= String.size(acc) 
      then x 
      else acc
  )("")(l);

fun longest_string_helper(f)(l) = 
  List.foldl(
    fn (x, acc) => 
      if f(String.size(x), String.size(acc)) 
      then x 
      else acc)
  ("")(l);

val longest_string3 = 
  longest_string_helper(fn (x, y) => x > y);

val longest_string4 = 
  longest_string_helper(fn (x, y) => x >= y);

val longest_capitalized = longest_string1 o only_capitals;

val rev_string = String.implode o rev o String.explode;

exception NoAnswer;

fun first_answer(f)(l) = 
  case l of
    [] => raise NoAnswer
    | x::xs => 
      case f(x) of
        SOME(x') => x'
        | NONE => first_answer(f)(xs);

fun all_answers(f)(l) = 
  case l of
    [] => SOME([])
    | xs => 
      let 
        fun iterate(acc, ol) = 
          case ol of
            [] => SOME(acc)
            | SOME(y)::ys => iterate(acc @ y, ys)
            | NONE::ys => NONE;
      in
        iterate([], List.map(f)(xs))
      end;

datatype pattern = Wildcard
  | Variable of string
  | UnitP
  | ConstP of int
  | TupleP of pattern list
  | ConstructorP of string * pattern;

datatype valu = Const of int
  | Unit
  | Tuple of valu list
  | Constructor of string * valu;

fun g(f1)(f2)(p) =
  let 
    val r = g(f1)(f2); 
  in
    case p of
      Wildcard => f1()
      | Variable x => f2(x)
      | TupleP x => List.foldl(fn (x', i) => r(x') + i)(0)(x)
      | ConstructorP(_, x) => r(x)
      | _ => 0
  end;

fun count_wildcards(p) = 
    g(fn () => 1)(fn x => 0)(p);

fun count_wild_and_variable_lengths(p) =
    g(fn () => 1)(fn x => String.size(x))(p);

fun count_some_var(s, p) =
    g(fn () => 0)(fn x => if x = s then 1 else 0)(p);

fun check_pat(p) = 
  let 
    fun get_list(p) = 
      case p of
        Variable x => [x]
        | TupleP xs => List.foldl(fn (xs, acc) => acc @ get_list(xs))([])(xs)
        | _ => [];

    fun has_duplicates(l) = 
      case l of
        [] => false
        | x::xs => List.exists((fn x' => x = x'))(xs) orelse has_duplicates(xs);
  in
    (not o has_duplicates o get_list)(p)
  end;

fun match(v, p) = 
  case (v, p) of
    (_, Wildcard) => SOME([])
    | (_, Variable x) => SOME([(x, v)])
    | (Unit, UnitP) => SOME([])
    | (Const x, ConstP y) => 
      if x = y 
      then SOME([]) 
      else NONE
    | (Tuple x, TupleP y) => 
      if List.length(x) = List.length(y) 
      then all_answers(match)(ListPair.zip(x, y))
      else NONE
    | (Constructor(x, x'), ConstructorP(y, y')) => 
      if x = y 
      then match(x', y') 
      else NONE
    | _ => NONE;

fun first_match(v)(l) = 
  (SOME(
    first_answer((fn p => match(v, p)))(l)
  )) handle NoAnswer => NONE;