fun same_string(s1: string, s2: string) =
    s1 = s2;


fun all_except_option(string, list) =
  let
    fun gather(prev, next) = 
      case next of
        [] => NONE
        | x::xs =>
            if same_string(string, x)
            then SOME(prev @ xs)
            else gather(prev @ [x], xs);
  in
    gather([], list)
  end;


fun get_substitutions1(lists, string) =
  case lists of
    [] => []
    | x::xs =>
      case all_except_option(string, x) of
        NONE => get_substitutions1(xs, string)
        | SOME items => items @ get_substitutions1(xs, string);


fun get_substitutions2(lists, string) =
  let
    fun get(list) =
      case list of
        NONE => []
        | SOME items => items;

    fun gather(lists, substitutions) =
      case lists of
        [] => []
        | x::xs =>
          case get(all_except_option(string, x)) of
            [] => gather(xs, substitutions)
            | items => gather(xs, items @ substitutions);
  in
    gather(lists, [])
  end;


fun similar_names (substitutions, { 
  first = first_name, 
  middle = middle_name, 
  last = last_name
}) =
  let
    fun get_with_name(name) = { 
      first =  name, 
      middle = middle_name, 
      last = last_name 
    };

    fun generate_names(substitutions, names) = 
      case substitutions of
        [] => names
        | x::xs =>
          generate_names(xs, names @ [get_with_name(x)]);
  in
    generate_names (
      get_substitutions1(substitutions, first_name),
      [get_with_name(first_name)]
    )
end;




datatype suit = Clubs | Diamonds | Hearts | Spades;
datatype rank = Jack | Queen | King | Ace | Num of int;
type card = suit * rank;
datatype color = Red | Black;
datatype move = Discard of card | Draw ;
exception IllegalMove;


fun card_color(card, _) =
  case card of
    Clubs => Black
    | Spades => Black
    | others => Red;


fun card_value(_, card) =
  case card of
    Num int => int
    | Ace => 11
    | others => 10;


fun remove_card(c, cs, e) =
  let
    fun remove(prev, next) = 
      case next of
        [] => raise e
        | x::xs =>
            if x = c
            then prev @ xs
            else remove(prev @ [x], xs);
  in
    remove([], cs)
  end;


fun all_same_color(cs) =
  case cs of
    [] => true
    | one::[] => true
    | x::y::xs =>
      card_color(x) = card_color(y)
      andalso all_same_color(y::xs);


fun sum_cards(cs) =
  let
    fun sum(cs, accounter) = 
      case cs of
        [] => accounter
        | x::xs => accounter + card_value(x) + sum_cards(xs);
  in
    sum(cs, 0)
  end;


fun score(cs, goal) =
  let
    val sum = sum_cards(cs);
    val main = abs(sum - goal);
    val prevScore = if sum > goal then 3 * main else main;
  in
    if all_same_color(cs)
    then prevScore div 2
    else prevScore
  end;


fun officiate(cards, moves, goal) =
  let
    fun play(cards, player_cards, moves) = 
      case moves of
        [] => score(player_cards, goal)
        | x::xs =>
          let
            fun draw_card(cards, player_cards, moves) = 
              case cards of
                [] => score(player_cards, goal)
                | x::xs =>
                  let
                    val drawed_player_cards = x::player_cards;
                  in
                    if sum_cards(drawed_player_cards) > goal
                    then score(drawed_player_cards, goal)
                    else play(xs, drawed_player_cards, moves)
                  end;

            fun discard_card(cards, player_cards, card, moves) =
              play(cards, remove_card(card, player_cards, IllegalMove), moves);
          in
            case x of
              Draw => draw_card(cards, player_cards, xs)
              | Discard card => discard_card(cards, player_cards, card, xs)
          end;
  in
    play(cards, [], moves)
  end;