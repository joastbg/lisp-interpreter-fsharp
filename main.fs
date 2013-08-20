open FParsec

let str s = pstring s
let floatBetweenBrackets = str "[" >>. pfloat .>> str "]"

let floatList = str "[" >>. sepBy pfloat (str ",") .>> str "]"

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test pfloat "1.25"
test pfloat "3.141592654"

test floatBetweenBrackets "[1.0]";;

test floatBetweenBrackets "[]";;

test floatList "[]";;

test floatList "[1.0]";;

test floatList "[4,5,6]";;